#!/usr/bin/env Rscript
# scripts/ai_backfill.R - gated deep-scan AI-tooling-detection backfill for vcs-signals.
#
# Sub-commands wired together by CI (.github/workflows/ai-backfill.yml, the one-time full
# onset scan, and .github/workflows/ai-weekly.yml, the weekly incremental that swaps gate for
# gate-incremental):
#   enumerate -> full active github roster from the published summary's repos table (one job)
#   cheap     -> Tier-D marker + PR-agent pass over one mod-N shard, write a flagged partial
#                (matrix job)
#   gate      -> union every cheap shard's flagged partials into one flagged-roster (one job)
#   gate-incremental -> like gate, but narrow the flagged roster to repos carrying a tool not
#                yet in the published vcs_ai_signals detail (the weekly incremental gate used
#                by .github/workflows/ai-weekly.yml in place of gate)
#   deep      -> commit-history onset scan over one mod-N shard of the flagged roster,
#                build vcs_ai_signals detail rows (matrix job)
#   merge     -> reconcile node_id identity, reduce prior+incoming onsets, rebuild the
#                summary rollups, and republish (one job)
if (!exists("STARGAZER_PAGE"))       source("scripts/config.R")
if (!exists("ensure_series_schema")) source("scripts/helpers.R")
if (!exists("build_tree_query"))     source("scripts/github.R")
if (!exists("gh_release_exists"))    source("scripts/update.R")   # default_io, gh_release_*, seed_working_db
if (!exists("build_ai_detail"))      source("scripts/ai_signals.R")
if (!exists("write_roster"))         source("scripts/backfill.R") # shard_rows via helpers, roster idiom
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

AI_ROSTER_TABLE <- "roster"

# ---- roster IO --------------------------------------------------------------
write_ai_roster <- function(path, roster_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, sprintf("CREATE TABLE %s (
    repo_id TEXT PRIMARY KEY, owner TEXT NOT NULL, name TEXT NOT NULL,
    node_id TEXT, done INTEGER NOT NULL DEFAULT 0)", AI_ROSTER_TABLE))
  if (nrow(roster_df) > 0)
    DBI::dbWriteTable(con, AI_ROSTER_TABLE,
                      roster_df[c("repo_id", "owner", "name", "node_id", "done")], append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

load_ai_roster <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbReadTable(con, AI_ROSTER_TABLE)
}

# ---- enumerate --------------------------------------------------------------
#' Build the FULL active github roster from the published summary's embedded repos
#' table (NOT the star-filtered vcs_signals_summary that run_enumerate uses): the
#' zero-signal long tail is exactly where solo maintainers quietly adopt an AI tool.
#' Uses the native owner/name/node_id columns, so no slug split and node_id rides
#' through for the identity reconcile. Re-resolves owner/name from node_id for every row
#' that already carries one (mirrors resolve_node_ids's build_resolve_query/parse_resolve
#' pair, github.R:107/204, followRenames:true already baked in), so a rename since the
#' row's node_id was first attached does not leave a stale slug flowing into Task 7/9's
#' owner/name-keyed queries. A resolve hit that comes back with a DIFFERENT node_id than
#' the row's own means the old slug has been squatted (or otherwise reassigned) by an
#' unrelated repo, and that row is dropped from the roster entirely rather than updated,
#' so the squatter is never scanned under this row's repo_id. Same download as
#' backfill.R's enumerate.
run_enumerate_ai <- function(io, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  if (!isTRUE(io$download("vcs-signals-summary.db", out_dir)))
    stop("could not download vcs-signals-summary.db from the published release; nothing to enumerate")
  summary_path <- file.path(out_dir, "vcs-signals-summary.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), summary_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con,
    "SELECT repo_id, owner, name, node_id FROM repos WHERE host = 'github' AND status = 'active'")
  roster <- data.frame(repo_id = rows$repo_id, owner = rows$owner, name = rows$name,
                       node_id = rows$node_id, done = 0L, stringsAsFactors = FALSE)

  # Re-resolve owner/name from the immutable node_id for rows that already have one, so a
  # rename since the last resolve does not leave a stale slug. The query still runs by the
  # row's current (possibly stale) owner/name with followRenames:true, so the hit it comes
  # back with must be checked against the row's OWN node_id before it is trusted: a genuine
  # rename resolves to the SAME node_id at a new slug (owner/name are updated below), but a
  # SQUATTED old slug resolves to a DIFFERENT repo's node_id entirely. Trusting that second
  # case would point this roster row's owner/name at the squatter, and the cheap/deep passes
  # would then scan the squatter and write ITS markers/PRs/commits into the immutable
  # vcs_ai_signals table under this row's (unrelated) repo_id - so a node_id mismatch drops
  # the row from the roster outright rather than updating it. A batch that still faults
  # after the retry (or a row whose resolve comes back NA) keeps its pre-existing owner/name
  # (retried on the next enumerate), never dropped from the roster.
  have_id <- !is.na(roster$node_id) & nzchar(roster$node_id)
  drop_idx <- integer(0)
  for (rowset in chunk(which(have_id), CHEAP_BATCH)) {
    sub <- roster[rowset, , drop = FALSE]
    res <- tryCatch(io$graphql(build_resolve_query(sub$owner, sub$name)),
                    error = function(e) list(.err = TRUE))
    Sys.sleep(BATCH_DELAY_S)
    ok <- is.list(res) && is.null(res$.err) && !is.null(res$data) &&
      (is.null(res$errors) || errors_are_alias_not_found(res$errors))
    if (!ok) next
    pr <- parse_resolve(res$data, nrow(sub))
    for (j in seq_len(nrow(sub))) {
      r <- pr[pr$idx == (j - 1L), ]
      if (is.na(r$node_id) || is.na(r$name_with_owner)) next
      if (!identical(r$node_id, sub$node_id[j])) {
        drop_idx <- c(drop_idx, rowset[j])   # slug squatted/reassigned: exclude, never scan
        next
      }
      parts <- strsplit(r$name_with_owner, "/", fixed = TRUE)[[1]]
      roster$owner[rowset[j]] <- parts[1]
      roster$name[rowset[j]]  <- paste(parts[-1], collapse = "/")
    }
  }
  if (length(drop_idx) > 0) roster <- roster[-drop_idx, , drop = FALSE]

  message(sprintf("ai enumerate: %d active github repos", nrow(roster)))
  write_ai_roster(file.path(out_dir, "vcs-ai-roster.db"), roster)
}

# ---- flagged partial IO -----------------------------------------------------
.ai_empty_flagged <- function()
  data.frame(repo_id = character(), owner = character(), name = character(),
             node_id = character(), is_fork = integer(), parent = character(),
             pr_onset_date = character(), stringsAsFactors = FALSE)
.ai_empty_ev <- function()
  data.frame(repo_id = character(), tool = character(), tier = character(),
             marker = character(), agnostic = integer(), stringsAsFactors = FALSE)

write_flagged_partial <- function(path, flagged_df, evidence_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, "CREATE TABLE flagged (repo_id TEXT PRIMARY KEY, owner TEXT, name TEXT,
    node_id TEXT, is_fork INTEGER, parent TEXT, pr_onset_date TEXT)")
  DBI::dbExecute(con, "CREATE TABLE evidence (repo_id TEXT, tool TEXT, tier TEXT,
    marker TEXT, agnostic INTEGER)")
  if (nrow(flagged_df) > 0) DBI::dbWriteTable(con, "flagged", flagged_df, append = TRUE)
  if (nrow(evidence_df) > 0) DBI::dbWriteTable(con, "evidence", evidence_df, append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

read_flagged <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  list(
    flagged  = if (DBI::dbExistsTable(con, "flagged")) DBI::dbReadTable(con, "flagged") else .ai_empty_flagged(),
    evidence = if (DBI::dbExistsTable(con, "evidence")) DBI::dbReadTable(con, "evidence") else .ai_empty_ev())
}

# ---- cheap pass -------------------------------------------------------------
#' Cheap Tier-D marker + PR-agent pass over one even mod-N shard of the roster. Batches
#' TIER_D_BATCH repos through fetch_tree_markers + fetch_pr_agents, assembles evidence,
#' and writes only the flagged repos (repo_has_ai_signal) to a two-table partial. A repo
#' whose whole cheap batch faulted is absent from both fetch results and is skipped
#' (deferred, retried next run), never written as clean. Before each batch, a
#' graphql_rate_remaining(io) preflight (mirrors update.R:130-137) pauses the shard when
#' the budget is below AI_POINT_RESERVE, so an exhausted token stops the pass cleanly
#' instead of faulting batches into silent single-repo drops; the unscanned tail of this
#' shard is picked up by the next workflow_dispatch (enumerate + cheap re-run
#' deterministically over the same shard). fetch_tree_markers/fetch_pr_agents already
#' pace themselves with BATCH_DELAY_S, so this loop does not sleep again per batch.
run_cheap <- function(io, out_dir, roster_path, i, N, batch_size = TIER_D_BATCH) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  roster <- load_ai_roster(roster_path)
  mine <- roster[shard_rows(nrow(roster), i, N), , drop = FALSE]
  message(sprintf("ai cheap shard %d/%d: %d of %d repos", i, N, nrow(mine), nrow(roster)))

  flagged <- list(); evrows <- list(); scanned <- 0L
  for (idx in unname(chunk(seq_len(nrow(mine)), batch_size))) {
    rl <- graphql_rate_remaining(io)
    if (rl < AI_POINT_RESERVE) {
      message(sprintf(
        "ai cheap shard %d/%d: graphql rate remaining (%s) below reserve (%d); pausing after %d of %d repos",
        i, N, rl, AI_POINT_RESERVE, scanned, nrow(mine)))
      break
    }
    repos <- mine[idx, , drop = FALSE]
    trees <- tryCatch(fetch_tree_markers(io, repos, batch_size), error = function(e) NULL)
    prs   <- tryCatch(fetch_pr_agents(io, repos, batch_size), error = function(e) NULL)
    for (r in seq_len(nrow(repos))) {
      rid <- repos$repo_id[r]
      tree <- if (is.null(trees)) NULL else trees[[rid]]
      pr   <- if (is.null(prs)) NULL else prs[[rid]]
      if (is.null(tree) && is.null(pr)) next            # both channels errored -> deferred
      ev <- assemble_repo_evidence(tree, pr)
      if (!repo_has_ai_signal(ev)) next
      flagged[[length(flagged) + 1L]] <- data.frame(
        repo_id = rid, owner = repos$owner[r], name = repos$name[r], node_id = repos$node_id[r],
        is_fork = as.integer(isTRUE(tree$is_fork)),
        parent = if (is.null(tree)) NA_character_ else (tree$parent %||% NA_character_),
        pr_onset_date = earliest_agent_pr_date(pr),
        stringsAsFactors = FALSE)
      ev$repo_id <- rid
      ev$agnostic <- as.integer(ev$agnostic)
      evrows[[length(evrows) + 1L]] <- ev[c("repo_id", "tool", "tier", "marker", "agnostic")]
    }
    scanned <- scanned + nrow(repos)
  }
  flagged_df <- if (length(flagged)) do.call(rbind, flagged) else .ai_empty_flagged()
  ev_df <- if (length(evrows)) do.call(rbind, evrows) else .ai_empty_ev()
  write_flagged_partial(file.path(out_dir, sprintf("vcs-ai-cheap-%d.db", i)), flagged_df, ev_df)
  message(sprintf("ai cheap shard %d/%d: %d flagged repos, %d evidence rows",
                  i, N, nrow(flagged_df), nrow(ev_df)))
}

# ---- gate -------------------------------------------------------------------
#' Union every cheap shard's flagged partial into one smaller flagged-roster the deep
#' matrix shards over. Dedups flagged rows by repo_id and evidence rows by
#' (repo_id, tool, marker), so a repo split across a shard boundary is folded once.
run_gate <- function(out_dir, parts_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  parts <- list.files(parts_dir, pattern = "^vcs-ai-cheap-.*\\.db$", full.names = TRUE)
  fl <- lapply(parts, function(p) read_flagged(p)$flagged)
  ev <- lapply(parts, function(p) read_flagged(p)$evidence)
  flagged_df <- if (length(fl)) do.call(rbind, fl) else .ai_empty_flagged()
  ev_df <- if (length(ev)) do.call(rbind, ev) else .ai_empty_ev()
  flagged_df <- flagged_df[!duplicated(flagged_df$repo_id), , drop = FALSE]
  ev_df <- ev_df[!duplicated(paste(ev_df$repo_id, ev_df$tool, ev_df$marker, sep = "\r")), , drop = FALSE]
  write_flagged_partial(file.path(out_dir, "vcs-ai-flagged-roster.db"), flagged_df, ev_df)
  message(sprintf("ai gate: %d flagged repos, %d evidence rows across %d shard(s)",
                  nrow(flagged_df), nrow(ev_df), length(parts)))
}

#' Read the published vcs_ai_signals detail (the incremental baseline) out of the current
#' release's summary shard. Returns the typed 0-row frame when no release exists yet (first
#' ever weekly run) or the table is absent, so the gate treats every flagged repo as new
#' rather than erroring - absence is never read as "clean".
.ai_read_published_detail <- function(io, out_dir) {
  if (!isTRUE(io$download("vcs-signals-summary.db", out_dir))) return(.ai_empty_signals())
  p <- file.path(out_dir, "vcs-signals-summary.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, "vcs_ai_signals")) return(.ai_empty_signals())
  DBI::dbReadTable(con, "vcs_ai_signals")
}

#' The weekly incremental gate. Unions the cheap partials into the full flagged roster with
#' run_gate verbatim, then narrows it to only the repos carrying a tool not yet present in the
#' published vcs_ai_signals detail (select_incremental_repos), so the deep matrix re-onsets
#' only genuinely new adoptions - re-detecting an already-published tool's ONSET would be a
#' no-op through ai_onset_reducer. The published detail is the sole baseline (no separate
#' last-week store). Before narrowing, also computes select_confirmation_rows over the FULL
#' evidence frame and writes it with export_ai_shard (the same writer run_deep uses) as
#' out_dir/vcs-ai-shard-confirm.db, so an already-published tool's last_confirmed_date still
#' advances even though its repo is skipped below - without that, the skip branch would freeze
#' last_confirmed_date at the onset date forever, since run_deep is the only other path that
#' stamps it. The name rides run_merge's unchanged vcs-ai-shard-*.db glob (ai_backfill.R:347),
#' so no merge code changes; only the workflow (Task 4) has to route the file into the merge
#' job's parts directory. A week with no new adoptions narrows the flagged roster to empty; the
#' deep matrix then produces empty shards and run_merge folds prior + nothing + confirmations =
#' prior with last_confirmed refreshed. Rewrites the same vcs-ai-flagged-roster.db the deep
#' matrix reads, so no downstream job changes to that artifact.
run_gate_incremental <- function(io, out_dir, parts_dir) {
  run_gate(out_dir, parts_dir)                       # full flagged roster (B2 verbatim)
  roster_path <- file.path(out_dir, "vcs-ai-flagged-roster.db")
  fr <- read_flagged(roster_path)
  published <- .ai_read_published_detail(io, out_dir)
  keep <- select_incremental_repos(fr$flagged, fr$evidence, published)

  today <- format(Sys.Date())
  confirm_rows <- select_confirmation_rows(fr$evidence, published, today)
  export_ai_shard(file.path(out_dir, "vcs-ai-shard-confirm.db"), confirm_rows)

  flagged_df <- fr$flagged[fr$flagged$repo_id %in% keep, , drop = FALSE]
  ev_df      <- fr$evidence[fr$evidence$repo_id %in% keep, , drop = FALSE]
  write_flagged_partial(roster_path, flagged_df, ev_df)
  message(sprintf(
    "ai gate (incremental): %d of %d flagged repos carry a new tool since the last publish, %d confirmation rows",
    nrow(flagged_df), nrow(fr$flagged), nrow(confirm_rows)))
}

# ---- deep onset shard IO ----------------------------------------------------
export_ai_shard <- function(path, rows) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  ensure_series_schema(con)                        # folds in the vcs_ai_signals CREATE
  if (nrow(rows) > 0) DBI::dbWriteTable(con, "vcs_ai_signals", rows, append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

#' Structured author-email commit search query for a bot-identity tool, or NA when the
#' tool has no email in AI_BOT_ALLOWLIST (marker-only tools like cursor/gemini/windsurf).
#' The author-email qualifier is an EXACT match, so a hit is a confirmed Tier-A onset,
#' unlike a fuzzy message-token search (a floor). Internal.
.ai_author_email <- function(tool) {
  em <- names(AI_BOT_ALLOWLIST)[AI_BOT_ALLOWLIST == tool]
  em <- em[grepl("@", em)]
  if (length(em)) em[1] else NA_character_
}

# ---- deep onset scan --------------------------------------------------------
#' Deep onset scan over one even mod-N shard of the flagged roster. Per repo:
#'   (0) a graphql_rate_remaining(io) preflight (mirrors update.R:130-137 and run_cheap's,
#'       Task 7): when the budget is below AI_POINT_RESERVE, pause the shard rather than
#'       let fetch_marker_onset fail closed to NA onset rows that never recover across
#'       deterministic re-runs;
#'   (1) date each COMMITTED Tier-D marker exactly by paging its REAL repo path's history
#'       (marker_repo_path prepends .github/ for a github-located marker; fetch_marker_onset,
#'       GraphQL budget) - a fault leaves that marker's onset NA (build_ai_detail tolerates
#'       it). An IGNORE-TOKEN marker names a .gitignore/.Rbuildignore entry, not a committed
#'       path, so it is NOT queried; it takes an honest censored floor of today via
#'       build_onset_map;
#'   (2) for each flagged bot-identity tool, one author-email commit search (io$search,
#'       REST-search budget) - a hit is an EXACT Tier-A onset and adds a Tier-A evidence
#'       row (authored = 1, since an author-email match means the bot itself authored the
#'       commit) so a marker + a bot commit corroborate to two tiers;
#'   (3) the PR onset carried from the cheap pass (exact);
#' then build_onset_map + apply_fork_guard (a fork censors every Tier-D onset to a floor)
#' + build_ai_detail collapse each tool through ai_onset_reducer, taking the tighter onset.
#' Writes the 7-col vcs_ai_signals partial. Template-seed (first-commit) detection is left
#' to first_commit_touches = character(0) here; only the fork guard fires in B2.
run_deep <- function(io, out_dir, roster_path, i, N,
                     marker_delay = BACKFILL_DELAY_S, search_delay = SEARCH_DELAY_S) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  fr <- read_flagged(roster_path)
  flagged <- fr$flagged; evidence <- fr$evidence
  mine <- flagged[shard_rows(nrow(flagged), i, N), , drop = FALSE]
  message(sprintf("ai deep shard %d/%d: %d of %d flagged repos", i, N, nrow(mine), nrow(flagged)))
  today <- format(Sys.Date())

  acc <- list()
  for (r in seq_len(nrow(mine))) {
    rl <- graphql_rate_remaining(io)
    if (rl < AI_POINT_RESERVE) {
      message(sprintf(
        "ai deep shard %d/%d: graphql rate remaining (%s) below reserve (%d); pausing after %d of %d repos",
        i, N, rl, AI_POINT_RESERVE, r - 1L, nrow(mine)))
      break
    }
    rid <- mine$repo_id[r]; owner <- mine$owner[r]; name <- mine$name[r]
    ev <- evidence[evidence$repo_id == rid, c("tool", "tier", "marker", "agnostic"), drop = FALSE]
    if (nrow(ev) == 0) next
    ev$authored <- 0L   # only a Tier-A author-email hit below sets authored = 1

    # (1) Tier-D onsets, keyed by the FULL evidence marker string. A COMMITTED marker (its
    #     marker is the tree entry name) is dated exactly by paging its REAL repo path's
    #     history - marker_repo_path prepends .github/ for a github-located marker, which
    #     GraphQL history(path:) resolves for files, nested paths, and directories alike. An
    #     IGNORE-TOKEN marker ("ignore:<path>") names a .gitignore/.Rbuildignore entry, not a
    #     committed path, so its history cannot be dated: it takes an honest censored floor of
    #     today (build_onset_map stamps first_seen_censored = 1), and no history call is spent
    #     on a path that does not exist in the tree.
    marker_dates <- list()
    for (marker in unique(ev$marker[ev$tier == "D"])) {
      if (startsWith(marker, "ignore:")) {
        # End-of-day instant so a same-day committed exact (e.g. "...T10:00:00Z") sorts
        # BEFORE this floor and correctly dominates it in the reducer; a bare date-only
        # "today" would be a lexicographic prefix of any same-day instant and wrongly win.
        # (`today` itself stays date-only for the last_confirmed stamp below.)
        marker_dates[[marker]] <- paste0(today, "T23:59:59Z")
        next
      }
      d <- tryCatch(fetch_marker_onset(io, owner, name, marker_repo_path(marker), delay = marker_delay),
                    error = function(e) NA_character_)
      if (!is.na(d)) marker_dates[[marker]] <- d
    }

    # (2) Tier-A author-email commit onsets (exact) for flagged bot-identity tools.
    commit_onsets <- NULL; extra_ev <- NULL
    for (tool in unique(ev$tool[!as.logical(ev$agnostic)])) {
      email <- .ai_author_email(tool)
      if (is.na(email)) next
      d <- tryCatch(io$search(owner, name, sprintf("author-email:%s", email), search_delay),
                    error = function(e) NA_character_)
      if (is.na(d)) next
      commit_onsets <- rbind(commit_onsets, data.frame(tool = tool, tier = "A",
        first_seen_date = d, confirmed = TRUE, stringsAsFactors = FALSE))
      extra_ev <- rbind(extra_ev, data.frame(tool = tool, tier = "A", marker = "A",
        agnostic = 0L, authored = 1L, stringsAsFactors = FALSE))
    }
    full_ev <- rbind(ev, extra_ev)

    # (3) assemble + guard + collapse.
    onsets <- build_onset_map(full_ev, marker_dates, commit_onsets, mine$pr_onset_date[r])
    guarded <- apply_fork_guard(full_ev, isTRUE(mine$is_fork[r] == 1L), mine$parent[r], character(0))
    detail <- build_ai_detail(rid, guarded, onsets, today)
    if (nrow(detail) > 0) acc[[length(acc) + 1L]] <- detail
  }
  rows <- if (length(acc)) do.call(rbind, acc) else .ai_empty_signals()
  export_ai_shard(file.path(out_dir, sprintf("vcs-ai-shard-%d.db", i)), rows)
  message(sprintf("ai deep shard %d/%d: %d onset detail rows", i, N, nrow(rows)))
}

# ---- merge ------------------------------------------------------------------
#' Fold every deep shard's vcs_ai_signals partial into the published onset table and
#' republish. Seeds the working DB from the recent shard (which already carries the prior
#' vcs_ai_signals; no explicit protect_history_pull here, since vcs_ai_signals has no year
#' component and publish()'s own internal pull handles the change-gate), then:
#' reconcile_ai_identity carries any node_id-collision onsets onto the canonical repo_id
#' (PK-safe, before the reduce); ai_onset_reducer merges the reconciled prior set with the
#' incoming partials by the six column rules; the working vcs_ai_signals is
#' DELETE-and-rewritten with the fully-reduced set (never blanket-deleted and re-detected -
#' the rows are immutable, the DELETE only follows the R-side reduce); the summary rollups
#' are rebuilt so ai_* columns reflect the merge. Publishes with touched_years =
#' character(0), so no year shard is re-exported.
run_merge <- function(io, out_dir, parts_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  working_path <- file.path(out_dir, "_ai_merge_working.db")
  seed_working_db(io, out_dir, working_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), working_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con)
  ensure_series_schema(con)

  # No explicit protect_history_pull here (unlike backfill.R::run_merge): vcs_ai_signals
  # has no year component, so there is no year-shard content to fold in or protect;
  # seed_working_db already carries the prior vcs_ai_signals via the recent shard, and
  # publish()'s own force_full-gated protect_history_pull handles the change-gate pull.
  # An explicit call here would just download the full published history twice.
  reconcile_ai_identity(con)

  prior <- if (DBI::dbExistsTable(con, "vcs_ai_signals")) DBI::dbReadTable(con, "vcs_ai_signals")
           else .ai_empty_signals()

  parts <- list.files(parts_dir, pattern = "^vcs-ai-shard-.*\\.db$", full.names = TRUE)
  part_rows <- lapply(parts, function(p) {
    pcon <- DBI::dbConnect(RSQLite::SQLite(), p)
    on.exit(DBI::dbDisconnect(pcon), add = TRUE)
    if (!DBI::dbExistsTable(pcon, "vcs_ai_signals")) return(.ai_empty_signals())
    DBI::dbReadTable(pcon, "vcs_ai_signals")
  })
  incoming <- if (length(part_rows)) do.call(rbind, part_rows) else .ai_empty_signals()

  reduced <- ai_onset_reducer(prior, incoming)
  DBI::dbExecute(con, "DELETE FROM vcs_ai_signals")
  if (nrow(reduced) > 0) DBI::dbWriteTable(con, "vcs_ai_signals", reduced, append = TRUE)

  # Rebuild the summary so ai_* rollups reflect the merged onsets. Non-AI columns come
  # from the seeded series_latest; descriptive + release facts carry forward from the
  # prior summary (no fresh gauge collection this run, so compute_release_facts = FALSE).
  today <- format(Sys.Date())
  repos_all <- DBI::dbReadTable(con, "repos")
  rp_all <- DBI::dbReadTable(con, "repo_packages")
  series_all <- DBI::dbGetQuery(con, "SELECT repo_id, date, metric, value FROM signals_series")
  latest_all <- DBI::dbGetQuery(con, "SELECT repo_id, metric, value FROM series_latest")
  prev_attrs <- DBI::dbGetQuery(con,
    "SELECT repo_id, license, topics, is_archived, last_commit_date,
            last_release_date, median_days_between_releases
       FROM vcs_signals_summary WHERE repo_id IS NOT NULL")
  if (nrow(prev_attrs) > 0) {
    prev_attrs <- prev_attrs[!duplicated(prev_attrs$repo_id), ]
    prev_attrs$is_archived <- as.integer(prev_attrs$is_archived)
  }
  repo_attrs <- merge(repos_all[, c("repo_id", "first_seen", "last_seen")], prev_attrs,
                      by = "repo_id", all.x = TRUE)
  summary_df <- build_signals_summary(latest_all, series_all, repo_attrs, rp_all, today,
                                      compute_release_facts = FALSE, ai_signals = reduced)
  DBI::dbExecute(con, "DELETE FROM vcs_signals_summary")
  if (nrow(summary_df) > 0) DBI::dbWriteTable(con, "vcs_signals_summary", summary_df, append = TRUE)

  message(sprintf("ai merge: %d prior, %d incoming, %d reduced onset rows",
                  nrow(prior), nrow(incoming), nrow(reduced)))
  invisible(publish(io, con, out_dir, tag = "current", source_kind = "live",
                    touched_years = character(0)))
}

# ---- CLI dispatch -----------------------------------------------------------
main <- function(mode, out_dir) {
  token <- Sys.getenv("VCS_SIGNALS_TOKEN")
  io <- list(
    graphql        = default_io(token)$graphql,
    search         = function(owner, name, query, delay = SEARCH_DELAY_S)
                       search_earliest_commit(token, owner, name, query, delay),
    release_exists = function() gh_release_exists(RELEASE_REPO),
    download       = function(pattern, dir) gh_release_download(RELEASE_REPO, pattern, dir),
    upload         = function(path) gh_release_upload(RELEASE_REPO, path))

  if (mode == "enumerate") {
    run_enumerate_ai(io, out_dir)
  } else if (mode == "cheap") {
    i <- suppressWarnings(as.integer(Sys.getenv("VCS_SHARD_I", "0")))
    N <- suppressWarnings(as.integer(Sys.getenv("VCS_SHARD_N", "1")))
    if (is.na(i) || is.na(N) || N < 1L || i < 0L || i >= N)
      stop("cheap: VCS_SHARD_I must be in [0, VCS_SHARD_N)")
    roster_dir <- Sys.getenv("VCS_ROSTER", out_dir)
    run_cheap(io, out_dir, file.path(roster_dir, "vcs-ai-roster.db"), i, N)
  } else if (mode == "gate") {
    run_gate(out_dir, Sys.getenv("VCS_PARTS", "parts"))
  } else if (mode == "gate-incremental") {
    run_gate_incremental(io, out_dir, Sys.getenv("VCS_PARTS", "parts"))
  } else if (mode == "deep") {
    i <- suppressWarnings(as.integer(Sys.getenv("VCS_SHARD_I", "0")))
    N <- suppressWarnings(as.integer(Sys.getenv("VCS_SHARD_N", "1")))
    if (is.na(i) || is.na(N) || N < 1L || i < 0L || i >= N)
      stop("deep: VCS_SHARD_I must be in [0, VCS_SHARD_N)")
    flagged_dir <- Sys.getenv("VCS_FLAGGED", out_dir)
    run_deep(io, out_dir, file.path(flagged_dir, "vcs-ai-flagged-roster.db"), i, N)
  } else if (mode == "merge") {
    run_merge(io, out_dir, Sys.getenv("VCS_PARTS", "parts"))
  } else {
    stop("usage: ai_backfill.R [enumerate|cheap|gate|gate-incremental|deep|merge]")
  }
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  mode <- if (length(args) >= 1) args[1] else ""
  out_dir <- Sys.getenv("VCS_OUT", "out")
  main(mode, out_dir)
}
