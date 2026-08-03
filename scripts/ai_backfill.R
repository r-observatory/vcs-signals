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

# ---- dev-tooling partial IO -------------------------------------------------
# A stateless presence snapshot, written as a SEPARATE shard from the AI flagged/evidence
# partials (those are scoped to the AI-flagged subset by the downstream gate/deep pipeline).
.devtool_empty_shard <- function() {
  out <- data.frame(repo_id = character(0), last_scanned = character(0), stringsAsFactors = FALSE)
  cbind(out, .devtool_empty())
}

#' Stack two dev-tooling snapshots that may not share a column set.
#'
#' The flag catalogue grows, so the published snapshot is written under whatever ruleset
#' was current when its rows were scanned while an incoming shard carries today's. A plain
#' rbind errors on that ("numbers of columns of arguments do not match") and takes the
#' whole merge with it, which is a data outage caused by adding a column.
#'
#' Columns absent from one side become NA there, never 0. A repository scanned before a
#' flag existed was not found to lack it; nobody looked. The 0 would be indistinguishable
#' from a real negative and would understate every new flag until the whole roster is
#' rescanned. Column order follows the current catalogue, with any column only the prior
#' side knows kept on the end rather than silently dropped, so a rolled-back ruleset does
#' not destroy data it no longer reads.
bind_dev_tooling <- function(prior, incoming) {
  if (is.null(prior) || !nrow(prior)) return(incoming)
  if (is.null(incoming) || !nrow(incoming)) return(prior)
  want <- c("repo_id", "last_scanned", dev_tooling_columns())
  cols <- c(want, setdiff(c(names(prior), names(incoming)), want))
  fill <- function(d) {
    for (cn in setdiff(cols, names(d))) d[[cn]] <- NA
    d[, cols, drop = FALSE]
  }
  rbind(fill(prior), fill(incoming))
}

write_dev_tooling_partial <- function(path, dev_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, dev_tooling_create_sql())
  if (nrow(dev_df) > 0) DBI::dbWriteTable(con, "vcs_dev_tooling", dev_df, append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

read_dev_tooling <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (DBI::dbExistsTable(con, "vcs_dev_tooling")) DBI::dbReadTable(con, "vcs_dev_tooling")
  else .devtool_empty_shard()
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

  flagged <- list(); evrows <- list(); dev_rows <- list(); scanned <- 0L
  today <- format(Sys.Date())
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
      # Dev-tooling snapshot for EVERY successfully-fetched repo, before the AI-only gate below.
      # Honest-NA: a repo whose tree channel errored (trees NULL) or whose alias came back null
      # (parse_tree_markers degrades a gone/private repo to empty entries with is_fork = NA) is
      # NOT assessed and gets no row - it is deferred, never written as clean all-zeros.
      if (!is.null(tree) && !is.na(tree$is_fork)) {
        dv <- classify_dev_tooling(tree$root_entries, tree$github_entries)
        dv$repo_id <- rid
        dv$last_scanned <- today
        dev_rows[[length(dev_rows) + 1L]] <- dv[c("repo_id", "last_scanned", dev_tooling_columns())]
      }
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
  dev_df <- if (length(dev_rows)) do.call(rbind, dev_rows) else .devtool_empty_shard()
  write_dev_tooling_partial(file.path(out_dir, sprintf("vcs-dev-tooling-%d.db", i)), dev_df)
  message(sprintf("ai cheap shard %d/%d: %d flagged repos, %d evidence rows, %d dev-tooling rows",
                  i, N, nrow(flagged_df), nrow(ev_df), nrow(dev_df)))
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
export_ai_shard <- function(path, rows, model_rows = NULL) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  ensure_series_schema(con)                        # folds in the vcs_ai_signals CREATE
  if (nrow(rows) > 0) DBI::dbWriteTable(con, "vcs_ai_signals", rows, append = TRUE)
  if (!is.null(model_rows) && nrow(model_rows) > 0)
    DBI::dbWriteTable(con, "vcs_ai_models", model_rows, append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

#' Structured author-email commit search query for a bot-identity tool, or NA when the
#' tool has no email in AI_BOT_ALLOWLIST (marker-only tools like cursor/gemini/windsurf).
#' The author-email qualifier is an EXACT match, so a hit is a confirmed Tier-A onset,
#' unlike a fuzzy message-token search (a floor). Internal.
#' Every Tier-A search term for a tool, one per allowlisted identity.
#'
#' The allowlist holds two shapes and only one was ever searched: an email
#' (noreply@anthropic.com) and a bot login (copilot-swe-agent[bot], cursor[bot],
#' devin-ai-integration[bot], google-labs-jules[bot], openhands-agent). The old
#' helper filtered on grepl("@"), so five of the six identities were silently
#' skipped and no Copilot, Cursor, Devin, Jules or OpenHands commit could ever be
#' found by Tier A, however well the query was formed.
#'
#' An email goes to author-email:, a login to author:. Returns character(0) when a
#' tool has no allowlisted identity, so the caller simply searches nothing.
.ai_author_queries <- function(tool) {
  ids <- names(AI_BOT_ALLOWLIST)[AI_BOT_ALLOWLIST == tool]
  if (!length(ids)) return(character(0))
  vapply(ids, function(id) {
    if (grepl("@", id, fixed = TRUE)) paste0("author-email:", id) else paste0("author:", id)
  }, character(1), USE.NAMES = FALSE)
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
#'   (2) for each flagged bot-identity tool, one author-email commit search (io$search_hit,
#'       REST-search budget) - a hit is an EXACT Tier-A onset and adds a Tier-A evidence
#'       row (authored = 1, since an author-email match means the bot itself authored the
#'       commit) so a marker + a bot commit corroborate to two tiers;
#'   (3) the PR onset carried from the cheap pass (exact);
#' then build_onset_map + apply_fork_guard (a fork censors every Tier-D onset to a floor)
#' + build_ai_detail collapse each tool through ai_onset_reducer, taking the tighter onset.
#' Writes the 7-col vcs_ai_signals partial. Template-seed (first-commit) detection is left
#' to first_commit_touches = character(0) here; only the fork guard fires in B2.
#' Repos whose published onset row was already confirmed today.
#'
#' A full gate hands every shard the entire flagged roster, so a second dispatch
#' would otherwise spend its whole budget redoing what the first one finished and
#' never reach the tail. This reads the roster's own confirmation dates, which
#' the pipeline already maintains, rather than inventing a campaign marker.
#'
#' Returns NULL when there is nothing to read, which the caller treats as "skip
#' nothing" rather than as "everything is done".
load_confirmed_today <- function(roster_path, today = format(Sys.Date())) {
  db <- file.path(dirname(roster_path), "vcs-signals-summary.db")
  if (!file.exists(db)) return(NULL)
  con <- tryCatch(DBI::dbConnect(RSQLite::SQLite(), db), error = function(e) NULL)
  if (is.null(con)) return(NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, "vcs_ai_signals")) return(NULL)
  tryCatch(
    DBI::dbGetQuery(con,
      "SELECT DISTINCT repo_id FROM vcs_ai_signals WHERE last_confirmed_date = ?",
      params = list(today))$repo_id,
    error = function(e) NULL)
}

run_deep <- function(io, out_dir, roster_path, i, N,
                     marker_delay = BACKFILL_DELAY_S, search_delay = SEARCH_DELAY_S) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  fr <- read_flagged(roster_path)
  flagged <- fr$flagged; evidence <- fr$evidence
  mine <- flagged[shard_rows(nrow(flagged), i, N), , drop = FALSE]
  message(sprintf("ai deep shard %d/%d: %d of %d flagged repos", i, N, nrow(mine), nrow(flagged)))
  today <- format(Sys.Date())

  acc <- list()
  unavailable <- 0L   # searches the API refused, kept apart from searches that missed
  model_rows <- list()   # one row per repo per tool per model named in a trailer

  # A shard that runs past the job's timeout is cancelled, and a cancelled job
  # skips its upload step, so every repo it scanned is discarded. The first full
  # re-scan lost all twelve shards that way: 181 repos at eleven searches and six
  # seconds a search is 3.3 hours before overhead, against a 240 minute cap.
  #
  # Stopping short of the cap turns that into a partial shard that is uploaded
  # and folded, which is the same bargain run_cheap already takes when the point
  # budget runs low. The tail is picked up by the next dispatch.
  deadline <- Sys.time() + AI_DEEP_BUDGET_S
  stopped_early <- FALSE
  skipped <- 0L
  # Repos whose published row was already confirmed today, i.e. by an earlier
  # dispatch of this same campaign.
  done_today <- tryCatch(load_confirmed_today(roster_path), error = function(e) NULL)
  for (r in seq_len(nrow(mine))) {
    if (Sys.time() >= deadline) {
      stopped_early <- TRUE
      message(sprintf(
        "ai deep shard %d/%d: stopping at %d of %d repos, %.0f min budget reached; the rest ride the next dispatch",
        i, N, r - 1L, nrow(mine), AI_DEEP_BUDGET_S / 60))
      break
    }
    rl <- graphql_rate_remaining(io)
    if (rl < AI_POINT_RESERVE) {
      message(sprintf(
        "ai deep shard %d/%d: graphql rate remaining (%s) below reserve (%d); pausing after %d of %d repos",
        i, N, rl, AI_POINT_RESERVE, r - 1L, nrow(mine)))
      break
    }
    rid <- mine$repo_id[r]; owner <- mine$owner[r]; name <- mine$name[r]
    # Already scanned in this campaign. A full gate hands every shard the whole
    # roster, so without this a second dispatch would spend its budget redoing
    # the repos the first one finished and never reach the tail. Keyed on the
    # published confirmation date rather than a campaign marker, because that is
    # a fact the pipeline already records.
    if (!is.null(done_today) && rid %in% done_today) { skipped <- skipped + 1L; next }

    ev <- evidence[evidence$repo_id == rid, c("tool", "tier", "marker", "agnostic"), drop = FALSE]
    if (nrow(ev) == 0) next
    ev$authored <- 0L   # only a Tier-A author-email hit below sets authored = 1
    # Cheap-pass evidence is markers and PRs: no commit search ran for it, so both
    # counts are "nobody asked" rather than zero. The tier-A and tier-B blocks
    # below append rows that carry real numbers.
    ev$authored_commits <- NA_integer_
    ev$assisted_commits <- NA_integer_

    # (1) Tier-D onsets, keyed by the FULL evidence marker string. A COMMITTED marker (its
    #     marker is the tree entry name) is dated exactly by paging its REAL repo path's
    #     history - marker_repo_path prepends .github/ for a github-located marker, which
    #     GraphQL history(path:) resolves for files, nested paths, and directories alike. An
    #     IGNORE-TOKEN marker ("gitignore:<path>" / "rbuildignore:<path>") names an entry in
    #     that file rather than a
    #     committed path, so its history cannot be dated: it takes an honest censored floor of
    #     today (build_onset_map stamps first_seen_censored = 1), and no history call is spent
    #     on a path that does not exist in the tree.
    marker_dates <- list()
    exact_ignores <- character(0)
    for (marker in unique(ev$marker[ev$tier == "D"])) {
      if (ai_is_ignore_marker(marker)) {
        # Bisect the ignore file's own history for the commit that added the line.
        # Stamping the scan date instead is what left most of the onset table sitting
        # on whichever day we last ran, and it is the reason the curve can only chart
        # a third of the detections.
        parts <- strsplit(marker, ":", fixed = TRUE)[[1]]
        file  <- if (identical(parts[1], "rbuildignore")) ".Rbuildignore" else ".gitignore"
        token <- paste(parts[-1], collapse = ":")
        got <- tryCatch(fetch_ignore_onset(io, owner, name, file, token, delay = marker_delay),
                        error = function(e) list(date = NA_character_, exact = FALSE))
        if (!is.na(got$date) && isTRUE(got$exact)) {
          marker_dates[[marker]] <- got$date
          exact_ignores <- c(exact_ignores, marker)
        } else if (!is.na(got$date)) {
          # Present at the oldest revision we can see, so the line predates the history.
          # A tighter floor than the scan date, and still a floor.
          marker_dates[[marker]] <- got$date
        } else {
          # End-of-day instant so a same-day committed exact sorts BEFORE this floor and
          # dominates it in the reducer; a bare date-only "today" would be a lexicographic
          # prefix of any same-day instant and wrongly win.
          marker_dates[[marker]] <- paste0(today, "T23:59:59Z")
        }
        next
      }
      d <- tryCatch(fetch_marker_onset(io, owner, name, marker_repo_path(marker), delay = marker_delay),
                    error = function(e) NA_character_)
      if (!is.na(d)) marker_dates[[marker]] <- d
    }

    # (2) Tier-A author-email commit onsets (exact) for flagged bot-identity tools.
    commit_onsets <- NULL; extra_ev <- NULL
    for (tool in unique(ev$tool[!as.logical(ev$agnostic)])) {
      # Every allowlisted identity for the tool, not just an email-shaped one.
      #
      # Routed through search_hit, which reports a refusal as a refusal. The old
      # transport collapsed both outcomes onto NA, so a throttled tier-A search
      # published as "this bot has never committed here" with exactly the
      # confidence of a measured absence. Tiers B and C already keep them apart;
      # tier A is the one that was still guessing, and it is the tier whose
      # zeros the canary reports.
      hits <- character(0)
      # The author qualifier is an exact match, so its total_count is the number
      # of commits this identity authored here. A refused search contributes
      # nothing, leaving the count NA rather than 0.
      n_authored <- NA_integer_
      asked <- FALSE
      for (term in .ai_author_queries(tool)) {
        hit <- tryCatch(io$search_hit(owner, name, term, search_delay),
                        error = function(e) list(date = NA_character_, unavailable = TRUE))
        if (isTRUE(hit$unavailable)) { unavailable <- unavailable + 1L; next }
        asked <- TRUE
        n_authored <- .ai_max_count(c(n_authored, .nn(hit$total_count, NA_integer_)))
        d <- .nn(hit$date, NA_character_)
        if (!is.na(d)) hits <- c(hits, d)
      }
      # A search that ran and matched nothing is a measured zero, and writing NA
      # there would claim nobody looked. A hit whose count did not come back is
      # not zero either: we are holding the commit that proves at least one.
      if (asked && is.na(n_authored)) n_authored <- if (length(hits)) 1L else 0L
      if (!length(hits)) {
        # Asked, and the answer was none. That is a measured zero and it belongs
        # on the tool's existing evidence rather than being dropped with the
        # onset: writing NA here would claim nobody looked, which is the
        # honest-NA rule broken in the direction people forget. No tier-A row is
        # added, because nothing was detected.
        if (asked) ev$authored_commits[ev$tool == tool] <- n_authored
        next
      }
      # The earliest across identities: a bot that changed login keeps its onset.
      commit_onsets <- rbind(commit_onsets, data.frame(tool = tool, tier = "A",
        first_seen_date = min(hits), confirmed = TRUE, stringsAsFactors = FALSE))
      extra_ev <- rbind(extra_ev, data.frame(tool = tool, tier = "A", marker = "A",
        agnostic = 0L, authored = 1L, authored_commits = n_authored,
        assisted_commits = NA_integer_, stringsAsFactors = FALSE))
    }
    # (2b) Tier-B commit trailers and Tier-C author suffixes. These were written into
    #      the ruleset and never called from any scan, so every published detection was
    #      a config marker and an AI co-author line was invisible. Each rule is searched
    #      literally, then verified against its real pattern: a verified hit carries an
    #      exact onset, an unverified one still counts as evidence but only dates a floor.
    #      Searched per repo because the flagged roster is what this pass walks.
    for (spec in c(lapply(AI_TRAILER_PATTERNS, function(r) list(rule = r, tier = "B")),
                   lapply(AI_AUTHOR_SUFFIXES,  function(r) list(rule = r, tier = "C")))) {
      q <- spec$rule$query
      if (is.null(q) || is.na(q) || !nzchar(q)) next
      hit <- tryCatch(io$search_hit(owner, name, q, search_delay),
                      error = function(e) list(date = NA_character_, unavailable = TRUE))
      # A refused question is not an absence of trailers. Count it, leave the repo
      # without a tier-B row, and record nothing either way: the alternative is what
      # happened on the first run, where throttling produced a confident zero across
      # the whole roster.
      if (isTRUE(hit$unavailable)) { unavailable <- unavailable + 1L; next }
      if (is.na(.nn(hit$date, NA_character_))) next
      v <- verify_search_hit(spec$rule, spec$tier, hit)
      # The count is kept only when the hit verifies against the rule's real
      # pattern. The query is deliberately fuzzy so the search will find the
      # trailer at all, which means an unverified hit is evidence the search
      # matched something we cannot vouch for, and its total_count would be
      # counting that too. A floor we cannot defend is worse than no number.
      n_assisted <- if (isTRUE(v$confirmed)) .nn(hit$total_count, NA_integer_) else NA_integer_
      # The page we already fetched names the model on three tools' trailers.
      # Only read it off a verified hit, for the same reason the count is: an
      # unverified hit matched something we cannot vouch for.
      if (isTRUE(v$confirmed) && !is.null(hit$items) && nrow(hit$items) > 0) {
        complete <- is.na(n_assisted) || n_assisted <= nrow(hit$items)
        mr <- build_ai_model_rows(rid, v$tool, hit$items, window_complete = complete)
        if (nrow(mr) > 0) model_rows[[length(model_rows) + 1L]] <- mr
      }
      commit_onsets <- rbind(commit_onsets, data.frame(tool = v$tool, tier = v$tier,
        first_seen_date = hit$date, confirmed = v$confirmed, stringsAsFactors = FALSE))
      extra_ev <- rbind(extra_ev, data.frame(tool = v$tool, tier = v$tier, marker = v$tier,
        agnostic = 0L, authored = 0L, authored_commits = NA_integer_,
        assisted_commits = as.integer(n_assisted), stringsAsFactors = FALSE))
    }

    full_ev <- rbind(ev, extra_ev)

    # (3) assemble + guard + collapse.
    onsets <- build_onset_map(full_ev, marker_dates, commit_onsets, mine$pr_onset_date[r],
                              exact_markers = exact_ignores)
    guarded <- apply_fork_guard(full_ev, isTRUE(mine$is_fork[r] == 1L), mine$parent[r], character(0))
    detail <- build_ai_detail(rid, guarded, onsets, today)
    if (nrow(detail) > 0) acc[[length(acc) + 1L]] <- detail
  }
  rows <- if (length(acc)) do.call(rbind, acc) else .ai_empty_signals()
  models_df <- if (length(model_rows)) do.call(rbind, model_rows) else .ai_empty_models()
  export_ai_shard(file.path(out_dir, sprintf("vcs-ai-shard-%d.db", i)), rows, models_df)
  if (skipped > 0L)
    message(sprintf("ai deep shard %d/%d: skipped %d repo(s) already confirmed today",
                    i, N, skipped))
  if (stopped_early)
    message(sprintf("ai deep shard %d/%d: PARTIAL, dispatch again to continue", i, N))
  if (nrow(models_df) > 0)
    message(sprintf("ai deep shard %d/%d: %d model row(s) across %d repo(s)",
                    i, N, nrow(models_df), length(unique(models_df$repo_id))))
  message(sprintf("ai deep shard %d/%d: %d onset detail rows", i, N, nrow(rows)))
  # Said out loud, because a run that could not ask is not a run that found nothing,
  # and the difference is invisible in the published table.
  if (unavailable > 0L) {
    message(sprintf(
      "ai deep shard %d/%d: WARNING %d commit search(es) were refused (rate limit or error); ",
      i, N, unavailable),
      "tiers A, B and C are UNDER-COUNTED for this shard, not absent")
  }
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

  # Every one of the detection bugs was visible here as a channel at exactly zero
  # across the roster, and nothing looked. This looks, per (tier, tool): a
  # tier-level check would have seen tier A's 103 detections and called it healthy
  # while four of its six identities had never produced one.
  roster_n <- tryCatch(
    DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM repos")$n[1], error = function(e) NA_integer_)
  canary_unexplained <- ai_canary_check(reduced, roster_n = roster_n)

  # The finding is published, not withheld. Failing before publish would hold
  # back the dev-tooling and summary data too, none of which is implicated by a
  # tool channel going quiet; a week of collateral staleness is a worse outcome
  # than a red build beside fresh data. The run still fails, at the end.
  silent_tbl <- ai_silent_channel_table(reduced)
  DBI::dbExecute(con, "DELETE FROM vcs_ai_silent_channels")
  if (nrow(silent_tbl) > 0)
    DBI::dbWriteTable(con, "vcs_ai_silent_channels", silent_tbl, append = TRUE)

  # Model rows, replaced wholesale from the shards that carry them. Not reduced
  # like onsets: a model tally describes the window that was examined this run,
  # and folding it into an older window would produce a count belonging to
  # neither. A repo not covered this run keeps its previous rows.
  model_parts <- lapply(parts, function(p) {
    pcon <- DBI::dbConnect(RSQLite::SQLite(), p)
    on.exit(DBI::dbDisconnect(pcon), add = TRUE)
    if (!DBI::dbExistsTable(pcon, "vcs_ai_models")) return(.ai_empty_models())
    DBI::dbReadTable(pcon, "vcs_ai_models")
  })
  models_in <- if (length(model_parts)) do.call(rbind, model_parts) else .ai_empty_models()
  if (nrow(models_in) > 0) {
    touched <- unique(models_in$repo_id)
    ph <- paste(rep("?", length(touched)), collapse = ",")
    DBI::dbExecute(con, sprintf("DELETE FROM vcs_ai_models WHERE repo_id IN (%s)", ph),
                   params = as.list(touched))
    DBI::dbWriteTable(con, "vcs_ai_models", models_in, append = TRUE)
  }
  message(sprintf("ai merge: %d model row(s) across %d repo(s)",
                  nrow(models_in), length(unique(models_in$repo_id))))

  # Republish the rule inventory so a consumer can state each tier's breadth
  # from data rather than asserting it.
  inv <- ai_rule_inventory()
  inv$ruleset_version <- AI_RULESET_VERSION
  DBI::dbExecute(con, "DELETE FROM vcs_ai_rule_inventory")
  DBI::dbWriteTable(con, "vcs_ai_rule_inventory", inv, append = TRUE)

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

  # Dev-tooling presence snapshot: union this dispatch's cheap shards and fold them OVER the prior
  # published snapshot (carried into con by seed_working_db), keeping the freshest row per repo_id
  # (incoming wins on a newer last_scanned). This is the spec's per-repo delete-by-repo_id-then-insert
  # overwrite, done NON-destructively: run_cheap can pause on GraphQL budget and emit a PARTIAL (or
  # empty) shard, so a whole-table wipe would drop every repo not scanned this dispatch. Folding
  # preserves un-scanned repos, exactly as ai_onset_reducer protects vcs_ai_signals. A presence
  # snapshot has no onset reduce and no node_id reconcile. Runs before publish() so it rides the
  # existing summary/recent embedding. Independent of the AI onset path above.
  dev_parts <- list.files(parts_dir, pattern = "^vcs-dev-tooling-.*\\.db$", full.names = TRUE)
  dev_list <- lapply(dev_parts, read_dev_tooling)
  dev_df <- if (length(dev_list)) do.call(rbind, dev_list) else .devtool_empty_shard()
  prior_dev <- if (DBI::dbExistsTable(con, "vcs_dev_tooling"))
    DBI::dbReadTable(con, "vcs_dev_tooling") else .devtool_empty_shard()
  merged_dev <- bind_dev_tooling(prior_dev, dev_df)
  # incoming-wins: order by (repo_id, last_scanned) ascending, keep the last (freshest) row per repo
  merged_dev <- merged_dev[order(merged_dev$repo_id, merged_dev$last_scanned), , drop = FALSE]
  merged_dev <- merged_dev[!duplicated(merged_dev$repo_id, fromLast = TRUE), , drop = FALSE]
  DBI::dbExecute(con, "DELETE FROM vcs_dev_tooling")
  if (nrow(merged_dev) > 0) DBI::dbWriteTable(con, "vcs_dev_tooling", merged_dev, append = TRUE)
  message(sprintf("ai merge: %d dev-tooling rows (%d incoming across %d shard(s))",
                  nrow(merged_dev), nrow(dev_df), length(dev_parts)))

  message(sprintf("ai merge: %d prior, %d incoming, %d reduced onset rows",
                  nrow(prior), nrow(incoming), nrow(reduced)))
  out <- publish(io, con, out_dir, tag = "current", source_kind = "live",
                 touched_years = character(0))

  # Raised after the data is out, so the alarm costs a red build and not a
  # week of stale dev-tooling rows.
  if (nrow(canary_unexplained) > 0) {
    stop(sprintf(paste0("AI detection canary: %d channel(s) detected nothing on the whole roster ",
                        "and are not recorded in AI_SILENT_CHANNELS_KNOWN: %s. ",
                        "Either the rule is broken or the zero is real; record which, with a date."),
                 nrow(canary_unexplained),
                 paste(canary_unexplained$tier, canary_unexplained$tool,
                       sep = "/", collapse = ", ")),
         call. = FALSE)
  }
  invisible(out)
}

# ---- CLI dispatch -----------------------------------------------------------
main <- function(mode, out_dir) {
  token <- Sys.getenv("VCS_SIGNALS_TOKEN")
  io <- list(
    graphql        = default_io(token)$graphql,
    search_hit     = function(owner, name, query, delay = SEARCH_DELAY_S)
                       search_earliest_commit_hit(token, owner, name, query, delay),
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
