#!/usr/bin/env Rscript
# scripts/ai_backfill.R - gated deep-scan AI-tooling-detection backfill for vcs-signals.
#
# Sub-commands wired together by CI (.github/workflows/ai-backfill.yml, the one-time full
# onset scan, and .github/workflows/ai-weekly.yml, the weekly incremental that swaps gate for
# gate-incremental):
#   enumerate -> full active github roster from the published summary's repos table (one job)
#   cheap     -> every repository's contents, recent pull requests and commits, and account
#                counts over one mod-N shard, written to the cheap partial (matrix job)
#   gate      -> union the cheap partials into the flagged roster and write the week's search
#                list, and on a full gate work within the campaign (one job)
#   gate-incremental -> write the same list without a campaign (the weekly gate used by
#                .github/workflows/ai-weekly.yml in place of gate)
#   deep      -> commit-history onset scan over one mod-N shard of the flagged roster,
#                build vcs_ai_signals detail rows (matrix job)
#   merge     -> reconcile node_id identity, reduce prior+incoming onsets, rebuild the
#                summary rollups, and republish (one job)
if (!exists("STARGAZER_PAGE"))       source("scripts/config.R")
if (!exists("ensure_series_schema")) source("scripts/helpers.R")
if (!exists("build_tree_query"))     source("scripts/github.R")
if (!exists("gh_release_exists"))    source("scripts/update.R")   # default_io, gh_release_*, seed_working_db
if (!exists("build_ai_detail"))      source("scripts/ai_signals.R")
if (!exists("url_points_into"))      source("scripts/dev_tooling.R")
if (!exists("write_roster"))         source("scripts/backfill.R") # shard_rows via helpers, roster idiom
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

AI_ROSTER_TABLE <- "roster"

# ---- roster IO --------------------------------------------------------------
# Read watermarks the cheap pass plans each repository's read from.
.AI_ROSTER_READ_COLS <- c("commits_read_on", "commits_read_through", "commits_ruleset",
                          "commits_history_complete", "prs_newest_created_at", "prs_walk_complete",
                          "prs_walk_started_on", "prs_walk_cursor")

write_ai_roster <- function(path, roster_df, roster_cran = NULL) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, sprintf("CREATE TABLE %s (
    repo_id TEXT PRIMARY KEY, owner TEXT NOT NULL, name TEXT NOT NULL,
    node_id TEXT, done INTEGER NOT NULL DEFAULT 0,
    commits_read_on TEXT, commits_read_through TEXT, commits_ruleset TEXT,
    commits_history_complete INTEGER, prs_newest_created_at TEXT, prs_walk_complete INTEGER,
    prs_walk_started_on TEXT, prs_walk_cursor TEXT)", AI_ROSTER_TABLE))
  cols <- intersect(c("repo_id", "owner", "name", "node_id", "done", .AI_ROSTER_READ_COLS), names(roster_df))
  if (nrow(roster_df) > 0) DBI::dbWriteTable(con, AI_ROSTER_TABLE, roster_df[cols], append = TRUE)
  # Rebuilt every week from one CRAN read, so no week depends on an earlier copy.
  DBI::dbExecute(con, "CREATE TABLE roster_cran (repo_id TEXT NOT NULL, package TEXT NOT NULL,
    cran_version TEXT NOT NULL, PRIMARY KEY (repo_id, package))")
  if (!is.null(roster_cran) && nrow(roster_cran) > 0)
    DBI::dbWriteTable(con, "roster_cran", roster_cran[c("repo_id", "package", "cran_version")], append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

load_ai_roster <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbReadTable(con, AI_ROSTER_TABLE)
}

load_roster_cran <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (DBI::dbExistsTable(con, "roster_cran")) DBI::dbReadTable(con, "roster_cran")
  else data.frame(repo_id = character(0), package = character(0), cran_version = character(0),
                  stringsAsFactors = FALSE)
}

#' Each roster repository's CRAN packages with today's CRAN version. A failed CRAN read is
#' logged and gives no rows, which only leaves the version comparison empty this week.
build_roster_cran <- function(io, repo_packages, roster_ids) {
  empty <- data.frame(repo_id = character(0), package = character(0), cran_version = character(0),
                      stringsAsFactors = FALSE)
  cran <- tryCatch(io$cran_packages(), error = function(e) {
    message("ai enumerate: the CRAN package list could not be read (", conditionMessage(e),
            "), so versions are not compared this week")
    NULL })
  if (is.null(cran) || !nrow(cran)) return(empty)
  rp <- repo_packages[repo_packages$repo_id %in% roster_ids, c("repo_id", "package"), drop = FALSE]
  out <- merge(rp, data.frame(package = cran$Package, cran_version = cran$Version, stringsAsFactors = FALSE),
               by = "package")
  out <- out[!duplicated(out[c("repo_id", "package")]), c("repo_id", "package", "cran_version"), drop = FALSE]
  if (nrow(out)) out else empty
}

# ---- contents query canary ---------------------------------------------------
.canary_repos <- function(slugs)
  data.frame(repo_id = paste0("github.com/", tolower(slugs)), owner = sub("/.*$", "", slugs),
             name = sub("^[^/]*/", "", slugs), stringsAsFactors = FALSE)

#' One contents document over every TREE_QUERY_CANARY repository, since the community-field
#' fault shows only on multi-repository batches. Stops the run when a floor has no candidate.
tree_query_canary <- function(io) {
  own <- TREE_QUERY_CANARY$own_community
  inherit <- TREE_QUERY_CANARY$inherited_pr_template
  slugs <- c(own, inherit)
  repos <- .canary_repos(slugs)
  fail <- function(what, values) stop(sprintf(paste0(
    "contents query canary: %s. %s. Run Rscript scripts/ai_backfill.R canary with a token and ",
    "read each candidate's value. If GitHub changed, fix the query or the parser before the next ",
    "pass. If the candidates changed their own files, replace them in TREE_QUERY_CANARY with ",
    "repositories that meet the floor today and re-run the enumerate job."), what, values),
    call. = FALSE)
  ask <- function() tryCatch(io$graphql(build_tree_query(repos)), error = function(e) list(.err = conditionMessage(e)))
  res <- ask()
  # Only a request that threw (a 502 or a timeout) is read again. A reply GitHub answered is
  # final, so the field-order fault stops the pass on its first reply.
  if (!is.null(res$.err)) {
    (if (is.function(io$sleep)) io$sleep else Sys.sleep)(AI_BATCH_RETRY_WAIT_S)
    res <- ask()
  }
  if (!is.null(res$.err)) fail("the query did not return", res$.err)
  if (is.null(res$data)) fail("GitHub returned no data", .fetch_first_error(res))
  if (length(res$errors) && !errors_are_alias_not_found(res$errors))
    fail("GitHub rejected the query", .fetch_first_error(res))
  if (all(vapply(sprintf("r%d", seq_along(slugs) - 1L), function(a) is.null(res$data[[a]]), logical(1))))
    fail("every candidate came back empty", paste(slugs, collapse = ", "))
  parsed <- parse_tree_markers(res, repos)
  el <- function(s) parsed[[paste0("github.com/", tolower(s))]]
  slug_of <- function(s) { nwo <- el(s)$name_with_owner %||% NA_character_; if (is.na(nwo)) s else nwo }
  own_ok <- vapply(own, function(s) url_points_into(el(s)$coc_url %||% NA_character_, slug_of(s)) &&
                     url_points_into(el(s)$contributing_url %||% NA_character_, slug_of(s)), logical(1))
  inherit_ok <- vapply(inherit, function(s) {
    held <- tolower(el(s)$pr_templates$repository %||% character(0))
    any(held == tolower(paste0(sub("/.*$", "", slug_of(s)), "/.github")), na.rm = TRUE)
  }, logical(1))
  shown <- function(x) if (is.null(x) || !length(x) || all(is.na(x))) "none" else paste(x, collapse = " ")
  if (!any(own_ok))
    fail("no own_community candidate returned a code of conduct and a contributing guide from its own repository",
         paste(vapply(own, function(s) sprintf("%s code of conduct %s, contributing guide %s", s,
                 shown(el(s)$coc_url), shown(el(s)$contributing_url)), character(1)), collapse = " | "))
  if (!any(inherit_ok))
    fail("no inherited_pr_template candidate reported its owner's .github pull request template",
         paste(vapply(inherit, function(s) sprintf("%s template from %s", s,
                 shown(el(s)$pr_templates$repository)), character(1)), collapse = " | "))
  message(sprintf("contents query canary: passed, own_community %d of %d, inherited_pr_template %d of %d",
                  sum(own_ok), length(own), sum(inherit_ok), length(inherit)))
  invisible(TRUE)
}

# ---- weekly document canary and read state ----------------------------------
.ai_canary_repos <- function(slugs)
  data.frame(repo_id = paste0("github.com/", slugs), owner = sub("/.*$", "", slugs),
             name = sub("^[^/]*/", "", slugs), stringsAsFactors = FALSE)

#' Send the weekly documents for repositories whose answers are known, and stop the run
#' when GitHub no longer returns them. The contents check comes first.
ai_query_canary <- function(io) {
  tree_query_canary(io)
  cfg <- AI_QUERY_CANARY
  fail <- function(fmt, ...) stop(sprintf(paste0("AI query canary: ", fmt), ...), call. = FALSE)
  ask <- function(q, label) {
    get <- function() tryCatch(io$graphql(q), error = function(e) list(.err = conditionMessage(e)))
    res <- get()
    # As in the contents check, only a request that threw (a 502 or a timeout) is sent again.
    if (!is.null(res$.err)) {
      (if (is.function(io$sleep)) io$sleep else Sys.sleep)(AI_BATCH_RETRY_WAIT_S)
      res <- get()
    }
    if (!is.null(res$.err)) fail("the %s document failed: %s", label, res$.err)
    if (is.null(res$data)) fail("the %s document returned no data: %s", label, .fetch_first_error(res))
    if (!is.null(res$errors) && !errors_are_alias_not_found(res$errors))
      fail("the %s document returned an error: %s", label, .fetch_first_error(res))
    res
  }
  acc_repos <- .ai_canary_repos(cfg$accounts)
  counts <- parse_account_counts(ask(build_account_count_query(acc_repos), "account-count"), acc_repos)
  for (rid in acc_repos$repo_id)
    if (is.null(counts[[rid]])) fail("%s came back empty in the account-count document", rid)
  n <- function(slug, tool) {
    d <- counts[[paste0("github.com/", slug)]]; v <- d$commits[d$tool == tool]
    if (length(v)) v else 0L
  }
  if (n("ss3sim/ss3sim", "claude") != 0L)
    fail("ss3sim reads %d commits by Claude's accounts where 0 is right: a github-actions address reached the filter",
         n("ss3sim/ss3sim", "claude"))
  if (n("ss3sim/ss3sim", "amazonq") < 2L)
    fail("ss3sim reads %d commits by Amazon Q's account, at least 2 expected", n("ss3sim/ss3sim", "amazonq"))
  if (n("johnpaulgosling/addivortes", "cursor") < cfg$addivortes_cursor_floor)
    fail(paste0("addivortes reads %d commits by Cursor's accounts, at least %d expected: ",
                "the account filter may no longer count cursoragent@cursor.com's commits"),
         n("johnpaulgosling/addivortes", "cursor"), cfg$addivortes_cursor_floor)
  act_repos <- .ai_canary_repos(cfg$activity)
  act <- parse_activity(ask(build_activity_query(act_repos), "activity"), act_repos)
  for (rid in act_repos$repo_id) {
    a <- act[[rid]]
    if (is.null(a)) fail("%s came back empty in the activity document", rid)
    if (!all(c("number", "created_at", "login", "association", "cross_repo", "head_ref") %in% names(a$prs)) ||
        !all(c("oid", "committed_at", "message", "author_name", "author_email") %in% names(a$commits)))
      fail("%s: the activity document's pull request or commit frame lacks a column", rid)
  }
  # Floors that only grow: the read takes pull requests in any state, #49 among them, and
  # the newest commits with no since, so an empty list is a fault in the reply.
  rows <- function(slug, part) NROW(act[[paste0("github.com/", slug)]][[part]])
  if (rows("ericrayanderson/shinyglass", "prs") < 1L)
    fail("ericrayanderson/shinyglass read no pull requests in the activity document, at least 1 expected")
  if (rows("ss3sim/ss3sim", "commits") < 1L)
    fail("ss3sim/ss3sim read no commits in the activity document, at least 1 expected")
  fixed <- ask(build_fixed_object_query(cfg$prs, cfg$commits), "fixed-object")$data
  pr <- function(k) fixed[[sprintf("p%d", k)]]$pullRequest
  cm <- function(k) fixed[[sprintf("c%d", k)]]$object
  for (k in seq_along(cfg$prs)) if (is.null(pr(k - 1L)))
    fail("%s#%s came back empty in the fixed-object document", cfg$prs[[k]][1], cfg$prs[[k]][2])
  for (k in seq_along(cfg$commits)) if (is.null(cm(k - 1L)))
    fail("%s@%s came back empty in the fixed-object document", cfg$commits[[k]][1], substr(cfg$commits[[k]][2], 1, 7))
  sg <- classify_prs(.ai_pr_nodes_frame(list(pr(0L))))
  if (!(nrow(sg) == 1L && sg$tool == "cursor" && sg$code == "PB" && sg$role == "authoring"))
    fail("ericrayanderson/shinyglass #49 no longer reads as a pull request Cursor wrote for its maintainer")
  na <- assemble_repo_evidence(list(), list(prs = .ai_pr_nodes_frame(list(pr(1L)))))
  if (!any(na$role == "outside" & na$tool == "cursor"))
    fail("apache/arrow-nanoarrow #927 no longer reads as a Cursor pull request from outside the project")
  if (nrow(build_cheap_rows(cbind(repo_id = "github.com/apache/arrow-nanoarrow", na), "canary")) > 0L)
    fail("apache/arrow-nanoarrow #927 would count as the package's own Cursor use")
  if (!("msg.cursor.made-with" %in% match_commit_findings(.ai_commit_nodes_frame(list(cm(0L))))$rule_key))
    fail("xrobin/pROC fe5c63c no longer matches its Made-with: Cursor line")
  bg <- unique(match_commit_findings(.ai_commit_nodes_frame(list(cm(1L))))$tool)
  if (!identical(bg, "antigravity"))
    fail("alyssafrazee/ballgown ab1da7b names %s, where Antigravity alone is right",
         if (length(bg)) paste(bg, collapse = ", ") else "no tool")
  message("AI query canary: passed")
  invisible(TRUE)
}

#' pipeline_state, which lives in the recent shard. An empty vector when there is no
#' release yet; a stop when there is one and its shard cannot be read.
.ai_read_pipeline_state <- function(io, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  if (!isTRUE(io$download("vcs-signals-recent.db", dir))) {
    if (isTRUE(io$release_exists()))
      stop("could not download vcs-signals-recent.db to read the pipeline state; stopping rather than ",
           "taking lost state for a first run", call. = FALSE)
    return(character(0))
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, "vcs-signals-recent.db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, "pipeline_state")) return(character(0))
  st <- DBI::dbReadTable(con, "pipeline_state")
  stats::setNames(st$value, st$key)
}

#' Once the state tables have been published, a summary without them is a lost table,
#' never a first run: a reset would re-read everything and lose the REST-only counts.
.ai_state_guard <- function(con, state) {
  since <- unname(state["ai_state_tables_since"])
  if (!length(since) || is.na(since)) return(invisible(TRUE))
  for (t in c("vcs_ai_repo_reads", "vcs_ai_search_log", "vcs_ai_account_counts")) {
    n <- if (DBI::dbExistsTable(con, t)) DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) AS n FROM "%s"', t))$n else 0L
    if (n == 0L)
      stop(sprintf(paste0("the published summary has no %s rows although the weekly AI read has kept them ",
                          "since %s; stopping so a lost table is never read as a first run"), t, since), call. = FALSE)
  }
  invisible(TRUE)
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
  # Before any read: a broken weekly document stops the pass here, not twelve shards later.
  ai_query_canary(io)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  if (!isTRUE(io$download("vcs-signals-summary.db", out_dir)))
    stop("could not download vcs-signals-summary.db from the published release; nothing to enumerate")
  summary_path <- file.path(out_dir, "vcs-signals-summary.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), summary_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ai_state_guard(con, .ai_read_pipeline_state(io, file.path(out_dir, "_state")))
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

  # Last week's read watermarks, so each repository's read can be planned.
  if (DBI::dbExistsTable(con, "vcs_ai_repo_reads")) {
    rd <- DBI::dbReadTable(con, "vcs_ai_repo_reads")
    m <- match(roster$repo_id, rd$repo_id)
    for (cn in .AI_ROSTER_READ_COLS) roster[[cn]] <- rd[[cn]][m]
  }
  rp <- DBI::dbGetQuery(con, "SELECT repo_id, package FROM repo_packages WHERE origin = 'cran'")
  roster_cran <- build_roster_cran(io, rp, roster$repo_id)
  message(sprintf("ai enumerate: %d active github repos, %d CRAN package versions to compare",
                  nrow(roster), nrow(roster_cran)))
  write_ai_roster(file.path(out_dir, "vcs-ai-roster.db"), roster, roster_cran)
}

# ---- flagged partial IO -----------------------------------------------------
.ai_empty_flagged <- function()
  data.frame(repo_id = character(), owner = character(), name = character(),
             node_id = character(), is_fork = integer(), parent = character(),
             pr_onset_date = character(), stringsAsFactors = FALSE)
.ai_empty_ev <- function()
  data.frame(repo_id = character(), tool = character(), tier = character(),
             marker = character(), agnostic = integer(), stringsAsFactors = FALSE)

write_flagged_partial <- function(path, flagged_df, evidence_df, work = NULL, campaign = NULL) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbWriteTable(con, "flagged", .ai_bind_like(.ai_empty_flagged(), list(flagged_df)), overwrite = TRUE)
  DBI::dbWriteTable(con, "evidence", .ai_bind_like(cbind(repo_id = character(0), .ai_empty_found()),
                                                   list(evidence_df)), overwrite = TRUE)
  # A roster written without a list keeps no work table, so the search pass can tell it
  # from a week with nothing to do.
  if (!is.null(work))
    DBI::dbWriteTable(con, "work", .ai_bind_like(.ai_empty_work(), list(work)), overwrite = TRUE)
  if (!is.null(campaign))
    DBI::dbWriteTable(con, "campaign", .ai_bind_like(data.frame(since = character(), stringsAsFactors = FALSE),
                                                     list(campaign)), overwrite = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

# One table of a partial, or NULL when the partial predates it.
.ai_part_table <- function(path, table) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (DBI::dbExistsTable(con, table)) DBI::dbReadTable(con, table) else NULL
}

read_flagged <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  get <- function(t, empty) if (DBI::dbExistsTable(con, t)) DBI::dbReadTable(con, t) else empty
  # NULL work means a roster written before the list existed, not an empty week.
  list(flagged  = get("flagged", .ai_empty_flagged()),
       evidence = get("evidence", .ai_empty_ev()),
       work     = get("work", NULL),
       campaign = get("campaign", data.frame(since = character(), stringsAsFactors = FALSE)))
}

# ---- dev-tooling partial IO -------------------------------------------------
# A stateless presence snapshot, written as a SEPARATE shard from the AI flagged/evidence
# partials (those are scoped to the AI-flagged subset by the downstream gate/deep pipeline).
.devtool_empty_shard <- function() {
  out <- data.frame(repo_id = character(0), last_scanned = character(0), ruleset_version = character(0),
                    stringsAsFactors = FALSE)
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
  want <- c("repo_id", "last_scanned", "ruleset_version", dev_tooling_columns())
  cols <- c(want, setdiff(c(names(prior), names(incoming)), want))
  fill <- function(d) {
    for (cn in setdiff(cols, names(d))) d[[cn]] <- NA
    d[, cols, drop = FALSE]
  }
  rbind(fill(prior), fill(incoming))
}

write_dev_tooling_partial <- function(path, dev_df, failures = NULL) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, dev_tooling_create_sql())
  if (nrow(dev_df) > 0) DBI::dbWriteTable(con, "vcs_dev_tooling", dev_df, append = TRUE)
  # Rides the dev-tooling partial because every merge job already downloads it.
  DBI::dbExecute(con, "CREATE TABLE failures (repo_id TEXT NOT NULL, query TEXT NOT NULL,
    error TEXT, failed_at TEXT)")
  if (!is.null(failures) && nrow(failures) > 0) DBI::dbWriteTable(con, "failures", failures, append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

read_dev_tooling <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (DBI::dbExistsTable(con, "vcs_dev_tooling")) DBI::dbReadTable(con, "vcs_dev_tooling")
  else .devtool_empty_shard()
}

read_scan_failures <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (DBI::dbExistsTable(con, "failures")) DBI::dbReadTable(con, "failures") else .fetch_failed_frame()
}

#' The merge's stop message when distinct failed repositories exceed the roster share, else NULL.
scan_failure_stop_message <- function(fails, roster_n) {
  n <- length(unique(fails$repo_id))
  denom <- if (is.na(roster_n) || roster_n < 1L) 1L else as.integer(roster_n)
  if (n == 0L || n <= AI_SCAN_FAILURE_MAX_SHARE * denom) return(NULL)
  sprintf(paste0("ai merge: %d of %d roster repositories failed a read this week, above the %s%% ",
                 "the merge accepts. First: %s. First message: %s"),
          n, denom, format(100 * AI_SCAN_FAILURE_MAX_SHARE),
          paste(utils::head(unique(fails$repo_id), 5L), collapse = ", "), fails$error[1])
}

#' TRUE when a shard's contents failures say the query itself is broken.
contents_shard_stops <- function(n_failed, attempted)
  n_failed >= TREE_DROP_MIN && n_failed >= TREE_DROP_MAX_SHARE * attempted

# ---- cheap pass -------------------------------------------------------------
# One repository's vcs_dev_tooling row, or NULL when its contents read failed or it is gone.
.ai_dev_tooling_row <- function(tree, rid, today, cran_links) {
  if (is.null(tree) || is.na(tree$is_fork)) return(NULL)
  dv <- classify_dev_tooling(tree$root_entries, tree$github_entries, repo = tree)
  bad <- attr(dv, "rbuildignore_bad_lines")
  cmp <- compare_repo_version(dv$repo_desc_package, dv$repo_desc_version,
                              cran_links[cran_links$repo_id == rid, c("package", "cran_version")])
  dv$cran_version_at_scan <- cmp$cran_version_at_scan
  dv$repo_version_vs_cran <- cmp$repo_version_vs_cran
  dv$repo_id <- rid
  dv$last_scanned <- today
  dv$ruleset_version <- DEV_TOOLING_RULESET_VERSION
  out <- dv[c("repo_id", "last_scanned", "ruleset_version", dev_tooling_columns())]
  attr(out, "rbuildignore_bad_lines") <- bad
  out
}

# Stops the shard when its contents failures say the query itself is broken.
.ai_shard_stop <- function(failed_df, scanned, i, N) {
  n_contents <- sum(failed_df$query == "contents")
  if (!contents_shard_stops(n_contents, scanned)) return(invisible(FALSE))
  first <- failed_df[failed_df$query == "contents", , drop = FALSE]
  stop(sprintf(paste0("ai cheap shard %d/%d: the contents read failed for %d of %d repositories, ",
                      "so the query itself is broken. First: %s. First message: %s"),
               i, N, n_contents, scanned, paste(utils::head(first$repo_id, 5L), collapse = ", "),
               first$error[1]), call. = FALSE)
}

# Every table the cheap partial carries, empty, in the shape the merge reads.
.ai_cheap_tables <- function() list(
  flagged = .ai_empty_flagged(),
  evidence = cbind(repo_id = character(0), .ai_empty_found()),
  repo_reads = cbind(.ai_empty_reads(), reached_first = integer()),
  account_counts = .ai_empty_counts(), review = .ai_empty_review(), outside_prs = .ai_empty_outside(),
  models = cbind(.ai_empty_models(), mode = character(), read_after = character()),
  search_log = cbind(.ai_empty_log(), mode = character(), read_after = character()))

write_cheap_partial <- function(path, tables) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  proto <- .ai_cheap_tables()
  for (nm in names(proto))
    DBI::dbWriteTable(con, nm, .ai_bind_like(proto[[nm]], list(tables[[nm]])), overwrite = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

# One document over a slice of the roster; every failure lands in sink$failures. A
# response the parser cannot read fails the slice for this document only.
.ai_fetch_doc <- function(io, repos, label, fetch, breaker, sink) {
  if (!nrow(repos)) return(list())
  got <- tryCatch(fetch(io, repos, breaker), error = function(e) list(results = list(),
    failed = .fetch_failed_frame(repos$repo_id, label, substr(conditionMessage(e), 1L, 200L), .utc_now())))
  if (!is.null(got$failed) && nrow(got$failed)) sink$failures[[length(sink$failures) + 1L]] <- got$failed
  got$results %||% list()
}

# Follow-up commit pages for the repositories whose read needs them. A repository whose
# page fails drops its whole activity read, so its watermark stays where it was.
.ai_follow_commits <- function(io, repos, plans, act, breaker, sink) {
  st <- list()
  for (r in seq_len(nrow(repos))) {
    a <- act[[repos$repo_id[r]]]
    if (is.null(a)) next
    st[[repos$repo_id[r]]] <- list(commits = a$commits, has_next = a$commits_has_next,
                                   cursor = a$commits_end_cursor, pages = 0L, plan = plans[[r]])
  }
  repeat {
    due <- names(st)[vapply(st, function(s)
      more_commit_pages(s$plan, .ai_earliest_chr(s$commits$committed_at), s$has_next, s$pages), logical(1))]
    if (!length(due)) break
    sub <- repos[match(due, repos$repo_id), c("repo_id", "owner", "name"), drop = FALSE]
    sub$after <- vapply(due, function(k) as.character(st[[k]]$cursor), character(1))
    sub$since <- vapply(due, function(k) as.character(st[[k]]$plan$since), character(1))
    got <- .ai_fetch_doc(io, sub, "activity", function(io, rp, b)
      fetch_aliased(io, rp, AI_COMMIT_PAGE_BATCH, build_commit_page_query, parse_commit_pages, "activity", b),
      breaker, sink)
    for (k in due) {
      pg <- got[[k]]
      if (is.null(pg)) { st[[k]] <- NULL; next }
      st[[k]]$commits <- rbind(st[[k]]$commits, pg$commits)
      st[[k]]$has_next <- pg$has_next; st[[k]]$cursor <- pg$end_cursor; st[[k]]$pages <- st[[k]]$pages + 1L
    }
  }
  st
}

# The one-off walk through older pull requests, and weekly catch-up pages, within the
# shard's point budget. A failed page keeps its cursor for next week.
.ai_walk_prs <- function(io, repos, pr_plans, breaker, sink, budget) {
  w <- list()
  for (k in names(pr_plans)) if (isTRUE(pr_plans[[k]]$walk))
    w[[k]] <- list(prs = .ai_pr_nodes_frame(list()), has_next = TRUE, cursor = pr_plans[[k]]$after,
                   reached_stop = FALSE, plan = pr_plans[[k]])
  repeat {
    due <- names(w)[vapply(w, function(x) !x$reached_stop && !isTRUE(x$failed) &&
                             more_pr_pages(x$plan, .ai_earliest_chr(x$prs$created_at), x$has_next, budget$points),
                           logical(1))]
    if (!length(due)) break
    for (grp in unname(chunk(due, AI_PR_WALK_BATCH))) {
      if (budget$points <= 0) break
      sub <- repos[match(grp, repos$repo_id), c("repo_id", "owner", "name"), drop = FALSE]
      sub$after <- vapply(grp, function(k) as.character(w[[k]]$cursor), character(1))
      got <- .ai_fetch_doc(io, sub, "walk", function(io, rp, b)
        fetch_aliased(io, rp, AI_PR_WALK_BATCH, build_pr_walk_query, parse_pr_walk, "walk", b), breaker, sink)
      budget$points <- budget$points - 1L
      for (k in grp) {
        pg <- got[[k]]
        if (is.null(pg)) { w[[k]]$failed <- TRUE; next }
        w[[k]]$prs <- rbind(w[[k]]$prs, pg$prs)
        w[[k]]$has_next <- pg$has_next; w[[k]]$cursor <- pg$end_cursor
        # The same stop more_pr_pages reads, so a catch-up ending on the stored pull request is done.
        w[[k]]$reached_stop <- any(pg$prs$created_at <= w[[k]]$plan$stop_before, na.rm = TRUE)
      }
    }
  }
  w
}

#' The weekly read of one mod-N shard: every repository's contents, newest pull requests
#' and commits, and its tools' account counts, matched locally, with follow-up pages and
#' the pull request walk. Everything the next run and the merge need is written to the
#' cheap partial; a repository whose read failed keeps last week's state.
run_cheap <- function(io, out_dir, roster_path, i, N, batch_size = TIER_D_BATCH) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  roster <- load_ai_roster(roster_path)
  cran_links <- load_roster_cran(roster_path)
  mine <- roster[shard_rows(nrow(roster), i, N), , drop = FALSE]
  message(sprintf("ai cheap shard %d/%d: %d of %d repos", i, N, nrow(mine), nrow(roster)))
  today <- format(Sys.Date())
  # One breaker per document for the whole shard, so a fault repeated across chunks trips it.
  brk <- list(contents = new_fetch_breaker(limit = AI_BREAKER_LIMIT),
              activity = new_fetch_breaker(limit = AI_BREAKER_LIMIT),
              accounts = new_fetch_breaker(limit = AI_BREAKER_LIMIT),
              walk = new_fetch_breaker(limit = AI_BREAKER_LIMIT))
  proto <- .ai_cheap_tables()
  sink <- new.env()
  for (nm in c(names(proto), "dev", "failures")) sink[[nm]] <- list()
  budget <- new.env(); budget$points <- AI_PR_WALK_POINTS
  scanned <- 0L; bad_rbi_lines <- 0L; capped <- 0L
  put <- function(nm, df) if (!is.null(df) && nrow(df)) sink[[nm]][[length(sink[[nm]]) + 1L]] <- df
  for (idx in unname(chunk(seq_len(nrow(mine)), batch_size))) {
    rl <- graphql_rate_remaining(io)
    if (rl < AI_POINT_RESERVE) {
      message(sprintf("ai cheap shard %d/%d: graphql rate remaining (%s) below reserve (%d); pausing after %d of %d repos",
                      i, N, rl, AI_POINT_RESERVE, scanned, nrow(mine)))
      break
    }
    repos <- mine[idx, , drop = FALSE]
    plans <- lapply(seq_len(nrow(repos)), function(r) plan_commit_read(repos[r, , drop = FALSE]))
    repos$since <- vapply(plans, function(p) as.character(p$since), character(1))
    n_failed_before <- length(sink$failures)
    trees <- .ai_fetch_doc(io, repos, "contents", function(io, rp, b) fetch_tree_markers(io, rp, batch_size, b),
                           brk$contents, sink)
    act <- .ai_fetch_doc(io, repos, "activity", fetch_activity, brk$activity, sink)
    accts <- .ai_fetch_doc(io, repos, "accounts", fetch_account_counts, brk$accounts, sink)
    cm <- .ai_follow_commits(io, repos, plans, act, brk$activity, sink)
    pr_plans <- stats::setNames(lapply(repos$repo_id, function(rid)
      if (rid %in% names(cm)) plan_pr_pages(repos[repos$repo_id == rid, , drop = FALSE], act[[rid]]) else list(walk = FALSE)),
      repos$repo_id)
    walks <- .ai_walk_prs(io, repos, pr_plans, brk$walk, sink, budget)
    # Only this chunk's repositories can have failed in this chunk.
    failed_now <- .ai_bind_like(.fetch_failed_frame(),
                                utils::tail(sink$failures, length(sink$failures) - n_failed_before))
    for (r in seq_len(nrow(repos))) {
      rid <- repos$repo_id[r]; prev <- repos[r, , drop = FALSE]
      tree <- trees[[rid]]; cnt <- if (rid %in% names(accts)) accts[[rid]] else NULL
      dv <- .ai_dev_tooling_row(tree, rid, today, cran_links)
      if (!is.null(dv)) { bad_rbi_lines <- bad_rbi_lines + attr(dv, "rbuildignore_bad_lines"); put("dev", dv) }
      read_row <- data.frame(repo_id = rid, stringsAsFactors = FALSE)
      activity <- NULL; whole <- FALSE
      if (rid %in% names(cm)) {
        a <- act[[rid]]; c_st <- cm[[rid]]; w <- walks[[rid]]
        st <- next_commit_read_state(prev, plans[[r]], c_st$commits, c_st$has_next, today)
        capped <- capped + as.integer(st$commits_window_complete == 0L)
        walked <- if (is.null(w)) NULL else list(has_next = w$has_next, end_cursor = w$cursor, reached_stop = w$reached_stop)
        ps <- next_pr_read_state(prev, a, if (is.null(w)) list(walk = FALSE) else w$plan, walked, today)
        read_row <- cbind(read_row, as.data.frame(c(st, ps), stringsAsFactors = FALSE))
        prs <- if (is.null(w)) a$prs else rbind(a$prs, w$prs)
        activity <- list(prs = prs, commits = c_st$commits)
        whole <- isTRUE(st$commits_history_complete == 1L)
        mode <- read_count_mode(plans[[r]], st)
        after <- if (identical(mode, "add")) as.character(.ai_col1(prev, "commits_read_through")) else NA_character_
        hits <- match_commit_findings(c_st$commits)
        put("search_log", read_log_rows(hits, rid, today, mode, after))
        mr <- read_model_rows(rid, c_st$commits, hits, mode, after)
        if (nrow(mr)) { mr$repo_id <- rid; put("models", mr) }
        put("outside_prs", outside_pr_rows(classify_prs(prs), rid, today))
      }
      if (!is.null(cnt)) {
        read_row$accounts_counted_on <- today
        if (nrow(cnt)) put("account_counts", data.frame(repo_id = rid, tool = cnt$tool, identity_set = "graphql",
          commits = cnt$commits, newest_commit_date = cnt$newest_commit_date, measured_on = today,
          stringsAsFactors = FALSE))
      }
      mine_failed <- failed_now[failed_now$repo_id == rid, , drop = FALSE]
      if (nrow(mine_failed)) {
        read_row$last_failed_on <- today
        read_row$last_failure <- substr(paste0(mine_failed$query[1], ": ", mine_failed$error[1]), 1, 200)
      }
      put("repo_reads", read_row)
      if (is.null(tree) && is.null(activity) && is.null(cnt)) next
      found <- assemble_repo_evidence(tree, activity, cnt, scanned_on = today, whole_history = whole)
      put("review", review_rows(found, rid, today))
      if (!repo_has_ai_signal(found)) next
      put("flagged", data.frame(repo_id = rid, owner = repos$owner[r], name = repos$name[r],
        node_id = repos$node_id[r], is_fork = as.integer(isTRUE(tree$is_fork)),
        parent = if (is.null(tree)) NA_character_ else (tree$parent %||% NA_character_),
        pr_onset_date = earliest_agent_pr_date(activity), stringsAsFactors = FALSE))
      put("evidence", cbind(repo_id = rid, found))
    }
    scanned <- scanned + nrow(repos)
  }
  tables <- lapply(stats::setNames(names(proto), names(proto)), function(nm) .ai_bind_like(proto[[nm]], sink[[nm]]))
  write_cheap_partial(file.path(out_dir, sprintf("vcs-ai-cheap-%d.db", i)), tables)
  dev_df <- if (length(sink$dev)) do.call(rbind, sink$dev) else .devtool_empty_shard()
  # The failure frame rides the dev-tooling partial, which every merge job downloads.
  f <- .ai_bind_like(.fetch_failed_frame(), sink$failures)
  write_dev_tooling_partial(file.path(out_dir, sprintf("vcs-dev-tooling-%d.db", i)), dev_df, f)
  message(sprintf("ai cheap shard %d/%d: %d flagged repos, %d dev-tooling rows, %d walk queries left",
                  i, N, nrow(tables$flagged), nrow(dev_df), budget$points))
  message(sprintf(paste0("ai cheap shard %d/%d: %d repositories failed (contents %d, activity %d, accounts %d, ",
                         "walk %d), %d not read this week, %d commit window(s) stopped at the page cap"),
                  i, N, length(unique(f$repo_id)), sum(f$query == "contents"), sum(f$query == "activity"),
                  sum(f$query == "accounts"), sum(f$query == "walk"), nrow(mine) - scanned, capped))
  if (bad_rbi_lines > 0L)
    message(sprintf("ai cheap shard %d/%d: %d .Rbuildignore line(s) did not compile and were skipped",
                    i, N, bad_rbi_lines))
  .ai_shard_stop(f, scanned, i, N)
}

# ---- gate -------------------------------------------------------------------
#' The published tables the gate plans from. A release whose summary cannot be read stops
#' the gate: planning from nothing would put every repository back on the list.
.ai_read_published_detail <- function(io, out_dir) {
  empty <- list(signals = .ai_empty_signals(), search_log = .ai_empty_log(),
                repo_reads = .ai_empty_reads(), account_counts = .ai_empty_counts())
  if (!isTRUE(io$download("vcs-signals-summary.db", out_dir))) {
    if (isTRUE(io$release_exists()))
      stop("release 'current' exists but vcs-signals-summary.db could not be downloaded; the gate ",
           "stops rather than plan the week from nothing", call. = FALSE)
    return(empty)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, "vcs-signals-summary.db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  get <- function(t, e) if (DBI::dbExistsTable(con, t)) .ai_bind_like(e, list(DBI::dbReadTable(con, t))) else e
  list(signals = get("vcs_ai_signals", empty$signals), search_log = get("vcs_ai_search_log", empty$search_log),
       repo_reads = get("vcs_ai_repo_reads", empty$repo_reads),
       account_counts = get("vcs_ai_account_counts", empty$account_counts))
}

#' Union the cheap partials into the flagged roster and write the week's search work.
#' A full gate works on a campaign: the stored one, or one dated today.
run_gate <- function(io, out_dir, parts_dir, full = TRUE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  parts <- list.files(parts_dir, pattern = "^vcs-ai-cheap-.*\\.db$", full.names = TRUE)
  fr <- lapply(parts, read_flagged)
  flagged_df <- .ai_bind_like(.ai_empty_flagged(), lapply(fr, `[[`, "flagged"))
  flagged_df <- flagged_df[!duplicated(flagged_df$repo_id), , drop = FALSE]
  ev_df <- .ai_bind_like(cbind(repo_id = character(0), .ai_empty_found()), lapply(fr, `[[`, "evidence"))
  # rule_key keeps the commits of a REST-only address apart from those of the GraphQL addresses.
  ev_df <- ev_df[!duplicated(paste(ev_df$repo_id, ev_df$tool, ev_df$tier, ev_df$marker, ev_df$role,
                                   ev_df$rule_key, sep = "\r")), , drop = FALSE]
  pub <- .ai_read_published_detail(io, out_dir)
  reads <- fold_repo_reads(pub$repo_reads, .ai_bind_like(.ai_empty_reads(),
                                                         lapply(parts, .ai_part_table, table = "repo_reads")))
  today <- format(Sys.Date())
  since <- NA_character_
  if (isTRUE(full)) {
    stored <- unname(.ai_read_pipeline_state(io, file.path(out_dir, "_state"))["ai_campaign_since"])
    since <- campaign_start(TRUE, if (length(stored)) stored else NA_character_, today)
  }
  work <- select_deep_work(flagged_df, ev_df, pub$signals, pub$search_log, reads, pub$account_counts,
                           campaign_since = since, today = today, full = full)
  write_flagged_partial(file.path(out_dir, "vcs-ai-flagged-roster.db"), flagged_df, ev_df, work = work,
                        campaign = data.frame(since = since, stringsAsFactors = FALSE))
  counts <- table(factor(work$reason, levels = names(AI_WORK_PRIORITY)))
  message(sprintf("ai gate: %d flagged repos across %d shard(s), %d searches or datings to do (%s)%s",
                  nrow(flagged_df), length(parts), nrow(work),
                  paste(sprintf("%s %d", names(counts), as.integer(counts)), collapse = ", "),
                  if (is.na(since)) "" else sprintf(", campaign since %s", since)))
}

#' The weekly gate: the same work list without a campaign.
run_gate_incremental <- function(io, out_dir, parts_dir) run_gate(io, out_dir, parts_dir, full = FALSE)

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

#' First-date searches for a tool's accounts: REST author-email is exact, so a hit dates
#' the account's first commit here. The REST-only address has its own count search.
.ai_author_queries <- function(tool) {
  a <- Find(function(x) identical(x$tool, tool), AI_ACCOUNTS)
  if (is.null(a)) return(character(0))
  paste0("author-email:", c(a$graphql, a$linked))
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
#' reconcile_ai_identity carries a no-longer-active slug's onsets onto the canonical repo_id
#' of its node_id and fills empty rows from an active sibling slug (PK-safe, before the
#' reduce); drop_unanchored_confirmations discards confirmations with no row to confirm;
#' ai_onset_reducer merges the reconciled prior set with the
#' incoming partials by the six column rules; the working vcs_ai_signals is
#' DELETE-and-rewritten with the fully-reduced set (never blanket-deleted and re-detected -
#' the rows are immutable, the DELETE only follows the R-side reduce); the summary rollups
#' are rebuilt so ai_* columns reflect the merge. Publishes with touched_years =
#' character(0), so no year shard is re-exported.
run_merge <- function(io, out_dir, parts_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  working_path <- file.path(out_dir, "_ai_merge_working.db")
  seed <- seed_working_db(io, out_dir, working_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), working_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con)
  ensure_series_schema(con)

  # No explicit protect_history_pull here (unlike backfill.R::run_merge): vcs_ai_signals
  # has no year component, so there is no year-shard content to fold in or protect;
  # seed_working_db already carries the prior vcs_ai_signals via the recent shard, and
  # publish() makes its own protect_history_pull whenever the release this merge was
  # seeded from lists any assets, which is the pull the change-gate and the regression
  # gate compare against.
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
  # A confirmation whose key the prior set does not hold would otherwise be written
  # out as a row carrying only a date, and nothing afterwards would ever fill it.
  n_incoming <- nrow(incoming)
  incoming <- drop_unanchored_confirmations(prior, incoming)
  if (nrow(incoming) < n_incoming)
    message(sprintf("ai merge: dropped %d confirmation row(s) with no prior row to confirm",
                    n_incoming - nrow(incoming)))

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
  DBI::dbExecute(con, "DELETE FROM vcs_dev_tooling_rules")
  DBI::dbWriteTable(con, "vcs_dev_tooling_rules", dev_tooling_rules_table(), append = TRUE)

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

  fail_list <- lapply(dev_parts, read_scan_failures)
  fails <- if (length(fail_list)) do.call(rbind, fail_list) else .fetch_failed_frame()
  active_n <- tryCatch(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM repos WHERE host = 'github' AND status = 'active'")$n[1],
    error = function(e) NA_integer_)
  message(sprintf("ai merge: %d repositories failed a read this week (contents %d) of %s on the roster",
                  length(unique(fails$repo_id)), length(unique(fails$repo_id[fails$query == "contents"])),
                  active_n))
  failure_stop <- scan_failure_stop_message(fails, active_n)

  message(sprintf("ai merge: %d prior, %d incoming, %d reduced onset rows",
                  nrow(prior), nrow(incoming), nrow(reduced)))
  out <- publish(io, con, out_dir, tag = "current", source_kind = "live",
                 touched_years = character(0), base_generation = attr(seed, "generation"))

  # Raised after the data is out, so the alarm costs a red build and not a
  # week of stale dev-tooling rows.
  canary_stop <- if (nrow(canary_unexplained) > 0)
    sprintf(paste0("AI detection canary: %d channel(s) detected nothing on the whole roster ",
                   "and are not recorded in AI_SILENT_CHANNELS_KNOWN: %s. ",
                   "Either the rule is broken or the zero is real; record which, with a date."),
            nrow(canary_unexplained),
            paste(canary_unexplained$tier, canary_unexplained$tool, sep = "/", collapse = ", "))
  stops <- c(failure_stop, canary_stop)
  if (length(stops)) stop(paste(stops, collapse = " "), call. = FALSE)
  invisible(out)
}

# ---- CLI dispatch -----------------------------------------------------------
# io is built here unless a test passes one, so the suite can drive the same entry
# point CI does.
main <- function(mode, out_dir, io = NULL) {
  token <- Sys.getenv("VCS_SIGNALS_TOKEN")
  if (is.null(io)) io <- list(
    graphql        = default_io(token)$graphql,
    search_hit     = function(owner, name, query, delay = SEARCH_DELAY_S)
                       search_earliest_commit_hit(token, owner, name, query, delay, sleep = Sys.sleep),
    sleep          = function(seconds) Sys.sleep(seconds),
    cran_packages  = function() {
      db <- tools::CRAN_package_db()
      db[!duplicated(db$Package), c("Package", "Version")]
    },
    release_exists = function() gh_release_exists(RELEASE_REPO),
    generation     = function() gh_release_generation(RELEASE_REPO),
    download       = function(pattern, dir) gh_release_download(RELEASE_REPO, pattern, dir),
    upload         = function(path) gh_release_upload(RELEASE_REPO, path))

  if (mode == "enumerate") {
    run_enumerate_ai(io, out_dir)
  } else if (mode == "canary") {
    ai_query_canary(io)
  } else if (mode == "cheap") {
    i <- suppressWarnings(as.integer(Sys.getenv("VCS_SHARD_I", "0")))
    N <- suppressWarnings(as.integer(Sys.getenv("VCS_SHARD_N", "1")))
    if (is.na(i) || is.na(N) || N < 1L || i < 0L || i >= N)
      stop("cheap: VCS_SHARD_I must be in [0, VCS_SHARD_N)")
    roster_dir <- Sys.getenv("VCS_ROSTER", out_dir)
    run_cheap(io, out_dir, file.path(roster_dir, "vcs-ai-roster.db"), i, N)
  } else if (mode == "gate") {
    run_gate(io, out_dir, Sys.getenv("VCS_PARTS", "parts"), full = TRUE)
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
    # If another publisher replaced the release between this merge's seed and its
    # publish (the weekly merge shares this Sunday cron), the merge waits for that
    # publisher to finish, seeds again from what it left, and rebuilds. The canary's
    # stop() comes after publish and is an ordinary error, so a run it fails is not
    # repeated.
    retry_on_publish_conflict(io, function() run_merge(io, out_dir, Sys.getenv("VCS_PARTS", "parts")))
  } else {
    stop("usage: ai_backfill.R [enumerate|canary|cheap|gate|gate-incremental|deep|merge]")
  }
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  mode <- if (length(args) >= 1) args[1] else ""
  out_dir <- Sys.getenv("VCS_OUT", "out")
  main(mode, out_dir)
}
