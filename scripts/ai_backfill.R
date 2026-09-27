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
#   deep      -> ask the shard's share of the week's search list and write its dated rows,
#                search log, account counts and campaign (matrix job)
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
export_ai_shard <- function(path, rows, model_rows = NULL, extra = list()) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  ensure_series_schema(con)                        # folds in the vcs_ai_signals CREATE
  if (nrow(rows) > 0) DBI::dbWriteTable(con, "vcs_ai_signals", rows, append = TRUE)
  if (!is.null(model_rows) && nrow(model_rows) > 0)
    DBI::dbWriteTable(con, "vcs_ai_models", model_rows, append = TRUE)
  for (nm in names(extra)) DBI::dbWriteTable(con, nm, extra[[nm]], overwrite = TRUE)
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
# One search_log row for one search's answer. A refusal has no count; a hit with no
# count returned still holds the commit that proves at least one.
.ai_log_row <- function(rid, key, rev, hit, verified, today) {
  outcome <- if (isTRUE(hit$unavailable)) "refused"
             else if (!is.na(.nn(hit$date, NA_character_)) || isTRUE(.nn(hit$total_count, 0L) > 0L)) "hit"
             else "none"
  data.frame(repo_id = rid, rule_key = key, rule_rev = as.integer(rev), ruleset_version = AI_RULESET_VERSION,
             asked_on = today, outcome = outcome,
             total_count = switch(outcome, refused = NA_integer_, none = 0L,
                                  hit = as.integer(.nn(hit$total_count, 1L))),
             verified = if (outcome == "hit") as.integer(isTRUE(verified)) else NA_integer_,
             incomplete = as.integer(.nn(hit$incomplete, 0L)),
             first_hit_on = if (outcome == "hit") substr(.nn(hit$date, NA_character_), 1, 10) else NA_character_,
             source = "search", stringsAsFactors = FALSE)
}

# vcs_ai_signals rows for one repository from this run's dates: the week's findings for
# the tools it worked on, files dated from their history, and every dated search hit.
.ai_deep_rows <- function(rid, s, evidence, repo, today) {
  tools <- unique(c(s$tools, if (!is.null(s$extra)) s$extra$tool))
  ev <- evidence[evidence$repo_id == rid & evidence$role == "authoring" & evidence$tool %in% tools &
                 !(evidence$marker %in% ai_non_naming_pr_keys()), , drop = FALSE]
  onset <- ev$onset; cens <- as.integer(ev$onset_censored); cens[is.na(cens)] <- 0L
  fill <- is.na(onset) & ev$tier %in% c("PR", "PB") & !is.na(repo$pr_onset_date)
  onset[fill] <- repo$pr_onset_date; cens[fill] <- 0L
  for (j in which(ev$tier == "D" & ev$marker %in% names(s$marker_dates))) {
    onset[j] <- s$marker_dates[[ev$marker[j]]]
    cens[j] <- if (ai_is_ignore_marker(ev$marker[j]) && !(ev$marker[j] %in% s$exact_ignores)) 1L else 0L
  }
  raw <- data.frame(tool = ev$tool, tier = ev$tier, marker = ev$marker, agnostic = as.integer(ev$agnostic %in% TRUE),
                    authored = as.integer(ev$tier == "A"), stringsAsFactors = FALSE)
  onsets <- data.frame(tool = ev$tool, marker = ev$marker, first_seen_date = onset,
                       first_seen_censored = cens, stringsAsFactors = FALSE)
  x <- s$extra
  if (!is.null(x) && nrow(x)) {
    raw <- rbind(raw, data.frame(tool = x$tool, tier = x$tier, marker = x$marker, agnostic = 0L,
                                 authored = as.integer(x$tier == "A"), stringsAsFactors = FALSE))
    onsets <- rbind(onsets, data.frame(tool = x$tool, marker = x$marker, first_seen_date = x$date,
                                       first_seen_censored = as.integer(!x$confirmed), stringsAsFactors = FALSE))
  }
  if (!nrow(raw)) return(.ai_empty_signals())
  onsets <- onsets[order(is.na(onsets$first_seen_date), onsets$first_seen_date), , drop = FALSE]
  guarded <- apply_fork_guard(raw, isTRUE(repo$is_fork == 1L), repo$parent, character(0))
  build_ai_detail(rid, guarded, onsets, today)
}

#' Works the gate's list most urgent first, so a stop at the budget or the point reserve leaves
#' only the least urgent items for the next run, and logs every answer so the merge derives the counts.
run_deep <- function(io, out_dir, roster_path, i, N,
                     marker_delay = BACKFILL_DELAY_S, search_delay = SEARCH_DELAY_S) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  fr <- read_flagged(roster_path)
  flagged <- fr$flagged
  evidence <- .ai_bind_like(cbind(repo_id = character(0), .ai_empty_found()), list(fr$evidence))
  evidence$role[is.na(evidence$role)] <- "authoring"
  today <- format(Sys.Date())
  mine_ids <- flagged$repo_id[shard_rows(nrow(flagged), i, N)]
  # A roster written before the list existed reads as every tool dated and every rule asked.
  work <- if (!is.null(fr$work)) fr$work else select_deep_work(flagged, evidence, NULL, NULL, NULL, NULL, today = today)
  work <- work[work$repo_id %in% mine_ids, , drop = FALSE]
  work <- work[order(work$priority, work$repo_id), , drop = FALSE]
  message(sprintf("ai deep shard %d/%d: %d item(s) for %d of %d flagged repos", i, N, nrow(work),
                  length(unique(work$repo_id)), nrow(flagged)))
  rules <- c(AI_TRAILER_PATTERNS, AI_AUTHOR_SUFFIXES, AI_REVIEW_RULES)
  state <- list(); logs <- list(); why <- character(0); counts <- list(); model_rows <- list()
  put_log <- function(row, reason) { logs[[length(logs) + 1L]] <<- row; why <<- c(why, reason) }
  search <- function(repo, q) tryCatch(io$search_hit(repo$owner, repo$name, q, search_delay),
                                       error = function(e) list(date = NA_character_, unavailable = TRUE))
  deadline <- Sys.time() + AI_DEEP_BUDGET_S
  stopped_early <- FALSE; done_why <- character(0)
  for (k in seq_len(nrow(work))) {
    if (Sys.time() >= deadline) {
      stopped_early <- TRUE
      message(sprintf("ai deep shard %d/%d: stopping at %d of %d items, %.0f min budget reached; the rest ride the next run",
                      i, N, k - 1L, nrow(work), AI_DEEP_BUDGET_S / 60))
      break
    }
    it <- work[k, ]; rid <- it$repo_id
    # Only dating spends GraphQL points, and a failed probe reads as none left and pauses the
    # shard, so the reserve is checked before a dating item only.
    if (identical(it$reason, "onset")) {
      rl <- graphql_rate_remaining(io)
      if (rl < AI_POINT_RESERVE) {
        message(sprintf("ai deep shard %d/%d: graphql rate remaining (%s) below reserve (%d); pausing after %d of %d items",
                        i, N, rl, AI_POINT_RESERVE, k - 1L, nrow(work)))
        break
      }
    }
    repo <- flagged[flagged$repo_id == rid, , drop = FALSE][1, ]
    s <- state[[rid]] %||% list(tools = character(0), marker_dates = list(), exact_ignores = character(0), extra = NULL)
    add_extra <- function(tool, tier, marker, date, confirmed)
      s$extra <<- rbind(s$extra, data.frame(tool = tool, tier = tier, marker = marker, date = date,
                                            confirmed = confirmed, stringsAsFactors = FALSE))
    if (identical(it$reason, "onset")) {
      s$tools <- unique(c(s$tools, it$tool))
      ev <- evidence[evidence$repo_id == rid & evidence$tool == it$tool & evidence$role == "authoring", , drop = FALSE]
      for (marker in unique(ev$marker[ev$tier == "D"])) {
        if (ai_is_ignore_marker(marker)) {
          parts <- strsplit(marker, ":", fixed = TRUE)[[1]]
          file <- if (identical(parts[1], "rbuildignore")) ".Rbuildignore" else ".gitignore"
          got <- tryCatch(fetch_ignore_onset(io, repo$owner, repo$name, file, paste(parts[-1], collapse = ":"),
                                             delay = marker_delay),
                          error = function(e) list(date = NA_character_, exact = FALSE))
          # An unreadable history falls back to the end of today, a floor that still sorts
          # after any exact date the same day.
          s$marker_dates[[marker]] <- if (!is.na(got$date)) got$date else paste0(today, "T23:59:59Z")
          if (!is.na(got$date) && isTRUE(got$exact)) s$exact_ignores <- c(s$exact_ignores, marker)
          next
        }
        d <- tryCatch(fetch_marker_onset(io, repo$owner, repo$name, marker_repo_path(marker), delay = marker_delay),
                      error = function(e) NA_character_)
        if (!is.na(d)) s$marker_dates[[marker]] <- d
      }
      if (any(ev$tier == "A")) for (q in .ai_author_queries(it$tool)) {
        hit <- search(repo, q)
        put_log(.ai_log_row(rid, paste0("author.", sub("^author-email:", "", q)), 1L, hit, TRUE, today), it$reason)
        if (!isTRUE(hit$unavailable) && !is.na(.nn(hit$date, NA_character_))) add_extra(it$tool, "A", "A", hit$date, TRUE)
      }
    } else if (startsWith(it$rule_key, "account.")) {
      addr <- sub("^account\\.", "", it$rule_key)
      hit <- search(repo, paste0("author-email:", addr))
      put_log(.ai_log_row(rid, it$rule_key, 1L, hit, TRUE, today), it$reason)
      n <- as.integer(.nn(hit$total_count, NA_integer_))
      if (!isTRUE(hit$unavailable) && !is.na(n) && n > 0L)
        counts[[length(counts) + 1L]] <- data.frame(repo_id = rid, tool = it$tool, identity_set = addr, commits = n,
          newest_commit_date = NA_character_, measured_on = today, stringsAsFactors = FALSE)
      if (!isTRUE(hit$unavailable) && !is.na(.nn(hit$date, NA_character_))) add_extra(it$tool, "A", "A", hit$date, TRUE)
    } else {
      rule <- Find(function(r) identical(r$key, it$rule_key), rules)
      if (is.null(rule)) next
      tier <- if (startsWith(rule$key, "name.")) "C" else "B"
      hit <- search(repo, rule$query)
      is_hit <- !isTRUE(hit$unavailable) && !is.na(.nn(hit$date, NA_character_))
      v <- if (is_hit) verify_search_hit(rule, tier, hit) else list(tool = rule$tool, confirmed = FALSE)
      put_log(.ai_log_row(rid, rule$key, rule$rev, hit, v$confirmed, today), it$reason)
      # Review credits are logged only: they never name a tool that wrote the package.
      if (is_hit && !startsWith(rule$key, "review.") && !is.na(v$tool)) {
        add_extra(v$tool, tier, if (isTRUE(v$confirmed)) rule$key else tier, hit$date, isTRUE(v$confirmed))
        if (isTRUE(v$confirmed) && !is.null(hit$items) && nrow(hit$items) > 0) {
          n <- .nn(hit$total_count, NA_integer_)
          # A search GitHub cut short returned part of the credits, so its tally is never the whole one.
          whole <- (is.na(n) || n <= nrow(hit$items)) && !isTRUE(hit$incomplete == 1L)
          mr <- build_ai_model_rows(rid, v$tool, hit$items, window_complete = whole)
          if (nrow(mr)) model_rows[[length(model_rows) + 1L]] <- mr
        }
      }
    }
    state[[rid]] <- s
    done_why <- c(done_why, it$reason)
  }
  rows <- .ai_bind_like(.ai_empty_signals(), lapply(names(state), function(rid)
    .ai_deep_rows(rid, state[[rid]], evidence, flagged[flagged$repo_id == rid, , drop = FALSE][1, ], today)))
  models_df <- .ai_bind_like(.ai_empty_models(), model_rows)
  log_df <- .ai_bind_like(.ai_empty_log(), logs)
  export_ai_shard(file.path(out_dir, sprintf("vcs-ai-shard-%d.db", i)), rows, models_df,
                  extra = list(search_log = log_df, account_counts = .ai_bind_like(.ai_empty_counts(), counts),
                               campaign = .ai_bind_like(data.frame(since = character(), stringsAsFactors = FALSE),
                                                        list(fr$campaign))))
  by_reason <- table(factor(done_why, levels = names(AI_WORK_PRIORITY)))
  message(sprintf("ai deep shard %d/%d: %d item(s) done (%s)", i, N, length(done_why),
                  paste(sprintf("%s %d", names(by_reason), as.integer(by_reason)), collapse = ", ")))
  # Asked, matched, none and refused for each reason, as the shard's own record of its searches.
  for (rs in intersect(names(AI_WORK_PRIORITY), unique(why))) {
    o <- log_df$outcome[why == rs]
    message(sprintf("ai deep shard %d/%d:   %s: %d asked, %d matched, %d none, %d refused",
                    i, N, rs, length(o), sum(o == "hit"), sum(o == "none"), sum(o == "refused")))
  }
  if (stopped_early) message(sprintf("ai deep shard %d/%d: PARTIAL, the rest ride the next run", i, N))
  if (nrow(models_df) > 0)
    message(sprintf("ai deep shard %d/%d: %d model row(s) across %d repo(s)", i, N, nrow(models_df),
                    length(unique(models_df$repo_id))))
  message(sprintf("ai deep shard %d/%d: %d dated row(s)", i, N, nrow(rows)))
  refused <- sum(log_df$outcome == "refused")
  if (refused > 0L)
    message(sprintf("ai deep shard %d/%d: WARNING %d commit search(es) were refused (rate limit or error); ", i, N, refused),
            "commit counts for this shard are UNDER-COUNTED, not absent")
}

# ---- merge ------------------------------------------------------------------
#' Fold the week's search shards and weekly read partials into every published AI table and republish.
#' Both stops run after publishing, so an alarm costs a red build and not a week of stale data.
run_merge <- function(io, out_dir, parts_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  working_path <- file.path(out_dir, "_ai_merge_working.db")
  seed <- seed_working_db(io, out_dir, working_path)
  con <- DBI::dbConnect(RSQLite::SQLite(), working_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con)
  ensure_series_schema(con)
  reconcile_ai_identity(con)
  today <- format(Sys.Date())
  have <- function(t, empty) if (DBI::dbExistsTable(con, t)) .ai_bind_like(empty, list(DBI::dbReadTable(con, t))) else empty
  put <- function(t, df) {
    DBI::dbExecute(con, sprintf('DELETE FROM "%s"', t))
    if (nrow(df)) DBI::dbWriteTable(con, t, df, append = TRUE)
  }
  deep <- list.files(parts_dir, pattern = "^vcs-ai-shard-.*\\.db$", full.names = TRUE)
  cheap <- list.files(parts_dir, pattern = "^vcs-ai-cheap-.*\\.db$", full.names = TRUE)
  from <- function(paths, t, empty) .ai_bind_like(empty, lapply(paths, .ai_part_table, table = t))
  # A cheap partial from before the weekly read has no read state, and its findings carry no dates.
  current <- cheap[vapply(cheap, function(p) !is.null(.ai_part_table(p, "repo_reads")), logical(1))]
  message(sprintf("ai merge: %d search shard(s), %d weekly read partial(s), %d from older code left out",
                  length(deep), length(cheap), length(cheap) - length(current)))

  prior <- have("vcs_ai_signals", .ai_empty_signals())
  found <- from(current, "evidence", cbind(repo_id = character(0), .ai_empty_found()))
  found$role[is.na(found$role)] <- "authoring"
  incoming <- rbind(from(deep, "vcs_ai_signals", .ai_empty_signals()), build_cheap_rows(found, today))

  # A weekly add counts from the watermark stored before this run, so the log and models read that one.
  reads_prior <- have("vcs_ai_repo_reads", .ai_empty_reads())
  reads_in <- from(cheap, "repo_reads", cbind(.ai_empty_reads(), reached_first = integer()))
  reads <- fold_repo_reads(reads_prior, reads_in)
  rebuilt <- rebuilt_log_repos(reads, reads_in)
  counted <- !is.na(reads_in$accounts_counted_on)
  counts <- fold_account_counts(have("vcs_ai_account_counts", .ai_empty_counts()),
                                rbind(from(cheap, "account_counts", .ai_empty_counts()),
                                      from(deep, "account_counts", .ai_empty_counts())),
                                counted_repos = stats::setNames(reads_in$accounts_counted_on[counted],
                                                                reads_in$repo_id[counted]))
  log_in <- cbind(.ai_empty_log(), mode = character(), read_after = character())
  log <- fold_search_log(have("vcs_ai_search_log", .ai_empty_log()),
                         rbind(from(cheap, "search_log", log_in), from(deep, "search_log", log_in)),
                         rebuilt_repos = rebuilt, reads = reads_prior)
  review <- fold_review_rows(have("vcs_ai_review_signals", .ai_empty_review()), from(cheap, "review", .ai_empty_review()))
  outside <- fold_outside_prs(have("vcs_ai_outside_prs", .ai_empty_outside()), from(cheap, "outside_prs", .ai_empty_outside()))

  moved <- ai_reclassify_rows(prior, found, log, reads, outside)
  prior <- moved$signals
  review <- fold_review_rows(review, moved$review)

  n_incoming <- nrow(incoming)
  incoming <- drop_unanchored_confirmations(prior, incoming)
  if (nrow(incoming) < n_incoming)
    message(sprintf("ai merge: dropped %d row(s) with no prior row to confirm", n_incoming - nrow(incoming)))
  reduced <- ai_onset_reducer(prior, incoming)
  reduced <- derive_assisted_counts(derive_authored_counts(reduced, counts, reads), log, reads)
  review <- derive_review_counts(review, log)

  put("vcs_ai_signals", reduced)
  put("vcs_ai_repo_reads", reads)
  put("vcs_ai_account_counts", counts)
  put("vcs_ai_search_log", log)
  put("vcs_ai_review_signals", review)
  put("vcs_ai_outside_prs", outside)

  roster_n <- tryCatch(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM repos")$n[1], error = function(e) NA_integer_)
  canary_unexplained <- ai_canary_check(reduced, roster_n = roster_n, outside = outside)
  put("vcs_ai_silent_channels", ai_silent_channel_table(reduced, outside = outside))

  models <- fold_models(have("vcs_ai_models", .ai_empty_models()), from(deep, "vcs_ai_models", .ai_empty_models()),
                        from(cheap, "models", cbind(.ai_empty_models(), mode = character(), read_after = character())),
                        rebuilt_repos = rebuilt, reads = reads_prior)
  put("vcs_ai_models", models)
  message(sprintf("ai merge: %d model row(s) across %d repo(s)", nrow(models), length(unique(models$repo_id))))

  inv <- ai_rule_inventory()
  inv$ruleset_version <- AI_RULESET_VERSION
  put("vcs_ai_rule_inventory", inv)
  put("vcs_ai_search_coverage", build_search_coverage(log, reads))
  record_ruleset_history(con, today, AI_RULESET_VERSION, AI_RULESET_CHANGE_KEYS)
  # Dated once the three tables enumerate guards hold rows, so an empty first publish is never read as a loss.
  if (nrow(reads) && nrow(log) && nrow(counts))
    DBI::dbExecute(con, "INSERT OR IGNORE INTO pipeline_state (key, value) VALUES ('ai_state_tables_since', ?)",
                   params = list(today))
  # A full gate's campaign date is kept until every flagged repository has been asked since.
  camp <- unique(stats::na.omit(from(deep, "campaign", data.frame(since = character(), stringsAsFactors = FALSE))$since))
  stored <- DBI::dbGetQuery(con, "SELECT value FROM pipeline_state WHERE key = 'ai_campaign_since'")$value
  since <- if (length(stored)) stored[1] else if (length(camp)) camp[1] else NA_character_
  if (!length(stored) && !is.na(since))
    DBI::dbExecute(con, "INSERT INTO pipeline_state (key, value) VALUES ('ai_campaign_since', ?)", params = list(since))
  flag_ids <- unique(from(cheap, "flagged", .ai_empty_flagged())$repo_id)
  if (!is.na(since) && length(flag_ids) && campaign_finished(flag_ids, log, reads, since)) {
    DBI::dbExecute(con, "DELETE FROM pipeline_state WHERE key = 'ai_campaign_since'")
    message(sprintf("ai merge: the campaign since %s has asked every flagged repository", since))
  }
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
  per_doc <- function(q) length(unique(fails$repo_id[fails$query == q]))
  message(sprintf(paste0("ai merge: %d repositories failed a read this week (contents %d, activity %d, ",
                         "accounts %d, walk %d) of %s on the roster"),
                  length(unique(fails$repo_id)), per_doc("contents"), per_doc("activity"), per_doc("accounts"),
                  per_doc("walk"), active_n))
  failure_stop <- scan_failure_stop_message(fails, active_n)

  message(sprintf("ai merge: %d prior, %d incoming, %d reduced onset rows",
                  nrow(prior), nrow(incoming), nrow(reduced)))
  out <- publish(io, con, out_dir, tag = "current", source_kind = "live",
                 touched_years = character(0), base_generation = attr(seed, "generation"))

  # Raised after the data is out, so the alarm costs a red build and not a
  # week of stale dev-tooling rows.
  canary_stop <- if (nrow(canary_unexplained) > 0)
    sprintf(paste0("Silent-search check: %d search(es) matched nothing in any scanned repository ",
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
