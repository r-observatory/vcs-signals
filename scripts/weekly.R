#!/usr/bin/env Rscript
# scripts/weekly.R - sharded weekly commit-count + contributor-count
# collection for vcs-signals.
#
# Same three sub-commands as scripts/backfill.R, wired together by CI:
#   enumerate -> reuse backfill's roster build (every github repo with any
#                signal) - one job
#   fetch     -> for one even mod-N shard of the roster, collect
#                commits_total (GraphQL history.totalCount, batched) and
#                contributors_total (REST /contributors count) - matrix job
#   merge     -> fold every shard's snapshot into the published series as a
#                change-only weekly point dated today, rebuild the summary,
#                and republish - one job
#
# Both collected values are current-value snapshots, not reconstructed
# history: commit history is too large to page and contributor history is
# not exposed by any API, so these two metrics only ever accrue forward from
# whenever a weekly run first sees a repo.
if (!exists("STARGAZER_PAGE"))      source("scripts/config.R")
if (!exists("ensure_series_schema")) source("scripts/helpers.R")
if (!exists("build_gauge_query"))    source("scripts/github.R")
if (!exists("gh_release_exists"))    source("scripts/update.R")  # default_io, gh_release_*, seed_working_db
if (!exists("run_enumerate"))        source("scripts/backfill.R") # run_enumerate, write_roster, load_roster
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

SNAPSHOT_TABLE <- "snapshot"

# ---- snapshot shard IO ------------------------------------------------------
#' Write a shard's collected (repo_id, commits_total, contributors_total)
#' rows to a fresh SQLite file. Either numeric column may be NA for a given
#' repo (a fetch that failed or was skipped this run).
export_snapshot_shard <- function(path, rows) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, sprintf("CREATE TABLE %s (
    repo_id TEXT PRIMARY KEY, commits_total INTEGER, contributors_total INTEGER)", SNAPSHOT_TABLE))
  if (nrow(rows) > 0)
    DBI::dbWriteTable(con, SNAPSHOT_TABLE,
      rows[c("repo_id", "commits_total", "contributors_total")], append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

# ---- fetch ------------------------------------------------------------------
#' Collect commits_total and contributors_total for one even mod-N shard of
#' the roster. Commits are swept in COMMIT_HISTORY_BATCH-sized chunks
#' through one aliased GraphQL query per chunk (fetch_commit_counts): a
#' chunk whose query itself fails leaves every repo in that chunk NA for
#' commits_total this run (left for a re-run), while every other chunk is
#' unaffected. Contributors are fetched one REST request per repo
#' (io$contributors, paced by contributor_delay): a failing repo is NA for
#' contributors_total only, independent of whether its commit count
#' succeeded. Neither failure mode ever aborts the shard. Writes a partial
#' `snapshot` table to out/vcs-signals-shard-<i>.db.
run_fetch_shard <- function(io, out_dir, roster_path, i, N,
                            commit_delay = BACKFILL_DELAY_S,
                            contributor_delay = CONTRIBUTOR_DELAY_S,
                            batch_size = COMMIT_HISTORY_BATCH) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  roster <- load_roster(roster_path)
  mine <- roster[shard_rows(nrow(roster), i, N), , drop = FALSE]
  total <- nrow(mine)
  message(sprintf("weekly fetch shard %d/%d: %d of %d repos", i, N, total, nrow(roster)))

  commits_total <- rep(NA_integer_, total)
  n_commit_ok <- 0L; n_commit_skipped <- 0L
  for (idx in unname(chunk(seq_len(total), batch_size))) {
    repos <- mine[idx, , drop = FALSE]
    got <- tryCatch(fetch_commit_counts(io, repos), error = function(e) NULL)
    if (commit_delay > 0) Sys.sleep(commit_delay)
    if (is.null(got)) { n_commit_skipped <- n_commit_skipped + length(idx); next }
    n_commit_ok <- n_commit_ok + length(idx)
    commits_total[idx] <- as.integer(unlist(got[repos$repo_id]))
  }

  contributors_total <- rep(NA_integer_, total)
  n_contrib_ok <- 0L; n_contrib_skipped <- 0L
  for (r in seq_len(total)) {
    v <- tryCatch(io$contributors(mine$owner[r], mine$name[r]), error = function(e) NA_integer_)
    if (contributor_delay > 0) Sys.sleep(contributor_delay)
    v <- suppressWarnings(as.integer(v))
    v <- if (length(v) == 1) v else NA_integer_
    if (is.na(v)) n_contrib_skipped <- n_contrib_skipped + 1L else n_contrib_ok <- n_contrib_ok + 1L
    contributors_total[r] <- v
  }

  rows <- data.frame(repo_id = mine$repo_id, commits_total = commits_total,
                     contributors_total = contributors_total, stringsAsFactors = FALSE)

  shard_path <- file.path(out_dir, sprintf("vcs-signals-shard-%d.db", i))
  export_snapshot_shard(shard_path, rows)
  message(sprintf("weekly fetch shard %d/%d: commits %d ok/%d skipped, contributors %d ok/%d skipped",
                  i, N, n_commit_ok, n_commit_skipped, n_contrib_ok, n_contrib_skipped))
  shard_path
}

# ---- merge --------------------------------------------------------------------
#' Fold every shard's commits_total/contributors_total snapshot into the
#' published series as a change-only point dated today, rebuild the summary,
#' and republish. Mirrors backfill.R::run_merge's seed + complete-history-load
#' pattern (seed_working_db for the recent window, then protect_history_pull
#' plus a year-shard fold so publish()'s re-export of the touched (current)
#' year never truncates other metrics' rows already in that shard).
#'
#' materialize_series is fed prev_latest restricted to WEEKLY_METRICS only,
#' so a daily metric (stars, forks, ...) already sitting in series_latest can
#' never be clobbered or spuriously re-emitted by this pass; snapshot_long is
#' likewise built from WEEKLY_METRICS alone. A repo unchanged since the last
#' weekly run contributes no new signals_series row for that metric.
run_merge <- function(io, out_dir, parts_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  working_path <- file.path(out_dir, "_weekly_working.db")
  seed_working_db(io, out_dir, working_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), working_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con)
  ensure_series_schema(con)

  protect_history_pull(io, out_dir)
  year_shards <- list.files(out_dir, pattern = "^vcs-signals-[0-9]{4}\\.db$", full.names = TRUE)
  for (ys in year_shards) {
    ycon <- DBI::dbConnect(RSQLite::SQLite(), ys)
    yrows <- tryCatch(
      if (DBI::dbExistsTable(ycon, "signals_series")) DBI::dbReadTable(ycon, "signals_series") else NULL,
      error = function(e) NULL)
    DBI::dbDisconnect(ycon)
    if (!is.null(yrows) && nrow(yrows) > 0)
      DBI::dbExecute(con,
        "INSERT OR IGNORE INTO signals_series (repo_id, date, metric, value) VALUES (?,?,?,?)",
        params = list(yrows$repo_id, yrows$date, yrows$metric, yrows$value))
  }

  today <- format(Sys.Date())

  parts <- list.files(parts_dir, pattern = "^vcs-signals-shard-.*\\.db$", full.names = TRUE)
  empty_snapshot <- data.frame(repo_id = character(), commits_total = integer(),
                               contributors_total = integer(), stringsAsFactors = FALSE)
  snap_rows <- lapply(parts, function(p) {
    pcon <- DBI::dbConnect(RSQLite::SQLite(), p)
    on.exit(DBI::dbDisconnect(pcon), add = TRUE)
    if (!DBI::dbExistsTable(pcon, SNAPSHOT_TABLE)) return(empty_snapshot)
    DBI::dbReadTable(pcon, SNAPSHOT_TABLE)
  })
  snapshot <- if (length(snap_rows)) do.call(rbind, snap_rows) else empty_snapshot
  snapshot <- snapshot[!duplicated(snapshot$repo_id), , drop = FALSE]

  snapshot_long <- data.frame(repo_id = character(), metric = character(),
                              value = integer(), stringsAsFactors = FALSE)
  for (metric in WEEKLY_METRICS) {
    v <- snapshot[[metric]]
    keep <- !is.na(v)
    if (any(keep))
      snapshot_long <- rbind(snapshot_long, data.frame(
        repo_id = snapshot$repo_id[keep], metric = metric,
        value = as.integer(v[keep]), stringsAsFactors = FALSE))
  }

  ph <- paste(sprintf("'%s'", WEEKLY_METRICS), collapse = ", ")
  prev_latest <- DBI::dbGetQuery(con, sprintf(
    "SELECT repo_id, metric, value FROM series_latest WHERE metric IN (%s)", ph))
  mat <- materialize_series(prev_latest, snapshot_long, today)

  n_inserted <- 0L
  if (nrow(mat$series_rows) > 0) {
    before <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM signals_series")$n
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO signals_series (repo_id, date, metric, value) VALUES (?,?,?,?)",
      params = list(mat$series_rows$repo_id, mat$series_rows$date,
                    mat$series_rows$metric, mat$series_rows$value))
    after <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM signals_series")$n
    n_inserted <- after - before
  }
  if (nrow(mat$new_latest) > 0) {
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO series_latest (repo_id, metric, value) VALUES (?,?,?)",
      params = list(mat$new_latest$repo_id, mat$new_latest$metric, mat$new_latest$value))
  }

  # Rebuild the summary so commits_total/contributors_total populate.
  # Descriptive attributes (license/topics/is_archived/last_commit_date) are
  # not collected this run, so they are carried forward from the prior
  # summary row exactly as run_update does for a repo it didn't collect
  # this run.
  repos_all <- DBI::dbReadTable(con, "repos")
  rp_all <- DBI::dbReadTable(con, "repo_packages")
  series_all <- DBI::dbGetQuery(con, "SELECT repo_id, date, metric, value FROM signals_series")
  latest_all <- DBI::dbGetQuery(con, "SELECT repo_id, metric, value FROM series_latest")

  prev_summary_attrs <- DBI::dbGetQuery(con,
    "SELECT repo_id, license, topics, is_archived, last_commit_date
       FROM vcs_signals_summary WHERE repo_id IS NOT NULL")
  if (nrow(prev_summary_attrs) > 0) {
    prev_summary_attrs <- prev_summary_attrs[!duplicated(prev_summary_attrs$repo_id), ]
    prev_summary_attrs$is_archived <- as.integer(prev_summary_attrs$is_archived)
  }
  repo_attrs <- merge(repos_all[, c("repo_id", "first_seen", "last_seen")], prev_summary_attrs,
                      by = "repo_id", all.x = TRUE)

  summary_df <- build_signals_summary(latest_all, series_all, repo_attrs, rp_all, today)
  DBI::dbExecute(con, "DELETE FROM vcs_signals_summary")
  if (nrow(summary_df) > 0) DBI::dbWriteTable(con, "vcs_signals_summary", summary_df, append = TRUE)

  touched_years <- unique(substr(mat$series_rows$date, 1, 4))
  message(sprintf("weekly merge: %d shard partials, %d repos snapshotted, %d changed rows, %d year(s) touched",
                  length(parts), nrow(snapshot), n_inserted, length(touched_years)))

  invisible(publish(io, con, out_dir, tag = "current", source_kind = "live", touched_years = touched_years))
}

# ---- CLI dispatch -----------------------------------------------------------------
main <- function(mode, out_dir) {
  token <- Sys.getenv("VCS_SIGNALS_TOKEN")
  io <- list(
    graphql        = default_io(token)$graphql,
    contributors   = function(owner, name) fetch_contributor_count(token, owner, name),
    release_exists = function() gh_release_exists(RELEASE_REPO),
    download       = function(pattern, dir) gh_release_download(RELEASE_REPO, pattern, dir),
    upload         = function(path) gh_release_upload(RELEASE_REPO, path))

  if (mode == "enumerate") {
    run_enumerate(io, out_dir)
  } else if (mode == "fetch") {
    i <- suppressWarnings(as.integer(Sys.getenv("VCS_SHARD_I", "0")))
    N <- suppressWarnings(as.integer(Sys.getenv("VCS_SHARD_N", "1")))
    if (is.na(i) || is.na(N) || N < 1L || i < 0L || i >= N)
      stop("fetch: VCS_SHARD_I must be in [0, VCS_SHARD_N)")
    roster_dir <- Sys.getenv("VCS_ROSTER", out_dir)
    run_fetch_shard(io, out_dir, file.path(roster_dir, "vcs-signals-roster.db"), i, N)
  } else if (mode == "merge") {
    run_merge(io, out_dir, Sys.getenv("VCS_PARTS", "parts"))
  } else {
    stop("usage: weekly.R [enumerate|fetch|merge]")
  }
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  mode <- if (length(args) >= 1) args[1] else ""
  out_dir <- Sys.getenv("VCS_OUT", "out")
  main(mode, out_dir)
}
