#!/usr/bin/env Rscript
# scripts/backfill.R - sharded historical cumulative-series backfill for
# vcs-signals (stars, forks, releases).
#
# Three sub-commands, wired together by CI:
#   enumerate -> build the GitHub repo roster from the published summary (one job)
#   fetch     -> paginate the requested metrics for one even mod-N shard of the
#                roster (matrix job)
#   merge     -> fold every shard partial's reconstructed rows into the published
#                series and republish (one job)
#
# Reconstruction (reconstruct_cumulative_series, scripts/helpers.R) and the
# query builder/parser/paginator (build_connection_query/parse_connection/
# paginate_connection, scripts/github.R) are pure/thin and unit-tested, driven
# by METRIC_CONNECTIONS (scripts/config.R); only the three run_* orchestrators
# below touch the network or disk, behind an injected io, mirroring
# scripts/update.R's run_update().
if (!exists("STARGAZER_PAGE"))      source("scripts/config.R")
if (!exists("ensure_series_schema")) source("scripts/helpers.R")
if (!exists("build_gauge_query"))    source("scripts/github.R")
if (!exists("gh_release_exists"))    source("scripts/update.R")  # default_io, gh_release_*, seed_working_db
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

ROSTER_TABLE <- "roster"

# ---- roster IO ---------------------------------------------------------------
write_roster <- function(path, roster_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, sprintf("CREATE TABLE %s (
    repo_id TEXT PRIMARY KEY, owner TEXT NOT NULL, name TEXT NOT NULL,
    stars INTEGER NOT NULL, done INTEGER NOT NULL DEFAULT 0)", ROSTER_TABLE))
  if (nrow(roster_df) > 0)
    DBI::dbWriteTable(con, ROSTER_TABLE, roster_df[c("repo_id", "owner", "name", "stars", "done")], append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

load_roster <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbReadTable(con, ROSTER_TABLE)
}

# ---- enumerate ----------------------------------------------------------------
#' Build the GitHub repo roster from the published summary shard: every
#' repo_id with at least one star, restricted to github.com (the only host
#' this backfill supports). owner/name are derived by splitting repo_id
#' (host_domain/owner/name) rather than re-resolving, since the summary
#' shard already carries a validated github.com repo_id.
run_enumerate <- function(io, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  if (!isTRUE(io$download("vcs-signals-summary.db", out_dir)))
    stop("could not download vcs-signals-summary.db from the published release; nothing to enumerate")

  summary_path <- file.path(out_dir, "vcs-signals-summary.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), summary_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con,
    "SELECT repo_id, MAX(stars) AS stars FROM vcs_signals_summary
      WHERE repo_id LIKE 'github.com/%'
        AND (stars > 0 OR forks > 0 OR releases_total > 0) GROUP BY repo_id")

  parts <- strsplit(rows$repo_id, "/", fixed = TRUE)
  roster <- data.frame(
    repo_id = rows$repo_id,
    owner   = vapply(parts, function(p) p[2], ""),
    name    = vapply(parts, function(p) p[3], ""),
    stars   = as.integer(rows$stars),
    done    = 0L,
    stringsAsFactors = FALSE)

  message(sprintf("enumerate: %d github repos, %s total stars",
                  nrow(roster), format(sum(roster$stars), big.mark = ",", scientific = FALSE)))
  write_roster(file.path(out_dir, "vcs-signals-roster.db"), roster)
}

# ---- fetch ----------------------------------------------------------------------
#' Paginate the requested metrics for one even mod-N shard of the roster and
#' reconstruct each repo's historical series per metric (cumulative for
#' stars/forks/releases_total, open-count for issues_open/prs_open). Each
#' metric's repos are swept in `batch_size`-sized chunks: one batched query
#' (fetch_batched_page) fetches every chunk member's first connection page in
#' a single ~1-point request, then only the repos whose first page reported
#' hasNextPage pay for individual continuation pagination (paginate_connection,
#' resuming from that page's cursor and prepending its already-fetched nodes
#' rather than re-fetching page 1). If the batched query itself fails, this
#' falls back to a full per-repo pagination for every repo in that chunk, so
#' one bad chunk does not lose its chunk-mates. Every repo/metric fetch -
#' batched or per-repo - is isolated: a failure (rate limit, transient 502,
#' repo gone) is skipped this run for that repo/metric only - left for a
#' re-run - rather than aborting the whole chunk or shard.
run_fetch_shard <- function(io, out_dir, roster_path, i, N, delay = BACKFILL_DELAY_S,
                            metrics = BACKFILL_METRICS, batch_size = BATCH_REPOS) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  roster <- load_roster(roster_path)
  mine <- roster[shard_rows(nrow(roster), i, N), , drop = FALSE]
  total <- nrow(mine)
  message(sprintf("fetch shard %d/%d: %d of %d repos, metrics: %s",
                  i, N, total, nrow(roster), paste(metrics, collapse = ",")))

  acc <- list(); n_ok <- 0L; n_skipped <- 0L; n_rows <- 0L; processed <- 0L
  total_ops <- total * length(metrics)

  for (metric in metrics) {
    mc <- METRIC_CONNECTIONS[[metric]]
    for (idx in unname(chunk(seq_len(total), batch_size))) {
      repos <- mine[idx, , drop = FALSE]

      batched <- tryCatch(fetch_batched_page(io, repos, metric), error = function(e) NULL)
      if (!is.null(batched) && delay > 0) Sys.sleep(delay)

      for (r in seq_len(nrow(repos))) {
        repo_id <- repos$repo_id[r]; owner <- repos$owner[r]; name <- repos$name[r]

        nodes <- tryCatch({
          if (is.null(batched)) {
            # whole-chunk batched query failed: fall back to a full per-repo
            # fetch so this repo's chunk-mates are not lost along with it.
            paginate_connection(io, owner, name, metric, delay = delay)
          } else {
            entry <- batched[[repo_id]]
            if (isTRUE(entry$has_next))
              paginate_connection(io, owner, name, metric, delay = delay,
                                  after = entry$end_cursor, first_nodes = entry$nodes)
            else entry$nodes
          }
        }, error = function(e) NULL)

        processed <- processed + 1L
        if (processed %% 500L == 0L)
          message(sprintf("fetch shard %d/%d: %d/%d repo-metric fetches processed (%d ok, %d skipped, %d series rows so far)",
                          i, N, processed, total_ops, n_ok, n_skipped, n_rows))

        if (is.null(nodes)) { n_skipped <- n_skipped + 1L; next }
        n_ok <- n_ok + 1L
        if (nrow(nodes) == 0) next

        series <- if (identical(mc$kind, "open"))
          reconstruct_open_series(repo_id, nodes$created, nodes$closed, metric)
        else
          reconstruct_cumulative_series(repo_id, nodes$ts, metric)
        acc[[length(acc) + 1L]] <- series
        n_rows <- n_rows + nrow(series)
      }
    }
  }
  rows <- if (length(acc)) do.call(rbind, acc) else
    data.frame(repo_id = character(), date = character(), metric = character(),
              value = integer(), stringsAsFactors = FALSE)

  shard_path <- file.path(out_dir, sprintf("vcs-signals-shard-%d.db", i))
  export_series_shard(shard_path, rows)
  message(sprintf("fetch shard %d/%d: %d metric-fetches ok, %d skipped, %d series rows",
                  i, N, n_ok, n_skipped, nrow(rows)))
  shard_path
}

# ---- merge ------------------------------------------------------------------------
#' Fold every shard partial's reconstructed rows (whatever metrics that run
#' fetched - stars, forks, releases, or any subset) into the published series
#' and republish. Rows are inserted with INSERT OR IGNORE keyed on
#' repo_id/date/metric: a backfilled row can never collide with a same-day
#' row for a different metric, and if it collides with today's forward point
#' for the same metric the existing (forward) value wins untouched.
#' series_latest is never written here. Only the distinct years actually
#' touched by the backfilled rows are re-exported (touched_years), so every
#' other published year shard - and the untouched portion of the recent
#' shard - are left exactly as protect_history_pull downloaded them.
run_merge <- function(io, out_dir, parts_dir, purge_metrics = character(0)) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  working_path <- file.path(out_dir, "_backfill_working.db")
  seed_working_db(io, out_dir, working_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), working_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con)
  ensure_series_schema(con)

  # Load the COMPLETE published history into the working DB, not just the
  # recent-window tail seed_working_db pulled. protect_history_pull downloads
  # the manifest, the recent shard, and every published year shard named in
  # manifest$summary$years; each year shard's signals_series rows are folded in
  # with INSERT OR IGNORE, so the recent-window overlap dedupes on the
  # (repo_id, date, metric) primary key. Without this, publish() would re-export
  # each touched year from an incomplete working DB and truncate forward rows -
  # of ALL metrics (forks/issues/PRs/releases and the forward stars points) -
  # that have aged out of the 400-day recent window. publish() re-pulls these
  # same shards afterward (idempotent), so the redundant download is harmless;
  # what matters is that this load happens before publish() re-exports.
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

  # Purge mis-named or retired metrics from the complete working history, and
  # remember the years they spanned so those shards are re-exported without them.
  purged_years <- character(0)
  if (length(purge_metrics) > 0) {
    ph <- paste(sprintf("'%s'", purge_metrics), collapse = ", ")
    purged_years <- DBI::dbGetQuery(con, sprintf(
      "SELECT DISTINCT substr(date, 1, 4) AS yr FROM signals_series WHERE metric IN (%s)", ph))$yr
    DBI::dbExecute(con, sprintf("DELETE FROM signals_series WHERE metric IN (%s)", ph))
    message(sprintf("merge: purged metric(s) %s across %d year(s)",
                    paste(purge_metrics, collapse = ","), length(purged_years)))
  }

  parts <- list.files(parts_dir, pattern = "^vcs-signals-shard-.*\\.db$", full.names = TRUE)
  part_rows <- lapply(parts, function(p) {
    pcon <- DBI::dbConnect(RSQLite::SQLite(), p)
    on.exit(DBI::dbDisconnect(pcon), add = TRUE)
    if (!DBI::dbExistsTable(pcon, "signals_series"))
      return(data.frame(repo_id = character(), date = character(), metric = character(),
                        value = integer(), stringsAsFactors = FALSE))
    DBI::dbReadTable(pcon, "signals_series")
  })
  backfill_rows <- if (length(part_rows)) do.call(rbind, part_rows) else
    data.frame(repo_id = character(), date = character(), metric = character(),
              value = integer(), stringsAsFactors = FALSE)
  backfill_rows <- backfill_rows[!duplicated(paste(backfill_rows$repo_id, backfill_rows$date, backfill_rows$metric)), , drop = FALSE]

  n_inserted <- 0L
  if (nrow(backfill_rows) > 0) {
    before <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM signals_series")$n
    DBI::dbExecute(con,
      "INSERT OR IGNORE INTO signals_series (repo_id, date, metric, value) VALUES (?,?,?,?)",
      params = list(backfill_rows$repo_id, backfill_rows$date, backfill_rows$metric, backfill_rows$value))
    after <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM signals_series")$n
    n_inserted <- after - before
  }
  touched_years <- unique(c(substr(backfill_rows$date, 1, 4), purged_years))
  message(sprintf("merge: %d shard partials, %d backfilled rows (%d newly inserted), %d year(s) touched",
                  length(parts), nrow(backfill_rows), n_inserted, length(touched_years)))

  invisible(publish(io, con, out_dir, tag = "current", source_kind = "live", touched_years = touched_years))
}

# ---- CLI dispatch -----------------------------------------------------------------
main <- function(mode, out_dir) {
  token <- Sys.getenv("VCS_SIGNALS_TOKEN")
  io <- list(
    graphql        = default_io(token)$graphql,
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
    metrics <- trimws(strsplit(Sys.getenv("VCS_METRICS", paste(BACKFILL_METRICS, collapse = ",")), ",")[[1]])
    unknown <- setdiff(metrics, names(METRIC_CONNECTIONS))
    if (length(unknown) > 0)
      stop(sprintf("fetch: unknown metric(s) in VCS_METRICS: %s", paste(unknown, collapse = ", ")))
    run_fetch_shard(io, out_dir, file.path(roster_dir, "vcs-signals-roster.db"), i, N, metrics = metrics)
  } else if (mode == "merge") {
    purge <- trimws(strsplit(Sys.getenv("VCS_PURGE_METRICS", ""), ",")[[1]])
    purge <- purge[nzchar(purge)]
    run_merge(io, out_dir, Sys.getenv("VCS_PARTS", "parts"), purge_metrics = purge)
  } else {
    stop("usage: backfill.R [enumerate|fetch|merge]")
  }
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  mode <- if (length(args) >= 1) args[1] else ""
  out_dir <- Sys.getenv("VCS_OUT", "out")
  main(mode, out_dir)
}
