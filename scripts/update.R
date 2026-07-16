#!/usr/bin/env Rscript
# scripts/update.R - vcs-signals orchestration.
# run_update(io, out_dir, opts) drives five ordered stages behind an injected
# io: (1) resolve, (2) node-id resolution + lifecycle, (3) forward gauge
# collection, (4) series/summary materialization + go-live watermark, (5)
# publish. main() builds the real io (network acquisition, GitHub GraphQL,
# gh-release download/upload) and calls run_update(); the hermetic test
# drives run_update() directly with a fake io, so no live network runs in CI.
#
# Sourced from tests/testthat/helper-setup.R (with cwd temporarily chdir'd to
# the repo root) as well as run directly via Rscript from the repo root, so
# these paths are always repo-root-relative.
source("scripts/config.R")
source("scripts/helpers.R")
source("scripts/github.R")
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

# ---- acquisition ------------------------------------------------------------
acquire_cran <- function() {
  pdb <- tools::CRAN_package_db()
  pdb <- pdb[!duplicated(pdb$Package), ]
  data.frame(package = pdb$Package, origin = "cran",
             url_raw = pdb$URL, bugreports_raw = pdb$BugReports, stringsAsFactors = FALSE)
}

fetch_views <- function(u) {
  txt <- tryCatch(paste(readLines(url(u), warn = FALSE), collapse = "\n"),
                  error = function(e) NA_character_)
  if (is.na(txt) || !grepl("(^|\n)Package:", txt))
    stop(sprintf("VIEWS fetch failed or empty: %s", u))
  txt
}

acquire_bioc <- function() {
  parts <- lapply(VIEWS_URLS, function(u) {
    m <- read.dcf(textConnection(fetch_views(u)))
    g <- function(f) if (f %in% colnames(m)) as.character(m[, f]) else rep(NA_character_, nrow(m))
    data.frame(package = g("Package"), origin = "bioc",
               url_raw = g("URL"), bugreports_raw = g("BugReports"), stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, parts)
  df[!duplicated(df$package), ]
}

# ---- working-DB bootstrap --------------------------------------------------
# Seeds a fresh out_dir/_working.db from the previously-published recent
# shard (repos, repo_packages, series_latest, pipeline_state, and the
# RECENT_WINDOW=400d tail of signals_series) when a "current" release
# already exists, so a run in a fresh CI checkout still preserves
# first_seen/node_id/series history instead of starting cold every day.
# signals_series matters here beyond change-detection: publish() re-exports
# every year present in the working DB's signals_series on every run, and
# RECENT_WINDOW (400d) is deliberately > 365d so this seed always carries a
# complete current-year history forward; without it, a fresh daily checkout
# would silently re-publish year/recent shards containing only that day's
# changed rows, discarding all prior accumulated history. Older, fully-past
# years are correctly left untouched (out of the 400d window, immutable).
# A no-op (empty working DB, first-run shape) when no release exists yet or
# the prior recent shard cannot be pulled.
seed_working_db <- function(io, out_dir, working_path) {
  if (file.exists(working_path)) unlink(working_path)
  if (!isTRUE(io$release_exists())) return(invisible(FALSE))
  if (!isTRUE(io$download("vcs-signals-recent.db", out_dir))) return(invisible(FALSE))
  prior_path <- file.path(out_dir, "vcs-signals-recent.db")
  if (!file.exists(prior_path)) return(invisible(FALSE))

  pcon <- DBI::dbConnect(RSQLite::SQLite(), prior_path)
  on.exit(DBI::dbDisconnect(pcon), add = TRUE)
  wcon <- DBI::dbConnect(RSQLite::SQLite(), working_path)
  on.exit(DBI::dbDisconnect(wcon), add = TRUE)
  ensure_repo_schema(wcon)
  ensure_series_schema(wcon)
  # vcs_signals_summary is included so that I4's stage-4 carry-forward has a
  # prior summary row to read for a repo not collected this run: it is
  # embedded into the published recent shard by .embed_recent_tables, so it
  # must be seeded back the same way the other four tables are.
  for (nm in c("repos", "repo_packages", "series_latest", "pipeline_state",
               "signals_series", "vcs_signals_summary")) {
    if (DBI::dbExistsTable(pcon, nm)) {
      df <- DBI::dbReadTable(pcon, nm)
      if (nrow(df) > 0) DBI::dbWriteTable(wcon, nm, df, append = TRUE)
    }
  }
  invisible(TRUE)
}

# ---- the five-stage orchestrator -------------------------------------------
#' Run one full vcs-signals update pass behind an injected io.
#'
#' io must expose: acquire() -> data.frame(package, origin, url_raw,
#' bugreports_raw); graphql(query) -> parsed GraphQL response (list, with
#' $data/$errors, or throws on transport error); release_exists() ->
#' logical; download(pattern, dir) -> logical; upload(path) -> invisible.
#' opts$force_full re-exports and re-uploads every shard regardless of the
#' change-gate; opts$tag overrides the release tag (default "current").
run_update <- function(io, out_dir, opts = list()) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  today <- Sys.Date()
  today_s <- format(today)
  force_full <- isTRUE(opts$force_full)
  tag <- if (!is.null(opts$tag)) opts$tag else "current"

  working_path <- file.path(out_dir, "_working.db")
  seed_working_db(io, out_dir, working_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), working_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con)
  ensure_series_schema(con)

  # ---- Stage 1: resolve (acquire -> resolve_all -> build_repo_index ->
  # write_repo_tables), guarded by the universe guard against a
  # catastrophic drop in resolved packages/repos. -------------------------
  input <- io$acquire()
  resolved <- resolve_all(input)
  idx <- build_repo_index(resolved)

  prev_pkgs <- DBI::dbGetQuery(con, "SELECT COUNT(DISTINCT package || origin) n FROM repo_packages")$n
  prev_repos <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM repos WHERE status IN ('active','moved')")$n
  curr_pkgs <- length(unique(paste(idx$repo_packages$package, idx$repo_packages$origin)))
  universe_guard(prev_pkgs, prev_repos, curr_pkgs, nrow(idx$repos))

  write_repo_tables(con, idx$repos, idx$repo_packages, today_s)
  print_coverage(input, resolved, idx)

  # ---- Rate-limit preflight (I3): below-reserve skips resolve + collection
  # + materialization entirely, publishing a clean heartbeat instead of
  # spending down points on a run that cannot safely complete. -------------
  rl <- graphql_rate_remaining(io)
  if (rl < POINT_RESERVE) {
    message(sprintf(
      "graphql rate remaining (%s) below reserve (%d); skipping node-id resolution and collection this run",
      rl, POINT_RESERVE))
    return(invisible(publish(io, con, out_dir, tag, source_kind = "live",
                              force_full = force_full, touched_years = character(0))))
  }

  # ---- Stage 2: node-id resolution + lifecycle (rename, gone) ------------
  needing <- DBI::dbGetQuery(con,
    "SELECT repo_id, owner, name FROM repos WHERE host = 'github' AND node_id IS NULL AND status = 'active'")
  resolved_ids <- resolve_node_ids(io, needing)
  update_repo_node_ids(con, resolved_ids)
  n_id_resolved <- sum(resolved_ids$status == "active")
  n_id_gone <- sum(resolved_ids$status == "gone")
  n_id_deferred <- nrow(needing) - nrow(resolved_ids)
  cat(sprintf("node ids: %d resolved, %d deferred, %d gone\n", n_id_resolved, n_id_deferred, n_id_gone))

  # ---- Stage 3: forward gauge collection over active github repos -------
  repo_map <- DBI::dbGetQuery(con,
    "SELECT node_id, repo_id FROM repos WHERE host = 'github' AND status = 'active' AND node_id IS NOT NULL")
  gauges <- collect_gauges(io, repo_map$node_id)
  snapshot_long <- gauges_to_long(gauges$snapshot, repo_map)
  n_gauges_collected <- if (!is.null(gauges$snapshot)) nrow(gauges$snapshot) else 0L
  cat(sprintf("gauges: collected %d repos, %d deferred\n", n_gauges_collected, length(gauges$deferred)))

  # ---- Stage 4: materialize series + summary + go-live watermark --------
  # I4 floor: when this run collected nothing at all (every repo deferred -
  # a sustained outage, or the last surviving repo 502ing on every retry),
  # gauges$snapshot is NULL/empty. Skip every stage-4 write outright (no
  # series_latest/vcs_signals_summary rebuild) and fall through to a
  # publish() heartbeat, rather than rebuild from an empty snapshot and
  # publish an all-NA dataset over the top of good accumulated history.
  if (!is.null(gauges$snapshot) && nrow(gauges$snapshot) > 0) {
    prev_latest <- DBI::dbGetQuery(con, "SELECT repo_id, metric, value FROM series_latest")
    mat <- materialize_series(prev_latest, snapshot_long, today_s)

    if (nrow(mat$series_rows) > 0) {
      DBI::dbExecute(con,
        "INSERT OR REPLACE INTO signals_series (repo_id, date, metric, value) VALUES (?,?,?,?)",
        params = list(mat$series_rows$repo_id, mat$series_rows$date,
                      mat$series_rows$metric, mat$series_rows$value))
    }
    # Upsert, never a blanket delete: a repo not collected this run (deferred,
    # rate-limited, still 502ing) keeps its prior series_latest value instead
    # of vanishing from the published snapshot. materialize_series() above
    # was still called with prev_latest (the pre-upsert values).
    if (nrow(mat$new_latest) > 0) {
      DBI::dbExecute(con,
        "INSERT OR REPLACE INTO series_latest (repo_id, metric, value) VALUES (?,?,?)",
        params = list(mat$new_latest$repo_id, mat$new_latest$metric, mat$new_latest$value))
    }

    repos_all <- DBI::dbReadTable(con, "repos")
    rp_all <- DBI::dbReadTable(con, "repo_packages")
    series_all <- DBI::dbGetQuery(con, "SELECT repo_id, date, metric, value FROM signals_series")

    # Descriptive repo attributes (license/topics/is_archived/last_commit_date)
    # are not columns on the repos table (that schema is frozen per the
    # design). For a repo collected this run they come from this run's
    # gauge snapshot, joined onto repo_id via repo_map; for a repo NOT
    # collected this run they are carried forward from the prior
    # vcs_signals_summary row (read here, before that table is rebuilt
    # below), so a deferred repo keeps its last-known descriptive attributes
    # instead of going NA. A repo with neither (never collected) gets NA.
    attrs <- data.frame(repo_id = character(), license = character(), topics = character(),
                        is_archived = integer(), last_commit_date = character(), stringsAsFactors = FALSE)
    if (nrow(repo_map) > 0) {
      sn <- merge(gauges$snapshot, repo_map, by = "node_id")
      pick <- function(col, default) if (col %in% names(sn)) sn[[col]] else rep(default, nrow(sn))
      attrs <- data.frame(repo_id = sn$repo_id,
                          license = pick("license", NA_character_),
                          topics = pick("topics", NA_character_),
                          is_archived = as.integer(pick("is_archived", NA_integer_)),
                          last_commit_date = pick("pushed_at", NA_character_),
                          stringsAsFactors = FALSE)
    }
    prev_summary_attrs <- DBI::dbGetQuery(con,
      "SELECT repo_id, license, topics, is_archived, last_commit_date,
              last_release_date, median_days_between_releases
         FROM vcs_signals_summary WHERE repo_id IS NOT NULL")
    if (nrow(prev_summary_attrs) > 0) {
      prev_summary_attrs <- prev_summary_attrs[!duplicated(prev_summary_attrs$repo_id), ]
      prev_summary_attrs$is_archived <- as.integer(prev_summary_attrs$is_archived)
    }
    # last_release_date/median_days_between_releases have no fresh source in
    # this run's gauge snapshot (attrs), so - unlike the descriptive fields
    # below, which prefer this run's fresh attrs when available - they always
    # carry forward from the prior summary for every repo, collected this run
    # or not; build_signals_summary(compute_release_facts = FALSE) uses these
    # as the carry-forward floor.
    release_facts <- prev_summary_attrs[, c("repo_id", "last_release_date", "median_days_between_releases")]
    descriptive_prev <- prev_summary_attrs[!(prev_summary_attrs$repo_id %in% attrs$repo_id),
                                            c("repo_id", "license", "topics", "is_archived", "last_commit_date")]
    combined_attrs <- rbind(attrs, descriptive_prev)
    repo_attrs <- merge(repos_all[, c("repo_id", "first_seen", "last_seen")], combined_attrs,
                        by = "repo_id", all.x = TRUE)
    repo_attrs <- merge(repo_attrs, release_facts, by = "repo_id", all.x = TRUE)

    # Built from the FULL post-upsert series_latest (every repo, including
    # ones deferred this run), not just this run's snapshot, so a deferred
    # repo keeps its numeric values in the summary too.
    latest_all <- DBI::dbGetQuery(con, "SELECT repo_id, metric, value FROM series_latest")
    # Recent-window collection only (no full history), so release cadence is
    # never recomputed here - it is carried forward via repo_attrs above.
    summary_df <- build_signals_summary(latest_all, series_all, repo_attrs, rp_all, today_s,
                                        compute_release_facts = FALSE)
    DBI::dbExecute(con, "DELETE FROM vcs_signals_summary")
    if (nrow(summary_df) > 0) DBI::dbWriteTable(con, "vcs_signals_summary", summary_df, append = TRUE)

    go_live <- DBI::dbGetQuery(con, "SELECT value FROM pipeline_state WHERE key = 'go_live'")
    if (nrow(go_live) == 0) {
      DBI::dbExecute(con, "INSERT INTO pipeline_state (key, value) VALUES ('go_live', ?)", params = list(today_s))
    }

    # touched_years is derived from mat$series_rows (the change-only rows
    # just materialized into signals_series this run), never from the
    # working DB's full signals_series - so a forward run only re-exports
    # the current year's shard. See publish()'s touched_years documentation
    # in scripts/helpers.R for why this matters.
    touched_years <- unique(substr(mat$series_rows$date, 1, 4))
  } else {
    message("stage-4 floor: collection returned nothing this run; skipping series/summary rebuild, publishing heartbeat")
    touched_years <- character(0)
  }

  # ---- Stage 5: publish --------------------------------------------------
  invisible(publish(io, con, out_dir, tag, source_kind = "live", force_full = force_full,
                     touched_years = touched_years))
}

# ---- gh-release IO for the real run ----------------------------------------
#' Whether the "current" (or `tag`) release exists on `repo` - fail CLOSED.
#'
#' A plain exit-status check cannot distinguish "release not found" from any
#' other `gh` failure (auth expired, rate limited, network blip, GitHub
#' outage), and this function's FALSE answer is read by both
#' protect_history_pull and seed_working_db as "no prior release, start
#' cold" - which would silently clobber accumulated history if returned for
#' a merely-transient error. So: exit 0 -> TRUE; exit non-zero AND the
#' captured output names a genuine not-found -> FALSE; any other non-zero
#' exit -> stop(), aborting the run rather than guessing.
gh_release_exists <- function(repo, tag = "current") {
  out <- suppressWarnings(system2("gh", c("release", "view", tag, "--repo", repo),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  status <- if (is.null(status)) 0L else as.integer(status)
  if (identical(status, 0L)) return(TRUE)
  text <- paste(out, collapse = "\n")
  not_found <- grepl("release not found", text, ignore.case = TRUE) ||
    grepl("HTTP 404", text, ignore.case = TRUE)
  if (not_found) return(FALSE)
  stop(sprintf("gh release view failed ambiguously, aborting to avoid clobbering history: %s", text))
}

gh_release_download <- function(repo, pattern, dir, tag = "current") {
  st <- suppressWarnings(system2("gh", c("release", "download", tag, "--repo", repo,
    "--pattern", pattern, "--dir", dir, "--clobber"), stdout = TRUE, stderr = TRUE))
  code <- attr(st, "status")
  is.null(code) || identical(as.integer(code), 0L)
}

gh_release_upload <- function(repo, path, tag = "current") {
  system2("gh", c("release", "upload", tag, "--repo", repo, path, "--clobber"),
          stdout = TRUE, stderr = TRUE)
  invisible(NULL)
}

main <- function(out_dir) {
  token <- Sys.getenv("VCS_SIGNALS_TOKEN")
  io <- list(
    acquire = function() rbind(acquire_cran(), acquire_bioc()),
    graphql = default_io(token)$graphql,
    release_exists = function() gh_release_exists(RELEASE_REPO),
    download = function(pattern, dir) gh_release_download(RELEASE_REPO, pattern, dir),
    upload = function(path) gh_release_upload(RELEASE_REPO, path))
  force_full <- tolower(Sys.getenv("FORCE_FULL_REBUILD", "")) %in% c("true", "1", "yes")
  res <- run_update(io, out_dir, list(force_full = force_full))
  cat("Changed shards:",
      if (length(res$changed_shards)) paste(res$changed_shards, collapse = ", ") else "(none)", "\n")
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1) args[1] else "out"
  main(out_dir)
}
