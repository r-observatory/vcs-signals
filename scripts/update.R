#!/usr/bin/env Rscript
# scripts/update.R - vcs-signals orchestration.
# run_update(io, out_dir, opts) drives five ordered stages behind an injected
# io: (1) SP1 resolve, (2) node-id resolution + lifecycle, (3) forward gauge
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

# ---- SP1 acquisition (unchanged) -------------------------------------------
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
  for (nm in c("repos", "repo_packages", "series_latest", "pipeline_state", "signals_series")) {
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

  # ---- Stage 1: SP1 resolve (acquire -> resolve_all -> build_repo_index ->
  # write_repo_tables), guarded by the SP1 universe guard against a
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

  # ---- Stage 2: node-id resolution + lifecycle (rename, gone) ------------
  needing <- DBI::dbGetQuery(con,
    "SELECT repo_id, owner, name FROM repos WHERE host = 'github' AND node_id IS NULL AND status = 'active'")
  resolved_ids <- resolve_node_ids(io, needing)
  update_repo_node_ids(con, resolved_ids)

  # ---- Stage 3: forward gauge collection over active github repos -------
  repo_map <- DBI::dbGetQuery(con,
    "SELECT node_id, repo_id FROM repos WHERE host = 'github' AND status = 'active' AND node_id IS NOT NULL")
  gauges <- collect_gauges(io, repo_map$node_id)
  snapshot_long <- gauges_to_long(gauges$snapshot, repo_map)

  # ---- Stage 4: materialize series + summary + go-live watermark --------
  prev_latest <- DBI::dbGetQuery(con, "SELECT repo_id, metric, value FROM series_latest")
  mat <- materialize_series(prev_latest, snapshot_long, today_s)

  if (nrow(mat$series_rows) > 0) {
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO signals_series (repo_id, date, metric, value) VALUES (?,?,?,?)",
      params = list(mat$series_rows$repo_id, mat$series_rows$date,
                    mat$series_rows$metric, mat$series_rows$value))
  }
  DBI::dbExecute(con, "DELETE FROM series_latest")
  if (nrow(mat$new_latest) > 0) {
    DBI::dbExecute(con, "INSERT INTO series_latest (repo_id, metric, value) VALUES (?,?,?)",
      params = list(mat$new_latest$repo_id, mat$new_latest$metric, mat$new_latest$value))
  }

  repos_all <- DBI::dbReadTable(con, "repos")
  rp_all <- DBI::dbReadTable(con, "repo_packages")
  series_all <- DBI::dbGetQuery(con, "SELECT repo_id, date, metric, value FROM signals_series")

  # Descriptive repo attributes (license/topics/is_archived/last_commit_date)
  # are not columns on the SP1 repos table (that schema is frozen per the
  # SP2 design); they come from this run's gauge snapshot instead, joined
  # onto repo_id via repo_map. A repo deferred this run (502, still pending)
  # simply carries NA descriptive attributes until it is next collected.
  attrs <- data.frame(repo_id = character(), license = character(), topics = character(),
                      is_archived = integer(), last_commit_date = character(), stringsAsFactors = FALSE)
  if (!is.null(gauges$snapshot) && nrow(gauges$snapshot) > 0 && nrow(repo_map) > 0) {
    sn <- merge(gauges$snapshot, repo_map, by = "node_id")
    pick <- function(col, default) if (col %in% names(sn)) sn[[col]] else rep(default, nrow(sn))
    attrs <- data.frame(repo_id = sn$repo_id,
                        license = pick("license", NA_character_),
                        topics = pick("topics", NA_character_),
                        is_archived = as.integer(pick("is_archived", NA_integer_)),
                        last_commit_date = pick("last_commit_date", NA_character_),
                        stringsAsFactors = FALSE)
  }
  repo_attrs <- merge(repos_all[, c("repo_id", "first_seen", "last_seen")], attrs,
                      by = "repo_id", all.x = TRUE)

  summary_df <- build_signals_summary(mat$new_latest, series_all, repo_attrs, rp_all, today_s)
  DBI::dbExecute(con, "DELETE FROM vcs_signals_summary")
  if (nrow(summary_df) > 0) DBI::dbWriteTable(con, "vcs_signals_summary", summary_df, append = TRUE)

  go_live <- DBI::dbGetQuery(con, "SELECT value FROM pipeline_state WHERE key = 'go_live'")
  if (nrow(go_live) == 0) {
    DBI::dbExecute(con, "INSERT INTO pipeline_state (key, value) VALUES ('go_live', ?)", params = list(today_s))
  }

  # ---- Stage 5: publish --------------------------------------------------
  invisible(publish(io, con, out_dir, tag, source_kind = "live", force_full = force_full))
}

# ---- gh-release IO for the real run ----------------------------------------
gh_release_exists <- function(repo, tag = "current") {
  st <- suppressWarnings(system2("gh", c("release", "view", tag, "--repo", repo),
                                 stdout = FALSE, stderr = FALSE))
  identical(as.integer(st), 0L)
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
