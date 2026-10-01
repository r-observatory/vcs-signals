# The summary's per-repository values, which every summary builder carries for a
# repository its run did not collect.

.attr_row <- function(repo_id, ...) {
  row <- data.frame(repo_id = repo_id, license = "MIT", topics = "r", is_archived = 0L,
                    last_commit_date = "2026-04-01T10:00:00Z", last_release_date = "2026-03-01T09:00:00Z",
                    last_release_tag = "v1.0.0", repo_created_at = "2015-02-03T04:05:06Z",
                    median_days_between_releases = 45L, stringsAsFactors = FALSE)
  over <- list(...)
  for (nm in names(over)) row[[nm]] <- over[[nm]]
  row
}

test_that("every carried column is a summary column, the two new ones included", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  have <- DBI::dbGetQuery(con, "PRAGMA table_info(vcs_signals_summary)")$name
  expect_true(all(REPO_ATTR_COLS %in% have))
  expect_true(all(c("last_release_tag", "repo_created_at") %in% REPO_ATTR_COLS))
})

test_that("prior_repo_attrs reads one typed row per repository", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  row <- .attr_row("github.com/a/one")
  DBI::dbWriteTable(con, "vcs_signals_summary",
    cbind(package = c("p1", "p2"), origin = "cran", rbind(row, row), stringsAsFactors = FALSE),
    append = TRUE)
  got <- prior_repo_attrs(con)
  expect_equal(nrow(got), 1L)
  expect_equal(names(got), c("repo_id", REPO_ATTR_COLS))
  expect_equal(got$last_release_tag, "v1.0.0")
  expect_type(got$is_archived, "integer")
  expect_type(got$median_days_between_releases, "integer")
})

test_that("a summary written before the new columns existed reads them as NA", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE vcs_signals_summary (package TEXT, origin TEXT, repo_id TEXT,
    license TEXT, topics TEXT, is_archived INTEGER, last_commit_date TEXT, last_release_date TEXT,
    median_days_between_releases INTEGER)")
  DBI::dbExecute(con, "INSERT INTO vcs_signals_summary VALUES
    ('p1', 'cran', 'github.com/a/one', 'MIT', 'r', 0, '2026-07-07', '2026-07-07', 30)")
  got <- prior_repo_attrs(con)
  expect_equal(got$license, "MIT")
  expect_equal(got$median_days_between_releases, 30L)
  expect_true(is.na(got$last_release_tag))
  expect_true(is.na(got$repo_created_at))
  expect_type(got$last_release_tag, "character")
})

test_that("a collected repository takes this run's values and keeps its cadence; another keeps its row", {
  prior <- rbind(.attr_row("github.com/a/one"), .attr_row("github.com/b/two"))
  fresh <- .attr_row("github.com/a/one", license = "GPL-3", last_commit_date = "2026-09-29T08:00:00Z",
                     last_release_date = "2026-04-09T15:21:33Z", last_release_tag = "v3.6.6",
                     median_days_between_releases = NA_integer_)
  got <- carry_repo_attrs(fresh, prior)
  one <- got[got$repo_id == "github.com/a/one", ]
  expect_equal(one$license, "GPL-3")
  expect_equal(one$last_commit_date, "2026-09-29T08:00:00Z")
  expect_equal(one$last_release_date, "2026-04-09T15:21:33Z")
  expect_equal(one$last_release_tag, "v3.6.6")
  expect_equal(one$median_days_between_releases, 45L)
  two <- got[got$repo_id == "github.com/b/two", ]
  expect_equal(unname(unlist(two[REPO_ATTR_COLS])), unname(unlist(prior[2, REPO_ATTR_COLS])))
})

test_that("a collected repository with no release loses a carried release date", {
  prior <- .attr_row("github.com/a/one", last_release_date = "2026-07-07", last_release_tag = NA_character_)
  fresh <- .attr_row("github.com/a/one", last_release_date = NA_character_, last_release_tag = NA_character_)
  got <- carry_repo_attrs(fresh, prior)
  expect_true(is.na(got$last_release_date))
  expect_true(is.na(got$last_release_tag))
})

test_that("with the reset, a repository not collected loses only its two dates", {
  prior <- rbind(.attr_row("github.com/a/one"), .attr_row("github.com/b/two"))
  got <- carry_repo_attrs(.attr_row("github.com/a/one"), prior, reset = TRUE)
  two <- got[got$repo_id == "github.com/b/two", ]
  expect_true(is.na(two$last_commit_date))
  expect_true(is.na(two$last_release_date))
  expect_equal(two$last_release_tag, "v1.0.0")
  expect_equal(two$license, "MIT")
  expect_equal(two$median_days_between_releases, 45L)
  expect_equal(got$last_commit_date[got$repo_id == "github.com/a/one"], "2026-04-01T10:00:00Z")
})

# ---- every summary builder ------------------------------------------------------
# weekly.R, backfill.R and ai_backfill.R all define run_merge, so each is loaded
# into its own environment.
.ra_env <- function(script) {
  env <- new.env(parent = globalenv())
  withr::with_dir(.repo_root, sys.source(file.path("scripts", script), envir = env))
  env
}
.ra_weekly <- .ra_env("weekly.R")
.ra_ai <- .ra_env("ai_backfill.R")
.ra_rid <- "github.com/a/ok"

# A published release with one repository whose summary row sets every carried
# column, and a release series whose rises are 45 days apart, so the weekly
# merge's recomputed cadence is the one the row carries.
.ra_release <- function() {
  rel <- tempfile("rel_attrs_"); dir.create(rel)
  today <- Sys.Date()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbWriteTable(con, "repos", data.frame(repo_id = .ra_rid, node_id = "R_1", host = "github",
    host_domain = "github.com", owner = "a", name = "ok", name_with_owner = "a/ok", supported = 1L,
    n_packages = 1L, first_seen = "2026-01-01", last_seen = format(today), status = "active",
    stringsAsFactors = FALSE), append = TRUE)
  DBI::dbWriteTable(con, "repo_packages", data.frame(repo_id = .ra_rid, package = "pkgA",
    origin = "cran", resolved_from = "url", stringsAsFactors = FALSE), append = TRUE)
  DBI::dbExecute(con, "INSERT INTO signals_series VALUES (?, ?, 'releases_total', 1), (?, ?, 'releases_total', 2)",
                 params = list(.ra_rid, format(today - 100), .ra_rid, format(today - 55)))
  DBI::dbExecute(con, "INSERT INTO series_latest VALUES (?, 'releases_total', 2)", params = list(.ra_rid))
  DBI::dbExecute(con, "INSERT INTO pipeline_state (key, value) VALUES ('go_live', ?)",
                 params = list(format(today - 1)))
  DBI::dbWriteTable(con, "vcs_signals_summary", cbind(package = "pkgA", origin = "cran",
    .attr_row(.ra_rid), first_seen = "2026-01-01", last_seen = format(today),
    stringsAsFactors = FALSE), append = TRUE)
  read <- function(nm) DBI::dbReadTable(con, nm)
  recent <- file.path(rel, "vcs-signals-recent.db")
  export_series_shard(recent, extract_recent_rows(con, today, RECENT_WINDOW))
  .embed_recent_tables(con, recent)
  export_summary_shard(file.path(rel, "vcs-signals-summary.db"), read("vcs_signals_summary"),
                       read("repos"), read("repo_packages"), read("vcs_ai_signals"),
                       read("vcs_dev_tooling"),
                       extra = stats::setNames(lapply(SUMMARY_EXTRA_TABLES, read), SUMMARY_EXTRA_TABLES))
  write_manifest(file.path(rel, "manifest.json"), character(0), "current",
                 list(source_kind = "live", years = list()))
  rel
}

.ra_published <- function(rel) {
  s <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-summary.db"))
  r <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-recent.db"))
  on.exit({ DBI::dbDisconnect(s); DBI::dbDisconnect(r) })
  list(summary = DBI::dbGetQuery(s, "SELECT * FROM vcs_signals_summary"),
       state = DBI::dbReadTable(r, "pipeline_state"))
}

.ra_weekly_parts <- function() {
  p <- tempfile("ra_weekly_parts_"); dir.create(p)
  .ra_weekly$export_snapshot_shard(file.path(p, "vcs-signals-shard-0.db"), data.frame(
    repo_id = .ra_rid, commits_total = 500L, contributors_total = 10L,
    median_days_to_close_issue = NA_integer_, median_days_to_close_pr = NA_integer_,
    median_open_issue_age_days = NA_integer_, stringsAsFactors = FALSE))
  p
}

.ra_ai_parts <- function() {
  p <- tempfile("ra_ai_parts_"); dir.create(p)
  .ra_ai$export_ai_shard(file.path(p, "vcs-ai-shard-0.db"), data.frame(
    repo_id = .ra_rid, tool = "copilot", first_seen_date = "2026-02-01", first_seen_censored = 0L,
    evidence_tiers = "A", authored = 1L, last_confirmed_date = format(Sys.Date()),
    stringsAsFactors = FALSE))
  p
}

.ra_expect_carried <- function(got) {
  want <- .attr_row(.ra_rid)
  for (cn in REPO_ATTR_COLS) expect_equal(got[[cn]], want[[cn]], info = cn)
}

test_that("the weekly merge keeps every carried value and recomputes the cadence from the rises", {
  rel <- .ra_release()
  suppressMessages(.ra_weekly$run_merge(local_release_io(rel), tempfile("ra_wk_"), .ra_weekly_parts()))
  got <- .ra_published(rel)
  .ra_expect_carried(got$summary)
  expect_equal(got$summary$commits_total, 500L)   # the merge did run
})

test_that("the AI merge keeps every carried value", {
  rel <- .ra_release()
  suppressMessages(.ra_ai$run_merge(local_release_io(rel), tempfile("ra_ai_"), .ra_ai_parts()))
  .ra_expect_carried(.ra_published(rel)$summary)
})

test_that("a merge before the first daily run with the gauge's dates only carries, and writes no key", {
  for (merge in c("weekly", "ai")) {
    rel <- .ra_release()
    if (merge == "weekly")
      suppressMessages(.ra_weekly$run_merge(local_release_io(rel), tempfile("ra_wk_"), .ra_weekly_parts()))
    else
      suppressMessages(.ra_ai$run_merge(local_release_io(rel), tempfile("ra_ai_"), .ra_ai_parts()))
    got <- .ra_published(rel)
    expect_equal(got$summary$last_commit_date, "2026-04-01T10:00:00Z", info = merge)
    expect_false(REPO_DATES_KEY %in% got$state$key, info = merge)
  }
})
