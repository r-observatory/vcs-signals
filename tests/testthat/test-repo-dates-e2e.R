# The daily run takes each repository's dates from the gauge answer, and carries
# every per-repository value for a repository it did not reach.

.rd_one <- "github.com/tidyverse/ggplot2"   # R_a, the node gauges_one.json answers for
.rd_two <- "github.com/a/ok"                # R_1, which that answer leaves out

.rd_acquire <- function() data.frame(package = c("ggplot2", "okpkg"), origin = "cran",
  url_raw = c("https://github.com/tidyverse/ggplot2", "https://github.com/a/ok"),
  bugreports_raw = NA, stringsAsFactors = FALSE)

.rd_graphql <- function(query) {
  if (grepl("rateLimit", query)) return(list(data = list(nodes = list())))
  f <- if (grepl("followRenames", query)) "resolve_one.json" else "gauges_one.json"
  jsonlite::fromJSON(readLines(file.path("fixtures", f), warn = FALSE), simplifyVector = FALSE)
}

# A release whose recent shard holds both repositories and their prior summary rows,
# with the pipeline_state key that says the dates are the gauge's own when `key`.
.rd_release <- function(key) {
  rel <- tempfile("rel_dates_"); dir.create(rel)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbWriteTable(con, "repos", data.frame(
    repo_id = c(.rd_one, .rd_two), node_id = c("R_a", "R_1"), host = "github",
    host_domain = "github.com", owner = c("tidyverse", "a"), name = c("ggplot2", "ok"),
    name_with_owner = c("tidyverse/ggplot2", "a/ok"), supported = 1L, n_packages = 1L,
    first_seen = "2020-01-01", last_seen = "2020-01-01", status = "active",
    stringsAsFactors = FALSE), append = TRUE)
  DBI::dbWriteTable(con, "repo_packages", data.frame(repo_id = c(.rd_one, .rd_two),
    package = c("ggplot2", "okpkg"), origin = "cran", resolved_from = "url",
    stringsAsFactors = FALSE), append = TRUE)
  DBI::dbExecute(con, "INSERT INTO series_latest VALUES (?, 'releases_total', 40)", params = list(.rd_one))
  DBI::dbWriteTable(con, "vcs_signals_summary", data.frame(
    package = c("ggplot2", "okpkg"), origin = "cran", repo_id = c(.rd_one, .rd_two),
    license = "MIT", topics = "r", is_archived = 0L, last_commit_date = "2026-07-01T00:00:00Z",
    last_release_date = "2026-07-07", last_release_tag = c(NA, "v0.9.0"),
    repo_created_at = c(NA, "2019-03-04T05:06:07Z"), median_days_between_releases = c(30L, 12L),
    first_seen = "2020-01-01", last_seen = "2020-01-01", stringsAsFactors = FALSE), append = TRUE)
  DBI::dbExecute(con, "INSERT INTO pipeline_state (key, value) VALUES ('go_live', '2026-07-07')")
  if (key) DBI::dbExecute(con, "INSERT INTO pipeline_state (key, value) VALUES (?, ?)",
                          params = list(REPO_DATES_KEY, REPO_DATES_SOURCE))
  writeLines('{"summary":{"years":[]}}', file.path(rel, "manifest.json"))
  rel
}

.rd_run <- function(rel) {
  out <- tempfile("out_dates_"); dir.create(out)
  suppressMessages(capture.output(
    run_update(local_release_io(rel, acquire = .rd_acquire, graphql = .rd_graphql), out, list())))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  list(summary = DBI::dbGetQuery(con, "SELECT * FROM vcs_signals_summary ORDER BY repo_id"),
       state = DBI::dbReadTable(con, "pipeline_state"))
}

test_that("a collected repository takes its dates, tag and creation time from the gauge answer", {
  got <- .rd_run(.rd_release(key = TRUE))$summary
  one <- got[got$repo_id == .rd_one, ]
  expect_equal(one$last_commit_date, "2026-06-30T12:00:00Z")    # newest commit, not pushedAt
  expect_equal(one$last_release_date, "2026-01-10T00:00:00Z")   # latestRelease.publishedAt
  expect_equal(one$last_release_tag, "v3.5.0")
  expect_equal(one$repo_created_at, "2008-05-25T00:00:00Z")
  expect_equal(one$median_days_between_releases, 30L)           # carried; the daily run has no full history
})

test_that("a repository the run did not reach keeps every carried value", {
  got <- .rd_run(.rd_release(key = TRUE))$summary
  two <- got[got$repo_id == .rd_two, ]
  expect_equal(two$last_commit_date, "2026-07-01T00:00:00Z")
  expect_equal(two$last_release_date, "2026-07-07")
  expect_equal(two$last_release_tag, "v0.9.0")
  expect_equal(two$repo_created_at, "2019-03-04T05:06:07Z")
  expect_equal(two$license, "MIT")
  expect_equal(two$median_days_between_releases, 12L)
})

test_that("before the key exists an unreached repository's old dates go, and the run writes the key", {
  res <- .rd_run(.rd_release(key = FALSE))
  two <- res$summary[res$summary$repo_id == .rd_two, ]
  expect_true(is.na(two$last_commit_date))
  expect_true(is.na(two$last_release_date))
  expect_equal(two$license, "MIT")
  expect_equal(two$median_days_between_releases, 12L)
  expect_equal(res$summary$last_release_date[res$summary$repo_id == .rd_one], "2026-01-10T00:00:00Z")
  expect_equal(res$state$value[res$state$key == REPO_DATES_KEY], REPO_DATES_SOURCE)
})
