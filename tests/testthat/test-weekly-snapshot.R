# scripts/weekly.R uses repo-root-relative paths (source("scripts/config.R")
# etc.), same as scripts/backfill.R (see helper-setup.R), so it must be
# sourced with cwd temporarily chdir'd to the repo root. test-weekly-snapshot.R
# sorts before test-weekly.R, so it cannot rely on that file's own source()
# call having already run.
if (!exists("export_snapshot_shard")) {
  .wks_wd <- setwd(.repo_root)
  source(file.path(.repo_root, "scripts", "weekly.R"))
  setwd(.wks_wd)
}

test_that("export_snapshot_shard round-trips every weekly metric column", {
  tmp <- tempfile(fileext = ".db")
  rows <- data.frame(repo_id = "github.com/o/r", commits_total = 5L, contributors_total = 3L,
    median_days_to_close_issue = 4L, median_days_to_close_pr = 2L,
    median_open_issue_age_days = 12L, stringsAsFactors = FALSE)
  export_snapshot_shard(tmp, rows)
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp); on.exit(DBI::dbDisconnect(con))
  got <- DBI::dbReadTable(con, "snapshot")
  expect_equal(sort(names(got)), sort(c("repo_id", WEEKLY_METRICS)))
  expect_equal(got$median_days_to_close_issue, 4L)
  expect_equal(got$median_open_issue_age_days, 12L)
})
