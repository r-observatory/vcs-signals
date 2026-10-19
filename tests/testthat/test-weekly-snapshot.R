# export_snapshot_shard is in scripts/helpers.R, shared by weekly.R and gitlab_weekly.R.
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
