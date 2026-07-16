# Working directory during test_dir() is tests/testthat (see
# helper-setup.R), so scripts/ lives two levels up.
.scripts_dir <- file.path("..", "..", "scripts")

test_that("update.R carries release facts forward and calls with compute_release_facts=FALSE", {
  src <- readLines(file.path(.scripts_dir, "update.R"))
  expect_true(any(grepl("compute_release_facts\\s*=\\s*FALSE", src)))
  expect_true(any(grepl("last_release_date", src)))              # in prev_summary_attrs SELECT
  expect_true(any(grepl("median_days_between_releases", src)))
})

test_that("weekly.R computes release facts from full history", {
  expect_true(any(grepl("compute_release_facts\\s*=\\s*TRUE", readLines(file.path(.scripts_dir, "weekly.R")))))
})
