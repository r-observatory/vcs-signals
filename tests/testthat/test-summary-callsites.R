# Working directory during test_dir() is tests/testthat (see
# helper-setup.R), so scripts/ lives two levels up.
.scripts_dir <- file.path("..", "..", "scripts")

test_that("update.R carries the per-repository values through prior_repo_attrs and never recomputes cadence", {
  src <- readLines(file.path(.scripts_dir, "update.R"))
  expect_true(any(grepl("compute_release_facts\\s*=\\s*FALSE", src)))
  expect_true(any(grepl("carry_repo_attrs(fresh, prior_repo_attrs(con)", src, fixed = TRUE)))
  expect_false(any(grepl("pushed_at", src, fixed = TRUE)))
})

test_that("weekly.R computes release facts from full history", {
  expect_true(any(grepl("compute_release_facts\\s*=\\s*TRUE", readLines(file.path(.scripts_dir, "weekly.R")))))
})

test_that("the weekly and AI merges read the carried values through prior_repo_attrs", {
  for (f in c("weekly.R", "ai_backfill.R")) {
    src <- readLines(file.path(.scripts_dir, f))
    expect_true(any(grepl("prior_repo_attrs(con)", src, fixed = TRUE)), info = f)
    expect_false(any(grepl("SELECT repo_id, license, topics", src, fixed = TRUE)), info = f)
  }
})
