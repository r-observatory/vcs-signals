test_that("build_signals_summary computes ratio, medians, and release facts", {
  latest <- data.frame(
    repo_id = "github.com/o/r",
    metric  = c("prs_merged", "prs_closed", "median_days_to_close_issue",
                "median_days_to_close_pr", "median_open_issue_age_days"),
    value   = c(30L, 10L, 4L, 2L, 12L), stringsAsFactors = FALSE)
  series <- data.frame(
    repo_id = "github.com/o/r", metric = "releases_total",
    date = c("2024-01-01", "2024-01-11", "2024-01-31"), value = c(1L, 2L, 3L),
    stringsAsFactors = FALSE)
  repos <- data.frame(repo_id = "github.com/o/r", first_seen = "2024-01-01",
    last_seen = "2024-02-01", last_commit_date = NA_character_, license = NA_character_,
    topics = NA_character_, is_archived = 0L,
    last_release_date = NA_character_, median_days_between_releases = NA_integer_,
    stringsAsFactors = FALSE)
  rp <- data.frame(package = "pkg", origin = "cran", repo_id = "github.com/o/r",
    stringsAsFactors = FALSE)

  out <- build_signals_summary(latest, series, repos, rp, "2024-02-01",
                               compute_release_facts = TRUE)
  expect_equal(out$pr_merge_ratio, 75L)
  expect_equal(out$median_days_to_close_issue, 4L)
  expect_equal(out$median_days_to_close_pr, 2L)
  expect_equal(out$median_open_issue_age_days, 12L)
  expect_equal(out$last_release_date, "2024-01-31")
  expect_equal(out$median_days_between_releases, 15L)   # gaps 10, 20 -> 15
})

test_that("without full history, cadence is carried forward not recomputed", {
  latest <- data.frame(repo_id = "github.com/o/r", metric = "prs_merged",
    value = 1L, stringsAsFactors = FALSE)
  # only one recent release row in the window -> cadence uncomputable here
  series <- data.frame(repo_id = "github.com/o/r", metric = "releases_total",
    date = "2024-02-01", value = 9L, stringsAsFactors = FALSE)
  repos <- data.frame(repo_id = "github.com/o/r", first_seen = "2020-01-01",
    last_seen = "2024-02-01", last_commit_date = NA_character_, license = NA_character_,
    topics = NA_character_, is_archived = 0L,
    last_release_date = "2024-02-01", median_days_between_releases = 42L,
    stringsAsFactors = FALSE)
  rp <- data.frame(package = "pkg", origin = "cran", repo_id = "github.com/o/r",
    stringsAsFactors = FALSE)

  out <- build_signals_summary(latest, series, repos, rp, "2024-02-02",
                               compute_release_facts = FALSE)
  expect_equal(out$median_days_between_releases, 42L)   # carried forward
  expect_equal(out$last_release_date, "2024-02-01")     # recomputed max == carried value
})

test_that("last_release_date carries forward when the window has no release rows", {
  latest <- data.frame(repo_id = "github.com/o/r", metric = "prs_merged",
    value = 1L, stringsAsFactors = FALSE)
  series <- data.frame(repo_id = character(), metric = character(),
    date = character(), value = integer(), stringsAsFactors = FALSE)  # no releases_total rows
  repos <- data.frame(repo_id = "github.com/o/r", first_seen = "2020-01-01",
    last_seen = "2024-02-01", last_commit_date = NA_character_, license = NA_character_,
    topics = NA_character_, is_archived = 0L,
    last_release_date = "2023-05-05", median_days_between_releases = 30L,
    stringsAsFactors = FALSE)
  rp <- data.frame(package = "pkg", origin = "cran", repo_id = "github.com/o/r",
    stringsAsFactors = FALSE)
  out <- build_signals_summary(latest, series, repos, rp, "2024-02-02",
                               compute_release_facts = FALSE)
  expect_equal(out$last_release_date, "2023-05-05")   # computed NA -> carried forward
})
