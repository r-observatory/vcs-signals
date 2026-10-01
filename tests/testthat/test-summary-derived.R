.sd_repos <- function(...) {
  r <- data.frame(repo_id = "github.com/o/r", first_seen = "2024-01-01", last_seen = "2024-02-01",
    last_commit_date = NA_character_, license = NA_character_, topics = NA_character_,
    is_archived = 0L, last_release_date = NA_character_, last_release_tag = NA_character_,
    repo_created_at = NA_character_, median_days_between_releases = NA_integer_,
    stringsAsFactors = FALSE)
  over <- list(...)
  for (nm in names(over)) r[[nm]] <- over[[nm]]
  r
}
.sd_rp <- data.frame(package = "pkg", origin = "cran", repo_id = "github.com/o/r",
                     stringsAsFactors = FALSE)
.sd_releases <- function(date, value)
  data.frame(repo_id = rep("github.com/o/r", length(date)), metric = rep("releases_total", length(date)),
             date = date, value = value, stringsAsFactors = FALSE)

test_that("build_signals_summary computes ratio, medians, and the cadence from the rises", {
  latest <- data.frame(
    repo_id = "github.com/o/r",
    metric  = c("prs_merged", "prs_closed", "median_days_to_close_issue",
                "median_days_to_close_pr", "median_open_issue_age_days"),
    value   = c(30L, 10L, 4L, 2L, 12L), stringsAsFactors = FALSE)
  series <- .sd_releases(c("2024-01-01", "2024-01-11", "2024-01-31"), c(1L, 2L, 3L))
  out <- build_signals_summary(latest, series, .sd_repos(last_release_date = "2024-01-31T12:00:00Z"),
                               .sd_rp, "2024-02-01", compute_release_facts = TRUE, go_live = "2026-07-07")
  expect_equal(out$pr_merge_ratio, 75L)
  expect_equal(out$median_days_to_close_issue, 4L)
  expect_equal(out$median_days_to_close_pr, 2L)
  expect_equal(out$median_open_issue_age_days, 12L)
  expect_equal(out$last_release_date, "2024-01-31T12:00:00Z")
  expect_equal(out$median_days_between_releases, 15L)   # gaps 10, 20 -> 15
})

test_that("the go-live row that repeats the count is not a release", {
  # A backfilled series, then the go-live day wrote the same count again.
  series <- .sd_releases(c("2025-01-01", "2025-01-11", "2026-07-07"), c(1L, 2L, 2L))
  out <- build_signals_summary(data.frame(repo_id = character(), metric = character(), value = integer()),
                               series, .sd_repos(), .sd_rp, "2026-07-12",
                               compute_release_facts = TRUE, go_live = "2026-07-07")
  expect_equal(out$median_days_between_releases, 10L)   # the 542-day gap to go-live is not counted
})

test_that("without full history, cadence is carried forward, not recomputed", {
  series <- .sd_releases("2024-02-01", 9L)
  out <- build_signals_summary(data.frame(repo_id = "github.com/o/r", metric = "prs_merged", value = 1L),
                               series, .sd_repos(last_release_date = "2024-02-01T00:00:00Z",
                                                 median_days_between_releases = 42L),
                               .sd_rp, "2024-02-02", compute_release_facts = FALSE)
  expect_equal(out$median_days_between_releases, 42L)
  expect_equal(out$last_release_date, "2024-02-01T00:00:00Z")
})

test_that("the release date is the attribute row's, never a series date", {
  # 2026-07-07 is when the forward run first saw the count, not a release.
  series <- .sd_releases(c("2026-04-08", "2026-07-07"), c(27L, 27L))
  out <- build_signals_summary(data.frame(repo_id = character(), metric = character(), value = integer()),
                               series, .sd_repos(last_release_date = "2026-04-09T15:21:33Z"),
                               .sd_rp, "2026-10-01", compute_release_facts = TRUE, go_live = "2026-07-07")
  expect_equal(out$last_release_date, "2026-04-09T15:21:33Z")
})

test_that("a repository with no releases has no release date, whatever its series holds", {
  series <- .sd_releases("2026-07-07", 0L)
  for (full in c(TRUE, FALSE)) {
    out <- build_signals_summary(data.frame(repo_id = character(), metric = character(), value = integer()),
                                 series, .sd_repos(), .sd_rp, "2026-10-01",
                                 compute_release_facts = full, go_live = "2026-07-07")
    expect_true(is.na(out$last_release_date))
    expect_true(is.na(out$median_days_between_releases))
  }
})

test_that("the release tag and creation date come from the attribute row", {
  out <- build_signals_summary(data.frame(repo_id = character(), metric = character(), value = integer()),
                               .sd_releases(character(0), integer(0)),
                               .sd_repos(last_release_tag = "v3.6.6", repo_created_at = "2015-02-03T04:05:06Z"),
                               .sd_rp, "2026-10-01")
  expect_equal(out$last_release_tag, "v3.6.6")
  expect_equal(out$repo_created_at, "2015-02-03T04:05:06Z")
  empty <- build_signals_summary(data.frame(repo_id = character(), metric = character(), value = integer()),
                                 .sd_releases(character(0), integer(0)), .sd_repos(),
                                 .sd_rp[0, ], "2026-10-01")
  expect_true(all(c("last_release_tag", "repo_created_at") %in% names(empty)))
})
