test_that("pr_merge_ratio is percent merged among decided PRs, NA when none decided", {
  expect_equal(pr_merge_ratio(30L, 10L), 75L)
  expect_equal(pr_merge_ratio(5L, 0L), 100L)
  expect_equal(pr_merge_ratio(0L, 3L), 0L)
  expect_true(is.na(pr_merge_ratio(0L, 0L)))     # no decided PRs -> NA, not 0
  expect_true(is.na(pr_merge_ratio(NA_integer_, 4L)))
})

test_that("median_days_between_releases is median gap of distinct sorted dates, NA when <2", {
  # gaps: 10, 20 -> median 15
  expect_equal(median_days_between_releases(c("2024-01-01", "2024-01-11", "2024-01-31")), 15L)
  expect_true(is.na(median_days_between_releases("2024-01-01")))
  expect_true(is.na(median_days_between_releases(character(0))))
  # duplicates collapse: only one distinct date -> NA
  expect_true(is.na(median_days_between_releases(c("2024-01-01", "2024-01-01"))))
})

test_that("median_days_to_close ignores NA-closed, NA on empty, mean-of-middles on even count", {
  # durations 2 and 8 -> median 5
  expect_equal(median_days_to_close(c("2024-01-01", "2024-01-01"),
                                    c("2024-01-03", "2024-01-09")), 5L)
  # an unclosed item (NA closed) is dropped
  expect_equal(median_days_to_close(c("2024-01-01", "2024-01-01"),
                                    c("2024-01-05", NA)), 4L)
  expect_true(is.na(median_days_to_close(character(0), character(0))))
  expect_true(is.na(median_days_to_close("2024-01-01", NA_character_)))
})

test_that("median_open_issue_age measures age from today, NA on empty", {
  expect_equal(median_open_issue_age(c("2024-01-01", "2024-01-11"), "2024-01-21"), 15L)
  expect_true(is.na(median_open_issue_age(character(0), "2024-01-21")))
})

test_that("release_rise_dates keeps the days the release count rose", {
  # A backfilled first release counts; the go-live row repeating the count does not.
  expect_equal(release_rise_dates(c("2019-02-01", "2020-05-01", "2026-04-08", "2026-07-07"),
                                  c(1L, 2L, 27L, 27L), "2026-07-07"),
               c("2019-02-01", "2020-05-01", "2026-04-08"))
  # A first row on or after go-live dates an observation, not a release.
  expect_equal(release_rise_dates(c("2026-07-07", "2026-08-01"), c(0L, 1L), "2026-07-07"), "2026-08-01")
  expect_equal(release_rise_dates(c("2026-07-09", "2026-08-01"), c(3L, 4L), "2026-07-07"), "2026-08-01")
  # A deleted release is a fall, and the release after it a rise.
  expect_equal(release_rise_dates(c("2024-01-01", "2024-02-01", "2024-03-01"), c(1L, 0L, 1L), "2026-07-07"),
               c("2024-01-01", "2024-03-01"))
  # Unsorted rows and instants.
  expect_equal(release_rise_dates(c("2024-01-01T00:00:00Z", "2023-01-01"), c(2L, 1L), "2026-07-07"),
               c("2023-01-01", "2024-01-01"))
  expect_equal(release_rise_dates(character(0), integer(0), "2026-07-07"), character(0))
  expect_equal(release_rise_dates("2019-02-01", 1L, NA_character_), character(0))
})
