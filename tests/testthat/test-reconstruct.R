test_that("reconstruct_star_series returns zero rows for empty input with the right columns", {
  out <- reconstruct_star_series("R", character(0))
  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("repo_id", "date", "metric", "value"))
})

test_that("one star yields one row with value 1", {
  out <- reconstruct_star_series("R", "2020-01-01T10:00:00Z")
  expect_equal(nrow(out), 1)
  expect_equal(out$date, "2020-01-01")
  expect_equal(out$metric, "stars")
  expect_equal(out$value, 1L)
})

test_that("three stars on the same day collapse to one row with value 3", {
  out <- reconstruct_star_series("R", c(
    "2020-01-01T01:00:00Z", "2020-01-01T02:00:00Z", "2020-01-01T03:00:00Z"))
  expect_equal(nrow(out), 1)
  expect_equal(out$value, 3L)
})

test_that("stars across three days (1,2,1) produce ascending cumulative rows", {
  starred_at <- c(
    "2020-01-01T00:00:00Z",
    "2020-01-02T00:00:00Z", "2020-01-02T01:00:00Z",
    "2020-01-03T00:00:00Z")
  out <- reconstruct_star_series("R", starred_at)
  expect_equal(out$date, c("2020-01-01", "2020-01-02", "2020-01-03"))
  expect_equal(out$value, c(1L, 3L, 4L))
})

test_that("unsorted input still yields ascending cumulative rows", {
  starred_at <- c("2020-01-03T00:00:00Z", "2020-01-01T00:00:00Z",
                 "2020-01-02T00:00:00Z", "2020-01-02T05:00:00Z")
  out <- reconstruct_star_series("R", starred_at)
  expect_equal(out$date, c("2020-01-01", "2020-01-02", "2020-01-03"))
  expect_equal(out$value, c(1L, 3L, 4L))
})

test_that("column types are character/character/character/integer", {
  out <- reconstruct_star_series("R", c("2020-01-01T00:00:00Z", "2020-01-02T00:00:00Z"))
  expect_type(out$repo_id, "character")
  expect_type(out$date, "character")
  expect_type(out$metric, "character")
  expect_type(out$value, "integer")
})

test_that("reconstruct_star_series wrapper matches reconstruct_cumulative_series(..., \"stars\")", {
  starred_at <- c("2020-01-01T00:00:00Z", "2020-01-02T00:00:00Z", "2020-01-02T05:00:00Z")
  expect_identical(reconstruct_star_series("R", starred_at),
                   reconstruct_cumulative_series("R", starred_at, "stars"))
})

test_that("reconstruct_cumulative_series returns zero rows for empty input regardless of metric", {
  out <- reconstruct_cumulative_series("R", character(0), "forks")
  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("repo_id", "date", "metric", "value"))
})

test_that("reconstruct_cumulative_series yields ascending cumulative rows for forks", {
  out <- reconstruct_cumulative_series("R", c(
    "2020-01-01T00:00:00Z", "2020-01-01T05:00:00Z", "2021-06-02T00:00:00Z"), "forks")
  expect_equal(out$date, c("2020-01-01", "2021-06-02"))
  expect_equal(out$metric, c("forks", "forks"))
  expect_equal(out$value, c(2L, 3L))
})

test_that("reconstruct_cumulative_series yields ascending cumulative rows for releases_total", {
  out <- reconstruct_cumulative_series("R", c(
    "2019-05-01T00:00:00Z", "2020-03-15T00:00:00Z"), "releases_total")
  expect_equal(out$date, c("2019-05-01", "2020-03-15"))
  expect_equal(out$metric, c("releases_total", "releases_total"))
  expect_equal(out$value, c(1L, 2L))
})

test_that("reconstruct_open_series returns zero rows for empty input with the right columns", {
  out <- reconstruct_open_series("R", character(0), character(0), "issues_open")
  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("repo_id", "date", "metric", "value"))
})

test_that("two issues opened day1, one closed day3 -> rows (day1,2),(day3,1)", {
  created <- c("2020-01-01T00:00:00Z", "2020-01-01T05:00:00Z")
  closed  <- c(NA_character_, "2020-01-03T00:00:00Z")
  out <- reconstruct_open_series("R", created, closed, "issues_open")
  expect_equal(out$date, c("2020-01-01", "2020-01-03"))
  expect_equal(out$metric, c("issues_open", "issues_open"))
  expect_equal(out$value, c(2L, 1L))
})

test_that("an issue opened and closed the same day nets zero change and emits no row", {
  out <- reconstruct_open_series("R",
    created = "2020-01-01T00:00:00Z", closed = "2020-01-01T08:00:00Z", metric = "issues_open")
  expect_equal(nrow(out), 0)
})

test_that("all-open input (no closes) reduces to the cumulative-created curve", {
  created <- c("2020-01-01T00:00:00Z", "2020-01-02T00:00:00Z", "2020-01-02T05:00:00Z")
  out_open <- reconstruct_open_series("R", created, rep(NA_character_, 3), "prs_open")
  out_cum  <- reconstruct_cumulative_series("R", created, "prs_open")
  expect_equal(out_open$date, out_cum$date)
  expect_equal(out_open$value, out_cum$value)
})

test_that("a day with a net-zero mix of opens/closes between two equal-value days emits no row for that day", {
  created <- c("2020-01-01T00:00:00Z", "2020-01-02T00:00:00Z")   # +1 on day1, +1 on day2
  closed  <- c(NA_character_, "2020-01-02T12:00:00Z")             # the day2 issue closes same day (net 0 on day2)
  out <- reconstruct_open_series("R", created, closed, "issues_open")
  expect_equal(out$date, "2020-01-01")
  expect_equal(out$value, 1L)
})

test_that("column types are character/character/character/integer for reconstruct_open_series", {
  out <- reconstruct_open_series("R", c("2020-01-01T00:00:00Z", "2020-01-02T00:00:00Z"),
                                 c(NA_character_, NA_character_), "issues_open")
  expect_type(out$repo_id, "character")
  expect_type(out$date, "character")
  expect_type(out$metric, "character")
  expect_type(out$value, "integer")
})
