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
