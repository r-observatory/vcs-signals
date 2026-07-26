# with_retry / fetch_views: surviving a transient bioconductor.org outage.
# Every test injects sleep and rand, so the suite never actually waits and the
# backoff schedule is asserted directly.

no_sleep <- function(s) invisible(NULL)

test_that("with_retry returns the first attempt's value without sleeping", {
  slept <- numeric(0)
  calls <- 0L

  val <- with_retry(function() { calls <<- calls + 1L; "ok" },
                    sleep = function(s) slept <<- c(slept, s))

  expect_equal(val, "ok")
  expect_equal(calls, 1L)
  expect_equal(slept, numeric(0))
})

test_that("with_retry retries a failing call and returns the first success", {
  calls <- 0L

  val <- with_retry(function() {
    calls <<- calls + 1L
    if (calls < 3L) stop("HTTP status was '504 Gateway Timeout'")
    "ok"
  }, sleep = no_sleep)

  expect_equal(val, "ok")
  expect_equal(calls, 3L)
})

test_that("with_retry waits the configured backoff between attempts", {
  slept <- numeric(0)

  expect_error(
    with_retry(function() stop("boom"), waits = c(1, 2, 4),
               sleep = function(s) slept <<- c(slept, s), rand = function() 1),
    "boom")

  expect_equal(slept, c(1, 2, 4))
})

test_that("with_retry spreads each wait with jitter", {
  slept <- numeric(0)

  expect_error(
    with_retry(function() stop("boom"), waits = c(10, 20),
               sleep = function(s) slept <<- c(slept, s), rand = function() 1.2),
    "boom")

  expect_equal(slept, c(12, 24))
})

test_that("with_retry makes one more attempt than it has waits, then propagates", {
  calls <- 0L

  expect_error(
    with_retry(function() {
      calls <<- calls + 1L
      stop("HTTP status was '504 Gateway Timeout'")
    }, waits = c(1, 2), sleep = no_sleep),
    "504 Gateway Timeout")

  expect_equal(calls, 3L)
})

test_that("the default VIEWS backoff covers more than fifteen minutes", {
  expect_gt(sum(VIEWS_RETRY_WAITS_S), 15 * 60)
})

test_that("fetch_views retries a failing read and returns the recovered body", {
  calls <- 0L
  body  <- "Package: abc\nVersion: 1.0\n"

  out <- fetch_views("https://bioconductor.org/packages/release/bioc/VIEWS",
                     read  = function(u) {
                       calls <<- calls + 1L
                       if (calls < 2L) stop("cannot open the connection")
                       body
                     },
                     sleep = no_sleep)

  expect_equal(out, body)
  expect_equal(calls, 2L)
})

test_that("fetch_views retries a body that is not VIEWS content", {
  calls <- 0L

  out <- fetch_views("https://bioconductor.org/packages/release/bioc/VIEWS",
                     read  = function(u) {
                       calls <<- calls + 1L
                       if (calls < 3L) "<html><body>504 Gateway Timeout</body></html>"
                       else "Package: abc\n"
                     },
                     sleep = no_sleep)

  expect_equal(calls, 3L)
  expect_match(out, "Package: abc")
})

test_that("fetch_views names the url and the cause when every attempt fails", {
  expect_error(
    fetch_views("https://bioconductor.org/packages/release/workflows/VIEWS",
                read  = function(u) stop("HTTP status was '504 Gateway Timeout'"),
                waits = c(0, 0), sleep = no_sleep),
    "VIEWS fetch failed or empty: https://bioconductor.org/packages/release/workflows/VIEWS")
})

test_that("fetch_views treats an NA body as a failed read", {
  expect_error(
    fetch_views("https://bioconductor.org/packages/release/bioc/VIEWS",
                read = function(u) NA_character_, waits = 0, sleep = no_sleep),
    "VIEWS fetch failed or empty")
})
