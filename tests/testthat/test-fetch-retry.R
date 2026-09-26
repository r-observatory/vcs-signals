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

# ---- fetch_aliased: failed reads are reported, never dropped ----------------

test_that("a failing batch is halved to single repositories and a survivor is reported", {
  io <- fake_contents_io(fail = "p03")
  got <- fetch_tree_markers(io, fake_repos(4), batch_size = 4)
  expect_setequal(names(got$results), paste0("github.com/o/", c("p01", "p02", "p04")))
  expect_equal(got$failed$repo_id, "github.com/o/p03")
  expect_equal(got$failed$query, "contents")
  expect_equal(got$failed$error, "Something went wrong while executing your query.")
  expect_match(got$failed$failed_at, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
})

test_that("a repository that fails alone is read once more after the retry wait", {
  io <- fake_contents_io(fail = "p01")
  got <- fetch_tree_markers(io, fake_repos(1), batch_size = 10)
  expect_equal(sum(io$log$slept == AI_BATCH_RETRY_WAIT_S), 1L)
  expect_length(contents_queries(io), 2L)
  expect_equal(nrow(got$failed), 1L)
})

test_that("a gone repository is neither a result row nor a failure", {
  io <- list(graphql = function(query) list(
    data = list(r0 = NULL),
    errors = list(list(type = "NOT_FOUND", path = list("r0"), message = "Could not resolve"))),
    sleep = function(s) invisible(NULL))
  got <- fetch_tree_markers(io, fake_repos(1), batch_size = 10)
  expect_true(is.na(got$results[["github.com/o/p01"]]$is_fork))
  expect_equal(nrow(got$failed), 0L)
})

test_that("with no breaker the fetch reports every failure and never stops early", {
  io <- fake_contents_io(fail = sprintf("p%02d", 1:25))
  got <- fetch_tree_markers(io, fake_repos(25), batch_size = 25)
  expect_equal(nrow(got$failed), 25L)
})

test_that("nineteen identical failures and then a success reset the breaker", {
  b <- new_fetch_breaker()
  io <- fake_contents_io(fail = sprintf("p%02d", 1:19))
  fetch_tree_markers(io, fake_repos(19), batch_size = 10, breaker = b)
  expect_equal(b$count, 19L)
  fetch_tree_markers(io, fake_repos(1, prefix = "q"), batch_size = 10, breaker = b)
  expect_equal(b$count, 0L)
  expect_true(is.na(b$message))
  expect_false(b$tripped)
})

test_that("failures alternating between two messages never trip the breaker", {
  b <- new_fetch_breaker()
  for (k in 1:20) {
    io <- fake_contents_io(fail = "p01", message = if (k %% 2) "first fault" else "second fault")
    fetch_tree_markers(io, fake_repos(1), batch_size = 10, breaker = b)
  }
  expect_false(b$tripped)
  expect_equal(b$count, 1L)
})

test_that("a tripped breaker makes no call and reports every repository with its message", {
  b <- new_fetch_breaker()
  b$tripped <- TRUE; b$message <- "Something went wrong while executing your query."
  io <- fake_contents_io()
  got <- fetch_tree_markers(io, fake_repos(10), batch_size = 10, breaker = b)
  expect_length(io$log$queries, 0L)
  expect_equal(nrow(got$failed), 10L)
  expect_true(all(got$failed$error == b$message))
})

test_that("GitHub's fault still trips the breaker when each reply names a new time and request id", {
  n <- 0L
  github_says <- function() {
    n <<- n + 1L
    sprintf(paste0("Something went wrong while executing your query on 2026-09-27T08:%02d:%02dZ. ",
                   "Please include `C80C:1D44DB:%06X:1F5EA85:6AB68AE2` when reporting this issue."),
            n %/% 60L, n %% 60L, n)
  }
  b <- new_fetch_breaker()
  io <- fake_contents_io(fail = sprintf("p%02d", 1:25), message = github_says)
  got <- fetch_tree_markers(io, fake_repos(25), batch_size = 25, breaker = b)
  expect_true(b$tripped)
  expect_equal(nrow(got$failed), 25L)
  expect_gt(length(unique(got$failed$error)), 1L)
  expect_match(got$failed$error[1], "^Something went wrong while executing your query on 2026-09-27T")
})

test_that("a 502, which reaches R as an error, is halved, read again and reported with its message", {
  slept <- numeric(0)
  io <- list(graphql = function(query) {
      if (grepl('name: "p02"', query, fixed = TRUE)) stop("gh api graphql returned no output")
      fake_contents_io()$graphql(query)
    },
    sleep = function(s) slept <<- c(slept, s))
  got <- fetch_tree_markers(io, fake_repos(4), batch_size = 4)
  expect_setequal(names(got$results), paste0("github.com/o/", c("p01", "p03", "p04")))
  expect_equal(got$failed$repo_id, "github.com/o/p02")
  expect_equal(got$failed$error, "gh api graphql returned no output")
  expect_equal(sum(slept == AI_BATCH_RETRY_WAIT_S), 1L)
})
