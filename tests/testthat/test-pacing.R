# A fake clock and a recording sleep; the real Sys.sleep is never called.
fake_pacer <- function(start = 3600 * 100, ...) {
  clock <- new.env()
  clock$t <- start
  clock$slept <- numeric()
  p <- host_pacer(
    ..., now = function() clock$t,
    sleep = function(s) { clock$slept <- c(clock$slept, s); clock$t <- clock$t + s }
  )
  list(p = p, clock = clock)
}

test_that("the interval is waited per host", {
  f <- fake_pacer(min_interval = c(default = 1, a = 5))
  expect_true(f$p$before("a"))
  expect_length(f$clock$slept, 0)
  expect_true(f$p$before("a"))
  expect_equal(f$clock$slept, 5)
  expect_true(f$p$before("b"))
  expect_equal(f$clock$slept, 5)
})

test_that("an elapsed interval is not waited", {
  f <- fake_pacer(min_interval = c(default = 2))
  f$p$before("a")
  f$clock$t <- f$clock$t + 10
  f$p$before("a")
  expect_length(f$clock$slept, 0)
})

test_that("the hourly budget is per host and resets with the clock hour", {
  f <- fake_pacer(min_interval = c(default = 0), hourly_budget = c(default = 2))
  expect_true(f$p$before("a"))
  expect_true(f$p$before("a"))
  expect_false(f$p$before("a"))
  expect_equal(f$p$spent("a"), 2)
  expect_true(f$p$before("b"))
  expect_equal(f$p$spent("b"), 1)
  f$clock$t <- f$clock$t + 3600
  expect_equal(f$p$spent("a"), 0)
  expect_true(f$p$before("a"))
})

test_that("a refused request neither sleeps nor spends", {
  f <- fake_pacer(min_interval = c(default = 3), hourly_budget = c(default = 1))
  f$p$before("a")
  expect_false(f$p$before("a"))
  expect_length(f$clock$slept, 0)
  expect_equal(f$p$spent("a"), 1)
})

test_that("per-host budgets override the default", {
  f <- fake_pacer(min_interval = c(default = 0), hourly_budget = c(default = Inf, a = 1))
  expect_true(f$p$before("a"))
  expect_false(f$p$before("a"))
  for (i in 1:5) expect_true(f$p$before("b"))
})

test_that("consecutive failures defer a host and a success resets the count", {
  f <- fake_pacer(breaker_after = 2L)
  f$p$before("a"); f$p$after("a", FALSE)
  f$p$before("a"); f$p$after("a", FALSE)
  expect_false(f$p$before("a"))
  expect_equal(f$p$deferred(), "a")
  f$p$after("b", FALSE); f$p$after("b", TRUE); f$p$after("b", FALSE)
  expect_true(f$p$before("b"))
  expect_equal(f$p$deferred(), "a")
})

test_that("deferred hosts are sorted and stay deferred across hours", {
  f <- fake_pacer(breaker_after = 1L)
  f$p$after("z", FALSE); f$p$after("m", FALSE)
  expect_equal(f$p$deferred(), c("m", "z"))
  f$clock$t <- f$clock$t + 7200
  expect_false(f$p$before("m"))
})

test_that("the run budget counts a host's requests over the pacer's life, not per clock hour", {
  f <- fake_pacer(min_interval = c(default = 0), run_budget = c(default = Inf, a = 2))
  expect_true(f$p$before("a"))
  f$clock$t <- f$clock$t + 7200
  expect_true(f$p$before("a"))
  f$clock$t <- f$clock$t + 7200
  expect_false(f$p$before("a"))
  expect_equal(f$p$spent("a"), 0)
  for (i in 1:3) expect_true(f$p$before("b"))
})
