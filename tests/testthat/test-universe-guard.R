test_that("universe_guard passes on first run and stable counts", {
  expect_true(universe_guard(0, 0, 100, 80))          # no prior
  expect_true(universe_guard(100, 80, 100, 80))       # stable
  expect_true(universe_guard(100, 80, 95, 76))        # within 10%
})

test_that("universe_guard aborts on a large drop", {
  expect_error(universe_guard(100, 80, 85, 80), "packages dropped")
  expect_error(universe_guard(100, 80, 100, 60), "repos dropped")
})
