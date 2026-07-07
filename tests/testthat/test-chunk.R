test_that("chunk splits into groups of at most n", {
  expect_equal(chunk(1:5, 2), list(`1` = 1:2, `2` = 3:4, `3` = 5L))
  expect_equal(length(chunk(1:40, 40)), 1)
  expect_equal(length(chunk(1:41, 40)), 2)
})

test_that("chunk handles empty input", {
  expect_equal(chunk(character(0), 10), list())
})
