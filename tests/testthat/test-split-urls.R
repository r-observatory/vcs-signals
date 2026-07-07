test_that("split_urls splits, trims, and strips angle brackets", {
  expect_identical(split_urls("https://a.io/x, https://github.com/o/n"),
                   c("https://a.io/x", "https://github.com/o/n"))
  expect_identical(split_urls("<https://github.com/o/n>"), "https://github.com/o/n")
  expect_identical(split_urls("https://github.com/o/n\n  https://a.io"),
                   c("https://github.com/o/n", "https://a.io"))
})

test_that("split_urls returns character(0) for empty/NA", {
  expect_identical(split_urls(NA_character_), character(0))
  expect_identical(split_urls(""), character(0))
  expect_identical(split_urls("   "), character(0))
  expect_identical(split_urls(NULL), character(0))
})
