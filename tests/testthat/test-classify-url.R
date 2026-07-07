test_that("classify_url classifies a direct repo URL as repo", {
  expect_equal(classify_url("https://github.com/o/n"), "repo")
})

test_that("classify_url classifies a mirror-owner github URL as mirror", {
  expect_equal(classify_url("https://github.com/cran/x"), "mirror")
})

test_that("classify_url classifies a denylisted domain as denied", {
  expect_equal(classify_url("https://doi.org/10.1/x"), "denied")
})

test_that("classify_url classifies non-VCS and NA input as other", {
  expect_equal(classify_url("https://example.org"), "other")
  expect_equal(classify_url(NA_character_), "other")
})
