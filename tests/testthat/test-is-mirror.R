test_that("is_mirror flags cran/bioc mirrors and the bioc git server", {
  expect_true(is_mirror("github", "cran", "foo", "github.com"))
  expect_true(is_mirror("github", "bioc", "foo", "github.com"))
  expect_true(is_mirror("other", "packages", "x", "git.bioconductor.org"))
})

test_that("is_mirror does not flag real repos, including the Bioconductor org", {
  expect_false(is_mirror("github", "Bioconductor", "S4Vectors", "github.com"))
  expect_false(is_mirror("github", "thelovelab", "tximeta", "github.com"))
})
