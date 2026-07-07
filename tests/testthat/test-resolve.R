test_that("repo_slug is lowercased and canonical", {
  expect_identical(repo_slug("github.com", "Foo", "Bar"), "github.com/foo/bar")
})

test_that("resolve picks the github repo from a pkgdown+github URL field", {
  r <- resolve_repo_for_package("https://thelovelab.github.io/tximeta, https://github.com/thelovelab/tximeta", NA)
  expect_equal(r$owner, "thelovelab"); expect_equal(r$name, "tximeta")
  expect_equal(r$resolved_from, "url")
})

test_that("resolve marks both when URL and BugReports agree", {
  r <- resolve_repo_for_package("https://github.com/tidyverse/ggplot2",
                                "https://github.com/tidyverse/ggplot2/issues")
  expect_equal(r$resolved_from, "both")
  expect_equal(r$name, "ggplot2")
})

test_that("resolve dedups the gitlab /-/ form to one repo (both)", {
  r <- resolve_repo_for_package("https://gitlab.com/o/n", "https://gitlab.com/o/n/-/issues")
  expect_equal(r$resolved_from, "both"); expect_equal(r$owner, "o"); expect_equal(r$name, "n")
})

test_that("resolve falls back to BugReports and prefers github over gitlab", {
  expect_equal(resolve_repo_for_package("https://homepage.example", "https://github.com/o/n")$resolved_from,
               "bugreports")
  expect_equal(resolve_repo_for_package("https://gitlab.com/g/p https://github.com/o/n", NA)$host, "github")
})

test_that("resolve uses github.io recovery only when no direct repo exists", {
  r <- resolve_repo_for_package("https://owner.github.io/pkg", NA)
  expect_equal(r$resolved_from, "pages"); expect_equal(r$owner, "owner"); expect_equal(r$name, "pkg")
})

test_that("resolve returns NULL for mirrors-only and empty", {
  expect_null(resolve_repo_for_package("https://github.com/cran/foo", NA))
  expect_null(resolve_repo_for_package(NA, NA))
})
