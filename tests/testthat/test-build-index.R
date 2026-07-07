mk <- function(...) {
  rows <- list(...)
  do.call(rbind, lapply(rows, function(r) data.frame(
    package = r[[1]], origin = r[[2]], host = "github", host_domain = "github.com",
    owner = r[[3]], name = r[[4]], resolved_from = "url", stringsAsFactors = FALSE)))
}

test_that("build_repo_index dedups repos and counts packages", {
  df <- mk(list("A", "cran", "Foo", "Bar"),   # same repo, different case
           list("B", "cran", "foo", "bar"),
           list("C", "cran", "org", "mono"))
  idx <- build_repo_index(df)
  expect_equal(nrow(idx$repos), 2)
  bar <- idx$repos[idx$repos$repo_id == "github.com/foo/bar", ]
  expect_equal(bar$n_packages, 2)
  expect_equal(bar$name_with_owner, "Foo/Bar")   # display case from first occurrence
  expect_equal(bar$supported, 1L)
  expect_equal(nrow(idx$repo_packages), 3)
})

test_that("build_repo_index keeps one repo_packages row per origin", {
  df <- mk(list("dupe", "cran", "o", "n"), list("dupe", "bioc", "o", "n"))
  idx <- build_repo_index(df)
  expect_equal(nrow(idx$repos), 1)
  expect_equal(idx$repos$n_packages, 2)
  expect_equal(sort(idx$repo_packages$origin), c("bioc", "cran"))
})

test_that("build_repo_index handles empty input", {
  idx <- build_repo_index(data.frame())
  expect_equal(nrow(idx$repos), 0)
  expect_equal(nrow(idx$repo_packages), 0)
})
