test_that("config constants load with expected shape", {
  expect_length(VIEWS_URLS, 4)
  expect_identical(unname(KNOWN_FORGES["github.com"]), "github")
  expect_true("doi.org" %in% NON_REPO_DENYLIST)
  expect_identical(SUPPORTED_HOSTS, "github")
})
