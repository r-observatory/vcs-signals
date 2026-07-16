test_that("config constants load with expected shape", {
  expect_length(VIEWS_URLS, 4)
  expect_identical(unname(KNOWN_FORGES["github.com"]), "github")
  expect_true("doi.org" %in% NON_REPO_DENYLIST)
  expect_identical(SUPPORTED_HOSTS, "github")
  expect_true(is.list(AI_MARKERS) && length(AI_MARKERS) > 10)
  expect_true(all(vapply(AI_MARKERS, function(m) all(c("path","tool","location","agnostic") %in% names(m)), logical(1))))
  expect_false(any(vapply(AI_MARKERS, function(m) m$path == ".replit", logical(1))))   # bare .replit is non-evidence
  expect_true(sum(vapply(AI_MARKERS, function(m) isTRUE(m$agnostic), logical(1))) == 1) # only agents-md
  expect_identical(unname(AI_BOT_ALLOWLIST["noreply@anthropic.com"]), "claude")
  expect_true("dependabot[bot]" %in% AI_BOT_DENYLIST)
  expect_true(is.character(AI_RULESET_VERSION) && nzchar(AI_RULESET_VERSION))
  expect_identical(unname(TIER_PRIORITY["A"]), 1L)
})
