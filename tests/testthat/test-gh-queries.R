test_that("build_gauge_query embeds ids and the metric fields", {
  q <- build_gauge_query(c("R_1", "R_2"))
  expect_match(q, 'nodes\\(ids: \\["R_1", "R_2"\\]\\)')
  expect_match(q, "stargazerCount")
  expect_match(q, "issues_open: issues\\(states: OPEN\\)")
  expect_match(q, "mergedPRs: pullRequests\\(states: MERGED\\)", fixed = FALSE)
  expect_match(q, "repositoryTopics\\(first: 20\\)")
})

test_that("build_commit_query asks for history totalCount and latest committedDate", {
  q <- build_commit_query("R_9")
  expect_match(q, 'nodes\\(ids: \\["R_9"\\]\\)')
  expect_match(q, "history \\{ totalCount \\}")
  expect_match(q, "last: history\\(first: 1\\)")
})

test_that("build_resolve_query aliases each repo and follows renames", {
  q <- build_resolve_query(c("tidyverse", "r-lib"), c("ggplot2", "scales"))
  expect_match(q, 'r0: repository\\(owner: "tidyverse", name: "ggplot2", followRenames: true\\)')
  expect_match(q, 'r1: repository\\(owner: "r-lib", name: "scales", followRenames: true\\)')
  expect_match(q, "nameWithOwner")
})
