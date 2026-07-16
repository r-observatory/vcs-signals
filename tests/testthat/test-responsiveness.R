test_that("build_responsiveness_query aliases each repo with the three connections", {
  repos <- data.frame(owner = c("a", "b"), name = c("x", "y"),
                      repo_id = c("github.com/a/x", "github.com/b/y"), stringsAsFactors = FALSE)
  q <- build_responsiveness_query(repos)
  expect_true(grepl('r0: repository\\(owner: "a", name: "x"\\)', q))
  expect_true(grepl('r1: repository\\(owner: "b", name: "y"\\)', q))
  expect_true(grepl("states: CLOSED", q))              # closed issues
  expect_true(grepl("states: \\[MERGED, CLOSED\\]", q)) # resolved PRs
  expect_true(grepl("states: OPEN", q))                 # open issues
})

test_that("parse_responsiveness reduces each repo to three medians, NA when empty", {
  repos <- data.frame(owner = c("a", "b"), name = c("x", "y"),
                      repo_id = c("github.com/a/x", "github.com/b/y"), stringsAsFactors = FALSE)
  resp <- list(data = list(
    r0 = list(
      closedIssues = list(nodes = list(
        list(createdAt = "2024-01-01T00:00:00Z", closedAt = "2024-01-03T00:00:00Z"),
        list(createdAt = "2024-01-01T00:00:00Z", closedAt = "2024-01-09T00:00:00Z"))),
      resolvedPRs = list(nodes = list(
        list(createdAt = "2024-01-01T00:00:00Z", closedAt = "2024-01-06T00:00:00Z"))),
      openIssues = list(nodes = list(
        list(createdAt = "2024-01-01T00:00:00Z"), list(createdAt = "2024-01-11T00:00:00Z")))),
    r1 = list(
      closedIssues = list(nodes = list()),
      resolvedPRs = list(nodes = list()),
      openIssues = list(nodes = list()))))
  out <- parse_responsiveness(resp, repos, "2024-01-21")
  expect_equal(out$median_days_to_close_issue[out$repo_id == "github.com/a/x"], 5L)  # 2,8 -> 5
  expect_equal(out$median_days_to_close_pr[out$repo_id == "github.com/a/x"], 5L)
  expect_equal(out$median_open_issue_age_days[out$repo_id == "github.com/a/x"], 15L) # 20,10 -> 15
  expect_true(is.na(out$median_days_to_close_issue[out$repo_id == "github.com/b/y"]))
  expect_true(is.na(out$median_open_issue_age_days[out$repo_id == "github.com/b/y"]))
})

test_that("parse_responsiveness tolerates a null repo alias (deleted/renamed) as all-NA", {
  repos <- data.frame(owner = "a", name = "x", repo_id = "github.com/a/x", stringsAsFactors = FALSE)
  resp <- list(data = list(r0 = NULL))
  out <- parse_responsiveness(resp, repos, "2024-01-21")
  expect_equal(nrow(out), 1)
  expect_true(is.na(out$median_days_to_close_pr))
})
