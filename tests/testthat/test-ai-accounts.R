test_that("account counts are read per tool, and an empty default branch is a counted zero", {
  repos <- data.frame(repo_id = c("github.com/johnpaulgosling/addivortes", "github.com/o/empty", "github.com/o/gone"),
                      owner = c("johnpaulgosling", "o", "o"), name = c("addivortes", "empty", "gone"),
                      stringsAsFactors = FALSE)
  resp <- list(data = list(
    r0 = list(defaultBranchRef = list(target = list(
      a_cursor = list(totalCount = 25L, nodes = list(list(committedDate = "2026-08-13T09:47:23Z"))),
      a_claude = list(totalCount = 0L, nodes = list())))),
    r1 = list(defaultBranchRef = NULL),
    r2 = NULL))
  got <- parse_account_counts(resp, repos)
  a <- got[["github.com/johnpaulgosling/addivortes"]]
  expect_equal(a$tool, "cursor")
  expect_equal(a$commits, 25L)
  expect_equal(a$newest_commit_date, "2026-08-13T09:47:23Z")
  expect_equal(nrow(got[["github.com/o/empty"]]), 0L)
  expect_null(got[["github.com/o/gone"]])
})

test_that("a commit's author address or bot name maps to the tool whose account it is", {
  out <- match_bot_identity(c("NoReply@Anthropic.com", "someone@example.com"),
                            c("dependabot[bot]", "devin-ai-integration[bot]"))
  expect_setequal(out$tool, c("claude", "devin"))
  expect_equal(nrow(match_bot_identity("noreply@anthropic.com.evil.net", character(0))), 0L)
  expect_equal(match_bot_identity("cursoragent@cursor.com", character(0))$tool, "cursor")
  expect_equal(match_bot_identity("208079219+amazon-q-developer[bot]@users.noreply.github.com",
                                  character(0))$tool, "amazonq")
  idx <- .ai_account_index()
  expect_true(all(idx$kind %in% c("graphql", "rest_only", "linked", "names")))
  expect_equal(idx$tool[idx$value == "41898282+claude[bot]@users.noreply.github.com"], "claude")
  expect_equal(idx$kind[idx$value == "41898282+claude[bot]@users.noreply.github.com"], "rest_only")
})

test_that("commits by a tool's accounts are a search for every tool that has accounts", {
  inv <- ai_rule_inventory()
  expect_setequal(inv$tool[inv$tier == "A"], unique(vapply(AI_ACCOUNTS, `[[`, "", "tool")))
})
