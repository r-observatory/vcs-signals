test_that("build_tree_query aliases each repo and asks for both trees, fork, parent", {
  repos <- data.frame(owner = c("o", "p"), name = c("n", "m"),
                      repo_id = c("github.com/o/n", "github.com/p/m"), stringsAsFactors = FALSE)
  q <- build_tree_query(repos)
  expect_match(q, 'r0: repository(owner: "o", name: "n")', fixed = TRUE)
  expect_match(q, 'r1: repository(owner: "p", name: "m")', fixed = TRUE)
  expect_match(q, 'rootTree: object(expression: "HEAD:")', fixed = TRUE)
  expect_match(q, 'githubTree: object(expression: "HEAD:.github")', fixed = TRUE)
  expect_match(q, "... on Tree { entries { name type } }", fixed = TRUE)
  expect_match(q, "isFork parent { nameWithOwner }", fixed = TRUE)
  expect_match(q, 'gitignore: object(expression: "HEAD:.gitignore")', fixed = TRUE)
  expect_match(q, 'rbuildignore: object(expression: "HEAD:.Rbuildignore")', fixed = TRUE)
  expect_match(q, "... on Blob { text }", fixed = TRUE)
})

test_that("parse_tree_markers demuxes per repo, guards null object() and null alias", {
  repos <- data.frame(repo_id = c("github.com/a/a", "github.com/b/b", "github.com/c/c"),
                      owner = c("a", "b", "c"), name = c("a", "b", "c"), stringsAsFactors = FALSE)
  resp <- list(data = list(
    r0 = list(isFork = FALSE, parent = NULL,
              rootTree = list(entries = list(list(name = ".claude", type = "tree"),
                                             list(name = "DESCRIPTION", type = "blob"))),
              githubTree = list(entries = list(list(name = "copilot-instructions.md", type = "blob"))),
              gitignore = list(text = ".Rproj.user\n.aiderignore\n"), rbuildignore = NULL),
    r1 = list(isFork = TRUE, parent = list(nameWithOwner = "up/stream"),
              rootTree = NULL, githubTree = NULL, gitignore = NULL, rbuildignore = NULL),
    r2 = NULL))
  out <- parse_tree_markers(resp, repos)
  expect_setequal(out[["github.com/a/a"]]$root_entries, c(".claude", "DESCRIPTION"))
  expect_equal(out[["github.com/a/a"]]$github_entries, "copilot-instructions.md")
  expect_false(out[["github.com/a/a"]]$is_fork)
  expect_true(is.na(out[["github.com/a/a"]]$parent))
  expect_true(".aiderignore" %in% out[["github.com/a/a"]]$gitignore_lines)   # blob text -> lines
  expect_equal(length(out[["github.com/a/a"]]$rbuildignore_lines), 0)        # null blob guarded
  expect_true(out[["github.com/b/b"]]$is_fork)
  expect_equal(out[["github.com/b/b"]]$parent, "up/stream")
  expect_equal(length(out[["github.com/b/b"]]$root_entries), 0)   # null object() guarded
  expect_equal(length(out[["github.com/c/c"]]$root_entries), 0)   # null alias guarded
})

test_that("parse_tree_markers ignore content feeds scan_ignore_tokens", {
  repos <- data.frame(repo_id = "github.com/a/a", owner = "a", name = "a", stringsAsFactors = FALSE)
  resp <- list(data = list(r0 = list(isFork = FALSE, parent = NULL, rootTree = NULL, githubTree = NULL,
    gitignore = list(text = ".aiderignore\n*.o\n"), rbuildignore = NULL)))
  m <- parse_tree_markers(resp, repos)[["github.com/a/a"]]
  ev <- scan_ignore_tokens(m$gitignore_lines, m$rbuildignore_lines)
  expect_true("aider" %in% ev$tool)
})

test_that("parse_tree_markers output feeds classify_tree_markers", {
  repos <- data.frame(repo_id = "github.com/a/a", owner = "a", name = "a", stringsAsFactors = FALSE)
  resp <- list(data = list(r0 = list(isFork = FALSE, parent = NULL,
    rootTree = list(entries = list(list(name = ".claude", type = "tree"))),
    githubTree = list(entries = list(list(name = "copilot-instructions.md", type = "blob"))))))
  m <- parse_tree_markers(resp, repos)[["github.com/a/a"]]
  ev <- classify_tree_markers(m$root_entries, m$github_entries)
  expect_setequal(ev$tool, c("claude", "copilot"))
  expect_true(all(ev$tier == "D"))
})

test_that("build_pr_agent_query aliases each repo, oldest-first, author login + __typename", {
  repos <- data.frame(owner = "o", name = "n", repo_id = "github.com/o/n", stringsAsFactors = FALSE)
  q <- build_pr_agent_query(repos)
  expect_match(q, 'r0: repository(owner: "o", name: "n")', fixed = TRUE)
  expect_match(q, "pullRequests(first: 50, orderBy: {field: CREATED_AT, direction: ASC})", fixed = TRUE)
  expect_match(q, "pageInfo { endCursor hasNextPage }", fixed = TRUE)
  expect_match(q, "author { login __typename } createdAt", fixed = TRUE)
})

test_that("parse_pr_agents surfaces login+typename, guards null author, never trusts Bot alone", {
  repos <- data.frame(repo_id = c("github.com/a/a", "github.com/b/b", "github.com/c/c"),
                      owner = c("a", "b", "c"), name = c("a", "b", "c"), stringsAsFactors = FALSE)
  resp <- list(data = list(
    r0 = list(pullRequests = list(
      pageInfo = list(endCursor = "c1", hasNextPage = TRUE),
      nodes = list(
        list(author = list(login = "octocat", `__typename` = "User"), createdAt = "2021-01-01T00:00:00Z"),
        list(author = list(login = "Copilot", `__typename` = "Bot"), createdAt = "2024-05-01T00:00:00Z"),
        list(author = list(login = "dependabot[bot]", `__typename` = "Bot"), createdAt = "2024-06-01T00:00:00Z"),
        list(author = NULL, createdAt = "2024-07-01T00:00:00Z")))),
    r1 = list(pullRequests = list(pageInfo = list(endCursor = NA, hasNextPage = FALSE), nodes = list())),
    r2 = NULL))
  out <- parse_pr_agents(resp, repos)
  a <- out[["github.com/a/a"]]
  expect_equal(nrow(a$prs), 4)
  expect_true(a$has_next)
  expect_equal(a$prs$login, c("octocat", "Copilot", "dependabot[bot]", NA))   # null author -> NA
  expect_equal(a$prs$typename[2], "Bot")
  # detection uses the allowlist, so Dependabot (also __typename Bot) never flags
  expect_equal(detect_pr_agents(a$prs$login)$tool, "copilot")
  expect_equal(nrow(out[["github.com/b/b"]]$prs), 0)                           # empty repo
  expect_false(out[["github.com/b/b"]]$has_next)
  expect_equal(nrow(out[["github.com/c/c"]]$prs), 0)                           # null alias guarded
})

test_that("parse_search_commit reads the earliest-match date, NA on no match or bad body", {
  body <- '{"total_count":3,"items":[{"commit":{"committer":{"date":"2024-02-15T09:00:00Z"}}}]}'
  expect_equal(parse_search_commit(body), "2024-02-15T09:00:00Z")
  expect_true(is.na(parse_search_commit('{"total_count":0,"items":[]}')))
  expect_true(is.na(parse_search_commit('{"items":[]}')))
  expect_true(is.na(parse_search_commit('not json at all')))
})
