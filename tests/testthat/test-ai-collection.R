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
