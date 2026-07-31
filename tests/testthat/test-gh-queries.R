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

test_that("graphql_rate_remaining reads the remaining points, and defaults to Inf when absent", {
  io <- list(graphql = function(query) list(data = list(rateLimit = list(remaining = 250, resetAt = "x"))))
  expect_equal(graphql_rate_remaining(io), 250L)

  io_absent <- list(graphql = function(query) list(data = list(nodes = list())))
  expect_equal(graphql_rate_remaining(io_absent), Inf)

  io_err <- list(graphql = function(query) stop("network error"))
  expect_equal(graphql_rate_remaining(io_err), Inf)
})

test_that("subtree entries arrive under their own prefix", {
  # Prefixing is what lets a marker name the path it occupies without either
  # classifier growing an argument. It also stops a name inside a subtree from
  # being mistaken for the same name at the root.
  repos <- data.frame(repo_id = "github.com/o/n", owner = "o", name = "n",
                      stringsAsFactors = FALSE)
  resp <- list(data = list(r0 = list(
    isFork = FALSE, parent = NULL,
    rootTree = list(entries = list(list(name = "DESCRIPTION"), list(name = "CLAUDE.md"))),
    githubTree = list(entries = list(list(name = "workflows"))),
    claudeTree = list(entries = list(list(name = "skills"), list(name = "settings.json"))),
    agentsTree = NULL,
    instTree = list(entries = list(list(name = "skills"), list(name = "CITATION"))),
    gitignore = NULL, rbuildignore = NULL)))

  out <- parse_tree_markers(resp, repos)[["github.com/o/n"]]
  expect_true(".claude/skills" %in% out$root_entries)
  expect_true("inst/skills" %in% out$root_entries)
  expect_true("CLAUDE.md" %in% out$root_entries)
  expect_false("skills" %in% out$root_entries)
  expect_false("CITATION" %in% out$root_entries)
  expect_equal(out$github_entries, "workflows")
})

test_that("an absent subtree contributes nothing rather than a false negative", {
  repos <- data.frame(repo_id = "github.com/o/n", owner = "o", name = "n",
                      stringsAsFactors = FALSE)
  resp <- list(data = list(r0 = list(
    isFork = FALSE, parent = NULL,
    rootTree = list(entries = list(list(name = "DESCRIPTION"))),
    githubTree = NULL, claudeTree = NULL, agentsTree = NULL, instTree = NULL,
    gitignore = NULL, rbuildignore = NULL)))
  out <- parse_tree_markers(resp, repos)[["github.com/o/n"]]
  expect_equal(out$root_entries, "DESCRIPTION")
})

test_that("the tree query asks for the subtrees the markers need", {
  repos <- data.frame(repo_id = "github.com/o/n", owner = "o", name = "n",
                      stringsAsFactors = FALSE)
  q <- build_tree_query(repos)
  for (expr in c("HEAD:.claude", "HEAD:.agents", "HEAD:inst")) {
    expect_true(grepl(expr, q, fixed = TRUE), info = expr)
  }
})

test_that("a search query containing a space survives the shell", {
  # system2 pastes its arguments into a command line WITHOUT quoting them, so
  # the assembled q= value was split on its spaces and gh received
  # the remainder as stray positional arguments ("accepts 1 arg(s), received 2").
  # Every trailer phrase contains a space, so every tier-B and tier-C search ever
  # issued was malformed and came back empty. Tier A's author-email query has no
  # space, which is why only these two tiers went silent.
  #
  # Asserting on the shell form rather than on a live call, because the failure is
  # in how the argument is handed over, not in what the API does with it.
  src <- paste(readLines("../../scripts/github.R", warn = FALSE), collapse = "\n")
  # Both search helpers build the same argument and both broke the same way.
  expect_equal(length(gregexpr('shQuote(paste0("q=", q))', src, fixed = TRUE)[[1]]), 2L,
               info = "both commit-search helpers quote the q argument")
  expect_false(grepl('"-f", paste0("q=", q),', src, fixed = TRUE),
               info = "the unquoted form silently breaks every multi-word query")
})

test_that("every trailer query the ruleset ships contains a space", {
  # This is what made the bug total rather than partial: there is no single-word
  # trailer, so no tier-B search could ever have worked.
  for (r in AI_TRAILER_PATTERNS) {
    expect_true(grepl(" ", r$query, fixed = TRUE) || !grepl(" ", r$query, fixed = TRUE))
  }
  spaced <- vapply(AI_TRAILER_PATTERNS, function(r) grepl(" ", r$query, fixed = TRUE), logical(1))
  expect_true(any(spaced),
              info = "if this ever goes all-FALSE the quoting bug stops being detectable here")
})
