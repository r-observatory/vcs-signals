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

  # A probe that errored used to read as Inf, i.e. as an unlimited budget, which
  # is the opposite of what a failed probe tells you. It now reads as none, and
  # the caller stops the shard rather than running on with no idea of the budget.
  io_err <- list(graphql = function(query) stop("network error"))
  expect_equal(graphql_rate_remaining(io_err), 0L)
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
  # space, which is why only these two tiers went silent at the time; tier A now
  # shares the same helper.
  #
  # Asserting on the shell form rather than on a live call, because the failure is
  # in how the argument is handed over, not in what the API does with it.
  src <- paste(readLines("../../scripts/github.R", warn = FALSE), collapse = "\n")
  # gregexpr returns -1 when nothing matches, and length(-1) is 1, so counting
  # this way reported one match for zero. With a single call site that made the
  # guard read 1 == 1 and pass whether the quoting was there or not: removing
  # shQuote entirely left the whole suite green. Count occurrences properly.
  n_matches <- function(pat, x) {
    m <- gregexpr(pat, x, fixed = TRUE)[[1]]
    if (length(m) == 1L && m[1] == -1L) 0L else length(m)
  }
  callsites <- n_matches('"search/commits"', src)
  quoted    <- n_matches('shQuote(paste0("q=", q))', src)
  expect_gt(callsites, 0L)
  expect_gt(quoted, 0L)
  expect_equal(quoted, callsites,
               info = "every commit-search helper quotes the q argument")
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

test_that("the tree query fetches the subtrees the ruleset actually reads", {
  # A rule naming vignettes/*.qmd or site/_litedown.yml can never fire if the
  # query never asks for those trees, which is a rule that looks right in the
  # config and reports zero forever.
  q <- build_tree_query(data.frame(owner = "o", name = "n", stringsAsFactors = FALSE))
  for (path in c("HEAD:", "HEAD:.github", "HEAD:inst", "HEAD:vignettes", "HEAD:site")) {
    expect_true(grepl(sprintf('expression: "%s"', path), q, fixed = TRUE), info = path)
  }
})

test_that("subtree entries reach the classifier under their own prefix", {
  resp <- list(data = list(r0 = list(
    isFork = FALSE, parent = NULL,
    rootTree = list(entries = list(list(name = "DESCRIPTION", type = "blob"),
                                   list(name = "vignettes", type = "tree"))),
    githubTree = NULL, claudeTree = NULL, agentsTree = NULL, instTree = NULL,
    vignettesTree = list(entries = list(list(name = "intro.qmd", type = "blob"))),
    siteTree = list(entries = list(list(name = "_litedown.yml", type = "blob"))),
    gitignore = NULL, rbuildignore = NULL)))
  got <- parse_tree_markers(resp, data.frame(repo_id = "github.com/o/n", owner = "o",
                                             name = "n", stringsAsFactors = FALSE))
  expect_true("vignettes/intro.qmd" %in% got[[1]]$root_entries)
  expect_true("site/_litedown.yml" %in% got[[1]]$root_entries)

  r <- classify_dev_tooling(got[[1]]$root_entries, got[[1]]$github_entries)
  expect_equal(r$has_litedown, 1L)
})

test_that("a refused rate-limit probe reads as no budget, not unlimited budget", {
  # The guard exists to stop a pass when the token is spent, and it returned Inf
  # on every failure shape: a caught transport error, and the 200-shaped
  # {"data":null,"errors":[{"type":"RATE_LIMITED"}]} that GitHub sends when the
  # budget is gone. Inf < AI_POINT_RESERVE is FALSE, so the pass continued
  # precisely when it should have stopped, and wrote rows with no onset date at
  # full speed because the failing fetch skips its own pacing sleep.
  throws  <- list(graphql = function(q) stop("connection reset"))
  limited <- list(graphql = function(q) list(data = NULL,
                    errors = list(list(type = "RATE_LIMITED", message = "exhausted"))))
  expect_equal(graphql_rate_remaining(throws), 0L)
  expect_equal(graphql_rate_remaining(limited), 0L)
  expect_true(graphql_rate_remaining(throws) < AI_POINT_RESERVE)
})

test_that("a real budget is reported as itself", {
  io <- list(graphql = function(q) list(data = list(rateLimit = list(remaining = 4200))))
  expect_equal(graphql_rate_remaining(io), 4200L)
})

test_that("a fake with no rateLimit field is not throttled", {
  # Test doubles answer other queries and carry no rateLimit. Treating that as a
  # spent budget would stop every fixture-driven scan.
  io <- list(graphql = function(q) list(data = list(repository = list(x = 1))))
  expect_true(is.infinite(graphql_rate_remaining(io)))
})


test_that("the counting helper itself distinguishes none from one", {
  # The bug this guard had. gregexpr signals no match with -1, whose length is
  # 1, so a naive count reports one match for zero and any equality against
  # another count of one silently holds.
  n_matches <- function(pat, x) {
    m <- gregexpr(pat, x, fixed = TRUE)[[1]]
    if (length(m) == 1L && m[1] == -1L) 0L else length(m)
  }
  expect_equal(n_matches("zzz", "abc"), 0L)
  expect_equal(length(gregexpr("zzz", "abc", fixed = TRUE)[[1]]), 1L)  # the trap
  expect_equal(n_matches("a", "abcabc"), 2L)
})
