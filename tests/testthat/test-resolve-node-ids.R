test_that("resolve_node_ids attaches ids, flags gone, and applies renames", {
  repos_needing <- data.frame(
    repo_id = c("github.com/tidyverse/ggplot2", "github.com/dead/repo", "github.com/old/name"),
    owner = c("tidyverse", "dead", "old"), name = c("ggplot2", "repo", "name"), stringsAsFactors = FALSE)
  io <- list(graphql = function(query) jsonlite::fromJSON(
    readLines("fixtures/resolve.json", warn = FALSE), simplifyVector = FALSE))
  out <- resolve_node_ids(io, repos_needing)
  expect_equal(out$node_id[out$repo_id == "github.com/tidyverse/ggplot2"], "R_a")
  expect_equal(out$status[out$repo_id == "github.com/dead/repo"], "gone")
  renamed <- out[out$repo_id == "github.com/old/name", ]
  expect_equal(renamed$name_with_owner, "new-owner/renamed")   # rename applied, repo_id unchanged
  expect_equal(renamed$status, "active")
})

test_that("resolve_node_ids defers (does not mark gone) on an error response", {
  repos_needing <- data.frame(repo_id = "github.com/o/n", owner = "o", name = "n", stringsAsFactors = FALSE)
  io_err <- list(graphql = function(query) list(data = NULL, errors = list(list(message = "RATE_LIMITED"))))
  out <- resolve_node_ids(io_err, repos_needing)
  expect_equal(nrow(out), 0)                     # deferred, no rows -> repo untouched, retried next run
  io_throw <- list(graphql = function(query) stop("502"))
  expect_equal(nrow(resolve_node_ids(io_throw, repos_needing)), 0)
})

test_that("one deleted repo does not discard the live repos batched alongside it", {
  # GitHub answers a batch containing a deleted repo with HTTP 200: partial data
  # (that alias null, the rest intact) plus an errors[] entry scoped to the alias.
  # The live repos in the batch must still resolve, or a single dead repo strands
  # its batch-mates with node_id NULL forever - they are re-selected, re-batched
  # with the same dead repo, and re-discarded on every subsequent run.
  repos_needing <- data.frame(
    repo_id = c("github.com/live/one", "github.com/dead/repo", "github.com/live/two"),
    owner = c("live", "dead", "live"), name = c("one", "repo", "two"), stringsAsFactors = FALSE)
  io <- list(graphql = function(query) list(
    data = list(
      r0 = list(id = "R_1", nameWithOwner = "live/one", isArchived = FALSE, isFork = FALSE,
                isMirror = FALSE, createdAt = "2020-01-01T00:00:00Z"),
      r1 = NULL,
      r2 = list(id = "R_2", nameWithOwner = "live/two", isArchived = FALSE, isFork = FALSE,
                isMirror = FALSE, createdAt = "2021-01-01T00:00:00Z")),
    errors = list(list(type = "NOT_FOUND", path = list("r1"),
                       message = "Could not resolve to a Repository with the name 'dead/repo'."))))
  out <- resolve_node_ids(io, repos_needing)
  expect_equal(nrow(out), 3)
  expect_equal(out$node_id[out$repo_id == "github.com/live/one"], "R_1")
  expect_equal(out$node_id[out$repo_id == "github.com/live/two"], "R_2")
  expect_equal(out$status[out$repo_id == "github.com/live/two"], "active")
  expect_equal(out$status[out$repo_id == "github.com/dead/repo"], "gone")
})

test_that("errors_are_alias_not_found separates a dead alias from a broken batch", {
  not_found <- list(list(type = "NOT_FOUND", path = list("r1"), message = "gone"))
  expect_true(errors_are_alias_not_found(not_found))
  # A rate limit or 502 is not alias-scoped: the batch is unusable and must be deferred,
  # never read as "every repo in it is gone".
  expect_false(errors_are_alias_not_found(list(list(type = "RATE_LIMITED", message = "slow down"))))
  expect_false(errors_are_alias_not_found(list(list(message = "502 Bad Gateway"))))
  # A mixed response is unusable too: the non-NOT_FOUND error may have nulled a live alias.
  expect_false(errors_are_alias_not_found(c(not_found, list(list(type = "RATE_LIMITED")))))
  expect_false(errors_are_alias_not_found(NULL))
})
