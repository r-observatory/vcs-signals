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
