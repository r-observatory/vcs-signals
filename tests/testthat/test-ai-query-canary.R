.aqc <- setwd(.repo_root); source(file.path(.repo_root, "scripts", "ai_backfill.R")); setwd(.aqc)

.stub_tree_canary <- function(calls) {
  orig <- tree_query_canary
  tree_query_canary <<- function(io) { calls$order <- c(calls$order, "contents"); invisible(TRUE) }
  orig
}

test_that("the documents pass on responses GitHub gave, contents checked first", {
  calls <- new.env(); calls$order <- character(0)
  orig <- .stub_tree_canary(calls); on.exit(tree_query_canary <<- orig, add = TRUE)
  expect_message(ai_query_canary(ai_canary_io()), "AI query canary: passed")
  expect_equal(calls$order, "contents")
})

test_that("a GitHub query fault stops the run before anything is read", {
  calls <- new.env(); orig <- .stub_tree_canary(calls); on.exit(tree_query_canary <<- orig, add = TRUE)
  broken <- list(data = NULL, errors = list(list(message = "Something went wrong while executing your query")))
  expect_error(ai_query_canary(ai_canary_io(accounts = broken)), "account-count")
})

test_that("a repository missing from the activity or account document stops the run", {
  calls <- new.env(); orig <- .stub_tree_canary(calls); on.exit(tree_query_canary <<- orig, add = TRUE)
  one_null <- list(data = list(r0 = NULL, r1 = NULL))
  expect_error(ai_query_canary(ai_canary_io(activity = one_null)), "shinyglass")
  acc <- ai_canary_io()$graphql("a_claude:")
  acc$data["r1"] <- list(NULL)
  expect_error(ai_query_canary(ai_canary_io(accounts = acc)), "addivortes")
})

test_that("the github-actions account reaching the Claude count stops the run", {
  calls <- new.env(); orig <- .stub_tree_canary(calls); on.exit(tree_query_canary <<- orig, add = TRUE)
  acc <- ai_canary_io()$graphql("a_claude:")
  acc$data$r0$defaultBranchRef$target$a_claude <- list(totalCount = 10L, nodes = list(list(committedDate = "2024-10-25T00:00:00Z")))
  expect_error(ai_query_canary(ai_canary_io(accounts = acc)), "ss3sim")
})

test_that("a fork's Cursor branch that would count as the package's use stops the run", {
  calls <- new.env(); orig <- .stub_tree_canary(calls); on.exit(tree_query_canary <<- orig, add = TRUE)
  fx <- ai_canary_io()$graphql("pullRequest(number:")
  fx$data$p1$pullRequest$isCrossRepository <- FALSE
  fx$data$p1$pullRequest$authorAssociation <- "OWNER"
  expect_error(ai_query_canary(ai_canary_io(fixed = fx)), "arrow-nanoarrow")
})
