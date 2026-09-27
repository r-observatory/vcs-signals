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

test_that("a request that throws is sent once more after the wait, and a second throw stops the run", {
  calls <- new.env(); orig <- .stub_tree_canary(calls); on.exit(tree_query_canary <<- orig, add = TRUE)
  waits <- numeric(0); n <- 0L
  io <- ai_canary_io(); answer <- io$graphql
  io$sleep <- function(s) waits <<- c(waits, s)
  io$graphql <- function(q) {
    if (grepl("a_claude:", q, fixed = TRUE)) { n <<- n + 1L; if (n == 1L) stop("gh api graphql returned no output") }
    answer(q)
  }
  expect_message(ai_query_canary(io), "AI query canary: passed")
  expect_equal(waits, AI_BATCH_RETRY_WAIT_S); expect_equal(n, 2L)
  waits <- numeric(0)
  io$graphql <- function(q)
    if (grepl("a_claude:", q, fixed = TRUE)) stop("gh api graphql returned no output") else answer(q)
  expect_error(ai_query_canary(io), "account-count document failed: gh api graphql returned no output")
  expect_equal(waits, AI_BATCH_RETRY_WAIT_S)
})

test_that("a reply GitHub answered is final and is not sent again", {
  calls <- new.env(); orig <- .stub_tree_canary(calls); on.exit(tree_query_canary <<- orig, add = TRUE)
  waits <- numeric(0); n <- 0L
  broken <- list(data = NULL, errors = list(list(message = "Something went wrong while executing your query")))
  io <- ai_canary_io(accounts = broken); answer <- io$graphql
  io$sleep <- function(s) waits <<- c(waits, s)
  io$graphql <- function(q) { if (grepl("a_claude:", q, fixed = TRUE)) n <<- n + 1L; answer(q) }
  expect_error(ai_query_canary(io), "account-count document returned no data")
  expect_equal(waits, numeric(0)); expect_equal(n, 1L)
})

test_that("an activity reply with no pull requests or no commits stops the run", {
  calls <- new.env(); orig <- .stub_tree_canary(calls); on.exit(tree_query_canary <<- orig, add = TRUE)
  act <- ai_canary_io()$graphql("activity")
  no_prs <- act; no_prs$data$r0$pullRequests$nodes <- list()
  expect_error(ai_query_canary(ai_canary_io(activity = no_prs)), "ericrayanderson/shinyglass read no pull requests")
  no_commits <- act; no_commits$data$r1$defaultBranchRef$target$recent$nodes <- list()
  expect_error(ai_query_canary(ai_canary_io(activity = no_commits)), "ss3sim/ss3sim read no commits")
})

test_that("arrow-nanoarrow #927 with a plain branch name stops the run naming it", {
  calls <- new.env(); orig <- .stub_tree_canary(calls); on.exit(tree_query_canary <<- orig, add = TRUE)
  fx <- ai_canary_io()$graphql("pullRequest(number:")
  fx$data$p1$pullRequest$headRefName <- "fix-bindings"
  expect_error(ai_query_canary(ai_canary_io(fixed = fx)),
               "apache/arrow-nanoarrow #927 no longer reads as a Cursor pull request from outside the project")
})

test_that("a ballgown commit that names no tool says so", {
  calls <- new.env(); orig <- .stub_tree_canary(calls); on.exit(tree_query_canary <<- orig, add = TRUE)
  fx <- ai_canary_io()$graphql("pullRequest(number:")
  fx$data$c1$object$message <- "Update the vignette"
  expect_error(ai_query_canary(ai_canary_io(fixed = fx)), "alyssafrazee/ballgown ab1da7b names no tool,")
})
