# Fakes for the weekly contents read, shared by the fetch, cheap-pass and canary tests.

# A contents io that answers each alias from its repository name, fails any batch
# naming a repository in `fail`, and records every query and every wait.
# `message` is a string, or a function called once per failing reply.
fake_contents_io <- function(fail = character(0), message = "Something went wrong while executing your query.",
                             alias = function(name) list(nameWithOwner = paste0("o/", name), isFork = FALSE,
                               parent = NULL, rootTree = list(entries = list(list(name = "DESCRIPTION", type = "blob")))),
                             remaining = function() 5000L) {
  log <- new.env(parent = emptyenv())
  log$queries <- character(0); log$slept <- numeric(0)
  list(
    graphql = function(query) {
      log$queries <- c(log$queries, query)
      if (grepl("rateLimit", query, fixed = TRUE))
        return(list(data = list(rateLimit = list(remaining = remaining(), resetAt = "2026-09-27T00:00:00Z"))))
      names <- sub('^name: "(.*)"$', "\\1", regmatches(query, gregexpr('name: "[^"]+"', query))[[1]])
      if (any(names %in% fail))
        return(list(errors = list(list(message = if (is.function(message)) message() else message))))
      if (grepl("pullRequests(first: 50", query, fixed = TRUE)) {
        empty <- list(pullRequests = list(pageInfo = list(endCursor = NA, hasNextPage = FALSE), nodes = list()))
        return(list(data = stats::setNames(rep(list(empty), length(names)), sprintf("r%d", seq_along(names) - 1L))))
      }
      list(data = stats::setNames(lapply(names, alias), sprintf("r%d", seq_along(names) - 1L)))
    },
    sleep = function(s) log$slept <- c(log$slept, s),
    log = log)
}

contents_queries <- function(io) io$log$queries[grepl("rootTree", io$log$queries, fixed = TRUE)]

# Repositories named p01, p02, ... as a roster or a batch.
fake_repos <- function(n, prefix = "p") {
  nm <- sprintf("%s%02d", prefix, seq_len(n))
  data.frame(repo_id = paste0("github.com/o/", nm), owner = "o", name = nm, node_id = NA_character_,
             done = 0L, stringsAsFactors = FALSE)
}

# BATCH_DELAY_S paces the untouched PR fetch with Sys.sleep; zero it for a test.
local_fast_batches <- function(env = parent.frame()) {
  old <- BATCH_DELAY_S
  assign("BATCH_DELAY_S", 0, envir = globalenv())
  withr::defer(assign("BATCH_DELAY_S", old, envir = globalenv()), envir = env)
}
