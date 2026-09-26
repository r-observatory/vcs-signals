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

# A canary response in which every TREE_QUERY_CANARY floor holds: the own_community
# repositories point at their own files, the inheritors report their owner's .github template.
contents_canary_ok <- function() {
  own <- TREE_QUERY_CANARY$own_community
  slugs <- c(own, TREE_QUERY_CANARY$inherited_pr_template)
  alias <- function(s) {
    base <- list(nameWithOwner = s, isFork = FALSE, parent = NULL, issueTemplates = list(),
                 rootTree = list(entries = list(list(name = "DESCRIPTION", type = "blob"))))
    if (s %in% own) c(base, list(
      pullRequestTemplates = list(),
      codeOfConduct = list(url = sprintf("https://github.com/%s/blob/main/.github/CODE_OF_CONDUCT.md", s)),
      contributingGuidelines = list(url = sprintf("https://github.com/%s/blob/main/.github/CONTRIBUTING.md", s))))
    else c(base, list(
      pullRequestTemplates = list(list(filename = "pull_request_template.md",
        repository = list(nameWithOwner = paste0(sub("/.*$", "", s), "/.github")))),
      codeOfConduct = NULL, contributingGuidelines = NULL))
  }
  list(data = stats::setNames(lapply(slugs, alias), sprintf("r%d", seq_along(slugs) - 1L)))
}

# Wraps a fake graphql so the enumerate step's canary query gets a passing answer.
with_contents_canary <- function(graphql, canary = contents_canary_ok) function(query) {
  # Built from the constant at call time, so replacing a candidate keeps the query recognised.
  s <- unlist(TREE_QUERY_CANARY, use.names = FALSE)
  keys <- sprintf('owner: "%s", name: "%s"', sub("/.*$", "", s), sub("^[^/]*/", "", s))
  if (all(vapply(keys, grepl, logical(1), x = query, fixed = TRUE))) canary() else graphql(query)
}

# The seven repositories of fixtures/contents-2026-09.json, parsed and classified, by repo_id.
classified_fixture <- function() {
  raw <- jsonlite::fromJSON(test_path("fixtures", "contents-2026-09.json"), simplifyVector = FALSE)
  repos <- data.frame(repo_id = vapply(raw$repos, function(r) r$repo_id, ""),
                      owner = vapply(raw$repos, function(r) r$owner, ""),
                      name = vapply(raw$repos, function(r) r$name, ""), stringsAsFactors = FALSE)
  parsed <- parse_tree_markers(list(data = raw$data), repos)
  lapply(parsed, function(p) classify_dev_tooling(p$root_entries, p$github_entries, repo = p))
}
