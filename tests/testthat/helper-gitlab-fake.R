# A fake GitLab over answer(domain, query, variables) and rest(domain, path); every call is kept.
fake_gitlab <- function(answer = NULL, rest = NULL) {
  calls <- new.env(parent = emptyenv()); calls$graphql <- list(); calls$rest <- character(0)
  list(calls = calls, io = list(
    gl_graphql = function(domain, query, variables = NULL) {
      calls$graphql[[length(calls$graphql) + 1L]] <- list(domain = domain, query = query, variables = variables)
      answer(domain, query, variables) },
    gl_rest = function(domain, path) { calls$rest <- c(calls$rest, paste(domain, path)); rest(domain, path) }))
}
gl_ok <- function(nodes) list(data = list(projects = list(nodes = nodes)), errors = NULL, status = 200L, error = NA_character_)
gl_free_pacer <- function() gl_pacer(now = function() 0, sleep = function(s) NULL, run_budget = 1000L)
gl_contents_fixture <- function(f) jsonlite::fromJSON(test_path("fixtures", "gitlab", f), simplifyVector = FALSE)
