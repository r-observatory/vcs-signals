# A fake Codeberg: a clock, a run state on it, answers shaped like http_request() results.
cb_clock <- function(t = 1e9) {
  e <- new.env(); e$t <- t
  list(now = function() e$t, sleep = function(s) e$t <- e$t + s, env = e)
}
cb_test_state <- function(clock = cb_clock(), log = NULL, run_budget = CODEBERG_MAX_REQUESTS)
  cb_run_state(cb_pacer(now = clock$now, sleep = clock$sleep, run_budget = run_budget), log)
cb_answer <- function(status = 200L, body = list(), headers = character(), r = 1990L)
  list(status = status, error = if (status >= 400L) sprintf("HTTP %d", status) else NA_character_,
       headers = c(headers, ratelimit = sprintf('"baseline";r=%d;t=600', r)),
       body = as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")))
cb_list <- function(objs, total = length(objs), ...)
  cb_answer(body = objs, headers = c(`x-total-count` = as.character(total)), ...)
# Replaces each given field whole; modifyList would merge a nested list such as topics.
cb_obj <- function(id, full_name, ...) {
  o <- list(id = id, full_name = full_name, owner = list(id = 1L, login = sub("/.*$", "", full_name)),
    empty = FALSE, fork = FALSE, mirror = FALSE, archived = FALSE, original_url = "", website = "",
    clone_url = sprintf("https://codeberg.org/%s.git", full_name), stars_count = 3L, forks_count = 1L,
    watchers_count = 2L, open_issues_count = 4L, open_pr_counter = 1L, release_counter = 2L, size = 1200L,
    topics = list("r", "stats"), created_at = "2024-01-23T22:06:52+01:00", has_issues = TRUE,
    has_pull_requests = TRUE, has_releases = TRUE, default_branch = "main")
  extra <- list(...)
  for (k in names(extra)) o[k] <- list(extra[[k]])
  o
}
cb_objects <- function(...) { o <- list(...); stats::setNames(o, vapply(o, function(x) cb_node_id(x$id), "")) }
cb_repos <- function(slugs, node_id = NA_character_)
  data.frame(repo_id = paste0("codeberg.org/", tolower(slugs)),
             node_id = as.character(rep(node_id, length.out = length(slugs))),
             owner = sub("/.*$", "", slugs), name = sub("^.*/", "", slugs), stringsAsFactors = FALSE)
# Answers by exact path, 404 for any other; every path asked is kept in seen$paths.
cb_fake <- function(routes, seen = new.env()) {
  seen$paths <- character(0)
  function(path) {
    seen$paths <- c(seen$paths, path)
    hit <- routes[[path]]
    if (is.null(hit)) cb_answer(404L, list(message = "The target couldn't be found."))
    else if (is.function(hit)) hit() else hit
  }
}
cb_fixture <- function(f) jsonlite::fromJSON(file.path("fixtures", "codeberg", f), simplifyVector = FALSE)
