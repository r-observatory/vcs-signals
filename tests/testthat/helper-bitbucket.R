# A fake Bitbucket: a clock, a run state on it, answers shaped like http_request() results.
bb_clock <- function(t = 1e9) {
  e <- new.env(); e$t <- t
  list(now = function() e$t, sleep = function(s) e$t <- e$t + s, env = e)
}
bb_test_state <- function(clock = bb_clock(), log = NULL, run_budget = BITBUCKET_MAX_REQUESTS)
  cb_run_state(bb_pacer(now = clock$now, sleep = clock$sleep, run_budget = run_budget), log)
bb_answer <- function(status = 200L, body = list())
  list(status = status, error = if (status >= 400L) sprintf("HTTP %d", status) else NA_character_,
       headers = character(),
       body = as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")))
# Answers by exact path, 404 for any other; every path asked is kept in seen$paths.
bb_fake <- function(routes, seen = new.env()) {
  seen$paths <- character(0)
  function(path) {
    seen$paths <- c(seen$paths, path)
    hit <- routes[[path]]
    if (is.null(hit)) bb_answer(404L, list(type = "error"))
    else if (is.function(hit)) hit() else hit
  }
}
