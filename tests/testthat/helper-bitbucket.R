WS <- "{5F9215A6-5741-4727-B5B6-88E66F6D8E66}"
RP <- "{0b2c396c-7791-4295-b801-2d08faace27f}"
NODE <- "bitbucket:5f9215a6-5741-4727-b5b6-88e66f6d8e66:0b2c396c-7791-4295-b801-2d08faace27f"
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
# A repository object as the fields= filter returns it.
bb_obj <- function(ws = WS, rp = RP, full_name = "bklamer/depower", main = "master", parent = NULL)
  list(uuid = rp, full_name = full_name, created_on = "2024-02-18T22:04:37.232840+00:00", size = 4741949L,
       parent = parent, mainbranch = if (is.null(main)) NULL else list(name = main),
       workspace = list(uuid = ws))
bb_fields <- function(path) paste0(path, "?fields=", BITBUCKET_REPO_FIELDS)
bb_count <- function(n) bb_answer(200L, list(size = n))
# The five answers behind one object: counts and the branch date.
bb_routes <- function(base = "/repositories/bklamer/depower", merged = bb_count(16L), main = "master") {
  r <- list(bb_count(2L), bb_count(1L), merged, bb_count(3L))
  names(r) <- c(paste0(base, "/forks?pagelen=1&fields=size"),
                paste0(base, "/pullrequests?state=", c("OPEN", "MERGED", "DECLINED"), "&pagelen=1&fields=size"))
  if (!is.null(main))
    r[[paste0(base, "/refs/branches/", main, "?fields=target.date")]] <-
      bb_answer(200L, list(target = list(date = "2026-06-15T02:37:55+00:00")))
  r
}
