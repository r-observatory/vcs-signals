# Codeberg transport, pacing and locating (scripts/codeberg.R), over the fake in helper-codeberg.R.

test_that("cb_get sends the User-Agent and JSON accept headers with the Codeberg timeout, and never -L", {
  got <- new.env()
  run <- function(cmd, args, ...) { got$cmd <- cmd; got$args <- args; structure(character(), status = 7L) }
  cb_get("/repos/zeileis/zoo", run = run)
  expect_identical(got$cmd, "curl")
  expect_true(shQuote(paste("User-Agent:", CODEBERG_USER_AGENT)) %in% got$args)
  expect_true(shQuote("Accept: application/json") %in% got$args)
  expect_identical(got$args[match("--max-time", got$args) + 1L], "30")
  expect_identical(utils::tail(got$args, 1L), shQuote("https://codeberg.org/api/v1/repos/zeileis/zoo"))
  expect_false(any(got$args %in% c("-L", "--location")))
  expect_identical(CODEBERG_USER_AGENT, "r-observatory/vcs-signals (+https://github.com/r-observatory/vcs-signals)")
})

test_that("the ratelimit r, x-total-count and redirect locations are read, NA when absent or off codeberg.org", {
  expect_identical(cb_ratelimit_r(c(ratelimit = '"baseline";r=1999;t=600')), 1999L)
  expect_identical(cb_ratelimit_r(c(`ratelimit-policy` = '"baseline";q=2000;w=600')), NA_integer_)
  expect_identical(cb_total_count(c(`x-total-count` = "44")), 44L)
  expect_identical(cb_total_count(c(`x-total` = "44")), NA_integer_)
  expect_identical(cb_api_path("/api/v1/repos/a/b"), "/repos/a/b")
  expect_identical(cb_api_path("https://codeberg.org/api/v1/users/n/repos?limit=50&page=1"), "/users/n/repos?limit=50&page=1")
  expect_identical(cb_api_path("https://elsewhere.example/api/v1/repos/a/b"), NA_character_)
  expect_identical(cb_api_path("/a/b"), NA_character_)
  expect_identical(cb_node_id(2277662), "codeberg:codeberg.org:2277662")
  expect_identical(is_codeberg_node(c("codeberg:codeberg.org:1", "codeberg:other.org:1", "R_x", NA)), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("a 429 or an r under the floor closes Codeberg for the run without a breaker failure, and three 5xx defer it", {
  st <- cb_test_state()
  io <- list(cb_get = function(path) if (path == "/a") cb_answer(r = 399L) else cb_answer())
  expect_identical(.cb_ask(io, st, "/a")$state, "ok")
  expect_identical(c(st$closed, as.character(st$low_r)), c("ratelimit", "399"))
  expect_identical(.cb_ask(io, st, "/b")$state, "stopped")
  expect_identical(st$asked, 1L)
  st2 <- cb_test_state()
  expect_identical(.cb_ask(list(cb_get = function(path) cb_answer(429L)), st2, "/b")$state, "stopped")
  expect_identical(st2$closed, "429")
  expect_length(st2$pacer$deferred(), 0L)
  st3 <- cb_test_state()
  for (i in 1:3) expect_identical(.cb_ask(list(cb_get = function(path) cb_answer(503L)), st3, "/c")$state, "failed")
  expect_identical(st3$pacer$deferred(), CODEBERG_DOMAIN)
  st4 <- cb_test_state()
  for (s in c(404L, 409L, 403L, 404L)) .cb_ask(list(cb_get = function(path) cb_answer(s)), st4, "/d")
  expect_length(st4$pacer$deferred(), 0L)
  expect_identical(.cb_ask(list(cb_get = function(path) stop("boom")), cb_test_state(), "/e")$state, "failed")
})

test_that("cb_locate finds each repository in its owner's listing, by id when it has one and by name without case otherwise", {
  seen <- new.env()
  io <- list(cb_get = cb_fake(list(
    "/users/someone/repos?limit=50&page=1" = cb_list(list(cb_obj(11L, "someone/Alpha"), cb_obj(12L, "someone/beta-new")))), seen))
  repos <- cb_repos(c("someone/alpha", "someone/beta"), node_id = c(NA, "codeberg:codeberg.org:12"))
  loc <- cb_locate(io, repos, cb_test_state())
  expect_identical(loc$found$repo_id, repos$repo_id)
  expect_identical(loc$found$node_id, c("codeberg:codeberg.org:11", "codeberg:codeberg.org:12"))
  expect_identical(loc$ids$repo_id, "codeberg.org/someone/alpha")
  expect_identical(unlist(loc$ids[1, c("owner", "name", "name_with_owner", "status")], use.names = FALSE),
                   c("someone", "Alpha", "someone/Alpha", "active"))
  expect_identical(seen$paths, "/users/someone/repos?limit=50&page=1")
})

test_that("a repository missing from its owner's listing is asked by path, and one 301 on codeberg.org is followed when the id matches", {
  seen <- new.env()
  io <- list(cb_get = cb_fake(list(
    "/users/someone/repos?limit=50&page=1" = cb_list(list()),
    "/repos/someone/gamma" = cb_answer(301L, headers = c(location = "/api/v1/repos/neworg/gamma")),
    "/repos/neworg/gamma" = cb_answer(body = cb_obj(13L, "neworg/gamma"))), seen))
  loc <- cb_locate(io, cb_repos("someone/gamma", "codeberg:codeberg.org:13"), cb_test_state())
  expect_identical(loc$found$node_id, "codeberg:codeberg.org:13")
  expect_identical(loc$objects[["codeberg:codeberg.org:13"]]$full_name, "neworg/gamma")
  expect_identical(seen$paths, c("/users/someone/repos?limit=50&page=1", "/repos/someone/gamma", "/repos/neworg/gamma"))
})

test_that("a 301 to another id, a 301 off codeberg.org, a second 301 and a 404 leave the repository not located, never gone", {
  seen <- new.env()
  io <- list(cb_get = cb_fake(list(
    "/users/o/repos?limit=50&page=1" = cb_list(list()),
    "/repos/o/a" = cb_answer(301L, headers = c(location = "/api/v1/repos/x/a")),
    "/repos/x/a" = cb_answer(body = cb_obj(99L, "x/a")),
    "/repos/o/b" = cb_answer(301L, headers = c(location = "https://elsewhere.example/api/v1/repos/x/b")),
    "/repos/o/c" = cb_answer(301L, headers = c(location = "/api/v1/repos/x/c")),
    "/repos/x/c" = cb_answer(301L, headers = c(location = "/api/v1/repos/y/c"))), seen))
  repos <- cb_repos(c("o/a", "o/b", "o/c", "o/d"), node_id = c("codeberg:codeberg.org:21", NA, NA, "codeberg:codeberg.org:24"))
  loc <- cb_locate(io, repos, cb_test_state())
  expect_identical(nrow(loc$found), 0L)
  expect_identical(sort(loc$not_located), repos$repo_id)
  expect_identical(nrow(loc$ids), 0L)
  expect_false(any(grepl("elsewhere|/repos/y/", seen$paths)))
})

test_that("an owner listing follows one owner rename and pages past 50", {
  objs <- lapply(1:51, function(i) cb_obj(100L + i, sprintf("newname/r%d", i)))
  seen <- new.env()
  io <- list(cb_get = cb_fake(list(
    "/users/oldname/repos?limit=50&page=1" = cb_answer(301L, headers = c(location = "/api/v1/users/newname/repos?limit=50&page=1")),
    "/users/newname/repos?limit=50&page=1" = cb_list(objs[1:50], total = 51L),
    "/users/newname/repos?limit=50&page=2" = cb_list(objs[51], total = 51L)), seen))
  loc <- cb_locate(io, cb_repos("oldname/r51", "codeberg:codeberg.org:151"), cb_test_state())
  expect_identical(loc$found$node_id, "codeberg:codeberg.org:151")
  expect_length(seen$paths, 3L)
})

test_that("a failed listing defers its owner's repositories without asking each by path, and an object without an id is not data", {
  seen <- new.env()
  io <- list(cb_get = cb_fake(list(
    "/users/down/repos?limit=50&page=1" = cb_answer(502L),
    "/users/up/repos?limit=50&page=1" = cb_list(list(list(full_name = "up/x"), cb_obj(31L, "up/y")))), seen))
  loc <- cb_locate(io, cb_repos(c("down/a", "up/x", "up/y")), cb_test_state())
  expect_identical(loc$deferred, "codeberg.org/down/a")
  expect_identical(loc$found$repo_id, "codeberg.org/up/y")
  expect_identical(loc$not_located, "codeberg.org/up/x")
  expect_false("/repos/down/a" %in% seen$paths)
})

test_that("nothing is asked past the deadline, and what is left is unreached", {
  clock <- cb_clock()
  io <- list(cb_get = function(path) { clock$env$t <- clock$env$t + 60; cb_list(list(cb_obj(41L, "a/one"))) })
  loc <- cb_locate(io, cb_repos(c("a/one", "b/two")), cb_test_state(clock), deadline = clock$now() + 30, now = clock$now)
  expect_identical(loc$found$repo_id, "codeberg.org/a/one")
  expect_identical(loc$unreached, "codeberg.org/b/two")
})

test_that("a redirect with a dot segment is refused, an empty page ends a listing, and an answer of the wrong shape defers", {
  expect_identical(cb_api_path("/api/v1/repos/../admin"), NA_character_)
  expect_identical(cb_api_path("https://codeberg.org/api/v1/users/x/../../repos/a/b"), NA_character_)
  expect_identical(cb_api_path("/api/v1/repos/a\\b/c"), NA_character_)
  seen <- new.env()
  io <- list(cb_get = cb_fake(list(
    "/users/odd/repos?limit=50&page=1" = cb_answer(body = list(message = "not a list")),
    "/users/ok/repos?limit=50&page=1" = cb_list(list(), total = 5L),
    "/repos/ok/a" = cb_answer(body = list(message = "not a repository"))), seen))
  loc <- cb_locate(io, cb_repos(c("odd/x", "ok/a")), cb_test_state())
  expect_identical(loc$deferred, c("codeberg.org/odd/x", "codeberg.org/ok/a"))
  expect_identical(seen$paths, c("/users/odd/repos?limit=50&page=1", "/users/ok/repos?limit=50&page=1", "/repos/ok/a"))
})

test_that("a listing page whose r closes Codeberg is still used, and what is left is deferred without a request", {
  seen <- new.env()
  objs <- lapply(1:50, function(i) cb_obj(200L + i, sprintf("big/r%d", i)))
  io <- list(cb_get = cb_fake(list(
    "/users/big/repos?limit=50&page=1" = cb_list(objs, total = 51L, r = 399L)), seen))
  loc <- cb_locate(io, cb_repos(c("big/r1", "big/r51", "zed/z")), cb_test_state())
  expect_identical(loc$found$repo_id, "codeberg.org/big/r1")
  expect_identical(loc$deferred, c("codeberg.org/big/r51", "codeberg.org/zed/z"))
  expect_identical(seen$paths, "/users/big/repos?limit=50&page=1")
})

test_that("each repository not located is logged by repo_id, a 429 is logged once, and no log line holds an address", {
  logs <- new.env()
  logs$lines <- character(0)
  st <- cb_test_state(log = function(m) logs$lines <- c(logs$lines, m))
  io <- list(cb_get = cb_fake(list(
    "/users/o/repos?limit=50&page=1" = cb_list(list(cb_obj(5L, "o/other", owner = list(id = 1L, login = "o", email = "x@example.invalid")))),
    "/repos/p/q" = cb_answer(429L))))
  loc <- cb_locate(io, cb_repos(c("o/gone", "p/q", "p/r")), st)
  expect_identical(loc$not_located, "codeberg.org/o/gone")
  expect_identical(sort(loc$deferred), c("codeberg.org/p/q", "codeberg.org/p/r"))
  expect_true(any(grepl("codeberg.org/o/gone: not located", logs$lines, fixed = TRUE)))
  expect_identical(sum(grepl("429", logs$lines, fixed = TRUE)), 1L)
  expect_false(any(grepl("@", logs$lines, fixed = TRUE)))
})

test_that("codeberg_active_repos lists the active Codeberg rows by repo_id, with or without a node id", {
  con <- new_test_db()
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSERT INTO repos (repo_id, node_id, host, host_domain, owner, name, name_with_owner,
    supported, n_packages, first_seen, last_seen, status) VALUES
    ('codeberg.org/zeileis/zoo', NULL, 'codeberg', 'codeberg.org', 'zeileis', 'zoo', 'zeileis/zoo', 0, 1, '2026-10-01', '2026-10-01', 'active'),
    ('codeberg.org/aitap/rcurl', 'codeberg:codeberg.org:7', 'codeberg', 'codeberg.org', 'aitap', 'RCurl', 'aitap/RCurl', 0, 1, '2026-10-01', '2026-10-01', 'active'),
    ('codeberg.org/r-packages/rock', NULL, 'codeberg', 'codeberg.org', 'R-packages', 'rock', 'R-packages/rock', 0, 1, '2026-10-01', '2026-10-01', 'retired'),
    ('github.com/tidyverse/ggplot2', 'R_x', 'github', 'github.com', 'tidyverse', 'ggplot2', 'tidyverse/ggplot2', 1, 1, '2026-10-01', '2026-10-01', 'active')")
  got <- codeberg_active_repos(con)
  expect_identical(names(got), c("repo_id", "node_id", "owner", "name"))
  expect_identical(got$repo_id, c("codeberg.org/aitap/rcurl", "codeberg.org/zeileis/zoo"))
  expect_identical(got$node_id, c("codeberg:codeberg.org:7", NA))
  expect_identical(got$name, c("RCurl", "zoo"))
})

test_that("the recorded fixtures hold no address", {
  for (f in list.files(file.path("fixtures", "codeberg"), full.names = TRUE))
    expect_false(any(grepl("@", readLines(f, warn = FALSE), fixed = TRUE)), info = f)
})
