# Codeberg transport, pacing, locating and daily gauges (scripts/codeberg.R), over the fake in helper-codeberg.R.

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

test_that("the baseline ratelimit r is read in either header order, the first r when none is baseline, and a search r closes nothing", {
  # The shape of the recon's web filter answer: the search policy first, then the baseline.
  h <- c(`ratelimit-policy` = '"search";q=30;w=60', ratelimit = '"search";r=29;t=60',
         `ratelimit-policy` = '"baseline";q=2000;w=600', ratelimit = '"baseline";r=1922;t=600')
  expect_identical(cb_ratelimit_r(h), 1922L)
  expect_identical(cb_ratelimit_r(rev(h)), 1922L)
  expect_identical(cb_ratelimit_r(c(ratelimit = '"other";r=7;t=60', ratelimit = '"more";r=9;t=60')), 7L)
  st <- cb_test_state()
  a <- .cb_ask(list(cb_get = function(path) list(status = 200L, error = NA_character_, headers = h, body = "[]")), st, "/x")
  expect_identical(a$state, "ok")
  expect_identical(st$closed, NA_character_)
  expect_identical(st$low_r, 1922L)
})

test_that("a 308 is a move like a 301, followed once by path with the id check and on a listing to the new owner, while 302 and 307 fail", {
  seen <- new.env()
  io <- list(cb_get = cb_fake(list(
    "/users/old/repos?limit=50&page=1" = cb_answer(308L, headers = c(location = "/api/v1/users/new/repos?limit=50&page=1")),
    "/users/new/repos?limit=50&page=1" = cb_list(list(cb_obj(51L, "new/kept"))),
    "/repos/old/moved" = cb_answer(308L, headers = c(location = "https://codeberg.org/api/v1/repos/neworg/moved")),
    "/repos/neworg/moved" = cb_answer(body = cb_obj(52L, "neworg/moved")),
    "/repos/old/taken" = cb_answer(308L, headers = c(location = "/api/v1/repos/neworg/taken")),
    "/repos/neworg/taken" = cb_answer(body = cb_obj(99L, "neworg/taken"))), seen))
  repos <- cb_repos(c("old/kept", "old/moved", "old/taken"), node_id = c("codeberg:codeberg.org:51", NA, "codeberg:codeberg.org:53"))
  loc <- cb_locate(io, repos, cb_test_state())
  expect_identical(loc$found$repo_id, c("codeberg.org/old/kept", "codeberg.org/old/moved"))
  expect_identical(loc$found$node_id, c("codeberg:codeberg.org:51", "codeberg:codeberg.org:52"))
  expect_identical(loc$not_located, "codeberg.org/old/taken")
  expect_identical(seen$paths, c("/users/old/repos?limit=50&page=1", "/users/new/repos?limit=50&page=1",
    "/repos/old/moved", "/repos/neworg/moved", "/repos/old/taken", "/repos/neworg/taken"))
  for (s in c(302L, 307L)) {
    a <- .cb_ask(list(cb_get = function(path) cb_answer(s, headers = c(location = "/api/v1/repos/x/y"))), cb_test_state(), "/repos/a/b")
    expect_identical(a$state, "failed")
  }
})

test_that("a 403 or 410 by path is an answer: the repository is not located and logged, and Codeberg goes on", {
  logs <- new.env()
  logs$lines <- character(0)
  st <- cb_test_state(log = function(m) logs$lines <- c(logs$lines, m))
  expect_identical(.cb_ask(list(cb_get = function(path) cb_answer(410L)), st, "/repos/a/b")$state, "absent")
  expect_identical(.cb_ask(list(cb_get = function(path) cb_answer(403L)), st, "/repos/a/b")$state, "absent")
  io <- list(cb_get = cb_fake(list(
    "/users/o/repos?limit=50&page=1" = cb_list(list()),
    "/repos/o/blocked" = cb_answer(403L),
    "/repos/o/removed" = cb_answer(410L),
    "/repos/o/here" = cb_answer(body = cb_obj(61L, "o/here")))))
  loc <- cb_locate(io, cb_repos(c("o/blocked", "o/here", "o/removed")), st)
  expect_identical(loc$not_located, c("codeberg.org/o/blocked", "codeberg.org/o/removed"))
  expect_identical(loc$found$repo_id, "codeberg.org/o/here")
  expect_length(loc$deferred, 0L)
  expect_true(any(grepl("codeberg.org/o/removed: not located (HTTP 410)", logs$lines, fixed = TRUE)))
  expect_length(st$pacer$deferred(), 0L)
})

test_that("a repository deferred while asked by path is logged by repo_id with what it got", {
  logs <- new.env()
  logs$lines <- character(0)
  st <- cb_test_state(log = function(m) logs$lines <- c(logs$lines, m))
  io <- list(cb_get = cb_fake(list(
    "/users/o/repos?limit=50&page=1" = cb_list(list()),
    "/repos/o/s" = cb_answer(503L),
    "/repos/o/t" = function() stop("Timeout was reached"),
    "/repos/o/w" = cb_answer(body = list(message = "not a repository")),
    "/repos/o/x" = cb_answer(302L, headers = c(location = "/api/v1/repos/o/y")),
    "/repos/o/z" = cb_answer(401L))))
  loc <- cb_locate(io, cb_repos(c("o/s", "o/t", "o/w", "o/x", "o/z")), st)
  expect_identical(loc$deferred, paste0("codeberg.org/o/", c("s", "t", "w", "x", "z")))
  expect_identical(logs$lines, sprintf("Codeberg codeberg.org/o/%s: deferred (%s), asked again next run", c("s", "t", "w", "x", "z"),
                                       c("HTTP 503", "no answer", "HTTP 200, not a repository", "HTTP 302", "HTTP 401")))
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

test_that("cb_gauge_row maps the recorded zoo object to the gauge columns", {
  obj <- cb_fixture("repo_zoo.json")
  row <- cb_gauge_row(obj, utils::modifyList(cb_extras_na(), list(issues_closed = 5L, prs_closed = 1L, prs_merged = 2L)))
  expect_identical(names(row), names(rows_df_empty_gauges()))
  expect_identical(vapply(row, class, ""), vapply(rows_df_empty_gauges(), class, ""))
  expect_identical(c(row$node_id, row$name_with_owner), c("codeberg:codeberg.org:2277662", "zeileis/zoo"))
  expect_identical(c(row$stars, row$forks, row$watchers, row$issues_open, row$prs_open, row$size_kb), c(1L, 0L, 1L, 0L, 0L, 23708L))
  expect_identical(c(row$issues_closed, row$prs_closed, row$prs_merged), c(5L, 1L, 2L))
  expect_true(is.na(row$releases_total))
  expect_identical(c(row$topics, row$created_at), c("", "2026-07-15T20:11:48Z"))
  expect_identical(c(row$is_archived, row$is_fork, row$is_mirror), c(0L, 0L, 0L))
  for (k in c("license", "owner_login", "owner_type", "owner_node_id", "pushed_at")) expect_true(is.na(row[[k]]), info = k)
})

test_that("cb_repo_extras reads closed issues, the merged split, the newest release and the newest commit date", {
  obj <- cb_obj(1345721L, "ropensci/nasapower", release_counter = 33L)
  seen <- new.env()
  a <- cb_repo_extras(list(cb_get = cb_fake(cb_gauge_routes("ropensci/nasapower", issues_closed = 77L), seen)), obj, cb_test_state())
  expect_identical(a$state, "ok")
  row <- cb_gauge_row(obj, a$extras)
  expect_identical(c(row$issues_closed, row$prs_closed, row$prs_merged, row$releases_total), c(77L, 1L, 1L, 33L))
  expect_identical(c(row$last_release_tag, row$last_release_at, row$head_committed_at),
                   c("v4.2.5", "2025-05-31T12:37:42Z", "2026-08-09T23:35:04Z"))
  expect_length(seen$paths, 4L)
})

test_that("a disabled unit or an external tracker reads NA and costs no request, and releases off hide a counter above 0", {
  seen <- new.env()
  off <- cb_obj(106L, "o/off", has_issues = FALSE, has_pull_requests = FALSE, has_releases = FALSE, release_counter = 7L)
  a <- cb_repo_extras(list(cb_get = cb_fake(cb_gauge_routes("o/off"), seen)), off, cb_test_state())
  expect_identical(seen$paths, "/repos/o/off/commits?limit=1&stat=false&verification=false&files=false")
  row <- cb_gauge_row(off, a$extras)
  for (k in c("issues_open", "issues_closed", "prs_open", "prs_closed", "prs_merged", "releases_total", "last_release_tag"))
    expect_true(is.na(row[[k]]), info = k)
  seen2 <- new.env()
  ext <- cb_obj(107L, "o/ext", external_tracker = list(external_tracker_url = "https://bugs.example/"))
  b <- cb_repo_extras(list(cb_get = cb_fake(cb_gauge_routes("o/ext"), seen2)), ext, cb_test_state())
  expect_false(any(grepl("type=issues", seen2$paths)))
  expect_true(is.na(cb_gauge_row(ext, b$extras)$issues_open))
  seen3 <- new.env()
  cb_repo_extras(list(cb_get = cb_fake(cb_gauge_routes("o/none"), seen3)), cb_obj(109L, "o/none", release_counter = 0L), cb_test_state())
  expect_false(any(grepl("/releases/", seen3$paths)))
})

test_that("a repository with only pre-releases keeps its count with no newest release", {
  obj <- cb_obj(108L, "o/pre", release_counter = 4L)
  a <- cb_repo_extras(list(cb_get = cb_fake(cb_gauge_routes("o/pre", latest = cb_answer(404L)))), obj, cb_test_state())
  row <- cb_gauge_row(obj, a$extras)
  expect_identical(a$state, "ok")
  expect_identical(row$releases_total, 4L)
  expect_true(is.na(row$last_release_tag)); expect_true(is.na(row$last_release_at))
})

test_that("an empty repository asks no commit, and a 409 from the commit list reads NA", {
  routes <- cb_gauge_routes("o/e", commit = cb_answer(409L, list(message = "Git Repository is empty.")))
  seen <- new.env()
  cb_repo_extras(list(cb_get = cb_fake(routes, seen)), cb_obj(105L, "o/e", empty = TRUE), cb_test_state())
  expect_false(any(grepl("/commits", seen$paths)))
  b <- cb_repo_extras(list(cb_get = cb_fake(routes)), cb_obj(105L, "o/e"), cb_test_state())
  expect_identical(b$state, "ok"); expect_true(is.na(b$extras$head_committed_at))
})

test_that("closed pull requests are split into merged and closed over pages of 50, and past 500 both are NA after one page", {
  p1 <- c(rep(list(cb_pr(TRUE)), 45), rep(list(cb_pr(FALSE)), 5)); p2 <- c(rep(list(cb_pr(TRUE)), 5), rep(list(cb_pr(FALSE)), 5))
  a <- cb_repo_extras(list(cb_get = cb_fake(cb_gauge_routes("o/pr", pulls = list(cb_list(p1, total = 60L), cb_list(p2, total = 60L))))),
                      cb_obj(99L, "o/pr"), cb_test_state())
  expect_identical(c(a$extras$prs_merged, a$extras$prs_closed), c(50L, 10L))
  seen <- new.env()
  big <- cb_gauge_routes("o/big", pulls = list(cb_list(rep(list(cb_pr(TRUE)), 50), total = 501L)))
  b <- cb_repo_extras(list(cb_get = cb_fake(big, seen)), cb_obj(98L, "o/big"), cb_test_state())
  expect_identical(c(b$extras$prs_closed, b$extras$prs_merged), c(NA_integer_, NA_integer_))
  expect_identical(sum(grepl("type=pulls", seen$paths)), 1L)
})

test_that("a pull request row without a merged flag, or pages that end short of the total, leave both counts NA", {
  odd <- cb_gauge_routes("o/odd", pulls = list(cb_list(list(cb_pr(TRUE), list(number = 2L, pull_request = list())))))
  a <- cb_repo_extras(list(cb_get = cb_fake(odd)), cb_obj(97L, "o/odd"), cb_test_state())
  expect_identical(a$state, "ok")
  expect_identical(c(a$extras$prs_closed, a$extras$prs_merged), c(NA_integer_, NA_integer_))
  short <- cb_gauge_routes("o/short", pulls = list(cb_list(rep(list(cb_pr(TRUE)), 50), total = 60L), cb_list(list(), total = 60L)))
  b <- cb_repo_extras(list(cb_get = cb_fake(short)), cb_obj(96L, "o/short"), cb_test_state())
  expect_identical(b$state, "ok")
  expect_identical(c(b$extras$prs_closed, b$extras$prs_merged), c(NA_integer_, NA_integer_))
})

test_that("a count list without x-total-count, a move or a body of the wrong shape fails the repository with no further call", {
  bad <- list(
    "/repos/o/w/issues?state=closed&type=issues&limit=1" = cb_answer(body = list()),
    "/repos/o/w/issues?state=closed&type=pulls&limit=50&page=1" = cb_answer(body = list(cb_pr(TRUE))),
    "/repos/o/w/releases/latest" = cb_answer(301L, headers = c(location = "/api/v1/repos/x/w/releases/latest")),
    "/repos/o/w/commits?limit=1&stat=false&verification=false&files=false" = cb_answer(body = list(message = "x")))
  for (p in names(bad)) {
    routes <- cb_gauge_routes("o/w")
    routes[[p]] <- bad[[p]]
    seen <- new.env()
    a <- cb_repo_extras(list(cb_get = cb_fake(routes, seen)), cb_obj(95L, "o/w"), cb_test_state())
    expect_identical(a$state, "failed", info = p)
    expect_identical(utils::tail(seen$paths, 1L), p, info = p)
  }
})

test_that("only a 404 from the newest release and a 409 from the commit list are answers: any other 4xx fails the repository and is logged", {
  issues <- "/repos/o/w/issues?state=closed&type=issues&limit=1"
  pulls <- "/repos/o/w/issues?state=closed&type=pulls&limit=50&page=1"
  latest <- "/repos/o/w/releases/latest"
  commits <- "/repos/o/w/commits?limit=1&stat=false&verification=false&files=false"
  cases <- list(list(commits, 403L), list(commits, 410L), list(commits, 404L), list(issues, 404L), list(issues, 403L),
                list(issues, 409L), list(pulls, 404L), list(pulls, 410L), list(latest, 403L), list(latest, 410L),
                list(latest, 409L))
  for (k in cases) {
    routes <- cb_gauge_routes("o/w")
    routes[[k[[1]]]] <- cb_answer(k[[2]])
    seen <- new.env()
    logs <- new.env()
    logs$lines <- character(0)
    st <- cb_test_state(log = function(m) logs$lines <- c(logs$lines, m))
    a <- cb_repo_extras(list(cb_get = cb_fake(routes, seen)), cb_obj(95L, "o/w"), st)
    info <- paste(k[[2]], k[[1]])
    expect_identical(a$state, "failed", info = info)
    expect_identical(utils::tail(seen$paths, 1L), k[[1]], info = info)
    expect_identical(logs$lines, sprintf("Codeberg %s: HTTP %d, the repository is deferred", k[[1]], k[[2]]), info = info)
  }
  for (k in list(list(latest, 404L), list(commits, 409L))) {
    routes <- cb_gauge_routes("o/w")
    routes[[k[[1]]]] <- cb_answer(k[[2]])
    a <- cb_repo_extras(list(cb_get = cb_fake(routes)), cb_obj(95L, "o/w"), cb_test_state())
    expect_identical(a$state, "ok", info = k[[1]])
  }
})

test_that("a migrated or mirrored repository has no created date, topics stop at 20, and a missing field is NA, never 0", {
  expect_true(is.na(cb_gauge_row(cb_obj(101L, "o/m", original_url = "https://github.com/o/m"))$created_at))
  expect_true(is.na(cb_gauge_row(cb_obj(102L, "o/n", mirror = TRUE))$created_at))
  many <- cb_gauge_row(cb_obj(103L, "o/t", topics = as.list(sprintf("t%02d", 1:25))))
  expect_identical(many$topics, paste(sprintf("t%02d", 1:20), collapse = ","))
  gone <- cb_obj(104L, "o/g"); gone$stars_count <- NULL; gone$release_counter <- NULL
  row <- cb_gauge_row(gone)
  expect_true(is.na(row$stars)); expect_true(is.na(row$releases_total))
  expect_identical(cb_na_counts(row)[["stars"]], 1L)
  # The columns Codeberg never fills are left out, so a jump in one that it fills shows.
  never <- c("owner_login", "owner_type", "owner_node_id", "license", "pushed_at")
  expect_identical(names(cb_na_counts(row)), setdiff(names(rows_df_empty_gauges()), never))
})

test_that("a created date is kept only when original_url and mirror are readable and say the repository was not moved", {
  expect_identical(cb_gauge_row(cb_fixture("repo_zoo.json"))$created_at, "2026-07-15T20:11:48Z")
  expect_identical(cb_gauge_row(cb_obj(120L, "o/k"))$created_at, "2024-01-23T21:06:52Z")
  no_url <- cb_obj(121L, "o/u"); no_url$original_url <- NULL
  no_mirror <- cb_obj(122L, "o/v"); no_mirror$mirror <- NULL
  odd <- list(no_url = no_url, no_mirror = no_mirror, null_url = cb_obj(123L, "o/w", original_url = NULL),
              mirror_text = cb_obj(124L, "o/x", mirror = "true"), url_number = cb_obj(125L, "o/y", original_url = 1L))
  for (k in names(odd)) expect_true(is.na(cb_gauge_row(odd[[k]])$created_at), info = k)
})

test_that("topics that are not a JSON array of strings read NA, never one topic", {
  expect_true(is.na(cb_gauge_row(cb_obj(126L, "o/s", topics = "r"))$topics))
  expect_true(is.na(cb_gauge_row(cb_obj(127L, "o/c", topics = c("r", "stats")))$topics))
  expect_true(is.na(cb_gauge_row(cb_obj(128L, "o/o", topics = list(a = "r")))$topics))
  expect_true(is.na(cb_gauge_row(cb_obj(129L, "o/n", topics = list("r", 1L)))$topics))
  expect_identical(cb_gauge_row(cb_obj(130L, "o/e", topics = list()))$topics, "")
  expect_identical(cb_gauge_row(cb_obj(131L, "o/r"))$topics, "r,stats")
})

test_that("cb_collect_gauges leaves a repository with a failed call or a 403 out of the snapshot, and keeps one whose newest release answered 404", {
  routes <- c(cb_gauge_routes("o/a", latest = cb_answer(500L)), cb_gauge_routes("o/b", latest = cb_answer(404L)),
              cb_gauge_routes("o/c", commit = cb_answer(403L)))
  logs <- new.env()
  logs$lines <- character(0)
  st <- cb_test_state(log = function(m) logs$lines <- c(logs$lines, m))
  g <- cb_collect_gauges(list(cb_get = cb_fake(routes)), cb_objects(cb_obj(71L, "o/a"), cb_obj(72L, "o/b"), cb_obj(73L, "o/c")),
                         st, today = as.Date("2026-10-20"))
  expect_identical(g$snapshot$node_id, "codeberg:codeberg.org:72")
  expect_identical(g$snapshot$issues_closed, 1L)
  expect_true(is.na(g$snapshot$last_release_tag)); expect_true(is.na(g$snapshot$last_release_at))
  expect_setequal(g$deferred, c("codeberg:codeberg.org:71", "codeberg:codeberg.org:73"))
  expect_identical(c(g$unreached, g$unresolvable), character(0))
  expect_true("Codeberg /repos/o/c/commits?limit=1&stat=false&verification=false&files=false: HTTP 403, the repository is deferred" %in%
              logs$lines)
})

test_that("repositories are visited in rotate_by_day order, and the deadline leaves the rest unreached", {
  objs <- do.call(cb_objects, lapply(81:84, function(i) cb_obj(i, sprintf("o/r%d", i))))
  clock <- cb_clock()
  fake <- cb_fake(do.call(c, lapply(81:84, function(i) cb_gauge_routes(sprintf("o/r%d", i)))))
  io <- list(cb_get = function(path) { clock$env$t <- clock$env$t + 10; fake(path) })
  day <- as.Date("2026-10-23")
  g <- cb_collect_gauges(io, objs, cb_test_state(clock), deadline = clock$now() + 75, now = clock$now, today = day)
  order <- rotate_by_day(sort(names(objs)), day)
  expect_identical(g$snapshot$node_id, order[1:2])
  expect_identical(g$unreached, order[3:4])
})

test_that("a 429 stops the pass with no further request, and three failures in a row defer Codeberg", {
  routes <- c(cb_gauge_routes("o/a"), cb_gauge_routes("o/b"))
  routes[["/repos/o/a/releases/latest"]] <- cb_answer(429L)
  seen <- new.env(); st <- cb_test_state()
  objs <- cb_objects(cb_obj(91L, "o/a"), cb_obj(92L, "o/b"))
  g <- cb_collect_gauges(list(cb_get = cb_fake(routes, seen)), objs, st, today = as.Date("2026-10-20"))
  expect_identical(utils::tail(seen$paths, 1L), "/repos/o/a/releases/latest")
  expect_true("codeberg:codeberg.org:91" %in% g$deferred)
  expect_identical(length(g$deferred) + NROW(g$snapshot), 2L)
  expect_identical(st$closed, "429")
  seen2 <- new.env(); seen2$paths <- character(0); st2 <- cb_test_state()
  objs4 <- do.call(cb_objects, lapply(94:97, function(i) cb_obj(i, sprintf("o/f%d", i))))
  g2 <- cb_collect_gauges(list(cb_get = function(path) { seen2$paths <- c(seen2$paths, path); cb_answer(502L) }),
                          objs4, st2, today = as.Date("2026-10-20"))
  expect_length(seen2$paths, 3L)
  expect_setequal(g2$deferred, names(objs4))
  expect_identical(st2$pacer$deferred(), CODEBERG_DOMAIN)
})

test_that("an r under 400 on the first answer stops the pass after that answer", {
  # Every path answers r = 350, so the first answer stops the pass whichever repository comes first.
  seen <- new.env(); seen$paths <- character(0); st <- cb_test_state()
  io <- list(cb_get = function(path) { seen$paths <- c(seen$paths, path); cb_list(list(), total = 1L, r = 350L) })
  g <- cb_collect_gauges(io, cb_objects(cb_obj(93L, "o/a"), cb_obj(193L, "o/b")), st, today = as.Date("2026-10-20"))
  expect_length(seen$paths, 1L)
  expect_match(seen$paths, "^/repos/o/[ab]/issues[?]state=closed&type=issues&limit=1$")
  expect_setequal(g$deferred, c("codeberg:codeberg.org:93", "codeberg:codeberg.org:193"))
  expect_identical(c(st$closed, as.character(st$low_r)), c("ratelimit", "350"))
})

test_that("no @ in the snapshot or the log", {
  lines <- new.env(); lines$v <- character(0)
  zoo <- cb_fixture("repo_zoo.json")
  zoo$owner$email <- "maintainer@example.org"
  st <- cb_test_state(log = function(m) lines$v <- c(lines$v, m))
  routes <- c(cb_gauge_routes("zeileis/zoo"), cb_gauge_routes("o/x", latest = cb_answer(500L, list(message = "x@example.org"))))
  g <- cb_collect_gauges(list(cb_get = cb_fake(routes)), cb_objects(zoo, cb_obj(5L, "o/x")), st, today = as.Date("2026-10-20"))
  expect_identical(g$snapshot$name_with_owner, "zeileis/zoo")
  expect_true(any(grepl("/repos/o/x/releases/latest: HTTP 500", lines$v, fixed = TRUE)))
  expect_false(any(grepl("@", c(unlist(lapply(g$snapshot, as.character)), lines$v), fixed = TRUE)))
})
