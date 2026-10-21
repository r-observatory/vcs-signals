test_that("a node id is built from two uuids and recognised by its shape", {
  expect_identical(bb_node_id(WS, RP), NODE)
  expect_true(is.na(bb_node_id("x", RP)))
  expect_true(is.na(bb_node_id(WS, NA_character_)))
  expect_identical(is_bitbucket_node(c(NODE, "codeberg:codeberg.org:12", NA, "bitbucket:a:b")),
                   c(TRUE, FALSE, FALSE, FALSE))
})

test_that("paths are built from ids or from owner and name", {
  expect_identical(bb_ids_path(NODE),
    "/repositories/%7B5f9215a6-5741-4727-b5b6-88e66f6d8e66%7D/%7B0b2c396c-7791-4295-b801-2d08faace27f%7D")
  expect_identical(bb_path("Grantlab", "bio3d"), "/repositories/grantlab/bio3d")
  expect_true(is.na(bb_path("a", "b/c")))
  expect_true(is.na(bb_path("", "b")))
  expect_true(is.na(bb_path("a b", "c")))
})

test_that(".bb_ask sorts answers into states", {
  st <- bb_test_state()
  io <- list(bb_get = bb_fake(list("/ok" = bb_answer(200L, list(a = 1L)), "/gone" = bb_answer(404L),
                                   "/moved" = bb_answer(301L), "/arr" = bb_answer(200L, list(1, 2)))))
  expect_identical(.bb_ask(io, st, "/ok")$state, "ok")
  expect_identical(.bb_ask(io, st, "/gone")$state, "absent")
  expect_identical(.bb_ask(io, st, "/moved")$state, "failed")
  expect_identical(.bb_ask(io, st, "/arr")$state, "failed")
  expect_identical(.bb_ask(io, st, "/missing")$state, "absent")
  expect_identical(st$asked, 5L)
})

test_that("three 5xx answers in a row stop the next ask with nothing sent", {
  seen <- new.env()
  st <- bb_test_state()
  io <- list(bb_get = bb_fake(list("/x" = bb_answer(500L)), seen))
  for (i in 1:3) expect_identical(.bb_ask(io, st, "/x")$state, "failed")
  expect_identical(.bb_ask(io, st, "/x")$state, "stopped")
  expect_length(seen$paths, 3L)
})

test_that("a 429 closes Bitbucket for the run and logs one warning", {
  seen <- new.env(); got <- new.env(); got$lines <- character()
  st <- bb_test_state(log = function(m) got$lines <- c(got$lines, m))
  io <- list(bb_get = bb_fake(list("/x" = bb_answer(429L)), seen))
  a <- .bb_ask(io, st, "/x")
  expect_identical(a$state, "stopped")
  expect_identical(a$status, 429L)
  expect_identical(st$closed, "429")
  expect_length(got$lines, 1L)
  expect_match(got$lines, "^::warning::Bitbucket: HTTP 429")
  expect_identical(.bb_ask(io, st, "/x")$state, "stopped")
  expect_length(seen$paths, 1L)
})

test_that("the run budget and the minimum interval are kept", {
  clock <- bb_clock()
  st <- bb_test_state(clock, run_budget = 2L)
  got <- new.env(); got$times <- numeric()
  io <- list(bb_get = function(path) { got$times <- c(got$times, clock$now()); bb_answer(200L, list(a = 1L)) })
  expect_identical(.bb_ask(io, st, "/a")$state, "ok")
  expect_identical(.bb_ask(io, st, "/b")$state, "ok")
  expect_false(st$refused)
  expect_identical(.bb_ask(io, st, "/c")$state, "stopped")
  expect_true(st$refused)
  expect_true(all(diff(got$times) >= BITBUCKET_MIN_INTERVAL_S))
})

test_that("bitbucket_active_repos returns only the active Bitbucket rows", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE repos (repo_id TEXT, node_id TEXT, host TEXT, owner TEXT, name TEXT, status TEXT)")
  DBI::dbExecute(con, "INSERT INTO repos VALUES
    ('bitbucket.org/grantlab/bio3d', NULL, 'bitbucket', 'grantlab', 'bio3d', 'active'),
    ('bitbucket.org/old/gone', NULL, 'bitbucket', 'old', 'gone', 'retired'),
    ('codeberg.org/a/b', NULL, 'codeberg', 'a', 'b', 'active')")
  r <- bitbucket_active_repos(con)
  expect_identical(r$repo_id, "bitbucket.org/grantlab/bio3d")
  expect_identical(names(r), c("repo_id", "node_id", "owner", "name"))
})

test_that("bb_gauge_row maps an object and its counts to one gauge row", {
  counts <- list(forks = 2L, prs_open = 1L, prs_merged = 16L, prs_closed = 3L, head_committed_at = "2026-06-15T02:37:55Z")
  row <- bb_gauge_row(bb_obj(), counts)
  expect_identical(names(row), names(rows_df_empty_gauges()))
  expect_identical(nrow(row), 1L)
  expect_identical(vapply(row, class, ""), vapply(rows_df_empty_gauges(), class, ""))
  expect_identical(row$node_id, NODE)
  expect_identical(row$name_with_owner, "bklamer/depower")
  expect_identical(row$size_kb, 4631L)
  expect_identical(row$is_fork, 0L)
  expect_identical(row$created_at, "2024-02-18T22:04:37Z")
  expect_identical(c(row$forks, row$prs_open, row$prs_merged, row$prs_closed), c(2L, 1L, 16L, 3L))
  expect_identical(row$head_committed_at, "2026-06-15T02:37:55Z")
  for (k in c("stars", "watchers", "issues_open", "issues_closed", "releases_total", "license", "topics",
              "is_archived", "is_mirror", "pushed_at", "last_release_at", "last_release_tag",
              "owner_login", "owner_type", "owner_node_id"))
    expect_true(is.na(row[[k]]), info = k)
  expect_identical(bb_gauge_row(bb_obj(parent = list(full_name = "x/y")), counts)$is_fork, 1L)
  o <- bb_obj(); o$parent <- NULL; o["parent"] <- NULL
  expect_true(is.na(bb_gauge_row(o, counts)$is_fork))
})

bb_repo <- function(node_id = NA_character_) data.frame(repo_id = "bitbucket.org/bklamer/depower", node_id = node_id,
  owner = "bklamer", name = "depower", stringsAsFactors = FALSE)
bb_obj_path <- bb_fields("/repositories/bklamer/depower")

test_that("bb_read_repo reads the object, three counts and the branch date", {
  seen <- new.env()
  routes <- c(setNames(list(bb_answer(200L, bb_obj())), bb_obj_path), bb_routes())
  r <- bb_read_repo(list(bb_get = bb_fake(routes, seen)), bb_repo(), bb_test_state())
  expect_identical(r$state, "ok")
  expect_identical(r$row$node_id, NODE)
  expect_identical(r$node_id, NODE)
  expect_false(r$moved)
  expect_identical(r$row$head_committed_at, "2026-06-15T02:37:55Z")
  expect_identical(seen$paths, c(bb_obj_path,
    "/repositories/bklamer/depower/forks?pagelen=1&fields=size",
    "/repositories/bklamer/depower/pullrequests?state=OPEN&pagelen=1&fields=size",
    "/repositories/bklamer/depower/pullrequests?state=MERGED&pagelen=1&fields=size",
    "/repositories/bklamer/depower/pullrequests?state=DECLINED&pagelen=1&fields=size",
    "/repositories/bklamer/depower/refs/branches/master?fields=target.date"))
  expect_true(all(grepl("fields=", seen$paths)))
  expect_false(any(grepl("commits", seen$paths)))
  expect_false(any(grepl("@", unlist(r$row), fixed = TRUE)))
})

test_that("bb_read_repo asks by ids first when a node id is stored", {
  seen <- new.env()
  routes <- c(setNames(list(bb_answer(200L, bb_obj())), bb_fields(bb_ids_path(NODE))), bb_routes())
  r <- bb_read_repo(list(bb_get = bb_fake(routes, seen)), bb_repo(NODE), bb_test_state())
  expect_identical(r$state, "ok")
  expect_identical(seen$paths[1], bb_fields(bb_ids_path(NODE)))
  expect_false(r$moved)
})

test_that("a repository moved to another workspace is read through its path and its id rewritten", {
  ws2 <- "{11111111-2222-3333-4444-555555555555}"
  seen <- new.env()
  routes <- c(setNames(list(bb_answer(200L, bb_obj(ws = ws2))), bb_obj_path), bb_routes())
  r <- bb_read_repo(list(bb_get = bb_fake(routes, seen)), bb_repo(NODE), bb_test_state())
  expect_identical(r$state, "ok")
  expect_true(r$moved)
  expect_identical(r$node_id, bb_node_id(ws2, RP))
  expect_identical(r$row$node_id, bb_node_id(ws2, RP))
  expect_identical(seen$paths[1:2], c(bb_fields(bb_ids_path(NODE)), bb_obj_path))
})

test_that("a path that answers another repository is not located", {
  other <- "{99999999-2222-3333-4444-555555555555}"
  seen <- new.env()
  routes <- setNames(list(bb_answer(200L, bb_obj(rp = other))), bb_obj_path)
  r <- bb_read_repo(list(bb_get = bb_fake(routes, seen)), bb_repo(NODE), bb_test_state())
  expect_identical(r$state, "not_located")
  expect_true(is.na(r$node_id))
  expect_null(r$row)
  expect_length(seen$paths, 2L)
})

test_that("a missing object is not located after one request without a node id", {
  seen <- new.env()
  r <- bb_read_repo(list(bb_get = bb_fake(list(), seen)), bb_repo(), bb_test_state())
  expect_identical(r$state, "not_located")
  expect_length(seen$paths, 1L)
  seen2 <- new.env()
  bad <- bb_repo(); bad$owner <- "a/b"
  expect_identical(bb_read_repo(list(bb_get = bb_fake(list(), seen2)), bad, bb_test_state())$state, "not_located")
  expect_length(seen2$paths, 0L)
})

test_that("an object without its identity, or with uuids that are not uuids, is deferred", {
  for (o in list(bb_obj(full_name = NULL), bb_obj(rp = "nope"), bb_obj(ws = NULL))) {
    r <- bb_read_repo(list(bb_get = bb_fake(setNames(list(bb_answer(200L, o)), bb_obj_path))), bb_repo(), bb_test_state())
    expect_identical(r$state, "deferred")
  }
})

test_that("a failed count defers the repository, logs once and asks nothing more", {
  seen <- new.env(); logged <- new.env(); logged$lines <- character(0)
  routes <- c(setNames(list(bb_answer(200L, bb_obj())), bb_obj_path), bb_routes(merged = bb_answer(500L)))
  st <- bb_test_state(log = function(m) logged$lines <- c(logged$lines, m))
  r <- bb_read_repo(list(bb_get = bb_fake(routes, seen)), bb_repo(), st)
  expect_identical(r$state, "deferred")
  expect_null(r$row)
  expect_false(any(grepl("refs/branches", seen$paths)))
  expect_length(seen$paths, 4L)
  expect_length(logged$lines, 1L)
  expect_match(logged$lines, "the repository is deferred")
  expect_match(logged$lines, "HTTP 500")
})

test_that("a count that is not an integer defers the repository", {
  for (bad in list(bb_answer(200L, list(size = "7")), bb_answer(200L, list()))) {
    routes <- c(setNames(list(bb_answer(200L, bb_obj())), bb_obj_path), bb_routes(merged = bad))
    expect_identical(bb_read_repo(list(bb_get = bb_fake(routes)), bb_repo(), bb_test_state())$state, "deferred")
  }
})

test_that("an empty repository or a deleted main branch is read with no head date", {
  seen <- new.env()
  routes <- c(setNames(list(bb_answer(200L, bb_obj(main = NULL))), bb_obj_path), bb_routes(main = NULL))
  r <- bb_read_repo(list(bb_get = bb_fake(routes, seen)), bb_repo(), bb_test_state())
  expect_identical(r$state, "ok")
  expect_true(is.na(r$row$head_committed_at))
  expect_length(seen$paths, 5L)
  routes <- c(setNames(list(bb_answer(200L, bb_obj())), bb_obj_path), bb_routes(main = NULL))
  r <- bb_read_repo(list(bb_get = bb_fake(routes)), bb_repo(), bb_test_state())
  expect_identical(r$state, "ok")
  expect_true(is.na(r$row$head_committed_at))
})

test_that("a branch call that fails defers, and a stopped run is stopped", {
  routes <- c(setNames(list(bb_answer(200L, bb_obj())), bb_obj_path), bb_routes(main = NULL),
              setNames(list(bb_answer(500L)), "/repositories/bklamer/depower/refs/branches/master?fields=target.date"))
  expect_identical(bb_read_repo(list(bb_get = bb_fake(routes)), bb_repo(), bb_test_state())$state, "deferred")
  st <- bb_test_state(run_budget = 0L)
  r <- bb_read_repo(list(bb_get = bb_fake(list())), bb_repo(), st)
  expect_identical(r$state, "stopped")
})

# ---- the daily slice ------------------------------------------------------------
bb_uuid <- function(i) sprintf("{00000000-0000-4000-8000-%012d}", i)
bb_nid <- function(i, ws = WS) vapply(i, function(k) bb_node_id(ws, bb_uuid(k)), "")
bb_many <- function(n, located = n) data.frame(
  repo_id = sprintf("bitbucket.org/o/r%02d", seq_len(n)),
  node_id = ifelse(seq_len(n) <= located, bb_nid(seq_len(n)), NA_character_),
  owner = "o", name = sprintf("r%02d", seq_len(n)), stringsAsFactors = FALSE)
# Routes for repositories r01.. : each object by path and its five answers.
bb_many_routes <- function(n, ...) {
  out <- list()
  for (i in seq_len(n)) {
    nm <- sprintf("r%02d", i)
    out[[bb_fields(paste0("/repositories/o/", nm))]] <- bb_answer(200L, bb_obj(rp = bb_uuid(i), full_name = paste0("o/", nm)))
    out <- c(out, bb_routes(paste0("/repositories/o/", nm)))
  }
  modifyList(out, list(...))
}

test_that("bb_slice reads the unlocated first, then a quarter of the located by day", {
  repos <- bb_many(32, located = 29)
  days <- as.Date("2026-10-20") + 0:3
  slices <- lapply(days, function(d) bb_slice(repos, d, budget = 50L))
  for (s in slices) expect_identical(s[1:3], repos$repo_id[30:32])
  located <- unlist(lapply(slices, function(s) s[-(1:3)]))
  expect_identical(sort(located), repos$repo_id[1:29])
  expect_length(located, 29L)
  expect_identical(bb_slice(repos[0, ], days[1]), character(0))
  # A budget below one repository still reads one a day.
  expect_length(bb_slice(bb_many(3), days[1], budget = 2L), 1L)
})

test_that("a 429 on the first request reads nothing, defers nothing and warns once", {
  e <- new.env(); e$logged <- character(0)
  st <- bb_test_state(log = function(m) e$logged <- c(e$logged, m))
  r <- bb_collect(list(bb_get = function(path) bb_answer(429L)), bb_many(3), st, now = function() 0,
                  today = as.Date("2026-10-21"))
  expect_identical(r$read, 0L)
  expect_null(r$snapshot)
  expect_length(r$deferred, 0L)
  expect_identical(grep("::warning::", e$logged, value = TRUE), "::warning::Bitbucket: HTTP 429, no more requests this run")
  expect_identical(st$asked, 1L)
})

test_that("a repository that cannot be finished within the budget is not started", {
  st <- bb_test_state(run_budget = 14L)
  seen <- new.env()
  r <- bb_collect(list(bb_get = bb_fake(bb_many_routes(3), seen)), bb_many(3, located = 0), st, now = function() 0,
                  today = as.Date("2026-10-21"), budget = 14L)
  expect_identical(r$read, 2L)
  expect_identical(r$later, 1L)
  expect_identical(st$asked, 12L)
  expect_identical(nrow(r$snapshot), 2L)
  expect_identical(r$ids$repo_id, c("bitbucket.org/o/r01", "bitbucket.org/o/r02"))
  expect_identical(r$ids$owner, c("o", "o"))
  expect_identical(r$ids$name_with_owner, c("o/r01", "o/r02"))
  expect_identical(r$ids$status, c("active", "active"))
  expect_identical(r$ids$node_id, bb_nid(1:2))
})

test_that("a passed deadline asks nothing and leaves the located slice unreached", {
  repos <- bb_many(4, located = 2)
  st <- bb_test_state()
  r <- bb_collect(list(bb_get = function(path) stop("asked")), repos, st, deadline = 5, now = function() 10,
                  today = as.Date("2026-10-21"))
  expect_identical(st$asked, 0L)
  expect_identical(r$read, 0L)
  expect_identical(sort(r$unreached), sort(repos$node_id[1:2]))
  expect_length(r$deferred, 0L)
})

test_that("a repository whose count fails is deferred and the next is read", {
  repos <- bb_many(2)
  routes <- bb_many_routes(2, "/repositories/o/r01/pullrequests?state=MERGED&pagelen=1&fields=size" = bb_answer(500L))
  r <- bb_collect(list(bb_get = bb_fake(routes)), repos, bb_test_state(), now = function() 0,
                  today = as.Date("2026-10-21"))
  expect_identical(r$deferred, repos$node_id[1])
  expect_identical(r$read, 1L)
  expect_identical(r$snapshot$node_id, repos$node_id[2])
  expect_length(r$ids$repo_id, 0L)
})

test_that("a repository not located is counted, a rewritten node id reaches ids", {
  ws2 <- "{11111111-2222-3333-4444-555555555555}"
  repos <- bb_many(2)
  routes <- bb_many_routes(2)
  routes[[bb_fields(bb_ids_path(repos$node_id[1]))]] <- bb_answer(200L, bb_obj(ws = ws2, rp = bb_uuid(1), full_name = "o/r01"))
  routes[[bb_fields("/repositories/o/r02")]] <- bb_answer(404L)
  routes[[bb_fields(bb_ids_path(repos$node_id[2]))]] <- bb_answer(404L)
  r <- bb_collect(list(bb_get = bb_fake(routes)), repos, bb_test_state(), now = function() 0,
                  today = as.Date("2026-10-21"))
  expect_identical(r$read, 1L)
  expect_identical(r$not_located, repos$repo_id[2])
  expect_identical(r$ids$repo_id, repos$repo_id[1])
  expect_identical(r$ids$node_id, bb_node_id(ws2, bb_uuid(1)))
  expect_length(r$deferred, 0L)
})

test_that("two rows with one node id are read once and give one snapshot row", {
  repos <- bb_many(2)
  repos$node_id[2] <- repos$node_id[1]
  seen <- new.env()
  routes <- bb_many_routes(2)
  routes[[bb_fields(bb_ids_path(repos$node_id[1]))]] <- routes[[bb_fields("/repositories/o/r01")]]
  r <- bb_collect(list(bb_get = bb_fake(routes, seen)), repos, bb_test_state(), now = function() 0,
                  today = as.Date("2026-10-21"))
  expect_length(seen$paths, 6L)
  expect_identical(nrow(r$snapshot), 1L)
  expect_identical(r$read, 1L)
})

test_that("two unlocated rows answering one uuid pair give one snapshot row and both ids", {
  repos <- bb_many(2, located = 0)
  routes <- bb_many_routes(2)
  routes[[bb_fields("/repositories/o/r02")]] <- bb_answer(200L, bb_obj(rp = bb_uuid(1), full_name = "o/r01"))
  r <- bb_collect(list(bb_get = bb_fake(routes)), repos, bb_test_state(), now = function() 0,
                  today = as.Date("2026-10-21"))
  expect_identical(nrow(r$snapshot), 1L)
  expect_identical(r$ids$repo_id, repos$repo_id)
  expect_identical(r$ids$node_id, rep(bb_nid(1), 2L))
})
