WS <- "{5F9215A6-5741-4727-B5B6-88E66F6D8E66}"
RP <- "{0b2c396c-7791-4295-b801-2d08faace27f}"
NODE <- "bitbucket:5f9215a6-5741-4727-b5b6-88e66f6d8e66:0b2c396c-7791-4295-b801-2d08faace27f"

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
