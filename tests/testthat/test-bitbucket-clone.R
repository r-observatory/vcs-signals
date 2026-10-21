# The Bitbucket weekly clone pass (scripts/bitbucket_clone.R), over the fixture repository in
# helper-codeberg-git.R, and its roster and CLI mode (weekly.R, sourced from the repo root).
if (!exists("run_weekly_enumerate")) {
  .bbc_wd <- setwd(.repo_root)
  source(file.path(.repo_root, "scripts", "weekly.R"))
  setwd(.bbc_wd)
}
NODE2 <- "bitbucket:5f9215a6-5741-4727-b5b6-88e66f6d8e66:11111111-2222-4333-8444-555555555555"
IDS_Q <- function(node) paste0(bb_ids_path(node), "?fields=full_name,uuid")

bbc_roster <- function(dir, slugs, ids) {
  path <- file.path(dir, BITBUCKET_ROSTER_DB)
  write_bitbucket_roster(path, data.frame(repo_id = paste0("bitbucket.org/", tolower(slugs)), node_id = ids,
    owner = sub("/.*$", "", slugs), name = sub("^.*/", "", slugs), stringsAsFactors = FALSE))
  path
}
bbc_snapshot <- function(out) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, BITBUCKET_SHARD_DB)); on.exit(DBI::dbDisconnect(con))
  DBI::dbReadTable(con, "snapshot")
}
bbc_logger <- function() {
  e <- new.env(); e$v <- character(0)
  list(log = function(m) e$v <- c(e$v, m), lines = function() e$v)
}
# A clone member that records each url and answers by a function of it.
bbc_clone <- function(seen, answer) function(url, dir) {
  seen$urls <- c(seen$urls, url)
  answer(url, dir)
}
bbc_gone <- function(url) list(status = 128L, output = sprintf("fatal: repository '%s/' not found", url))
bbc_run <- function(io, roster, lg = bbc_logger(), ...) {
  dir <- dirname(roster)
  clock <- bb_clock()
  run_bitbucket <- run_fetch_bitbucket(io, file.path(dir, "out"), roster, now = clock$now, sleep = clock$sleep,
                                       scratch = file.path(dir, "scratch"), log = lg$log, ...)
  run_bitbucket
}

test_that("a clone of the fixture repository gives its commit total and identity counts and leaves no directory", {
  dir <- withr::local_tempdir()
  roster <- bbc_roster(dir, "Own/Pkg", NODE)
  seen <- new.env(); seen$urls <- character()
  io <- list(bb_clone = bbc_clone(seen, function(url, d) cb_local_clone(cb_fixture_repo())(url, d)))
  lg <- bbc_logger()
  bbc_run(io, roster, lg)
  s <- bbc_snapshot(file.path(dir, "out"))
  expect_identical(s$repo_id, "bitbucket.org/own/pkg")
  expect_identical(c(s$commits_total, s$contributors_total, s$top_contributor_commits), c(6L, 4L, 3L))
  expect_true(is.na(s$top_contributor_bot))
  expect_identical(seen$urls, "https://bitbucket.org/own/pkg.git")
  expect_false(dir.exists(file.path(dir, "scratch")))
  expect_false(any(grepl("@", lg$lines(), fixed = TRUE)))
})

test_that("a not-found clone then an ids answer with the same uuid and a working second clone is read, with one fields= request", {
  dir <- withr::local_tempdir()
  roster <- bbc_roster(dir, "old/name", NODE)
  seen <- new.env(); seen$urls <- character(); api <- new.env()
  ok <- cb_local_clone(cb_fixture_repo())
  io <- list(
    bb_clone = bbc_clone(seen, function(url, d) if (grepl("/old/name", url)) bbc_gone(url) else ok(url, d)),
    bb_get = bb_fake(setNames(list(bb_answer(200L, list(full_name = "New/Name", uuid = RP))), IDS_Q(NODE)), api))
  lg <- bbc_logger()
  bbc_run(io, roster, lg)
  expect_identical(seen$urls, c("https://bitbucket.org/old/name.git", "https://bitbucket.org/new/name.git"))
  expect_identical(api$paths, IDS_Q(NODE))
  expect_identical(bbc_snapshot(file.path(dir, "out"))$commits_total, 6L)
  expect_match(lg$lines()[length(lg$lines())], "1 read, 0 failed, 0 empty, 0 not located, 0 not reached of 1 repos, 1 API requests", fixed = TRUE)
})

test_that("an ids answer with another uuid is not located, with no second clone", {
  dir <- withr::local_tempdir()
  roster <- bbc_roster(dir, "old/name", NODE)
  seen <- new.env(); seen$urls <- character()
  io <- list(
    bb_clone = bbc_clone(seen, function(url, d) bbc_gone(url)),
    bb_get = bb_fake(setNames(list(bb_answer(200L, list(full_name = "new/name", uuid = "{11111111-2222-4333-8444-555555555555}"))), IDS_Q(NODE))))
  lg <- bbc_logger()
  bbc_run(io, roster, lg)
  expect_length(seen$urls, 1L)
  expect_true(is.na(bbc_snapshot(file.path(dir, "out"))$commits_total))
  expect_match(lg$lines()[length(lg$lines())], "0 read, 0 failed, 0 empty, 1 not located, 0 not reached of 1 repos", fixed = TRUE)
})

test_that("a repository without a Bitbucket node id is not located when git says not found, and nothing is asked", {
  dir <- withr::local_tempdir()
  roster <- bbc_roster(dir, "old/name", "unlocated")
  api <- new.env(); api$paths <- character()
  io <- list(bb_clone = function(url, d) bbc_gone(url), bb_get = bb_fake(list(), api))
  lg <- bbc_logger()
  bbc_run(io, roster, lg)
  expect_length(api$paths, 0L)
  expect_match(lg$lines()[length(lg$lines())], "1 not located", fixed = TRUE)
})

test_that("a clone the server did not filter closes Bitbucket with one warning and no further clone", {
  dir <- withr::local_tempdir()
  roster <- bbc_roster(dir, c("o/a", "o/b", "o/c"), c(NODE, NODE2, "unlocated"))
  seen <- new.env(); seen$urls <- character()
  ok <- cb_local_clone(cb_fixture_repo())
  io <- list(bb_clone = bbc_clone(seen, function(url, d) {
    r <- ok(url, d); r$output <- c(r$output, "warning: filtering not recognized by server, ignoring"); r }))
  lg <- bbc_logger()
  bbc_run(io, roster, lg)
  expect_identical(seen$urls, "https://bitbucket.org/o/a.git")
  expect_identical(bbc_snapshot(file.path(dir, "out"))$commits_total, c(6L, NA, NA))
  expect_length(grep("^::warning::", lg$lines()), 1L)
  expect_match(lg$lines()[length(lg$lines())], "1 read, 0 failed, 0 empty, 0 not located, 2 not reached of 3 repos", fixed = TRUE)
})

test_that("git answering 429 closes Bitbucket with one warning", {
  dir <- withr::local_tempdir()
  roster <- bbc_roster(dir, c("o/a", "o/b"), c(NODE, NODE2))
  seen <- new.env(); seen$urls <- character()
  io <- list(bb_clone = bbc_clone(seen, function(url, d) list(status = 128L,
    output = "fatal: unable to access 'https://bitbucket.org/o/a.git/': The requested URL returned error: 429")))
  lg <- bbc_logger()
  bbc_run(io, roster, lg)
  expect_length(seen$urls, 1L)
  expect_length(grep("^::warning::Bitbucket [^:]*: git clone", lg$lines()), 1L)
  expect_match(lg$lines()[length(lg$lines())], "0 read, 0 failed, 0 empty, 0 not located, 2 not reached of 2 repos", fixed = TRUE)
})

test_that("three clone failures in a row close Bitbucket with a warning, and an answer about a repository does not count", {
  dir <- withr::local_tempdir()
  roster <- bbc_roster(dir, paste0("o/", letters[1:5]), rep("unlocated", 5))
  seen <- new.env(); seen$urls <- character()
  io <- list(bb_clone = bbc_clone(seen, function(url, d) list(status = 128L, output = "fatal: early EOF")))
  lg <- bbc_logger()
  bbc_run(io, roster, lg)
  expect_length(seen$urls, 3L)
  w <- grep("^::warning::Bitbucket: closed", lg$lines(), value = TRUE)
  expect_length(w, 1L)
  expect_match(w, "3 failures in a row", fixed = TRUE)
  expect_match(lg$lines()[length(lg$lines())], "0 read, 3 failed, 0 empty, 0 not located, 2 not reached of 5 repos", fixed = TRUE)
  dir2 <- withr::local_tempdir()
  seen2 <- new.env(); seen2$urls <- character()
  io2 <- list(bb_clone = bbc_clone(seen2, function(url, d) bbc_gone(url)))
  bbc_run(io2, bbc_roster(dir2, paste0("o/", letters[1:5]), rep("unlocated", 5)), bbc_logger())
  expect_length(seen2$urls, 5L)
})

test_that("the API budget caps only the fallback asks", {
  dir <- withr::local_tempdir()
  roster <- bbc_roster(dir, c("o/a", "o/b"), c(NODE, NODE2))
  api <- new.env()
  io <- list(bb_clone = function(url, d) bbc_gone(url),
             bb_get = bb_fake(list(), api))
  lg <- bbc_logger()
  bbc_run(io, roster, lg, run_budget = 1L)
  expect_length(api$paths, 1L)
  expect_match(lg$lines()[length(lg$lines())], "1 not located, 1 not reached of 2 repos, 1 API requests", fixed = TRUE)
})

test_that("the shard holds exactly the roster's repo_ids, git's version is logged once, and the log has no address", {
  dir <- withr::local_tempdir()
  roster <- bbc_roster(dir, c("o/good", "o/bad"), c(NODE, NODE2))
  ok <- cb_local_clone(cb_fixture_repo())
  io <- list(bb_clone = function(url, d) if (grepl("bad", url)) list(status = 128L, output = "fatal: by Ann <ann@example.invalid>") else ok(url, d))
  lg <- bbc_logger()
  p <- bbc_run(io, roster, lg)
  expect_identical(p, file.path(dir, "out", BITBUCKET_SHARD_DB))
  s <- bbc_snapshot(file.path(dir, "out"))
  expect_identical(s$repo_id, c("bitbucket.org/o/bad", "bitbucket.org/o/good"))
  expect_identical(s$commits_total, c(NA, 6L))
  expect_true(all(is.na(s$top_contributor_bot)))
  l <- lg$lines()
  expect_identical(l[1], "weekly fetch bitbucket: 2 repos")
  expect_identical(sum(grepl("^weekly fetch bitbucket: git version [0-9]", l)), 1L)
  expect_match(l[length(l)], "^weekly fetch bitbucket: 1 read, 1 failed, 0 empty, 0 not located, 0 not reached of 2 repos, 0 API requests, [0-9]+ s$")
  expect_false(any(grepl("@", l, fixed = TRUE)))
})

test_that("a missing roster is a warning and an empty shard, and a deadline in the past starts no clone", {
  dir <- withr::local_tempdir()
  lg <- bbc_logger()
  bbc_run(list(), file.path(dir, BITBUCKET_ROSTER_DB), lg)
  expect_identical(nrow(bbc_snapshot(file.path(dir, "out"))), 0L)
  expect_true(any(startsWith(lg$lines(), "::warning::Bitbucket: the roster could not be read")))
  dir2 <- withr::local_tempdir()
  seen <- new.env(); seen$urls <- character()
  roster <- bbc_roster(dir2, "o/a", NODE)
  bbc_run(list(bb_clone = bbc_clone(seen, function(url, d) bbc_gone(url))), roster, bbc_logger(), deadline_min = -1)
  expect_length(seen$urls, 0L)
})

test_that("the real clone member sends the filter, the User-Agent and no redirect, and refuses a URL off bitbucket.org", {
  got <- new.env(); got$n <- 0L
  run <- function(cmd, args, stdout, stderr, env, timeout) { got$n <- got$n + 1L; got$args <- args; got$env <- env; character() }
  bb_git_clone("https://bitbucket.org/o/r.git", "/nonexistent/x", run = run)
  expect_true(all(c("clone", "--bare", paste0("--filter=", BITBUCKET_CLONE_FILTER), "--single-branch", "--no-tags",
                    "credential.helper=", "http.followRedirects=false") %in% got$args))
  expect_true(shQuote(paste0("http.userAgent=", CODEBERG_USER_AGENT)) %in% got$args)
  expect_true("GIT_TERMINAL_PROMPT=0" %in% got$env)
  r <- bb_git_clone("https://elsewhere.example/o/r.git", "/nonexistent/x", run = run)
  expect_identical(got$n, 1L)
  expect_true(r$status != 0L)
})

test_that("a renamed repository whose second clone is also not found is not located, and one that fails is failed", {
  for (case in c("gone", "fail")) {
    dir <- withr::local_tempdir()
    roster <- bbc_roster(dir, "old/name", NODE)
    seen <- new.env(); seen$urls <- character()
    io <- list(
      bb_clone = bbc_clone(seen, function(url, d) if (case == "gone" || grepl("/old/name", url)) bbc_gone(url)
                                                  else list(status = 128L, output = "fatal: early EOF")),
      bb_get = bb_fake(setNames(list(bb_answer(200L, list(full_name = "new/name", uuid = RP))), IDS_Q(NODE))))
    lg <- bbc_logger()
    bbc_run(io, roster, lg)
    expect_length(seen$urls, 2L)
    expect_true(is.na(bbc_snapshot(file.path(dir, "out"))$commits_total))
    expect_match(lg$lines()[length(lg$lines())], if (case == "gone") "0 read, 0 failed, 0 empty, 1 not located, 0 not reached of 1 repos"
                                                 else "0 read, 1 failed, 0 empty, 0 not located, 0 not reached of 1 repos", fixed = TRUE)
  }
  st <- bb_test_state()
  io <- list(bb_clone = function(url, d) bbc_gone(url),
             bb_get = bb_fake(setNames(list(bb_answer(200L, list(full_name = "new/name", uuid = RP))), IDS_Q(NODE))))
  got <- bb_clone_counts(io, data.frame(owner = "old", name = "name", node_id = NODE), st, withr::local_tempdir())
  expect_identical(got$state, "not_located")
})

# ---- the weekly wiring -------------------------------------------------------------
bbc_summary <- function(path, rows) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
  ensure_series_schema(con); ensure_repo_schema(con)
  DBI::dbExecute(con, "INSERT INTO vcs_signals_summary (package, origin, repo_id, stars, forks, releases_total) VALUES
    ('data.table', 'cran', 'github.com/rdatatable/data.table', 3400, 960, 9)")
  DBI::dbExecute(con, paste("INSERT INTO repos VALUES", paste(rows, collapse = ",\n")))
  path
}
bbc_repo <- function(id, node, host, status = "active") sprintf(
  "('%s', %s, '%s', '%s', '%s', '%s', '%s', 1, 1, '2026-01-01', '2026-10-01', '%s')",
  id, if (is.na(node)) "NULL" else sprintf("'%s'", node), host, sub("/.*$", "", id),
  sub("^[^/]*/([^/]*)/.*$", "\\1", id), sub("^.*/", "", id), sub("^[^/]*/", "", id), status)

test_that("enumerate writes a Bitbucket roster of the active Bitbucket repositories with a Bitbucket id", {
  src <- bbc_summary(withr::local_tempfile(fileext = ".db"), c(
    bbc_repo("github.com/rdatatable/data.table", "R_dt", "github"),
    bbc_repo("bitbucket.org/someone/located", NODE, "bitbucket"),
    bbc_repo("bitbucket.org/someone/new", NA, "bitbucket"),
    bbc_repo("bitbucket.org/someone/gone", NODE2, "bitbucket", "retired"),
    bbc_repo("bitbucket.org/someone/odd", "codeberg:codeberg.org:5", "bitbucket")))
  out <- withr::local_tempdir()
  suppressMessages(run_weekly_enumerate(list(download = function(name, dir) file.copy(src, file.path(dir, name), overwrite = TRUE)), out))
  bb <- load_roster(file.path(out, BITBUCKET_ROSTER_DB))
  expect_identical(names(bb), c("repo_id", "node_id", "owner", "name"))
  expect_identical(bb$repo_id, "bitbucket.org/someone/located")
  expect_identical(c(bb$node_id, bb$owner, bb$name), c(NODE, "someone", "located"))
  expect_identical(load_roster(file.path(out, "vcs-signals-roster.db"))$repo_id, "github.com/rdatatable/data.table")
})

test_that("main runs fetch-bitbucket on the Bitbucket roster in VCS_ROSTER, and the usage line names it", {
  dir <- withr::local_tempdir()
  bbc_roster(dir, "o/good", NODE)
  withr::local_envvar(VCS_ROSTER = dir)
  out <- file.path(dir, "out")
  weekly <- new.env(parent = globalenv())
  withr::with_dir(.repo_root, sys.source(file.path("scripts", "weekly.R"), envir = weekly))
  io <- list(bb_clone = function(url, d) cb_local_clone(cb_fixture_repo())(url, d))
  expect_false(withVisible(suppressMessages(weekly$main("fetch-bitbucket", out, io = io)))$visible)
  expect_identical(bbc_snapshot(out)$repo_id, "bitbucket.org/o/good")
  expect_error(weekly$main("no-such-mode", out, io = list()), "fetch-bitbucket", fixed = TRUE)
})

test_that("a pass that reads none of its repositories warns once", {
  dir <- withr::local_tempdir()
  roster <- bbc_roster(dir, c("o/a", "o/b"), c("unlocated", "unlocated"))
  io <- list(bb_clone = function(url, d) list(status = 128L, output = "fatal: could not read Username for 'https://bitbucket.org'"))
  lg <- bbc_logger()
  bbc_run(io, roster, lg)
  expect_identical(grep("^::warning::", lg$lines(), value = TRUE),
                   "::warning::Bitbucket: 0 of 2 repositories read this run, every one keeps last week's values")
  dir2 <- withr::local_tempdir()
  lg2 <- bbc_logger()
  bbc_run(list(bb_clone = cb_local_clone(cb_fixture_repo())), bbc_roster(dir2, "o/a", "unlocated"), lg2)
  expect_length(grep("^::warning::", lg2$lines()), 0L)
})

test_that("an API 429 on a fallback ask closes the API only, and cloning goes on", {
  dir <- withr::local_tempdir()
  roster <- bbc_roster(dir, c("o/a-gone", "o/b", "o/c"), c(NODE, "unlocated", "unlocated"))
  ok <- cb_local_clone(cb_fixture_repo())
  io <- list(bb_clone = function(url, d) if (grepl("a-gone", url)) bbc_gone(url) else ok(url, d),
             bb_get = function(path) bb_answer(429L))
  lg <- bbc_logger()
  bbc_run(io, roster, lg)
  expect_identical(bbc_snapshot(file.path(dir, "out"))$commits_total, c(NA, 6L, 6L))
  expect_match(lg$lines()[length(lg$lines())], "2 read, 0 failed, 0 empty, 0 not located, 1 not reached of 3 repos, 1 API requests", fixed = TRUE)
})

test_that("an empty repository is empty with NA values, and a clone that is no repository is a failure", {
  dir <- withr::local_tempdir()
  bare <- file.path(withr::local_tempdir(), "empty.git")
  .cb_fixture_git(dirname(bare), c("init", "--bare", "--quiet", shQuote(bare)))
  roster <- bbc_roster(dir, "o/empty", "unlocated")
  lg <- bbc_logger()
  bbc_run(list(bb_clone = cb_local_clone(bare)), roster, lg)
  s <- bbc_snapshot(file.path(dir, "out"))
  expect_true(is.na(s$commits_total))
  expect_match(lg$lines()[length(lg$lines())], "0 read, 0 failed, 1 empty, 0 not located, 0 not reached of 1 repos", fixed = TRUE)
  expect_false(any(grepl("clone failed|failed \\(exit", lg$lines())))
  dir2 <- withr::local_tempdir()
  lg2 <- bbc_logger()
  bbc_run(list(bb_clone = function(url, d) { dir.create(d); list(status = 0L, output = character()) }),
          bbc_roster(dir2, "o/hollow", "unlocated"), lg2)
  expect_match(lg2$lines()[length(lg2$lines())], "0 read, 1 failed, 0 empty", fixed = TRUE)
})

test_that("an ids lookup answering 500 leaves the repository failed and logs the status", {
  dir <- withr::local_tempdir()
  roster <- bbc_roster(dir, "old/name", NODE)
  io <- list(bb_clone = function(url, d) bbc_gone(url),
             bb_get = bb_fake(setNames(list(bb_answer(500L)), IDS_Q(NODE))))
  lg <- bbc_logger()
  bbc_run(io, roster, lg)
  expect_true(any(grepl("the id lookup got HTTP 500", lg$lines(), fixed = TRUE)))
  expect_match(lg$lines()[length(lg$lines())], "0 read, 1 failed, 0 empty, 0 not located, 0 not reached of 1 repos", fixed = TRUE)
})

test_that("a deadline that passes partway leaves the rest not reached, and the shard holds every roster repository", {
  dir <- withr::local_tempdir()
  roster <- bbc_roster(dir, c("o/a", "o/b", "o/c"), rep("unlocated", 3))
  clock <- bb_clock(); ok <- cb_local_clone(cb_fixture_repo())
  io <- list(bb_clone = function(url, d) { clock$env$t <- clock$env$t + 600; ok(url, d) })
  lg <- bbc_logger()
  run_fetch_bitbucket(io, file.path(dir, "out"), roster, now = clock$now, sleep = clock$sleep, deadline_min = 5, log = lg$log)
  s <- bbc_snapshot(file.path(dir, "out"))
  expect_identical(s$repo_id, c("bitbucket.org/o/a", "bitbucket.org/o/b", "bitbucket.org/o/c"))
  expect_identical(s$commits_total, c(6L, NA, NA))
  expect_match(lg$lines()[length(lg$lines())], "1 read, 0 failed, 0 empty, 0 not located, 2 not reached of 3 repos", fixed = TRUE)
})
