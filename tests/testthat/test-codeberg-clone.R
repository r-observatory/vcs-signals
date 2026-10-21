# The Codeberg clone readers and the weekly job (scripts/codeberg_clone.R), over the fixture repository in
# helper-codeberg-git.R, and the Codeberg roster (weekly.R). weekly.R is sourced from the repo root.
if (!exists("run_weekly_enumerate")) {
  .cbw_wd <- setwd(.repo_root)
  source(file.path(.repo_root, "scripts", "weekly.R"))
  setwd(.cbw_wd)
}

test_that("identity counts are distinct name and address pairs and the top count, and nothing else is returned", {
  lines <- c("Ann\x1fann@example.invalid", "Ann\x1fann@example.invalid", "Ann\x1fann.b@example.invalid", "Bo\x1fbo@example.invalid")
  expect_identical(cb_identity_counts(lines), list(contributors_total = 3L, top_contributor_commits = 2L))
  expect_identical(cb_identity_counts(character()), list(contributors_total = NA_integer_, top_contributor_commits = NA_integer_))
})

test_that("a clone of the fixture repository counts every commit on the default branch, merges included", {
  got <- cb_clone_counts(list(cb_clone = cb_local_clone(cb_fixture_repo())), cb_obj(61L, "someone/cbpkg"),
                         cb_test_state(), withr::local_tempdir())
  expect_identical(got$state, "ok")
  expect_identical(c(got$commits_total, got$contributors_total, got$top_contributor_commits), c(6L, 4L, 3L))
})

test_that("the clone directory is gone after a success and after an error", {
  scratch <- withr::local_tempdir()
  boom <- list(cb_clone = function(url, dir) { dir.create(dir); writeLines("x", file.path(dir, "f")); stop("network broke") })
  expect_identical(cb_clone_counts(boom, cb_obj(62L, "o/r"), cb_test_state(), scratch)$state, "failed")
  expect_length(list.files(scratch, all.files = TRUE, no.. = TRUE), 0L)
  ok <- cb_clone_counts(list(cb_clone = cb_local_clone(cb_fixture_repo())), cb_obj(63L, "o/s"), cb_test_state(), scratch)
  expect_identical(ok$state, "ok")
  expect_length(list.files(scratch, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("git answering 429 closes Codeberg for the run, and a missing repository is an answer, not a failure", {
  st <- cb_test_state()
  r429 <- list(cb_clone = function(url, dir) list(status = 128L,
    output = "fatal: unable to access 'https://codeberg.org/o/r.git/': The requested URL returned error: 429"))
  expect_identical(cb_clone_counts(r429, cb_obj(64L, "o/r"), st, withr::local_tempdir())$state, "stopped")
  expect_identical(st$closed, "429")
  expect_identical(st$pacer$hourly_budget(CODEBERG_DOMAIN), 0)
  called <- new.env(); called$n <- 0L
  counting <- list(cb_clone = function(url, dir) { called$n <- called$n + 1L; list(status = 0L, output = character()) })
  expect_identical(cb_clone_counts(counting, cb_obj(65L, "o/q"), st, withr::local_tempdir())$state, "stopped")
  expect_identical(called$n, 0L)
  st2 <- cb_test_state()
  gone <- list(cb_clone = function(url, dir) list(status = 128L, output = "fatal: repository 'https://codeberg.org/o/r.git/' not found"))
  for (i in 1:4) expect_identical(cb_clone_counts(gone, cb_obj(66L, "o/r"), st2, withr::local_tempdir())$state, "failed")
  expect_length(st2$pacer$deferred(), 0L)
})

test_that("git answering 403 or 410 is an answer about the repository, while three other failures in a row stop the clones", {
  st <- cb_test_state()
  for (code in c(403L, 410L, 403L, 410L)) {
    refused <- list(cb_clone = function(url, dir) list(status = 128L, output = sprintf(
      "fatal: unable to access 'https://codeberg.org/o/r.git/': The requested URL returned error: %d", code)))
    expect_identical(cb_clone_counts(refused, cb_obj(71L, "o/r"), st, withr::local_tempdir())$state, "failed")
  }
  expect_length(st$pacer$deferred(), 0L)
  st2 <- cb_test_state()
  eof <- list(cb_clone = function(url, dir) list(status = 128L, output = "fatal: early EOF"))
  for (i in 1:3) expect_identical(cb_clone_counts(eof, cb_obj(72L, "o/r"), st2, withr::local_tempdir())$state, "failed")
  expect_identical(st2$pacer$deferred(), CODEBERG_DOMAIN)
  expect_identical(cb_clone_counts(eof, cb_obj(72L, "o/r"), st2, withr::local_tempdir())$state, "stopped")
})

test_that("only git's wording for a missing repository is an answer, never another message that says not found", {
  thrown <- list(cb_clone = function(url, dir) stop("object 'x' not found"))
  backend <- list(cb_clone = function(url, dir) list(status = 128L, output = "remote: storage backend not found"))
  for (io in list(thrown, backend)) {
    st <- cb_test_state()
    for (i in 1:3) expect_identical(cb_clone_counts(io, cb_obj(74L, "o/r"), st, withr::local_tempdir())$state, "failed")
    expect_identical(st$pacer$deferred(), CODEBERG_DOMAIN)
  }
  st <- cb_test_state()
  gone <- list(cb_clone = function(url, dir) list(status = 128L,
    output = c("remote: Not found.", "fatal: repository 'https://codeberg.org/o/r.git/' not found")))
  for (i in 1:3) expect_identical(cb_clone_counts(gone, cb_obj(74L, "o/r"), st, withr::local_tempdir())$state, "failed")
  expect_length(st$pacer$deferred(), 0L)
})

test_that("st$asked grows by CODEBERG_CLONE_SLOTS for each clone started and not at all when the slots are refused", {
  st <- cb_test_state()
  gone <- list(cb_clone = function(url, dir) list(status = 128L, output = "fatal: repository 'https://codeberg.org/o/r.git/' not found"))
  cb_clone_counts(gone, cb_obj(75L, "o/r"), st, withr::local_tempdir())
  expect_identical(st$asked, as.integer(CODEBERG_CLONE_SLOTS))
  cb_clone_counts(list(cb_clone = cb_local_clone(cb_fixture_repo())), cb_obj(76L, "o/s"), st, withr::local_tempdir())
  expect_identical(st$asked, 2L * CODEBERG_CLONE_SLOTS)
  st$pacer$set_hourly_budget(CODEBERG_DOMAIN, 0)
  expect_identical(cb_clone_counts(gone, cb_obj(77L, "o/t"), st, withr::local_tempdir())$state, "stopped")
  expect_identical(st$asked, 2L * CODEBERG_CLONE_SLOTS)
})

test_that("a clone member answering with something other than a list is failed with one log line, and its directory is gone", {
  lines <- new.env(); lines$v <- character(0)
  st <- cb_test_state(log = function(m) lines$v <- c(lines$v, m))
  scratch <- withr::local_tempdir()
  odd <- list(cb_clone = function(url, dir) { dir.create(dir); "fatal: something odd" })
  expect_identical(cb_clone_counts(odd, cb_obj(78L, "o/odd"), st, scratch)$state, "failed")
  expect_length(lines$v, 1L)
  expect_match(lines$v, "Codeberg o/odd: not counted:", fixed = TRUE)
  expect_length(list.files(scratch, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("the fixture clone holds no trees, as a treeless Codeberg clone does, and the commits are counted without a lazy fetch", {
  dir <- file.path(withr::local_tempdir(), "c")
  r <- cb_local_clone(cb_fixture_repo())("https://codeberg.org/someone/cbpkg.git", dir)
  expect_identical(r$status, 0L)
  expect_false(any(grepl("filtering not recognized", r$output, fixed = TRUE)))
  expect_identical(cb_git_local(dir, c("cat-file", "-e", "HEAD"))$status, 0L)
  expect_false(cb_git_local(dir, c("cat-file", "-e", shQuote("HEAD^{tree}")))$status == 0L)
  expect_identical(cb_git_local(dir, c("rev-list", "--count", "HEAD")), list(status = 0L, lines = "6"))
})

test_that("a clone URL off codeberg.org is never cloned, and a clone takes CODEBERG_CLONE_SLOTS pacer slots", {
  called <- new.env(); called$n <- 0L
  io <- list(cb_clone = function(url, dir) { called$n <- called$n + 1L; list(status = 0L, output = character()) })
  got <- cb_clone_counts(io, cb_obj(67L, "o/r", clone_url = "https://elsewhere.example/o/r.git"), cb_test_state(), withr::local_tempdir())
  expect_identical(c(got$state, as.character(called$n)), c("failed", "0"))
  st <- cb_test_state()
  cb_clone_counts(list(cb_clone = cb_local_clone(cb_fixture_repo())), cb_obj(68L, "o/r"), st, withr::local_tempdir())
  expect_identical(st$pacer$spent(CODEBERG_DOMAIN), as.integer(CODEBERG_CLONE_SLOTS))
})

test_that("the real clone member sends the User-Agent, the filter and the no-prompt environment, no credential, and never follows a redirect", {
  got <- new.env()
  run <- function(cmd, args, stdout, stderr, env, timeout) {
    got$cmd <- cmd; got$args <- args; got$env <- env; got$timeout <- timeout; character() }
  cb_git_clone("https://codeberg.org/o/r.git", "/nonexistent/x", run = run)
  expect_identical(got$cmd, "git")
  expect_true(shQuote(paste0("http.userAgent=", CODEBERG_USER_AGENT)) %in% got$args)
  expect_true(all(c("clone", "--bare", paste0("--filter=", CODEBERG_CLONE_FILTER), "--single-branch", "--no-tags", "credential.helper=") %in% got$args))
  expect_true(all(c("GIT_TERMINAL_PROMPT=0", "GIT_NO_LAZY_FETCH=1", "GIT_CONFIG_GLOBAL=/dev/null", "GIT_CONFIG_NOSYSTEM=1") %in% got$env))
  expect_identical(got$timeout, CODEBERG_CLONE_TIMEOUT_S)
  at <- match("http.followRedirects=false", got$args)
  expect_identical(got$args[at - 1L], "-c")
  expect_lt(at, match("clone", got$args))
})

test_that("a git run that throws or times out is a non-zero status, and local git reads stdout only under the local timeout", {
  url <- "https://codeberg.org/o/r.git"
  expect_true(cb_git_clone(url, "/nonexistent/x", run = function(...) stop("cannot run git"))$status != 0L)
  expect_identical(cb_git_clone(url, "/nonexistent/x", run = function(...) structure(character(), status = 124L))$status, 124L)
  expect_identical(cb_git_clone(url, "/nonexistent/x", run = function(...) character()), list(status = 0L, output = character()))
  got <- new.env()
  run <- function(cmd, args, stdout, stderr, env, timeout) {
    got$cmd <- cmd; got$args <- args; got$stdout <- stdout; got$stderr <- stderr; got$env <- env; got$timeout <- timeout; "6" }
  expect_identical(cb_git_local("/tmp/a b", c("rev-list", "--count", "HEAD"), run = run), list(status = 0L, lines = "6"))
  expect_identical(c(got$cmd, got$args), c("git", "--git-dir", shQuote("/tmp/a b"), "rev-list", "--count", "HEAD"))
  expect_identical(c(got$stdout, got$stderr), c(TRUE, FALSE))
  expect_true(all(c("GIT_TERMINAL_PROMPT=0", "GIT_NO_LAZY_FETCH=1", "GIT_CONFIG_GLOBAL=/dev/null", "GIT_CONFIG_NOSYSTEM=1") %in% got$env))
  expect_identical(got$timeout, CODEBERG_LOCAL_GIT_TIMEOUT_S)
})

test_that("a clone git cannot read is failed with every count NA, and one log line names the repository", {
  lines <- new.env(); lines$v <- character(0)
  st <- cb_test_state(log = function(m) lines$v <- c(lines$v, m))
  hollow <- list(cb_clone = function(url, dir) { dir.create(dir); list(status = 0L, output = character()) })
  expect_identical(cb_clone_counts(hollow, cb_obj(73L, "o/hollow"), st, withr::local_tempdir()),
                   list(state = "failed", commits_total = NA_integer_, contributors_total = NA_integer_,
                        top_contributor_commits = NA_integer_))
  expect_length(lines$v, 1L)
  expect_match(lines$v, "o/hollow", fixed = TRUE)
})

test_that("a clone the server did not filter still counts, and one log line names the repository", {
  lines <- new.env(); lines$v <- character(0)
  st <- cb_test_state(log = function(m) lines$v <- c(lines$v, m))
  inner <- cb_local_clone(cb_fixture_repo())
  full <- list(cb_clone = function(url, dir) {
    r <- inner(url, dir)
    r$output <- c(r$output, "warning: filtering not recognized by server, ignoring")
    r
  })
  got <- cb_clone_counts(full, cb_obj(70L, "o/full"), st, withr::local_tempdir())
  expect_identical(c(got$state, as.character(got$commits_total)), c("ok", "6"))
  expect_length(lines$v, 1L)
  expect_match(lines$v, "o/full", fixed = TRUE)
  expect_match(lines$v, "filter", fixed = TRUE)
  expect_false(grepl("@", lines$v, fixed = TRUE))
})

test_that("a failed clone's log line carries no address", {
  lines <- new.env(); lines$v <- character(0)
  st <- cb_test_state(log = function(m) lines$v <- c(lines$v, m))
  bad <- list(cb_clone = function(url, dir) list(status = 128L, output = "fatal: bad object by Ann <ann@example.invalid>"))
  cb_clone_counts(bad, cb_obj(69L, "o/r"), st, withr::local_tempdir())
  expect_gt(length(lines$v), 0L)
  expect_false(any(grepl("@", lines$v, fixed = TRUE)))
})

# ---- the weekly job ----------------------------------------------------------------
cbw_roster <- function(dir, slugs, ids) {
  path <- file.path(dir, CODEBERG_ROSTER_DB)
  write_codeberg_roster(path, data.frame(repo_id = paste0("codeberg.org/", tolower(slugs)), node_id = ids,
    owner = sub("/.*$", "", slugs), name = sub("^.*/", "", slugs), stringsAsFactors = FALSE))
  path
}
cbw_io <- function(clone = cb_local_clone(cb_fixture_repo())) list(
  cb_get = cb_fake(list("/users/o/repos?limit=50&page=1" = cb_list(list(cb_obj(1L, "o/good"), cb_obj(3L, "o/bad"))))),
  cb_clone = function(url, dir) if (grepl("/o/bad\\.git$", url)) list(status = 128L, output = "fatal: early EOF") else clone(url, dir))
cbw_snapshot <- function(out) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, CODEBERG_SHARD_DB)); on.exit(DBI::dbDisconnect(con))
  DBI::dbReadTable(con, "snapshot")
}
cbw_logger <- function() {
  e <- new.env(); e$v <- character(0)
  list(log = function(m) e$v <- c(e$v, m), lines = function() e$v)
}
# A summary as the release holds it: data.table on GitHub, with stars, and these repos rows.
cbw_summary <- function(path, rows) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
  ensure_series_schema(con); ensure_repo_schema(con)
  DBI::dbExecute(con, "INSERT INTO vcs_signals_summary (package, origin, repo_id, stars, forks, releases_total) VALUES
    ('data.table', 'cran', 'github.com/rdatatable/data.table', 3400, 960, 9)")
  DBI::dbExecute(con, paste("INSERT INTO repos VALUES", paste(rows, collapse = ",\n")))
  path
}
# One repos row, owner and name taken from the repo_id.
cbw_repo <- function(id, node, host, status = "active") sprintf(
  "('%s', %s, '%s', '%s', '%s', '%s', '%s', 1, 1, '2026-01-01', '2026-10-01', '%s')",
  id, if (is.na(node)) "NULL" else sprintf("'%s'", node), host, sub("/.*$", "", id),
  sub("^[^/]*/([^/]*)/.*$", "\\1", id), sub("^.*/", "", id), sub("^[^/]*/", "", id), status)
cbw_download <- function(src) list(download = function(name, dir) file.copy(src, file.path(dir, name), overwrite = TRUE))

test_that("enumerate writes a Codeberg roster of the active Codeberg repositories with a Codeberg id", {
  src <- cbw_summary(withr::local_tempfile(fileext = ".db"), c(
    cbw_repo("github.com/rdatatable/data.table", "R_dt", "github"),
    cbw_repo("codeberg.org/zeileis/zoo", "codeberg:codeberg.org:2277662", "codeberg"),
    cbw_repo("codeberg.org/someone/new", NA, "codeberg"),
    cbw_repo("codeberg.org/someone/gone", "codeberg:codeberg.org:9", "codeberg", "retired"),
    cbw_repo("codeberg.org/someone/odd", "gitlab:gitlab.com:5", "codeberg")))
  out <- withr::local_tempdir()
  suppressMessages(run_weekly_enumerate(cbw_download(src), out))
  cb <- load_roster(file.path(out, CODEBERG_ROSTER_DB))
  expect_identical(names(cb), c("repo_id", "node_id", "owner", "name"))
  expect_identical(cb$repo_id, "codeberg.org/zeileis/zoo")
  expect_identical(c(cb$node_id, cb$owner, cb$name), c("codeberg:codeberg.org:2277662", "zeileis", "zoo"))
  expect_identical(load_roster(file.path(out, "vcs-signals-roster.db"))$repo_id, "github.com/rdatatable/data.table")
})

test_that("a Codeberg roster that cannot be written is logged, and the GitHub and GitLab rosters are still written", {
  src <- cbw_summary(withr::local_tempfile(fileext = ".db"), c(
    cbw_repo("github.com/rdatatable/data.table", "R_dt", "github"),
    cbw_repo("gitlab.com/g/p", "gitlab:gitlab.com:5", "gitlab"),
    cbw_repo("codeberg.org/zeileis/zoo", "codeberg:codeberg.org:2277662", "codeberg")))
  out <- withr::local_tempdir()
  # A directory where the roster goes makes writing it fail.
  dir.create(file.path(out, CODEBERG_ROSTER_DB))
  msgs <- testthat::capture_messages(run_weekly_enumerate(cbw_download(src), out))
  expect_true(any(grepl("enumerate: no codeberg roster", msgs, fixed = TRUE)))
  expect_identical(load_roster(file.path(out, "vcs-signals-roster.db"))$repo_id, "github.com/rdatatable/data.table")
  expect_identical(load_roster(file.path(out, "vcs-signals-roster-gitlab.db"))$repo_id, "gitlab.com/g/p")
})

test_that("a Codeberg roster that fails partway is removed, so fetch-codeberg finds no roster", {
  src <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), src)
  # Without its primary key, repos holds zoo twice, and the roster's key refuses the second row.
  DBI::dbExecute(con, "CREATE TABLE repos (repo_id TEXT, node_id TEXT, host TEXT NOT NULL, host_domain TEXT NOT NULL,
    owner TEXT NOT NULL, name TEXT NOT NULL, name_with_owner TEXT NOT NULL, supported INTEGER NOT NULL,
    n_packages INTEGER NOT NULL, first_seen TEXT NOT NULL, last_seen TEXT NOT NULL, status TEXT NOT NULL DEFAULT 'active')")
  DBI::dbDisconnect(con)
  zoo <- cbw_repo("codeberg.org/zeileis/zoo", "codeberg:codeberg.org:2277662", "codeberg")
  cbw_summary(src, c(cbw_repo("github.com/rdatatable/data.table", "R_dt", "github"), zoo, zoo))
  out <- withr::local_tempdir()
  msgs <- testthat::capture_messages(run_weekly_enumerate(cbw_download(src), out))
  expect_true(any(grepl("enumerate: no codeberg roster", msgs, fixed = TRUE)))
  expect_false(file.exists(file.path(out, CODEBERG_ROSTER_DB)))
  expect_identical(load_roster(file.path(out, "vcs-signals-roster.db"))$repo_id, "github.com/rdatatable/data.table")
})

test_that("a missing roster is a workflow warning and an empty shard, and no file appears where the roster belongs", {
  dir <- withr::local_tempdir()
  roster <- file.path(dir, CODEBERG_ROSTER_DB); scratch <- file.path(dir, "scratch")
  lg <- cbw_logger()
  p <- run_fetch_codeberg(list(), file.path(dir, "out"), roster, now = cb_clock()$now, sleep = function(s) NULL,
                          scratch = scratch, log = lg$log)
  expect_identical(p, file.path(dir, "out", CODEBERG_SHARD_DB))
  expect_identical(nrow(cbw_snapshot(file.path(dir, "out"))), 0L)
  expect_true(any(startsWith(lg$lines(), "::warning::Codeberg: the roster could not be read")))
  expect_false(file.exists(roster))
  expect_false(dir.exists(scratch))
})

test_that("the weekly job reads each located repository and leaves NA where a clone failed or nothing was found", {
  dir <- withr::local_tempdir(); scratch <- file.path(dir, "scratch")
  roster <- cbw_roster(dir, c("o/good", "o/bad", "o/lost"), sprintf("codeberg:codeberg.org:%d", c(1L, 3L, 5L)))
  clock <- cb_clock()
  suppressMessages(run_fetch_codeberg(cbw_io(), file.path(dir, "out"), roster, now = clock$now, sleep = clock$sleep, scratch = scratch))
  s <- cbw_snapshot(file.path(dir, "out"))
  expect_identical(s$repo_id, c("codeberg.org/o/bad", "codeberg.org/o/good", "codeberg.org/o/lost"))
  expect_identical(s$commits_total, c(NA, 6L, NA))
  expect_identical(s$contributors_total, c(NA, 4L, NA))
  expect_identical(s$top_contributor_commits, c(NA, 3L, NA))
  expect_true(all(is.na(s$top_contributor_bot)))
  expect_false(dir.exists(scratch))
})

test_that("a failing clone or a fault outside any answer still writes the shard, and a fault in one clone loses only that repository", {
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a"); dir.create(a)
  suppressMessages(run_fetch_codeberg(cbw_io(), file.path(a, "out"), cbw_roster(a, "o/bad", "codeberg:codeberg.org:3"),
                                      now = cb_clock()$now, sleep = function(s) NULL))
  s <- cbw_snapshot(file.path(a, "out"))
  expect_identical(s$repo_id, "codeberg.org/o/bad")
  expect_true(is.na(s$commits_total))

  # The pause before the second listing throws, outside any answer, and locating stops.
  b <- file.path(dir, "b"); dir.create(b); scratch <- file.path(b, "scratch")
  lg <- cbw_logger()
  run_fetch_codeberg(cbw_io(), file.path(b, "out"), cbw_roster(b, c("o/good", "p/x"), sprintf("codeberg:codeberg.org:%d", 1:2)),
                     now = cb_clock()$now, sleep = function(s) stop("simulated pause fault"), scratch = scratch, log = lg$log)
  s <- cbw_snapshot(file.path(b, "out"))
  expect_identical(s$repo_id, c("codeberg.org/o/good", "codeberg.org/p/x"))
  expect_true(all(is.na(s$commits_total)))
  expect_true(any(startsWith(lg$lines(), "::warning::Codeberg: locating stopped by a fault")))
  expect_false(dir.exists(scratch))

  # The pause before o/next's clone throws; o/good and o/zed are still read.
  d <- file.path(dir, "d"); dir.create(d)
  clock <- cb_clock(); arm <- new.env(); arm$on <- FALSE
  clone <- cb_local_clone(cb_fixture_repo())
  io <- list(cb_get = cb_fake(list("/users/o/repos?limit=50&page=1" =
               cb_list(list(cb_obj(1L, "o/good"), cb_obj(2L, "o/next"), cb_obj(4L, "o/zed"))))),
             cb_clone = function(url, dir) { r <- clone(url, dir); arm$on <- grepl("/o/good\\.git$", url); r })
  sleep <- function(s) { if (arm$on) { arm$on <- FALSE; stop("simulated pause fault") }; clock$sleep(s) }
  lg <- cbw_logger()
  run_fetch_codeberg(io, file.path(d, "out"), cbw_roster(d, c("o/good", "o/next", "o/zed"), sprintf("codeberg:codeberg.org:%d", c(1L, 2L, 4L))),
                     now = clock$now, sleep = sleep, log = lg$log)
  expect_identical(cbw_snapshot(file.path(d, "out"))$commits_total, c(6L, NA, 6L))
  expect_true(any(grepl("o/next", lg$lines(), fixed = TRUE) & grepl("fault", lg$lines(), fixed = TRUE)))
})

test_that("a fault in the clone stage outside any one repository is a workflow warning, and the shard is still written", {
  dir <- withr::local_tempdir()
  clock <- cb_clock(); arm <- new.env(); arm$on <- FALSE
  # The listing arms a clock that throws once: at the deadline check before the first clone.
  listing <- cb_list(list(cb_obj(1L, "o/good")))
  io <- list(cb_get = function(path) { arm$on <- TRUE; listing }, cb_clone = cb_local_clone(cb_fixture_repo()))
  now <- function() { if (arm$on) { arm$on <- FALSE; stop("simulated clock fault") }; clock$now() }
  lg <- cbw_logger()
  run_fetch_codeberg(io, file.path(dir, "out"), cbw_roster(dir, "o/good", "codeberg:codeberg.org:1"),
                     now = now, sleep = clock$sleep, log = lg$log)
  expect_identical(cbw_snapshot(file.path(dir, "out"))$repo_id, "codeberg.org/o/good")
  expect_true(is.na(cbw_snapshot(file.path(dir, "out"))$commits_total))
  expect_true(any(startsWith(lg$lines(), "::warning::Codeberg: cloning stopped by a fault")))
})

test_that("past the deadline no clone starts, and those repositories read NA", {
  dir <- withr::local_tempdir()
  roster <- cbw_roster(dir, c("o/good", "o/bad"), sprintf("codeberg:codeberg.org:%d", c(1L, 3L)))
  clock <- cb_clock()
  io <- cbw_io(); inner <- io$cb_clone
  started <- new.env(); started$urls <- character(0)
  io$cb_clone <- function(url, dir) { started$urls <- c(started$urls, url); clock$env$t <- clock$env$t + 600; inner(url, dir) }
  suppressMessages(run_fetch_codeberg(io, file.path(dir, "out"), roster, now = clock$now, sleep = clock$sleep, deadline_min = 5))
  expect_identical(sum(!is.na(cbw_snapshot(file.path(dir, "out"))$commits_total)), 0L)
  expect_identical(started$urls, "https://codeberg.org/o/bad.git")
})

test_that("one line gives git's version, and the summary line counts each outcome, the requests with clone slots and the lowest r", {
  dir <- withr::local_tempdir()
  roster <- cbw_roster(dir, c("o/bad", "o/empty", "o/good", "o/lost", "o/zed"),
                       sprintf("codeberg:codeberg.org:%d", c(3L, 7L, 1L, 5L, 9L)))
  clock <- cb_clock()
  clone <- cb_local_clone(cb_fixture_repo())
  io <- list(
    cb_get = cb_fake(list("/users/o/repos?limit=50&page=1" = cb_list(list(cb_obj(1L, "o/good"), cb_obj(3L, "o/bad"),
      cb_obj(7L, "o/empty", empty = TRUE), cb_obj(9L, "o/zed")), r = 1500L))),
    cb_clone = function(url, dir) {
      if (grepl("/o/bad\\.git$", url)) return(list(status = 128L, output = "fatal: early EOF"))
      r <- clone(url, dir)
      if (grepl("/o/good\\.git$", url)) clock$env$t <- clock$env$t + 600
      r
    })
  lg <- cbw_logger()
  run_fetch_codeberg(io, file.path(dir, "out"), roster, now = clock$now, sleep = clock$sleep, deadline_min = 5, log = lg$log)
  lines <- lg$lines()
  expect_identical(sum(grepl("^weekly fetch codeberg: git version [0-9]", lines)), 1L)
  expect_identical(lines[length(lines)], paste(
    "weekly fetch codeberg: 1 read, 1 failed, 1 empty, 1 not located, 1 not reached of 5 repos,",
    "8 requests and clone slots, lowest r 1500, 607 s"))
})

test_that("the Codeberg shard's snapshot table has the GitHub shard's columns", {
  dir <- withr::local_tempdir()
  write_roster(file.path(dir, "vcs-signals-roster.db"), data.frame(repo_id = "github.com/a/ok", owner = "a",
    name = "ok", stars = 1L, done = 0L, stringsAsFactors = FALSE))
  gh_io <- list(graphql = function(query) list(data = list(r0 = list(defaultBranchRef = list(target = list(history = list(totalCount = 100)))))),
                contributors = function(owner, name) list(count = 7L, top_commits = 5L, top_type = "User"))
  gh <- suppressMessages(run_fetch_shard(gh_io, file.path(dir, "gh"), file.path(dir, "vcs-signals-roster.db"), 0, 1,
                                         commit_delay = 0, contributor_delay = 0))
  roster <- cbw_roster(dir, "o/good", "codeberg:codeberg.org:1")
  cb <- suppressMessages(run_fetch_codeberg(cbw_io(), file.path(dir, "cb"), roster, now = cb_clock()$now, sleep = function(s) NULL))
  schema <- function(p) { con <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(con))
    DBI::dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE name = 'snapshot'")$sql }
  expect_identical(schema(cb), schema(gh))
  snap <- cbw_snapshot(file.path(dir, "cb"))
  for (m in c("top_contributor_bot", "median_days_to_close_issue", "median_days_to_close_pr", "median_open_issue_age_days"))
    expect_true(is.na(snap[[m]]), info = m)
})

test_that("no @ reaches any table the job writes or any line it logs", {
  dir <- withr::local_tempdir()
  roster <- cbw_roster(dir, c("o/good", "o/bad"), sprintf("codeberg:codeberg.org:%d", c(1L, 3L)))
  io <- cbw_io()
  io$cb_clone <- function(url, dir) if (grepl("bad", url)) list(status = 128L, output = "fatal: by Ann <ann@example.invalid>")
                                    else cb_local_clone(cb_fixture_repo())(url, dir)
  lg <- cbw_logger()
  msgs <- testthat::capture_messages(run_fetch_codeberg(io, file.path(dir, "out"), roster, now = cb_clock()$now,
    sleep = function(s) NULL, log = lg$log))
  expect_gt(length(lg$lines()), 0L)
  expect_false(any(grepl("@", c(lg$lines(), msgs), fixed = TRUE)))
  dbs <- c(roster, list.files(file.path(dir, "out"), pattern = "\\.db$", full.names = TRUE))
  expect_length(dbs, 2L)
  for (db in dbs) {
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    for (t in DBI::dbListTables(con))
      expect_false(any(grepl("@", unlist(lapply(DBI::dbReadTable(con, t), as.character)), fixed = TRUE)), info = t)
    DBI::dbDisconnect(con)
  }
})

test_that("main runs fetch-codeberg on the Codeberg roster in VCS_ROSTER, and the usage line names it", {
  dir <- withr::local_tempdir()
  cbw_roster(dir, "o/good", "codeberg:codeberg.org:1")
  withr::local_envvar(VCS_ROSTER = dir)
  out <- file.path(dir, "out")
  # weekly.R's own main, whichever script last defined a global one.
  weekly <- new.env(parent = globalenv())
  withr::with_dir(.repo_root, sys.source(file.path("scripts", "weekly.R"), envir = weekly))
  # The listing fails, so nothing more is asked and no pause is taken.
  suppressMessages(weekly$main("fetch-codeberg", out, io = list(cb_get = function(path) stop("offline"))))
  expect_identical(cbw_snapshot(out)$repo_id, "codeberg.org/o/good")
  expect_error(weekly$main("no-such-mode", out, io = list()), "fetch-codeberg", fixed = TRUE)
})
