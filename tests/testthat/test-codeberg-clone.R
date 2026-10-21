# The Codeberg clone readers (scripts/codeberg_clone.R), over the fixture repository in helper-codeberg-git.R.

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
  expect_identical(cb_clone_counts(r429, cb_obj(65L, "o/q"), st, withr::local_tempdir())$state, "stopped")
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
