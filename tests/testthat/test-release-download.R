# gh_release_download: a transient failure is downloaded again after a wait, and
# a release that does not list the asset is not. Every test injects run, sleep and
# rand, so nothing reaches GitHub and the suite never waits.

no_jitter <- function() 1

# Stands in for system2("gh", ...). fail(pattern, n) is NULL for the n-th download
# of `pattern` to succeed, or the text gh prints when it exits 1. A download that
# succeeds writes the asset into --dir, and the manifest lists the 2010 shard.
fake_gh_download <- function(fail = function(pattern, n) NULL) {
  log <- new.env(parent = emptyenv())
  log$patterns <- character(0)
  log$args <- list()
  run <- function(command, args, stdout, stderr) {
    pattern <- args[which(args == "--pattern") + 1L]
    dir <- args[which(args == "--dir") + 1L]
    log$patterns <- c(log$patterns, pattern)
    log$args[[length(log$args) + 1L]] <- c(command, args)
    err <- fail(pattern, sum(log$patterns == pattern))
    if (!is.null(err)) return(structure(err, status = 1L))
    if (identical(pattern, "manifest.json")) {
      write_manifest(file.path(dir, pattern), character(0), "current",
                     list(source_kind = "live", years = list(2010L)))
    } else {
      writeLines("shard", file.path(dir, pattern))
    }
    character(0)
  }
  list(run = run, patterns = function() log$patterns, args = function() log$args)
}

download_dir <- function() {
  d <- tempfile("dl_")
  dir.create(d)
  d
}

test_that("a download that fails twice and then succeeds returns TRUE after the planned waits", {
  slept <- numeric(0)
  gh <- fake_gh_download(function(pattern, n) if (n <= 2L) "gh: HTTP 502" else NULL)
  dir <- download_dir()
  ok <- suppressMessages(gh_release_download("o/r", "vcs-signals-2010.db", dir, run = gh$run,
                                             sleep = function(s) slept <<- c(slept, s),
                                             rand = no_jitter))
  expect_true(ok)
  expect_length(gh$patterns(), 3L)
  expect_equal(slept, c(15, 45))
  expect_true(file.exists(file.path(dir, "vcs-signals-2010.db")))
})

test_that("a download that always fails is FALSE after four attempts and the planned waits", {
  slept <- numeric(0)
  gh <- fake_gh_download(function(pattern, n) "gh: HTTP 504")
  ok <- suppressMessages(gh_release_download("o/r", "vcs-signals-2010.db", download_dir(),
                                             run = gh$run,
                                             sleep = function(s) slept <<- c(slept, s),
                                             rand = no_jitter))
  expect_false(ok)
  expect_length(gh$patterns(), 4L)
  expect_equal(slept, c(15, 45, 90))
  expect_equal(slept, RELEASE_DOWNLOAD_RETRY_WAITS_S)
})

test_that("an asset the release does not list is FALSE at once, with no wait", {
  for (said in c("no assets match the file pattern", "no assets to download")) {
    slept <- numeric(0)
    gh <- fake_gh_download(function(pattern, n) said)
    ok <- suppressMessages(gh_release_download("o/r", "vcs-signals-2010.db", download_dir(),
                                               run = gh$run,
                                               sleep = function(s) slept <<- c(slept, s),
                                               rand = no_jitter))
    expect_false(ok, info = said)
    expect_length(gh$patterns(), 1L)
    expect_length(slept, 0L)
  }
})

test_that("release not found is downloaded again, since gh words a failed lookup that way", {
  # gh 2.96 and later look the tag up as a published release and as a draft at
  # once, and a 502 on the published lookup comes back as "release not found".
  slept <- numeric(0)
  gh <- fake_gh_download(function(pattern, n) if (n == 1L) "release not found" else NULL)
  ok <- suppressMessages(gh_release_download("o/r", "manifest.json", download_dir(), run = gh$run,
                                             sleep = function(s) slept <<- c(slept, s),
                                             rand = no_jitter))
  expect_true(ok)
  expect_length(gh$patterns(), 2L)
  expect_equal(slept, 15)
})

test_that("each failed attempt is logged with what gh said", {
  gh <- fake_gh_download(function(pattern, n) if (n == 1L) "gh: HTTP 502" else NULL)
  expect_message(
    gh_release_download("o/r", "vcs-signals-2010.db", download_dir(), run = gh$run,
                        sleep = function(s) invisible(NULL), rand = no_jitter),
    "vcs-signals-2010.db.*attempt 1 of 4.*HTTP 502")
})

test_that("the download asks gh for the same asset, directory and overwrite as before", {
  gh <- fake_gh_download()
  dir <- download_dir()
  expect_true(gh_release_download("r-observatory/vcs-signals", "manifest.json", dir, run = gh$run))
  expect_equal(gh$args()[[1]],
               c("gh", "release", "download", "current", "--repo", "r-observatory/vcs-signals",
                 "--pattern", "manifest.json", "--dir", dir, "--clobber"))
})

# ---- through protect_history_pull, the way every publisher pulls ---------------

pull_through_gh <- function(gh, slept_env) {
  io <- list(
    release_exists = function() TRUE,
    download = function(pattern, dir)
      gh_release_download("o/r", pattern, dir, run = gh$run,
                          sleep = function(s) slept_env$s <- c(slept_env$s, s),
                          rand = no_jitter))
  out <- download_dir()
  tryCatch(suppressMessages(protect_history_pull(io, out)), error = function(e) e)
}

today_error <- function(pattern) {
  paste0("release 'current' exists but ", pattern, " could not be downloaded; ",
         "aborting to protect accumulated history.", lost_asset_hint(pattern))
}

test_that("a year shard that fails twice during a 502 burst is pulled and the pull goes on", {
  slept <- new.env(); slept$s <- numeric(0)
  gh <- fake_gh_download(function(pattern, n)
    if (identical(pattern, "vcs-signals-2010.db") && n <= 2L) "gh: HTTP 502" else NULL)
  got <- pull_through_gh(gh, slept)
  expect_false(inherits(got, "error"))
  expect_equal(got, c("manifest.json", "vcs-signals-recent.db", "vcs-signals-2010.db",
                      "vcs-signals-summary.db"))
  expect_equal(slept$s, c(15, 45))
})

test_that("a year shard that fails every attempt stops the pull with the same error as before", {
  slept <- new.env(); slept$s <- numeric(0)
  gh <- fake_gh_download(function(pattern, n)
    if (identical(pattern, "vcs-signals-2010.db")) "gh: HTTP 504" else NULL)
  got <- pull_through_gh(gh, slept)
  expect_s3_class(got, "error")
  expect_identical(conditionMessage(got), today_error("vcs-signals-2010.db"))
  expect_match(conditionMessage(got), "The per-year release 2010 keeps the copy", fixed = TRUE)
  expect_equal(sum(gh$patterns() == "vcs-signals-2010.db"), 4L)
  expect_equal(slept$s, c(15, 45, 90))
  expect_false("vcs-signals-summary.db" %in% gh$patterns())
})

test_that("a year shard the release does not list stops the pull at once, with the hint", {
  slept <- new.env(); slept$s <- numeric(0)
  gh <- fake_gh_download(function(pattern, n)
    if (identical(pattern, "vcs-signals-2010.db")) "no assets match the file pattern" else NULL)
  got <- pull_through_gh(gh, slept)
  expect_s3_class(got, "error")
  expect_identical(conditionMessage(got), today_error("vcs-signals-2010.db"))
  expect_equal(sum(gh$patterns() == "vcs-signals-2010.db"), 1L)
  expect_length(slept$s, 0L)
})

test_that("a lost manifest or recent shard stops the pull at once with its own hint", {
  for (asset in c("manifest.json", "vcs-signals-recent.db")) {
    slept <- new.env(); slept$s <- numeric(0)
    gh <- fake_gh_download(function(pattern, n)
      if (identical(pattern, asset)) "no assets match the file pattern" else NULL)
    got <- pull_through_gh(gh, slept)
    expect_identical(conditionMessage(got), today_error(asset), info = asset)
    expect_match(conditionMessage(got), sprintf("No other release keeps a copy of %s", asset),
                 fixed = TRUE, info = asset)
    expect_length(slept$s, 0L)
  }
})

test_that("every publisher's real io downloads through gh_release_download", {
  # update.R, weekly.R, backfill.R and ai_backfill.R share the one function, so
  # the retry reaches the daily update, the weekly run and both backfills alike.
  for (script in c("update.R", "weekly.R", "backfill.R", "ai_backfill.R")) {
    src <- paste(readLines(file.path(.repo_root, "scripts", script), warn = FALSE), collapse = "\n")
    expect_true(grepl(paste0("download\\s*=\\s*function\\(pattern, dir\\)\\s*",
                             "gh_release_download\\(RELEASE_REPO, pattern, dir\\)"), src),
                info = script)
  }
  expect_false(grepl("gh_release_download <- function",
                     paste(unlist(lapply(c("weekly.R", "backfill.R", "ai_backfill.R"), function(s)
                       readLines(file.path(.repo_root, "scripts", s), warn = FALSE))), collapse = "\n"),
                     fixed = TRUE))
})
