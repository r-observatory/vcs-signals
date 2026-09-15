# In-memory DB with the SP1 schema applied, for persistence tests.
new_test_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  ensure_repo_schema(con)
  ensure_series_schema(con)
  con
}

# A release kept in a local directory, standing in for "current".
#
# publish() now refuses to upload unless the release is still the one its
# working database was seeded from, and afterwards confirms that every asset it
# uploaded carries the digest of the file it sent. A stub whose upload records a
# name and throws the bytes away cannot pass that confirmation, and a stub whose
# download reports success without writing anything never had a release behind
# it. So every test that publishes goes through this one fake, where upload
# copies into the directory, download copies out of it, and the generation is
# computed from what is really there, in the same "name<TAB>sha256:<hex>" form
# gh_release_generation reads from GitHub.
local_release_generation <- function(rel) {
  f <- list.files(rel, full.names = TRUE)
  if (!length(f)) return("")
  lines <- paste(basename(f), paste0("sha256:", vapply(f, file_sha256, character(1))), sep = "\t")
  paste(sort(lines, method = "radix"), collapse = "\n")
}

# md5 of every file in the release, by name: what a test compares to prove a
# refused publish left the release exactly as it found it.
local_release_snapshot <- function(rel) {
  f <- list.files(rel, full.names = TRUE)
  stats::setNames(unname(tools::md5sum(f)), basename(f))
}

# The io a publisher needs, over the release in `rel`. Anything in `...`
# replaces or adds a member (acquire, graphql, contributors, or a broken
# generation or download). The hooks let a test run a second publisher at an
# exact point inside this one: on_download(pattern) after a download has
# landed, on_upload(name) after an upload has landed, and on_generation(n)
# after the n-th generation read but before its value is returned.
# calls() and uploaded() report what this io was asked to do, in order.
local_release_io <- function(rel, ..., on_download = NULL, on_upload = NULL,
                             on_generation = NULL) {
  log <- new.env(parent = emptyenv())
  log$calls <- character(0)
  log$uploaded <- character(0)
  log$generations <- 0L
  note <- function(what) log$calls <- c(log$calls, what)
  io <- list(
    release_exists = function() length(list.files(rel)) > 0,
    generation = function() {
      note("generation")
      g <- local_release_generation(rel)
      log$generations <- log$generations + 1L
      if (!is.null(on_generation)) on_generation(log$generations)
      g
    },
    download = function(pattern, dir) {
      note(paste("download", pattern))
      f <- list.files(rel, pattern = utils::glob2rx(pattern), full.names = TRUE)
      if (!length(f)) return(FALSE)
      ok <- all(file.copy(f, file.path(dir, basename(f)), overwrite = TRUE))
      if (!is.null(on_download)) on_download(pattern)
      ok
    },
    upload = function(path) {
      note(paste("upload", basename(path)))
      if (!isTRUE(file.copy(path, file.path(rel, basename(path)), overwrite = TRUE)))
        stop("fake release could not store ", path)
      log$uploaded <- c(log$uploaded, basename(path))
      if (!is.null(on_upload)) on_upload(basename(path))
      invisible(NULL)
    },
    sleep = function(seconds) invisible(NULL),
    calls = function() log$calls,
    uploaded = function() log$uploaded)
  utils::modifyList(io, list(...))
}

# Auto-sourced by testthat before any test file runs. At this point the
# working directory is tests/testthat, so scripts/update.R lives two levels
# up. update.R's own top-level source() calls (config.R/helpers.R/github.R)
# use paths relative to the repo root, so temporarily chdir there while
# sourcing it, then restore the tests/testthat cwd the other test files rely
# on (their fixture paths are relative to tests/testthat).
.repo_root <- normalizePath(file.path(getwd(), "..", ".."))
.orig_wd <- setwd(.repo_root)
source(file.path(.repo_root, "scripts", "update.R"))
setwd(.orig_wd)
