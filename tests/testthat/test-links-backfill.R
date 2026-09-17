# The committed history of package-to-repository links, and the reader that
# refuses to apply any of it when any of it is wrong.

.write_backfill <- function(lines, gz = TRUE) {
  path <- tempfile(fileext = if (gz) ".csv.gz" else ".csv")
  con <- if (gz) gzfile(path, "w") else file(path, "w")
  writeLines(lines, con)
  close(con)
  path
}

.good_lines <- c(
  "repo_id,package,origin,first_seen,last_seen",
  "github.com/o/one,pkgA,cran,2026-08-01,2026-09-15",
  "gitlab.com/group/sub/two,pkgB,bioc,2026-08-04,2026-08-30")

test_that("a well-formed backfill reads back as five character columns", {
  got <- read_links_backfill(.write_backfill(.good_lines))
  expect_equal(names(got), c("repo_id", "package", "origin", "first_seen", "last_seen"))
  expect_true(all(vapply(got, is.character, logical(1))))
  expect_equal(got$repo_id, c("github.com/o/one", "gitlab.com/group/sub/two"))
  expect_equal(got$last_seen, c("2026-09-15", "2026-08-30"))
})

test_that("a package named NA stays a package named NA", {
  # read.csv turns the string NA into a missing value by default, which would
  # then read as an empty package name and refuse a file that is fine.
  got <- read_links_backfill(.write_backfill(c(.good_lines[1],
    "github.com/o/na,NA,cran,2026-08-01,2026-08-02")))
  expect_identical(got$package, "NA")
})

.refuses <- function(lines, pattern) {
  expect_error(read_links_backfill(.write_backfill(lines)), pattern)
}

test_that("a missing file stops the run", {
  expect_error(read_links_backfill(file.path(tempdir(), "no-such-backfill.csv.gz")),
               "not found")
})

test_that("a malformed backfill stops the run instead of applying what parsed", {
  hdr <- .good_lines[1]; ok <- .good_lines[2]
  .refuses(c("repo,package,origin,first_seen,last_seen", ok), "header")
  .refuses(c(hdr), "no links")
  .refuses(c(hdr, ok, "github.com/o/x,pkgX,cran,2026-08-01"), "5 fields")
  .refuses(c(hdr, ok, "github.com/o/x,pkgX,cran,2026-08-01,2026-08-02,extra"), "5 fields")
  .refuses(c(hdr, ok, ",pkgX,cran,2026-08-01,2026-08-02"), "repo_id")
  .refuses(c(hdr, ok, "GitHub.com/O/X,pkgX,cran,2026-08-01,2026-08-02"), "repo_id")
  .refuses(c(hdr, ok, "github.com/x,pkgX,cran,2026-08-01,2026-08-02"), "repo_id")
  .refuses(c(hdr, ok, "github.com/o/x,,cran,2026-08-01,2026-08-02"), "package")
  .refuses(c(hdr, ok, "github.com/o/x,pkgX,CRAN,2026-08-01,2026-08-02"), "origin")
  .refuses(c(hdr, ok, "github.com/o/x,pkgX,cran,2026-8-01,2026-08-02"), "first_seen")
  .refuses(c(hdr, ok, "github.com/o/x,pkgX,cran,2026-08-01,2026-02-30"), "last_seen")
  .refuses(c(hdr, ok, "github.com/o/x,pkgX,cran,2026-08-03,2026-08-02"), "after")
  .refuses(c(hdr, ok, ok), "more than once")
})

test_that("the refusal names the offending line so it can be found", {
  expect_error(read_links_backfill(.write_backfill(c(.good_lines,
    "github.com/o/x,pkgX,cran,2026-08-01,yesterday"))), "line 4")
})

test_that("a blank line is refused where it is, not skipped", {
  # Skipping it would renumber every line after it, so the refusal for a bad
  # row further down would point one line above the row it means.
  expect_error(read_links_backfill(.write_backfill(c(.good_lines[1:2], "", .good_lines[3]))),
               "line 3 is blank")
})

test_that("a # in a field is data, as it is to the parser that reads the rows", {
  # The field count and the parse have to read the file the same way. Counted
  # with the default comment character, everything after the # vanished and a
  # sound row was refused for having two fields.
  got <- read_links_backfill(.write_backfill(c(.good_lines,
    "github.com/o/x,pkg#X,cran,2026-08-01,2026-08-02")))
  expect_equal(got$package[3], "pkg#X")
})

test_that("the committed backfill is well formed and complete", {
  # The history it carries lives nowhere else once the run artifacts it was
  # rebuilt from expire, so a truncated or hand-edited copy is worth a red suite
  # rather than a quiet partial table.
  got <- read_links_backfill(LINKS_BACKFILL_PATH)
  expect_equal(nrow(got), 16474L)
  expect_equal(sum(duplicated(got[c("repo_id", "package", "origin")])), 0L)
  expect_equal(min(got$first_seen), "2026-08-01")
  expect_equal(max(got$last_seen), "2026-09-15")

  # The counts and the date range above survive an edit that keeps them: a
  # repo_id re-keyed or a window narrowed passes every one of them, and the
  # regression gate then holds every later build to the edited history. The
  # hash is of the rows rather than the .gz, so recompressing the same rows with
  # another gzip does not trip it, and changing the rows has to change it here.
  raw <- tempfile(fileext = ".csv")
  gz <- gzfile(LINKS_BACKFILL_PATH, "rb")
  writeBin(readBin(gz, "raw", n = 64 * 1024^2), raw)
  close(gz)
  expect_equal(file_sha256(raw),
               "87e7f993d02f738bea5a77369eb7ee3a317d61143a38a161e2632708ad489972")
})
