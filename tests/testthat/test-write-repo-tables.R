repos1 <- data.frame(repo_id = "github.com/o/n", host = "github", host_domain = "github.com",
  owner = "o", name = "n", name_with_owner = "o/n", supported = 1L, n_packages = 1L, stringsAsFactors = FALSE)
rp1 <- data.frame(repo_id = "github.com/o/n", package = "p1", origin = "cran",
  resolved_from = "url", stringsAsFactors = FALSE)

test_that("write_repo_tables inserts new rows", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  write_repo_tables(con, repos1, rp1, "2026-07-06")
  got <- DBI::dbGetQuery(con, "SELECT * FROM repos")
  expect_equal(nrow(got), 1)
  expect_equal(got$first_seen, "2026-07-06")
  expect_equal(got$status, "active")
  expect_true(is.na(got$node_id))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM repo_packages")$n, 1)
})

test_that("re-running preserves first_seen and node_id (idempotent UPSERT)", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  write_repo_tables(con, repos1, rp1, "2026-07-06")
  DBI::dbExecute(con, "UPDATE repos SET node_id = 'R_abc' WHERE repo_id = 'github.com/o/n'")
  repos1b <- repos1; repos1b$n_packages <- 5L
  write_repo_tables(con, repos1b, rp1, "2026-07-07")
  got <- DBI::dbGetQuery(con, "SELECT * FROM repos")
  expect_equal(got$first_seen, "2026-07-06")   # preserved
  expect_equal(got$last_seen, "2026-07-07")    # updated
  expect_equal(got$node_id, "R_abc")           # preserved
  expect_equal(got$n_packages, 5L)             # updated
})

test_that("a repo absent this run is retired, not deleted", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  write_repo_tables(con, repos1, rp1, "2026-07-06")
  empty_repos <- repos1[0, ]; empty_rp <- rp1[0, ]
  write_repo_tables(con, empty_repos, empty_rp, "2026-07-07")
  got <- DBI::dbGetQuery(con, "SELECT repo_id, status FROM repos")
  expect_equal(nrow(got), 1)
  expect_equal(got$status, "retired")
})

test_that("gone/moved status is preserved on update", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  write_repo_tables(con, repos1, rp1, "2026-07-06")
  DBI::dbExecute(con, "UPDATE repos SET status = 'gone' WHERE repo_id = 'github.com/o/n'")
  write_repo_tables(con, repos1, rp1, "2026-07-07")
  expect_equal(DBI::dbGetQuery(con, "SELECT status FROM repos")$status, "gone")
})

# ---------------------------------------------------------------------------
# repo_package_links: the package-to-repository link outlives the package.
# ---------------------------------------------------------------------------

.links <- function(con) DBI::dbGetQuery(con,
  "SELECT repo_id, package, origin, first_seen, last_seen FROM repo_package_links
    ORDER BY repo_id, package, origin")

.repo_row <- function(id) {
  p <- strsplit(id, "/", fixed = TRUE)[[1]]
  data.frame(repo_id = id, host = "github", host_domain = p[1], owner = p[2], name = p[3],
             name_with_owner = paste(p[2], p[3], sep = "/"), supported = 1L, n_packages = 1L,
             stringsAsFactors = FALSE)
}
.rp_row <- function(id, pkg, origin = "cran")
  data.frame(repo_id = id, package = pkg, origin = origin, resolved_from = "url",
             stringsAsFactors = FALSE)

test_that("a resolved package is linked to its repository on the day it is seen", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  write_repo_tables(con, repos1, rp1, "2026-07-06")
  got <- .links(con)
  expect_equal(nrow(got), 1)
  expect_equal(got$package, "p1")
  expect_equal(got$first_seen, "2026-07-06")
  expect_equal(got$last_seen, "2026-07-06")
})

test_that("a delisted package keeps its link, frozen at the last day it resolved", {
  # repo_packages is rewritten from today's resolution, so it forgets the package
  # the day CRAN archives it, and every AI row on that repository is left with
  # nothing that names a package. The link table is what still does.
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  r1 <- "github.com/o/one"; r2 <- "github.com/o/two"
  write_repo_tables(con, rbind(.repo_row(r1), .repo_row(r2)),
                    rbind(.rp_row(r1, "pkgA"), .rp_row(r2, "pkgB")), "2026-09-01")
  write_repo_tables(con, .repo_row(r1), .rp_row(r1, "pkgA"), "2026-09-02")

  expect_equal(DBI::dbGetQuery(con, "SELECT package FROM repo_packages")$package, "pkgA")
  expect_equal(DBI::dbGetQuery(con, "SELECT status FROM repos WHERE repo_id = ?",
                               params = list(r2))$status, "retired")
  got <- .links(con)
  expect_equal(nrow(got), 2)
  b <- got[got$package == "pkgB", ]
  expect_equal(b$repo_id, r2)
  expect_equal(b$first_seen, "2026-09-01")
  expect_equal(b$last_seen, "2026-09-01")
  expect_equal(got$last_seen[got$package == "pkgA"], "2026-09-02")
})

test_that("a relisted package widens its link instead of starting a new one", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  r2 <- "github.com/o/two"
  write_repo_tables(con, .repo_row(r2), .rp_row(r2, "pkgB"), "2026-09-01")
  write_repo_tables(con, .repo_row(r2)[0, ], .rp_row(r2, "pkgB")[0, ], "2026-09-02")
  write_repo_tables(con, .repo_row(r2), .rp_row(r2, "pkgB"), "2026-09-03")
  got <- .links(con)
  expect_equal(nrow(got), 1)
  expect_equal(got$first_seen, "2026-09-01")
  expect_equal(got$last_seen, "2026-09-03")
})

test_that("a package whose URL moves keeps both links", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  r1 <- "github.com/o/one"; r3 <- "gitlab.com/o/three"
  write_repo_tables(con, .repo_row(r1), .rp_row(r1, "pkgA"), "2026-09-01")
  write_repo_tables(con, .repo_row(r3), .rp_row(r3, "pkgA"), "2026-09-02")
  got <- .links(con)
  expect_equal(got$repo_id, c(r1, r3))
  expect_equal(got$first_seen, c("2026-09-01", "2026-09-02"))
  expect_equal(got$last_seen, c("2026-09-01", "2026-09-02"))
  expect_equal(DBI::dbGetQuery(con, "SELECT repo_id FROM repo_packages")$repo_id, r3)
})

test_that("the same package under two origins is two links", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  r1 <- "github.com/o/one"
  write_repo_tables(con, .repo_row(r1), rbind(.rp_row(r1, "pkgA"), .rp_row(r1, "pkgA", "bioc")),
                    "2026-09-01")
  expect_equal(.links(con)$origin, c("bioc", "cran"))
})

test_that("an earlier run day never narrows a link", {
  # Dates arrive from whichever run writes them, and a run is not guaranteed to
  # be later than the one before it: a re-run of an older day, or the backfill
  # below, can report a day inside the window. Only widening moves anything.
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  r1 <- "github.com/o/one"
  write_repo_tables(con, .repo_row(r1), .rp_row(r1, "pkgA"), "2026-09-01")
  write_repo_tables(con, .repo_row(r1), .rp_row(r1, "pkgA"), "2026-09-05")
  write_repo_tables(con, .repo_row(r1), .rp_row(r1, "pkgA"), "2026-09-03")
  got <- .links(con)
  expect_equal(got$first_seen, "2026-09-01")
  expect_equal(got$last_seen, "2026-09-05")
})

.backfill <- function() data.frame(
  repo_id    = c("github.com/o/one", "github.com/gone/old", "github.com/o/one"),
  package    = c("pkgA", "oldpkg", "pkgMoved"),
  origin     = c("cran", "cran", "bioc"),
  first_seen = c("2026-08-01", "2026-08-01", "2026-08-04"),
  last_seen  = c("2026-08-20", "2026-08-09", "2026-08-30"),
  stringsAsFactors = FALSE)

test_that("the backfill adds the links no run can see any more and widens the rest", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  r1 <- "github.com/o/one"
  write_repo_tables(con, .repo_row(r1), .rp_row(r1, "pkgA"), "2026-09-15",
                    links_backfill = .backfill())
  got <- .links(con)
  expect_equal(nrow(got), 3)
  a <- got[got$package == "pkgA", ]
  expect_equal(a$first_seen, "2026-08-01")   # earlier sighting from the snapshots
  expect_equal(a$last_seen, "2026-09-15")    # today still wins the other end
  old <- got[got$package == "oldpkg", ]
  expect_equal(c(old$first_seen, old$last_seen), c("2026-08-01", "2026-08-09"))
  # repo_packages keeps meaning today's listed packages only.
  expect_equal(DBI::dbGetQuery(con, "SELECT package FROM repo_packages")$package, "pkgA")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM repos")$n, 1L)
})

test_that("applying the backfill twice gives the same table", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  r1 <- "github.com/o/one"
  write_repo_tables(con, .repo_row(r1), .rp_row(r1, "pkgA"), "2026-09-15",
                    links_backfill = .backfill())
  once <- .links(con)
  write_repo_tables(con, .repo_row(r1), .rp_row(r1, "pkgA"), "2026-09-15",
                    links_backfill = .backfill())
  expect_identical(.links(con), once)
})

test_that("a link table that was reset gets the backfilled rows back on the next run", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  r1 <- "github.com/o/one"
  write_repo_tables(con, .repo_row(r1), .rp_row(r1, "pkgA"), "2026-09-15",
                    links_backfill = .backfill())
  before <- .links(con)
  DBI::dbExecute(con, "DELETE FROM repo_package_links")
  write_repo_tables(con, .repo_row(r1), .rp_row(r1, "pkgA"), "2026-09-15",
                    links_backfill = .backfill())
  expect_identical(.links(con), before)
})
