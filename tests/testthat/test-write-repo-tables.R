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
