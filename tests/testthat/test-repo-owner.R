# vcs_repo_owner: the current owner of every active GitHub repository, rebuilt
# by the daily run from the gauge query.

.today <- "2026-09-25"

.put_owner <- function(con, repo_id, node_id, login, type, owner_node, nwo, observed_on)
  DBI::dbExecute(con, "INSERT INTO vcs_repo_owner
    (repo_id, node_id, owner_login_current, owner_type, owner_node_id,
     name_with_owner_current, observed_on) VALUES (?,?,?,?,?,?,?)",
    params = list(repo_id, node_id, login, type, owner_node, nwo, observed_on))

.owner_rows <- function(con)
  DBI::dbGetQuery(con, "SELECT * FROM vcs_repo_owner ORDER BY repo_id")

test_that("the owner table is keyed by repo_id, typed, and indexed for the readers", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  sql <- DBI::dbGetQuery(con,
    "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = 'vcs_repo_owner'")$sql
  expect_length(sql, 1L)
  expect_match(sql, "WITHOUT ROWID", fixed = TRUE)
  idx <- DBI::dbGetQuery(con,
    "SELECT name, sql FROM sqlite_master WHERE type = 'index' AND tbl_name = 'vcs_repo_owner'")
  expect_setequal(idx$name, c("idx_vro_login", "idx_vro_owner_node", "idx_vro_node"))
  expect_match(idx$sql[idx$name == "idx_vro_login"], "COLLATE NOCASE", fixed = TRUE)
  .put_owner(con, "github.com/o/r", "R_1", "o", "Organization", "O_1", "o/r", .today)
  expect_equal(.owner_rows(con)$owner_type, "Organization")
  expect_error(.put_owner(con, "github.com/o/s", "R_2", "o", "Mannequin", "O_1", "o/s", .today),
               "CHECK constraint failed")
})

test_that("every publish carries the owner table, as the last of the extra tables", {
  expect_identical(tail(SUMMARY_EXTRA_TABLES, 1L), "vcs_repo_owner")
})

# ---- the publish gate --------------------------------------------------------

.mk_owner_summary <- function(n, with_table = TRUE) {
  path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  if (!with_table) {
    DBI::dbExecute(con, "DROP TABLE vcs_repo_owner")
    return(path)
  }
  if (n > 0) DBI::dbWriteTable(con, "vcs_repo_owner", data.frame(
    repo_id = sprintf("github.com/o/r%04d", seq_len(n)), node_id = sprintf("R_%d", seq_len(n)),
    owner_login_current = "o", owner_type = "Organization", owner_node_id = "O_1",
    name_with_owner_current = sprintf("o/r%04d", seq_len(n)), observed_on = .today,
    stringsAsFactors = FALSE), append = TRUE)
  path
}

test_that("the first publish that carries the owner table is not compared", {
  expect_equal(summary_regressions(.mk_owner_summary(0, with_table = FALSE),
                                   .mk_owner_summary(100)), character(0))
  expect_equal(summary_regressions(.mk_owner_summary(0), .mk_owner_summary(100)), character(0))
})

test_that("an owner table that lost 5% of its rows is refused and one that lost 1% is not", {
  prev <- .mk_owner_summary(100)
  expect_match(paste(summary_regressions(prev, .mk_owner_summary(95)), collapse = " "),
               "vcs_repo_owner: 95 rows, was 100", fixed = TRUE)
  expect_equal(summary_regressions(prev, .mk_owner_summary(99)), character(0))
})
