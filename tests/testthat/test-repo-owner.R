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

# ---- write rules ---------------------------------------------------------------

.owner_map <- function(repo_id, node_id)
  data.frame(repo_id = repo_id, node_id = node_id, stringsAsFactors = FALSE)

.owner_snapshot <- function(node_id, name_with_owner, owner_login, owner_type, owner_node_id)
  data.frame(node_id = node_id, name_with_owner = name_with_owner, owner_login = owner_login,
             owner_type = owner_type, owner_node_id = owner_node_id, stars = 1L,
             stringsAsFactors = FALSE)

.quiet_write <- function(...) {
  res <- NULL
  utils::capture.output(res <- write_repo_owner(...))
  res
}

test_that("a repository's owner is written with the day it was seen", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  res <- .quiet_write(con,
    .owner_snapshot("R_a", "tidyverse/ggplot2", "tidyverse", "Organization", "O_1"),
    .owner_map("github.com/tidyverse/ggplot2", "R_a"), .today)
  got <- .owner_rows(con)
  expect_equal(nrow(got), 1L)
  expect_equal(got$repo_id, "github.com/tidyverse/ggplot2")
  expect_equal(got$node_id, "R_a")
  expect_equal(got$owner_login_current, "tidyverse")
  expect_equal(got$owner_type, "Organization")
  expect_equal(got$owner_node_id, "O_1")
  expect_equal(got$name_with_owner_current, "tidyverse/ggplot2")
  expect_equal(got$observed_on, .today)
  expect_equal(res$written, 1L)
})

test_that("a transferred repository keeps its repo_id and is stored under its new owner", {
  # sbfnk/rbi now resolves to epiforecasts/rbi, while the DESCRIPTION URL still names sbfnk.
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  rid <- "github.com/sbfnk/rbi"; node <- "MDEwOlJlcG9zaXRvcnkxMDI0MzMwOA=="
  .put_owner(con, rid, node, "sbfnk", "User", "U_sbfnk", "sbfnk/rbi", "2026-09-20")
  .quiet_write(con,
    .owner_snapshot(node, "epiforecasts/rbi", "epiforecasts", "Organization",
                    "MDEyOk9yZ2FuaXphdGlvbjU0Njc1MDk0"),
    .owner_map(rid, node), .today)
  got <- .owner_rows(con)
  expect_equal(nrow(got), 1L)
  expect_equal(got$repo_id, rid)
  expect_equal(got$owner_login_current, "epiforecasts")
  expect_equal(got$owner_type, "Organization")
  expect_equal(got$owner_node_id, "MDEyOk9yZ2FuaXphdGlvbjU0Njc1MDk0")
  expect_equal(got$name_with_owner_current, "epiforecasts/rbi")
  expect_equal(got$observed_on, .today)
})

test_that("the login and the name are stored in GitHub's case, not the repo_id's", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  .quiet_write(con,
    .owner_snapshot("R_d", "DOI-USGS/dataRetrieval", "DOI-USGS", "Organization", "O_usgs"),
    .owner_map("github.com/usgs-r/dataretrieval", "R_d"), .today)
  got <- .owner_rows(con)
  expect_equal(got$repo_id, "github.com/usgs-r/dataretrieval")
  expect_equal(got$owner_login_current, "DOI-USGS")
  expect_equal(got$name_with_owner_current, "DOI-USGS/dataRetrieval")
  expect_equal(DBI::dbGetQuery(con, "SELECT repo_id FROM vcs_repo_owner
    WHERE owner_login_current = 'doi-usgs' COLLATE NOCASE")$repo_id, got$repo_id)
})

test_that("two repo_ids on one repository both get a row, even when the query returns the node twice", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  node <- "MDEwOlJlcG9zaXRvcnk4NjA1Njc="
  sn <- .owner_snapshot(node, "r-lib/log4r", "r-lib", "Organization", "O_rlib")
  res <- .quiet_write(con, rbind(sn, sn),
    .owner_map(c("github.com/johnmyleswhite/log4r", "github.com/r-lib/log4r"), c(node, node)),
    .today)
  got <- .owner_rows(con)
  expect_equal(got$repo_id, c("github.com/johnmyleswhite/log4r", "github.com/r-lib/log4r"))
  expect_equal(unique(got$node_id), node)
  expect_equal(unique(got$owner_login_current), "r-lib")
  expect_equal(unique(got$name_with_owner_current), "r-lib/log4r")
  expect_equal(res$written, 2L)
})

test_that("a null owner or an unknown owner type is not written, and both are counted", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  .put_owner(con, "github.com/o/a", "R_a", "o", "Organization", "O_1", "o/a", "2026-09-22")
  sn <- rbind(.owner_snapshot("R_a", "o/a", NA_character_, NA_character_, NA_character_),
              .owner_snapshot("R_b", "o/b", "o", "Enterprise", "E_1"))
  expect_output(res <- write_repo_owner(con, sn,
    .owner_map(c("github.com/o/a", "github.com/o/b"), c("R_a", "R_b")), .today),
    "1 with no owner returned, 1 with another owner type")
  got <- .owner_rows(con)
  expect_equal(got$repo_id, "github.com/o/a")
  expect_equal(got$observed_on, "2026-09-22")
  expect_equal(res$written, 0L)
  expect_equal(res$no_owner, 1L)
  expect_equal(res$other_type, 1L)
})

test_that("a repo_id that is no longer an active GitHub repository loses its row", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  .put_owner(con, "github.com/o/gone", "R_g", "o", "Organization", "O_1", "o/gone", "2026-09-24")
  res <- .quiet_write(con, .owner_snapshot("R_a", "o/a", "o", "Organization", "O_1"),
                      .owner_map("github.com/o/a", "R_a"), .today)
  expect_equal(.owner_rows(con)$repo_id, "github.com/o/a")
  expect_equal(res$removed, 1L)
})

test_that("a repository the gauge query missed keeps its row and is counted", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  .put_owner(con, "github.com/o/kept", "R_k", "o", "Organization", "O_1", "o/kept", "2026-09-23")
  expect_output(res <- write_repo_owner(con,
    .owner_snapshot("R_a", "o/a", "o", "Organization", "O_1"),
    .owner_map(c("github.com/o/a", "github.com/o/kept"), c("R_a", "R_k")), .today),
    "1 not collected this run")
  got <- .owner_rows(con)
  expect_equal(got$repo_id, c("github.com/o/a", "github.com/o/kept"))
  expect_equal(got$observed_on[got$repo_id == "github.com/o/kept"], "2026-09-23")
  expect_equal(res$not_collected, 1L)
})

test_that("an owner row outlives its last sighting by 14 days and no more", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  expect_equal(OWNER_STALE_DAYS, 14L)
  .put_owner(con, "github.com/o/d14", "R_14", "o", "Organization", "O_1", "o/d14", "2026-09-11")
  .put_owner(con, "github.com/o/d15", "R_15", "o", "Organization", "O_1", "o/d15", "2026-09-10")
  res <- .quiet_write(con, .owner_snapshot("R_a", "o/a", "o", "Organization", "O_1"),
    .owner_map(c("github.com/o/a", "github.com/o/d14", "github.com/o/d15"), c("R_a", "R_14", "R_15")),
    .today)
  expect_equal(.owner_rows(con)$repo_id, c("github.com/o/a", "github.com/o/d14"))
  expect_equal(res$removed, 1L)
})

test_that("an error part way through leaves the prior rows exactly as they were", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  .put_owner(con, "github.com/o/a", "R_a", "o", "Organization", "O_1", "o/a", "2026-09-20")
  .put_owner(con, "github.com/o/gone", "R_g", "o", "Organization", "O_1", "o/gone", "2026-09-20")
  before <- .owner_rows(con)
  # The upsert succeeds; the delete of the retired repo_id then fails.
  DBI::dbExecute(con, "CREATE TRIGGER vro_refuse BEFORE DELETE ON vcs_repo_owner
    BEGIN SELECT RAISE(ABORT, 'delete refused'); END")
  expect_error(utils::capture.output(write_repo_owner(con,
    rbind(.owner_snapshot("R_a", "o2/a", "o2", "User", "U_2"),
          .owner_snapshot("R_n", "o/new", "o", "Organization", "O_1")),
    .owner_map(c("github.com/o/a", "github.com/o/new"), c("R_a", "R_n")), .today)),
    "delete refused")
  expect_equal(.owner_rows(con), before)
  expect_no_error({ DBI::dbBegin(con); DBI::dbRollback(con) })
})

test_that("an organization rename rewrites the repositories the query returned, and a missed one keeps the old login on the same owner node", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  .put_owner(con, "github.com/oldorg/a", "R_a", "oldorg", "Organization", "O_9", "oldorg/a", "2026-09-24")
  .put_owner(con, "github.com/oldorg/b", "R_b", "oldorg", "Organization", "O_9", "oldorg/b", "2026-09-24")
  .quiet_write(con, .owner_snapshot("R_a", "neworg/a", "neworg", "Organization", "O_9"),
    .owner_map(c("github.com/oldorg/a", "github.com/oldorg/b"), c("R_a", "R_b")), .today)
  got <- .owner_rows(con)
  expect_equal(got$owner_login_current, c("neworg", "oldorg"))
  expect_equal(got$name_with_owner_current, c("neworg/a", "oldorg/b"))
  expect_equal(got$observed_on, c(.today, "2026-09-24"))
  expect_equal(unique(got$owner_node_id), "O_9")
})

test_that("an account converted to an organization is stored with its new type and node, and a missed repository keeps the old ones", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  .put_owner(con, "github.com/lab/a", "R_a", "lab", "User", "U_lab", "lab/a", "2026-09-24")
  .put_owner(con, "github.com/lab/b", "R_b", "lab", "User", "U_lab", "lab/b", "2026-09-24")
  .quiet_write(con, .owner_snapshot("R_a", "lab/a", "lab", "Organization", "O_lab"),
    .owner_map(c("github.com/lab/a", "github.com/lab/b"), c("R_a", "R_b")), .today)
  got <- .owner_rows(con)
  expect_equal(got$owner_type, c("Organization", "User"))
  expect_equal(got$owner_node_id, c("O_lab", "U_lab"))
  expect_equal(got$owner_login_current, c("lab", "lab"))
  expect_equal(got$observed_on, c(.today, "2026-09-24"))
})

test_that("a second run on the same day with the same answer leaves the table as it was", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  sn <- .owner_snapshot(c("R_a", "R_b"), c("o/a", "u/b"), c("o", "u"), c("Organization", "User"),
                        c("O_1", "U_1"))
  map <- .owner_map(c("github.com/o/a", "github.com/u/b"), c("R_a", "R_b"))
  .quiet_write(con, sn, map, .today)
  first <- .owner_rows(con)
  res <- .quiet_write(con, sn, map, .today)
  expect_identical(.owner_rows(con), first)
  expect_equal(res$written, 2L)
  expect_equal(res$removed, 0L)
})

test_that("the first run after three weeks without one keeps every repository it reached", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  old <- format(as.Date(.today) - 21)
  for (x in c("a", "b", "c"))
    .put_owner(con, paste0("github.com/o/", x), paste0("R_", x), "o", "Organization", "O_1",
               paste0("o/", x), old)
  res <- .quiet_write(con,
    .owner_snapshot(c("R_a", "R_b"), c("o/a", "o/b"), "o", "Organization", "O_1"),
    .owner_map(c("github.com/o/a", "github.com/o/b", "github.com/o/c"), c("R_a", "R_b", "R_c")),
    .today)
  got <- .owner_rows(con)
  expect_equal(got$repo_id, c("github.com/o/a", "github.com/o/b"))
  expect_equal(got$observed_on, c(.today, .today))
  expect_equal(res$not_collected, 1L)
  expect_equal(res$removed, 1L)
})

# ---- the publish gate's rule for the owner table ----------------------------------

# A summary whose repos table lists r0001 to r0100 as active GitHub repositories with a node
# id (those in `retired` as retired), and whose owner rows are `owner_ids`, seen on `observed_on`.
.mk_owner_gate <- function(owner_ids, observed_on, retired = integer(0), with_table = TRUE) {
  ids <- 1:100
  path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbWriteTable(con, "repos", data.frame(
    repo_id = sprintf("github.com/o/r%04d", ids), node_id = sprintf("R_%d", ids), host = "github",
    host_domain = "github.com", owner = "o", name = sprintf("r%04d", ids),
    name_with_owner = sprintf("o/r%04d", ids), supported = 1L, n_packages = 1L,
    first_seen = "2026-01-01", last_seen = .today,
    status = ifelse(ids %in% retired, "retired", "active"), stringsAsFactors = FALSE), append = TRUE)
  if (!with_table) {
    DBI::dbExecute(con, "DROP TABLE vcs_repo_owner")
  } else if (length(owner_ids)) {
    DBI::dbWriteTable(con, "vcs_repo_owner", data.frame(
      repo_id = sprintf("github.com/o/r%04d", owner_ids), node_id = sprintf("R_%d", owner_ids),
      owner_login_current = "o", owner_type = "Organization", owner_node_id = "O_1",
      name_with_owner_current = sprintf("o/r%04d", owner_ids), observed_on = observed_on,
      stringsAsFactors = FALSE), append = TRUE)
  }
  path
}
.yesterday <- "2026-09-24"
.expired <- "2026-09-10"   # 15 days before .today, so the writer's cutoff of 2026-09-11 removed it

test_that("an owner table on one side only is not compared", {
  with <- .mk_owner_gate(1:100, .yesterday)
  without <- .mk_owner_gate(integer(0), .today, with_table = FALSE)
  expect_equal(summary_regressions(without, with), character(0))
  expect_equal(summary_regressions(with, without), character(0))
})

test_that("an emptied owner table is refused", {
  r <- summary_regressions(.mk_owner_gate(1:100, .yesterday), .mk_owner_gate(integer(0), .today))
  expect_match(paste(r, collapse = " "), "vcs_repo_owner: published empty, was 100 rows", fixed = TRUE)
})

test_that("an owner row whose observed_on moved back is refused", {
  prev <- .mk_owner_gate(1:100, .today)
  r <- summary_regressions(prev, .mk_owner_gate(1:100, rep(c(.today, .yesterday), c(99, 1))))
  expect_match(paste(r, collapse = " "),
               "vcs_repo_owner: 1 row(s) had observed_on moved earlier: github.com/o/r0100", fixed = TRUE)
})

test_that("5% of the owner rows gone, each unseen for more than 14 days, is accepted", {
  prev <- .mk_owner_gate(1:100, rep(c(.yesterday, .expired), c(95, 5)))
  expect_equal(summary_regressions(prev, .mk_owner_gate(1:95, .today)), character(0))
  # The writer keeps a row seen exactly 14 days before, so its loss is not expiry.
  kept <- .mk_owner_gate(1:100, rep(c(.yesterday, "2026-09-11"), c(95, 5)))
  expect_match(paste(summary_regressions(kept, .mk_owner_gate(1:95, .today)), collapse = " "),
               "vcs_repo_owner: 5 row(s) are gone", fixed = TRUE)
})

test_that("5% of the owner rows gone because their repositories are retired is accepted", {
  prev <- .mk_owner_gate(1:100, .yesterday)
  expect_equal(summary_regressions(prev, .mk_owner_gate(1:95, .today, retired = 96:100)), character(0))
})

test_that("one active owner row seen within 14 days that leaves is refused and named", {
  prev <- .mk_owner_gate(1:100, .yesterday)
  r <- paste(summary_regressions(prev, .mk_owner_gate(setdiff(1:100, 7L), .today)), collapse = " ")
  expect_match(r, paste0("vcs_repo_owner: 1 row(s) are gone while the repository is still active ",
                         "and was seen within 14 days: github.com/o/r0007"), fixed = TRUE)
  four <- paste(summary_regressions(prev, .mk_owner_gate(5:100, .today)), collapse = " ")
  expect_match(four, "github.com/o/r0001, github.com/o/r0002, github.com/o/r0003, and 1 more", fixed = TRUE)
})

# Runs the statements against a gate summary in place and returns its path.
.alter_gate <- function(path, ...) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  for (q in c(...)) DBI::dbExecute(con, q)
  path
}
.owner_reasons <- function(prev, nxt)
  grep("^vcs_repo_owner:", summary_regressions(prev, nxt), value = TRUE)

test_that("an owner row may leave when its repository has no node id, is not on GitHub or is not in repos", {
  prev <- .mk_owner_gate(1:100, .yesterday)
  for (q in c("UPDATE repos SET node_id = NULL WHERE repo_id = 'github.com/o/r0007'",
              "UPDATE repos SET host = 'gitlab' WHERE repo_id = 'github.com/o/r0007'",
              "DELETE FROM repos WHERE repo_id = 'github.com/o/r0007'"))
    expect_equal(.owner_reasons(prev, .alter_gate(.mk_owner_gate(setdiff(1:100, 7L), .today), q)),
                 character(0), info = q)
})

test_that("a repos table without status explains no lost row, and an owner table without observed_on is refused", {
  prev <- .mk_owner_gate(1:100, .yesterday)
  no_status <- .alter_gate(.mk_owner_gate(1:95, .today, retired = 96:100),
                           "ALTER TABLE repos DROP COLUMN status")
  expect_match(paste(.owner_reasons(prev, no_status), collapse = " "),
               paste0("vcs_repo_owner: 5 row(s) are gone while the repository is still active ",
                      "and was seen within 14 days: github.com/o/r0096, github.com/o/r0097, ",
                      "github.com/o/r0098, and 2 more"), fixed = TRUE)
  no_date <- .alter_gate(.mk_owner_gate(1:100, .today), "DROP TABLE vcs_repo_owner",
                         "CREATE TABLE vcs_repo_owner (repo_id TEXT)",
                         "INSERT INTO vcs_repo_owner VALUES ('github.com/o/r0001')")
  expect_equal(.owner_reasons(prev, no_date),
               paste0("vcs_repo_owner: published without observed_on, so the gate cannot tell ",
                      "which owner rows were kept"))
})
