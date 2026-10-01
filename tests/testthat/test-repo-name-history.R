# vcs_repo_name_history dates each repository's name and owner from the daily gauge
# answer: one episode per name and owner, closed the day another is seen.

.nh_snap <- function(node_id, nwo, owner_node)
  data.frame(node_id = node_id, name_with_owner = nwo, owner_login = sub("/.*$", "", nwo),
             owner_type = "Organization", owner_node_id = owner_node, stars = 1L,
             stringsAsFactors = FALSE)
.nh_map <- function(repo_id, node_id)
  data.frame(repo_id = repo_id, node_id = node_id, stringsAsFactors = FALSE)
.nh_rows <- function(con)
  DBI::dbGetQuery(con, "SELECT * FROM vcs_repo_name_history ORDER BY node_id, episode_seq")
.nh_write <- function(con, snap, map, today) {
  res <- NULL
  utils::capture.output(res <- write_repo_owner(con, snap, map, today))
  res
}
.nh_log4r <- .nh_map("github.com/johnmyleswhite/log4r", "R_1")

test_that("a first sighting opens a censored episode, and later sightings extend it", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  .nh_write(con, .nh_snap("R_1", "johnmyleswhite/log4r", "U_jmw"), .nh_log4r, "2026-10-01")
  .nh_write(con, .nh_snap("R_1", "johnmyleswhite/log4r", "U_jmw"), .nh_log4r, "2026-10-03")
  got <- .nh_rows(con)
  expect_equal(nrow(got), 1L)
  expect_equal(got$episode_seq, 1L)
  expect_equal(c(got$first_seen, got$last_seen), c("2026-10-01", "2026-10-03"))
  expect_equal(got$first_seen_exact, 0L)
  expect_true(is.na(got$ended_on))
})

test_that("a rename closes the open episode and opens seq 2, dated exactly", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  .nh_write(con, .nh_snap("R_1", "johnmyleswhite/log4r", "U_jmw"), .nh_log4r, "2026-10-01")
  .nh_write(con, .nh_snap("R_1", "johnmyleswhite/log4r-archive", "U_jmw"), .nh_log4r, "2026-10-02")
  got <- .nh_rows(con)
  expect_equal(got$episode_seq, 1:2)
  expect_equal(got$name_with_owner, c("johnmyleswhite/log4r", "johnmyleswhite/log4r-archive"))
  expect_equal(got$ended_on, c("2026-10-02", NA))
  expect_equal(got$last_seen, c("2026-10-01", "2026-10-02"))
  expect_equal(got$first_seen_exact, c(0L, 1L))
})

test_that("a transfer changes the owner and opens the next episode", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  .nh_write(con, .nh_snap("R_1", "johnmyleswhite/log4r", "U_jmw"), .nh_log4r, "2026-10-01")
  .nh_write(con, .nh_snap("R_1", "r-lib/log4r", "O_rlib"), .nh_log4r, "2026-10-02")
  .nh_write(con, .nh_snap("R_1", "r-lib/log4r", "O_rlib"), .nh_log4r, "2026-10-04")
  got <- .nh_rows(con)
  expect_equal(got$owner_node_id, c("U_jmw", "O_rlib"))
  expect_equal(got$first_seen, c("2026-10-01", "2026-10-02"))
  expect_equal(got$last_seen, c("2026-10-01", "2026-10-04"))
  expect_equal(got$ended_on, c("2026-10-02", NA))
})

test_that("a change of case alone is a rename", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  map <- .nh_map("github.com/usgs-r/dataretrieval", "R_d")
  .nh_write(con, .nh_snap("R_d", "DOI-USGS/dataretrieval", "O_usgs"), map, "2026-10-01")
  .nh_write(con, .nh_snap("R_d", "DOI-USGS/dataRetrieval", "O_usgs"), map, "2026-10-02")
  expect_equal(.nh_rows(con)$episode_seq, 1:2)
})

test_that("a repository the answer left out keeps its open episode untouched", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  map <- rbind(.nh_log4r, .nh_map("github.com/o/other", "R_2"))
  .nh_write(con, rbind(.nh_snap("R_1", "johnmyleswhite/log4r", "U_jmw"), .nh_snap("R_2", "o/other", "O_1")),
            map, "2026-10-01")
  .nh_write(con, .nh_snap("R_1", "johnmyleswhite/log4r", "U_jmw"), map, "2026-10-02")
  got <- .nh_rows(con)
  two <- got[got$node_id == "R_2", ]
  expect_equal(nrow(two), 1L)
  expect_equal(two$last_seen, "2026-10-01")
  expect_true(is.na(two$ended_on))
})

test_that("two repo_ids on one node write one episode", {
  # jpstat and japanstat are one repository, so its node is queried twice.
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  map <- rbind(.nh_map("github.com/o/jpstat", "R_j"), .nh_map("github.com/o/japanstat", "R_j"))
  snap <- rbind(.nh_snap("R_j", "o/jpstat", "O_1"), .nh_snap("R_j", "o/jpstat", "O_1"))
  expect_no_error(.nh_write(con, snap, map, "2026-10-01"))
  expect_equal(nrow(.nh_rows(con)), 1L)
})

test_that("a node answered with no owner or name writes no episode", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  snap <- .nh_snap("R_1", "johnmyleswhite/log4r", NA_character_)
  .nh_write(con, snap, .nh_log4r, "2026-10-01")
  expect_equal(nrow(.nh_rows(con)), 0L)
})

test_that("a node can hold only one open episode", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  ins <- function(seq) DBI::dbExecute(con, "INSERT INTO vcs_repo_name_history VALUES
    ('R_1', ?, 'a/b', 'O_1', '2026-10-01', '2026-10-01', 0, NULL)", params = list(seq))
  ins(1L)
  expect_error(ins(2L), "UNIQUE constraint failed")
})

test_that("the owner rows and the name episodes are written in one transaction", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "DROP TABLE vcs_repo_name_history")
  expect_error(.nh_write(con, .nh_snap("R_1", "johnmyleswhite/log4r", "U_jmw"), .nh_log4r, "2026-10-01"))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM vcs_repo_owner")$n, 0L)
})
