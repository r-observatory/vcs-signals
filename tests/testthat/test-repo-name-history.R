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

# ---- carried through every publish -------------------------------------------------

.nh_acquire <- function() data.frame(package = "ggplot2", origin = "cran",
  url_raw = "https://github.com/tidyverse/ggplot2", bugreports_raw = NA, stringsAsFactors = FALSE)
.nh_graphql <- function(query) {
  f <- if (grepl("followRenames", query)) "resolve_one.json"
       else if (grepl("history \\{ totalCount", query)) "commits.json" else "gauges_one.json"
  jsonlite::fromJSON(readLines(file.path("fixtures", f), warn = FALSE), simplifyVector = FALSE)
}

test_that("the daily run publishes the name history in both shards and dates when it began", {
  out <- tempfile("nh_out_"); dir.create(out)
  rel <- tempfile("nh_rel_"); dir.create(rel)
  suppressMessages(capture.output(
    run_update(local_release_io(rel, acquire = .nh_acquire, graphql = .nh_graphql), out,
               list(force_full = TRUE))))
  for (f in c("vcs-signals-summary.db", "vcs-signals-recent.db")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, f))
    got <- .nh_rows(con)
    idx <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type = 'index'
                                   AND tbl_name = 'vcs_repo_name_history'")$name
    DBI::dbDisconnect(con)
    expect_equal(got$node_id, "R_a", info = f)
    expect_equal(got$name_with_owner, "tidyverse/ggplot2", info = f)
    expect_equal(got$owner_node_id, "O_1", info = f)
    expect_equal(got$first_seen_exact, 0L, info = f)
    expect_true("ux_vrnh_open" %in% idx, info = f)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT value FROM pipeline_state WHERE key = ?",
                               params = list(NAME_HISTORY_SINCE_KEY))$value, format(Sys.Date()))
})

# A release as a publish by older code leaves it: the recent shard has lost the name
# history but still says since when it was published, and `prev` is what the
# previous summary on the release holds.
.nh_lost_release <- function(prev) {
  rel <- tempfile("nh_lost_"); dir.create(rel)
  mk <- function(name, rows = NULL, key = FALSE) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, name)); on.exit(DBI::dbDisconnect(con))
    ensure_repo_schema(con); ensure_series_schema(con)
    DBI::dbExecute(con, "DROP TABLE vcs_repo_name_history")
    if (!is.null(rows)) {
      DBI::dbExecute(con, "CREATE TABLE vcs_repo_name_history (node_id TEXT, episode_seq INTEGER,
        name_with_owner TEXT, owner_node_id TEXT, first_seen TEXT, last_seen TEXT,
        first_seen_exact INTEGER, ended_on TEXT)")
      DBI::dbWriteTable(con, "vcs_repo_name_history", rows, append = TRUE)
    }
    if (key) DBI::dbExecute(con, "INSERT INTO pipeline_state VALUES (?, '2026-10-02')",
                            params = list(NAME_HISTORY_SINCE_KEY))
  }
  mk("vcs-signals-recent.db", key = TRUE)
  mk("vcs-signals-summary.db")
  if (!is.null(prev)) mk("vcs-signals-summary-prev.db", prev)
  writeLines('{"summary":{"years":[]}}', file.path(rel, "manifest.json"))
  rel
}

test_that("a seed that lost the name history restores it from the previous summary", {
  saved <- data.frame(node_id = "R_1", episode_seq = 1:2, name_with_owner = c("o/old", "o/new"),
                      owner_node_id = "O_1", first_seen = c("2026-10-01", "2026-10-04"),
                      last_seen = c("2026-10-03", "2026-10-05"), first_seen_exact = 0:1,
                      ended_on = c("2026-10-04", NA), stringsAsFactors = FALSE)
  rel <- .nh_lost_release(saved)
  out <- tempfile("nh_seed_"); dir.create(out)
  work <- file.path(out, "_working.db")
  suppressMessages(seed_working_db(local_release_io(rel), out, work))
  con <- DBI::dbConnect(RSQLite::SQLite(), work); on.exit(DBI::dbDisconnect(con))
  expect_equal(.nh_rows(con)$episode_seq, 1:2)
})

test_that("with no copy left the seed stops rather than restart the name history", {
  rel <- .nh_lost_release(NULL)
  out <- tempfile("nh_seed_"); dir.create(out)
  expect_error(seed_working_db(local_release_io(rel), out, file.path(out, "_working.db")),
               "vcs_repo_name_history since 2026-10-02")
})
