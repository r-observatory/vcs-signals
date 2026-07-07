test_that("extract_year_rows and extract_recent_rows filter by date", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSERT INTO signals_series VALUES ('R','2025-05-01','stars',10),('R','2026-07-01','stars',20)")
  expect_equal(nrow(extract_year_rows(con, 2025)), 1)
  expect_equal(extract_recent_rows(con, as.Date("2026-07-06"), 400)$value, 20)
})

test_that("export_series_shard round-trips", {
  rows <- data.frame(repo_id = "R", date = "2026-07-01", metric = "stars", value = 20L, stringsAsFactors = FALSE)
  p <- tempfile(fileext = ".db"); on.exit(unlink(p))
  export_series_shard(p, rows)
  c2 <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(c2), add = TRUE)
  expect_equal(DBI::dbGetQuery(c2, "SELECT value FROM signals_series")$value, 20)
})

test_that("write_manifest preserves empty changed_shards as []", {
  p <- tempfile(fileext = ".json"); on.exit(unlink(p))
  write_manifest(p, character(0), "v1", list(source_kind = "live"))
  txt <- paste(readLines(p), collapse = "")
  expect_match(txt, '"changed_shards"\\s*:\\s*\\[\\]')
})
