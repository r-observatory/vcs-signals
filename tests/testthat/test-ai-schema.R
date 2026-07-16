test_that("ensure_series_schema creates vcs_ai_signals with the (repo_id, tool) PK", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  ensure_series_schema(con)
  expect_true(DBI::dbExistsTable(con, "vcs_ai_signals"))
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(vcs_ai_signals)")
  expect_setequal(cols$name, c("repo_id","tool","first_seen_date","first_seen_censored",
                               "evidence_tiers","authored","last_confirmed_date"))
  pk <- cols$name[cols$pk > 0][order(cols$pk[cols$pk > 0])]
  expect_equal(pk, c("repo_id","tool"))
})
