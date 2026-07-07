# In-memory DB with the SP1 schema applied, for persistence tests.
new_test_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  ensure_repo_schema(con)
  ensure_series_schema(con)
  con
}
