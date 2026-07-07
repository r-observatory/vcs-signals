# In-memory DB with the SP1 schema applied, for persistence tests.
new_test_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  ensure_repo_schema(con)
  ensure_series_schema(con)
  con
}

# Auto-sourced by testthat before any test file runs. At this point the
# working directory is tests/testthat, so scripts/update.R lives two levels
# up. update.R's own top-level source() calls (config.R/helpers.R/github.R)
# use paths relative to the repo root, so temporarily chdir there while
# sourcing it, then restore the tests/testthat cwd the other test files rely
# on (their fixture paths are relative to tests/testthat).
.repo_root <- normalizePath(file.path(getwd(), "..", ".."))
.orig_wd <- setwd(.repo_root)
source(file.path(.repo_root, "scripts", "update.R"))
setwd(.orig_wd)
