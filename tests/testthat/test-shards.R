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

test_that("export_summary_shard writes the three tables", {
  p <- tempfile(fileext = ".db"); on.exit(unlink(p))
  summ <- data.frame(package = "pkgA", origin = "cran", repo_id = "R", stars = 5L, forks = 1L,
    issues_open = 0L, prs_open = 0L, commits_total = 10L, releases_total = 0L,
    last_commit_date = "2026-07-01", license = "MIT", topics = "r", is_archived = 0L,
    trend_30d = NA_real_, first_seen = "2026-07-06", last_seen = "2026-07-06", stringsAsFactors = FALSE)
  repos <- data.frame(repo_id = "R", node_id = NA_character_, host = "github", host_domain = "github.com",
    owner = "o", name = "n", name_with_owner = "o/n", supported = 1L, n_packages = 1L,
    first_seen = "2026-07-06", last_seen = "2026-07-06", status = "active", stringsAsFactors = FALSE)
  rp <- data.frame(repo_id = "R", package = "pkgA", origin = "cran", resolved_from = "url", stringsAsFactors = FALSE)
  export_summary_shard(p, summ, repos, rp)
  c2 <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(c2), add = TRUE)
  expect_equal(DBI::dbGetQuery(c2, "SELECT COUNT(*) n FROM vcs_signals_summary")$n, 1)
  expect_equal(DBI::dbGetQuery(c2, "SELECT COUNT(*) n FROM repos")$n, 1)
  expect_equal(DBI::dbGetQuery(c2, "SELECT COUNT(*) n FROM repo_packages")$n, 1)
})
