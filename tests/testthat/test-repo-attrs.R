# The summary's per-repository values, which every summary builder carries for a
# repository its run did not collect.

.attr_row <- function(repo_id, ...) {
  row <- data.frame(repo_id = repo_id, license = "MIT", topics = "r", is_archived = 0L,
                    last_commit_date = "2026-04-01T10:00:00Z", last_release_date = "2026-03-01T09:00:00Z",
                    last_release_tag = "v1.0.0", repo_created_at = "2015-02-03T04:05:06Z",
                    median_days_between_releases = 45L, stringsAsFactors = FALSE)
  over <- list(...)
  for (nm in names(over)) row[[nm]] <- over[[nm]]
  row
}

test_that("every carried column is a summary column, the two new ones included", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  have <- DBI::dbGetQuery(con, "PRAGMA table_info(vcs_signals_summary)")$name
  expect_true(all(REPO_ATTR_COLS %in% have))
  expect_true(all(c("last_release_tag", "repo_created_at") %in% REPO_ATTR_COLS))
})

test_that("prior_repo_attrs reads one typed row per repository", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  row <- .attr_row("github.com/a/one")
  DBI::dbWriteTable(con, "vcs_signals_summary",
    cbind(package = c("p1", "p2"), origin = "cran", rbind(row, row), stringsAsFactors = FALSE),
    append = TRUE)
  got <- prior_repo_attrs(con)
  expect_equal(nrow(got), 1L)
  expect_equal(names(got), c("repo_id", REPO_ATTR_COLS))
  expect_equal(got$last_release_tag, "v1.0.0")
  expect_type(got$is_archived, "integer")
  expect_type(got$median_days_between_releases, "integer")
})

test_that("a summary written before the new columns existed reads them as NA", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE vcs_signals_summary (package TEXT, origin TEXT, repo_id TEXT,
    license TEXT, topics TEXT, is_archived INTEGER, last_commit_date TEXT, last_release_date TEXT,
    median_days_between_releases INTEGER)")
  DBI::dbExecute(con, "INSERT INTO vcs_signals_summary VALUES
    ('p1', 'cran', 'github.com/a/one', 'MIT', 'r', 0, '2026-07-07', '2026-07-07', 30)")
  got <- prior_repo_attrs(con)
  expect_equal(got$license, "MIT")
  expect_equal(got$median_days_between_releases, 30L)
  expect_true(is.na(got$last_release_tag))
  expect_true(is.na(got$repo_created_at))
  expect_type(got$last_release_tag, "character")
})

test_that("a collected repository takes this run's values and keeps its cadence; another keeps its row", {
  prior <- rbind(.attr_row("github.com/a/one"), .attr_row("github.com/b/two"))
  fresh <- .attr_row("github.com/a/one", license = "GPL-3", last_commit_date = "2026-09-29T08:00:00Z",
                     last_release_date = "2026-04-09T15:21:33Z", last_release_tag = "v3.6.6",
                     median_days_between_releases = NA_integer_)
  got <- carry_repo_attrs(fresh, prior)
  one <- got[got$repo_id == "github.com/a/one", ]
  expect_equal(one$license, "GPL-3")
  expect_equal(one$last_commit_date, "2026-09-29T08:00:00Z")
  expect_equal(one$last_release_date, "2026-04-09T15:21:33Z")
  expect_equal(one$last_release_tag, "v3.6.6")
  expect_equal(one$median_days_between_releases, 45L)
  two <- got[got$repo_id == "github.com/b/two", ]
  expect_equal(unname(unlist(two[REPO_ATTR_COLS])), unname(unlist(prior[2, REPO_ATTR_COLS])))
})

test_that("a collected repository with no release loses a carried release date", {
  prior <- .attr_row("github.com/a/one", last_release_date = "2026-07-07", last_release_tag = NA_character_)
  fresh <- .attr_row("github.com/a/one", last_release_date = NA_character_, last_release_tag = NA_character_)
  got <- carry_repo_attrs(fresh, prior)
  expect_true(is.na(got$last_release_date))
  expect_true(is.na(got$last_release_tag))
})

test_that("with the reset, a repository not collected loses only its two dates", {
  prior <- rbind(.attr_row("github.com/a/one"), .attr_row("github.com/b/two"))
  got <- carry_repo_attrs(.attr_row("github.com/a/one"), prior, reset = TRUE)
  two <- got[got$repo_id == "github.com/b/two", ]
  expect_true(is.na(two$last_commit_date))
  expect_true(is.na(two$last_release_date))
  expect_equal(two$last_release_tag, "v1.0.0")
  expect_equal(two$license, "MIT")
  expect_equal(two$median_days_between_releases, 45L)
  expect_equal(got$last_commit_date[got$repo_id == "github.com/a/one"], "2026-04-01T10:00:00Z")
})
