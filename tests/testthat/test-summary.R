test_that("build_signals_summary fans repo out to packages with metric values", {
  latest <- data.frame(repo_id = "R", metric = c("stars", "forks", "issues_open", "prs_open", "commits_total", "releases_total"),
                       value = c(100L, 5L, 3L, 1L, 400L, 8L), stringsAsFactors = FALSE)
  series <- data.frame(repo_id = "R", date = c("2026-06-01", "2026-07-06"), metric = "stars",
                       value = c(80L, 100L), stringsAsFactors = FALSE)
  repos <- data.frame(repo_id = "R", last_commit_date = "2026-07-01", license = "MIT",
                      topics = "r", is_archived = 0L, first_seen = "2026-07-06", last_seen = "2026-07-06",
                      stringsAsFactors = FALSE)
  rp <- data.frame(repo_id = c("R", "R"), package = c("pkgA", "pkgB"), origin = c("cran", "bioc"),
                   stringsAsFactors = FALSE)
  s <- build_signals_summary(latest, series, repos, rp, "2026-07-06")
  expect_equal(nrow(s), 2)
  a <- s[s$package == "pkgA", ]
  expect_equal(a$stars, 100L); expect_equal(a$commits_total, 400L); expect_equal(a$license, "MIT")
  expect_equal(a$trend_30d, 25)     # (100-80)/80*100
})

test_that("build_signals_summary returns a typed empty frame for empty repo_packages", {
  empty_rp <- data.frame(repo_id = character(), package = character(), origin = character(), stringsAsFactors = FALSE)
  s <- build_signals_summary(data.frame(repo_id=character(), metric=character(), value=integer()),
                             data.frame(repo_id=character(), date=character(), metric=character(), value=integer()),
                             data.frame(repo_id=character()), empty_rp, "2026-07-06")
  expect_equal(nrow(s), 0)
  expect_true(all(c("package", "origin", "stars", "trend_30d") %in% names(s)))
})

test_that("the summary carries the seven new tables, after the link table and before the owner table", {
  new <- c("vcs_ai_repo_reads", "vcs_ai_account_counts", "vcs_ai_search_log",
           "vcs_ai_search_coverage", "vcs_ai_review_signals", "vcs_ai_outside_prs",
           "vcs_ai_ruleset_history")
  at <- match(new, SUMMARY_EXTRA_TABLES)
  expect_false(anyNA(at))
  expect_true(all(at > match("repo_package_links", SUMMARY_EXTRA_TABLES)))
  if ("vcs_dev_tooling_rules" %in% SUMMARY_EXTRA_TABLES)
    expect_true(all(at > match("vcs_dev_tooling_rules", SUMMARY_EXTRA_TABLES)))
  if ("vcs_repo_owner" %in% SUMMARY_EXTRA_TABLES) {
    expect_true(all(at < match("vcs_repo_owner", SUMMARY_EXTRA_TABLES)))
    expect_equal(tail(SUMMARY_EXTRA_TABLES, 1), "vcs_repo_owner")
  }
})

test_that("the first publish of a ruleset is dated once, with its change key when it has one", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  ensure_series_schema(con)
  keys <- c("2026-10-04" = "ungated-weekly-read")
  record_ruleset_history(con, "2026-10-06", "2026-10-04", keys)
  record_ruleset_history(con, "2026-10-13", "2026-10-04", keys)   # a later merge, same ruleset
  record_ruleset_history(con, "2026-11-02", "2026-11-01", keys)   # a later ruleset with no note
  got <- DBI::dbGetQuery(con, "SELECT * FROM vcs_ai_ruleset_history ORDER BY ruleset_version")
  expect_equal(got$first_published_on, c("2026-10-06", "2026-11-02"))
  expect_equal(got$change_key[1], "ungated-weekly-read")
  expect_true(is.na(got$change_key[2]))
})
