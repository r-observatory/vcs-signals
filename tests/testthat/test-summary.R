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
