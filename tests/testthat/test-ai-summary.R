test_that("build_signals_summary attaches ai rollups by repo_id, NULL when below threshold", {
  latest <- data.frame(repo_id="github.com/a/a", metric="stars", value=10L, stringsAsFactors=FALSE)
  series <- data.frame(repo_id=character(), date=character(), metric=character(), value=integer(),
                       stringsAsFactors=FALSE)
  repos <- data.frame(repo_id=c("github.com/a/a","github.com/b/b"), first_seen="2024-01-01",
                      last_seen="2024-06-01", last_commit_date=NA_character_, license=NA_character_,
                      topics=NA_character_, is_archived=0L, last_release_date=NA_character_,
                      median_days_between_releases=NA_integer_, stringsAsFactors=FALSE)
  rp <- data.frame(package=c("a","b"), origin="cran", repo_id=c("github.com/a/a","github.com/b/b"),
                   stringsAsFactors=FALSE)
  ai <- data.frame(repo_id="github.com/a/a", tool="claude", first_seen_date="2024-03-01",
                   first_seen_censored=0L, evidence_tiers="A", authored=1L,
                   last_confirmed_date="2025-01-01", stringsAsFactors=FALSE)
  out <- build_signals_summary(latest, series, repos, rp, "2024-06-01", ai_signals = ai)
  a <- out[out$package == "a", ]; b <- out[out$package == "b", ]
  expect_true(isTRUE(as.logical(a$ai_markers_detected)))
  expect_equal(a$ai_first_tool, "claude")
  expect_true(is.na(b$ai_markers_detected))        # below threshold -> NULL/NA, never FALSE
  expect_true(is.na(b$ai_first_tool))
})
