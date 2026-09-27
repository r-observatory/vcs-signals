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

.cov_log <- function(repo, key, outcome, rev = 1L, on = "2026-10-05")
  data.frame(repo_id = repo, rule_key = key, rule_rev = rev, ruleset_version = AI_RULESET_VERSION,
             asked_on = on, outcome = outcome, total_count = if (outcome == "refused") NA_integer_ else 1L,
             verified = NA_integer_, incomplete = 0L, first_hit_on = NA_character_, source = "search",
             stringsAsFactors = FALSE)

test_that("coverage counts the repositories each search reached, apart from whole-history reads", {
  log <- rbind(.cov_log("r1", "msg.claude.coauthor", "hit"), .cov_log("r2", "msg.claude.coauthor", "none"),
               .cov_log("r3", "msg.claude.coauthor", "refused"),
               .cov_log("r4", "msg.claude.coauthor", "none", rev = 0L),
               .cov_log("r6", "msg.claude.coauthor", "none"),
               .cov_log("r5", "author.noreply@anthropic.com", "hit", on = "2026-10-06"))
  reads <- .ai_bind_like(.ai_empty_reads(), list(data.frame(
    repo_id = c("r6", "r7"), commits_history_complete = 1L,
    commits_ruleset = c(AI_RULESET_VERSION, "2026-01-01"), stringsAsFactors = FALSE)))
  cov <- build_search_coverage(log, reads)
  cc <- cov[cov$rule_key == "msg.claude.coauthor", ]
  expect_equal(c(cc$repos_asked, cc$repos_hit, cc$repos_refused, cc$repos_read_whole), c(2L, 1L, 1L, 1L))
  au <- cov[cov$rule_key == "author.claude", ]
  expect_equal(c(au$repos_asked, au$repos_read_whole), c(1L, 1L))
  expect_equal(au$last_asked_on, "2026-10-06")
  expect_setequal(cov$rule_key, .ai_coverage_rules()$rule_key)
  expect_equal(cov$tool[cov$rule_key == "msg.any.assisted-by"], "any")
  expect_equal(cov$channel[cov$rule_key == "name.aider.suffix"], "commit-author-name")
  expect_equal(cov$channel[cov$rule_key == "review.copilot.suggestion"], "review-credit")
  never <- cov[cov$rule_key == "msg.opencode.address", ]
  expect_equal(never$repos_asked, 0L); expect_true(is.na(never$last_asked_on))
})

test_that("a repository read to its first commit counts for the author and review searches too", {
  seen <- .cov_log("w1", "review.copilot.suggestion", "hit", on = "2026-10-11"); seen$source <- "read"
  log <- rbind(seen, .cov_log("w1", "author.noreply@anthropic.com", "none", on = "2026-10-11"),
               .cov_log("r1", "author.noreply@anthropic.com", "hit", on = "2026-10-06"))
  # r1's later read is not whole, so its date must not count.
  reads <- .ai_bind_like(.ai_empty_reads(), list(data.frame(
    repo_id = c("w1", "r1"), commits_history_complete = c(1L, 0L), commits_ruleset = AI_RULESET_VERSION,
    commits_read_on = c("2026-10-11", "2026-10-12"), stringsAsFactors = FALSE)))
  cov <- build_search_coverage(log, reads)
  rv <- cov[cov$rule_key == "review.copilot.suggestion", ]
  expect_equal(c(rv$repos_asked, rv$repos_hit, rv$repos_read_whole), c(0L, 0L, 1L))
  expect_equal(rv$last_asked_on, "2026-10-11")
  au <- cov[cov$rule_key == "author.claude", ]
  expect_equal(c(au$repos_asked, au$repos_hit, au$repos_read_whole), c(1L, 1L, 1L))
  expect_equal(au$last_asked_on, "2026-10-11")
  expect_true(all(cov$repos_read_whole == 1L))
})

.mk_cov <- function(keys) {
  path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbWriteTable(con, "vcs_ai_search_coverage", data.frame(rule_key = keys, tool = "claude",
    channel = "commit-credit", rule_rev = 1L, repos_asked = 1L, repos_hit = 0L, repos_refused = 0L,
    repos_read_whole = 0L, last_asked_on = "2026-10-05", stringsAsFactors = FALSE), append = TRUE)
  path
}

test_that("a search leaves the coverage table only when its rule has left the ruleset", {
  keys <- .ai_coverage_rules()$rule_key
  prev <- .mk_cov(c(keys[1:3], "msg.retired.rule"))
  expect_equal(summary_regressions(prev, .mk_cov(keys[1:3])), character(0))
  bad <- summary_regressions(prev, .mk_cov(keys[1:2]))
  expect_true(any(grepl("vcs_ai_search_coverage", bad)))
  expect_true(any(grepl(keys[3], bad, fixed = TRUE)))
})

test_that("the weekly read's state tables refuse a build that lost their rows", {
  mk <- function(n) {
    path <- tempfile(fileext = ".db")
    con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
    ensure_repo_schema(con); ensure_series_schema(con)
    if (n) DBI::dbWriteTable(con, "vcs_ai_search_log", do.call(rbind, lapply(seq_len(n), function(i)
      .cov_log(sprintf("r%d", i), "msg.claude.coauthor", "none"))), append = TRUE)
    path
  }
  prev <- mk(100L)
  expect_equal(summary_regressions(prev, mk(99L)), character(0))
  expect_true(any(grepl("vcs_ai_search_log", summary_regressions(prev, mk(50L)))))
})
