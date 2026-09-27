.bot <- "account.41898282+claude[bot]@users.noreply.github.com"
.f <- function(repo, tool, code, value, rule_key = NA_character_, newest = NA_character_, role = "authoring")
  cbind(repo_id = repo, .ai_found(tool, code, value, role = role, rule_key = rule_key,
                                  onset = "2026-10-04T23:59:59Z", newest_at = newest))
.lg <- function(repo, key, outcome, on, rev = 1L)
  data.frame(repo_id = repo, rule_key = key, rule_rev = rev, ruleset_version = AI_RULESET_VERSION,
             asked_on = on, outcome = outcome,
             total_count = switch(outcome, refused = NA_integer_, none = 0L, hit = 1L),
             verified = if (outcome == "hit") 1L else NA_integer_, incomplete = 0L,
             first_hit_on = if (outcome == "hit") "2025-01-01" else NA_character_, source = "search",
             stringsAsFactors = FALSE)
.pub <- function(repo, tool, codes, date = "2025-01-01", censored = 0L) .ai_align_signals(data.frame(
  repo_id = repo, tool = tool, first_seen_date = date, first_seen_censored = censored, evidence_tiers = codes,
  markers = codes, authored = 0L, last_confirmed_date = "2026-09-27", stringsAsFactors = FALSE))
.fl <- function(ids, fork = 0L) data.frame(repo_id = ids, is_fork = fork, stringsAsFactors = FALSE)
.whole <- function(repo) .ai_bind_like(.ai_empty_reads(), list(data.frame(repo_id = repo,
  commits_history_complete = 1L, commits_ruleset = AI_RULESET_VERSION, stringsAsFactors = FALSE)))
.world <- function() {
  list(
    flagged = .fl(paste0("r", 1:7)),
    found = rbind(.f("r1", "claude", "D", "CLAUDE.md"),
                  .f("r2", "claude", "A", "A", rule_key = .bot, newest = "2026-10-03T00:00:00Z"),
                  .f("r3", "corteza", "B", "msg.corteza.address", "msg.corteza.address", "2026-10-02T00:00:00Z"),
                  .f("r4", "claude", "B", "msg.claude.coauthor", "msg.claude.coauthor", "2026-10-03T00:00:00Z"),
                  .f("r7", "codex", "PB", "pr.codex.branch", "pr.codex.branch")),
    published = rbind(.pub("r4", "claude", "B"), .pub("r5", "claude", "D")),
    log = rbind(.lg("r4", "msg.claude.coauthor", "hit", "2026-09-28"),
                .lg("r5", "msg.claude.generated", "refused", "2026-09-28")),
    reads = .whole("r6"),
    counts = .ai_empty_counts())
}
.work <- function(w, ...) select_deep_work(w$flagged, w$found, w$published, w$log, w$reads, w$counts,
                                           today = "2026-10-04", ...)
.has <- function(w, repo, key, reason) any(w$repo_id == repo & w$rule_key %in% key & w$reason == reason)

test_that("the work list puts dating first and backfills last, each item once", {
  w <- .work(.world())
  expect_setequal(w$repo_id[w$reason == "onset"], c("r1", "r2", "r3"))
  expect_true(.has(w, "r2", .bot, "account-count"))
  expect_true(.has(w, "r3", "msg.corteza.address", "window-hit"))
  expect_true(.has(w, "r4", "msg.claude.coauthor", "count-refresh"))
  expect_true(.has(w, "r5", "msg.claude.generated", "re-ask"))
  expect_true(.has(w, "r1", "msg.claude.session", "rule-new"))
  expect_true(.has(w, "r1", "msg.claude.coauthor", "never-asked"))
  expect_equal(w$priority, sort(w$priority))
  expect_equal(anyDuplicated(paste(w$repo_id, w$tool, w$rule_key)), 0L)
  expect_equal(unname(AI_WORK_PRIORITY[w$reason]), w$priority)
})

test_that("a repository whose whole history was read gets no commit search", {
  w <- .work(.world())
  expect_false(any(w$repo_id == "r6" & grepl("^(msg|name)\\.", w$rule_key)))
})

test_that("a codex branch alone puts the repository's searches on the list but is never an item", {
  w <- .work(.world())
  expect_true(any(w$repo_id == "r7" & w$reason %in% c("rule-new", "never-asked")))
  expect_false(any(w$rule_key %in% "pr.codex.branch"))
  expect_false(any(w$repo_id == "r7" & w$reason == "onset"))
})

test_that("a published row with no first date in a flagged repository is sent back for dating", {
  w0 <- .world()
  undated <- rbind(.pub("r5", "cursor", "D"), .pub("r9", "cursor", "D"))
  undated$first_seen_date <- NA_character_
  w0$published <- rbind(w0$published, undated)
  w <- .work(w0)
  expect_true(any(w$repo_id == "r5" & w$tool == "cursor" & w$reason == "onset" & is.na(w$rule_key)))
  expect_false(any(w$repo_id == "r9"))
})

test_that("a new rule stays on the list until every flagged repository has been asked, whatever ruleset follows", {
  w0 <- .world()
  w0$log <- rbind(w0$log, do.call(rbind, lapply(paste0("r", 1:3), function(r)
    .lg(r, "msg.claude.session", "none", "2026-10-05"))))
  old <- AI_RULESET_VERSION; on.exit(AI_RULESET_VERSION <<- old, add = TRUE)
  AI_RULESET_VERSION <<- "2099-01-01"
  w <- .work(w0)
  asked <- paste0("r", 1:3); rest <- setdiff(paste0("r", 1:7), asked)
  expect_false(any(w$repo_id %in% asked & w$rule_key %in% "msg.claude.session"))
  expect_true(all(rest %in% w$repo_id[w$rule_key %in% "msg.claude.session" & w$reason == "rule-new"]))
})

test_that("a published Gemini credit asks both Gemini searches again", {
  w0 <- .world(); w0$published <- rbind(w0$published, .pub("r1", "gemini", "B"))
  w <- .work(w0)
  expect_true(.has(w, "r1", "msg.gemini.coauthor", "re-ask"))
  expect_true(.has(w, "r1", "msg.gemini.bot", "re-ask"))
})

test_that("a full gate starts a campaign, and a later dispatch skips what it already asked", {
  expect_equal(campaign_start(TRUE, NA_character_, "2026-10-04"), "2026-10-04")
  expect_equal(campaign_start(TRUE, "2026-09-01", "2026-10-04"), "2026-09-01")
  expect_true(is.na(campaign_start(FALSE, "2026-09-01", "2026-10-04")))
  w0 <- .world(); w0$log <- rbind(w0$log, .lg("r1", "msg.claude.session", "none", "2026-10-05"))
  w <- .work(w0, campaign_since = "2026-10-04", full = TRUE)
  expect_true(.has(w, "r1", "msg.claude.coauthor", "campaign"))
  expect_false(any(w$repo_id == "r1" & w$rule_key %in% "msg.claude.session"))
  expect_false(any(w$reason %in% c("rule-new", "never-asked")))
  always <- .ai_search_rules(); always <- always[always$search == "always", ]
  done <- do.call(rbind, lapply(always$key, function(k)
    .lg("r1", k, "none", "2026-10-05", rev = always$rev[always$key == k])))
  expect_true(campaign_finished("r1", done, .ai_empty_reads(), "2026-10-04"))
  expect_false(campaign_finished("r1", done[-1, ], .ai_empty_reads(), "2026-10-04"))
})

test_that("a rule matched after its none or never asked is refreshed, and one matched before its none is not", {
  cred <- function(at) .f("r8", "claude", "B", "msg.claude.coauthor", "msg.claude.coauthor", at)
  sw <- function(at, log) select_deep_work(.fl("r8"), cred(at), NULL, log, NULL, NULL, today = "2026-10-04")
  none <- .lg("r8", "msg.claude.coauthor", "none", "2026-09-28")
  expect_true(.has(sw("2026-10-02T00:00:00Z", none), "r8", "msg.claude.coauthor", "count-refresh"))
  expect_false(any(sw("2026-09-20T00:00:00Z", none)$rule_key %in% "msg.claude.coauthor"))
  expect_true(.has(sw("2026-10-02T00:00:00Z", NULL), "r8", "msg.claude.coauthor", "count-refresh"))
  refused <- .lg("r8", "msg.claude.coauthor", "refused", "2026-09-28")
  expect_true(.has(sw("2026-10-02T00:00:00Z", refused), "r8", "msg.claude.coauthor", "re-ask"))
})

test_that("scan-day floors and account-dated rows go back for dating only when something can date them", {
  floor <- "2026-09-27T23:59:59Z"
  p <- rbind(.pub("a", "claude", "D", floor, 1L), .pub("b", "claude", "D", floor, 1L),
             .pub("c", "claude", "D", floor, 1L), .pub("d", "claude", "D"))
  fd <- rbind(.f("a", "claude", "D", "CLAUDE.md"), .f("b", "claude", "D", "gitignore:.claude"),
              .f("c", "claude", "D", "CLAUDE.md"), .f("d", "claude", "D", "CLAUDE.md"))
  w <- select_deep_work(rbind(.fl(c("a", "b", "d")), .fl("c", 1L)), fd, p, NULL, NULL, NULL,
                        today = "2026-10-04")
  expect_equal(w$repo_id[w$reason == "onset"], "a")
  acct <- .pub("e", "claude", "A", "2026-09-01T00:00:00Z", 1L)
  w <- select_deep_work(.fl("e"), NULL, acct, NULL, NULL, NULL, today = "2026-10-04")
  expect_true(any(w$repo_id == "e" & w$reason == "onset"))
  asked <- .lg("e", "author.noreply@anthropic.com", "none", "2026-09-28")
  w <- select_deep_work(.fl("e"), NULL, acct, asked, NULL, NULL, today = "2026-10-04")
  expect_false(any(w$reason == "onset"))
})

test_that("a REST-only count is due for a newer commit, and a refused one is asked again while REST-only", {
  seen <- .f("r2", "claude", "A", "A", rule_key = .bot, newest = "2026-10-03T00:00:00Z")
  cnt <- function(on) data.frame(repo_id = "r2", tool = "claude", identity_set = sub("^account\\.", "", .bot),
                                 commits = 3L, newest_commit_date = NA_character_, measured_on = on,
                                 stringsAsFactors = FALSE)
  expect_false(any(select_deep_work(.fl("r2"), seen, NULL, NULL, NULL, cnt("2026-10-03"),
                                    today = "2026-10-04")$reason == "account-count"))
  expect_true(any(select_deep_work(.fl("r2"), seen, NULL, NULL, NULL, cnt("2026-10-02"),
                                   today = "2026-10-04")$reason == "account-count"))
  w <- select_deep_work(.fl("w1"), NULL, NULL, .lg("w1", .bot, "refused", "2026-10-04"), .whole("w1"), NULL,
                        today = "2026-10-11")
  expect_true(.has(w, "w1", .bot, "re-ask"))
  expect_equal(w$tool[w$rule_key %in% .bot], "claude")
  retired <- .lg("r5", "account.gone[bot]@users.noreply.github.com", "refused", "2026-10-04")
  w <- select_deep_work(.fl("r5"), NULL, NULL, retired, NULL, NULL, today = "2026-10-11")
  expect_false(any(w$rule_key %in% retired$rule_key))
})

test_that("a history read whole gets neither a refresh nor a message re-ask", {
  f <- .f("w1", "claude", "B", "msg.claude.coauthor", "msg.claude.coauthor", "2026-10-02T00:00:00Z")
  lg <- rbind(.lg("w1", "msg.claude.coauthor", "hit", "2026-09-28"),
              .lg("w1", "msg.claude.generated", "refused", "2026-09-28"))
  w <- select_deep_work(.fl("w1"), f, NULL, lg, .whole("w1"), NULL, today = "2026-10-04")
  expect_false(any(w$reason %in% c("count-refresh", "re-ask")))
})

test_that("a campaign is not finished while a rule was last asked at an older revision", {
  always <- .ai_search_rules(); always <- always[always$search == "always", ]
  done <- do.call(rbind, lapply(seq_len(nrow(always)), function(i)
    .lg("r1", always$key[i], "none", "2026-10-05", rev = always$rev[i])))
  expect_true(campaign_finished("r1", done, .ai_empty_reads(), "2026-10-04"))
  done$rule_rev[1] <- done$rule_rev[1] - 1L
  expect_false(campaign_finished("r1", done, .ai_empty_reads(), "2026-10-04"))
})
