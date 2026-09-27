.f <- function(repo, tool, tier, marker, rule_key = NA_character_, newest = NA_character_, role = "authoring")
  cbind(repo_id = repo, .ai_found(tool, tier, marker, role = role, rule_key = rule_key,
                                  onset = "2026-10-04T23:59:59Z", newest_at = newest))
.lg <- function(repo, key, outcome, on, rev = 1L)
  data.frame(repo_id = repo, rule_key = key, rule_rev = rev, ruleset_version = AI_RULESET_VERSION,
             asked_on = on, outcome = outcome, total_count = if (outcome == "refused") NA_integer_ else 1L,
             verified = if (outcome == "hit") 1L else NA_integer_, incomplete = 0L,
             first_hit_on = if (outcome == "hit") "2025-01-01" else NA_character_, source = "search",
             stringsAsFactors = FALSE)
.pub <- function(repo, tool, tiers) .ai_align_signals(data.frame(repo_id = repo, tool = tool,
  first_seen_date = "2025-01-01", first_seen_censored = 0L, evidence_tiers = tiers, markers = tiers,
  authored = 0L, last_confirmed_date = "2026-09-27", stringsAsFactors = FALSE))
.world <- function() {
  gh <- "account.41898282+claude[bot]@users.noreply.github.com"
  list(
    flagged = data.frame(repo_id = paste0("r", 1:7), is_fork = 0L, stringsAsFactors = FALSE),
    found = rbind(.f("r1", "claude", "D", "CLAUDE.md"),
                  .f("r2", "claude", "A", "A", rule_key = gh, newest = "2026-10-03T00:00:00Z"),
                  .f("r3", "corteza", "B", "msg.corteza.address", "msg.corteza.address", "2026-10-02T00:00:00Z"),
                  .f("r4", "claude", "B", "msg.claude.coauthor", "msg.claude.coauthor", "2026-10-03T00:00:00Z"),
                  .f("r7", "codex", "PB", "pr.codex.branch", "pr.codex.branch")),
    published = rbind(.pub("r4", "claude", "B"), .pub("r5", "claude", "D")),
    log = rbind(.lg("r4", "msg.claude.coauthor", "hit", "2026-09-28"),
                .lg("r5", "msg.claude.generated", "refused", "2026-09-28")),
    reads = .ai_bind_like(.ai_empty_reads(), list(data.frame(repo_id = "r6", commits_history_complete = 1L,
              commits_ruleset = AI_RULESET_VERSION, stringsAsFactors = FALSE))),
    counts = .ai_empty_counts())
}
.work <- function(w, ...) select_deep_work(w$flagged, w$found, w$published, w$log, w$reads, w$counts,
                                           today = "2026-10-04", ...)
.has <- function(w, repo, key, reason) any(w$repo_id == repo & w$rule_key %in% key & w$reason == reason)

test_that("the work list puts dating first and backfills last, each item once", {
  w <- .work(.world())
  expect_setequal(w$repo_id[w$reason == "onset"], c("r1", "r2", "r3"))
  expect_true(.has(w, "r2", "account.41898282+claude[bot]@users.noreply.github.com", "account-count"))
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
