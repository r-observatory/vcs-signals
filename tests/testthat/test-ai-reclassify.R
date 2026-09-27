.s <- function(repo, tool, codes, values, date = "2025-01-01", last = "2026-09-27")
  .ai_align_signals(data.frame(repo_id = repo, tool = tool, first_seen_date = date, first_seen_censored = 0L,
    evidence_tiers = codes, markers = values, authored = 0L, last_confirmed_date = last,
    stringsAsFactors = FALSE))
.none <- function(repo, key, rev = NULL) data.frame(repo_id = repo, rule_key = key,
  rule_rev = rev %||% Find(function(r) r$key == key, AI_TRAILER_PATTERNS)$rev,
  ruleset_version = AI_RULESET_VERSION,
  asked_on = "2026-10-05", outcome = "none", total_count = 0L, verified = NA_integer_, incomplete = 0L,
  first_hit_on = NA_character_, source = "search", stringsAsFactors = FALSE)
.go <- function(prior, cheap = NULL, log = NULL, reads = NULL, outside = NULL)
  suppressMessages(ai_reclassify_rows(prior, cheap %||% cbind(repo_id = character(0), .ai_empty_found()),
                                      log, reads, outside))

test_that("CodeRabbit rows move to the review table", {
  got <- .go(.s("r1", "coderabbit", "D", ".coderabbit.yaml"))
  expect_equal(nrow(got$signals), 0L)
  expect_equal(got$review$tool, "coderabbit"); expect_equal(got$review$markers, ".coderabbit.yaml")
  expect_true(is.na(got$review$assisted_commits)); expect_equal(got$moved[["coderabbit"]], 1L)
})

test_that("a review-only .gemini folder moves to Gemini Code Assist and the row keeps its other files", {
  cheap <- cbind(repo_id = "r1", .ai_found("gemini-code-assist", "D", ".gemini", role = "review"))
  got <- .go(.s("r1", "gemini", "D", ".gemini,GEMINI.md"), cheap)
  expect_equal(got$signals$markers, "GEMINI.md"); expect_equal(got$signals$evidence_tiers, "D")
  expect_equal(got$signals$first_seen_date, "2025-01-01")   # no code was lost, so the date stands
  expect_equal(got$review$tool, "gemini-code-assist")
  alone <- .go(.s("r1", "gemini", "D", ".gemini"), cheap)
  expect_equal(nrow(alone$signals), 0L)
})

test_that("a Gemini credit that both Gemini searches now answer none for leaves the row", {
  log <- rbind(.none("r1", "msg.gemini.coauthor"), .none("r1", "msg.gemini.bot"))
  got <- .go(.s("r1", "gemini", "B,D", "B,GEMINI.md"), log = log)
  expect_equal(got$signals$evidence_tiers, "D"); expect_equal(got$signals$markers, "GEMINI.md")
  expect_true(is.na(got$signals$first_seen_date)); expect_equal(got$signals$first_seen_censored, 0L)
  half <- .go(.s("r1", "gemini", "B,D", "B,GEMINI.md"), log = log[1, ])
  expect_equal(half$signals$evidence_tiers, "B,D")
})

test_that("a Gemini credit read from an Assisted-by line stays when both Gemini searches answer none", {
  log <- rbind(.none("r1", "msg.gemini.coauthor"), .none("r1", "msg.gemini.bot"))
  got <- .go(.s("r1", "gemini", "B,D", "B,GEMINI.md,msg.any.assisted-by"), log = log)
  expect_equal(got$signals$evidence_tiers, "B,D")
  expect_equal(got$signals$markers, "GEMINI.md,msg.any.assisted-by")
  expect_equal(got$signals$first_seen_date, "2025-01-01"); expect_equal(got$moved[["gemini_review_credit"]], 1L)
  expect_equal(.go(got$signals, log = log)$moved[["gemini_review_credit"]], 0L)
  only <- .go(.s("r1", "gemini", "B", "msg.any.assisted-by"), log = log)
  expect_equal(only$signals$evidence_tiers, "B"); expect_equal(only$signals$markers, "msg.any.assisted-by")
  expect_equal(only$moved[["gemini_review_credit"]], 0L)
})

test_that("a repository read to its first commit answers both Gemini searches it holds no row for", {
  whole <- .ai_bind_like(.ai_empty_reads(), list(data.frame(repo_id = "r1", commits_history_complete = 1L,
    commits_ruleset = AI_RULESET_VERSION, commits_read_on = "2026-10-04", stringsAsFactors = FALSE)))
  gone <- .go(.s("r1", "gemini", "B", "B"), reads = whole)
  expect_equal(nrow(gone$signals), 0L); expect_equal(gone$moved[["gemini_review_credit"]], 1L)
  hit <- .none("r1", "msg.gemini.bot")
  hit$outcome <- "hit"; hit$total_count <- 2L; hit$source <- "read"; hit$first_hit_on <- "2025-06-01"
  kept <- .go(.s("r1", "gemini", "B", "B"), log = hit, reads = whole)
  expect_equal(kept$signals$evidence_tiers, "B"); expect_equal(kept$moved[["gemini_review_credit"]], 0L)
  older <- transform(whole, commits_ruleset = "2026-01-01")
  expect_equal(nrow(.go(.s("r1", "gemini", "B", "B"), reads = older)$signals), 1L)
})

test_that("a Gemini search that answered none under an older revision of its rule changes nothing", {
  stale <- Find(function(r) r$key == "msg.gemini.bot", AI_TRAILER_PATTERNS)$rev - 1L
  log <- rbind(.none("r1", "msg.gemini.coauthor"), .none("r1", "msg.gemini.bot", rev = stale))
  got <- .go(.s("r1", "gemini", "B", "B"), log = log)
  expect_equal(got$signals$evidence_tiers, "B"); expect_equal(got$signals$markers, "B")
  expect_equal(got$moved[["gemini_review_credit"]], 0L)
})

test_that("a pull request row the full walk found only outside the project leaves the table", {
  reads <- .ai_bind_like(.ai_empty_reads(), list(data.frame(repo_id = "r1", prs_walk_complete = 1L,
    prs_walk_started_on = "2026-10-04", stringsAsFactors = FALSE)))
  outside <- data.frame(repo_id = "r1", pr_number = 5L, tool = "copilot", found_via = "pr-author",
    created_at = "2025-03-01T00:00:00Z", from_fork = 1L, author_association = "NONE",
    last_confirmed_date = "2026-10-04", stringsAsFactors = FALSE)
  got <- .go(.s("r1", "copilot", "PR", "PR", last = "2026-09-27"), reads = reads, outside = outside)
  expect_equal(nrow(got$signals), 0L); expect_equal(got$moved[["pr_superseded"]], 1L)
  seen_since <- .go(.s("r1", "copilot", "PR", "PR", last = "2026-10-04"), reads = reads, outside = outside)
  expect_equal(nrow(seen_since$signals), 1L)
})

test_that("running the moves twice changes nothing the second time", {
  prior <- rbind(.s("r1", "coderabbit", "D", ".coderabbit.yml"), .s("r2", "claude", "D", "CLAUDE.md"))
  once <- .go(prior)
  twice <- .go(once$signals)
  expect_equal(twice$signals, once$signals)
  expect_equal(nrow(twice$review), 0L)
  expect_equal(sum(twice$moved), 0L)
})

test_that("every merge logs one line per move with its count, zeros included", {
  msgs <- capture_messages(ai_reclassify_rows(NULL, NULL, NULL, NULL, NULL))
  expect_length(msgs, 4L)
  expect_match(msgs, "^ai merge: moved or trimmed 0 row\\(s\\): ")
})
