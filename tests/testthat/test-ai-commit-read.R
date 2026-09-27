hits_for <- function(short) match_commit_findings(ai_commit(short))
keys_of <- function(h) sort(unique(h$rule_key[!is.na(h$rule_key)]))

test_that("a commit made with Cursor is credited to Cursor", {
  h <- hits_for("fe5c63c")
  expect_equal(keys_of(h), "msg.cursor.made-with")
  expect_equal(unique(h$tool), "cursor")
})

test_that("an Antigravity credit names Antigravity and not Gemini", {
  h <- hits_for("ab1da7b")
  expect_equal(unique(h$tool), "antigravity")
  for (addr in c("gemini@google.com", "noreply@google.com", "antigravity@gemini.google.com"))
    expect_equal(unique(match_commit_findings(data.frame(oid = "x", committed_at = "2026-07-10T00:00:00Z",
      message = sprintf("fix\n\nCo-authored-by: Antigravity <%s>", addr), author_name = "p",
      author_email = "p@example.org", author_login = "p", stringsAsFactors = FALSE))$tool),
      "antigravity", info = addr)
  bot <- data.frame(oid = "x", committed_at = "2026-07-10T00:00:00Z",
                    message = "fix\n\nCo-authored-by: Antigravity Bot <bot@antigravity.ai>",
                    author_name = "p", author_email = "p@example.org", author_login = "p",
                    stringsAsFactors = FALSE)
  expect_equal(nrow(match_commit_findings(bot)), 0L)
})

test_that("the three Copilot credit shapes each name Copilot", {
  expect_equal(keys_of(hits_for("1a85ae8")), "msg.copilot.cli")
  expect_equal(keys_of(hits_for("f3b46fb")), "msg.copilot.vscode")
  h <- hits_for("6a4205c")
  expect_true("msg.copilot.cloud-coauthor" %in% h$rule_key)
  expect_true(all(h$tool == "copilot"))
})

test_that("a VS Code Copilot line inside its false window is ignored and one after it is kept", {
  mk <- function(date) data.frame(oid = date, committed_at = paste0(date, "T12:00:00Z"),
    message = "fix\n\nCo-authored-by: Copilot <copilot@github.com>", author_name = "p",
    author_email = "p@example.org", author_login = "p", stringsAsFactors = FALSE)
  expect_equal(nrow(match_commit_findings(mk("2026-04-25"))), 0L)
  expect_equal(nrow(match_commit_findings(mk("2026-05-06"))), 0L)
  expect_equal(match_commit_findings(mk("2026-05-07"))$rule_key, "msg.copilot.vscode")
})

test_that("a Copilot Autofix credit matches nothing, and an accepted review suggestion goes to review", {
  mk <- function(line) data.frame(oid = "x", committed_at = "2026-09-01T00:00:00Z",
    message = paste0("fix\n\n", line), author_name = "p", author_email = "p@example.org",
    author_login = "p", stringsAsFactors = FALSE)
  expect_equal(nrow(match_commit_findings(mk(
    "Co-authored-by: Copilot Autofix powered by AI <175728472+Copilot@users.noreply.github.com>"))), 0L)
  expect_equal(nrow(match_commit_findings(mk(
    "Co-authored-by:\tCopilot Autofix powered by AI <175728472+Copilot@users.noreply.github.com>"))), 0L)
  expect_equal(nrow(match_commit_findings(mk(
    "Co-authored-by: Copilot Autofix powered by AI <62310815+github-advanced-security[bot]@users.noreply.github.com>"))), 0L)
  for (line in c("Co-authored-by: Copilot <175728472+Copilot@users.noreply.github.com>",
                 "Co-authored-by:Copilot <175728472+Copilot@users.noreply.github.com>")) {
    h <- match_commit_findings(mk(line))
    expect_equal(h$role, "review", info = line)
    expect_equal(h$tool, "copilot-review", info = line)
    expect_equal(h$rule_key, "review.copilot.suggestion", info = line)
  }
  gca <- match_commit_findings(mk(
    "Co-authored-by: gemini-code-assist[bot] <176961590+gemini-code-assist[bot]@users.noreply.github.com>"))
  expect_equal(gca$tool, "gemini-code-assist")
  expect_equal(gca$role, "review")
})

test_that("a commit by the claude-code-action default address names Claude and asks for a count", {
  h <- hits_for("eae1b2c")
  expect_equal(unique(h$tool), "claude")
  expect_equal(h$code, "A")
  expect_equal(h$rule_key, "account.41898282+claude[bot]@users.noreply.github.com")
})

test_that("a commit by the Cursor app's id-less address asks for its own count", {
  # The filter's 206951365+ address misses commits written with this id-less form, so REST counts it alone.
  h <- hits_for("d169ea0")
  expect_equal(unique(h$tool), "cursor")
  expect_equal(h$rule_key, "account.cursor[bot]@users.noreply.github.com")
})

test_that("codex@local, an Assisted-by address and Crush's own line name their tools", {
  expect_equal(keys_of(hits_for("c759783")), "msg.codex.coauthor")
  ab <- hits_for("593077b")
  expect_equal(ab$tool, "codex")
  expect_equal(ab$rule_key, "msg.any.assisted-by")
  cr <- hits_for("55e6cd3")
  expect_equal(unique(cr$tool), "crush")
  expect_equal(ai_assisted_by_tools("Assisted-by: GPT-6")[[1]], character(0))
})

test_that("a credit with the name removed and a session line each name Claude", {
  expect_true("msg.claude.address" %in% hits_for("d310618")$rule_key)
  expect_true("msg.claude.session" %in% hits_for("83356e1")$rule_key)
  expect_true(all(c(hits_for("d310618")$tool, hits_for("83356e1")$tool) == "claude"))
})

test_that("a commit message with CRLF endings still names its tool", {
  # Commits made on Windows keep \r\n in the message GitHub returns.
  crlf <- function(msg) data.frame(oid = "x", committed_at = "2026-09-01T00:00:00Z",
    message = gsub("\n", "\r\n", msg, fixed = TRUE), author_name = "p", author_email = "p@example.org",
    author_login = "p", stringsAsFactors = FALSE)
  expect_equal(match_commit_findings(crlf("fix\n\nMade-with: Cursor\n"))$rule_key, "msg.cursor.made-with")
  expect_true("msg.claude.session" %in%
    match_commit_findings(crlf("fix\n\nClaude-Session: https://claude.ai/code/session_01N6\n"))$rule_key)
  ab <- match_commit_findings(crlf("ci\n\nAssisted-by: GPT-6 Astra <codex@openai.com>\n"))
  expect_equal(ab$tool, "codex")
  expect_equal(unique(match_commit_findings(crlf("x\n\nCo-authored-by: Antigravity <gemini@google.com>\n"))$tool),
               "antigravity")
})

test_that("a search hit on the Assisted-by line is credited to the tool the line names", {
  rule <- Find(function(r) r$key == "msg.any.assisted-by", AI_TRAILER_PATTERNS)
  v <- verify_search_hit(rule, "B", list(message = "ci: x\n\nAssisted-by: GPT-6 Astra <codex@openai.com>"))
  expect_true(v$confirmed); expect_equal(v$tool, "codex")
  expect_false(verify_search_hit(rule, "B", list(message = "ci: x\n\nAssisted-by: GPT-6"))$confirmed)
  vs <- Find(function(r) r$key == "msg.copilot.vscode", AI_TRAILER_PATTERNS)
  inside <- list(message = "x\n\nCo-authored-by: Copilot <copilot@github.com>", date = "2026-04-30T00:00:00Z")
  expect_false(verify_search_hit(vs, "B", inside)$confirmed)
  expect_true(is.na(verify_search_hit(vs, "B", inside)$tool))
  inside$date <- "2026-05-07T00:00:00Z"
  expect_true(verify_search_hit(vs, "B", inside)$confirmed)
  expect_equal(verify_search_hit(vs, "B", inside)$tool, "copilot")
})

.commits <- function(dates, message = "chore: x") data.frame(oid = sprintf("o%03d", seq_along(dates)),
  committed_at = dates, message = message, author_name = "p", author_email = "p@example.org",
  author_login = "p", stringsAsFactors = FALSE)

test_that("a read that reaches the first commit dates exactly and logs only the rules that matched", {
  cm <- rbind(ai_commit("fe5c63c"), ai_commit("83356e1"))
  plan <- plan_commit_read(NULL)
  st <- next_commit_read_state(NULL, plan, cm, has_next = FALSE, today = "2026-10-04")
  expect_equal(c(st$commits_history_complete, st$commits_window_complete, st$reached_first), c(1L, 1L, 1L))
  expect_equal(st$commits_read_through, "2026-08-06T01:50:50Z")
  expect_equal(read_count_mode(plan, st), "replace")
  lg <- read_log_rows(match_commit_findings(cm), "github.com/o/r", "2026-10-04", "replace")
  expect_setequal(lg$rule_key, c("msg.cursor.made-with", "msg.claude.coauthor", "msg.claude.address", "msg.claude.session"))
  expect_true(all(lg$source == "read" & lg$verified == 1L & lg$outcome == "hit" & lg$mode == "replace"))
  expect_equal(lg$first_hit_on[lg$rule_key == "msg.cursor.made-with"], "2026-03-18")
  f <- assemble_repo_evidence(list(), list(commits = cm), whole_history = TRUE)
  expect_true(all(f$onset_censored == 0L))
})

test_that("a weekly read adds only commits after the last one read, so overlap is never counted twice", {
  prev <- data.frame(commits_read_on = "2026-10-04", commits_read_through = "2026-10-01T00:00:00Z",
                     commits_ruleset = AI_RULESET_VERSION, commits_history_complete = 1L, stringsAsFactors = FALSE)
  plan <- plan_commit_read(prev)
  cm <- .commits(c("2026-09-25T00:00:00Z", "2026-10-03T00:00:00Z"),
                 "fix\n\nCo-authored-by: Claude Opus 4.8 <noreply@anthropic.com>")
  st <- next_commit_read_state(prev, plan, cm, has_next = FALSE, today = "2026-10-11")
  expect_equal(st$commits_history_complete, 1L); expect_equal(read_count_mode(plan, st), "add")
  lg <- read_log_rows(match_commit_findings(cm), "r", "2026-10-11", "add", after = prev$commits_read_through)
  expect_equal(lg$total_count[lg$rule_key == "msg.claude.coauthor"], 1L)
  mr <- read_model_rows("r", cm, match_commit_findings(cm), "add", after = prev$commits_read_through)
  expect_equal(nrow(mr), 1L); expect_equal(mr$commits, 1L); expect_equal(mr$mode, "add")
})

test_that("Assisted-by lines are counted per tool the line names", {
  cm <- .commits(sprintf("2026-%02d-01T00:00:00Z", c(rep(1:12, 4), 1, 2, 3)),
                 c(rep("x\n\nAssisted-by: Copilot", 50), "x\n\nAssisted-by: Claude Opus 5"))
  lg <- read_log_rows(match_commit_findings(cm), "r", "2026-10-04", "replace")
  expect_equal(lg$total_count[lg$rule_key == "msg.any.assisted-by.copilot"], 50L)
  expect_equal(lg$total_count[lg$rule_key == "msg.any.assisted-by.claude"], 1L)
  expect_false("msg.any.assisted-by" %in% lg$rule_key)
})

test_that("a ruleset change pages a grown whole history back to its first commit", {
  prev <- data.frame(commits_read_on = "2026-10-04", commits_read_through = "2026-10-01T00:00:00Z",
                     commits_ruleset = "2026-01-01", commits_history_complete = 1L, stringsAsFactors = FALSE)
  plan <- plan_commit_read(prev)
  expect_true(more_commit_pages(plan, "2025-06-01T00:00:00Z", TRUE, 0L))
  expect_true(more_commit_pages(plan, "2024-06-01T00:00:00Z", TRUE, 1L))
  expect_false(more_commit_pages(plan, "2023-06-01T00:00:00Z", FALSE, 2L))
  st <- next_commit_read_state(prev, plan, .commits(rep("2025-01-01T00:00:00Z", 250)), has_next = FALSE,
                               today = "2026-10-11")
  expect_equal(c(st$commits_history_complete, st$commits_window_complete, st$commits_read), c(1L, 1L, 250L))
})

test_that("a read stopped by the page cap records the gap and loses its whole-history status", {
  prev <- data.frame(commits_read_on = "2026-10-04", commits_read_through = "2026-10-01T00:00:00Z",
                     commits_ruleset = "2026-01-01", commits_history_complete = 1L, stringsAsFactors = FALSE)
  plan <- plan_commit_read(prev)
  expect_false(more_commit_pages(plan, "2024-01-01T00:00:00Z", TRUE, AI_COMMIT_PAGE_CAP))
  st <- next_commit_read_state(prev, plan, .commits(rep("2025-01-01T00:00:00Z", 600)), has_next = TRUE,
                               today = "2026-10-11")
  expect_equal(c(st$commits_window_complete, st$commits_history_complete), c(0L, 0L))
  expect_true(is.na(read_count_mode(plan, st)))
})

test_that("a busy week reads back to two weeks before the last read, within the cap", {
  prev <- data.frame(commits_read_on = "2026-10-04", commits_read_through = "2026-10-01T00:00:00Z",
                     commits_ruleset = AI_RULESET_VERSION, commits_history_complete = 0L, stringsAsFactors = FALSE)
  plan <- plan_commit_read(prev)
  expect_true(more_commit_pages(plan, "2026-10-05T00:00:00Z", TRUE, 0L))
  expect_false(more_commit_pages(plan, "2026-10-05T00:00:00Z", TRUE, AI_COMMIT_PAGE_CAP))
  expect_false(more_commit_pages(plan_commit_read(NULL), "2026-10-05T00:00:00Z", TRUE, 0L))
})

test_that("the pull request walk runs once back to the cutoff, then only catches up", {
  page1 <- list(prs = data.frame(created_at = c("2026-10-01T00:00:00Z", "2026-06-01T00:00:00Z")),
                prs_has_next = TRUE, prs_end_cursor = "P1")
  first <- plan_pr_pages(NULL, page1)
  expect_true(first$walk); expect_equal(first$kind, "walk"); expect_equal(first$after, "P1")
  expect_equal(first$stop_before, AI_PR_CUTOFF)
  resume <- plan_pr_pages(data.frame(prs_walk_complete = 0L, prs_walk_cursor = "P9", stringsAsFactors = FALSE), page1)
  expect_equal(resume$after, "P9")
  st <- next_pr_read_state(NULL, page1, first, list(has_next = TRUE, end_cursor = "P2", reached_stop = FALSE), "2026-10-04")
  expect_equal(c(st$prs_walk_complete, st$prs_walk_cursor, st$prs_walk_started_on), c("0", "P2", "2026-10-04"))
  done <- next_pr_read_state(NULL, page1, first, list(has_next = TRUE, end_cursor = "P7", reached_stop = TRUE), "2026-10-04")
  expect_equal(done$prs_walk_complete, 1L); expect_true(is.na(done$prs_walk_cursor))
  walked <- data.frame(prs_walk_complete = 1L, prs_newest_created_at = "2026-05-01T00:00:00Z",
                       prs_walk_started_on = "2026-10-04", stringsAsFactors = FALSE)
  catch <- plan_pr_pages(walked, page1)
  expect_equal(catch$kind, "catch-up"); expect_equal(catch$stop_before, "2026-05-01T00:00:00Z")
  quiet <- plan_pr_pages(walked, list(prs = page1$prs, prs_has_next = FALSE, prs_end_cursor = NA))
  expect_false(quiet$walk)
  expect_false(more_pr_pages(first, "2022-12-01T00:00:00Z", TRUE, 10L))
  expect_false(more_pr_pages(first, "2025-01-01T00:00:00Z", TRUE, 0L))
  expect_true(more_pr_pages(first, "2025-01-01T00:00:00Z", TRUE, 10L))
})

test_that("a weekly read's rows name the watermark they counted after, so the log adds them only on that chain", {
  cm <- .commits(c("2026-09-25T00:00:00Z", "2026-10-03T00:00:00Z"),
                 "fix\n\nCo-authored-by: Claude Opus 4.8 <noreply@anthropic.com>")
  h <- match_commit_findings(cm); wm <- "2026-09-25T00:00:00Z"
  add <- read_log_rows(h, "r", "2026-10-11", "add", after = wm)
  expect_identical(unique(add$read_after), wm)
  expect_identical(unique(read_model_rows("r", cm, h, "add", after = wm)$read_after), wm)
  expect_identical(unique(read_log_rows(h, "r", "2026-10-11", "replace")$read_after), NA_character_)
  expect_identical(unique(read_model_rows("r", cm, h, "replace")$read_after), NA_character_)
  for (none in list(read_log_rows(h, "r", "2026-10-11", NA_character_), read_log_rows(NULL, "r", "2026-10-11", "add"),
                    read_model_rows("r", cm, h, NA_character_)))
    expect_true(all(c("mode", "read_after") %in% names(none)) && nrow(none) == 0L)
  # The stored read reached the first commit and counted through wm.
  prior <- read_log_rows(match_commit_findings(cm[1, ]), "r", "2026-10-04", "replace")
  reads <- data.frame(repo_id = "r", commits_read_through = wm, stringsAsFactors = FALSE)
  folded <- fold_search_log(prior, add, rebuilt_repos = character(0), reads = reads)
  expect_equal(folded$total_count[folded$rule_key == "msg.claude.coauthor"], 2L)
  later <- transform(reads, commits_read_through = "2026-10-03T00:00:00Z")
  kept <- fold_search_log(prior, add, rebuilt_repos = character(0), reads = later)
  expect_equal(kept$total_count[kept$rule_key == "msg.claude.coauthor"], 1L)
})
