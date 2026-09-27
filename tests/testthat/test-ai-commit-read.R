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
  inside$date <- "2026-05-07T00:00:00Z"
  expect_true(verify_search_hit(vs, "B", inside)$confirmed)
})
