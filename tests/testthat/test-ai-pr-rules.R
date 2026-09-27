.pr_slugs <- c("ericrayanderson/shinyglass", "buzzcauldron/bib-ocr", "adibender/pammtools",
               "jsugarelli/switchcase", "apache/arrow-nanoarrow", "alrobles/sobol", "alrobles/maxentcpp",
               "zachmayer/caretEnsemble", "ss3sim/ss3sim", "temuulene/mongolstats", "itchyshin/drmTMB")
.pr_repos <- data.frame(repo_id = paste0("github.com/", .pr_slugs), owner = sub("/.*$", "", .pr_slugs),
                        name = sub("^.*/", "", .pr_slugs), stringsAsFactors = FALSE)
.prs_of <- function(slug, number = NULL) {
  prs <- parse_activity(ai_fixture("activity-prs.json"), .pr_repos)[[paste0("github.com/", slug)]]$prs
  if (is.null(number)) prs else prs[prs$number == number, , drop = FALSE]
}
.one_pr <- function(head, body = "", created = "2026-06-01T00:00:00Z", assoc = "OWNER",
                    cross = FALSE, login = "maintainer")
  .ai_pr_nodes_frame(list(list(number = 1L, createdAt = created,
    author = list(login = login, `__typename` = "User"), authorAssociation = assoc,
    isCrossRepository = cross, headRefName = head, body = body)))

test_that("a pull request keeps no description once its rules are read", {
  prs <- .prs_of("ericrayanderson/shinyglass")
  expect_false("body" %in% names(prs))
  expect_true(all(vapply(AI_PR_RULES, function(r) r$key %in% names(prs), logical(1))))
})

test_that("Cursor's cloud agent under the maintainer's account names Cursor, renamed branch or not", {
  c49 <- classify_prs(.prs_of("ericrayanderson/shinyglass", 49))
  expect_equal(c49$tool, "cursor"); expect_equal(c49$code, "PB"); expect_equal(c49$role, "authoring")
  expect_equal(c49$rule_key, "pr.cursor.agent-body")
  c29 <- classify_prs(.prs_of("ericrayanderson/shinyglass", 29))
  expect_true("pr.cursor.agent-branch" %in% strsplit(c29$rule_key, ",")[[1]])
})

test_that("a pull request the Cursor app opened stays one opened by the tool's account", {
  cl <- classify_prs(.prs_of("buzzcauldron/bib-ocr"))
  expect_equal(cl$code, "PR"); expect_equal(cl$tool, "cursor"); expect_equal(cl$rule_key, "PR")
})

test_that("an old text-cursor branch names nothing", {
  expect_equal(nrow(classify_prs(.one_pr("cursor/fix-blink", created = "2018-05-02T00:00:00Z"))), 0L)
  expect_equal(nrow(classify_prs(.one_pr("cursor/fix-blink"))), 0L)                          # no hex suffix
  expect_equal(nrow(classify_prs(.one_pr("cursor/fix-blink-1a2b", created = "2025-03-01T00:00:00Z"))), 0L)
  expect_equal(nrow(classify_prs(.one_pr("cursor/fix-blink-1a2b", created = "2025-06-01T00:00:00Z"))), 1L)
})

test_that("a codex/ branch alone admits the repository and names no one", {
  cl <- classify_prs(.prs_of("adibender/pammtools"))
  expect_equal(cl$rule_key, "pr.codex.branch"); expect_equal(cl$role, "authoring")
  expect_equal(nrow(outside_pr_rows(cl, "github.com/adibender/pammtools", "2026-10-04")), 0L)
  fork <- classify_prs(.one_pr("codex/speedup", assoc = "NONE", cross = TRUE))
  expect_equal(nrow(fork), 0L)
  outsider <- classify_prs(.one_pr("codex/speedup", assoc = "CONTRIBUTOR"))
  expect_equal(nrow(outsider), 0L)
})

test_that("a Codex task link names Codex and the branch rule is not stored beside it", {
  cl <- classify_prs(.prs_of("jsugarelli/switchcase"))
  expect_equal(cl$tool, "codex"); expect_equal(cl$code, "PB"); expect_equal(cl$rule_key, "pr.codex.task")
})

test_that("a Cursor branch from a fork is an outside contributor's tool", {
  cl <- classify_prs(.prs_of("apache/arrow-nanoarrow"))
  expect_equal(cl$role, "outside"); expect_equal(cl$from_fork, 1L); expect_equal(cl$association, "NONE")
  out <- outside_pr_rows(cl, "github.com/apache/arrow-nanoarrow", "2026-10-04")
  expect_equal(out$pr_number, 927L); expect_equal(out$found_via, "pr.cursor.agent-branch")
  expect_equal(out$tool, "cursor"); expect_equal(out$last_confirmed_date, "2026-10-04")
})

test_that("the same Cursor note from someone without write access is an outside contributor's tool", {
  prs <- .prs_of("ericrayanderson/shinyglass", 49)
  prs$association <- "CONTRIBUTOR"
  cl <- classify_prs(prs)
  expect_equal(cl$role, "outside"); expect_equal(cl$from_fork, 0L)
  expect_equal(outside_pr_rows(cl, "github.com/ericrayanderson/shinyglass", "2026-10-04")$found_via,
               "pr.cursor.agent-body")
})

test_that("Devin sessions, an OpenHands branch and Claude branches and sessions name their tools", {
  s43 <- classify_prs(.prs_of("alrobles/sobol", 43))
  expect_equal(s43$tool, "devin")
  expect_setequal(strsplit(s43$rule_key, ",")[[1]], c("pr.devin.session", "pr.devin.branch"))
  expect_equal(classify_prs(.prs_of("alrobles/maxentcpp"))$rule_key, "pr.devin.session")
  oh <- classify_prs(.prs_of("zachmayer/caretEnsemble"))
  expect_equal(oh$tool, "openhands"); expect_equal(oh$rule_key, "pr.openhands.branch")
  mg <- classify_prs(.prs_of("temuulene/mongolstats"))
  expect_setequal(strsplit(mg$rule_key, ",")[[1]], c("pr.claude.branch", "pr.claude.session"))
  expect_equal(classify_prs(.prs_of("itchyshin/drmTMB"))$rule_key, "pr.cursor.made-with")
})

test_that("pull requests opened by Claude's and Amazon Q's accounts count through the account", {
  s36 <- classify_prs(.prs_of("alrobles/sobol", 36))
  expect_equal(s36$code, "PR"); expect_equal(s36$tool, "claude")
  q <- classify_prs(.prs_of("ss3sim/ss3sim"))
  expect_equal(q$code, "PR"); expect_equal(q$tool, "amazonq")
})

test_that("branch and description rules read their text as written", {
  expect_equal(classify_prs(.one_pr("Q-DEV-issue-12-3"))$tool, "amazonq")
  expect_equal(nrow(classify_prs(.one_pr("q-dev-issue-12-3"))), 0L)
  expect_equal(nrow(classify_prs(.one_pr("fix", body = "<!-- cursor_agent_pr_body_begin -->\nx"))), 0L)
  expect_equal(classify_prs(.one_pr("fix", body = "MADE WITH [Cursor](https://cursor.com)"))$rule_key,
               "pr.cursor.made-with")
})

test_that("the earliest pull request date comes only from ones the package's own people opened", {
  prs <- rbind(.prs_of("apache/arrow-nanoarrow"), .prs_of("ericrayanderson/shinyglass", 29))
  expect_equal(earliest_agent_pr_date(list(prs = prs)), "2026-08-21T14:01:40Z")
})

test_that("a pull request from a deleted account parses and names only what its branch says", {
  # GitHub returns author: null for a deleted account, shown on the site as "ghost".
  ghost <- function(head, assoc) .ai_pr_nodes_frame(list(list(number = 7L, createdAt = "2026-06-01T00:00:00Z",
    author = NULL, authorAssociation = assoc, isCrossRepository = FALSE, headRefName = head, body = NULL)))
  g <- ghost("fix-typo", "NONE")
  expect_true(is.na(g$login)); expect_true(is.na(g$typename))
  expect_equal(nrow(classify_prs(g)), 0L)
  outside <- classify_prs(ghost("cursor/fix-docs-a1b2", "NONE"))
  expect_equal(outside$role, "outside"); expect_equal(outside$from_fork, 0L); expect_equal(outside$code, "PB")
})

test_that("a person whose login is a tool's name opens a pull request that names nothing", {
  # Bare devin, jules, amp, kiro, junie and copilot are people's accounts, not the tools'.
  people <- c("devin", "jules", "amp", "kiro", "junie", "copilot")
  expect_false(any(people %in% tolower(names(AI_PR_AGENT_LOGINS))))
  for (who in people)
    expect_equal(nrow(classify_prs(.one_pr("fix-readme", login = who))), 0L, info = who)
})

test_that("a description written in the browser, with CRLF endings, still names Cursor", {
  # Descriptions typed on github.com come back with \r\n line endings.
  body <- "<!-- CURSOR_AGENT_PR_BODY_BEGIN -->\r\n## Summary\r\n\r\nMade with [Cursor](https://cursor.com)\r\n"
  got <- classify_prs(.one_pr("fix", body = body))
  expect_setequal(strsplit(got$rule_key, ",")[[1]], c("pr.cursor.agent-body", "pr.cursor.made-with"))
  sess <- classify_prs(.one_pr("fix", body = "Summary\r\n\r\nhttps://app.devin.ai/sessions/0a1b\r\n"))
  expect_equal(sess$rule_key, "pr.devin.session")
})
