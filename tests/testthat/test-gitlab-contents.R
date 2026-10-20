one <- function(repo_id, node_id) data.frame(repo_id = repo_id, node_id = node_id, stringsAsFactors = FALSE)

test_that("rurl's recorded answer reads like the GitHub parser's output and finds its files", {
  el <- gl_parse_contents(gl_contents_fixture("contents-rurl.json"),
                          one("gitlab.com/bart-turczynski/rurl", "gitlab:gitlab.com:85027322"))[[1]]
  expect_true(all(c("CLAUDE.md", "AGENTS.md", ".gitlab/issue_templates", ".gitlab/merge_request_templates")
                  %in% el$root_entries))
  expect_setequal(el$github_entries, c("ISSUE_TEMPLATE", "pull_request_template.md"))
  expect_false(el$is_fork); expect_true(is.na(el$parent)); expect_identical(el$gl_pages_count, 1L)
  expect_true("workflows" %in% names(el)); expect_null(el$workflows)
  expect_identical(nchar(el$gitlab_ci$text), 43399L)
  f <- classify_tree_markers(el$root_entries, el$github_entries)
  expect_setequal(paste(f$tool, f$marker, sep = ":"), c("claude:CLAUDE.md", "agents-md:AGENTS.md"))
  ig <- scan_ignore_tokens(el$gitignore_lines, el$rbuildignore_lines, el$root_entries)
  expect_true("claude" %in% ig$tool)
  dv <- classify_dev_tooling(el$root_entries, el$github_entries, repo = el)
  expect_equal(unname(unlist(dv[c("ci_gitlab", "ci_github_actions", "has_dependabot", "has_cran_comments",
                                "has_cran_submission", "has_issue_template")])), c(1L, 0L, 0L, 1L, 1L, 1L))
  expect_identical(c(dv$pr_template_source, dv$ci_workflow_files), c("repo", "[]"))
  expect_identical(c(dv$gitlab_ci_rcmdcheck, dv$gitlab_ci_coverage, dv$gitlab_ci_site_deploy), c(1L, 1L, 1L))
})

test_that("hyd1d reads as its GitHub row did, with no merge request template answer", {
  el <- gl_parse_contents(gl_contents_fixture("contents-hyd1d.json"),
                          one("gitlab.opencode.de/bafg-bund/hyd1d", "gitlab:gitlab.opencode.de:10513"))[[1]]
  dv <- classify_dev_tooling(el$root_entries, el$github_entries, repo = el)
  expect_equal(unname(unlist(dv[c("has_rproj", "has_data_raw", "has_pkgdown", "has_gitattributes", "has_pages",
                                "gitlab_ci_rcmdcheck")])), c(1L, 1L, 1L, 1L, 0L, 0L))
  expect_identical(dv$readme_source, "rmd"); expect_true(is.na(dv$has_pr_template))
})

test_that("an old host names the fields it lacks, and its retry reads them as unknown", {
  expect_setequal(gl_schema_fields(gl_contents_fixture("contents-leipzig-rejected.json")), c("forkedFrom", "pagesDeployments"))
  el <- gl_parse_contents(gl_contents_fixture("contents-leipzig-retry.json"),
                          one("git.sc.uni-leipzig.de/my221hepi/childsds", "gitlab:git.sc.uni-leipzig.de:1184"))[[1]]
  expect_false(el$is_fork); expect_true(is.na(el$parent)); expect_true(is.na(el$gl_pages_count))
  expect_true(is.na(classify_dev_tooling(el$root_entries, el$github_entries, repo = el)$has_pages))
  expect_false(grepl("forkedFrom|pagesDeployments", gl_contents_query(GITLAB_OPTIONAL_FIELDS)))
})

hyd1d <- function() gl_contents_fixture("contents-hyd1d.json")$data$projects$nodes[[1]]
hyd1d_row <- one("gitlab.opencode.de/bafg-bund/hyd1d", "gitlab:gitlab.opencode.de:10513")

test_that("a listing of exactly 100 names is marked cut off, and a cut-off root is no reading", {
  node <- hyd1d()
  node$repository$vignettes$blobs$nodes <- lapply(sprintf("v%03d.Rmd", 1:100), function(n) list(name = n))
  expect_true("vignettes" %in% gl_parse_contents(gl_ok(list(node)), hyd1d_row)[[1]]$truncated)
  node$repository$root$blobs$nodes <- lapply(sprintf("f%03d", 1:100), function(n) list(name = n))
  got <- gl_parse_contents(gl_ok(list(node)), hyd1d_row)
  expect_length(got, 0L); expect_identical(attr(got, "unreadable"), hyd1d_row$repo_id)
})

test_that("the collector retries an old host once without its missing fields and keeps them off", {
  rej <- gl_contents_fixture("contents-leipzig-rejected.json"); ok <- gl_contents_fixture("contents-leipzig-retry.json")
  f <- fake_gitlab(function(domain, query, variables) if (grepl("forkedFrom", query, fixed = TRUE)) rej else ok)
  roster <- one(sprintf("git.sc.uni-leipzig.de/o/r%02d", 1:21), sprintf("gitlab:git.sc.uni-leipzig.de:%d", 1183 + 1:21))
  got <- gl_collect_contents(f$io, roster, gl_free_pacer())
  q <- vapply(f$calls$graphql, `[[`, "", "query")
  expect_length(q, 3L)
  expect_false(any(grepl("forkedFrom", q[-1], fixed = TRUE)))
  expect_setequal(got$dropped[["git.sc.uni-leipzig.de"]], GITLAB_OPTIONAL_FIELDS)
  expect_named(got$trees, "git.sc.uni-leipzig.de/o/r01"); expect_equal(nrow(got$failed), 0L)
})

test_that("a host whose schema lacks a required field fails without asking again", {
  bad <- list(data = NULL, status = 200L, error = NA_character_, errors = list(list(
    message = "Field 'isForked' doesn't exist on type 'Project'",
    extensions = list(code = "undefinedField", fieldName = "isForked"))))
  f <- fake_gitlab(function(...) bad)
  roster <- one(sprintf("h.example/o/r%02d", 1:25), sprintf("gitlab:h.example:%d", 1:25))
  got <- gl_collect_contents(f$io, roster, gl_free_pacer())
  expect_length(f$calls$graphql, 1L)
  expect_setequal(got$failed$repo_id, roster$repo_id); expect_true(all(got$failed$query == "gitlab-contents"))
})

test_that("a batch that times out is halved down to single projects, and only the bad one fails", {
  roster <- one(sprintf("gitlab.opencode.de/o/r%02d", 1:20), sprintf("gitlab:gitlab.opencode.de:%d", 1:20))
  run <- function(bad) {
    f <- fake_gitlab(function(domain, query, variables) {
      ids <- unlist(variables$ids)
      if (any(ids == sprintf("gid://gitlab/Project/%d", bad))) return(list(error = "Operation timed out", status = NA_integer_))
      gl_ok(lapply(ids, function(i) utils::modifyList(hyd1d(), list(id = i))))
    })
    gl_collect_contents(f$io, roster, gl_free_pacer())
  }
  for (bad in c(1L, 20L)) {
    got <- run(bad)
    expect_length(got$trees, 19L)
    expect_identical(got$failed$repo_id, roster$repo_id[bad])
    expect_match(got$failed$error, "timed out")
  }
})

test_that("GitHub workflow files on a GitLab repository are read in a second request", {
  node <- hyd1d()
  node$repository$github$trees$nodes <- list(list(name = "workflows"))
  node$repository$workflows$blobs$nodes <- list(list(name = "R-CMD-check.yaml"))
  wf <- list(id = node$id, repository = list(blobs = list(nodes = list(list(path = ".github/workflows/R-CMD-check.yaml",
    rawTextBlob = "jobs:\n  check:\n    steps:\n      - uses: r-lib/actions/check-r-package@v2\n")))))
  run <- function(fail) {
    f <- fake_gitlab(function(domain, query, variables)
      if (is.null(variables$paths)) gl_ok(list(node)) else if (fail) list(error = "HTTP 502", status = 502L) else gl_ok(list(wf)))
    list(f = f, el = gl_collect_contents(f$io, hyd1d_row, gl_free_pacer())$trees[[1]])
  }
  a <- run(FALSE)
  expect_identical(unlist(a$f$calls$graphql[[2]]$variables$paths), ".github/workflows/R-CMD-check.yaml")
  dv <- classify_dev_tooling(a$el$root_entries, a$el$github_entries, repo = a$el)
  expect_identical(c(dv$ci_github_actions, dv$ci_rcmdcheck), c(1L, 1L))
  expect_identical(dv$ci_workflow_files, '["R-CMD-check.yaml"]')
  b <- run(TRUE)
  expect_true(is.na(classify_dev_tooling(b$el$root_entries, b$el$github_entries, repo = b$el)$ci_rcmdcheck))
})

test_that("a project with no YAML workflows reads as having none, with no second request", {
  node <- hyd1d()
  f <- fake_gitlab(function(domain, query, variables) gl_ok(list(node)))
  got <- gl_collect_contents(f$io, hyd1d_row, gl_free_pacer())
  expect_length(f$calls$graphql, 1L)
  expect_true("workflows" %in% names(got$trees[[1]])); expect_null(got$trees[[1]]$workflows)
})

test_that("gl_pacer takes a run budget and keeps its default cap without one", {
  p <- gl_pacer(now = function() 0, sleep = function(s) NULL, run_budget = 2L)
  expect_identical(c(p$before("h"), p$before("h"), p$before("h")), c(TRUE, TRUE, FALSE))
  d <- gl_pacer(now = function() 0, sleep = function(s) NULL)
  asked <- vapply(seq_len(GITLAB_MAX_REQUESTS + 1L), function(i) d$before("h"), NA)
  expect_identical(sum(asked), GITLAB_MAX_REQUESTS)
})

test_that("a project is failed with a request cap reason when the pacer refuses it", {
  f <- fake_gitlab(function(domain, query, variables) gl_ok(list()))
  p <- gl_pacer(now = function() 0, sleep = function(s) NULL, run_budget = 0L)
  got <- gl_collect_contents(f$io, hyd1d_row, p)
  expect_length(f$calls$graphql, 0L)
  expect_identical(got$failed$repo_id, hyd1d_row$repo_id); expect_identical(got$failed$error, "request cap")
})

test_that("the .gitlab/duo files the read returned join the root entries", {
  duo <- function(paths) {
    node <- hyd1d()
    node$repository$gitlab$trees$nodes <- list(list(name = "duo"))
    node$repository$blobs$nodes <- c(node$repository$blobs$nodes,
                                     lapply(paths, function(p) list(path = p, rawTextBlob = "x")))
    gl_parse_contents(gl_ok(list(node)), hyd1d_row)[[1]]
  }
  review <- ".gitlab/duo/mr-review-instructions.yaml"
  only <- duo(review)
  expect_true(all(c(".gitlab/duo", review) %in% only$root_entries))
  expect_false(".gitlab/duo/chat-rules.md" %in% only$root_entries)
  # The folder rule: a review-only folder counts for nothing, one with another file counts.
  expect_false("gitlab-duo" %in% classify_tree_markers(only$root_entries, only$github_entries)$tool)
  none <- duo(character(0))
  f <- classify_tree_markers(none$root_entries, none$github_entries)
  expect_identical(f$marker[f$tool == "gitlab-duo"], ".gitlab/duo")
  unk <- duo(c(review, ".gitlab/duo/chat-rules.md"))
  expect_true(".gitlab/duo/chat-rules.md" %in% unk$root_entries)
})

test_that("the CI config path is the default for a default project, the custom path, or NA when not read", {
  node <- hyd1d()
  read <- function(n) gl_parse_contents(gl_ok(list(n)), hyd1d_row)[[1]]$gitlab_ci$config_path
  expect_identical(read(node), ".gitlab-ci.yml")
  node$ciConfigPathOrDefault <- "ci/main.yml"; expect_identical(read(node), "ci/main.yml")
  node$ciConfigPathOrDefault <- NULL; expect_true(is.na(read(node)))
})

test_that("a project the answer leaves out is no element and no failure, and a null repository is unreadable", {
  node <- hyd1d()
  roster <- one(c(hyd1d_row$repo_id, "gitlab.opencode.de/o/gone"),
                c(hyd1d_row$node_id, "gitlab:gitlab.opencode.de:1"))
  got <- gl_parse_contents(gl_ok(list(node)), roster)
  expect_named(got, hyd1d_row$repo_id); expect_length(attr(got, "unreadable"), 0L)
  node$repository <- NULL
  got <- gl_parse_contents(gl_ok(list(node)), hyd1d_row)
  expect_length(got, 0L); expect_identical(attr(got, "unreadable"), hyd1d_row$repo_id)
})

test_that("the query asks for no page info and leaves the dropped fields out", {
  q <- gl_contents_query()
  expect_false(grepl("pageInfo", q, fixed = TRUE))
  expect_true(all(vapply(c("forkedFrom", "pagesDeployments", "issuesEnabled", "ciConfigPathOrDefault", "blobs(paths:"),
                         grepl, NA, x = q, fixed = TRUE)))
  expect_true(all(vapply(unname(GITLAB_TREE_ALIASES), function(p) grepl(sprintf('tree(path: "%s")', p), q, fixed = TRUE), NA)))
  expect_false(grepl("pagesDeployments", gl_contents_query("pagesDeployments"), fixed = TRUE))
})

test_that("the fixtures hold no email address", {
  for (f in list.files(test_path("fixtures", "gitlab"), pattern = "^contents-", full.names = TRUE))
    expect_false(any(grepl("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", readLines(f, warn = FALSE))))
})

# ---- commit-message search ----------------------------------------------------
# GitLab hands the regexp to git's commit grep: basic syntax, \| between alternatives, any case
# (measured on gitlab.com 2026-10-20). Each alternative reads the same in that grep and in PCRE.
gitlab_grep <- function(re, x)
  Reduce(`|`, lapply(strsplit(re, "\\|", fixed = TRUE)[[1]], grepl, x = x, ignore.case = TRUE, perl = TRUE))

# Footers as the tools write them, at least one per message rule.
footers <- c(
  "msg.claude.coauthor" = "Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>",
  "msg.claude.coauthor" = "Co-Authored-By: Claude <noreply@anthropic.com>",
  "msg.claude.coauthor" = "Co-authored-by:Claude Opus 5 <noreply@anthropic.com>",
  "msg.claude.generated" = "\U0001F916 Generated with [Claude Code](https://claude.com/claude-code)",
  "msg.claude.generated" = "Generated with [Claude Code](https://claude.ai/code)",
  "msg.replit.author" = "Replit-Commit-Author: Agent",
  "msg.codex.coauthor" = "Co-authored-by: Codex <codex@openai.com>",
  "msg.codex.cli" = "Co-authored-by: codex-cli <noreply@openai.com>",
  "msg.cursor.agent-coauthor" = "Co-authored-by: Cursor Agent <cursoragent@cursor.com>",
  "msg.cursor.at-agent" = "Co-authored-by: Cursor <cursor@agent>",
  "msg.devin.bot-coauthor" = "Co-Authored-By: Devin AI <158243242+devin-ai-integration[bot]@users.noreply.github.com>",
  "msg.openhands.coauthor" = "Co-authored-by: openhands <openhands@all-hands.dev>",
  "msg.jules.coauthor" = "Co-authored-by: google-labs-jules[bot] <161369871+google-labs-jules[bot]@users.noreply.github.com>",
  "msg.windsurf.coauthor" = "Co-Authored-By: Windsurf <noreply@windsurf.com>",
  "msg.windsurf.bot" = "Co-authored-by: windsurf-bot[bot] <189301087+windsurf-bot[bot]@users.noreply.github.com>",
  "msg.antigravity.coauthor" = "Co-authored-by: Antigravity <antigravity@google.com>",
  "msg.gemini.coauthor" = "Co-authored-by: Gemini <gemini@google.com>",
  "msg.gemini.bot" = "Co-authored-by: gemini-cli[bot] <218195315+gemini-cli[bot]@users.noreply.github.com>",
  "msg.aider.coauthor" = "Co-authored-by: aider (anthropic/claude-sonnet-4-5) <aider@aider.chat>",
  "msg.claude.address" = "Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>",
  "msg.claude.session" = "Claude-Session: https://claude.ai/code/session_011CUuzzYbPz",
  "msg.cursor.made-with" = "Made-with: Cursor",
  "msg.cursor.made-with" = "Made-with:Cursor",
  "msg.copilot.cli" = "Co-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>",
  "msg.copilot.vscode" = "Co-authored-by: Copilot <copilot@github.com>",
  "msg.copilot.cloud-coauthor" = "Co-authored-by: Copilot <198982749+Copilot@users.noreply.github.com>",
  "msg.devin.generated" = "Generated with [Devin](https://devin.ai)",
  "msg.devin.cognition" = "Co-Authored-By: Devin <devin@cognition.ai>",
  "msg.windsurf.cascade" = "Co-authored-by: Cascade <cascade@codeium.com>",
  "msg.any.assisted-by" = "Assisted-by: Claude Code",
  "msg.any.assisted-by" = "Assisted-by: Crush:kimi-k3",
  "msg.corteza.address" = "Co-authored-by: Corteza <noreply@cornball.ai>",
  "msg.eca.address" = "Co-authored-by: ECA <git@eca.dev>",
  "msg.warp.address" = "Co-Authored-By: Warp <agent@warp.dev>",
  "msg.qwen.address" = "Co-authored-by: Qwen-Coder <qwen-coder@alibabacloud.com>",
  "msg.crush.address" = "Co-Authored-By: Crush <crush@charm.land>",
  "msg.amp.address" = "Co-authored-by: Amp <amp@ampcode.com>",
  "msg.vibe.address" = "Co-Authored-By: Mistral Vibe <vibe@mistral.ai>",
  "msg.factory.bot" = "Co-authored-by: factory-droid[bot] <138933559+factory-droid[bot]@users.noreply.github.com>",
  "msg.grok.address" = "Co-authored-by: Grok <grok@x.ai>",
  "msg.kimi.address" = "Co-Authored-By: Kimi <noreply@moonshot.ai>",
  "msg.continue.address" = "Co-authored-by: Continue <noreply@continue.dev>",
  "msg.augment.address" = "Co-authored-by: Augment Code <noreply@augmentcode.com>",
  "msg.opencode.address" = "Co-Authored-By: opencode <noreply@opencode.ai>",
  "review.gemini-code-assist.coauthor" = "Co-authored-by: gemini-code-assist[bot] <176961590+gemini-code-assist[bot]@users.noreply.github.com>",
  "review.copilot.suggestion" = "Co-authored-by: Copilot <175728472+Copilot@users.noreply.github.com>")
msg_keys <- c(vapply(AI_TRAILER_PATTERNS, `[[`, "", "key"), vapply(AI_REVIEW_RULES, `[[`, "", "key"))
footer_commits <- function(msgs) data.frame(oid = sprintf("s%03d", seq_along(msgs)), committed_at = "2026-03-02T09:00:00Z",
  message = unname(msgs), author_name = NA_character_, author_email = NA_character_, stringsAsFactors = FALSE)

test_that("the prefilter catches every rule's own phrase and holds no lookaround or flag group", {
  re <- gl_message_regex()
  for (r in c(AI_TRAILER_PATTERNS, AI_REVIEW_RULES))
    expect_true(gitlab_grep(re, gsub('"', "", r$query)), info = r$key)
  expect_true(gitlab_grep(re, "Co-authored-by:Claude Opus 5"))
  expect_false(grepl("(?", re, fixed = TRUE))
})

test_that("each prefilter alternative is plain text, one-character classes and word gaps, read alike by RE2 and git", {
  alts <- strsplit(gl_message_regex(), "\\|", fixed = TRUE)[[1]]
  expect_false(anyDuplicated(alts) > 0L)
  rest <- gsub("[^A-Za-z0-9]*", "", alts, fixed = TRUE)
  rest <- gsub("\\[[^]^\\\\]\\]", "", rest)
  rest <- gsub("\\\\[][\\\\^]", "", rest)
  expect_false(any(grepl("[][\\\\.+*?(){}|^$]", rest)), info = paste(rest, collapse = " "))
  expect_true("223556219[+]Copilot" %in% alts)
})

test_that("real footers are credits for their rule and pass the prefilter, the bracketed Claude Code one too", {
  expect_true(all(msg_keys %in% names(footers)))
  re <- gl_message_regex()
  for (i in seq_along(footers)) {
    msg <- paste0("Fix the parser\n\n", footers[[i]])
    expect_true(names(footers)[i] %in% match_commit_findings(footer_commits(msg))$rule_key, info = footers[[i]])
    expect_true(gitlab_grep(re, msg), info = footers[[i]])
  }
})

test_that("a whole search through GitLab's grep writes a hit, never none, for every rule with a credit in the history", {
  msgs <- c(paste0("Change ", seq_along(footers), "\n\n", footers), "Update README", "Merge branch 'main'")
  f <- fake_gitlab(function(domain, query, variables) {
    keep <- which(gitlab_grep(variables$re, msgs))
    gl_ok(lapply(unlist(variables$ids), function(gid) list(id = gid, repository = list(commits = list(
      pageInfo = list(hasNextPage = FALSE),
      nodes = lapply(keep, function(i) list(sha = sprintf("s%03d", i), committedDate = "2026-03-02T09:00:00Z",
                                            message = msgs[i])))))))
  })
  got <- gl_collect_messages(f$io, one("gitlab.com/o/p", "gitlab:gitlab.com:7"), gl_free_pacer())
  rd <- got$reads[["gitlab.com/o/p"]]
  expect_true(rd$complete); expect_equal(nrow(rd$commits), length(footers))
  rows <- gl_message_log_rows(match_commit_findings(rd$commits), "gitlab.com/o/p", "2026-10-25", rd$complete)
  expect_true(all(msg_keys %in% rows$rule_key))
  expect_true(all(rows$outcome == "hit"))
})

test_that("the message query asks for no author and takes a cursor only for a follow-up page", {
  q <- gl_message_query(); qa <- gl_message_query(after = TRUE)
  expect_true(grepl("$re: UntrustedRegexp!", q, fixed = TRUE))
  expect_true(grepl('commits(ref: "HEAD", query: $re, first: 100)', q, fixed = TRUE))
  expect_true(grepl("nodes { sha committedDate message }", q, fixed = TRUE))
  expect_false(grepl("after", q, fixed = TRUE)); expect_false(any(grepl("author", c(q, qa), fixed = TRUE)))
  expect_true(grepl("$after: String", qa, fixed = TRUE)); expect_true(grepl("first: 100, after: $after", qa, fixed = TRUE))
  expect_true(grepl(sprintf("projects(ids: $ids, first: %d)", GITLAB_MESSAGE_BATCH), q, fixed = TRUE))
})

no_commits <- list(data = NULL, errors = list(list(message = "Field 'commits' doesn't exist on type 'Repository'",
                                                   extensions = list(code = "undefinedField", fieldName = "commits"))))

test_that("a host without the search is recognised, a slow answer is not", {
  expect_true(gl_message_unsupported(no_commits))
  expect_true(gl_message_unsupported(list(errors = list(list(message = "Field 'commits' doesn't accept argument 'query'",
    extensions = list(code = "argumentNotAccepted", name = "commits", argumentName = "query"))))))
  expect_true(gl_message_unsupported(list(errors = list(list(message = "UntrustedRegexp isn't a defined input type (on $re)")))))
  expect_false(gl_message_unsupported(list(error = "Operation timed out after 30001 milliseconds")))
  expect_false(gl_message_unsupported(list(data = NULL, errors = list(list(
    message = "Query has complexity of 230, which exceeds max complexity of 200")))))
  expect_false(gl_message_unsupported(gl_ok(list())))
})

test_that("message commits come back in UTC with no author", {
  cm <- gl_message_commits(list(list(sha = "abc", committedDate = "2026-03-02T10:00:00+01:00",
                                     message = "x\n\nCo-Authored-By: Claude Opus 5 <noreply@anthropic.com>")))
  expect_identical(cm$committed_at, "2026-03-02T09:00:00Z")
  expect_true(is.na(cm$author_email) && is.na(cm$author_name))
  expect_setequal(match_commit_findings(cm)$rule_key, c("msg.claude.coauthor", "msg.claude.address"))
  expect_identical(nrow(gl_message_commits(list())), 0L)
})

test_that("a whole search answers every message rule, a cut-off one only its hits", {
  cm <- gl_message_commits(list(list(sha = "a1", committedDate = "2026-03-02T09:00:00Z",
    message = "x\n\nCo-Authored-By: Claude Opus 5 <noreply@anthropic.com>\nAssisted-by: Claude Code")))
  rows <- gl_message_log_rows(match_commit_findings(cm), "gitlab.com/o/p", "2026-10-25", complete = TRUE)
  keys <- c(vapply(AI_TRAILER_PATTERNS, `[[`, "", "key"), vapply(AI_REVIEW_RULES, `[[`, "", "key"))
  expect_true(all(keys %in% rows$rule_key))
  co <- rows[rows$rule_key == "msg.claude.coauthor", ]
  expect_identical(list(co$outcome, co$total_count, co$verified, co$first_hit_on, co$source, co$mode),
                   list("hit", 1L, 1L, "2026-03-02", "search", "replace"))
  expect_identical(rows$outcome[rows$rule_key == "msg.cursor.made-with"], "none")
  expect_true("msg.any.assisted-by.claude" %in% rows$rule_key)
  cut <- gl_message_log_rows(match_commit_findings(cm), "gitlab.com/o/p", "2026-10-25", complete = FALSE)
  expect_true(all(cut$outcome == "hit") && all(cut$incomplete == 1L) && all(is.na(cut$first_hit_on)) && all(is.na(cut$mode)))
})

test_that("log rows carry each rule's revision and the search log's columns", {
  cm <- gl_message_commits(list(list(sha = "a1", committedDate = "2026-03-02T09:00:00Z",
    message = "x\n\nCo-Authored-By: Claude Opus 5 <noreply@anthropic.com>\nAssisted-by: Claude Code")))
  rows <- gl_message_log_rows(match_commit_findings(cm), "gitlab.com/o/p", "2026-10-25", complete = TRUE)
  expect_identical(names(rows), names(cbind(.ai_empty_log(), mode = character(), read_after = character())))
  rules <- c(AI_TRAILER_PATTERNS, AI_REVIEW_RULES)
  rev <- stats::setNames(vapply(rules, function(r) as.integer(r$rev), 1L), vapply(rules, `[[`, "", "key"))
  base <- sub("^(msg\\.any\\.assisted-by)\\..*$", "\\1", rows$rule_key)
  expect_identical(rows$rule_rev, unname(rev[base]))
  expect_true(all(rows$ruleset_version == AI_RULESET_VERSION & rows$asked_on == "2026-10-25" & rows$incomplete == 0L))
  expect_true(all(is.na(rows$read_after)))
  none <- rows[rows$outcome == "none", ]
  expect_true(all(none$total_count == 0L & is.na(none$verified) & is.na(none$first_hit_on)))
  expect_identical(nrow(gl_message_log_rows(.ai_empty_commit_hits(), "gitlab.com/o/p", "2026-10-25", complete = FALSE)), 0L)
})

test_that("a whole GitLab search measures a zero for a tool it found no credit for and counts the repository as asked", {
  r <- "gitlab.com/o/p"
  cm <- gl_message_commits(list(list(sha = "a1", committedDate = "2026-03-02T09:00:00Z",
    message = "x\n\nCo-Authored-By: Claude Opus 5 <noreply@anthropic.com>")))
  rows <- gl_message_log_rows(match_commit_findings(cm), r, "2026-10-25", complete = TRUE)
  sig <- .ai_bind_like(.ai_empty_signals(), list(data.frame(repo_id = r, tool = c("cursor", "claude"),
    evidence_tiers = "D", markers = c(".cursor", "CLAUDE.md"), stringsAsFactors = FALSE)))
  got <- derive_assisted_counts(sig, rows, .ai_empty_reads())
  expect_identical(got$assisted_commits, c(0L, 1L))
  expect_identical(got$assisted_measured_on, c("2026-10-25", "2026-10-25"))
  cov <- build_search_coverage(rows, .ai_empty_reads())
  expect_identical(cov$repos_asked[cov$rule_key %in% c("msg.cursor.made-with", "msg.claude.coauthor")], c(1L, 1L))
  expect_identical(cov$repos_hit[cov$rule_key %in% c("msg.claude.coauthor", "msg.cursor.made-with")], c(1L, 0L))
})

# One project's page `page`: one crediting commit; p1 ends on page 3, p2 never ends.
msg_page <- function(gid, page) list(id = gid, repository = list(commits = list(
  pageInfo = list(hasNextPage = !(gid == .gl_gid("gitlab:gitlab.com:1") && page >= 3L), endCursor = paste0("c", page)),
  nodes = list(list(sha = paste0(gid, "-", page), committedDate = "2026-03-02T09:00:00Z",
                    message = "x\n\nCo-Authored-By: Claude Opus 5 <noreply@anthropic.com>")))))
msg_fake <- function(fail_p1_page = NA) fake_gitlab(function(domain, query, variables) {
  if (domain == "git.sc.uni-leipzig.de") return(no_commits)
  ids <- unlist(variables$ids)
  page <- if (is.null(variables$after)) 1L else as.integer(sub("^c", "", variables$after)) + 1L
  if (identical(page, fail_p1_page) && identical(ids, .gl_gid("gitlab:gitlab.com:1"))) return(list(error = "HTTP 502", status = 502L))
  gl_ok(lapply(ids, msg_page, page = page))
})
msg_roster <- data.frame(repo_id = c("gitlab.com/o/p1", "gitlab.com/o/p2", sprintf("git.sc.uni-leipzig.de/o/q%02d", 1:12)),
                         node_id = c("gitlab:gitlab.com:1", "gitlab:gitlab.com:2", sprintf("gitlab:git.sc.uni-leipzig.de:%d", 1:12)))

test_that("the collector stops asking a host without the search and pages the others to the cap", {
  f <- msg_fake(); got <- gl_collect_messages(f$io, msg_roster, gl_free_pacer())
  expect_equal(sum(vapply(f$calls$graphql, function(x) x$domain == "git.sc.uni-leipzig.de", NA)), 1L)
  expect_identical(got$unsupported, "git.sc.uni-leipzig.de")
  expect_false(any(startsWith(names(got$reads), "git.sc.uni-leipzig.de")))
  expect_true(got$reads[["gitlab.com/o/p1"]]$complete); expect_equal(nrow(got$reads[["gitlab.com/o/p1"]]$commits), 3L)
  expect_false(got$reads[["gitlab.com/o/p2"]]$complete)
  expect_equal(nrow(got$reads[["gitlab.com/o/p2"]]$commits), GITLAB_MESSAGE_PAGE_CAP)
  follow <- Filter(function(x) !is.null(x$variables$after), f$calls$graphql)
  expect_true(all(lengths(lapply(follow, function(x) unlist(x$variables$ids))) == 1L))
})

test_that("a failed follow-up page leaves the project unsearched this week", {
  got <- gl_collect_messages(msg_fake(fail_p1_page = 2L)$io, msg_roster, gl_free_pacer())
  expect_false("gitlab.com/o/p1" %in% names(got$reads))
  expect_identical(got$failed$repo_id[got$failed$query == "gitlab-messages"], "gitlab.com/o/p1")
})

test_that("a first-page batch that times out is halved down to the one bad project", {
  roster <- one(sprintf("gitlab.com/o/r%02d", 1:10), sprintf("gitlab:gitlab.com:%d", 101:110))
  f <- fake_gitlab(function(domain, query, variables) {
    ids <- unlist(variables$ids)
    if (.gl_gid("gitlab:gitlab.com:104") %in% ids) return(list(error = "Operation timed out", status = NA_integer_))
    gl_ok(lapply(ids, function(gid) list(id = gid, repository = list(commits = list(
      pageInfo = list(hasNextPage = FALSE), nodes = list())))))
  })
  got <- gl_collect_messages(f$io, roster, gl_free_pacer())
  expect_setequal(names(got$reads), roster$repo_id[-4])
  expect_true(all(vapply(got$reads, function(x) x$complete && nrow(x$commits) == 0L, NA)))
  expect_identical(got$failed$repo_id, "gitlab.com/o/r04"); expect_match(got$failed$error, "timed out")
})

test_that("a project whose commits come back null, whose next page has no cursor, or that the pacer refuses, is a failure with no entry", {
  f <- fake_gitlab(function(domain, query, variables)
    gl_ok(lapply(unlist(variables$ids), function(gid) list(id = gid, repository = list(commits = NULL)))))
  got <- gl_collect_messages(f$io, one("gitlab.com/o/a", "gitlab:gitlab.com:5"), gl_free_pacer())
  expect_length(got$reads, 0L); expect_identical(got$failed$repo_id, "gitlab.com/o/a")
  nc <- fake_gitlab(function(domain, query, variables) gl_ok(lapply(unlist(variables$ids), function(gid)
    list(id = gid, repository = list(commits = list(pageInfo = list(hasNextPage = TRUE), nodes = list()))))))
  got <- gl_collect_messages(nc$io, one("gitlab.com/o/a", "gitlab:gitlab.com:5"), gl_free_pacer())
  expect_length(got$reads, 0L); expect_identical(got$failed$error, "no cursor for the next page")
  expect_length(nc$calls$graphql, 1L)
  p <- gl_pacer(now = function() 0, sleep = function(s) NULL, run_budget = 0L)
  got <- gl_collect_messages(f$io, one("gitlab.com/o/a", "gitlab:gitlab.com:5"), p)
  expect_length(got$reads, 0L)
  expect_identical(got$failed$error, "request cap"); expect_identical(got$failed$query, "gitlab-messages")
})
