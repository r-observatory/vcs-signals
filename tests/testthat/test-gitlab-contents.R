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

# The reason each project failed, in roster order.
why_failed <- function(got, ids) unname(stats::setNames(got$failed$error, got$failed$repo_id)[ids])
deferred_why <- "host deferred after repeated failures"

test_that("projects refused after the breaker deferred their host say so, not request cap", {
  roster <- one(sprintf("gitlab.com/o/r%02d", 1:5), sprintf("gitlab:gitlab.com:%d", 101:105))
  down <- fake_gitlab(function(domain, query, variables) list(error = "HTTP 502", status = 502L))
  # r01 to r03 fail one at a time and defer the host; r04 and r05 are never asked.
  expect_identical(why_failed(gl_collect_contents(down$io, roster, gl_free_pacer()), roster$repo_id),
                   c(rep("HTTP 502", 3), rep(deferred_why, 2)))
  expect_identical(why_failed(gl_collect_messages(down$io, roster, gl_free_pacer()), roster$repo_id),
                   c(rep("HTTP 502", 3), rep(deferred_why, 2)))
})

test_that("a follow-up page refused after the breaker deferred the host says so, and one past the cap says request cap", {
  roster <- one(sprintf("gitlab.com/o/r%02d", 1:4), sprintf("gitlab:gitlab.com:%d", 101:104))
  f <- fake_gitlab(function(domain, query, variables) {
    if (!is.null(variables$after)) return(list(error = "HTTP 502", status = 502L))
    gl_ok(lapply(unlist(variables$ids), function(gid) list(id = gid, repository = list(commits = list(
      pageInfo = list(hasNextPage = TRUE, endCursor = "c1"), nodes = list())))))
  })
  expect_identical(why_failed(gl_collect_messages(f$io, roster, gl_free_pacer()), roster$repo_id),
                   c(rep("HTTP 502", 3), deferred_why))
  capped <- gl_pacer(now = function() 0, sleep = function(s) NULL, run_budget = 1L)
  expect_identical(why_failed(gl_collect_messages(f$io, roster[1, ], capped), roster$repo_id[1]), "request cap")
})

duo_node <- function() {
  node <- hyd1d()
  node$repository$gitlab$trees$nodes <- list(list(name = "duo"))
  node
}
duo_answer <- function(node, paths) gl_ok(list(list(id = node$id, repository = list(blobs = list(nodes =
  lapply(paths, function(p) list(path = p, rawTextBlob = "x")))))))

test_that("GitLab Duo files are read in a second request where .gitlab lists duo, and read as the single query did", {
  review <- ".gitlab/duo/mr-review-instructions.yaml"; chat <- ".gitlab/duo/chat-rules.md"
  run <- function(paths, fail = FALSE) {
    node <- duo_node()
    f <- fake_gitlab(function(domain, query, variables)
      if (is.null(variables$paths)) gl_ok(list(node))
      else if (fail) list(error = "HTTP 502", status = 502L) else duo_answer(node, paths))
    list(f = f, got = gl_collect_contents(f$io, hyd1d_row, gl_free_pacer()))
  }
  # The single query returned the Duo files with the others, and they joined the end of the root entries.
  single <- function(paths) {
    el <- gl_parse_contents(gl_ok(list(duo_node())), hyd1d_row)[[1]]
    el$root_entries <- c(el$root_entries, paths)
    el
  }
  for (paths in list(character(0), review, c(review, chat))) {
    a <- run(paths)
    expect_length(a$f$calls$graphql, 2L)
    expect_setequal(unlist(a$f$calls$graphql[[2]]$variables$paths), GITLAB_DUO_FILES)
    el <- a$got$trees[[1]]; ref <- single(paths)
    expect_identical(el$root_entries, ref$root_entries)
    expect_identical(classify_dev_tooling(el$root_entries, el$github_entries, repo = el),
                     classify_dev_tooling(ref$root_entries, ref$github_entries, repo = ref))
    expect_identical(assemble_repo_evidence(el, scanned_on = "2026-10-20"), assemble_repo_evidence(ref, scanned_on = "2026-10-20"))
  }
  # The folder rule: a review-only folder counts for nothing, one with another file or an unread one counts.
  ev <- function(paths) assemble_repo_evidence(run(paths)$got$trees[[1]], scanned_on = "2026-10-20")
  expect_false("gitlab-duo" %in% ev(review)$tool)
  expect_identical(ev(review)$tool[ev(review)$role == "review"], "gitlab-duo-review")
  expect_identical(ev(character(0))$marker[ev(character(0))$tool == "gitlab-duo"], ".gitlab/duo")
  expect_true(chat %in% ev(c(review, chat))$marker)
  # A failed second request leaves the project unread rather than counting its folder alone.
  b <- run(chat, fail = TRUE)
  expect_length(b$got$trees, 0L)
  expect_identical(b$got$failed$repo_id, hyd1d_row$repo_id); expect_match(b$got$failed$error, "Duo files not read")
  # A project whose .gitlab lists no duo costs no second request.
  f <- fake_gitlab(function(domain, query, variables) gl_ok(list(hyd1d())))
  gl_collect_contents(f$io, hyd1d_row, gl_free_pacer())
  expect_length(f$calls$graphql, 1L)
})

test_that("a Duo read refused after the breaker deferred the host says so, and one past the cap says request cap", {
  roster <- rbind(hyd1d_row, one(sprintf("gitlab.opencode.de/o/r%d", 2:4), sprintf("gitlab:gitlab.opencode.de:%d", 2:4)))
  f <- fake_gitlab(function(domain, query, variables) {
    if (!is.null(variables$paths)) return(duo_answer(duo_node(), ".gitlab/duo/chat-rules.md"))
    if (identical(unlist(variables$ids), duo_node()$id)) gl_ok(list(duo_node())) else list(error = "HTTP 502", status = 502L)
  })
  # hyd1d is read, then r2 to r4 fail one at a time and defer the host before its Duo files are asked.
  got <- gl_collect_contents(f$io, roster, gl_free_pacer())
  expect_length(got$trees, 0L)
  expect_identical(why_failed(got, roster$repo_id), c(deferred_why, rep("HTTP 502", 3)))
  capped <- gl_pacer(now = function() 0, sleep = function(s) NULL, run_budget = 1L)
  expect_identical(why_failed(gl_collect_contents(f$io, hyd1d_row, capped), hyd1d_row$repo_id), "request cap")
})

# A host that answers GitLab's complexity error for any query asking more than `max_first` projects.
complex_host <- function(max_first) fake_gitlab(function(domain, query, variables) {
  n <- as.integer(sub("(?s)^.*projects[(]ids: [$]ids, first: ([0-9]+)[)].*$", "\\1", query, perl = TRUE))
  if (n > max_first) return(list(data = NULL, status = 200L, error = NA_character_, errors = list(list(
    message = sprintf("Query has complexity of %d, which exceeds max complexity of 200", 190L + n)))))
  gl_ok(lapply(unlist(variables$ids), function(i) utils::modifyList(hyd1d(), list(id = i))))
})
complex_run <- function(max_first) {
  roster <- one(sprintf("h.example/o/r%02d", 1:45), sprintf("gitlab:h.example:%d", 1:45))
  f <- complex_host(max_first)
  kept <- new.env(); kept$lines <- character(0)
  p <- gl_free_pacer()
  got <- gl_collect_contents(f$io, roster, p, log = function(m) kept$lines <- c(kept$lines, m))
  firsts <- vapply(f$calls$graphql, function(x) as.integer(sub("(?s)^.*projects[(]ids: [$]ids, first: ([0-9]+)[)].*$", "\\1", x$query, perl = TRUE)), 1L)
  list(got = got, lines = kept$lines, deferred = p$deferred(), firsts = firsts)
}

test_that("a host that refuses the query as too complex is asked half as many at a time, and every project is read", {
  a <- complex_run(10L)
  expect_length(a$got$trees, 45L); expect_equal(nrow(a$got$failed), 0L)
  expect_identical(a$firsts, c(20L, rep(10L, 5L)))
  expect_length(a$deferred, 0L)
  said <- grep("complexity|scored", a$lines, value = TRUE)
  expect_length(said, 1L)
  expect_match(said, "GitLab h.example: the contents query scored 210 at 20 projects a request, over the limit of 200, so it asked 10 at a time", fixed = TRUE)
  # Three refusals in a row (20, 10 and 5) never trip the breaker.
  b <- complex_run(2L)
  expect_length(b$got$trees, 45L); expect_equal(nrow(b$got$failed), 0L)
  expect_identical(b$firsts[1:3], c(20L, 10L, 5L)); expect_true(all(b$firsts[-(1:3)] == 2L))
  expect_length(b$deferred, 0L)
  expect_match(grep("scored", b$lines, value = TRUE), "so it asked 2 at a time", fixed = TRUE)
})

test_that("a query too complex even for one project fails the host's projects without asking again", {
  a <- complex_run(0L)
  expect_identical(a$firsts, c(20L, 10L, 5L, 2L, 1L))
  expect_length(a$got$trees, 0L)
  expect_equal(nrow(a$got$failed), 45L); expect_true(all(grepl("query complexity 191 over the limit of 200", a$got$failed$error)))
  expect_length(a$deferred, 0L)
  expect_length(grep("even for one project", a$lines), 1L)
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
  expect_false(any(vapply(GITLAB_DUO_FILES, grepl, NA, x = q, fixed = TRUE)))
  expect_true(grepl("projects(ids: $ids, first: 7)", gl_contents_query(first = 7L), fixed = TRUE))
  expect_true(grepl(sprintf("first: %d)", GITLAB_CONTENTS_BATCH), q, fixed = TRUE))
})

test_that("GitLab's complexity error is read for its score and limit, and nothing else is", {
  err <- function(m) list(data = NULL, status = 200L, error = NA_character_, errors = list(list(message = m)))
  expect_identical(gl_complexity_refusal(err("Query has complexity of 201, which exceeds max complexity of 200")),
                   c(score = 201L, limit = 200L))
  expect_null(gl_complexity_refusal(err("Field 'forkedFrom' doesn't exist on type 'Project'")))
  expect_null(gl_complexity_refusal(gl_ok(list())))
  expect_null(gl_complexity_refusal(list(error = "curl timed out (exit 28)", status = NA_integer_)))
})

test_that("the fixtures hold no email address", {
  for (f in list.files(test_path("fixtures", "gitlab"), pattern = "^contents-", full.names = TRUE))
    expect_false(any(grepl("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", readLines(f, warn = FALSE))))
})

# ---- commit-message search ----------------------------------------------------
# GitLab hands the regexp to git's commit grep: basic syntax, \| between alternatives (measured on
# gitlab.com 2026-10-20). Case is matched as written, since the query cannot ask to ignore it.
# Each alternative reads the same in that grep and in PCRE.
gitlab_grep <- function(re, x)
  Reduce(`|`, lapply(strsplit(re, "\\|", fixed = TRUE)[[1]], grepl, x = x, perl = TRUE))

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
  "msg.windsurf.coauthor" = "Co-authored-by: Cascade <cascade@windsurf.com>",
  "msg.windsurf.coauthor" = "Co-authored-by: SWE-1 <noreply@windsurf.ai>",
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

test_that("each prefilter alternative holds only two-case letter classes, one-character classes, escapes and word gaps", {
  alts <- strsplit(gl_message_regex(), "\\|", fixed = TRUE)[[1]]
  expect_false(anyDuplicated(alts) > 0L)
  rest <- gsub("[^A-Za-z0-9]*", "", alts, fixed = TRUE)
  pairs <- regmatches(rest, gregexpr("\\[[A-Za-z][A-Za-z]\\]", rest))
  expect_true(all(vapply(unlist(pairs), function(p) substr(p, 2, 2) == toupper(substr(p, 3, 3)) &&
                                                     substr(p, 3, 3) == tolower(substr(p, 2, 2)), NA)))
  rest <- gsub("\\[[A-Za-z][A-Za-z]\\]", "", rest)
  rest <- gsub("\\[[^]^\\\\A-Za-z]\\]", "", rest)
  rest <- gsub("\\\\[][\\\\^]", "", rest)
  expect_false(any(grepl("[A-Za-z]", rest)), info = paste(rest, collapse = " "))
  expect_false(any(grepl("[][\\\\.+*?(){}|^$]", rest)), info = paste(rest, collapse = " "))
  expect_true("223556219[+][Cc][Oo][Pp][Ii][Ll][Oo][Tt]" %in% alts)
})

test_that("the prefilter matches a footer whatever its case, without the host ignoring case", {
  re <- gl_message_regex()
  expect_false(gitlab_grep("Co-Authored-By", "co-authored-by"))
  odd <- c("msg.codex.coauthor" = "CO-AUTHORED-BY: CODEX <codex@openai.com>",
           "msg.cursor.at-agent" = "co-authored-by: cursor <cursor@agent>",
           "msg.gemini.coauthor" = "co-Authored-BY: gemini <gemini@google.com>",
           "msg.cursor.made-with" = "MADE-WITH: CURSOR")
  for (i in seq_along(odd)) {
    msg <- paste0("Fix the parser\n\n", odd[[i]])
    expect_true(names(odd)[i] %in% match_commit_findings(footer_commits(msg))$rule_key, info = odd[[i]])
    expect_true(gitlab_grep(re, msg), info = odd[[i]])
  }
})

test_that("a windsurf address under another name passes the prefilter, and GitHub's search phrases stay as they were", {
  re <- gl_message_regex()
  cascade <- "Fix\n\nCo-authored-by: Cascade <cascade@windsurf.com>"
  swe <- "Fix\n\nCo-authored-by: SWE-1 <noreply@windsurf.ai>"
  expect_setequal(match_commit_findings(footer_commits(cascade))$rule_key, c("msg.windsurf.coauthor", "msg.windsurf.cascade"))
  expect_identical(match_commit_findings(footer_commits(swe))$rule_key, "msg.windsurf.coauthor")
  expect_true(gitlab_grep(re, cascade)); expect_true(gitlab_grep(re, swe))
  expect_false(gitlab_grep(gl_message_regex(extra = character(0)), cascade))
  queries <- vapply(c(AI_TRAILER_PATTERNS, AI_REVIEW_RULES), `[[`, "", "query")
  expect_false(any(grepl("@windsurf", queries, fixed = TRUE)))
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

# ---- onset dating over REST ----------------------------------------------------
test_that("a GitLab file is dated by its oldest commit, without following renames", {
  f <- fake_gitlab(rest = function(domain, path)
    if (grepl("[?&]page=1(&|$)", path)) rest_ok(commits_body("s2", "2026-03-01T10:00:00.000+01:00"), "2")
    else rest_ok(commits_body("s1", "2026-01-24T13:59:09.000+01:00")))
  got <- gl_file_onset(f$io, "gitlab.com", 85027322, ".claude/skills", free_rest_pacer())
  expect_identical(got, list(date = "2026-01-24T12:59:09Z", stopped = FALSE))
  expect_true(all(grepl("follow=false", f$calls$rest, fixed = TRUE)))
  expect_true(all(grepl("path=.claude%2Fskills", f$calls$rest, fixed = TRUE)))
  bad <- fake_gitlab(rest = function(domain, path) if (grepl("[?&]page=1(&|$)", path))
    rest_ok(commits_body("s2", "2026-03-01T10:00:00Z"), "2") else list(status = 502L, error = "HTTP 502"))
  expect_identical(gl_file_onset(bad$io, "gitlab.com", 1, "CLAUDE.md", free_rest_pacer()),
                   list(date = NA_character_, stopped = FALSE))
  p <- free_rest_pacer(); p$set_hourly_budget("gitlab.com", 0L)
  expect_identical(gl_file_onset(f$io, "gitlab.com", 1, "CLAUDE.md", p), list(date = NA_character_, stopped = TRUE))
})

test_that("an ignore line on GitLab is dated by bisecting the file's revisions", {
  hist <- commits_body(c("g3", "g2", "g1"), c("2026-05-03T00:00:00Z", "2026-05-02T00:00:00Z", "2026-05-01T00:00:00Z"))
  fake <- function(has) fake_gitlab(rest = function(domain, path) {
    if (grepl("/repository/commits", path, fixed = TRUE)) return(rest_ok(hist))
    sha <- sub("^.*ref=", "", path)
    rest_ok(if (sha %in% has) "*.o\n.claude\n" else "*.o\n")
  })
  expect_identical(gl_ignore_line_onset(fake(c("g2", "g3"))$io, "code.usgs.gov", 4711, ".gitignore", ".claude", free_rest_pacer()),
                   list(date = "2026-05-02T00:00:00Z", exact = TRUE, stopped = FALSE))
  expect_false(gl_ignore_line_onset(fake(c("g1", "g2", "g3"))$io, "code.usgs.gov", 4711, ".gitignore", ".claude",
                                    free_rest_pacer())$exact)
})

test_that("a history past the page cap or a failed revision read gives no date, and a host that stops answering stops the item", {
  endless <- fake_gitlab(rest = function(domain, path) rest_ok(commits_body("s1", "2026-01-01T00:00:00Z"), "9"))
  expect_identical(gl_file_onset(endless$io, "gitlab.com", 1, "CLAUDE.md", free_rest_pacer()),
                   list(date = NA_character_, stopped = FALSE))
  expect_length(endless$calls$rest, GITLAB_ONSET_PAGE_CAP)
  limited <- fake_gitlab(rest = function(domain, path) list(status = 429L, error = "HTTP 429", headers = character(), body = ""))
  expect_identical(gl_file_onset(limited$io, "gitlab.com", 1, "CLAUDE.md", free_rest_pacer()),
                   list(date = NA_character_, stopped = TRUE))
  last <- fake_gitlab(rest = function(domain, path) {
    r <- rest_ok(commits_body("s1", "2026-01-01T00:00:00Z")); r$headers <- c(r$headers, `ratelimit-remaining` = "0"); r })
  expect_true(gl_file_onset(last$io, "gitlab.com", 1, "CLAUDE.md", free_rest_pacer())$stopped)
  hist <- commits_body(c("g2", "g1"), c("2026-05-02T00:00:00Z", "2026-05-01T00:00:00Z"))
  broken <- fake_gitlab(rest = function(domain, path)
    if (grepl("/repository/commits", path, fixed = TRUE)) rest_ok(hist) else list(status = 502L, error = "HTTP 502"))
  expect_identical(gl_ignore_line_onset(broken$io, "gitlab.com", 1, ".Rbuildignore", "CLAUDE.md", free_rest_pacer()),
                   list(date = NA_character_, exact = FALSE, stopped = FALSE))
  expect_true(any(grepl("/repository/files/.Rbuildignore/raw?ref=g", broken$calls$rest, fixed = TRUE)))
})
