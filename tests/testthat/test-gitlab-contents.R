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

test_that("a batch that times out is halved down to single projects", {
  f <- fake_gitlab(function(domain, query, variables) {
    ids <- unlist(variables$ids)
    if (length(ids) > 5L) return(list(error = "Operation timed out", status = NA_integer_))
    gl_ok(lapply(ids, function(i) utils::modifyList(hyd1d(), list(id = i))))
  })
  roster <- one(sprintf("gitlab.opencode.de/o/r%02d", 1:20), sprintf("gitlab:gitlab.opencode.de:%d", 1:20))
  got <- gl_collect_contents(f$io, roster, gl_free_pacer())
  expect_length(got$trees, 20L); expect_equal(nrow(got$failed), 0L)
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
    node$repository$blobs$nodes <- c(node$repository$blobs$nodes,
                                     lapply(paths, function(p) list(path = p, rawTextBlob = "x")))
    gl_parse_contents(gl_ok(list(node)), hyd1d_row)[[1]]
  }
  only <- duo(".gitlab/duo/mr-review-instructions.yaml")
  expect_true(".gitlab/duo/mr-review-instructions.yaml" %in% only$root_entries)
  expect_false(".gitlab/duo/chat-rules.md" %in% only$root_entries)
  unk <- duo(c(".gitlab/duo/mr-review-instructions.yaml", ".gitlab/duo/chat-rules.md"))
  expect_true(all(c(".gitlab/duo/mr-review-instructions.yaml", ".gitlab/duo/chat-rules.md") %in% unk$root_entries))
  # The folder rule: a review-only folder counts for nothing, one with another file counts.
  expect_false("gitlab-duo" %in% classify_tree_markers(only$root_entries, only$github_entries)$tool)
  expect_true("gitlab-duo" %in% classify_tree_markers(unk$root_entries, unk$github_entries)$tool)
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
