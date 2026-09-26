fx <- classified_fixture()
row <- function(slug) fx[[paste0("github.com/", tolower(slug))]]

test_that("inherited community files are named as the owner's defaults", {
  r <- row("epiverse-trace/linelist")
  expect_equal(r$pr_template_source, "account_default"); expect_equal(r$has_pr_template, 1L)
  expect_equal(r$coc_source, "account_default");          expect_equal(r$has_code_of_conduct, 1L)
  expect_equal(r$contributing_source, "account_default"); expect_equal(r$has_contributing, 1L)
})

test_that("a repository's own template directory is its own, beside an inherited code of conduct", {
  r <- row("epiverse-trace/cfr")
  expect_equal(r$pr_template_source, "repo")
  expect_equal(r$coc_source, "account_default")
  expect_equal(r$contributing_source, "repo")
})

test_that("files in the repository's own .github are its own, and no template reads none", {
  r <- row("tidyverse/forcats")
  expect_equal(r$coc_source, "repo")
  expect_equal(r$contributing_source, "repo")
  expect_equal(r$pr_template_source, "none"); expect_equal(r$has_pr_template, 0L)
  expect_equal(r$has_issue_template, 1L)
})

test_that("a root CONDUCT.md counts even though GitHub does not report it", {
  r <- row("acoppock/ri2")
  expect_equal(r$coc_source, "repo"); expect_equal(r$has_code_of_conduct, 1L)
  expect_equal(r$contributing_source, "none"); expect_equal(r$has_contributing, 0L)
})

test_that("a repository that moved is compared with the name GitHub returns now", {
  # The roster still names the old owner; only GitHub's reply carries the name the files live under.
  roster <- data.frame(repo_id = "github.com/old-owner/pkg", owner = "old-owner", name = "pkg",
                       stringsAsFactors = FALSE)
  resp <- list(data = list(r0 = list(
    nameWithOwner = "new-owner/pkg", isFork = FALSE, parent = NULL,
    pullRequestTemplates = list(list(filename = "pull_request_template.md",
                                     repository = list(nameWithOwner = "new-owner/pkg"))),
    issueTemplates = list(),
    codeOfConduct = list(url = "https://github.com/new-owner/pkg/blob/main/CODE_OF_CONDUCT.md"),
    rootTree = list(entries = list(list(name = "DESCRIPTION", type = "blob"))))))
  p <- parse_tree_markers(resp, roster)[["github.com/old-owner/pkg"]]
  r <- classify_dev_tooling(p$root_entries, p$github_entries, repo = p)
  expect_equal(r$coc_source, "repo")
  expect_equal(r$pr_template_source, "repo")
})

test_that("the owner's own .github repository holds its files, it does not inherit them", {
  repo <- list(name_with_owner = "epiverse-trace/.github",
               coc_url = "https://github.com/epiverse-trace/.github/blob/main/CODE_OF_CONDUCT.md",
               contributing_url = NA_character_,
               pr_templates = data.frame(filename = "pull_request_template.md", repository = "epiverse-trace/.github"))
  r <- classify_dev_tooling(character(0), character(0), repo = repo)
  expect_equal(r$coc_source, "repo")
  expect_equal(r$pr_template_source, "repo")
})

test_that("a file the repository holds wins over the owner's default GitHub reports for it", {
  repo <- list(name_with_owner = "o/pkg",
               coc_url = "https://github.com/o/.github/blob/main/CODE_OF_CONDUCT.md",
               contributing_url = "https://github.com/o/.github/blob/main/CONTRIBUTING.md",
               pr_templates = data.frame(filename = "pull_request_template.md", repository = "o/.github"))
  r <- classify_dev_tooling(c("DESCRIPTION", "CONTRIBUTING.md"),
                            c("CODE_OF_CONDUCT.md", "pull_request_template.md"), repo = repo)
  expect_equal(r$coc_source, "repo")
  expect_equal(r$contributing_source, "repo")
  expect_equal(r$pr_template_source, "repo")
  expect_equal(r$has_code_of_conduct, 1L)
  unread <- classify_dev_tooling(c("DESCRIPTION"), c("CODE_OF_CONDUCT.md"))
  expect_equal(unread$coc_source, "repo")
  expect_true(is.na(unread$contributing_source))
})

test_that("a url pointing anywhere else is not a claim either way", {
  repo <- list(name_with_owner = "o/pkg", coc_url = "https://example.org/conduct.html",
               contributing_url = NA_character_, pr_templates = data.frame(filename = "x.md", repository = "else/where"))
  r <- classify_dev_tooling(character(0), character(0), repo = repo)
  expect_true(is.na(r$coc_source)); expect_true(is.na(r$has_code_of_conduct))
  expect_true(is.na(r$pr_template_source))
})

wf_file <- function(name) paste(readLines(test_path("fixtures", "workflows", name)), collapse = "\n")
wf_repo <- function(files, texts = files) {
  wf <- data.frame(name = texts, text = vapply(texts, wf_file, ""), stringsAsFactors = FALSE)
  list(github = c("workflows", paste0("workflows/", files)), repo = list(workflows = wf))
}
wf_row <- function(w) classify_dev_tooling(character(0), w$github, repo = w$repo)

test_that("a standard r-lib workflow set reads as check, platforms, devel, coverage, site and lint", {
  files <- c("R-CMD-check.yaml", "test-coverage.yaml", "pkgdown.yaml", "lint.yaml", "pr-commands.yaml")
  r <- wf_row(wf_repo(files))
  expect_equal(r$ci_workflow_files, as.character(jsonlite::toJSON(files)))
  expect_equal(r$ci_rcmdcheck, 1L)
  expect_equal(r$ci_platforms, '["linux","macos","windows"]')
  expect_equal(r$ci_r_devel, 1L)
  expect_equal(r$ci_coverage, 1L)
  expect_equal(r$ci_site_deploy, 1L)
  expect_equal(r$ci_lint, 1L)
})

test_that("a comment-triggered format bot is not a lint step", {
  expect_equal(wf_row(wf_repo("pr-commands.yaml"))$ci_lint, 0L)
})

test_that("macOS and a pinned Ubuntu are both named, and no devel entry reads 0", {
  r <- wf_row(wf_repo("check-matrix.yml"))
  expect_equal(r$ci_platforms, '["linux","macos"]')
  expect_equal(r$ci_r_devel, 0L)
})

test_that("a reusable workflow names no platform, so the platforms are unknown", {
  r <- wf_row(wf_repo("reusable.yml"))
  expect_equal(r$ci_rcmdcheck, 1L)
  expect_true(is.na(r$ci_platforms))
})

test_that("a workflow GitHub returns no text for still counts by its name", {
  r <- wf_row(list(github = c("workflows", "workflows/R-CMD-check.yaml"),
                   repo = list(workflows = data.frame(name = character(0), text = character(0)))))
  expect_equal(r$ci_rcmdcheck, 1L)
  expect_true(is.na(r$ci_platforms))
  expect_equal(r$ci_r_devel, 0L)
  expect_equal(r$ci_coverage, 0L)
})

test_that("no workflows directory reads 0, and no contents read reads NA", {
  none <- list(); none["workflows"] <- list(NULL)
  r <- classify_dev_tooling(c("DESCRIPTION"), character(0), repo = none)
  expect_equal(r$ci_workflow_files, "[]")
  for (cn in c("ci_rcmdcheck", "ci_coverage", "ci_site_deploy", "ci_lint")) expect_equal(r[[cn]], 0L, info = cn)
  expect_true(is.na(r$ci_platforms)); expect_true(is.na(r$ci_r_devel))
  unread <- classify_dev_tooling(c("DESCRIPTION"), c("workflows"))
  for (cn in c("ci_workflow_files", "ci_rcmdcheck", "ci_coverage", "ci_site_deploy", "ci_lint"))
    expect_true(is.na(unread[[cn]]), info = cn)
})

test_that("a non-workflow file in the directory is not read for rules", {
  w <- list(github = c("workflows", "workflows/README.md"),
            repo = list(workflows = data.frame(name = "README.md", text = "We run codecov and lintr:: here.")))
  r <- wf_row(w)
  expect_equal(r$ci_workflow_files, "[]")
  expect_equal(r$ci_coverage, 0L)
  expect_equal(r$ci_lint, 0L)
})

test_that("each alternative of each workflow rule matches, and case matters", {
  one <- function(text) wf_row(list(github = "workflows/x.yml",
                                    repo = list(workflows = data.frame(name = "x.yml", text = text))))
  for (t in c("check-r-package", "rcmdcheck", "R CMD check", "devtools::check", "BiocCheck", "rworkflows"))
    expect_equal(one(t)$ci_rcmdcheck, 1L, info = t)
  expect_equal(one("r cmd check")$ci_rcmdcheck, 0L)
  for (t in c("covr::", "codecov", "test-coverage", "coveralls")) expect_equal(one(t)$ci_coverage, 1L, info = t)
  for (t in c("build_site_github_pages", "pkgdown::deploy", "github-pages-deploy-action", "actions/deploy-pages",
              "peaceiris/actions-gh-pages", "altdoc::render", "quarto publish", "quarto-actions/publish"))
    expect_equal(one(t)$ci_site_deploy, 1L, info = t)
  for (t in c("lintr::", "lint_package", "jarl", "air format", "styler::")) expect_equal(one(t)$ci_lint, 1L, info = t)
  expect_equal(one("rcmdcheck\nr-version: 'devel'")$ci_r_devel, 1L)
  expect_equal(one("rcmdcheck\nr: devel")$ci_r_devel, 1L)
  expect_equal(one("rcmdcheck on windows-2022")$ci_platforms, '["windows"]')
})
