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
