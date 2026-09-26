# Every path the oracle compares: the release items, their spellings and a vignette pair.
oracle_paths <- c("README.md", "README.Rmd", "README.qmd", "NEWS.md", "NEWS", "tests", "vignettes",
                  "vignettes/articles", "vignettes/intro.Rmd", "vignettes/refs.bib", "_pkgdown.yml",
                  "_pkgdown.yaml", "inst", "inst/_pkgdown.yml", "inst/_pkgdown.yaml", "pkgdown", "docs",
                  "CODE_OF_CONDUCT.md", "CONDUCT.md", "CONTRIBUTING.md", "data-raw", ".github",
                  ".github/CODE_OF_CONDUCT.md")
oracle_dirs <- c("tests", "vignettes", "vignettes/articles", "inst", "pkgdown", "docs", "data-raw", ".github")

package_dir_with <- function(text) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  for (p in oracle_dirs) dir.create(file.path(d, p), recursive = TRUE, showWarnings = FALSE)
  for (p in setdiff(oracle_paths, oracle_dirs)) file.create(file.path(d, p))
  writeBin(charToRaw(text), file.path(d, ".Rbuildignore"))
  d
}

test_that("the evaluator agrees with R CMD build on 300 real .Rbuildignore files", {
  corpus <- jsonlite::fromJSON(test_path("fixtures", "rbuildignore-corpus.json"), simplifyVector = FALSE)
  expect_length(corpus, 300L)
  compared <- 0L
  for (e in corpus) {
    d <- package_dir_with(e$text)
    want <- tryCatch(suppressWarnings(tools:::inRbuildignore(oracle_paths, d)), error = function(err) NULL)
    if (is.null(want)) next   # a line R cannot compile has its own test below
    expect_identical(rbuildignore_path_matches(e$text, oracle_paths)$hit, want, info = e$repo_id)
    compared <- compared + 1L
  }
  expect_gt(compared, 290L)
})

test_that("a line that does not compile is skipped and the rest still apply", {
  got <- rbuildignore_path_matches("^README\\.md$\n[unclosed\n^tests$\n", c("README.md", "tests", "NEWS.md"))
  expect_identical(got$hit, c(TRUE, TRUE, FALSE))
  expect_equal(got$bad_lines, 1L)
  d <- package_dir_with("^README\\.md$\n^tests$\n")
  expect_identical(got$hit, tools:::inRbuildignore(c("README.md", "tests", "NEWS.md"), d))
})

at_root <- c("DESCRIPTION", ".Rbuildignore")
ex <- function(text, entries) as.vector(rbuildignore_excluded(text, c(at_root, entries)))

test_that("CRLF and CR-only files are read like readLines reads them", {
  expect_equal(ex("^README\\.md$\r\n^tests$\r\n", c("README.md", "tests")), '["README.md","tests"]')
  expect_equal(ex("^README\\.md$\r^NEWS\\.md$\r", c("README.md", "NEWS.md")), '["README.md","NEWS.md"]')
})

test_that("a line starting with # is a pattern like any other, and lines are not trimmed", {
  expect_equal(ex("#*README\\.md\n", "README.md"), '["README.md"]')
  expect_equal(ex("^README\\.md$ \n", "README.md"), "[]")
})

test_that("an excluded directory takes the items under it", {
  expect_equal(ex("^vignettes$\n", c("vignettes", "vignettes/articles", "vignettes/intro.Rmd")),
               '["vignettes","vignettes/articles"]')
  expect_equal(ex("^inst$\n", c("inst", "inst/_pkgdown.yml")), '["_pkgdown.yml"]')
})

test_that("vignettes is left out when every source is, even if a .bib survives", {
  expect_equal(ex("\\.Rmd$\n", c("vignettes", "vignettes/intro.Rmd", "vignettes/refs.bib")), '["vignettes"]')
  expect_equal(ex("^vignettes/intro\\.Rmd$\n", c("vignettes", "vignettes/intro.Rmd", "vignettes/more.Rmd")), "[]")
})

test_that("each spelling is reported under its shared item name", {
  expect_equal(ex("^_pkgdown\\.yaml$\n", "_pkgdown.yaml"), '["_pkgdown.yml"]')
  expect_equal(ex("^inst/_pkgdown\\.yml$\n", c("inst", "inst/_pkgdown.yml")), '["_pkgdown.yml"]')
  expect_equal(ex("^CONDUCT\\.md$\n", "CONDUCT.md"), '["CODE_OF_CONDUCT.md"]')
  expect_equal(ex("^CONTRIBUTING\\.MD$\n", "CONTRIBUTING.MD"), '["CONTRIBUTING.md"]')
})

test_that("the value is [] without a .Rbuildignore and NA when it cannot be read", {
  expect_equal(as.vector(rbuildignore_excluded(NA_character_, c("DESCRIPTION", "README.md"))), "[]")
  expect_true(is.na(rbuildignore_excluded(NA_character_, c("DESCRIPTION", ".Rbuildignore", "README.md"))))
  expect_true(is.na(rbuildignore_excluded("^README\\.md$\n", c(".Rbuildignore", "pkg", "README.md"))))
})

test_that("the classifier keeps the text and states what the release leaves out", {
  repo <- list(rbuildignore_text = "^\\.github$\n^README\\.Rmd$\n")
  r <- classify_dev_tooling(c("DESCRIPTION", ".Rbuildignore", "README.Rmd", "README.md", ".github"),
                            character(0), repo = repo)
  expect_equal(r$rbuildignore_excluded, '["README.Rmd",".github"]')
  expect_equal(r$rbuildignore_text, "^\\.github$\n^README\\.Rmd$\n")
  big <- list(rbuildignore_text = strrep("x", RBUILDIGNORE_TEXT_MAX_BYTES + 1L))
  expect_true(is.na(classify_dev_tooling(c("DESCRIPTION", ".Rbuildignore"), character(0), repo = big)$rbuildignore_text))
  expect_true(is.na(classify_dev_tooling(c("DESCRIPTION", ".Rbuildignore"), character(0))$rbuildignore_excluded))
})

test_that("the fixture repositories read as their .Rbuildignore files say", {
  fx <- classified_fixture()
  expect_equal(fx[["github.com/acoppock/ri2"]]$rbuildignore_excluded,
               '["README.md","README.Rmd","_pkgdown.yml","pkgdown","CODE_OF_CONDUCT.md",".github"]')
  expect_equal(fx[["github.com/tidyverse/forcats"]]$rbuildignore_excluded,
               '["README.Rmd","_pkgdown.yml","pkgdown","data-raw",".github"]')
})
