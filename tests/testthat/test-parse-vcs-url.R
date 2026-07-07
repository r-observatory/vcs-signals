test_that("parse_vcs_url handles github variants", {
  expect_equal(parse_vcs_url("https://github.com/tidyverse/ggplot2"),
               list(host = "github", host_domain = "github.com", owner = "tidyverse", name = "ggplot2"))
  expect_equal(parse_vcs_url("https://github.com/tidyverse/ggplot2/issues")$name, "ggplot2")
  expect_equal(parse_vcs_url("https://github.com/foo/bar.git")$name, "bar")
  expect_equal(parse_vcs_url("git@github.com:foo/bar.git"),
               list(host = "github", host_domain = "github.com", owner = "foo", name = "bar"))
  expect_equal(parse_vcs_url("http://www.github.com/Foo/Bar/")$owner, "Foo")
  expect_equal(parse_vcs_url("https://github.com/foo/bar/tree/main/sub")$name, "bar")
  expect_equal(parse_vcs_url("https://github.com/foo/bar?tab=readme")$name, "bar")
})

test_that("parse_vcs_url handles gitlab scope and nesting", {
  expect_equal(parse_vcs_url("https://gitlab.com/o/n/-/issues"),
               list(host = "gitlab", host_domain = "gitlab.com", owner = "o", name = "n"))
  expect_equal(parse_vcs_url("https://gitlab.com/group/subgroup/project"),
               list(host = "gitlab", host_domain = "gitlab.com", owner = "group/subgroup", name = "project"))
})

test_that("parse_vcs_url handles other known forges", {
  expect_equal(parse_vcs_url("https://codeberg.org/o/r")$host, "codeberg")
  expect_equal(parse_vcs_url("https://bitbucket.org/o/r")$host, "bitbucket")
  expect_equal(parse_vcs_url("https://git.sr.ht/~user/repo")$host, "sourcehut")
  expect_equal(parse_vcs_url("https://r-forge.r-project.org/projects/x/y")$host, "rforge")
})

test_that("parse_vcs_url promotes self-hosted forges to other, preserving domain", {
  r <- parse_vcs_url("https://gitlab.example.edu/o/n")
  expect_equal(r$host, "other")
  expect_equal(r$host_domain, "gitlab.example.edu")
  expect_equal(parse_vcs_url("https://git.example.org/o/n")$host, "other")
})

test_that("parse_vcs_url rejects non-repo and malformed inputs", {
  expect_null(parse_vcs_url("https://doi.org/10.1/abc"))
  expect_null(parse_vcs_url("https://arxiv.org/abs/1234.5678"))
  expect_null(parse_vcs_url("https://cran.r-project.org/package=foo"))
  expect_null(parse_vcs_url("https://sites.google.com/view/x"))
  expect_null(parse_vcs_url("https://example.org"))
  expect_null(parse_vcs_url("https://github.com/onlyowner"))
  expect_null(parse_vcs_url("https://thelovelab.github.io/tximeta"))  # pages handled elsewhere
  expect_null(parse_vcs_url("README"))
  expect_null(parse_vcs_url(NA_character_))
})
