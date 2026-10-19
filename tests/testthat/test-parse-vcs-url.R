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

test_that("parse_vcs_url promotes unlisted self-hosted forges to other, preserving domain", {
  r <- parse_vcs_url("https://gitlab.example.edu/o/n")
  expect_equal(r$host, "other")
  expect_equal(r$host_domain, "gitlab.example.edu")
  expect_equal(parse_vcs_url("https://git.example.org/o/n")$host, "other")
  expect_equal(parse_vcs_url("https://gitlab.mbb.univ-montp2.fr/o/n")$host, "other")
})

test_that("parse_vcs_url treats listed GitLab domains as gitlab and keeps nested paths", {
  r <- parse_vcs_url("https://code.usgs.gov/a/b/c")
  expect_equal(r$host, "gitlab")
  expect_equal(r$host_domain, "code.usgs.gov")
  expect_equal(r$owner, "a/b")
  expect_equal(r$name, "c")
  expect_equal(parse_vcs_url("https://forge.inrae.fr/x/y")$host, "gitlab")
  r <- parse_vcs_url("https://forgemia.inra.fr/x/y")
  expect_equal(r$host, "gitlab")
  expect_equal(repo_slug(r$host_domain, r$owner, r$name), "forgemia.inra.fr/x/y")
})

test_that("repos.supported is 1 for gitlab and 0 for other", {
  df <- data.frame(package = c("a", "b"), origin = "cran", resolved_from = "url",
                   host = c("gitlab", "other"), host_domain = c("code.usgs.gov", "git.example.org"),
                   owner = c("a/b", "o"), name = c("c", "n"), stringsAsFactors = FALSE)
  expect_equal(build_repo_index(df)$repos$supported, c(1L, 0L))
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

test_that("parse_vcs_url reads forgemia.inra.fr as a GitLab host", {
  expect_equal(parse_vcs_url("https://forgemia.inra.fr/pappso/metaprotr"),
               list(host = "gitlab", host_domain = "forgemia.inra.fr", owner = "pappso", name = "metaprotr"))
  expect_equal(parse_vcs_url("https://forgemia.inra.fr/genotoul-bioinfo/mixKernel/-/issues")$name, "mixKernel")
})
