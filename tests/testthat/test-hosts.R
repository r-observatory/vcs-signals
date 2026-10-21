test_that("the predicate names exactly the hosts it is given", {
  expect_identical(host_predicate(GITHUB_HOSTS), "host IN ('github')")
  expect_identical(host_predicate(c("github", "gitlab"), "r.host"), "r.host IN ('github', 'gitlab')")
  expect_identical(host_predicate("o'x"), "host IN ('o''x')")
})

test_that("no script filters on a literal host any more", {
  src <- unlist(lapply(list.files("../../scripts", "\\.R$", full.names = TRUE), readLines))
  expect_false(any(grepl("host = 'github'|host='github'|LIKE 'github\\.com/%'", src)))
})

test_that("every host filter selects the GitHub hosts, except the daily run's two GitLab stages, the GitLab roster, the GitLab licence and the Codeberg roster", {
  calls_in <- function(f) {
    src <- readLines(f)
    unlist(regmatches(src, gregexpr("host_predicate\\([^)]*\\)", src)))
  }
  files <- list.files("../../scripts", "\\.R$", full.names = TRUE)
  calls <- unlist(lapply(files, calls_in))
  expect_length(calls, 11L)
  expect_equal(sum(grepl("^host_predicate\\(GITHUB_HOSTS[,)]", calls)), 6L)
  gl <- calls[grepl("^host_predicate\\(GITLAB_HOSTS[,)]", calls)]
  expect_length(gl, 4L)
  expect_length(grep("GITLAB_HOSTS", calls_in("../../scripts/update.R")), 2L)
  # The weekly and the AI GitLab rosters share gitlab_roster_rows().
  expect_length(grep("GITLAB_HOSTS", calls_in("../../scripts/gitlab.R")), 1L)
  expect_length(grep("GITLAB_HOSTS", calls_in("../../scripts/weekly.R")), 0L)
  expect_length(grep("GITLAB_HOSTS", calls_in("../../scripts/helpers.R")), 1L)
  expect_equal(sum(grepl("^host_predicate\\(CODEBERG_HOSTS[,)]", calls)), 1L)
  expect_length(grep("CODEBERG_HOSTS", calls_in("../../scripts/codeberg.R")), 1L)
})

test_that("a gitlab repository never reaches the GitHub collectors, even when gitlab is a supported host", {
  old <- SUPPORTED_HOSTS
  assign("SUPPORTED_HOSTS", c("github", "gitlab"), envir = globalenv())
  withr::defer(assign("SUPPORTED_HOSTS", old, envir = globalenv()))

  con <- new_test_db()
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSERT INTO repos (repo_id, host, host_domain, owner, name, name_with_owner, supported,
    n_packages, first_seen, last_seen, status) VALUES
    ('github.com/tidyverse/ggplot2', 'github', 'github.com', 'tidyverse', 'ggplot2', 'tidyverse/ggplot2', 1, 1, '2026-10-01', '2026-10-01', 'active'),
    ('gitlab.com/epiverse/outbreaks/linelist', 'gitlab', 'gitlab.com', 'epiverse/outbreaks', 'linelist',
     'epiverse/outbreaks/linelist', 1, 1, '2026-10-01', '2026-10-01', 'active')")
  picked <- DBI::dbGetQuery(con, sprintf("SELECT repo_id FROM repos WHERE %s", host_predicate(GITHUB_HOSTS)))
  expect_identical(picked$repo_id, "github.com/tidyverse/ggplot2")

  # A daily run: the gitlab repository is marked supported but is never sent to GitHub or marked gone.
  out <- tempfile("out"); dir.create(out)
  rel <- tempfile("rel"); dir.create(rel)
  seen <- new.env()
  seen$queries <- character(0)
  io <- local_release_io(rel,
    acquire = function() data.frame(package = c("ggplot2", "linelist"), origin = c("cran", "bioc"),
      url_raw = c("https://github.com/tidyverse/ggplot2", "https://gitlab.com/epiverse/outbreaks/linelist"),
      bugreports_raw = NA, stringsAsFactors = FALSE),
    graphql = function(query) {
      seen$queries <- c(seen$queries, query)
      f <- if (grepl("followRenames", query)) "resolve_one.json"
           else if (grepl("history \\{ totalCount", query)) "commits.json" else "gauges_one.json"
      jsonlite::fromJSON(readLines(file.path("fixtures", f), warn = FALSE), simplifyVector = FALSE)
    })
  suppressMessages(run_update(io, out, list(force_full = TRUE)))
  pub <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-signals-recent.db"))
  withr::defer(DBI::dbDisconnect(pub))
  gl <- DBI::dbGetQuery(pub, "SELECT supported, node_id, status FROM repos WHERE host = 'gitlab'")
  expect_identical(nrow(gl), 1L)
  expect_identical(gl$supported, 1L)
  expect_true(is.na(gl$node_id))
  expect_identical(gl$status, "active")
  expect_true(length(seen$queries) > 0)
  expect_false(any(grepl("outbreaks|linelist", seen$queries)))
})
