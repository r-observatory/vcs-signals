# The golden record: each publishing path run over one fixture roster, with every table its
# jobs leave in their out dirs, every request and release call it makes, and what it publishes
# as the manifest and the release notes. tests/golden/record.R writes it.

# Every recorded run is dated this day, so values derived from the date hold on any day the
# suite runs. The release first carries two earlier daily runs, so the series has a star count
# older than 30 days and two release rises.
GOLDEN_DAY <- "2026-10-04"
GOLDEN_EARLIER <- c("2026-08-20", "2026-09-10")
GOLDEN_SHARDS <- 2L

golden_paths <- function() c("daily", "weekly", "ai", "backfill")

# The publishing workflows set VCS_SKIP_GOLDEN_PARITY=1 on runs from main, whose code was checked
# on push, so a package release cannot stop the day's data there.
skip_golden_parity <- function()
  testthat::skip_if(identical(Sys.getenv("VCS_SKIP_GOLDEN_PARITY"), "1"),
                    "VCS_SKIP_GOLDEN_PARITY is 1 (a publishing run from main, checked on push)")

# ---- the roster -------------------------------------------------------------
# One acquisition. linelist is listed twice, on GitHub for CRAN and on a nested GitLab group for
# Bioc. cfr arrives on the recorded day, so that day's run resolves one repository, which GitHub
# answers under a new owner and name.
.golden_acquire <- function(back = 0L) {
  df <- data.frame(
    package = c("forcats", "goodpractice", "bqmm", "ri2", "linelist", "linelist", "cfr", "usgspkg",
                "cbpkg", "rfpkg", "data.table"),
    origin = c("cran", "cran", "cran", "cran", "cran", "bioc", "bioc", "cran", "cran", "cran", "cran"),
    url_raw = c("https://forcats.tidyverse.org, https://github.com/tidyverse/forcats",
                "https://docs.ropensci.org/goodpractice/ https://github.com/ropensci-review-tools/goodpractice",
                "https://github.com/kvenkita/bqmm-r", "https://github.com/acoppock/ri2",
                "https://github.com/epiverse-trace/linelist", "https://gitlab.com/epiverse/outbreaks/linelist",
                "https://github.com/epiverse-trace/cfr", "https://code.usgs.gov/water/toxics/usgspkg",
                "https://codeberg.org/someone/cbpkg", "https://r-forge.r-project.org/projects/rfpkg/",
                "https://github.com/Rdatatable/data.table"),
    bugreports_raw = c("https://github.com/tidyverse/forcats/issues", NA, NA, NA,
                       "https://github.com/epiverse-trace/linelist/issues", NA, NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE)
  if (back > 0L) df[df$package != "cfr", , drop = FALSE] else df
}

# Links from before the run: forcats once lived elsewhere, and retired has left.
.golden_links_backfill <- function(dir) {
  path <- file.path(dir, "links-backfill.csv.gz")
  con <- gzfile(path, "w")
  writeLines(c("repo_id,package,origin,first_seen,last_seen",
               "github.com/old-owner/forcats,forcats,cran,2026-08-01,2026-09-15",
               "gitlab.com/someone/retired,retired,cran,2026-08-01,2026-09-10"), con)
  close(con)
  path
}

# The GitHub repositories the roster names, as GitHub answers on the recorded day. asked is the
# slug a package gives and nwo what GitHub answers, so bqmm-r and cfr were renamed and ri2 is
# gone; bqmm is empty and cfr archived.
.golden_gh <- data.frame(
  asked = c("tidyverse/forcats", "ropensci-review-tools/goodpractice", "kvenkita/bqmm-r", "acoppock/ri2",
            "epiverse-trace/linelist", "epiverse-trace/cfr", "Rdatatable/data.table"),
  nwo = c("tidyverse/forcats", "ropensci-review-tools/goodpractice", "kvenkita/bqmm", NA,
          "epiverse-trace/linelist", "epiverse/cfr-estimates", "Rdatatable/data.table"),
  id = c("R_forcats", "R_goodpractice", "R_bqmm", NA, "R_linelist", "R_cfr", "R_datatable"),
  user = c(FALSE, FALSE, TRUE, NA, FALSE, FALSE, FALSE),
  created = c("2015-08-23T21:23:24Z", "2016-03-08T10:12:00Z", "2024-02-11T09:30:00Z", NA,
              "2023-03-14T11:00:00Z", "2023-01-20T15:45:00Z", "2012-04-02T10:00:00Z"),
  stars = c(570L, 470L, 3L, NA, 45L, 20L, 3400L),
  forks = c(130L, 37L, 0L, NA, 8L, 6L, 960L),
  issues = c(21L, 14L, 0L, NA, 5L, 2L, 800L),
  prs = c(3L, 2L, 0L, NA, 1L, 0L, 40L),
  releases = c(12L, 5L, 0L, NA, 4L, 3L, 9L),
  tag = c("v1.0.1", "v1.0.5", NA, NA, "v1.2.0", "v0.2.0", "v1.16.0"),
  published = c("2025-09-03T10:00:00Z", "2025-02-17T12:00:00Z", NA, NA, "2025-05-30T09:00:00Z",
                "2024-11-02T08:00:00Z", "2024-01-30T09:00:00Z"),
  head = c("2026-09-20T08:00:00Z", "2026-08-30T17:10:00Z", NA, NA, "2026-09-25T13:40:00Z",
           "2026-07-02T16:05:00Z", "2026-09-28T11:20:00Z"),
  license = c("MIT", "GPL-2.0", NA, NA, "MIT", "MIT", "MPL-2.0"),
  topics = c("r,factors,tidyverse", "r,linter", "", NA, "r,epidemiology", "r", "r,data-table"),
  archived = c(FALSE, FALSE, FALSE, NA, FALSE, TRUE, FALSE),
  commits = c(1210L, 800L, NA, NA, 910L, 640L, 5600L),
  stringsAsFactors = FALSE)

# GitHub as it answered `back` daily runs before the recorded day: fewer stars, forks and
# releases, more open issues, and bqmm not yet renamed.
.golden_world <- function(back = 0L) {
  gh <- .golden_gh
  gh$stars <- gh$stars - back * (gh$stars %/% 20L)
  gh$forks <- pmax(gh$forks - back, 0L)
  gh$releases <- pmax(gh$releases - back, 0L)
  gh$issues <- gh$issues + back
  if (back > 0L) gh$nwo[gh$id %in% "R_bqmm"] <- "kvenkita/bqmm-r"
  gh
}

# How many daily runs before the recorded day `day` is.
.golden_back <- function(day) {
  days <- c(GOLDEN_EARLIER, GOLDEN_DAY)
  length(days) - match(day, days)
}

# Star, fork and release dates the backfill pages through, three to a page.
.golden_history <- list(
  stars = list(
    "tidyverse/forcats" = c("2016-01-10T08:00:00Z", "2018-05-02T09:30:00Z", "2020-03-03T10:00:00Z",
                            "2024-11-20T14:00:00Z", "2026-09-30T18:00:00Z"),
    "ropensci-review-tools/goodpractice" = c("2017-04-01T00:00:00Z", "2025-12-24T12:00:00Z"),
    "kvenkita/bqmm" = c("2025-01-05T00:00:00Z"),
    "epiverse-trace/linelist" = c("2023-05-01T00:00:00Z", "2026-02-14T07:00:00Z"),
    "epiverse/cfr-estimates" = c("2023-02-01T00:00:00Z"),
    "Rdatatable/data.table" = c("2014-03-01T00:00:00Z", "2021-06-15T12:00:00Z")),
  forks = list(
    "tidyverse/forcats" = c("2016-06-01T00:00:00Z", "2022-08-08T08:08:08Z"),
    "epiverse-trace/linelist" = c("2024-04-04T04:04:04Z")),
  releases_total = list(
    "tidyverse/forcats" = c("2016-08-29T00:00:00Z", "2023-01-29T00:00:00Z", "2025-09-03T10:00:00Z"),
    "ropensci-review-tools/goodpractice" = c("2025-02-17T12:00:00Z"),
    "epiverse/cfr-estimates" = c("2024-11-02T08:00:00Z")))

# The repository a slug names, by the slug asked or the one GitHub answers, or NULL.
.golden_row <- function(gh, owner, name) {
  s <- tolower(paste(owner, name, sep = "/"))
  i <- which(tolower(gh$asked) == s | tolower(gh$nwo) %in% s)
  if (length(i)) gh[i[1], , drop = FALSE] else NULL
}

# ---- the fake GitHub ----------------------------------------------------------
.golden_aliases <- function(q) {
  m <- regmatches(q, gregexpr('[a-z][0-9]+: repository\\(owner: "[^"]+", name: "[^"]+"', q))[[1]]
  p <- regmatches(m, regexec('^([a-z][0-9]+): repository\\(owner: "([^"]+)", name: "([^"]+)"', m))
  data.frame(alias = vapply(p, `[`, "", 2L), owner = vapply(p, `[`, "", 3L), name = vapply(p, `[`, "", 4L),
             stringsAsFactors = FALSE)
}

# One answer per alias, a NOT_FOUND for a repository GitHub does not have, and an error for an
# answer that is a .golden_failed().
.golden_failed <- function(message) structure(list(message = message), class = "golden_failed")

.golden_aliased <- function(q, gh, answer) {
  a <- .golden_aliases(q)
  data <- list(); errors <- list()
  for (k in seq_len(nrow(a))) {
    r <- .golden_row(gh, a$owner[k], a$name[k])
    if (is.null(r) || is.na(r$id)) {
      data[a$alias[k]] <- list(NULL)
      errors[[length(errors) + 1L]] <- list(type = "NOT_FOUND", path = list(a$alias[k]),
        message = sprintf("Could not resolve to a Repository with the name '%s/%s'.", a$owner[k], a$name[k]))
    } else {
      x <- answer(r)
      if (inherits(x, "golden_failed")) {
        data[a$alias[k]] <- list(NULL)
        errors[[length(errors) + 1L]] <- list(path = list(a$alias[k]), message = x$message)
      } else data[[a$alias[k]]] <- x
    }
  }
  if (length(errors)) list(data = data, errors = errors) else list(data = data)
}

.golden_owner <- function(r) {
  login <- sub("/.*$", "", r$nwo)
  list(`__typename` = if (r$user) "User" else "Organization", login = login,
       id = paste0(if (r$user) "U_" else "O_", login))
}

.golden_gauge_node <- function(r) list(
  id = r$id, nameWithOwner = r$nwo, stargazerCount = r$stars, forkCount = r$forks,
  owner = .golden_owner(r), watchers = list(totalCount = r$stars %/% 10L),
  issues_open = list(totalCount = r$issues), issues_closed = list(totalCount = 10L * r$issues),
  prs_open = list(totalCount = r$prs), prs_closed = list(totalCount = r$prs + 1L),
  mergedPRs = list(totalCount = 7L * r$prs), releases = list(totalCount = r$releases),
  latestRelease = if (!is.na(r$tag)) list(tagName = r$tag, publishedAt = r$published, isPrerelease = FALSE),
  defaultBranchRef = if (!is.na(r$head)) list(target = list(committedDate = r$head)),
  licenseInfo = if (!is.na(r$license)) list(spdxId = r$license),
  repositoryTopics = list(nodes = lapply(strsplit(r$topics, ",", fixed = TRUE)[[1]],
                                         function(t) list(topic = list(name = t)))),
  isArchived = r$archived, isFork = FALSE, isMirror = FALSE, isDisabled = FALSE,
  createdAt = r$created, pushedAt = r$head, updatedAt = r$head, diskUsage = 40L * r$stars + 100L,
  primaryLanguage = list(name = "R"))

.golden_gauges <- function(q, gh) {
  ids <- gsub('"', "", regmatches(q, gregexpr('"R_[^"]+"', q))[[1]], fixed = TRUE)
  nodes <- list(); errors <- list()
  for (k in seq_along(ids)) {
    r <- gh[which(gh$id == ids[k]), , drop = FALSE]
    if (nrow(r)) nodes[[k]] <- .golden_gauge_node(r) else {
      nodes[k] <- list(NULL)
      errors[[length(errors) + 1L]] <- list(type = "NOT_FOUND", path = list("nodes", k - 1L),
        message = sprintf("Could not resolve to a node with the global id of '%s'", ids[k]))
    }
  }
  if (length(errors)) list(data = list(nodes = nodes), errors = errors) else list(data = list(nodes = nodes))
}

.golden_resolved <- function(r) list(id = r$id, nameWithOwner = r$nwo, isArchived = r$archived,
                                     isFork = FALSE, isMirror = FALSE, isDisabled = FALSE, createdAt = r$created)

# The recorded contents answer for the repository, with a Claude file in forcats, a .claude
# ignore line in goodpractice and a CodeRabbit config in linelist.
.golden_contents <- function(r) {
  raw <- jsonlite::fromJSON(file.path("fixtures", "contents-2026-09.json"), simplifyVector = FALSE)$data
  hit <- Filter(function(a) tolower(a$nameWithOwner) %in% tolower(c(r$nwo, r$asked)), raw)
  a <- if (length(hit)) hit[[1]] else list(nameWithOwner = r$nwo, isFork = FALSE, parent = NULL,
    rootTree = list(entries = list(list(name = "DESCRIPTION", type = "blob"))))
  # A renamed repository answers under its new name.
  if (!identical(tolower(a$nameWithOwner), tolower(r$nwo))) a$nameWithOwner <- r$nwo
  add <- function(a, entry) { a$rootTree$entries <- c(a$rootTree$entries, list(list(name = entry, type = "blob"))); a }
  if (identical(r$nwo, "tidyverse/forcats")) {
    a <- add(a, "CLAUDE.md")
    a$descBlob <- list(byteSize = 38L, text = "Package: forcats\nVersion: 1.0.1.9000\n")
  }
  if (identical(r$nwo, "ropensci-review-tools/goodpractice")) a$gitignore <- list(text = ".Rproj.user\n.claude\n")
  if (identical(r$nwo, "epiverse-trace/linelist")) a <- add(a, ".coderabbit.yaml")
  a
}

.golden_commit_node <- function(oid) Find(function(n) startsWith(n$oid, oid), ai_fixture("commits.json"))

# linelist carries a Cursor pull request from outside the project.
.golden_activity <- function(r) {
  prs <- ai_fixture("activity-prs.json")$data
  plain <- list(oid = "0a1b2c3d4e", committedDate = "2026-09-25T13:40:00Z", message = "Tidy the vignette",
                author = list(name = "Maintainer", email = "maintainer@example.org", user = list(login = "maint")))
  s <- switch(r$nwo,
    "tidyverse/forcats" = list(prs = list(prs$r2$pullRequests$nodes[[1]]), commits = list(.golden_commit_node("d3106187a2"))),
    "ropensci-review-tools/goodpractice" = list(prs = list(prs$r6$pullRequests$nodes[[1]]),
                                                commits = list(.golden_commit_node("1a85ae819e"))),
    "epiverse-trace/linelist" = list(prs = list(prs$r4$pullRequests$nodes[[1]]), commits = list(plain)),
    "epiverse/cfr-estimates" = list(prs = list(), commits = list(.golden_commit_node("fe5c63c197"))),
    list(prs = list(), commits = list()))
  page <- list(endCursor = NULL, hasNextPage = FALSE)
  # goodpractice has more commits than one page, so its first read is not whole and its rules are searched.
  more <- if (identical(r$nwo, "ropensci-review-tools/goodpractice")) list(endCursor = "C1", hasNextPage = TRUE) else page
  list(pullRequests = list(totalCount = length(s$prs), pageInfo = page, nodes = s$prs),
       defaultBranchRef = if (!is.na(r$head)) list(target = list(recent = list(pageInfo = more, nodes = s$commits))))
}

# data.table's read fails, so the AI merge stops after it publishes.
.golden_accounts <- function(r) {
  if (identical(r$id, "R_datatable"))
    return(.golden_failed("Something went wrong while executing your query. This may be the result of a timeout."))
  counts <- if (identical(r$nwo, "tidyverse/forcats"))
    list(a_claude = list(totalCount = 2L, nodes = list(list(committedDate = "2025-10-05T05:49:23Z")))) else list()
  list(defaultBranchRef = if (!is.na(r$head)) list(target = counts))
}

.golden_responsiveness <- function(r) {
  k <- match(r$id, .golden_gh$id)
  at <- function(day, by = 0L) paste0(format(as.Date(day) + by), "T00:00:00Z")
  if (is.na(r$head)) return(list(closedIssues = list(nodes = list()), resolvedPRs = list(nodes = list()),
                                 openIssues = list(nodes = list())))
  list(closedIssues = list(nodes = list(list(createdAt = at("2026-08-01"), closedAt = at("2026-08-04", k)),
                                        list(createdAt = at("2026-07-01"), closedAt = at("2026-07-30", 2L * k)))),
       resolvedPRs = list(nodes = list(list(createdAt = at("2026-09-01"), closedAt = at("2026-09-02", k)))),
       openIssues = list(nodes = list(list(createdAt = at("2026-06-01", -k)), list(createdAt = at("2026-09-20")))))
}

.golden_commit_count <- function(r)
  list(defaultBranchRef = if (!is.na(r$commits)) list(target = list(history = list(totalCount = r$commits))))

# One page of a backfill connection, three items to a page from the cursor "c<offset>".
.golden_connection <- function(r, metric, q) {
  mc <- METRIC_CONNECTIONS[[metric]]
  ts <- .golden_history[[metric]][[r$nwo]] %||% character(0)
  after <- regmatches(q, regexec('after: "c([0-9]+)"', q))[[1]]
  from <- if (length(after) == 2L) as.integer(after[2]) else 0L
  take <- ts[seq_along(ts) > from & seq_along(ts) <= from + 3L]
  items <- lapply(take, function(t) stats::setNames(list(t), mc$ts))
  more <- from + 3L < length(ts)
  conn <- list(pageInfo = list(endCursor = if (more) sprintf("c%d", from + 3L), hasNextPage = more))
  conn[[mc$sel]] <- items
  stats::setNames(list(conn), mc$conn)
}

# One page of a file's history, and the ignore file at one commit, for the onset dating.
.golden_file_history <- function(q) {
  p <- regmatches(q, regexec('repository\\(owner: "([^"]+)", name: "([^"]+)"\\)', q))[[1]]
  path <- regmatches(q, regexec('path: "([^"]+)"', q))[[1]][2]
  key <- paste(tolower(paste(p[2], p[3], sep = "/")), path)
  nodes <- switch(key,
    "tidyverse/forcats CLAUDE.md" = list(list(committedDate = "2025-11-02T10:00:00Z"),
                                         list(committedDate = "2025-06-12T08:00:00Z")),
    "ropensci-review-tools/goodpractice .gitignore" = list(list(oid = "g2", committedDate = "2026-05-01T00:00:00Z"),
                                                           list(oid = "g1", committedDate = "2025-12-01T00:00:00Z")),
    list())
  list(data = list(repository = list(defaultBranchRef = list(target = list(history = list(
    pageInfo = list(endCursor = NULL, hasNextPage = FALSE), nodes = nodes))))))
}

.golden_blob_at <- function(q) {
  oid <- regmatches(q, regexec('object\\(expression: "([^:"]+):', q))[[1]][2]
  text <- if (identical(oid, "g2")) ".Rproj.user\n.claude\n" else ".Rproj.user\n"
  list(data = list(repository = list(object = list(text = text))))
}

.golden_github <- function(q, gh) {
  has <- function(s) grepl(s, q, fixed = TRUE)
  if (has("rateLimit")) return(list(data = list(rateLimit = list(remaining = 4800L, resetAt = "2026-10-04T13:00:00Z"))))
  if (has("nodes(ids:")) return(.golden_gauges(q, gh))
  if (has("followRenames: true")) return(.golden_aliased(q, gh, .golden_resolved))
  if (has("rootTree")) return(.golden_aliased(q, gh, .golden_contents))
  if (has("a_claude:")) return(.golden_aliased(q, gh, .golden_accounts))
  if (has("pullRequests(first: 50")) return(.golden_aliased(q, gh, .golden_activity))
  if (has("closedIssues:")) return(.golden_aliased(q, gh, .golden_responsiveness))
  if (has("history { totalCount }")) return(.golden_aliased(q, gh, .golden_commit_count))
  if (has("path: \"")) return(.golden_file_history(q))
  if (has("object(expression:")) return(.golden_blob_at(q))
  for (metric in names(.golden_history)) {
    if (!has(paste0(METRIC_CONNECTIONS[[metric]]$conn, "(first:"))) next
    if (!has("r0: repository")) {
      p <- regmatches(q, regexec('repository\\(owner: "([^"]+)", name: "([^"]+)"\\)', q))[[1]]
      return(list(data = list(repository = .golden_connection(.golden_row(gh, p[2], p[3]), metric, q))))
    }
    return(.golden_aliased(q, gh, function(r) .golden_connection(r, metric, q)))
  }
  stop("the golden GitHub has no answer for: ", substr(q, 1L, 200L))
}

# ---- the fake GitLab ----------------------------------------------------------
# The roster's GitLab projects as GitLab answers `back` daily runs before the recorded day.
# linelist gains a star each run and lists an upcoming release first; usgspkg has merge
# requests switched off and its path resolves only on the recorded day.
.golden_gl_projects <- function(domain, back = 0L) {
  rel <- function(tag, at, upcoming) list(tagName = tag, releasedAt = at, upcomingRelease = upcoming)
  base <- list(archived = FALSE, isForked = FALSE, issuesEnabled = TRUE, mergeRequestsEnabled = TRUE)
  switch(domain,
    "gitlab.com" = list(c(list(
      id = "gid://gitlab/Project/46520077", fullPath = "epiverse/outbreaks/linelist",
      starCount = 12L - back, forksCount = 3L, topics = list("r", "outbreaks"),
      createdAt = "2023-06-01T09:00:00Z", lastActivityAt = "2026-09-29T08:15:00Z",
      openIssuesCount = 4L + back, openMergeRequestsCount = 1L, issueStatusCounts = list(closed = 9L),
      mrClosed = list(count = 2L), mrMerged = list(count = 15L),
      releases = list(count = 3L, nodes = list(rel("v2.0.0", "2026-12-01T00:00:00Z", TRUE),
                                               rel("v1.3.0", "2026-07-15T10:00:00Z", FALSE))),
      repository = list(tree = list(lastCommit = list(committedDate = "2026-09-29T10:15:00+02:00")))), base)),
    "code.usgs.gov" = if (back == 0L) list(utils::modifyList(base, list(
      id = "gid://gitlab/Project/4711", fullPath = "water/toxics/usgspkg",
      starCount = 4L, forksCount = 1L, topics = list(), mergeRequestsEnabled = FALSE,
      createdAt = "2021-03-02T16:20:00Z", lastActivityAt = "2026-08-27T17:11:41Z",
      openIssuesCount = 2L, openMergeRequestsCount = 0L, issueStatusCounts = list(closed = 5L),
      mrClosed = list(count = 0L), mrMerged = list(count = 0L), releases = list(count = 0L, nodes = list()),
      repository = list(tree = list(lastCommit = list(committedDate = "2026-08-27T17:00:00Z")))))) else list(),
    list())
}

# Commit totals as GitLab answers them by project id; code.usgs.gov runs a GitLab without commitCount.
.golden_gl_commits <- function(domain, ids) {
  if (identical(domain, "code.usgs.gov"))
    return(list(data = NULL, status = 200L, error = NA_character_,
                errors = list(list(message = "Field 'commitCount' doesn't exist on type 'Repository'"))))
  counts <- c("gid://gitlab/Project/46520077" = 310L)
  ids <- unlist(ids)
  nodes <- lapply(ids[ids %in% names(counts)], function(i) list(id = i, repository = list(commitCount = counts[[i]])))
  list(data = list(projects = list(nodes = nodes)), errors = NULL, status = 200L, error = NA_character_)
}

.golden_gl_ok <- function(nodes) list(data = list(projects = list(nodes = nodes)), errors = NULL, status = 200L,
                                      error = NA_character_)
.golden_gl_schema_error <- function(type, fields) list(data = NULL, status = 200L, error = NA_character_,
  errors = lapply(fields, function(f) list(message = sprintf("Field '%s' doesn't exist on type '%s'", f, type),
                                           extensions = list(code = "undefinedField", typeName = type, fieldName = f))))

# The weekly AI read's projects. linelist has CLAUDE.md, issue templates under .gitlab and a CI file that
# checks on R-devel, measures coverage and deploys Pages. usgspkg ignores .claude and takes its CI from
# an include:, on a GitLab without forkedFrom, pagesDeployments or the commit-message search.
.golden_gl_repos <- list(
  "gitlab.com" = list(id = "gid://gitlab/Project/46520077", fullPath = "epiverse/outbreaks/linelist", pages = 1L,
    trees = c("R", "tests", "vignettes"), blobs = c(".gitlab-ci.yml", "CLAUDE.md", "DESCRIPTION", "NAMESPACE", "README.md"),
    gitlab = "issue_templates",
    files = list(DESCRIPTION = "Package: linelist\nVersion: 1.3.0\n",
                 ".gitlab-ci.yml" = paste0("image: rocker/r-ver:devel\n\ncheck:\n  script:\n    - R CMD build .\n",
                                           "    - R CMD check --no-manual linelist_*.tar.gz\n",
                                           "    - Rscript -e 'covr::package_coverage()'\n\n",
                                           "pages:\n  script:\n    - Rscript -e 'pkgdown::build_site()'\n",
                                           "  artifacts:\n    paths:\n      - public\n"))),
  "code.usgs.gov" = list(id = "gid://gitlab/Project/4711", fullPath = "water/toxics/usgspkg", old = TRUE,
    trees = "R", blobs = c(".gitignore", ".gitlab-ci.yml", "DESCRIPTION", "NAMESPACE"), gitlab = character(0),
    files = list(DESCRIPTION = "Package: usgspkg\nVersion: 0.4.2\n", .gitignore = ".Rproj.user\n.claude\n",
                 ".gitlab-ci.yml" = "include:\n  - project: water/ci-templates\n    file: /r-package.yml\n")))

# The contents of the asked projects, with the fields the query asks; an old host refuses the optional ones.
.golden_gl_contents <- function(domain, query, variables) {
  r <- .golden_gl_repos[[domain]]
  asked <- GITLAB_OPTIONAL_FIELDS[vapply(GITLAB_OPTIONAL_FIELDS, grepl, NA, x = query, fixed = TRUE)]
  if (isTRUE(r$old) && length(asked)) return(.golden_gl_schema_error("Project", asked))
  if (is.null(r) || !r$id %in% unlist(variables$ids)) return(.golden_gl_ok(list()))
  listing <- function(trees = character(0), blobs = character(0))
    list(trees = list(nodes = lapply(trees, function(n) list(name = n))),
         blobs = list(nodes = lapply(blobs, function(n) list(name = n))))
  sub_trees <- lapply(GITLAB_TREE_ALIASES, function(a) listing())
  sub_trees$gitlab <- listing(r$gitlab)
  node <- list(id = r$id, fullPath = r$fullPath, isForked = FALSE, forkedFrom = NULL,
               ciConfigPathOrDefault = ".gitlab-ci.yml", pagesDeployments = list(count = r$pages), issuesEnabled = TRUE,
               repository = c(list(rootRef = "main", empty = FALSE, root = listing(r$trees, r$blobs)), sub_trees,
                 list(blobs = list(nodes = lapply(names(r$files), function(f) list(path = f, rawTextBlob = r$files[[f]]))))))
  .golden_gl_ok(list(node[!names(node) %in% setdiff(GITLAB_OPTIONAL_FIELDS, asked)]))
}

# One commit in linelist credits Claude; usgspkg's host has no message search.
.golden_gl_messages <- function(domain, variables) {
  r <- .golden_gl_repos[[domain]]
  if (isTRUE(r$old)) return(.golden_gl_schema_error("Repository", "commits"))
  if (is.null(r) || !r$id %in% unlist(variables$ids)) return(.golden_gl_ok(list()))
  credit <- list(sha = "5e1f0a7c2b", committedDate = "2026-03-05T11:00:00+01:00",
                 message = "Check onset dates\n\nCo-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>")
  .golden_gl_ok(list(list(id = r$id, repository = list(commits = list(
    pageInfo = list(hasNextPage = FALSE, endCursor = NULL), nodes = list(credit))))))
}

# No golden project lists .github/workflows, so a workflow read finds no files.
.golden_gl_workflows <- function(variables)
  .golden_gl_ok(lapply(unlist(variables$ids), function(i) list(id = i, repository = list(blobs = list(nodes = list())))))

# As GitLab answers each query: unknown paths and ids left out, paths matched without case.
.golden_gitlab <- function(domain, query, variables, back = 0L) {
  has <- function(s) grepl(s, query, fixed = TRUE)
  # The workflow read also asks rawTextBlob, so it is told apart first.
  if (has("blobs(paths: $paths)")) return(.golden_gl_workflows(variables))
  if (has("rawTextBlob")) return(.golden_gl_contents(domain, query, variables))
  if (has("$re")) return(.golden_gl_messages(domain, variables))
  if (has("commitCount")) return(.golden_gl_commits(domain, variables$ids))
  keys <- tolower(unlist(variables$keys))
  hit <- Filter(function(n) tolower(n$fullPath) %in% keys || tolower(n$id) %in% keys, .golden_gl_projects(domain, back))
  list(data = list(projects = list(nodes = hit)),
       errors = NULL, status = 200L, error = NA_character_)
}

# The weekly REST answers: the contributor count in x-total, rows that carry a name and an email the
# job never keeps, and licence keys, usgspkg's one GitLab files as other. The AI dating's answers: a
# file's commits, newest first, each with an address the job never keeps (linelist's CLAUDE.md, and
# usgspkg's .gitignore), and .gitignore at a commit, with the .claude line at g2 and without it at g1.
# Any other project is a 404.
.golden_gl_rest <- function(domain, path) {
  answer <- function(body, headers = character(), status = 200L)
    list(status = status, headers = headers, error = if (status >= 400L) sprintf("HTTP %d", status) else NA_character_,
         body = as.character(jsonlite::toJSON(body, auto_unbox = TRUE)))
  projects <- list("gitlab.com" = list(id = "46520077", total = "6", top = 210L, license = "mit"),
                   "code.usgs.gov" = list(id = "4711", total = "2", top = 75L, license = "other"))
  history <- list("46520077 CLAUDE.md" = list(c("c9e1", "2026-06-03T08:00:00Z"), c("c4b2", "2026-02-10T12:00:00Z")),
                  "4711 .gitignore" = list(c("g2", "2026-04-14T09:30:00Z"), c("g1", "2025-11-20T15:00:00Z")))
  p <- if (domain %in% names(projects)) projects[[domain]]
  if (is.null(p) || !grepl(sprintf("^/projects/%s[/?]", p$id), path)) return(answer(list(message = "404 Project Not Found"), status = 404L))
  row <- function(n) list(list(name = "Maintainer", email = "maintainer@example.org", commits = n, additions = 0L, deletions = 0L))
  if (grepl("/repository/commits?path=", path, fixed = TRUE)) {
    h <- history[[paste(p$id, utils::URLdecode(sub("^.*[?&]path=([^&]*).*$", "\\1", path)))]]
    answer(lapply(h, function(x) list(id = x[1], committed_date = x[2], author_email = "maintainer@example.org")),
           c(`x-next-page` = ""))
  }
  else if (grepl("/repository/files/.gitignore/raw?ref=", path, fixed = TRUE))
    list(status = 200L, headers = character(), error = NA_character_,
         body = if (endsWith(path, "=g2")) ".Rproj.user\n.claude\n" else ".Rproj.user\n")
  else if (grepl("order_by=commits", path, fixed = TRUE)) answer(row(p$top))
  else if (grepl("/repository/contributors", path, fixed = TRUE)) answer(row(3L), c(`x-total` = p$total))
  else answer(list(id = as.integer(p$id), license = list(key = p$license, name = "Licence")))
}

# ---- the fake Codeberg --------------------------------------------------------
# someone/cbpkg as Codeberg answers `back` daily runs before the recorded day: every unit on, one
# star more and one open issue fewer each run, three closed pull requests (two merged), a newest
# release and a newest commit. Any other path is a 404. No answer carries an address.
.golden_codeberg <- function(path, back = 0L) {
  obj <- list(id = 4242L, full_name = "someone/cbpkg", owner = list(id = 77L, login = "someone"),
    empty = FALSE, fork = FALSE, mirror = FALSE, archived = FALSE, original_url = "", website = "",
    clone_url = "https://codeberg.org/someone/cbpkg.git", stars_count = 5L - back, forks_count = 1L,
    watchers_count = 2L, open_issues_count = 1L + back, open_pr_counter = 1L, release_counter = 2L, size = 820L,
    topics = list("r", "stats"), created_at = "2023-02-11T09:30:00+01:00", has_issues = TRUE,
    has_pull_requests = TRUE, has_releases = TRUE, default_branch = "main")
  pr <- function(merged) list(number = 1L, pull_request = list(merged = merged))
  switch(path,
    "/users/someone/repos?limit=50&page=1" = cb_list(list(obj)),
    "/repos/someone/cbpkg/issues?state=closed&type=issues&limit=1" = cb_list(list(list(number = 2L)), total = 7L),
    "/repos/someone/cbpkg/issues?state=closed&type=pulls&limit=50&page=1" = cb_list(list(pr(TRUE), pr(TRUE), pr(FALSE))),
    "/repos/someone/cbpkg/releases/latest" = cb_answer(body = list(tag_name = "v0.3.0", published_at = "2026-06-01T10:00:00+02:00")),
    "/repos/someone/cbpkg/commits?limit=1&stat=false&verification=false&files=false" =
      cb_list(list(list(sha = "5d1c9e0a", commit = list(committer = list(name = "Maintainer", date = "2026-09-30T12:00:00+02:00"))))),
    cb_answer(404L, list(message = "The target couldn't be found.")))
}

# A clone of someone/cbpkg is the fixture repository of helper-codeberg-git.R; any other is not found,
# in git's own words.
.golden_cb_clone <- function(url, dir) {
  if (identical(url, "https://codeberg.org/someone/cbpkg.git")) return(cb_local_clone(cb_fixture_repo())(url, dir))
  list(status = 128L, output = sprintf("fatal: repository '%s' not found", url))
}

# The weekly documents' and the contents check's own repositories get the answers the canary tests use.
.golden_graphql <- function(gh) {
  cfg <- AI_QUERY_CANARY
  slugs <- unique(c(cfg$accounts, cfg$activity))
  keys <- sprintf('owner: "%s", name: "%s"', sub("/.*$", "", slugs), sub("^[^/]*/", "", slugs))
  canary <- ai_canary_io()$graphql
  with_contents_canary(function(q) {
    if (grepl("pullRequest(number:", q, fixed = TRUE) || any(vapply(keys, grepl, NA, x = q, fixed = TRUE)))
      return(canary(q))
    .golden_github(q, gh)
  })
}

# A bare count is the older contributors reply; cfr's read fails.
.golden_contributors <- function(owner, name) switch(tolower(paste(owner, name, sep = "/")),
  "tidyverse/forcats" = list(count = 45L, top_commits = 310L, top_type = "User"),
  "ropensci-review-tools/goodpractice" = list(count = 12L, top_commits = 200L, top_type = "Bot"),
  "kvenkita/bqmm-r" = 1L,
  "epiverse-trace/linelist" = list(count = 9L, top_commits = 150L, top_type = "Anonymous"),
  stop("HTTP 502 from the contributors endpoint"))

# Claude credits in forcats and goodpractice; every Assisted-by search is refused, the rest find nothing.
.golden_search <- function(owner, name, query) {
  page <- function(date, message) data.frame(date = date, message = message, stringsAsFactors = FALSE)
  hit <- function(items, n) list(date = items$date[1], message = items$message[1], author = "maint", total_count = n,
                                 items = items, unavailable = FALSE, incomplete = 0L)
  repo <- tolower(paste(owner, name, sep = "/"))
  if (grepl("Assisted-by", query, fixed = TRUE))
    return(list(date = NA_character_, message = NA_character_, author = NA_character_, total_count = NA_integer_,
                items = page(character(0), character(0)), unavailable = TRUE, incomplete = 0L))
  if (identical(repo, "tidyverse/forcats") && grepl("anthropic|claude", query, ignore.case = TRUE))
    return(hit(page(c("2025-10-05T05:49:23Z", "2026-01-12T09:00:00Z"),
                    c("Add a vignette\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>",
                      "Fix the docs\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>")), 2L))
  if (identical(repo, "ropensci-review-tools/goodpractice") && grepl("Co-Authored-By: Claude", query, fixed = TRUE))
    return(hit(page("2026-02-02T11:00:00Z", "Lint rules\n\nCo-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"), 1L))
  list(date = NA_character_, message = NA_character_, author = NA_character_, total_count = 0L,
       items = page(character(0), character(0)), unavailable = FALSE, incomplete = 0L)
}

.golden_cran <- function() data.frame(Package = c("forcats", "goodpractice", "bqmm", "linelist"),
                                      Version = c("1.0.1", "1.0.5", "0.1.0", "1.2.0"), stringsAsFactors = FALSE)

# The release io over `rel` as GitHub stood `back` daily runs ago. Every transport member and
# every release call notes itself in `log` first, so a request or an upload that goes missing
# or appears changes the record.
.golden_io <- function(rel, log = NULL, back = 0L) {
  note <- function(...) if (!is.null(log)) log$requests <- c(log$requests, paste(.golden_escape(c(...)), collapse = "\t"))
  graphql <- .golden_graphql(.golden_world(back))
  io <- local_release_io(rel,
    acquire = function() { note("acquire"); .golden_acquire(back) },
    graphql = function(query) { note("graphql", query); graphql(query) },
    gl_graphql = function(domain, query, variables) {
      note("gl_graphql", domain, query, as.character(jsonlite::toJSON(variables, auto_unbox = TRUE)))
      .golden_gitlab(domain, query, variables, back) },
    gl_rest = function(domain, path) { note("gl_rest", domain, path); .golden_gl_rest(domain, path) },
    cb_get = function(path) { note("cb_get", path); .golden_codeberg(path, back) },
    cb_clone = function(url, dir) { note("cb_clone", url); .golden_cb_clone(url, dir) },
    contributors = function(owner, name) { note("contributors", owner, name); .golden_contributors(owner, name) },
    # delay is pacing, not part of the request.
    search_hit = function(owner, name, query, delay = 0) {
      note("search_hit", owner, name, query); .golden_search(owner, name, query) },
    cran_packages = function() { note("cran_packages"); .golden_cran() })
  release <- io
  io$release_exists <- function() { note("release_exists"); release$release_exists() }
  io$generation <- function() { note("generation"); release$generation() }
  io$download <- function(pattern, dir) { note("download", pattern); release$download(pattern, dir) }
  io$upload <- function(path) { note("upload", basename(path)); release$upload(path) }
  io
}

# ---- running a path -----------------------------------------------------------
.golden_scripts <- c(daily = "update.R", weekly = "weekly.R", ai = "ai_backfill.R", backfill = "backfill.R")

# A fresh environment holding what the path's entry script defines, so no two paths share a run_merge.
.golden_env <- function(path) {
  e <- new.env(parent = globalenv())
  wd <- setwd(.repo_root)
  on.exit(setwd(wd))
  sys.source(file.path("scripts", .golden_scripts[[path]]), envir = e)
  e
}

# A job of the path's workflow: its own directory under `jobs`, as each Actions job has a fresh checkout.
.golden_job <- function(jobs, name) {
  d <- file.path(jobs, name)
  dir.create(d, recursive = TRUE)
  d
}

# upload-artifact from one job's out dir, download-artifact into another job's directory.
.golden_artifact <- function(from_job, pattern, to_dir) {
  dir.create(to_dir, showWarnings = FALSE, recursive = TRUE)
  f <- list.files(file.path(from_job, "out"), pattern = utils::glob2rx(pattern), full.names = TRUE)
  invisible(file.copy(f, to_dir, overwrite = TRUE))
}

.golden_daily <- function(fn, io, out, root, force_full)
  fn("run_update")(io, out, list(force_full = force_full, links_backfill = .golden_links_backfill(root)))

.golden_shards <- function() seq_len(GOLDEN_SHARDS) - 1L

# The GitLab job's clock: noon UTC on the recorded day, moved only by the job's own pauses.
.golden_gl_clock <- function() {
  clock <- new.env(parent = emptyenv())
  clock$t <- as.numeric(as.POSIXct(paste(GOLDEN_DAY, "12:00:00"), tz = "UTC"))
  list(now = function() clock$t, sleep = function(s) clock$t <- clock$t + s)
}

# weekly.yml and backfill.yml: enumerate, a fetch job per shard reading the roster artifact, and
# a merge reading every shard artifact. weekly.yml also runs fetch-gitlab on the GitLab roster and
# fetch-codeberg on the Codeberg roster, and its merge reads their shards and the GitLab cursor state.
# `fn` finds each stage in the path's own environment.
.golden_sharded <- function(fn, io, jobs, root, gitlab = FALSE, codeberg = FALSE) {
  e <- .golden_job(jobs, "enumerate")
  fn(if (gitlab || codeberg) "run_weekly_enumerate" else "run_enumerate")(io, file.path(e, "out"))
  for (i in .golden_shards()) {
    f <- .golden_job(jobs, sprintf("fetch-%d", i))
    .golden_artifact(e, "vcs-signals-roster.db", file.path(f, "roster"))
    fn("run_fetch_shard")(io, file.path(f, "out"), file.path(f, "roster", "vcs-signals-roster.db"), i, GOLDEN_SHARDS)
  }
  if (gitlab) {
    g <- .golden_job(jobs, "fetch-gitlab")
    .golden_artifact(e, "vcs-signals-roster-gitlab.db", file.path(g, "roster"))
    clock <- .golden_gl_clock()
    fn("run_fetch_gitlab")(io, file.path(g, "out"), file.path(g, "roster", "vcs-signals-roster-gitlab.db"),
                           file.path(g, "out", "vcs-signals-gitlab-state.db"), now = clock$now, sleep = clock$sleep)
  }
  if (codeberg) {
    cb <- .golden_job(jobs, "fetch-codeberg")
    .golden_artifact(e, "vcs-signals-roster-codeberg.db", file.path(cb, "roster"))
    clock <- .golden_gl_clock()
    fn("run_fetch_codeberg")(io, file.path(cb, "out"), file.path(cb, "roster", "vcs-signals-roster-codeberg.db"),
                             now = clock$now, sleep = clock$sleep)
  }
  m <- .golden_job(jobs, "merge")
  for (i in .golden_shards())
    .golden_artifact(file.path(jobs, sprintf("fetch-%d", i)), "vcs-signals-shard-*.db", file.path(m, "parts"))
  if (gitlab)
    for (f in c("vcs-signals-shard-gitlab.db", "vcs-signals-gitlab-state.db")) .golden_artifact(g, f, file.path(m, "parts"))
  if (codeberg) .golden_artifact(cb, "vcs-signals-shard-codeberg.db", file.path(m, "parts"))
  fn("run_merge")(io, file.path(m, "out"), file.path(m, "parts"))
}

# ai-weekly.yml: enumerate, cheap jobs on the roster artifact and gitlab-contents on its GitLab roster,
# the gate on the cheap partials, deep jobs and gitlab-onsets on the flagged roster, and a merge on the
# search, cheap and dev-tooling artifacts.
.golden_ai <- function(fn, io, jobs, root) {
  e <- .golden_job(jobs, "enumerate")
  fn("run_enumerate_ai")(io, file.path(e, "out"))
  gl <- .golden_job(jobs, "gitlab-contents")
  .golden_artifact(e, "vcs-ai-roster-gitlab.db", file.path(gl, "roster"))
  clock <- .golden_gl_clock()
  fn("run_gitlab_contents")(io, file.path(gl, "out"), file.path(gl, "roster", "vcs-ai-roster-gitlab.db"),
                            now = clock$now, sleep = clock$sleep)
  for (i in .golden_shards()) {
    c_ <- .golden_job(jobs, sprintf("cheap-%d", i))
    .golden_artifact(e, "vcs-ai-roster.db", file.path(c_, "roster"))
    fn("run_cheap")(io, file.path(c_, "out"), file.path(c_, "roster", "vcs-ai-roster.db"), i, GOLDEN_SHARDS)
  }
  cheap <- c(file.path(jobs, sprintf("cheap-%d", .golden_shards())), gl)
  g <- .golden_job(jobs, "gate")
  for (c_ in cheap) .golden_artifact(c_, "vcs-ai-cheap-*.db", file.path(g, "parts"))
  fn("run_gate")(io, file.path(g, "out"), file.path(g, "parts"))
  for (i in .golden_shards()) {
    d <- .golden_job(jobs, sprintf("deep-%d", i))
    .golden_artifact(g, "vcs-ai-flagged-roster.db", file.path(d, "flagged"))
    fn("run_deep")(io, file.path(d, "out"), file.path(d, "flagged", "vcs-ai-flagged-roster.db"), i, GOLDEN_SHARDS)
  }
  o <- .golden_job(jobs, "gitlab-onsets")
  .golden_artifact(g, "vcs-ai-flagged-roster.db", file.path(o, "flagged"))
  clock <- .golden_gl_clock()
  fn("run_gitlab_onsets")(io, file.path(o, "out"), file.path(o, "flagged", "vcs-ai-flagged-roster.db"),
                          now = clock$now, sleep = clock$sleep)
  m <- .golden_job(jobs, "merge")
  for (d in c(file.path(jobs, sprintf("deep-%d", .golden_shards())), o))
    .golden_artifact(d, "vcs-ai-shard-*.db", file.path(m, "parts"))
  for (c_ in cheap) {
    .golden_artifact(c_, "vcs-ai-cheap-*.db", file.path(m, "parts"))
    .golden_artifact(c_, "vcs-dev-tooling-*.db", file.path(m, "parts"))
  }
  fn("run_merge")(io, file.path(m, "out"), file.path(m, "parts"))
}

.golden_steps <- list(
  daily = function(fn, io, jobs, root) .golden_daily(fn, io, file.path(.golden_job(jobs, "update"), "out"), root, TRUE),
  weekly = function(fn, io, jobs, root) .golden_sharded(fn, io, jobs, root, gitlab = TRUE, codeberg = TRUE),
  ai = .golden_ai,
  backfill = .golden_sharded)

# Stars for the r-forge repository in the seeded release, so the GitHub-only rosters of the
# later paths have a signalled row with no collector to leave out. The GitLab and Codeberg rows
# carry what the daily runs collected.
.golden_other_hosts <- function(rel) {
  ids <- "r-forge.r-project.org/projects/rfpkg"
  stars <- 2L
  for (asset in c("vcs-signals-recent.db", "vcs-signals-summary.db")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, asset))
    if (identical(asset, "vcs-signals-recent.db"))
      DBI::dbExecute(con, "INSERT INTO series_latest (repo_id, metric, value) VALUES (?, 'stars', ?)",
                     params = list(ids, stars))
    DBI::dbExecute(con, "UPDATE vcs_signals_summary SET stars = ? WHERE repo_id = ?", params = list(stars, ids))
    DBI::dbDisconnect(con)
  }
}

# Pins the day and drops the pauses for one run. set(day) moves the pinned day; restore() puts
# everything back.
.golden_pin <- function() {
  g <- globalenv()
  pinned <- c("Sys.Date", "BATCH_DELAY_S", "BACKFILL_DELAY_S", "CONTRIBUTOR_DELAY_S")
  had <- vapply(pinned, exists, NA, envir = g, inherits = FALSE)
  old <- mget(pinned[had], envir = g)
  clock <- new.env(parent = emptyenv())
  clock$day <- as.Date(GOLDEN_DAY)
  assign("Sys.Date", function() clock$day, envir = g)
  for (n in pinned[-1]) assign(n, 0, envir = g)
  list(set = function(day) clock$day <- as.Date(day),
       restore = function() {
         rm(list = pinned[!had], envir = g)
         for (n in names(old)) assign(n, old[[n]], envir = g)
       })
}

.golden_quiet <- function(expr) {
  utils::capture.output(res <- suppressMessages(expr))
  invisible(res)
}

# Runs are deterministic, so one per path serves every test in a session.
.golden_cache <- new.env(parent = emptyenv())

#' Runs one path over a release the earlier daily runs built (the other paths also over the
#' recorded day's daily run, plus .golden_other_hosts) and returns its tables, normalised, the
#' requests and release calls it made, in order, its manifest and release notes, and the
#' message it stopped with, if any.
golden_run <- function(path, record_requests = TRUE) {
  stopifnot(length(path) == 1L, path %in% golden_paths())
  key <- paste(path, record_requests)
  if (!is.null(.golden_cache[[key]])) return(.golden_cache[[key]])
  t0 <- Sys.time()
  root <- tempfile(paste0("golden-", path, "-"))
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  rel <- file.path(root, "release")
  dir.create(rel)
  daily <- .golden_env("daily")
  env <- if (identical(path, "daily")) daily else .golden_env(path)
  clock <- .golden_pin()
  on.exit(clock$restore(), add = TRUE)
  fn <- function(e) function(name) get(name, envir = e, mode = "function")
  for (day in c(GOLDEN_EARLIER, if (!identical(path, "daily")) GOLDEN_DAY)) {
    clock$set(day)
    .golden_quiet(.golden_daily(fn(daily), .golden_io(rel, back = .golden_back(day)),
                                file.path(root, paste0("seed-", day)), root, identical(day, GOLDEN_DAY)))
  }
  if (!identical(path, "daily")) .golden_other_hosts(rel)
  clock$set(GOLDEN_DAY)
  log <- new.env(parent = emptyenv())
  log$requests <- character(0)
  jobs <- file.path(root, "jobs")
  # A path that stops keeps what its jobs left, and its message is recorded.
  err <- tryCatch({
    .golden_quiet(.golden_steps[[path]](fn(env), .golden_io(rel, if (record_requests) log), jobs, root))
    NULL
  }, error = function(e) conditionMessage(e))
  t1 <- Sys.time()
  run <- c(.golden_jobs_snapshot(jobs, GOLDEN_DAY, t0, t1), list(requests = log$requests))
  if (!is.null(err)) run$files[["error.txt"]] <- .golden_normalise_text(err, GOLDEN_DAY, t0, t1)
  assign(key, run, envir = .golden_cache)
  run
}

# ---- snapshot -------------------------------------------------------------------
# Every table of every .db in each job's out dir, and its schema, named "<job>@<db file>/<table>",
# with each job's manifest and release notes as text. A job's published files come before its
# own working files, whose names start with an underscore.
.golden_jobs_snapshot <- function(jobs, run_date, t0, t1) {
  tables <- list(); files <- character(0)
  for (job in sort(list.files(jobs), method = "radix")) {
    out <- file.path(jobs, job, "out")
    dbs <- list.files(out, pattern = "\\.db$")
    for (db in dbs[order(startsWith(dbs, "_"), dbs, method = "radix")]) {
      label <- paste0(job, "@", db)
      con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, db))
      for (t in sort(DBI::dbListTables(con), method = "radix"))
        tables[[paste(label, t, sep = "/")]] <- golden_normalise(DBI::dbReadTable(con, t), run_date, t0, t1)
      tables[[paste(label, "sqlite_master", sep = "/")]] <-
        DBI::dbGetQuery(con, "SELECT type, name, tbl_name, sql FROM sqlite_master")
      DBI::dbDisconnect(con)
    }
    if (file.exists(file.path(out, "manifest.json")))
      files[[paste0(job, "@manifest.json.tsv")]] <- .golden_manifest(file.path(out, "manifest.json"), run_date, t0, t1)
    if (file.exists(file.path(out, "release_notes.md")))
      files[[paste0(job, "@release_notes.md")]] <- .golden_normalise_text(
        paste(readLines(file.path(out, "release_notes.md"), warn = FALSE), collapse = "\n"), run_date, t0, t1)
  }
  list(tables = tables, files = files)
}

.golden_iso_re <- paste0("([0-9]{4}-[0-9]{2}-[0-9]{2})[T ]([0-9]{2}:[0-9]{2}:[0-9]{2})",
                         "(\\.[0-9]+)?(Z|[+-][0-9]{2}:?[0-9]{2})?")

.golden_iso_seconds <- function(x) {
  m <- regmatches(x, regexec(paste0("^", .golden_iso_re, "$"), x))
  vapply(m, function(p) {
    if (length(p) != 5L) return(NA_real_)
    s <- as.numeric(as.POSIXct(paste(p[2], p[3]), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    z <- gsub(":", "", p[5], fixed = TRUE)
    if (nchar(z) == 5L)
      s <- s - (if (startsWith(z, "-")) -1 else 1) * (3600 * as.numeric(substr(z, 2, 3)) + 60 * as.numeric(substr(z, 4, 5)))
    s
  }, numeric(1))
}

#' Masks a cell only when it is the run's date, or a timestamp inside the run's wall-clock
#' window [t0, t1], which keeps its shape with each digit as d. Other columns keep their type.
golden_normalise <- function(df, run_date, t0, t1) {
  lo <- floor(as.numeric(t0)); hi <- ceiling(as.numeric(t1))
  for (cn in names(df)) {
    x <- df[[cn]]
    if (!is.character(x)) next
    s <- .golden_iso_seconds(x)
    now <- !is.na(s) & s >= lo & s <= hi
    x[!is.na(x) & x == run_date] <- "<DATE>"
    x[now] <- paste0("<TS:", gsub("[0-9]", "d", x[now]), ">")
    df[[cn]] <- x
  }
  df
}

# The same masks over free text: timestamps of the run, then the run's date standing alone.
.golden_normalise_text <- function(txt, run_date, t0, t1) {
  m <- gregexpr(.golden_iso_re, txt)
  regmatches(txt, m) <- lapply(regmatches(txt, m), function(s)
    golden_normalise(data.frame(v = s, stringsAsFactors = FALSE), run_date, t0, t1)$v)
  gsub(sprintf("(?<![0-9:-])%s(?![0-9T:])", run_date), "<DATE>", txt, perl = TRUE)
}

.golden_escape <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  gsub("\r", "\\r", x, fixed = TRUE)
}

# One column as text: doubles exactly, NA as \N.
.golden_text <- function(x) {
  if (is.list(x)) x <- vapply(x, function(v) if (is.null(v)) NA_character_ else paste(format(v), collapse = ""), "")
  na <- is.na(x)
  s <- if (inherits(x, "integer64")) as.character(x)
       else if (is.double(x)) {
         s <- sprintf("%.15g", x)
         loose <- !na & suppressWarnings(as.numeric(s)) != x
         s[loose] <- sprintf("%.17g", x[loose])
         s
       } else .golden_escape(as.character(x))
  s[na] <- "\\N"
  s
}

.golden_tsv <- function(df) {
  cols <- lapply(unname(as.list(df)), .golden_text)
  rows <- character(0)
  if (nrow(df)) {
    o <- do.call(order, c(cols, list(method = "radix")))
    rows <- do.call(paste, c(lapply(cols, `[`, o), sep = "\t"))
  }
  paste(c(paste(names(df), collapse = "\t"), rows), collapse = "\n")
}

# manifest.json as one "key<TAB>value" line per field, in its own order, so the record
# does not depend on how jsonlite prints. The summary db's byte size and sha256 (and
# those of the previous summary) are masked: the SQLite file header carries the library
# version, so they change with an RSQLite upgrade while every table and schema they cover is recorded.
.golden_manifest <- function(path, run_date, t0, t1) {
  masked <- c("db_bytes", "db_sha256", "summary.previous_summary.bytes", "summary.previous_summary.sha256")
  flat <- function(x, key) {
    if (is.null(x)) return(stats::setNames("null", key))
    if (is.list(x)) {
      if (!length(x)) return(stats::setNames(if (is.null(names(x))) "[]" else "{}", key))
      sub <- if (is.null(names(x))) sprintf("%s[%d]", key, seq_along(x))
             else if (nzchar(key)) paste(key, names(x), sep = ".") else names(x)
      return(unlist(unname(Map(flat, x, sub))))
    }
    v <- if (is.logical(x)) tolower(as.character(x)) else .golden_text(x)
    if (is.character(x)) v <- golden_normalise(data.frame(v = v, stringsAsFactors = FALSE), run_date, t0, t1)$v
    stats::setNames(v, key)
  }
  kv <- flat(jsonlite::fromJSON(path, simplifyVector = FALSE), "")
  kv[names(kv) %in% masked] <- "<MASKED>"
  paste(names(kv), kv, sep = "\t", collapse = "\n")
}

#' Each table as sorted TSV text, the requests one per line, each job's manifest and
#' release notes, and the stop message, by record file name.
golden_snapshot_text <- function(run) {
  tsv <- vapply(run$tables, .golden_tsv, character(1))
  names(tsv) <- paste0(sub("/", "__", names(run$tables), fixed = TRUE), ".tsv")
  c(tsv, requests.txt = paste(run$requests, collapse = "\n"), run$files)
}
