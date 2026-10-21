# The GitLab job of the weekly AI scan (ai_backfill.R gitlab-contents), its roster, and how the merge
# folds what it writes. ai_backfill.R is sourced from the repo root.
.gla_wd <- setwd(.repo_root); source(file.path(.repo_root, "scripts", "ai_backfill.R")); setwd(.gla_wd)

gla_email <- "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[a-z]{2,}"

# A fake clock: sleeping moves it.
gla_clock <- function() {
  clock <- new.env(parent = emptyenv())
  clock$t <- 0
  list(now = function() clock$t, sleep = function(s) clock$t <- clock$t + s)
}

# A log that keeps its lines.
gla_log <- function() {
  kept <- new.env(parent = emptyenv())
  kept$lines <- character(0)
  list(log = function(m) kept$lines <- c(kept$lines, m), lines = function() kept$lines)
}

# A GitLab AI roster file for repositories <domain>/<owner>/<name> with these node ids.
gla_roster <- function(dir, repo_ids, node_ids, cran = NULL) {
  path <- file.path(dir, AI_GITLAB_ROSTER_DB)
  write_ai_gitlab_roster(path, data.frame(repo_id = repo_ids, node_id = node_ids,
    domain = sub(.GL_NODE_RE, "\\1", node_ids), project_id = as.integer(sub("^.*:", "", node_ids)),
    owner = sub("^[^/]+/(.*)/[^/]+$", "\\1", repo_ids), name = sub("^.*/", "", repo_ids),
    stringsAsFactors = FALSE), cran)
  path
}

# A project's contents answer: not a fork, a DESCRIPTION whose author has an address, and `root` names.
gla_node <- function(gid, root = character(0), desc = TRUE) {
  files <- if (desc) list(DESCRIPTION = "Package: p\nVersion: 1.0\nAuthors@R: person('A', 'B', email = 'a.b@example.org')\n")
  list(id = gid, fullPath = "o/p", isForked = FALSE, forkedFrom = NULL, ciConfigPathOrDefault = ".gitlab-ci.yml",
       pagesDeployments = list(count = 0L), issuesEnabled = TRUE,
       repository = list(rootRef = "main", empty = FALSE,
         root = list(trees = list(nodes = list(list(name = "R"))),
                     blobs = list(nodes = lapply(c(names(files), root), function(n) list(name = n)))),
         blobs = list(nodes = lapply(names(files), function(p) list(path = p, rawTextBlob = files[[p]])))))
}

gla_credit <- "x\n\nCo-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
gla_commit <- function(sha, at) list(sha = sha, committedDate = at, message = gla_credit)
gla_commits <- function(gid, nodes, more = FALSE, cursor = NULL)
  list(id = gid, repository = list(commits = list(pageInfo = list(hasNextPage = more, endCursor = cursor), nodes = nodes)))
gla_no_search <- list(data = NULL, status = 200L, error = NA_character_, errors = list(list(
  message = "Field 'commits' doesn't exist on type 'Repository'",
  extensions = list(code = "undefinedField", fieldName = "commits"))))
gla_scored <- function(res, score) { res$data$queryComplexity <- list(score = score, limit = 200L); res }

gla_expect_no_email <- function(dir) {
  for (db in list.files(dir, "\\.db$", full.names = TRUE)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    for (t in DBI::dbListTables(con)) for (v in DBI::dbReadTable(con, t))
      expect_false(any(grepl(gla_email, v)), info = paste(db, t))
    DBI::dbDisconnect(con)
  }
}

# ---- roster ----------------------------------------------------------------------
# A published summary with one active GitHub repository and, unless `gitlab` is FALSE, an active
# GitLab repository with a node id, one without, and a retired one; each has a CRAN package.
gla_summary_release <- function(gitlab = TRUE) {
  rel <- tempfile("rel_"); dir.create(rel)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-summary.db"))
  ensure_repo_schema(con)
  rows <- c("('github.com/a/keep','R_a','github','github.com','a','keep','a/keep',1,1,'2024-01-01','2026-10-01','active')",
    if (gitlab) c(
      "('gitlab.com/grp/sub/pkg','gitlab:gitlab.com:77','gitlab','gitlab.com','grp/sub','pkg','grp/sub/pkg',1,1,'2024-01-01','2026-10-01','active')",
      "('gitlab.com/o/unresolved',NULL,'gitlab','gitlab.com','o','unresolved','o/unresolved',1,1,'2024-01-01','2026-10-01','active')",
      "('code.usgs.gov/o/retired','gitlab:code.usgs.gov:5','gitlab','code.usgs.gov','o','retired','o/retired',1,1,'2024-01-01','2026-10-01','gone')"))
  DBI::dbExecute(con, paste("INSERT INTO repos (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status) VALUES",
                            paste(rows, collapse = ",\n")))
  pk <- c("('github.com/a/keep','keep','cran','url')",
          if (gitlab) c("('gitlab.com/grp/sub/pkg','glpkg','cran','url')", "('gitlab.com/o/unresolved','unres','cran','url')",
                        "('code.usgs.gov/o/retired','gone','cran','url')"))
  DBI::dbExecute(con, paste("INSERT INTO repo_packages (repo_id, package, origin, resolved_from) VALUES", paste(pk, collapse = ", ")))
  DBI::dbDisconnect(con)
  rcon <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-recent.db"))
  ensure_series_schema(rcon)
  DBI::dbDisconnect(rcon)
  rel
}

# The weekly documents' own repositories get the answers the canary tests use; the re-resolve gets none.
gla_enum_graphql <- function() {
  slugs <- unique(c(AI_QUERY_CANARY$accounts, AI_QUERY_CANARY$activity))
  keys <- sprintf('owner: "%s", name: "%s"', sub("/.*$", "", slugs), sub("^[^/]*/", "", slugs))
  canary <- ai_canary_io()$graphql
  with_contents_canary(function(q)
    if (grepl("pullRequest(number:", q, fixed = TRUE) || any(vapply(keys, grepl, NA, x = q, fixed = TRUE))) canary(q)
    else list(data = list()))
}

gla_enumerate <- function(rel, out = tempfile("out_")) {
  cran_calls <- new.env(parent = emptyenv()); cran_calls$n <- 0L
  io <- list(release_exists = function() TRUE, graphql = gla_enum_graphql(),
    download = function(pattern, dir) {
      f <- list.files(rel, pattern = utils::glob2rx(pattern), full.names = TRUE)
      if (!length(f)) return(FALSE)
      all(file.copy(f, file.path(dir, basename(f)), overwrite = TRUE)) },
    cran_packages = function() {
      cran_calls$n <- cran_calls$n + 1L
      data.frame(Package = c("keep", "glpkg", "unres", "gone"), Version = c("1.0", "2.0", "3.0", "4.0"))
    })
  dir.create(out, showWarnings = FALSE)
  msgs <- testthat::capture_messages(run_enumerate_ai(io, out))
  list(out = out, cran_calls = cran_calls$n, msgs = msgs)
}

test_that("enumerate writes the GitLab roster with its CRAN versions and leaves the GitHub roster as it was", {
  with_gl <- gla_enumerate(gla_summary_release(gitlab = TRUE))
  without <- gla_enumerate(gla_summary_release(gitlab = FALSE))
  gh <- function(run) file.path(run$out, "vcs-ai-roster.db")
  expect_identical(load_ai_roster(gh(with_gl)), load_ai_roster(gh(without)))
  expect_identical(load_roster_cran(gh(with_gl)), load_roster_cran(gh(without)))
  expect_identical(load_roster_cran(gh(with_gl))$package, "keep")
  expect_identical(nrow(load_ai_gitlab_roster(file.path(without$out, AI_GITLAB_ROSTER_DB))), 0L)
  gl_path <- file.path(with_gl$out, AI_GITLAB_ROSTER_DB)
  expect_identical(load_ai_gitlab_roster(gl_path), data.frame(repo_id = "gitlab.com/grp/sub/pkg",
    node_id = "gitlab:gitlab.com:77", domain = "gitlab.com", project_id = 77L, owner = "grp/sub", name = "pkg",
    stringsAsFactors = FALSE))
  expect_identical(load_roster_cran(gl_path), data.frame(repo_id = "gitlab.com/grp/sub/pkg", package = "glpkg",
                                                         cran_version = "2.0", stringsAsFactors = FALSE))
  expect_identical(with_gl$cran_calls, 1L)
})

test_that("a GitLab roster that cannot be written is logged, and the GitHub roster is written as it was", {
  out <- tempfile("out_")
  # A folder where the file should go makes the GitLab write fail.
  dir.create(file.path(out, AI_GITLAB_ROSTER_DB), recursive = TRUE)
  run <- gla_enumerate(gla_summary_release(gitlab = TRUE), out)
  expect_identical(load_ai_roster(file.path(out, "vcs-ai-roster.db"))$repo_id, "github.com/a/keep")
  expect_identical(load_roster_cran(file.path(out, "vcs-ai-roster.db"))$package, "keep")
  expect_true(dir.exists(file.path(out, AI_GITLAB_ROSTER_DB)))
  expect_true(any(grepl("no GitLab roster written", run$msgs, fixed = TRUE)))
})

test_that("a GitLab node id is gitlab, a domain and a project number", {
  expect_identical(is_gitlab_node(c("gitlab:gitlab.com:7", "gitlab:git.sc.uni-leipzig.de:12", "R_x", NA,
                                    "gitlab:gitlab.com:", "gitlab:gitlab.com:7x")),
                   c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
})

# ---- the GitLab read -----------------------------------------------------------
# gitlab.com/o/a has CLAUDE.md and one credited commit; git.sc.uni-leipzig.de/o/b has no message search;
# gitlab.com/o/c times out on its contents and the message search leaves it out; gitlab.com/o/d credits
# Claude on every page, past the page cap.
gla_four <- c(a = "gitlab:gitlab.com:1", b = "gitlab:git.sc.uni-leipzig.de:2", c = "gitlab:gitlab.com:3",
              d = "gitlab:gitlab.com:4")
gla_four_ids <- c("gitlab.com/o/a", "git.sc.uni-leipzig.de/o/b", "gitlab.com/o/c", "gitlab.com/o/d")
gla_four_world <- function() fake_gitlab(function(domain, query, variables) {
  ids <- unlist(variables$ids)
  if (!is.null(variables$re)) {
    if (domain == "git.sc.uni-leipzig.de") return(gla_no_search)
    page <- if (is.null(variables$after)) 1L else as.integer(sub("^c", "", variables$after)) + 1L
    nodes <- lapply(ids, function(g) {
      if (g == .gl_gid(gla_four[["a"]])) gla_commits(g, list(gla_commit("a1", "2026-03-02T09:00:00Z")))
      else if (g == .gl_gid(gla_four[["d"]]))
        gla_commits(g, list(gla_commit(paste0("d", page), "2026-04-01T09:00:00Z")), more = TRUE, cursor = paste0("c", page))
    })
    return(gl_ok(Filter(Negate(is.null), nodes)))
  }
  if (.gl_gid(gla_four[["c"]]) %in% ids) return(list(error = "Operation timed out", status = NA_integer_))
  gl_ok(lapply(ids, function(g) gla_node(g, root = if (g == .gl_gid(gla_four[["a"]])) "CLAUDE.md")))
})

test_that("the GitLab read writes dev-tooling rows, file and commit-credit findings, and no email", {
  dir <- withr::local_tempdir(); out <- file.path(dir, "out")
  roster_path <- gla_roster(dir, gla_four_ids, unname(gla_four))
  clock <- gla_clock()
  run_gitlab_contents(gla_four_world()$io, out, roster_path, now = clock$now, sleep = clock$sleep, log = function(...) NULL)
  dev <- read_dev_tooling(file.path(out, "vcs-dev-tooling-gitlab.db"))
  expect_setequal(dev$repo_id, c("gitlab.com/o/a", "git.sc.uni-leipzig.de/o/b", "gitlab.com/o/d"))
  expect_true(all(dev$ruleset_version == DEV_TOOLING_RULESET_VERSION))
  fr <- read_flagged(file.path(out, "vcs-ai-cheap-gitlab.db"))
  expect_setequal(fr$flagged$repo_id, c("gitlab.com/o/a", "gitlab.com/o/d"))
  a <- fr$flagged[fr$flagged$repo_id == "gitlab.com/o/a", ]
  expect_identical(list(a$owner, a$name, a$node_id, a$is_fork, a$pr_onset_date),
                   list("o", "a", "gitlab:gitlab.com:1", 0L, NA_character_))
  ev <- fr$evidence[fr$evidence$repo_id == "gitlab.com/o/a", ]
  expect_true("CLAUDE.md" %in% ev$marker)
  expect_identical(ev$onset_censored[ev$rule_key %in% "msg.claude.coauthor"], 0L)
  expect_identical(ev$onset[ev$rule_key %in% "msg.claude.coauthor"], "2026-03-02T09:00:00Z")
  log <- .ai_part_table(file.path(out, "vcs-ai-cheap-gitlab.db"), "search_log")
  expect_false(any(log$repo_id == "git.sc.uni-leipzig.de/o/b"))
  expect_true(all(log$mode[log$repo_id == "gitlab.com/o/a"] == "replace"))
  expect_identical(log$outcome[log$repo_id == "gitlab.com/o/a" & log$rule_key == "msg.claude.coauthor"], "hit")
  expect_setequal(gitlab_rebuilt_repos(log), "gitlab.com/o/a")
  d <- fr$evidence[fr$evidence$repo_id == "gitlab.com/o/d" & fr$evidence$rule_key %in% "msg.claude.coauthor", ]
  expect_identical(d$onset_censored, 1L)
  models <- .ai_part_table(file.path(out, "vcs-ai-cheap-gitlab.db"), "models")
  expect_identical(models$version[models$repo_id == "gitlab.com/o/a"], "5")
  expect_false("gitlab.com/o/d" %in% models$repo_id)
  for (t in c("repo_reads", "account_counts", "outside_prs"))
    expect_identical(nrow(.ai_part_table(file.path(out, "vcs-ai-cheap-gitlab.db"), t)), 0L, info = t)
  expect_identical(read_scan_failures(file.path(out, "vcs-dev-tooling-gitlab.db"))$query, "gitlab-contents")
  gla_expect_no_email(out)
})

test_that("a repository whose read gave no fork flag is flagged as a possible fork", {
  dir <- withr::local_tempdir(); out <- file.path(dir, "out")
  roster_path <- gla_roster(dir, "gitlab.com/o/a", "gitlab:gitlab.com:1")
  f <- fake_gitlab(function(domain, query, variables) {
    g <- unlist(variables$ids)
    if (!is.null(variables$re)) return(gl_ok(list(gla_commits(g, list()))))
    n <- gla_node(g, root = "CLAUDE.md"); n$isForked <- NULL
    gl_ok(list(n))
  })
  clock <- gla_clock()
  run_gitlab_contents(f$io, out, roster_path, now = clock$now, sleep = clock$sleep, log = function(...) NULL)
  expect_identical(read_flagged(file.path(out, "vcs-ai-cheap-gitlab.db"))$flagged$is_fork, 1L)
})

test_that("the GitLab read logs each host's projects, the fields it left out, the search and its highest complexity", {
  dir <- withr::local_tempdir()
  roster_path <- gla_roster(dir, c("gitlab.com/o/a", "gitlab.com/o/c", "git.sc.uni-leipzig.de/o/b"),
                            c("gitlab:gitlab.com:1", "gitlab:gitlab.com:3", "gitlab:git.sc.uni-leipzig.de:2"))
  rejected <- list(data = NULL, status = 200L, error = NA_character_, errors = list(
    list(message = "Field 'forkedFrom' doesn't exist on type 'Project'", extensions = list(code = "undefinedField", fieldName = "forkedFrom")),
    list(message = "Field 'pagesDeployments' doesn't exist on type 'Project'", extensions = list(code = "undefinedField", fieldName = "pagesDeployments"))))
  f <- fake_gitlab(function(domain, query, variables) {
    ids <- unlist(variables$ids)
    if (!is.null(variables$re)) {
      if (domain == "git.sc.uni-leipzig.de") return(gla_no_search)
      return(gla_scored(gl_ok(lapply(ids, function(g) gla_commits(g, list()))), 40L))
    }
    if (domain == "git.sc.uni-leipzig.de") {
      if (grepl("forkedFrom", query, fixed = TRUE)) return(rejected)
      return(gla_scored(gl_ok(lapply(ids, gla_node)), 120L))
    }
    gla_scored(gl_ok(lapply(ids, gla_node)), 190L)
  })
  lg <- gla_log(); clock <- gla_clock()
  run_gitlab_contents(f$io, file.path(dir, "out"), roster_path, now = clock$now, sleep = clock$sleep, log = lg$log)
  line <- function(d) grep(sprintf("^gitlab contents: %s,", d), lg$lines(), value = TRUE)
  expect_length(line("gitlab.com"), 1L); expect_length(line("git.sc.uni-leipzig.de"), 1L)
  expect_match(line("gitlab.com"), "projects asked 2, read 2", fixed = TRUE)
  expect_match(line("gitlab.com"), "fields left out: none", fixed = TRUE)
  expect_match(line("gitlab.com"), "message search: there, searched 2 (2 whole)", fixed = TRUE)
  expect_match(line("gitlab.com"), "highest complexity 190", fixed = TRUE)
  expect_match(line("git.sc.uni-leipzig.de"), "projects asked 1, read 1", fixed = TRUE)
  expect_match(line("git.sc.uni-leipzig.de"), "fields left out: forkedFrom, pagesDeployments", fixed = TRUE)
  expect_match(line("git.sc.uni-leipzig.de"), "message search: not on this host, searched 0", fixed = TRUE)
  expect_match(line("git.sc.uni-leipzig.de"), "highest complexity 120", fixed = TRUE)
})

test_that("the GitLab read shares one request cap between the contents read and the message search", {
  dir <- withr::local_tempdir()
  n <- 2L * GITLAB_CONTENTS_BATCH
  roster_path <- gla_roster(dir, sprintf("gitlab.com/o/p%03d", seq_len(n)), sprintf("gitlab:gitlab.com:%d", seq_len(n)))
  f <- fake_gitlab(function(domain, query, variables) {
    ids <- unlist(variables$ids)
    if (!is.null(variables$re))
      return(gl_ok(lapply(ids, function(g) gla_commits(g, list(gla_commit(paste0(g, "-1"), "2026-03-02T09:00:00Z")),
                                                       more = TRUE, cursor = "c1"))))
    gl_ok(lapply(ids, gla_node))
  })
  clock <- gla_clock()
  run_gitlab_contents(f$io, file.path(dir, "out"), roster_path, now = clock$now, sleep = clock$sleep, log = function(...) NULL)
  expect_identical(length(f$calls$graphql), GITLAB_AI_MAX_REQUESTS)
  expect_identical(sum(vapply(f$calls$graphql, function(x) is.null(x$variables$re), NA)), 2L)
})

test_that("the GitLab read stops before writing when most projects come back empty", {
  expect_match(gitlab_contents_canary(25L, list()), "came back")
  expect_null(gitlab_contents_canary(5L, list()))
  trees <- function(n_desc) stats::setNames(lapply(seq_len(25), function(i)
    list(root_entries = c("R", if (i <= n_desc) "DESCRIPTION"))), sprintf("gitlab.com/o/p%02d", 1:25))
  expect_match(gitlab_contents_canary(25L, trees(5L)), "DESCRIPTION")
  expect_null(gitlab_contents_canary(25L, trees(13L)))
  expect_match(gitlab_contents_canary(25L, trees(25L)[1:12]), "came back")
  # 25 asked, 25 returned, 5 with DESCRIPTION: the run stops before the message search and writes no file.
  dir <- withr::local_tempdir(); out <- file.path(dir, "out")
  roster_path <- gla_roster(dir, sprintf("gitlab.com/o/p%02d", 1:25), sprintf("gitlab:gitlab.com:%d", 1:25))
  f <- fake_gitlab(function(domain, query, variables) {
    if (!is.null(variables$re)) stop("the message search must not be asked")
    gl_ok(lapply(unlist(variables$ids), function(g) gla_node(g, desc = as.integer(sub("^.*/", "", g)) <= 5L)))
  })
  clock <- gla_clock()
  expect_error(run_gitlab_contents(f$io, out, roster_path, now = clock$now, sleep = clock$sleep, log = function(...) NULL),
               "DESCRIPTION")
  expect_length(list.files(out), 0L)
  expect_false(any(vapply(f$calls$graphql, function(x) !is.null(x$variables$re), NA)))
})

# The message check: GITLAB_MESSAGE_CANARY and one other project on gitlab.com.
gla_canary_run <- function(credits = TRUE, unsupported = FALSE, on_roster = TRUE) {
  dir <- withr::local_tempdir(.local_envir = parent.frame()); out <- file.path(dir, "out")
  puny <- "gitlab:gitlab.com:85027327"
  ids <- c(if (on_roster) GITLAB_MESSAGE_CANARY, "gitlab.com/o/q")
  roster_path <- gla_roster(dir, ids, c(if (on_roster) puny, "gitlab:gitlab.com:9"))
  f <- fake_gitlab(function(domain, query, variables) {
    gids <- unlist(variables$ids)
    if (is.null(variables$re)) return(gl_ok(lapply(gids, gla_node)))
    if (unsupported) return(gla_no_search)
    gl_ok(lapply(gids, function(g) gla_commits(g,
      if (credits && g == .gl_gid(puny)) list(gla_commit("p1", "2026-05-24T09:58:54Z")) else list())))
  })
  lg <- gla_log(); clock <- gla_clock(); got <- new.env()
  tryCatch(run_gitlab_contents(f$io, out, roster_path, now = clock$now, sleep = clock$sleep, log = lg$log),
           error = function(e) got$err <- conditionMessage(e))
  list(err = got$err, files = list.files(out), log = lg$lines())
}

test_that("the GitLab read stops before writing when a repository known to credit a tool shows no credit", {
  expect_true(startsWith(GITLAB_MESSAGE_CANARY, "gitlab.com/"))
  none <- gla_canary_run(credits = FALSE)
  expect_match(none$err, GITLAB_MESSAGE_CANARY, fixed = TRUE)
  expect_match(none$err, "no commit crediting a tool", fixed = TRUE)
  expect_length(none$files, 0L)
  ok <- gla_canary_run(credits = TRUE)
  expect_null(ok$err)
  expect_setequal(ok$files, c("vcs-ai-cheap-gitlab.db", "vcs-dev-tooling-gitlab.db"))
  expect_true(any(grepl("GitLab message check: passed", ok$log, fixed = TRUE)))
})

test_that("the message check is skipped with a warning, not failed, when its repository is not on the roster or its host has no search", {
  skipped <- function(log, why) grep(paste0("^::warning::GitLab message check: .*", why, ", skipped$"), log, value = TRUE)
  off <- gla_canary_run(credits = FALSE, on_roster = FALSE)
  expect_null(off$err); expect_length(off$files, 2L)
  expect_length(skipped(off$log, "is not on the roster"), 1L)
  bare <- gla_canary_run(credits = FALSE, unsupported = TRUE)
  expect_null(bare$err); expect_length(bare$files, 2L)
  expect_length(skipped(bare$log, "without the message search"), 1L)
  roster <- data.frame(repo_id = GITLAB_MESSAGE_CANARY, node_id = "gitlab:gitlab.com:85027327", stringsAsFactors = FALSE)
  lg <- gla_log()
  expect_null(gitlab_message_canary(roster, list(reads = list(), unsupported = character(0)), lg$log))
  expect_length(skipped(lg$lines(), "was not read this run"), 1L)
  expect_false(any(grepl("::warning::", gla_canary_run(credits = TRUE)$log, fixed = TRUE)))
})

# Until the calling test ends, the .gitlab-ci.yml classifier throws on a file holding "boom", and on one
# holding "quote" with an error that quotes an address.
gla_ci_throws <- function(env = parent.frame()) {
  real <- gitlab_ci_columns
  assign("gitlab_ci_columns", function(root_entries, repo) {
    txt <- repo$gitlab_ci$text %||% ""
    if (grepl("boom", txt, fixed = TRUE)) stop("the CI file could not be read\nat line 2")
    if (grepl("quote", txt, fixed = TRUE)) stop("cannot read 'Maintainer: A B <a.b@example.org>' at a.b@example.org")
    real(root_entries, repo)
  }, envir = globalenv())
  withr::defer(assign("gitlab_ci_columns", real, envir = globalenv()), envir = env)
}
# A contents answer whose .gitlab-ci.yml says `ci[[node]]` for the projects named there, else runs a check;
# every message search is empty.
gla_ci_world <- function(ci) fake_gitlab(function(domain, query, variables) {
  gids <- unlist(variables$ids)
  if (!is.null(variables$re)) return(gl_ok(lapply(gids, function(g) gla_commits(g, list()))))
  said <- stats::setNames(unname(ci), .gl_gid(names(ci)))
  gl_ok(lapply(gids, function(g) {
    n <- gla_node(g, root = c("CLAUDE.md", ".gitlab-ci.yml"))
    n$repository$blobs$nodes <- c(n$repository$blobs$nodes, list(list(path = ".gitlab-ci.yml",
      rawTextBlob = if (g %in% names(said)) paste("script:", said[[g]]) else "script: R CMD check")))
    n
  }))
})

test_that("a repository whose CI file makes the classifier throw becomes a failure row, and the rest are written", {
  gla_ci_throws()
  dir <- withr::local_tempdir(); out <- file.path(dir, "out")
  ids <- c("gitlab.com/o/a", "gitlab.com/o/bad", "gitlab.com/o/quoted")
  roster_path <- gla_roster(dir, ids, sprintf("gitlab:gitlab.com:%d", 1:3))
  lg <- gla_log(); clock <- gla_clock()
  world <- gla_ci_world(c("gitlab:gitlab.com:2" = "boom", "gitlab:gitlab.com:3" = "quote"))
  expect_no_error(run_gitlab_contents(world$io, out, roster_path, now = clock$now, sleep = clock$sleep, log = lg$log))
  fails <- read_scan_failures(file.path(out, "vcs-dev-tooling-gitlab.db"))
  expect_identical(fails[c("repo_id", "query", "error")], data.frame(repo_id = ids[2:3], query = "gitlab-contents",
    error = c("the CI file could not be read", "cannot read '...' at <address>"), stringsAsFactors = FALSE))
  dev <- read_dev_tooling(file.path(out, "vcs-dev-tooling-gitlab.db"))
  expect_identical(dev$repo_id, ids[1]); expect_identical(dev$gitlab_ci_rcmdcheck, 1L)
  cheap <- file.path(out, "vcs-ai-cheap-gitlab.db")
  expect_identical(read_flagged(cheap)$flagged$repo_id, ids[1])
  for (t in c("evidence", "search_log"))
    expect_identical(unique(.ai_part_table(cheap, t)$repo_id), ids[1], info = t)
  expect_true(any(grepl("gitlab contents: gitlab.com/o/bad not processed: the CI file could not be read", lg$lines(), fixed = TRUE)))
  expect_true(any(grepl("2 repositories failed a read (contents 2, messages 0)", lg$lines(), fixed = TRUE)))
  expect_false(any(grepl(gla_email, lg$lines())))
  gla_expect_no_email(out)
})

test_that("the contents check counts a repository that could not be processed as not come back", {
  gla_ci_throws()
  dir <- withr::local_tempdir(); out <- file.path(dir, "out")
  n <- GITLAB_CANARY_MIN_ASKED
  nodes <- sprintf("gitlab:gitlab.com:%d", seq_len(n))
  roster_path <- gla_roster(dir, sprintf("gitlab.com/o/p%02d", seq_len(n)), nodes)
  clock <- gla_clock()
  boom <- function(at) stats::setNames(rep("boom", length(at)), at)
  # Every project comes back, and all but the first throw.
  expect_error(run_gitlab_contents(gla_ci_world(boom(nodes[-1]))$io, out, roster_path, now = clock$now,
                                   sleep = clock$sleep, log = function(...) NULL),
               sprintf("1 of %d projects asked came back", n), fixed = TRUE)
  expect_length(list.files(out), 0L)
  # Half of them throwing is still a run.
  expect_no_error(run_gitlab_contents(gla_ci_world(boom(nodes[seq_len(n / 2)]))$io, out, roster_path, now = clock$now,
                                      sleep = clock$sleep, log = function(...) NULL))
  expect_length(read_dev_tooling(file.path(out, "vcs-dev-tooling-gitlab.db"))$repo_id, n / 2)
})

test_that("a missing or empty GitLab roster logs a line and writes nothing", {
  dir <- withr::local_tempdir(); out <- file.path(dir, "out")
  f <- fake_gitlab(function(...) stop("no request may be sent"))
  lg <- gla_log()
  run_gitlab_contents(f$io, out, file.path(dir, "absent.db"), log = lg$log)
  empty <- gla_roster(dir, character(0), character(0))
  run_gitlab_contents(f$io, out, empty, log = lg$log)
  expect_length(list.files(out), 0L)
  expect_length(grep("no GitLab roster", lg$lines()), 2L)
})

test_that("main runs the GitLab read over the roster VCS_ROSTER names", {
  dir <- withr::local_tempdir()
  gla_roster(dir, "gitlab.com/o/a", "gitlab:gitlab.com:1")
  withr::local_envvar(VCS_ROSTER = dir)
  f <- fake_gitlab(function(domain, query, variables) {
    g <- unlist(variables$ids)
    if (!is.null(variables$re)) gl_ok(list(gla_commits(g, list()))) else gl_ok(list(gla_node(g)))
  })
  out <- file.path(dir, "out")
  suppressMessages(main("gitlab-contents", out, io = f$io))
  expect_setequal(list.files(out), c("vcs-ai-cheap-gitlab.db", "vcs-dev-tooling-gitlab.db"))
  expect_identical(read_dev_tooling(file.path(out, "vcs-dev-tooling-gitlab.db"))$repo_id, "gitlab.com/o/a")
})

test_that("the GitLab read sends no token, even when the environment holds one", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = "not-a-real-token")
  dir <- withr::local_tempdir()
  roster_path <- gla_roster(dir, "gitlab.com/o/a", "gitlab:gitlab.com:1")
  seen <- new.env(); seen$sent <- character(0); seen$args <- character(0)
  gid <- .gl_gid("gitlab:gitlab.com:1")
  # A fake curl: notes whether a header file (where a token would go) was passed, and answers.
  run <- function(cmd, args, stdout = TRUE, ...) {
    plain <- gsub("^'|'$", "", args)
    seen$args <- c(seen$args, plain)
    seen$sent <- c(seen$sent, if (any(grepl("^@", plain[which(plain == "-H") + 1L]))) "token" else "anon")
    q <- paste(readLines(sub("^@", "", plain[match("--data-binary", plain) + 1L]), warn = FALSE), collapse = "\n")
    node <- if (grepl("UntrustedRegexp", q, fixed = TRUE)) gla_commits(gid, list()) else gla_node(gid)
    writeLines(c("HTTP/2 200", ""), plain[match("-D", plain) + 1L])
    writeLines(as.character(jsonlite::toJSON(list(data = list(projects = list(nodes = list(node)))),
                                             auto_unbox = TRUE, null = "null")),
               plain[match("-o", plain) + 1L], sep = "")
    "200"
  }
  lg <- gla_log(); clock <- gla_clock()
  run_gitlab_contents(gitlab_contents_io(run = run, log = lg$log), file.path(dir, "out"), roster_path,
                      now = clock$now, sleep = clock$sleep, log = lg$log)
  expect_identical(seen$sent, c("anon", "anon"))
  expect_identical(read_dev_tooling(file.path(dir, "out", "vcs-dev-tooling-gitlab.db"))$repo_id, "gitlab.com/o/a")
  expect_false(any(grepl("not-a-real-token", c(seen$args, lg$lines()), fixed = TRUE)))
})

# ---- the merge -----------------------------------------------------------------
# A release holding one GitHub and two GitLab repositories, plus what `seed(con)` writes.
gla_release <- function(seed = NULL) {
  rel <- tempfile("rel_"); dir.create(rel)
  io <- local_release_io(rel)
  w <- tempfile("w_"); dir.create(w)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(w, "w.db"))
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "INSERT INTO repos (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status) VALUES
    ('github.com/o/r','R_r','github','github.com','o','r','o/r',1,1,'2024-01-01','2026-10-11','active'),
    ('gitlab.com/o/a','gitlab:gitlab.com:1','gitlab','gitlab.com','o','a','o/a',1,1,'2024-01-01','2026-10-11','active'),
    ('git.sc.uni-leipzig.de/o/b','gitlab:git.sc.uni-leipzig.de:2','gitlab','git.sc.uni-leipzig.de','o','b','o/b',1,1,'2024-01-01','2026-10-11','active')")
  if (!is.null(seed)) seed(con)
  suppressMessages(publish(io, con, w, tag = "current", source_kind = "live", force_full = TRUE, base_generation = ""))
  DBI::dbDisconnect(con)
  io
}
gla_pub <- function(io, t, asset = "vcs-signals-summary.db") {
  chk <- tempfile("chk_"); dir.create(chk)
  io$download(asset, chk)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(chk, asset))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbReadTable(con, t)
}
gla_model <- function(repo_id, version, mode = NULL) {
  m <- data.frame(repo_id = repo_id, tool = "claude", provider = NA_character_, family = "Opus", version = version,
                  context_window = NA_character_, commits = 3L, first_seen = "2025-01-01", last_seen = "2026-09-01",
                  window_complete = 1L, stringsAsFactors = FALSE)
  if (is.null(mode)) m else cbind(m, mode = mode, read_after = NA_character_)
}
gla_logged <- function(repo_id, key, outcome, on = "2026-10-11")
  data.frame(repo_id = repo_id, rule_key = key, rule_rev = 1L, ruleset_version = AI_RULESET_VERSION, asked_on = on,
             outcome = outcome, total_count = if (outcome == "hit") 4L else 0L,
             verified = if (outcome == "hit") 1L else NA_integer_, incomplete = 0L,
             first_hit_on = if (outcome == "hit") "2026-01-02" else NA_character_, source = "search", stringsAsFactors = FALSE)
gla_dev_row <- function(repo_id, on) {
  d <- classify_dev_tooling(c("DESCRIPTION", ".lintr"), character(0))
  d$repo_id <- repo_id; d$last_scanned <- on; d$ruleset_version <- DEV_TOOLING_RULESET_VERSION
  d[c("repo_id", "last_scanned", "ruleset_version", dev_tooling_columns())]
}
gla_github_parts <- function(parts, failures = NULL) {
  today <- format(Sys.Date())
  write_cheap_partial(file.path(parts, "vcs-ai-cheap-0.db"), list(
    flagged = data.frame(repo_id = "github.com/o/r", owner = "o", name = "r", node_id = "R_r", is_fork = 0L,
                         parent = NA_character_, pr_onset_date = NA_character_, stringsAsFactors = FALSE),
    repo_reads = data.frame(repo_id = "github.com/o/r", commits_read_on = today, commits_ruleset = AI_RULESET_VERSION,
                            commits_history_complete = 1L, reached_first = 1L, stringsAsFactors = FALSE)))
  write_dev_tooling_partial(file.path(parts, "vcs-dev-tooling-0.db"), gla_dev_row("github.com/o/r", today), failures)
  export_ai_shard(file.path(parts, "vcs-ai-shard-0.db"), .ai_empty_signals())
  parts
}

test_that("a whole GitLab search replaces last week's message rows and models for that repository", {
  gl <- "gitlab.com/o/a"; gh <- "github.com/o/r"
  # Last week's tally of a was whole too, so its Opus 4.8 row vanishing is reported, not refused.
  io <- gla_release(function(con) {
    DBI::dbWriteTable(con, "vcs_ai_search_log", rbind(gla_logged(gl, "msg.cursor.made-with", "hit"),
                                                      gla_logged(gh, "msg.cursor.made-with", "hit")), append = TRUE)
    DBI::dbWriteTable(con, "vcs_ai_models", rbind(gla_model(gl, "4.8"), gla_model(gh, "4.8")), append = TRUE)
  })
  today <- format(Sys.Date())
  cm <- gl_message_commits(list(gla_commit("a1", "2026-10-15T09:00:00Z")))
  hits <- match_commit_findings(cm)
  parts <- withr::local_tempdir()
  write_cheap_partial(file.path(parts, "vcs-ai-cheap-gitlab.db"), list(
    search_log = gl_message_log_rows(hits, gl, today, complete = TRUE),
    models = read_model_rows(gl, cm, hits, "replace")))
  write_dev_tooling_partial(file.path(parts, "vcs-dev-tooling-gitlab.db"), .devtool_empty_shard())
  got <- new.env()
  msgs <- testthat::capture_messages(tryCatch(run_merge(io, tempfile("m_"), parts),
                                              error = function(e) got$err <- conditionMessage(e)))
  expect_null(got$err)
  warned <- grep("^::warning::", msgs, value = TRUE)
  expect_length(warned, 1L)
  expect_match(warned, "gitlab.com/o/a/claude Opus 4.8", fixed = TRUE)
  expect_false(grepl(gh, warned, fixed = TRUE))
  lg <- gla_pub(io, "vcs_ai_search_log")
  expect_identical(lg$outcome[lg$repo_id == gl & lg$rule_key == "msg.cursor.made-with"], "none")
  expect_identical(lg$outcome[lg$repo_id == gl & lg$rule_key == "msg.claude.coauthor"], "hit")
  m <- gla_pub(io, "vcs_ai_models")
  expect_identical(m$version[m$repo_id == gl], "5")
  expect_identical(lg$outcome[lg$repo_id == gh & lg$rule_key == "msg.cursor.made-with"], "hit")
  expect_identical(lg$asked_on[lg$repo_id == gh], "2026-10-11")
  expect_identical(m$version[m$repo_id == gh], "4.8")
})

# A summary holding the release's repositories and these model rows.
gla_models_summary <- function(models) {
  path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "INSERT INTO repos (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status) VALUES
    ('github.com/o/r','R_r','github','github.com','o','r','o/r',1,1,'2024-01-01','2026-10-11','active'),
    ('gitlab.com/o/a','gitlab:gitlab.com:1','gitlab','gitlab.com','o','a','o/a',1,1,'2024-01-01','2026-10-11','active')")
  DBI::dbWriteTable(con, "vcs_ai_models", models, append = TRUE)
  DBI::dbDisconnect(con)
  path
}

test_that("a whole GitHub model row that disappears is still refused, beside a GitLab one that is let go", {
  prev <- gla_models_summary(rbind(gla_model("github.com/o/r", "4.8"), gla_model("github.com/o/r", "5"),
                                   gla_model("gitlab.com/o/a", "4.8"), gla_model("gitlab.com/o/a", "5")))
  nxt <- gla_models_summary(rbind(gla_model("github.com/o/r", "5"), gla_model("gitlab.com/o/a", "5")))
  msgs <- testthat::capture_messages(got <- summary_regressions(prev, nxt))
  expect_identical(got, paste0("vcs_ai_models: 1 model row(s) are gone from 1 repository/tool pair(s) the outgoing ",
                               "build still scans and the published one had read to the end of: github.com/o/r/claude"))
  expect_true(any(grepl("^::warning::.*gitlab.com/o/a/claude Opus 4.8", msgs)))
})

test_that("GitLab failures never stop the merge and a failed GitLab week keeps last week's rows", {
  gl_fail <- .fetch_failed_frame(c("gitlab.com/o/a", "git.sc.uni-leipzig.de/o/b"), c("gitlab-contents", "gitlab-messages"),
                                 "Operation timed out", "2026-10-18T10:00:00Z")
  gh_fail <- .fetch_failed_frame("github.com/o/r", "activity", "Something went wrong", "2026-10-18T10:00:00Z")
  # (a) Failures on every GitLab repository and none on GitHub: no stop, and their own log line.
  io <- gla_release()
  parts <- gla_github_parts(withr::local_tempdir())
  write_dev_tooling_partial(file.path(parts, "vcs-dev-tooling-gitlab.db"), .devtool_empty_shard(), gl_fail)
  msgs <- testthat::capture_messages(expect_no_error(run_merge(io, tempfile("m_"), parts)))
  expect_true(any(grepl("2 GitLab repositories failed a read this week (contents 1, messages 1)", msgs, fixed = TRUE)))
  expect_true(any(grepl("ai merge: 0 repositories failed a read this week", msgs, fixed = TRUE)))
  # With a GitHub failure too, the stop is the one the GitHub failures alone give.
  io2 <- gla_release()
  parts2 <- gla_github_parts(withr::local_tempdir(), failures = gh_fail)
  write_dev_tooling_partial(file.path(parts2, "vcs-dev-tooling-gitlab.db"), .devtool_empty_shard(), gl_fail)
  got <- new.env()
  suppressMessages(tryCatch(run_merge(io2, tempfile("m_"), parts2), error = function(e) got$err <- conditionMessage(e)))
  expect_identical(got$err, scan_failure_stop_message(gh_fail, 1L))
  # (b) No GitLab partials at all: last week's GitLab rows stay published.
  io3 <- gla_release(function(con) {
    DBI::dbWriteTable(con, "vcs_dev_tooling", gla_dev_row("gitlab.com/o/a", "2026-10-11"), append = TRUE)
    DBI::dbExecute(con, "INSERT INTO vcs_ai_signals (repo_id,tool,first_seen_date,first_seen_censored,evidence_tiers,markers,authored,last_confirmed_date)
      VALUES ('gitlab.com/o/a','claude','2026-10-11T23:59:59Z',1,'D','CLAUDE.md',0,'2026-10-11')")
  })
  suppressMessages(run_merge(io3, tempfile("m_"), gla_github_parts(withr::local_tempdir())))
  dev <- gla_pub(io3, "vcs_dev_tooling")
  expect_setequal(dev$repo_id, c("github.com/o/r", "gitlab.com/o/a"))
  expect_identical(dev$last_scanned[dev$repo_id == "gitlab.com/o/a"], "2026-10-11")
  sig <- gla_pub(io3, "vcs_ai_signals")
  expect_identical(sig$markers[sig$repo_id == "gitlab.com/o/a"], "CLAUDE.md")
})

test_that("a campaign finishes on the GitHub repositories alone", {
  io <- gla_release()
  parts <- gla_github_parts(withr::local_tempdir())
  export_ai_shard(file.path(parts, "vcs-ai-shard-0.db"), .ai_empty_signals(),
                  extra = list(campaign = data.frame(since = "2026-10-04", stringsAsFactors = FALSE)))
  today <- format(Sys.Date())
  cm <- gl_message_commits(list(gla_commit("a1", "2026-10-15T09:00:00Z")))
  log <- gl_message_log_rows(match_commit_findings(cm), "gitlab.com/o/a", today, complete = TRUE)
  expect_false(any(startsWith(log$rule_key, "name.")))
  write_cheap_partial(file.path(parts, "vcs-ai-cheap-gitlab.db"), list(
    flagged = data.frame(repo_id = "gitlab.com/o/a", owner = "o", name = "a", node_id = "gitlab:gitlab.com:1", is_fork = 0L,
                         parent = NA_character_, pr_onset_date = NA_character_, stringsAsFactors = FALSE),
    evidence = cbind(repo_id = "gitlab.com/o/a", .ai_found("claude", "D", "CLAUDE.md", onset = paste0(today, "T23:59:59Z"))),
    search_log = log))
  msgs <- testthat::capture_messages(run_merge(io, tempfile("m_"), parts))
  st <- gla_pub(io, "pipeline_state", "vcs-signals-recent.db")
  expect_false("ai_campaign_since" %in% st$key)
  expect_true(any(grepl("the campaign since 2026-10-04 has asked every flagged repository", msgs, fixed = TRUE)))
})

test_that("after the merge a repository on a host without the message search reads as not searched, never as none", {
  dir <- withr::local_tempdir(); parts <- file.path(dir, "parts")
  roster_path <- gla_roster(dir, c("gitlab.com/o/a", "git.sc.uni-leipzig.de/o/b"),
                            c("gitlab:gitlab.com:1", "gitlab:git.sc.uni-leipzig.de:2"))
  f <- fake_gitlab(function(domain, query, variables) {
    ids <- unlist(variables$ids)
    if (!is.null(variables$re))
      return(if (domain == "git.sc.uni-leipzig.de") gla_no_search else gl_ok(lapply(ids, function(g) gla_commits(g, list()))))
    gl_ok(lapply(ids, function(g) gla_node(g, root = "CLAUDE.md")))
  })
  clock <- gla_clock()
  run_gitlab_contents(f$io, parts, roster_path, now = clock$now, sleep = clock$sleep, log = function(...) NULL)
  io <- gla_release()
  gla_github_parts(parts)
  suppressMessages(run_merge(io, tempfile("m_"), parts))
  sig <- gla_pub(io, "vcs_ai_signals")
  claude <- function(r) sig[sig$repo_id == r & sig$tool == "claude", ]
  expect_identical(claude("gitlab.com/o/a")$assisted_commits, 0L)
  expect_true(is.na(claude("git.sc.uni-leipzig.de/o/b")$assisted_commits))
  expect_false(any(gla_pub(io, "vcs_ai_search_log")$repo_id == "git.sc.uni-leipzig.de/o/b"))
  cov <- gla_pub(io, "vcs_ai_search_coverage")
  expect_identical(cov$repos_asked[cov$rule_key == "msg.claude.coauthor"], 1L)
  expect_setequal(gla_pub(io, "vcs_dev_tooling")$repo_id, c("github.com/o/r", "gitlab.com/o/a", "git.sc.uni-leipzig.de/o/b"))
})

# ---- broken GitLab partials ------------------------------------------------------
gla_sql <- function(path, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, sql)
}
# Sound GitLab partials of each kind for gitlab.com/o/a.
gla_good <- list(
  cheap = function(path) write_cheap_partial(path, list(
    flagged = data.frame(repo_id = "gitlab.com/o/a", owner = "o", name = "a", node_id = "gitlab:gitlab.com:1", is_fork = 0L,
                         parent = NA_character_, pr_onset_date = NA_character_, stringsAsFactors = FALSE),
    evidence = cbind(repo_id = "gitlab.com/o/a", .ai_found("claude", "D", "CLAUDE.md",
                                                           onset = paste0(format(Sys.Date()), "T23:59:59Z"))))),
  dev = function(path) write_dev_tooling_partial(path, gla_dev_row("gitlab.com/o/a", format(Sys.Date()))),
  shard = function(path) export_ai_shard(path, .ai_bind_like(.ai_empty_signals(), list(data.frame(
    repo_id = "gitlab.com/o/a", tool = "claude", first_seen_date = "2026-01-02T10:00:00Z", first_seen_censored = 0L,
    evidence_tiers = "D", markers = "CLAUDE.md", authored = 0L, last_confirmed_date = format(Sys.Date()),
    stringsAsFactors = FALSE)))))
gla_gl_file <- c(cheap = "vcs-ai-cheap-gitlab.db", dev = "vcs-dev-tooling-gitlab.db", shard = "vcs-ai-shard-gitlab.db")
# Ways a GitLab partial comes out broken: not a database, cut short, a key left NULL, text where a number belongs.
gla_broken <- list(
  garbage = function(path, kind) writeLines("not a database at all", path),
  truncated = function(path, kind) {
    gla_good[[kind]](path)
    b <- readBin(path, "raw", file.size(path))
    writeBin(b[seq_len(length(b) %/% 2)], path)
  },
  `NULL node_id` = function(path, kind) { gla_good$cheap(path); gla_sql(path, "UPDATE flagged SET node_id = NULL") },
  `NULL rule_key` = function(path, kind) {
    gla_good$cheap(path)
    gla_sql(path, paste("INSERT INTO search_log (repo_id, rule_key, rule_rev, ruleset_version, asked_on, outcome,",
                        "total_count, source, mode) VALUES ('gitlab.com/o/a', NULL, 1, '2026-09-27', '2026-10-18', 'hit',",
                        "1, 'search', 'replace')"))
  },
  `text onset_censored` = function(path, kind) { gla_good$cheap(path); gla_sql(path, "UPDATE evidence SET onset_censored = 'x'") })
# GitHub parts, with each GitLab partial sound except `broken`, which `case` breaks.
gla_mixed_parts <- function(broken = NULL, case = NULL) {
  parts <- gla_github_parts(withr::local_tempdir(.local_envir = parent.frame()))
  for (kind in names(gla_gl_file)) {
    path <- file.path(parts, gla_gl_file[[kind]])
    if (identical(kind, broken)) gla_broken[[case]](path, kind) else gla_good[[kind]](path)
  }
  parts
}
gla_gate <- function(parts) {
  out <- tempfile("gate_")
  msgs <- testthat::capture_messages(run_gate(list(download = function(pattern, dir) FALSE,
                                                   release_exists = function() FALSE), out, parts, full = FALSE))
  list(flagged = read_flagged(file.path(out, "vcs-ai-flagged-roster.db"))$flagged$repo_id, msgs = msgs)
}
gla_merge <- function(parts, io = gla_release()) {
  got <- new.env()
  utils::capture.output(msgs <- testthat::capture_messages(tryCatch(run_merge(io, tempfile("m_"), parts),
    error = function(e) got$err <- conditionMessage(e))))
  list(io = io, err = got$err, msgs = msgs)
}
gla_warned <- function(msgs, file) grep(sprintf("^::warning::%s left out: ", file), msgs, value = TRUE)

test_that("the gate leaves out a broken GitLab partial with a warning and still plans the GitHub repositories", {
  sound <- gla_gate(gla_mixed_parts())
  expect_setequal(sound$flagged, c("github.com/o/r", "gitlab.com/o/a"))
  expect_false(any(grepl("::warning::", sound$msgs, fixed = TRUE)))
  for (case in c("garbage", "truncated", "NULL node_id", "text onset_censored")) {
    run <- gla_gate(gla_mixed_parts("cheap", case))
    expect_identical(run$flagged, "github.com/o/r", info = case)
    expect_length(gla_warned(run$msgs, "vcs-ai-cheap-gitlab.db"), 1L)
  }
  # A GitLab table the gate does not fold does not cost it the partial.
  expect_setequal(gla_gate(gla_mixed_parts("cheap", "NULL rule_key"))$flagged, c("github.com/o/r", "gitlab.com/o/a"))
})

test_that("the merge leaves out a broken GitLab partial with a warning and still publishes the GitHub rows", {
  sound <- gla_merge(gla_mixed_parts())
  expect_null(sound$err)
  expect_false(any(grepl("left out:", sound$msgs, fixed = TRUE)))
  expect_setequal(gla_pub(sound$io, "vcs_dev_tooling")$repo_id, c("github.com/o/r", "gitlab.com/o/a"))
  expect_true("gitlab.com/o/a" %in% gla_pub(sound$io, "vcs_ai_signals")$repo_id)
  cases <- list(c("cheap", "garbage"), c("cheap", "truncated"), c("cheap", "NULL node_id"), c("cheap", "NULL rule_key"),
                c("cheap", "text onset_censored"), c("dev", "garbage"), c("dev", "truncated"), c("shard", "garbage"),
                c("shard", "truncated"))
  for (k in cases) {
    run <- gla_merge(gla_mixed_parts(k[1], k[2]))
    info <- paste(k, collapse = " ")
    expect_null(run$err, info = info)
    expect_length(gla_warned(run$msgs, gla_gl_file[[k[1]]]), 1L)
    expect_identical(gla_pub(run$io, "vcs_ai_repo_reads")$repo_id, "github.com/o/r", info = info)
    dev <- gla_pub(run$io, "vcs_dev_tooling")$repo_id
    expect_true("github.com/o/r" %in% dev, info = info)
    expect_identical("gitlab.com/o/a" %in% dev, k[1] != "dev", info = info)
  }
})

test_that("the merge log says which GitLab partials it folded", {
  folded <- function(run) grep("GitLab partials folded: ", run$msgs, value = TRUE)
  expect_match(folded(gla_merge(gla_mixed_parts())), "GitLab partials folded: cheap yes, dev tooling yes, dating yes",
               fixed = TRUE)
  expect_match(folded(gla_merge(gla_github_parts(withr::local_tempdir()))),
               "GitLab partials folded: cheap no, dev tooling no, dating no", fixed = TRUE)
  expect_match(folded(gla_merge(gla_mixed_parts("cheap", "garbage"))),
               "GitLab partials folded: cheap no, dev tooling yes, dating yes", fixed = TRUE)
  expect_match(folded(gla_merge(gla_mixed_parts("shard", "garbage"))),
               "GitLab partials folded: cheap yes, dev tooling yes, dating no", fixed = TRUE)
})

# ---- a capped message search -------------------------------------------------------
# n commits crediting Claude, an hour apart from 2026-01-24 12:00 UTC.
gla_credits <- function(n) gl_message_commits(lapply(seq_len(n), function(i) list(sha = sprintf("c%04d", i),
  committedDate = format(as.POSIXct("2026-01-24 12:00:00", tz = "UTC") + i * 3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  message = gla_credit)))

test_that("a capped GitLab search keeps the whole search's first date and the larger count", {
  gl <- "gitlab.com/o/a"; key <- "msg.claude.coauthor"
  # Last week a whole search read 990 credits from 2026-01-24.
  whole <- gl_message_log_rows(match_commit_findings(gla_credits(990)), gl, "2026-10-25", complete = TRUE)
  io <- gla_release(function(con) DBI::dbWriteTable(con, "vcs_ai_search_log", whole[names(.ai_empty_log())], append = TRUE))
  # This week the page cap stops the search at the newest 1,000 of 1,040.
  newest <- gla_credits(1040)[1040:41, ]
  capped <- gl_message_log_rows(match_commit_findings(newest), gl, "2026-11-01", complete = FALSE)
  parts <- gla_github_parts(withr::local_tempdir())
  write_cheap_partial(file.path(parts, "vcs-ai-cheap-gitlab.db"), list(search_log = capped))
  expect_null(gla_merge(parts, io)$err)
  lg <- gla_pub(io, "vcs_ai_search_log")
  row <- lg[lg$repo_id == gl & lg$rule_key == key, c("outcome", "total_count", "verified", "incomplete", "first_hit_on",
                                                       "source", "asked_on")]
  rownames(row) <- NULL
  expect_identical(row, data.frame(outcome = "hit", total_count = 1000L, verified = 1L, incomplete = 1L,
    first_hit_on = "2026-01-24", source = "search", asked_on = "2026-11-01", stringsAsFactors = FALSE))
  # A smaller capped count keeps the larger one.
  low <- capped[capped$rule_key == key, ]; low$total_count <- 500L
  again <- fold_search_log(lg, low, rebuilt_repos = character(0), gitlab_repos = gl)
  got <- again[again$repo_id == gl & again$rule_key == key, ]
  expect_identical(list(got$total_count, got$first_hit_on), list(1000L, "2026-01-24"))
  # A cut-off search outside GitLab replaces its row as before.
  other <- fold_search_log(lg, low, rebuilt_repos = character(0))
  got <- other[other$repo_id == gl & other$rule_key == key, ]
  expect_identical(list(got$total_count, got$first_hit_on), list(500L, NA_character_))
})

# ---- dating GitLab findings over REST ---------------------------------------------
# A flagged roster of GitLab repositories with these node ids, each with CLAUDE.md, and one dating item each.
gla_flagged <- function(dir, repo_ids, node_ids) {
  path <- file.path(dir, "vcs-ai-flagged-roster.db")
  write_flagged_partial(path,
    data.frame(repo_id = repo_ids, owner = "o", name = sub("^.*/", "", repo_ids), node_id = node_ids, is_fork = 0L,
               parent = NA_character_, pr_onset_date = NA_character_, stringsAsFactors = FALSE),
    cbind(repo_id = repo_ids, .ai_found("claude", "D", "CLAUDE.md")),
    work = data.frame(repo_id = repo_ids, tool = "claude", rule_key = NA_character_, reason = "onset", priority = 1L,
                      stringsAsFactors = FALSE))
  path
}
gla_onset_rows <- function(out) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-ai-shard-gitlab.db"))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbReadTable(con, "vcs_ai_signals")
}

test_that("GitLab dating writes exact file dates, keeps commit-credit dates, and censors a fork", {
  dir <- withr::local_tempdir(); parts <- file.path(dir, "parts"); gate <- file.path(dir, "gate")
  ids <- c(a = "gitlab.com/o/a", b = "code.usgs.gov/o/b", f = "gitlab.com/o/f")
  nodes <- c(a = "gitlab:gitlab.com:1", b = "gitlab:code.usgs.gov:2", f = "gitlab:gitlab.com:3")
  gid <- stats::setNames(.gl_gid(nodes), names(nodes))
  roster_path <- gla_roster(dir, unname(ids), unname(nodes))
  # a: CLAUDE.md and a commit crediting Cursor; b: a .claude line in .gitignore; f: a fork with CLAUDE.md.
  g <- fake_gitlab(function(domain, query, variables) {
    gids <- unlist(variables$ids)
    if (!is.null(variables$re)) return(gl_ok(lapply(gids, function(x) gla_commits(x, if (x == gid[["a"]])
      list(list(sha = "a1", committedDate = "2026-03-02T09:00:00Z", message = "x\n\nMade-with: Cursor")) else list()))))
    gl_ok(lapply(gids, function(x) {
      if (x == gid[["b"]]) {
        n <- gla_node(x)
        n$repository$blobs$nodes <- c(n$repository$blobs$nodes, list(list(path = ".gitignore", rawTextBlob = "*.o\n.claude\n")))
        return(n)
      }
      n <- gla_node(x, root = "CLAUDE.md")
      if (x == gid[["f"]]) { n$isForked <- TRUE; n$forkedFrom <- list(fullPath = "up/f") }
      n
    }))
  })
  clock <- gla_clock()
  run_gitlab_contents(g$io, parts, roster_path, now = clock$now, sleep = clock$sleep, log = function(...) NULL)
  suppressMessages(run_gate(list(download = function(pattern, dir) FALSE, release_exists = function() FALSE), gate, parts,
                            full = FALSE))
  work <- read_flagged(file.path(gate, "vcs-ai-flagged-roster.db"))$work
  expect_setequal(paste(work$repo_id, work$tool, work$reason),
                  paste(c(ids[["a"]], ids[["a"]], ids[["b"]], ids[["f"]]), c("claude", "cursor", "claude", "claude"), "onset"))
  r <- fake_gitlab(rest = function(domain, path) {
    if (grepl("/repository/files/", path, fixed = TRUE))
      return(rest_ok(if (sub("^.*ref=", "", path) %in% c("g2", "g3")) "*.o\n.claude\n" else "*.o\n"))
    if (startsWith(path, "/projects/2/"))
      return(rest_ok(commits_body(c("g3", "g2", "g1"), c("2026-05-03T00:00:00Z", "2026-05-02T00:00:00Z", "2026-05-01T00:00:00Z"))))
    if (startsWith(path, "/projects/3/")) return(rest_ok(commits_body("f1", "2025-11-30T08:00:00Z")))
    if (grepl("[?&]page=1(&|$)", path)) rest_ok(commits_body("a2", "2026-03-01T10:00:00.000+01:00"), "2")
    else rest_ok(commits_body("a1", "2026-01-24T13:59:09.000+01:00"))
  })
  out <- file.path(dir, "onsets")
  run_gitlab_onsets(r$io, out, file.path(gate, "vcs-ai-flagged-roster.db"), now = clock$now, sleep = clock$sleep,
                    rest_pacer = gl_rest_pacer(clock$now, clock$sleep), log = function(...) NULL)
  sig <- gla_onset_rows(out)
  at <- function(id, tool) { s <- sig[sig$repo_id == id & sig$tool == tool, ]; list(s$first_seen_date, s$first_seen_censored) }
  expect_identical(at(ids[["a"]], "claude"), list("2026-01-24T12:59:09Z", 0L))
  expect_identical(at(ids[["a"]], "cursor"), list("2026-03-02T09:00:00Z", 0L))
  expect_identical(at(ids[["b"]], "claude"), list("2026-05-02T00:00:00Z", 0L))
  expect_identical(at(ids[["f"]], "claude"), list("2025-11-30T08:00:00Z", 1L))
  # The Cursor credit needed no request: project 1 was asked for CLAUDE.md's two pages only.
  on_a <- grep("^gitlab.com /projects/1/", r$calls$rest, value = TRUE)
  expect_length(on_a, 2L)
  expect_true(all(grepl("path=CLAUDE.md&follow=false", on_a, fixed = TRUE)))
  gla_expect_no_email(out)
})

test_that("GitLab dating waits for the next hour when the budget runs out, and gives up at the deadline", {
  ids <- sprintf("gitlab.com/o/p%d", 1:3)
  dating <- function(deadline_min) {
    dir <- tempfile("gl_onsets_"); dir.create(dir)
    path <- gla_flagged(dir, ids, sprintf("gitlab:gitlab.com:%d", 1:3))
    f <- fake_gitlab(rest = function(domain, path) rest_ok(commits_body("s1", "2026-01-24T12:59:09Z")))
    clock <- gla_clock(); lg <- gla_log()
    naps <- new.env(); naps$s <- numeric(0)
    sleep <- function(s) { naps$s <- c(naps$s, s); clock$sleep(s) }
    p <- gl_rest_pacer(clock$now, sleep); p$set_hourly_budget("gitlab.com", 2L)
    run_gitlab_onsets(f$io, file.path(dir, "out"), path, now = clock$now, sleep = sleep, rest_pacer = p,
                      deadline_min = deadline_min, log = lg$log)
    list(sig = gla_onset_rows(file.path(dir, "out")), naps = naps$s, t = clock$now(), log = lg$lines(),
         asked = length(f$calls$rest))
  }
  long <- dating(130)
  expect_setequal(long$sig$repo_id, ids)
  expect_true(all(long$sig$first_seen_date == "2026-01-24T12:59:09Z" & long$sig$first_seen_censored == 0L))
  expect_identical(sum(long$naps >= 60), 1L)
  expect_gt(long$t, 3600)
  expect_identical(long$asked, 3L)
  short <- dating(5)
  expect_setequal(short$sig$repo_id, ids[1:2])
  expect_identical(sum(short$naps >= 60), 0L)
  expect_identical(short$asked, 2L)
  expect_true(any(grepl("gitlab.com/o/p3 claude", short$log, fixed = TRUE)))
  expect_false(any(grepl("gitlab.com/o/p[12] claude", short$log)))
})

test_that("a host that answers 429 keeps its items undated, and a history GitLab cannot read leaves its finding undated", {
  dir <- withr::local_tempdir()
  ids <- c("gitlab.com/o/p1", "gitlab.com/o/p2", "code.usgs.gov/o/q1", "code.usgs.gov/o/q2")
  path <- gla_flagged(dir, ids, c("gitlab:gitlab.com:1", "gitlab:gitlab.com:2", "gitlab:code.usgs.gov:3",
                                  "gitlab:code.usgs.gov:4"))
  f <- fake_gitlab(rest = function(domain, path) {
    if (domain == "gitlab.com") return(list(status = 429L, error = "HTTP 429", headers = character(), body = ""))
    if (startsWith(path, "/projects/4/")) return(list(status = 502L, error = "HTTP 502", headers = character(), body = ""))
    rest_ok(commits_body("s1", "2026-01-24T12:59:09Z"))
  })
  clock <- gla_clock(); lg <- gla_log()
  run_gitlab_onsets(f$io, file.path(dir, "out"), path, now = clock$now, sleep = clock$sleep,
                    rest_pacer = gl_rest_pacer(clock$now, clock$sleep), log = lg$log)
  sig <- gla_onset_rows(file.path(dir, "out"))
  expect_setequal(sig$repo_id, c("code.usgs.gov/o/q1", "code.usgs.gov/o/q2"))
  expect_identical(sig$first_seen_date[sig$repo_id == "code.usgs.gov/o/q1"], "2026-01-24T12:59:09Z")
  # The finding stays, with the date the read gave it (none here), never one made up.
  q2 <- sig[sig$repo_id == "code.usgs.gov/o/q2", ]
  expect_true(is.na(q2$first_seen_date)); expect_identical(q2$markers, "CLAUDE.md")
  expect_identical(sum(startsWith(f$calls$rest, "gitlab.com ")), 1L)
  expect_true(any(grepl("gitlab.com/o/p1 claude, gitlab.com/o/p2 claude", lg$lines(), fixed = TRUE)))
})

test_that("main runs the GitLab dating over the flagged roster VCS_FLAGGED names", {
  dir <- withr::local_tempdir()
  gla_flagged(dir, "gitlab.com/o/a", "gitlab:gitlab.com:1")
  withr::local_envvar(VCS_FLAGGED = dir)
  f <- fake_gitlab(rest = function(domain, path) rest_ok(commits_body("s1", "2026-01-24T12:59:09Z")))
  out <- file.path(dir, "out")
  suppressMessages(main("gitlab-onsets", out, io = f$io))
  sig <- gla_onset_rows(out)
  expect_identical(list(sig$repo_id, sig$first_seen_date), list("gitlab.com/o/a", "2026-01-24T12:59:09Z"))
})

test_that("the GitLab dating sends the gitlab.com token in a header file, and a 401 drops it and lowers the budget", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = "not-a-real-token")
  clock <- gla_clock(); lg <- gla_log()
  p <- gl_rest_pacer(clock$now, clock$sleep)
  seen <- new.env(); seen$args <- character(0); seen$sent <- character(0)
  # A fake curl: 401 to a request carrying a header file, one page of commits to one without.
  run <- function(cmd, args, stdout = TRUE, ...) {
    plain <- gsub("^'|'$", "", args)
    seen$args <- c(seen$args, plain)
    tok <- any(grepl("^@", plain[which(plain == "-H") + 1L]))
    seen$sent <- c(seen$sent, if (tok) "token" else "anon")
    status <- if (tok) 401L else 200L
    writeLines(c(sprintf("HTTP/2 %d", status), "x-next-page: ", ""), plain[match("-D", plain) + 1L])
    writeLines(if (tok) "{}" else commits_body("s1", "2026-01-24T12:59:09Z"), plain[match("-o", plain) + 1L], sep = "")
    as.character(status)
  }
  expect_identical(p$hourly_budget("gitlab.com"), GITLAB_REST_HOURLY_TOKEN)
  got <- gl_file_onset(gitlab_onsets_io(p, run = run, log = lg$log), "gitlab.com", 1, "CLAUDE.md", p)
  expect_identical(got, list(date = "2026-01-24T12:59:09Z", stopped = FALSE))
  expect_identical(seen$sent, c("token", "anon"))
  expect_identical(p$hourly_budget("gitlab.com"), GITLAB_REST_HOURLY[["gitlab.com"]])
  expect_false(any(grepl("not-a-real-token", c(seen$args, lg$lines()), fixed = TRUE)))
})

test_that("the script on its own loads every helper the GitLab dating calls", {
  probe <- paste0("suppressMessages(source('scripts/ai_backfill.R')); ",
                  "cat(all(vapply(c('.gl_rest_block', '.gl_redact', 'gl_file_onset', 'gl_ignore_line_onset', ",
                  "'gl_rest_pacer', 'run_gitlab_onsets'), exists, NA)))")
  got <- withr::with_dir(.repo_root, system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(probe)),
                                             stdout = TRUE, stderr = FALSE))
  expect_identical(utils::tail(got, 1L), "TRUE")
})

test_that("an item the hour's budget cuts off part way goes to the back of its queue and is read again from its first page", {
  dir <- withr::local_tempdir()
  ids <- c("gitlab.com/o/p1", "gitlab.com/o/p2", "gitlab.com/o/p3")
  path <- gla_flagged(dir, ids, sprintf("gitlab:gitlab.com:%d", 1:3))
  # p1 and p2 take two pages each, p3 one.
  f <- fake_gitlab(rest = function(domain, path) {
    if (startsWith(path, "/projects/3/")) return(rest_ok(commits_body("c1", "2026-03-03T00:00:00Z")))
    if (grepl("[?&]page=1(&|$)", path)) rest_ok(commits_body("s2", "2026-02-01T00:00:00Z"), "2")
    else rest_ok(commits_body("s1", "2026-01-24T12:59:09Z"))
  })
  clock <- gla_clock(); lg <- gla_log()
  p <- gl_rest_pacer(clock$now, clock$sleep); p$set_hourly_budget("gitlab.com", 3L)
  run_gitlab_onsets(f$io, file.path(dir, "out"), path, now = clock$now, sleep = clock$sleep, rest_pacer = p, log = lg$log)
  sig <- gla_onset_rows(file.path(dir, "out"))
  expect_identical(sig$first_seen_date[match(ids, sig$repo_id)],
                   c("2026-01-24T12:59:09Z", "2026-01-24T12:59:09Z", "2026-03-03T00:00:00Z"))
  # Hour one: p1's two pages and p2's first; hour two: p3, then p2 from its first page again.
  expect_identical(sub("^gitlab.com /projects/([0-9]+)/.*page=([0-9]+)$", "\\1.\\2", f$calls$rest),
                   c("1.1", "1.2", "2.1", "3.1", "2.1", "2.2"))
  expect_true(any(grepl("gitlab.com, 3 of 3 item(s) done", lg$lines(), fixed = TRUE)))
})

test_that("an ignore line whose file GitLab cannot read keeps the floor of the day it was seen, not today", {
  dir <- withr::local_tempdir()
  # Seen nine days ago, so its floor and today's differ.
  seen <- paste0(format(Sys.Date() - 9), "T23:59:59Z")
  ids <- c("gitlab.com/o/raw", "gitlab.com/o/hist")
  path <- file.path(dir, "vcs-ai-flagged-roster.db")
  write_flagged_partial(path,
    data.frame(repo_id = ids, owner = "o", name = c("raw", "hist"), node_id = c("gitlab:gitlab.com:1", "gitlab:gitlab.com:2"),
               is_fork = 0L, parent = NA_character_, pr_onset_date = NA_character_, stringsAsFactors = FALSE),
    cbind(repo_id = ids, .ai_found("claude", "D", "gitignore:.claude", onset = seen, onset_censored = 1L)),
    work = data.frame(repo_id = ids, tool = "claude", rule_key = NA_character_, reason = "onset", priority = 1L,
                      stringsAsFactors = FALSE))
  # raw: the history reads and every revision read fails; hist: the history read fails.
  f <- fake_gitlab(rest = function(domain, path) {
    if (startsWith(path, "/projects/1/repository/commits"))
      return(rest_ok(commits_body(c("g2", "g1"), c("2026-05-02T00:00:00Z", "2026-05-01T00:00:00Z"))))
    list(status = 502L, error = "HTTP 502", headers = character(), body = "")
  })
  clock <- gla_clock()
  run_gitlab_onsets(f$io, file.path(dir, "out"), path, now = clock$now, sleep = clock$sleep,
                    rest_pacer = gl_rest_pacer(clock$now, clock$sleep), log = function(...) NULL)
  sig <- gla_onset_rows(file.path(dir, "out"))
  expect_identical(sig$first_seen_date[match(ids, sig$repo_id)], c(seen, seen))
  expect_identical(sig$first_seen_censored[match(ids, sig$repo_id)], c(1L, 1L))
  expect_true(any(grepl("/repository/files/.gitignore/raw?ref=", f$calls$rest, fixed = TRUE)))
})
