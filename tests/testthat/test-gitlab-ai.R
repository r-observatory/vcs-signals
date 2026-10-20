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

test_that("the message check is skipped, not failed, when its repository is not on the roster or its host has no search", {
  off <- gla_canary_run(credits = FALSE, on_roster = FALSE)
  expect_null(off$err); expect_length(off$files, 2L)
  expect_true(any(grepl("not on the roster, skipped", off$log, fixed = TRUE)))
  bare <- gla_canary_run(credits = FALSE, unsupported = TRUE)
  expect_null(bare$err); expect_length(bare$files, 2L)
  expect_true(any(grepl("without the message search, skipped", bare$log, fixed = TRUE)))
  roster <- data.frame(repo_id = GITLAB_MESSAGE_CANARY, node_id = "gitlab:gitlab.com:85027327", stringsAsFactors = FALSE)
  lg <- gla_log()
  expect_null(gitlab_message_canary(roster, list(reads = list(), unsupported = character(0)), lg$log))
  expect_true(any(grepl("not read this run, skipped", lg$lines(), fixed = TRUE)))
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
  # Last week's GitLab tally was cut off: the publish gate holds a whole tally's rows, on either host.
  cut <- transform(gla_model(gl, "4.8"), window_complete = 0L)
  io <- gla_release(function(con) {
    DBI::dbWriteTable(con, "vcs_ai_search_log", rbind(gla_logged(gl, "msg.cursor.made-with", "hit"),
                                                      gla_logged(gh, "msg.cursor.made-with", "hit")), append = TRUE)
    DBI::dbWriteTable(con, "vcs_ai_models", rbind(cut, gla_model(gh, "4.8")), append = TRUE)
  })
  today <- format(Sys.Date())
  cm <- gl_message_commits(list(gla_commit("a1", "2026-10-15T09:00:00Z")))
  hits <- match_commit_findings(cm)
  parts <- withr::local_tempdir()
  write_cheap_partial(file.path(parts, "vcs-ai-cheap-gitlab.db"), list(
    search_log = gl_message_log_rows(hits, gl, today, complete = TRUE),
    models = read_model_rows(gl, cm, hits, "replace")))
  write_dev_tooling_partial(file.path(parts, "vcs-dev-tooling-gitlab.db"), .devtool_empty_shard())
  suppressMessages(run_merge(io, tempfile("m_"), parts))
  lg <- gla_pub(io, "vcs_ai_search_log")
  expect_identical(lg$outcome[lg$repo_id == gl & lg$rule_key == "msg.cursor.made-with"], "none")
  expect_identical(lg$outcome[lg$repo_id == gl & lg$rule_key == "msg.claude.coauthor"], "hit")
  m <- gla_pub(io, "vcs_ai_models")
  expect_identical(m$version[m$repo_id == gl], "5")
  expect_identical(lg$outcome[lg$repo_id == gh & lg$rule_key == "msg.cursor.made-with"], "hit")
  expect_identical(lg$asked_on[lg$repo_id == gh], "2026-10-11")
  expect_identical(m$version[m$repo_id == gh], "4.8")
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
