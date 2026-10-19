# The weekly GitLab job: the roster (weekly.R), and the rotation over each host's REST budget, the
# shard and the cursor state (gitlab_weekly.R). weekly.R is sourced from the repo root.
if (!exists("run_weekly_enumerate")) {
  .wkg_wd <- setwd(.repo_root)
  source(file.path(.repo_root, "scripts", "weekly.R"))
  setwd(.wkg_wd)
}

# A fake clock, 50 minutes into an hour: sleeping moves it, and every sleep is kept.
wkg_clock <- function(start = 3600 * 100 + 3000) {
  clock <- new.env()
  clock$t <- start
  clock$slept <- numeric()
  clock$now <- function() clock$t
  clock$sleep <- function(s) { clock$slept <- c(clock$slept, s); clock$t <- clock$t + s }
  clock
}

# A GitLab roster of projects <domain>/g/p<id>, written as enumerate writes it.
wkg_roster <- function(dir, ids) {
  rows <- do.call(rbind, lapply(names(ids), function(d) data.frame(
    repo_id = sprintf("%s/g/p%d", d, ids[[d]]), node_id = sprintf("gitlab:%s:%d", d, ids[[d]]),
    domain = d, project_id = as.integer(ids[[d]]), stringsAsFactors = FALSE)))
  path <- file.path(dir, "vcs-signals-roster-gitlab.db")
  write_gitlab_roster(path, rows)
  path
}

wkg_answer <- function(status = 200L, body = list(), headers = character())
  list(status = status, headers = headers, error = if (status >= 400L) sprintf("HTTP %d", status) else NA_character_,
       body = as.character(jsonlite::toJSON(body, auto_unbox = TRUE)))

# GitLab as the job asks it: project <id> has <id> contributors, a top contributor with 10 x <id>
# commits and an email in every row, 100 x <id> commits and the licence key mit. `special(domain,
# path)` answers first unless it returns NULL. The release holds the state at `state_from`, if any.
wkg_io <- function(seen = new.env(), special = function(domain, path) NULL, state_from = NULL) {
  seen$rest <- character()
  list(
    gl_graphql = function(domain, query, variables) {
      ids <- as.integer(sub("^.*/", "", unlist(variables$ids)))
      nodes <- lapply(ids, function(i) list(id = sprintf("gid://gitlab/Project/%d", i),
                                            repository = list(commitCount = 100L * i)))
      list(data = list(projects = list(nodes = nodes)), errors = NULL, status = 200L, error = NA_character_)
    },
    gl_rest = function(domain, path) {
      seen$rest <- c(seen$rest, paste0(domain, path))
      s <- special(domain, path)
      if (!is.null(s)) return(s)
      id <- as.integer(sub("^/projects/([0-9]+).*$", "\\1", path))
      row <- list(list(name = "Person", email = "person@example.invalid", commits = 10L * id))
      if (grepl("order_by=commits", path, fixed = TRUE)) wkg_answer(body = row)
      else if (grepl("/contributors", path, fixed = TRUE)) wkg_answer(body = row, headers = c(`x-total` = as.character(id)))
      else wkg_answer(body = list(id = id, license = list(key = "mit")))
    },
    download = function(pattern, dir) {
      if (is.null(state_from) || !file.exists(state_from)) return(FALSE)
      file.copy(state_from, file.path(dir, pattern), overwrite = TRUE)
    })
}

# One run on the fake clock, with `budget` setting hourly budgets by host. Returns the out dir and
# the REST pacer.
wkg_run <- function(io, roster, clock, budget = integer(), deadline_min = 200, log = function(m) NULL) {
  out <- tempfile("gl_out")
  pacer <- gl_rest_pacer(clock$now, clock$sleep)
  for (k in names(budget)) pacer$set_hourly_budget(k, budget[[k]])
  run_fetch_gitlab(io, out, roster, file.path(out, "vcs-signals-gitlab-state.db"), now = clock$now,
                   sleep = clock$sleep, rest_pacer = pacer, deadline_min = deadline_min, log = log)
  list(out = out, pacer = pacer)
}

wkg_table <- function(path, table) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbReadTable(con, table)
}
wkg_snapshot <- function(out) {
  s <- wkg_table(file.path(out, "vcs-signals-shard-gitlab.db"), "snapshot")
  s[order(s$repo_id, method = "radix"), , drop = FALSE]
}
wkg_cursor <- function(out) {
  cur <- wkg_table(file.path(out, "vcs-signals-gitlab-state.db"), "cursor")
  stats::setNames(cur$last_repo_id, cur$domain)[order(cur$domain, method = "radix")]
}
# The repositories whose contributors were read, in repo_id order.
wkg_counted <- function(out) { s <- wkg_snapshot(out); s$repo_id[!is.na(s$contributors_total)] }
wkg_ids <- function(domain, ids) sprintf("%s/g/p%d", domain, ids)

test_that("enumerate writes a GitLab roster of the active GitLab repositories with a project id", {
  src <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), src)
  ensure_series_schema(con); ensure_repo_schema(con)
  DBI::dbExecute(con, "INSERT INTO vcs_signals_summary (package, origin, repo_id, stars, forks, releases_total) VALUES
    ('data.table', 'cran', 'github.com/rdatatable/data.table', 3400, 960, 9),
    ('linelist', 'bioc', 'gitlab.com/epiverse/outbreaks/linelist', 12, 0, 0)")
  repo <- function(id, node, host, domain, status = "active") sprintf(
    "('%s', %s, '%s', '%s', 'o', 'n', 'o/n', 1, 1, '2026-01-01', '2026-10-01', '%s')",
    id, if (is.na(node)) "NULL" else sprintf("'%s'", node), host, domain, status)
  DBI::dbExecute(con, paste("INSERT INTO repos VALUES", paste(c(
    repo("github.com/rdatatable/data.table", "R_dt", "github", "github.com"),
    repo("gitlab.com/epiverse/outbreaks/linelist", "gitlab:gitlab.com:46520077", "gitlab", "gitlab.com"),
    repo("code.usgs.gov/water/toxics/usgspkg", "gitlab:code.usgs.gov:4711", "gitlab", "code.usgs.gov"),
    repo("gitlab.com/someone/retired", "gitlab:gitlab.com:99", "gitlab", "gitlab.com", "gone"),
    repo("gitlab.com/someone/unresolved", NA, "gitlab", "gitlab.com"),
    repo("codeberg.org/someone/cbpkg", NA, "codeberg", "codeberg.org")), collapse = ",\n")))
  DBI::dbDisconnect(con)
  io <- list(download = function(name, dir) file.copy(src, file.path(dir, name), overwrite = TRUE))
  out <- withr::local_tempdir()
  suppressMessages(run_weekly_enumerate(io, out))
  gl <- load_roster(file.path(out, "vcs-signals-roster-gitlab.db"))
  expect_identical(names(gl), c("repo_id", "node_id", "domain", "project_id"))
  expect_identical(gl$repo_id, c("code.usgs.gov/water/toxics/usgspkg", "gitlab.com/epiverse/outbreaks/linelist"))
  expect_identical(gl$node_id, c("gitlab:code.usgs.gov:4711", "gitlab:gitlab.com:46520077"))
  expect_identical(gl$domain, c("code.usgs.gov", "gitlab.com"))
  expect_identical(gl$project_id, c(4711L, 46520077L))
  # The GitHub roster is unchanged.
  expect_identical(load_roster(file.path(out, "vcs-signals-roster.db"))$repo_id, "github.com/rdatatable/data.table")
})

test_that("a budget of 9 requests and no time for the next hour reads 3 repositories, and the next run the next 3", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = NA)
  dir <- withr::local_tempdir()
  roster <- wkg_roster(dir, list(gitlab.com = 1:7))
  rel <- file.path(dir, "release-state.db")
  seen <- new.env()
  clock <- wkg_clock()
  r1 <- wkg_run(wkg_io(seen, state_from = rel), roster, clock, budget = c(gitlab.com = 9L), deadline_min = 5)
  expect_length(seen$rest, 9L)
  expect_true(all(clock$slept < 60))
  expect_identical(wkg_counted(r1$out), wkg_ids("gitlab.com", 1:3))
  # Commit totals come over GraphQL for every row.
  expect_identical(wkg_snapshot(r1$out)$commits_total, 100L * 1:7)
  expect_identical(wkg_cursor(r1$out), c(gitlab.com = "gitlab.com/g/p3"))

  # The merge publishes the state and the next run downloads it.
  file.copy(file.path(r1$out, "vcs-signals-gitlab-state.db"), rel, overwrite = TRUE)
  r2 <- wkg_run(wkg_io(seen, state_from = rel), roster, clock, budget = c(gitlab.com = 9L), deadline_min = 5)
  expect_identical(wkg_counted(r2$out), wkg_ids("gitlab.com", 4:6))
  expect_identical(wkg_cursor(r2$out), c(gitlab.com = "gitlab.com/g/p6"))

  # The one after wraps to the start once the last repository is read.
  file.copy(file.path(r2$out, "vcs-signals-gitlab-state.db"), rel, overwrite = TRUE)
  r3 <- wkg_run(wkg_io(seen, state_from = rel), roster, clock, budget = c(gitlab.com = 9L), deadline_min = 5)
  expect_identical(wkg_counted(r3$out), wkg_ids("gitlab.com", c(1:2, 7)))
  expect_identical(wkg_cursor(r3$out), c(gitlab.com = "gitlab.com/g/p2"))
})

test_that("a run whose budget is spent with deadline left sleeps to the next UTC hour and goes on", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = NA)
  roster <- wkg_roster(withr::local_tempdir(), list(gitlab.com = 1:5))
  seen <- new.env()
  clock <- wkg_clock()
  r <- wkg_run(wkg_io(seen), roster, clock, budget = c(gitlab.com = 9L))
  expect_true(any(clock$slept > 500))
  expect_equal(floor(clock$t / 3600), 101)
  expect_length(seen$rest, 15L)
  expect_false(anyDuplicated(seen$rest) > 0)
  expect_identical(wkg_counted(r$out), wkg_ids("gitlab.com", 1:5))
  expect_identical(wkg_cursor(r$out), c(gitlab.com = "gitlab.com/g/p5"))
})

test_that("a host that throws does not stop another host, and the shard and state are still written", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = NA)
  roster <- wkg_roster(withr::local_tempdir(), list(code.usgs.gov = 1:2, gitlab.com = 1:2))
  logs <- new.env(); logs$lines <- character()
  # A reply that is not a list makes the collector itself fail.
  io <- wkg_io(special = function(domain, path) if (domain == "code.usgs.gov") 1)
  r <- wkg_run(io, roster, wkg_clock(), log = function(m) logs$lines <- c(logs$lines, m))
  expect_true(file.exists(file.path(r$out, "vcs-signals-shard-gitlab.db")))
  expect_identical(wkg_counted(r$out), wkg_ids("gitlab.com", 1:2))
  expect_identical(wkg_cursor(r$out), c(gitlab.com = "gitlab.com/g/p2"))
  expect_true(any(grepl("code.usgs.gov", logs$lines, fixed = TRUE) & grepl("fault", logs$lines, fixed = TRUE)))
})

test_that("a cursor whose repository left the roster restarts at the next one by order", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = NA)
  dir <- withr::local_tempdir()
  roster <- wkg_roster(dir, list(gitlab.com = c(1L, 2L, 4L, 5L)))
  rel <- file.path(dir, "release-state.db")
  write_gitlab_cursors(rel, c(gitlab.com = "gitlab.com/g/p3"))
  seen <- new.env()
  r <- wkg_run(wkg_io(seen, state_from = rel), roster, wkg_clock(), budget = c(gitlab.com = 3L), deadline_min = 5)
  expect_match(seen$rest[1], "^gitlab.com/projects/4/")
  expect_identical(wkg_counted(r$out), wkg_ids("gitlab.com", 4))
  expect_identical(wkg_cursor(r$out), c(gitlab.com = "gitlab.com/g/p4"))
})

test_that("a 429 closes the host for the run and leaves the cursor at the last repository read", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = NA)
  roster <- wkg_roster(withr::local_tempdir(), list(code.usgs.gov = 1L, gitlab.com = 1:5))
  seen <- new.env()
  clock <- wkg_clock()
  io <- wkg_io(seen, special = function(domain, path)
    if (domain == "gitlab.com" && path == "/projects/3?license=true") wkg_answer(429L))
  r <- wkg_run(io, roster, clock)
  # Nothing more is asked of gitlab.com, and the run does not wait for the next hour.
  expect_identical(utils::tail(seen$rest, 1L), "gitlab.com/projects/3?license=true")
  expect_true(all(clock$slept < 60))
  expect_identical(r$pacer$deferred(), character())
  expect_identical(wkg_cursor(r$out), c(code.usgs.gov = "code.usgs.gov/g/p1", gitlab.com = "gitlab.com/g/p2"))
  # The contributors read before the 429 are kept; its licence was not read.
  expect_identical(wkg_counted(r$out), c("code.usgs.gov/g/p1", wkg_ids("gitlab.com", 1:3)))
  lic <- wkg_table(file.path(r$out, "vcs-signals-shard-gitlab.db"), "license")
  expect_identical(sort(lic$repo_id, method = "radix"), c("code.usgs.gov/g/p1", wkg_ids("gitlab.com", 1:2)))
})

test_that("a 5xx or a 404 visits its repository but writes no licence row, and a failed download starts at the first", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = NA)
  roster <- wkg_roster(withr::local_tempdir(), list(gitlab.com = 1:5))
  io <- wkg_io(special = function(domain, path) {
    if (path == "/projects/2?license=true") return(wkg_answer(500L))
    if (startsWith(path, "/projects/3")) return(wkg_answer(404L))
    # A 200 whose licence is null is a reading: NA.
    if (path == "/projects/5?license=true") return(wkg_answer(body = list(id = 5L, license = NULL)))
    NULL
  })
  io$download <- function(pattern, dir) stop("HTTP 502")
  r <- wkg_run(io, roster, wkg_clock())
  snap <- wkg_snapshot(r$out)
  expect_identical(snap$contributors_total, c(1L, 2L, NA, 4L, 5L))
  expect_identical(snap$top_contributor_commits, c(10L, 20L, NA, 40L, 50L))
  lic <- wkg_table(file.path(r$out, "vcs-signals-shard-gitlab.db"), "license")
  lic <- lic[order(lic$repo_id, method = "radix"), ]
  expect_identical(lic$repo_id, wkg_ids("gitlab.com", c(1L, 4L, 5L)))
  expect_identical(lic$license, c("MIT", "MIT", NA))
  expect_identical(unique(lic$read_on), format(Sys.Date()))
  expect_identical(wkg_cursor(r$out), c(gitlab.com = "gitlab.com/g/p5"))
})

test_that("a repository whose calls all failed is visited, and the cursor moves to it", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = NA)
  roster <- wkg_roster(withr::local_tempdir(), list(gitlab.com = 1:4))
  seen <- new.env()
  io <- wkg_io(seen, special = function(domain, path) if (startsWith(path, "/projects/3")) wkg_answer(500L))
  r <- wkg_run(io, roster, wkg_clock(), budget = c(gitlab.com = 9L), deadline_min = 5)
  expect_false(any(startsWith(seen$rest, "gitlab.com/projects/4")))
  expect_identical(wkg_cursor(r$out), c(gitlab.com = "gitlab.com/g/p3"))
})

test_that("two failing repositories in a row pass the cursor although the breaker then defers the host", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = NA)
  dir <- withr::local_tempdir()
  roster <- wkg_roster(dir, list(gitlab.com = 1:5))
  rel <- file.path(dir, "release-state.db")
  failing <- function(domain, path) if (grepl("^/projects/[12][/?]", path)) wkg_answer(500L)
  logs <- new.env(); logs$lines <- character()
  r1 <- wkg_run(wkg_io(special = failing, state_from = rel), roster, wkg_clock(),
                log = function(m) logs$lines <- c(logs$lines, m))
  expect_identical(r1$pacer$deferred(), "gitlab.com")
  expect_true(any(grepl("deferred", logs$lines, fixed = TRUE)))
  expect_identical(wkg_cursor(r1$out), c(gitlab.com = "gitlab.com/g/p2"))
  # The next run starts at p3 and reads the rest.
  file.copy(file.path(r1$out, "vcs-signals-gitlab-state.db"), rel, overwrite = TRUE)
  seen <- new.env()
  r2 <- wkg_run(wkg_io(seen, special = failing, state_from = rel), roster, wkg_clock())
  expect_match(seen$rest[1], "^gitlab.com/projects/3/")
  expect_identical(wkg_counted(r2$out), wkg_ids("gitlab.com", 3:5))
})

test_that("a host whose hour turned while another host ran goes again before the run sleeps", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = NA)
  roster <- wkg_roster(withr::local_tempdir(), list(code.usgs.gov = 1:2, gitlab.com = 1:2))
  # 100 seconds before the hour; each gitlab.com request takes 40 seconds.
  clock <- wkg_clock(start = 3600 * 100 + 3500)
  io <- wkg_io(special = function(domain, path) { if (domain == "gitlab.com") clock$t <- clock$t + 40; NULL })
  r <- wkg_run(io, roster, clock, budget = c(code.usgs.gov = 3L))
  expect_true(all(clock$slept < 60))
  expect_equal(floor(clock$t / 3600), 101)
  expect_identical(wkg_counted(r$out), c(wkg_ids("code.usgs.gov", 1:2), wkg_ids("gitlab.com", 1:2)))
})

test_that("the GitLab shard's snapshot table has the GitHub shard's columns, with no bot flag or medians", {
  dir <- withr::local_tempdir()
  write_roster(file.path(dir, "vcs-signals-roster.db"), data.frame(repo_id = "github.com/a/ok", owner = "a",
    name = "ok", stars = 1L, done = 0L, stringsAsFactors = FALSE))
  gh_io <- list(graphql = function(query) list(data = list(
                  r0 = list(defaultBranchRef = list(target = list(history = list(totalCount = 100)))))),
                contributors = function(owner, name) list(count = 7L, top_commits = 5L, top_type = "User"))
  gh <- suppressMessages(run_fetch_shard(gh_io, file.path(dir, "gh"), file.path(dir, "vcs-signals-roster.db"), 0, 1,
                                         commit_delay = 0, contributor_delay = 0))
  r <- wkg_run(wkg_io(), wkg_roster(dir, list(gitlab.com = 1L)), wkg_clock())
  schema <- function(path) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE name = 'snapshot'")$sql
  }
  expect_identical(schema(file.path(r$out, "vcs-signals-shard-gitlab.db")), schema(gh))
  snap <- wkg_snapshot(r$out)
  expect_identical(names(snap), names(wkg_table(gh, "snapshot")))
  for (m in c("top_contributor_bot", "median_days_to_close_issue", "median_days_to_close_pr", "median_open_issue_age_days"))
    expect_true(is.na(snap[[m]]))
})

test_that("no @ reaches any table the job writes or any line it logs", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = NA)
  dir <- withr::local_tempdir()
  roster <- wkg_roster(dir, list(code.usgs.gov = 1:2, gitlab.com = 1:3))
  logs <- new.env(); logs$lines <- character()
  io <- wkg_io(special = function(domain, path) if (grepl("/projects/2/", path, fixed = TRUE)) wkg_answer(500L))
  msgs <- testthat::capture_messages(
    r <- wkg_run(io, roster, wkg_clock(), log = function(m) logs$lines <- c(logs$lines, m)))
  expect_gt(length(logs$lines), 0L)
  expect_false(any(grepl("@", c(logs$lines, msgs), fixed = TRUE)))
  dbs <- c(roster, list.files(r$out, pattern = "\\.db$", full.names = TRUE))
  expect_length(dbs, 3L)
  for (db in dbs) {
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    for (t in DBI::dbListTables(con)) {
      cells <- unlist(lapply(DBI::dbReadTable(con, t), as.character))
      expect_false(any(grepl("@", cells, fixed = TRUE)), info = paste(basename(db), t))
    }
    DBI::dbDisconnect(con)
  }
})
