test_that("run_update resolves, collects, materializes, and publishes with a fake io", {
  out <- tempfile("out"); dir.create(out)
  # fake io: SP1 acquisition returns a fixed 1-repo input; graphql returns fixtures;
  # release IO is an empty local release that records uploads.
  rel <- tempfile("rel"); dir.create(rel)
  io <- local_release_io(rel,
    acquire = function() data.frame(package = "ggplot2", origin = "cran",
      url_raw = "https://github.com/tidyverse/ggplot2", bugreports_raw = NA, stringsAsFactors = FALSE),
    graphql = function(query) {
      f <- if (grepl("followRenames", query)) "resolve_one.json"
           else if (grepl("history \\{ totalCount", query)) "commits.json" else "gauges_one.json"
      jsonlite::fromJSON(readLines(file.path("fixtures", f), warn = FALSE), simplifyVector = FALSE)
    })
  run_update(io, out, list(force_full = TRUE))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-signals-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  expect_true(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM signals_series")$n >= 1)
  expect_equal(DBI::dbGetQuery(con, "SELECT value FROM series_latest WHERE metric='stars'")$value, 6959)
  expect_true(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM vcs_signals_summary WHERE package='ggplot2'")$n == 1)
  expect_true("manifest.json" %in% io$uploaded())
})

test_that("run_update floor: a total collection failure on a later run leaves series_latest and summary unchanged", {
  # Same out_dir and the same local release across both calls, mirroring a real
  # daily run: the second call seeds from exactly what the first call's publish()
  # uploaded.
  out <- tempfile("out_floor"); dir.create(out)
  rel <- tempfile("rel_floor"); dir.create(rel)
  acquire_one <- function() data.frame(package = "ggplot2", origin = "cran",
    url_raw = "https://github.com/tidyverse/ggplot2", bugreports_raw = NA, stringsAsFactors = FALSE)

  io1 <- local_release_io(rel,
    acquire = acquire_one,
    graphql = function(query) {
      if (grepl("rateLimit", query)) return(list(data = list(nodes = list())))
      f <- if (grepl("followRenames", query)) "resolve_one.json"
           else if (grepl("history \\{ totalCount", query)) "commits.json" else "gauges_one.json"
      jsonlite::fromJSON(readLines(file.path("fixtures", f), warn = FALSE), simplifyVector = FALSE)
    })
  run_update(io1, out, list(force_full = TRUE))

  con1 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-signals-recent.db"))
  stars1 <- DBI::dbGetQuery(con1, "SELECT value FROM series_latest WHERE metric='stars'")$value
  summary1 <- DBI::dbGetQuery(con1, "SELECT stars, license, is_archived FROM vcs_signals_summary WHERE package='ggplot2'")
  DBI::dbDisconnect(con1)
  expect_equal(stars1, 6959)   # sanity: run 1 actually collected

  # Run 2: rateLimit preflight reports unlimited (so I3's preflight does not
  # itself explain an empty run), but every gauge/commit query errors, so
  # stage-3 collection returns nothing at all.
  io2 <- local_release_io(rel,
    acquire = acquire_one,
    graphql = function(query) {
      if (grepl("rateLimit", query)) return(list(data = list(nodes = list())))
      list(data = NULL, errors = list(list(message = "SERVICE_UNAVAILABLE")))
    })
  run_update(io2, out, list(force_full = FALSE))

  con2 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-signals-recent.db"))
  on.exit(DBI::dbDisconnect(con2))
  stars2 <- DBI::dbGetQuery(con2, "SELECT value FROM series_latest WHERE metric='stars'")$value
  summary2 <- DBI::dbGetQuery(con2, "SELECT stars, license, is_archived FROM vcs_signals_summary WHERE package='ggplot2'")

  expect_equal(stars2, stars1)         # not wiped/NA by the failed second run
  expect_equal(summary2, summary1)
})

.e2e_acquire <- function() data.frame(package = "ggplot2", origin = "cran",
  url_raw = "https://github.com/tidyverse/ggplot2", bugreports_raw = NA, stringsAsFactors = FALSE)

.e2e_fixture <- function(query) {
  f <- if (grepl("followRenames", query)) "resolve_one.json"
       else if (grepl("history \\{ totalCount", query)) "commits.json" else "gauges_one.json"
  jsonlite::fromJSON(readLines(file.path("fixtures", f), warn = FALSE), simplifyVector = FALSE)
}

.owner_table <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  list(rows = DBI::dbReadTable(con, "vcs_repo_owner"),
       sql = DBI::dbGetQuery(con,
         "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = 'vcs_repo_owner'")$sql,
       indexes = sort(DBI::dbGetQuery(con,
         "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'vcs_repo_owner'")$name))
}

test_that("the daily run publishes each repository's owner, and a later run seeded from it keeps the row", {
  out <- tempfile("out_owner"); dir.create(out)
  rel <- tempfile("rel_owner"); dir.create(rel)
  suppressMessages(capture.output(
    run_update(local_release_io(rel, acquire = .e2e_acquire, graphql = .e2e_fixture), out,
               list(force_full = TRUE))))

  for (f in c("vcs-signals-summary.db", "vcs-signals-recent.db")) {
    t <- .owner_table(file.path(rel, f))
    expect_equal(nrow(t$rows), 1L, info = f)
    expect_equal(t$rows$repo_id, "github.com/tidyverse/ggplot2", info = f)
    expect_equal(t$rows$node_id, "R_a", info = f)
    expect_equal(t$rows$owner_login_current, "tidyverse", info = f)
    expect_equal(t$rows$owner_type, "Organization", info = f)
    expect_equal(t$rows$owner_node_id, "O_1", info = f)
    expect_equal(t$rows$name_with_owner_current, "tidyverse/ggplot2", info = f)
    expect_equal(t$rows$observed_on, format(Sys.Date()), info = f)
    expect_match(t$sql, "WITHOUT ROWID", fixed = TRUE, info = f)
    expect_equal(t$indexes, c("idx_vro_login", "idx_vro_node", "idx_vro_owner_node"), info = f)
  }

  # The second run's gauge answer does not include R_a, so its row can only
  # come from the seed.
  other <- function(query) {
    if (grepl("followRenames|history \\{ totalCount", query)) return(.e2e_fixture(query))
    list(data = list(nodes = list(list(id = "R_other", nameWithOwner = "x/y",
      owner = list(`__typename` = "User", login = "x", id = "U_x")))))
  }
  suppressMessages(capture.output(
    run_update(local_release_io(rel, acquire = .e2e_acquire, graphql = other), out, list())))
  t2 <- .owner_table(file.path(rel, "vcs-signals-recent.db"))
  expect_equal(t2$rows$repo_id, "github.com/tidyverse/ggplot2")
  expect_equal(t2$rows$owner_login_current, "tidyverse")
})

test_that("a run that collected nothing, or stopped at the rate-limit reserve, leaves the owner table as seeded", {
  out <- tempfile("out_owner_floor"); dir.create(out)
  rel <- tempfile("rel_owner_floor"); dir.create(rel)
  rid <- repo_slug("github.com", "tidyverse", "ggplot2")
  # Dated three days back, so a run that rewrote the row would change it.
  seen <- format(Sys.Date() - 3)
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-recent.db"))
  ensure_repo_schema(scon); ensure_series_schema(scon)
  DBI::dbExecute(scon, "INSERT INTO repos
    (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status)
    VALUES (?,?,?,?,?,?,?,?,?,?,?,?)",
    params = list(rid, "R_a", "github", "github.com", "tidyverse", "ggplot2", "tidyverse/ggplot2",
                  1L, 1L, "2020-01-01", "2020-01-01", "active"))
  DBI::dbExecute(scon, "INSERT INTO repo_packages (repo_id,package,origin,resolved_from) VALUES (?,?,?,?)",
    params = list(rid, "ggplot2", "cran", "url"))
  DBI::dbExecute(scon, "INSERT INTO vcs_repo_owner
    (repo_id, node_id, owner_login_current, owner_type, owner_node_id, name_with_owner_current, observed_on)
    VALUES (?,?,?,?,?,?,?)",
    params = list(rid, "R_a", "tidyverse", "Organization", "O_1", "tidyverse/ggplot2", seen))
  seeded <- DBI::dbReadTable(scon, "vcs_repo_owner")
  DBI::dbDisconnect(scon)
  writeLines('{"summary":{"years":[]}}', file.path(rel, "manifest.json"))

  failing <- function(query) {
    if (grepl("rateLimit", query)) return(list(data = list(nodes = list())))
    list(data = NULL, errors = list(list(message = "SERVICE_UNAVAILABLE")))
  }
  suppressMessages(capture.output(
    run_update(local_release_io(rel, acquire = .e2e_acquire, graphql = failing), out, list())))
  expect_identical(.owner_table(file.path(rel, "vcs-signals-recent.db"))$rows, seeded)
  expect_identical(.owner_table(file.path(rel, "vcs-signals-summary.db"))$rows, seeded)

  spent <- function(query) {
    if (grepl("rateLimit", query)) return(list(data = list(rateLimit = list(remaining = 0L))))
    .e2e_fixture(query)
  }
  suppressMessages(capture.output(
    run_update(local_release_io(rel, acquire = .e2e_acquire, graphql = spent), out, list())))
  expect_identical(.owner_table(file.path(rel, "vcs-signals-recent.db"))$rows, seeded)
  expect_identical(.owner_table(file.path(rel, "vcs-signals-summary.db"))$rows, seeded)
})

test_that("the first daily run over a release that never carried the owner table publishes it", {
  out <- tempfile("out_owner_first"); dir.create(out)
  rel <- tempfile("rel_owner_first"); dir.create(rel)
  io <- local_release_io(rel, acquire = .e2e_acquire, graphql = .e2e_fixture)
  suppressMessages(capture.output(run_update(io, out, list(force_full = TRUE))))
  # What code from before the table publishes: neither shard has it.
  for (f in c("vcs-signals-summary.db", "vcs-signals-recent.db")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, f))
    DBI::dbExecute(con, "DROP TABLE vcs_repo_owner")
    DBI::dbDisconnect(con)
  }

  suppressMessages(capture.output(run_update(io, out, list())))
  for (f in c("vcs-signals-summary.db", "vcs-signals-recent.db")) {
    t <- .owner_table(file.path(rel, f))
    expect_equal(t$rows$owner_login_current, "tidyverse", info = f)
    expect_equal(t$indexes, c("idx_vro_login", "idx_vro_node", "idx_vro_owner_node"), info = f)
  }
})

test_that("a repository deleted on GitHub keeps its owner row while the rest of its batch is written", {
  out <- tempfile("out_owner_gone"); dir.create(out)
  rel <- tempfile("rel_owner_gone"); dir.create(rel)
  ids <- c(repo_slug("github.com", "someone", "gone"), repo_slug("github.com", "tidyverse", "ggplot2"))
  seen <- format(Sys.Date() - 3)
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-recent.db"))
  ensure_repo_schema(scon); ensure_series_schema(scon)
  DBI::dbExecute(scon, "INSERT INTO repos
    (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status)
    VALUES (?,?,?,?,?,?,?,?,?,?,?,?)",
    params = list(ids, c("R_gone", "R_a"), rep("github", 2), rep("github.com", 2),
                  c("someone", "tidyverse"), c("gone", "ggplot2"), c("someone/gone", "tidyverse/ggplot2"),
                  rep(1L, 2), rep(1L, 2), rep("2020-01-01", 2), rep("2020-01-01", 2), rep("active", 2)))
  DBI::dbExecute(scon, "INSERT INTO repo_packages (repo_id,package,origin,resolved_from) VALUES (?,?,?,?)",
    params = list(ids, c("gonepkg", "ggplot2"), rep("cran", 2), rep("url", 2)))
  DBI::dbExecute(scon, "INSERT INTO vcs_repo_owner
    (repo_id, node_id, owner_login_current, owner_type, owner_node_id, name_with_owner_current, observed_on)
    VALUES (?,?,?,?,?,?,?)",
    params = list(ids[1], "R_gone", "someone", "User", "U_1", "someone/gone", seen))
  DBI::dbDisconnect(scon)
  writeLines('{"summary":{"years":[]}}', file.path(rel, "manifest.json"))

  acquire_two <- function() data.frame(package = c("gonepkg", "ggplot2"), origin = "cran",
    url_raw = c("https://github.com/someone/gone", "https://github.com/tidyverse/ggplot2"),
    bugreports_raw = NA, stringsAsFactors = FALSE)
  # GitHub answers a deleted node with null plus a NOT_FOUND error, which fails the
  # whole batch, so the collector splits it until the deleted node is alone.
  one <- .e2e_fixture("gauges")$data$nodes[[1]]
  graphql <- function(query) {
    if (grepl("rateLimit", query)) return(list(data = list(nodes = list())))
    if (grepl("R_gone", query, fixed = TRUE))
      return(list(data = list(nodes = if (grepl("R_a", query, fixed = TRUE)) list(NULL, one) else list(NULL)),
                  errors = list(list(type = "NOT_FOUND", path = list("nodes", 0L),
                                     message = "Could not resolve to a node with the global id of 'R_gone'"))))
    .e2e_fixture(query)
  }
  suppressMessages(capture.output(
    run_update(local_release_io(rel, acquire = acquire_two, graphql = graphql), out, list())))

  for (f in c("vcs-signals-summary.db", "vcs-signals-recent.db")) {
    rows <- .owner_table(file.path(rel, f))$rows
    expect_equal(rows$repo_id, ids, info = f)
    expect_equal(rows$owner_login_current, c("someone", "tidyverse"), info = f)
    expect_equal(rows$observed_on, c(seen, format(Sys.Date())), info = f)
  }
})

.recent_repo <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  list(repo = DBI::dbGetQuery(con, "SELECT node_id, status FROM repos"),
       stars = DBI::dbGetQuery(con, "SELECT value FROM series_latest WHERE metric = 'stars'")$value)
}

.dead_gauge <- function(resolve, counter) function(query) {
  if (grepl("followRenames", query)) { counter$resolve <- counter$resolve + 1L; return(resolve(query)) }
  if (grepl("history \\{ totalCount|rateLimit", query)) return(.e2e_fixture(query))
  counter$gauge <- counter$gauge + 1L
  list(data = list(nodes = list(NULL)),
       errors = list(list(type = "NOT_FOUND", path = list("nodes", 0L),
                          message = "Could not resolve to a node with the global id of 'R_a'")))
}

.seeded_dead_run <- function(tag) {
  out <- tempfile(paste0("out_", tag)); dir.create(out)
  rel <- tempfile(paste0("rel_", tag)); dir.create(rel)
  suppressMessages(capture.output(
    run_update(local_release_io(rel, acquire = .e2e_acquire, graphql = .e2e_fixture), out,
               list(force_full = TRUE))))
  list(out = out, rel = rel)
}

test_that("one transient NOT_FOUND followed by a healthy day leaves the repo active and gauged", {
  d <- .seeded_dead_run("transient")
  n <- new.env(); n$gauge <- 0L; n$resolve <- 0L
  log <- capture.output(suppressMessages(run_update(
    local_release_io(d$rel, acquire = .e2e_acquire, graphql = .dead_gauge(.e2e_fixture, n)), d$out, list())))
  expect_equal(n$gauge, 1L)
  expect_equal(n$resolve, 1L)
  expect_true(any(grepl("1 node ids no longer resolve, 0 moved", log, fixed = TRUE)))
  after <- .recent_repo(file.path(d$rel, "vcs-signals-recent.db"))
  expect_equal(after$repo$node_id, "R_a")
  expect_equal(after$repo$status, "active")
  expect_equal(after$stars, 6959)

  gauged <- 0L
  healthy <- function(query) {
    if (!grepl("followRenames|history \\{ totalCount|rateLimit", query)) gauged <<- gauged + 1L
    .e2e_fixture(query)
  }
  suppressMessages(capture.output(run_update(
    local_release_io(d$rel, acquire = .e2e_acquire, graphql = healthy), d$out, list())))
  expect_equal(gauged, 1L)
  after <- .recent_repo(file.path(d$rel, "vcs-signals-recent.db"))
  expect_equal(after$repo$node_id, "R_a")
  expect_equal(after$repo$status, "active")
  expect_equal(after$stars, 6959)
})

test_that("a repo whose name lookup also finds nothing keeps its id and is never marked gone", {
  d <- .seeded_dead_run("404")
  n <- new.env(); n$gauge <- 0L; n$resolve <- 0L
  missing <- function(query) list(data = list(r0 = NULL),
    errors = list(list(type = "NOT_FOUND", path = list("r0"), message = "Could not resolve to a Repository")))
  for (i in 1:3) suppressMessages(capture.output(run_update(
    local_release_io(d$rel, acquire = .e2e_acquire, graphql = .dead_gauge(missing, n)), d$out, list())))
  after <- .recent_repo(file.path(d$rel, "vcs-signals-recent.db"))
  expect_equal(after$repo$node_id, "R_a")
  expect_equal(after$repo$status, "active")
  expect_equal(after$stars, 6959)
  expect_equal(n$gauge, 3L)
})

test_that("a repo recreated under the same name moves to its new node id", {
  d <- .seeded_dead_run("recreated")
  n <- new.env(); n$gauge <- 0L; n$resolve <- 0L
  recreated <- function(query) list(data = list(r0 = list(id = "R_new", nameWithOwner = "tidyverse/ggplot2",
    isArchived = FALSE, isFork = FALSE, isMirror = FALSE, createdAt = "2026-09-01T00:00:00Z")))
  log <- capture.output(suppressMessages(run_update(
    local_release_io(d$rel, acquire = .e2e_acquire, graphql = .dead_gauge(recreated, n)), d$out, list())))
  expect_true(any(grepl("1 node ids no longer resolve, 1 moved", log, fixed = TRUE)))
  after <- .recent_repo(file.path(d$rel, "vcs-signals-recent.db"))
  expect_equal(after$repo$node_id, "R_new")
  expect_equal(after$repo$status, "active")
})

test_that("a run past its gauge deadline still publishes and keeps the last values", {
  out <- tempfile("out_deadline"); dir.create(out)
  rel <- tempfile("rel_deadline"); dir.create(rel)
  suppressMessages(capture.output(
    run_update(local_release_io(rel, acquire = .e2e_acquire, graphql = .e2e_fixture), out,
               list(force_full = TRUE))))

  gauge_queries <- 0L
  counting <- function(query) {
    if (!grepl("followRenames|history \\{ totalCount|rateLimit", query)) gauge_queries <<- gauge_queries + 1L
    .e2e_fixture(query)
  }
  io <- local_release_io(rel, acquire = .e2e_acquire, graphql = counting)
  log <- capture.output(res <- suppressMessages(run_update(io, out, list(gauge_deadline_min = 0))))
  expect_true(res$gauge_cut_fail)
  expect_equal(gauge_queries, 0L)
  expect_true("manifest.json" %in% io$uploaded())
  expect_true(any(grepl("1 repos not reached by the 0 min deadline", log, fixed = TRUE)))
  expect_true(any(grepl("^published \\(", log)))
  expect_true(any(grepl("^::warning::gauges: 1 of 1 repos", log)))
  expect_true(any(grepl("^::error::gauges:", log)))
  expect_equal(.recent_repo(file.path(rel, "vcs-signals-recent.db"))$stars, 6959)
})

# ---- GitLab beside GitHub ---------------------------------------------------------
.e2e_gl_acquire <- function() data.frame(package = c("ggplot2", "rurl", "inlpubs"), origin = "cran",
  url_raw = c("https://github.com/tidyverse/ggplot2", "https://gitlab.com/bart-turczynski/rurl",
              "https://code.usgs.gov/inl/inlpubs"),
  bugreports_raw = NA, stringsAsFactors = FALSE)

.e2e_gl_nodes <- function(domain) {
  f <- c(gitlab.com = "gitlab_com.json", code.usgs.gov = "code_usgs_gov.json")[[domain]]
  jsonlite::fromJSON(file.path("fixtures", "gitlab", f), simplifyVector = FALSE)$data$projects$nodes
}

# GitLab as it answers: unknown keys left out, paths matched without case. A host in `html`
# answers a sign-in page, a path in `drop` is left out, and `edit` changes each project.
# Every request is kept in seen$gl as "<domain> <keys>".
.e2e_gitlab <- function(seen = new.env(), html = character(0), drop = character(0), edit = identity) {
  seen$gl <- character(0)
  function(domain, query, variables) {
    keys <- unlist(variables$keys)
    seen$gl <- c(seen$gl, paste(domain, paste(keys, collapse = " ")))
    if (domain %in% html) return(list(data = NULL, errors = NULL, status = 200L, error = "response is not JSON"))
    hit <- Filter(function(n) (tolower(n$fullPath) %in% tolower(keys) || n$id %in% keys) && !(n$fullPath %in% drop),
                  .e2e_gl_nodes(domain))
    list(data = list(projects = list(nodes = lapply(hit, edit))), errors = NULL, status = 200L, error = NA_character_)
  }
}

# GitHub from the recorded fixtures, every query kept in seen$gh; `spent` answers the
# rate-limit probe with nothing left.
.e2e_github <- function(seen = new.env(), spent = FALSE) {
  seen$gh <- character(0)
  function(query) {
    seen$gh <- c(seen$gh, query)
    if (spent && grepl("rateLimit", query)) return(list(data = list(rateLimit = list(remaining = 0L))))
    .e2e_fixture(query)
  }
}

.e2e_gl_io <- function(rel, seen = new.env(), spent = FALSE, ...)
  local_release_io(rel, acquire = .e2e_gl_acquire, graphql = .e2e_github(seen, spent),
                   gl_graphql = .e2e_gitlab(seen, ...))

.e2e_read <- function(rel, sql, db = "vcs-signals-recent.db") {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, db))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

.e2e_summary <- function(rel, pkg)
  .e2e_read(rel, sprintf("SELECT * FROM vcs_signals_summary WHERE package = '%s'", pkg), "vcs-signals-summary.db")

.e2e_latest <- function(rel, repo_id) {
  x <- .e2e_read(rel, sprintf("SELECT metric, value FROM series_latest WHERE repo_id = '%s'", repo_id))
  stats::setNames(x$value, x$metric)
}

.e2e_gl_first_run <- function(tag) {
  out <- tempfile(paste0("out_", tag)); dir.create(out)
  rel <- tempfile(paste0("rel_", tag)); dir.create(rel)
  seen <- new.env()
  suppressMessages(capture.output(run_update(.e2e_gl_io(rel, seen), out, list(force_full = TRUE))))
  list(out = out, rel = rel, seen = seen)
}

test_that("the daily run collects GitLab repositories beside GitHub's, with no owner rows and NULL for what GitLab lacks", {
  d <- .e2e_gl_first_run("gl")
  rurl <- .e2e_summary(d$rel, "rurl")
  expect_identical(rurl$repo_id, "gitlab.com/bart-turczynski/rurl")
  expect_identical(c(rurl$stars, rurl$forks, rurl$issues_open, rurl$prs_open, rurl$releases_total), c(7L, 3L, 0L, 2L, 2L))
  expect_identical(rurl$topics, "domain-name-checker,url-parsing")
  expect_identical(rurl$is_archived, 0L)
  expect_identical(rurl$repo_created_at, "2026-08-01T15:08:14Z")
  expect_identical(rurl$last_commit_date, "2026-10-12T20:40:43Z")
  expect_identical(rurl$last_release_date, "2026-10-02T15:39:10Z")
  expect_identical(rurl$last_release_tag, "v3.1.0")
  expect_identical(rurl$license, NA_character_)
  inl <- .e2e_summary(d$rel, "inlpubs")
  expect_identical(inl$repo_id, "code.usgs.gov/inl/inlpubs")
  expect_identical(c(inl$stars, inl$forks, inl$issues_open, inl$prs_open, inl$releases_total), c(0L, 2L, 0L, 0L, 10L))
  expect_identical(inl$topics, "")
  expect_identical(inl$last_commit_date, "2026-07-06T22:14:50Z")
  expect_identical(inl$last_release_tag, "v1.4.0")
  expect_identical(inl$license, NA_character_)

  latest <- .e2e_latest(d$rel, "gitlab.com/bart-turczynski/rurl")
  expect_equal(latest[c("issues_closed", "prs_closed", "prs_merged")], c(issues_closed = 0, prs_closed = 3, prs_merged = 238))
  expect_false(any(c("watchers", "size_kb") %in% names(latest)))
  expect_false(any(c("watchers", "size_kb") %in% names(.e2e_latest(d$rel, "code.usgs.gov/inl/inlpubs"))))

  repos <- .e2e_read(d$rel, "SELECT repo_id, node_id, status FROM repos ORDER BY repo_id")
  expect_identical(repos$node_id, c("gitlab:code.usgs.gov:4520", "R_a", "gitlab:gitlab.com:85027322"))
  expect_identical(repos$status, rep("active", 3))
  expect_identical(.e2e_read(d$rel, "SELECT repo_id FROM vcs_repo_owner")$repo_id, "github.com/tidyverse/ggplot2")
  expect_identical(.e2e_read(d$rel, "SELECT node_id FROM vcs_repo_name_history")$node_id, "R_a")
  expect_identical(.e2e_summary(d$rel, "ggplot2")$stars, 6959L)

  # Routed by host: GitHub never hears of a GitLab path, nor GitLab of a GitHub one.
  expect_false(any(grepl("rurl|inlpubs|bart-turczynski", d$seen$gh)))
  expect_false(any(grepl("ggplot2|tidyverse", d$seen$gl)))
  expect_identical(d$seen$gl, c("code.usgs.gov inl/inlpubs", "gitlab.com bart-turczynski/rurl",
                                "code.usgs.gov gid://gitlab/Project/4520", "gitlab.com gid://gitlab/Project/85027322"))
})

test_that("a GitHub budget below the reserve skips only GitHub, and GitLab is still collected", {
  d <- .e2e_gl_first_run("gl_spent")
  owners <- .e2e_read(d$rel, "SELECT * FROM vcs_repo_owner")
  seen <- new.env()
  more <- function(n) { n$starCount <- n$starCount + 5L; n }
  io <- .e2e_gl_io(d$rel, seen, spent = TRUE, edit = more)
  res <- suppressMessages(capture.output(run_update(io, d$out, list())))
  expect_length(seen$gh, 1L)
  expect_match(seen$gh, "rateLimit", fixed = TRUE)
  expect_identical(.e2e_summary(d$rel, "rurl")$stars, 12L)
  expect_identical(.e2e_summary(d$rel, "inlpubs")$stars, 5L)
  expect_equal(.e2e_latest(d$rel, "gitlab.com/bart-turczynski/rurl")[["stars"]], 12)
  # GitHub keeps what it had, its owner row included.
  expect_identical(.e2e_summary(d$rel, "ggplot2")$stars, 6959L)
  expect_identical(.e2e_read(d$rel, "SELECT * FROM vcs_repo_owner"), owners)
  expect_true("manifest.json" %in% io$uploaded())
})

test_that("with GitHub over budget and every GitLab host failing, the run publishes a heartbeat", {
  d <- .e2e_gl_first_run("gl_none")
  summary1 <- .e2e_summary(d$rel, "rurl")
  latest1 <- .e2e_read(d$rel, "SELECT * FROM series_latest ORDER BY repo_id, metric")
  io <- .e2e_gl_io(d$rel, spent = TRUE, html = c("gitlab.com", "code.usgs.gov"))
  said <- new.env(); said$m <- character(0)
  withCallingHandlers(capture.output(run_update(io, d$out, list())), message = function(m) {
    said$m <- c(said$m, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_true(any(grepl("stage-4 floor", said$m, fixed = TRUE)))
  expect_identical(.e2e_summary(d$rel, "rurl"), summary1)
  expect_identical(.e2e_read(d$rel, "SELECT * FROM series_latest ORDER BY repo_id, metric"), latest1)
})

test_that("a GitLab host answering HTML defers only its own repositories", {
  d <- .e2e_gl_first_run("gl_html")
  seen <- new.env()
  more <- function(n) { n$starCount <- n$starCount + 5L; n }
  io <- .e2e_gl_io(d$rel, seen, html = "code.usgs.gov", edit = more)
  log <- capture.output(res <- suppressMessages(run_update(io, d$out, list())))
  expect_false(isTRUE(res$gauge_cut_fail))
  expect_true(any(grepl("nodes(ids:", seen$gh, fixed = TRUE)))
  expect_identical(.e2e_summary(d$rel, "rurl")$stars, 12L)
  inl <- .e2e_summary(d$rel, "inlpubs")
  expect_identical(c(inl$stars, inl$forks, inl$releases_total), c(0L, 2L, 10L))
  expect_identical(inl$last_release_tag, "v1.4.0")
  expect_identical(.e2e_read(d$rel, "SELECT node_id, status FROM repos WHERE repo_id = 'code.usgs.gov/inl/inlpubs'"),
                   data.frame(node_id = "gitlab:code.usgs.gov:4520", status = "active", stringsAsFactors = FALSE))
  expect_true(any(grepl("code.usgs.gov.*response is not JSON", log)))
})

test_that("a GitLab project missing from the answer keeps its id and values and is never looked up on GitHub", {
  d <- .e2e_gl_first_run("gl_missing")
  seen <- new.env()
  for (i in 1:2) suppressMessages(capture.output(
    run_update(.e2e_gl_io(d$rel, seen, drop = "bart-turczynski/rurl"), d$out, list())))
  expect_false(any(grepl("followRenames", seen$gh, fixed = TRUE)))
  expect_identical(.e2e_read(d$rel, "SELECT node_id, status FROM repos WHERE repo_id = 'gitlab.com/bart-turczynski/rurl'"),
                   data.frame(node_id = "gitlab:gitlab.com:85027322", status = "active", stringsAsFactors = FALSE))
  expect_identical(.e2e_summary(d$rel, "rurl")$stars, 7L)
  expect_identical(.e2e_summary(d$rel, "rurl")$last_release_tag, "v3.1.0")
})

test_that("an unresolved GitLab path stays active without an id and is asked again the next run", {
  out <- tempfile("out_gl_unresolved"); dir.create(out)
  rel <- tempfile("rel_gl_unresolved"); dir.create(rel)
  seen <- new.env()
  suppressMessages(capture.output(
    run_update(.e2e_gl_io(rel, seen, drop = "inl/inlpubs"), out, list(force_full = TRUE))))
  expect_identical(.e2e_read(rel, "SELECT node_id, status FROM repos WHERE repo_id = 'code.usgs.gov/inl/inlpubs'"),
                   data.frame(node_id = NA_character_, status = "active", stringsAsFactors = FALSE))
  suppressMessages(capture.output(run_update(.e2e_gl_io(rel, seen), out, list())))
  expect_true("code.usgs.gov inl/inlpubs" %in% seen$gl)
  expect_identical(.e2e_summary(rel, "inlpubs")$releases_total, 10L)
})

test_that("one GitLab pacer serves both stages, so a host's request cap covers the whole run", {
  old <- GITLAB_MAX_REQUESTS
  assign("GITLAB_MAX_REQUESTS", 1L, envir = globalenv())
  withr::defer(assign("GITLAB_MAX_REQUESTS", old, envir = globalenv()))
  d <- .e2e_gl_first_run("gl_cap")
  expect_identical(d$seen$gl, c("code.usgs.gov inl/inlpubs", "gitlab.com bart-turczynski/rurl"))
  expect_identical(.e2e_read(d$rel, "SELECT node_id FROM repos WHERE repo_id = 'gitlab.com/bart-turczynski/rurl'")$node_id,
                   "gitlab:gitlab.com:85027322")
  expect_length(.e2e_latest(d$rel, "gitlab.com/bart-turczynski/rurl"), 0L)
})

test_that("the real daily io reaches GitLab through gl_graphql", {
  src <- paste(readLines(file.path(.repo_root, "scripts", "update.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("gl_graphql\\s*=\\s*gl_graphql", src))
})
