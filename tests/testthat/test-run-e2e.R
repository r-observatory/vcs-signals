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
# Every request is kept in seen$gl as "<domain> <keys>", and in seen$all after GitHub's.
# With `clock`, each request moves clock$t on by `cost` seconds.
.e2e_gitlab <- function(seen = new.env(), html = character(0), drop = character(0), edit = identity,
                        clock = NULL, cost = 0) {
  seen$gl <- character(0)
  function(domain, query, variables) {
    keys <- unlist(variables$keys)
    seen$gl <- c(seen$gl, paste(domain, paste(keys, collapse = " ")))
    seen$all <- c(seen$all, paste("gitlab", domain))
    if (!is.null(clock)) clock$t <- clock$t + cost
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
    seen$all <- c(seen$all, if (grepl("nodes(ids:", query, fixed = TRUE)) "github gauges" else "github")
    if (spent && grepl("rateLimit", query)) return(list(data = list(rateLimit = list(remaining = 0L))))
    .e2e_fixture(query)
  }
}

.e2e_gl_io <- function(rel, seen = new.env(), spent = FALSE, ...) {
  seen$all <- character(0)
  local_release_io(rel, acquire = .e2e_gl_acquire, graphql = .e2e_github(seen, spent),
                   gl_graphql = .e2e_gitlab(seen, ...))
}

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
  expect_true(any(grepl("^gauges: GitHub skipped", res)))
  expect_false(any(grepl("collected 0 repos", res, fixed = TRUE)))
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
  expect_true(any(grepl("^GitLab code.usgs.gov: .*response is not JSON", log)))
  expect_false(any(grepl("gitlab: GitLab", log, fixed = TRUE)))
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

test_that("a day GitHub skips while GitLab collects leaves an owner row last seen 15 days ago, and publishes", {
  d <- .e2e_gl_first_run("gl_owner_old")
  old <- format(Sys.Date() - 15)
  for (f in c("vcs-signals-recent.db", "vcs-signals-summary.db")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d$rel, f))
    DBI::dbExecute(con, "UPDATE vcs_repo_owner SET observed_on = ?", params = list(old))
    DBI::dbDisconnect(con)
  }
  owners <- .e2e_read(d$rel, "SELECT * FROM vcs_repo_owner")
  expect_identical(owners$observed_on, old)
  more <- function(n) { n$starCount <- n$starCount + 5L; n }
  io <- .e2e_gl_io(d$rel, spent = TRUE, edit = more)
  expect_no_error(suppressMessages(capture.output(run_update(io, d$out, list()))))
  expect_true("manifest.json" %in% io$uploaded())
  expect_identical(.e2e_summary(d$rel, "rurl")$stars, 12L)
  for (f in c("vcs-signals-recent.db", "vcs-signals-summary.db"))
    expect_identical(.e2e_read(d$rel, "SELECT * FROM vcs_repo_owner", f), owners, info = f)
})

test_that("a fault in the GitLab code is logged and leaves GitHub and the publish going", {
  old <- gl_parse_projects
  assign("gl_parse_projects", function(...) stop("simulated fault in gitlab.R"), envir = globalenv())
  restored <- FALSE
  withr::defer(if (!restored) assign("gl_parse_projects", old, envir = globalenv()))
  out <- tempfile("out_gl_fault"); dir.create(out)
  rel <- tempfile("rel_gl_fault"); dir.create(rel)
  seen <- new.env()
  io <- .e2e_gl_io(rel, seen)
  log <- capture.output(res <- suppressMessages(run_update(io, out, list(force_full = TRUE))))
  expect_true(any(grepl("nodes(ids:", seen$gh, fixed = TRUE)))
  expect_true("manifest.json" %in% io$uploaded())
  expect_identical(.e2e_summary(rel, "ggplot2")$stars, 6959L)
  expect_true(any(grepl("simulated fault in gitlab.R", log, fixed = TRUE)))
  expect_identical(.e2e_read(rel, "SELECT node_id, status FROM repos WHERE host = 'gitlab'")$status, c("active", "active"))
  expect_true(all(is.na(.e2e_read(rel, "SELECT node_id FROM repos WHERE host = 'gitlab'")$node_id)))

  # A fault in the gauge step, once the ids are known, keeps GitLab's last values.
  assign("gl_parse_projects", old, envir = globalenv())
  restored <- TRUE
  suppressMessages(capture.output(run_update(.e2e_gl_io(rel), out, list())))
  expect_identical(.e2e_summary(rel, "rurl")$stars, 7L)
  old_cg <- gl_collect_gauges
  assign("gl_collect_gauges", function(...) stop("simulated gauge fault"), envir = globalenv())
  withr::defer(assign("gl_collect_gauges", old_cg, envir = globalenv()))
  more <- function(n) { n$starCount <- n$starCount + 5L; n }
  seen2 <- new.env()
  io2 <- .e2e_gl_io(rel, seen2, edit = more)
  log2 <- capture.output(res2 <- suppressMessages(run_update(io2, out, list())))
  expect_true(any(grepl("simulated gauge fault", log2, fixed = TRUE)))
  expect_true(any(grepl("nodes(ids:", seen2$gh, fixed = TRUE)))
  expect_true("manifest.json" %in% io2$uploaded())
  expect_identical(.e2e_summary(rel, "rurl")$stars, 7L)
  expect_identical(.e2e_summary(rel, "ggplot2")$stars, 6959L)
})

test_that("GitLab asks nothing until GitHub's gauge pass is done", {
  d <- .e2e_gl_first_run("gl_order")
  all <- d$seen$all
  expect_true(any(all == "github gauges"))
  expect_gt(min(grep("^gitlab ", all)), max(which(all == "github gauges")))
  # The next day too, when only the gauges are asked.
  seen <- new.env()
  suppressMessages(capture.output(run_update(.e2e_gl_io(d$rel, seen), d$out, list())))
  expect_gt(min(grep("^gitlab ", seen$all)), max(which(seen$all == "github gauges")))
})

test_that("GitLab still runs after GitHub's gauge pass is cut by its deadline", {
  d <- .e2e_gl_first_run("gl_after_cut")
  seen <- new.env()
  more <- function(n) { n$starCount <- n$starCount + 5L; n }
  log <- capture.output(res <- suppressMessages(
    run_update(.e2e_gl_io(d$rel, seen, edit = more), d$out, list(gauge_deadline_min = 0))))
  expect_false(any(grepl("nodes(ids:", seen$gh, fixed = TRUE)))
  expect_true(any(grepl("1 repos not reached by the 0 min deadline", log, fixed = TRUE)))
  expect_length(seen$gl, 2L)
  expect_identical(.e2e_summary(d$rel, "rurl")$stars, 12L)
  expect_identical(.e2e_summary(d$rel, "inlpubs")$stars, 5L)
  expect_identical(.e2e_summary(d$rel, "ggplot2")$stars, 6959L)
})

test_that("GitLab gets its own allowance past GitHub's deadline, and what it does not reach keeps its values", {
  expect_identical(GITLAB_DEADLINE_EXTRA_MIN, 10)
  clock <- new.env(); clock$t <- 1e9
  out <- tempfile("out_gl_slow"); dir.create(out)
  rel <- tempfile("rel_gl_slow"); dir.create(rel)
  seen <- new.env()
  # Each GitLab request takes 6 minutes, so the 10 minutes past GitHub's deadline fit two.
  io <- .e2e_gl_io(rel, seen, clock = clock, cost = 360)
  log <- capture.output(suppressMessages(
    run_update(io, out, list(force_full = TRUE, gauge_deadline_min = 0, now = function() clock$t))))
  expect_identical(seen$gl, c("code.usgs.gov inl/inlpubs", "gitlab.com bart-turczynski/rurl"))
  expect_true("manifest.json" %in% io$uploaded())
  ids <- .e2e_read(rel, "SELECT repo_id, node_id, status FROM repos WHERE host = 'gitlab' ORDER BY repo_id")
  expect_identical(ids$node_id, c("gitlab:code.usgs.gov:4520", "gitlab:gitlab.com:85027322"))
  expect_identical(ids$status, c("active", "active"))
  expect_true(any(grepl("2 not reached", log, fixed = TRUE)))
  expect_length(.e2e_latest(rel, "gitlab.com/bart-turczynski/rurl"), 0L)

  # A path the allowance does not reach stays without an id, active, for the next run.
  clock2 <- new.env(); clock2$t <- 2e9
  out2 <- tempfile("out_gl_slow2"); dir.create(out2)
  rel2 <- tempfile("rel_gl_slow2"); dir.create(rel2)
  seen2 <- new.env()
  io2 <- .e2e_gl_io(rel2, seen2, clock = clock2, cost = 660)
  suppressMessages(capture.output(
    run_update(io2, out2, list(force_full = TRUE, gauge_deadline_min = 0, now = function() clock2$t))))
  expect_identical(seen2$gl, "code.usgs.gov inl/inlpubs")
  ids2 <- .e2e_read(rel2, "SELECT node_id, status FROM repos WHERE host = 'gitlab' ORDER BY repo_id")
  expect_identical(ids2$node_id, c("gitlab:code.usgs.gov:4520", NA))
  expect_identical(ids2$status, c("active", "active"))
})

# ---- Codeberg after GitLab ---------------------------------------------------------
.e2e_cb_acquire <- function(extra = character(0)) function() data.frame(
  package = c("ggplot2", "sandwich", names(extra)), origin = "cran",
  url_raw = c("https://github.com/tidyverse/ggplot2", "https://codeberg.org/zeileis/sandwich", unname(extra)),
  bugreports_raw = NA, stringsAsFactors = FALSE)
.e2e_cb_obj <- function(stars = 1L) cb_obj(2371475L, "zeileis/sandwich", stars_count = stars, forks_count = 1L,
  watchers_count = 1L, open_issues_count = 2L, open_pr_counter = 2L, has_releases = FALSE, release_counter = 0L,
  size = 9104L, topics = list(), created_at = "2023-05-04T10:00:00+02:00")
.e2e_cb_routes <- function(stars = 1L) c(
  list("/users/zeileis/repos?limit=50&page=1" = cb_list(list(.e2e_cb_obj(stars)))),
  cb_gauge_routes("zeileis/sandwich", commit = cb_commits("2026-10-12T00:19:58+02:00")))
# Codeberg as it answers `routes`; each request is kept in seen$cb and seen$all, and moves clock$t on by `cost`.
.e2e_codeberg <- function(seen, routes = .e2e_cb_routes(), clock = NULL, cost = 0) {
  inner <- new.env(); fake <- cb_fake(routes, inner); seen$cb <- character(0)
  function(path) {
    seen$cb <- c(seen$cb, path); seen$all <- c(seen$all, paste("codeberg", path))
    if (!is.null(clock)) clock$t <- clock$t + cost
    fake(path)
  }
}
.e2e_cb_io <- function(rel, seen = new.env(), spent = FALSE, acquire = .e2e_cb_acquire(), ...) {
  seen$all <- character(0)
  local_release_io(rel, acquire = acquire, graphql = .e2e_github(seen, spent), cb_get = .e2e_codeberg(seen, ...))
}
.e2e_cb_first_run <- function(tag, ...) {
  out <- tempfile(paste0("out_", tag)); dir.create(out)
  rel <- tempfile(paste0("rel_", tag)); dir.create(rel)
  seen <- new.env()
  log <- capture.output(suppressMessages(run_update(.e2e_cb_io(rel, seen, ...), out, list(force_full = TRUE))))
  list(out = out, rel = rel, seen = seen, log = log)
}

test_that("the daily run collects a Codeberg repository after GitHub, with no licence, no owner row, and its watchers and size", {
  d <- .e2e_cb_first_run("cb")
  s <- .e2e_summary(d$rel, "sandwich")
  expect_identical(s$repo_id, "codeberg.org/zeileis/sandwich")
  expect_identical(c(s$stars, s$forks, s$issues_open, s$prs_open), c(1L, 1L, 2L, 2L))
  expect_identical(c(s$last_commit_date, s$repo_created_at, s$topics), c("2026-10-11T22:19:58Z", "2023-05-04T08:00:00Z", ""))
  expect_true(is.na(s$license)); expect_true(is.na(s$releases_total)); expect_true(is.na(s$last_release_date))
  expect_equal(.e2e_latest(d$rel, "codeberg.org/zeileis/sandwich")[c("watchers", "size_kb", "issues_closed", "prs_merged", "prs_closed")],
               c(watchers = 1, size_kb = 9104, issues_closed = 1, prs_merged = 1, prs_closed = 1))
  expect_identical(.e2e_read(d$rel, "SELECT node_id, supported, status FROM repos WHERE host = 'codeberg'"),
                   data.frame(node_id = "codeberg:codeberg.org:2371475", supported = 1L, status = "active", stringsAsFactors = FALSE))
  expect_identical(.e2e_read(d$rel, "SELECT repo_id FROM vcs_repo_owner")$repo_id, "github.com/tidyverse/ggplot2")
  expect_identical(.e2e_summary(d$rel, "ggplot2")$stars, 6959L)
  expect_gt(min(grep("^codeberg ", d$seen$all)), max(which(d$seen$all == "github gauges")))
  expect_false(any(grepl("sandwich|zeileis", d$seen$gh)))
  expect_false(any(grepl("@", d$log, fixed = TRUE)))
  expect_true(any(grepl("^Codeberg: 1 located, 0 not located, 0 deferred, 0 not reached, 4 requests, lowest r 1990$", d$log)))
  # Only the fields this repository could have filled: releases are off.
  expect_identical(grep("^Codeberg NA by field", d$log, value = TRUE),
                   "Codeberg NA by field: releases_total 1, last_release_at 1, last_release_tag 1")
  # No address in any table the run published.
  for (f in c("vcs-signals-recent.db", "vcs-signals-summary.db")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(d$rel, f))
    cells <- unlist(lapply(DBI::dbListTables(con), function(t) as.character(unlist(DBI::dbReadTable(con, t)))))
    DBI::dbDisconnect(con)
    expect_false(any(grepl("@", cells, fixed = TRUE)), info = f)
  }
})

test_that("a 403 from Codeberg's commit list keeps every last value of the repository, its commit date among them, and is logged", {
  d <- .e2e_cb_first_run("cb_403")
  expect_identical(.e2e_summary(d$rel, "sandwich")$last_commit_date, "2026-10-11T22:19:58Z")
  routes <- .e2e_cb_routes(stars = 9L)
  routes[["/repos/zeileis/sandwich/commits?limit=1&stat=false&verification=false&files=false"]] <- cb_answer(403L)
  log <- capture.output(suppressMessages(run_update(.e2e_cb_io(d$rel, routes = routes), d$out, list())))
  s <- .e2e_summary(d$rel, "sandwich")
  expect_identical(s$last_commit_date, "2026-10-11T22:19:58Z")
  expect_identical(s$stars, 1L)
  expect_true(any(grepl("^Codeberg /repos/zeileis/sandwich/commits[?].*: HTTP 403, the repository is deferred$", log)))
  expect_true(any(grepl("^Codeberg: 1 located, 0 not located, 1 deferred, 0 not reached", log)))
})

test_that("a GitHub budget below the reserve skips only GitHub, and Codeberg is still collected", {
  d <- .e2e_cb_first_run("cb_spent", spent = TRUE)
  expect_true(any(grepl("^gauges: GitHub skipped", d$log)))
  expect_false(any(grepl("nodes(ids:", d$seen$gh, fixed = TRUE)))
  expect_identical(.e2e_summary(d$rel, "sandwich")$stars, 1L)
})

test_that("a fault in the Codeberg code is logged and leaves GitHub and the publish going, with Codeberg's last values", {
  d <- .e2e_cb_first_run("cb_fault")
  old <- cb_collect_gauges
  assign("cb_collect_gauges", function(...) stop("simulated Codeberg fault"), envir = globalenv())
  withr::defer(assign("cb_collect_gauges", old, envir = globalenv()))
  seen <- new.env()
  io <- .e2e_cb_io(d$rel, seen, routes = .e2e_cb_routes(stars = 9L))
  log <- capture.output(suppressMessages(run_update(io, d$out, list())))
  expect_true(any(grepl("simulated Codeberg fault", log, fixed = TRUE)))
  expect_true(any(grepl("nodes(ids:", seen$gh, fixed = TRUE)))
  expect_true("manifest.json" %in% io$uploaded())
  expect_identical(.e2e_summary(d$rel, "sandwich")$stars, 1L)
  expect_identical(.e2e_summary(d$rel, "ggplot2")$stars, 6959L)

  # A fault while locating keeps them too.
  old_loc <- cb_locate
  assign("cb_locate", function(...) stop("simulated locate fault"), envir = globalenv())
  withr::defer(assign("cb_locate", old_loc, envir = globalenv()))
  seen2 <- new.env()
  io2 <- .e2e_cb_io(d$rel, seen2, routes = .e2e_cb_routes(stars = 9L))
  log2 <- capture.output(suppressMessages(run_update(io2, d$out, list())))
  expect_true(any(grepl("^Codeberg: locating stopped by a fault.*simulated locate fault", log2)))
  expect_true(any(grepl("^Codeberg gauges: collected 0 repos, 1 deferred, 0 not reached", log2)))
  expect_length(seen2$cb, 0L)
  expect_true("manifest.json" %in% io2$uploaded())
  expect_identical(.e2e_summary(d$rel, "sandwich")$stars, 1L)
})

# GitHub, GitLab and Codeberg in one run.
.e2e_three_io <- function(rel, seen = new.env()) {
  seen$all <- character(0)
  acquire <- function() rbind(.e2e_gl_acquire(), .e2e_cb_acquire()()[2, , drop = FALSE])
  local_release_io(rel, acquire = acquire, graphql = .e2e_github(seen), gl_graphql = .e2e_gitlab(seen),
                   cb_get = .e2e_codeberg(seen))
}

test_that("a fault in the Codeberg map query or in its merge into the gauge frame is logged, and GitHub and GitLab still publish", {
  check <- function(tag, fault) {
    out <- tempfile(paste0("out_", tag)); dir.create(out)
    rel <- tempfile(paste0("rel_", tag)); dir.create(rel)
    io <- .e2e_three_io(rel)
    log <- capture.output(res <- suppressMessages(run_update(io, out, list(force_full = TRUE))))
    expect_true(any(grepl(paste0("^Codeberg: stopped by a fault.*", fault), log)), info = tag)
    expect_true("manifest.json" %in% io$uploaded(), info = tag)
    expect_identical(.e2e_summary(rel, "ggplot2")$stars, 6959L, info = tag)
    expect_identical(.e2e_summary(rel, "rurl")$stars, 7L, info = tag)
    expect_identical(.e2e_summary(rel, "inlpubs")$releases_total, 10L, info = tag)
    expect_length(.e2e_latest(rel, "codeberg.org/zeileis/sandwich"), 0L)
  }
  old_node <- is_codeberg_node
  assign("is_codeberg_node", function(...) stop("simulated map fault"), envir = globalenv())
  restored <- FALSE
  withr::defer(if (!restored) assign("is_codeberg_node", old_node, envir = globalenv()))
  check("cb_map_fault", "simulated map fault")
  assign("is_codeberg_node", old_node, envir = globalenv())
  restored <- TRUE

  # A snapshot whose columns do not match makes the rbind into the gauge frame fail.
  old_pass <- .codeberg_pass
  assign(".codeberg_pass", function(io, con, deadline, now, today) list(
    snapshot = data.frame(node_id = "codeberg:codeberg.org:2371475", stars = 1L), deferred = character(0),
    unresolvable = character(0), unreached = character(0),
    map = data.frame(node_id = "codeberg:codeberg.org:2371475", repo_id = "codeberg.org/zeileis/sandwich")),
    envir = globalenv())
  withr::defer(assign(".codeberg_pass", old_pass, envir = globalenv()))
  check("cb_rbind_fault", "columns")
})

test_that("Codeberg gets 15 minutes past GitLab's deadline, and what it does not reach keeps its values", {
  expect_identical(CODEBERG_DEADLINE_EXTRA_MIN, 15)
  routes <- function(stars) c(list("/users/zeileis/repos?limit=50&page=1" = cb_list(list(.e2e_cb_obj(stars),
      cb_obj(2371476L, "zeileis/zoo", stars_count = stars, has_releases = FALSE)))),
    cb_gauge_routes("zeileis/sandwich"), cb_gauge_routes("zeileis/zoo"))
  acquire <- .e2e_cb_acquire(c(zoo = "https://codeberg.org/zeileis/zoo"))
  d <- .e2e_cb_first_run("cb_slow", acquire = acquire, routes = routes(1L))
  expect_identical(c(.e2e_summary(d$rel, "sandwich")$stars, .e2e_summary(d$rel, "zoo")$stars), c(1L, 1L))
  clock <- new.env(); clock$t <- 1e9
  seen <- new.env()
  # GitHub's deadline is the start, GitLab has nothing to do, so Codeberg's deadline is 25 minutes in.
  # Each request takes 400 s: the listing, then one repository's three calls, then the cut, in either order.
  io <- .e2e_cb_io(d$rel, seen, acquire = acquire, routes = routes(9L), clock = clock, cost = 400)
  log <- capture.output(suppressMessages(
    run_update(io, d$out, list(gauge_deadline_min = 0, now = function() clock$t))))
  expect_length(seen$cb, 4L)
  expect_true(any(grepl("1 not reached", log, fixed = TRUE)))
  expect_true("manifest.json" %in% io$uploaded())
  expect_identical(sort(c(.e2e_summary(d$rel, "sandwich")$stars, .e2e_summary(d$rel, "zoo")$stars)), c(1L, 9L))
})

test_that("a day with no Codeberg repository asks Codeberg nothing and needs no Codeberg io", {
  out <- tempfile("out_no_cb"); dir.create(out); rel <- tempfile("rel_no_cb"); dir.create(rel)
  seen <- new.env(); seen$all <- character(0)
  io <- local_release_io(rel, acquire = function() .e2e_cb_acquire()()[1, , drop = FALSE], graphql = .e2e_github(seen))
  log <- capture.output(suppressMessages(run_update(io, out, list(force_full = TRUE))))
  expect_false(any(grepl("Codeberg: .*fault|apply non-function", log)))
  expect_false(any(grepl("^Codeberg", log)))
  expect_true("manifest.json" %in% io$uploaded())
  # With a Codeberg io present, nothing reaches it either.
  out2 <- tempfile("out_no_cb2"); dir.create(out2); rel2 <- tempfile("rel_no_cb2"); dir.create(rel2)
  seen2 <- new.env()
  io2 <- .e2e_cb_io(rel2, seen2, acquire = function() .e2e_cb_acquire()()[1, , drop = FALSE])
  suppressMessages(capture.output(run_update(io2, out2, list(force_full = TRUE))))
  expect_length(seen2$cb, 0L)
})

test_that("two package URLs for one Codeberg repository share one id and one set of requests", {
  routes <- .e2e_cb_routes()
  routes[["/repos/zeileis/sandwich-old"]] <- cb_answer(301L, headers = c(location = "/api/v1/repos/zeileis/sandwich"))
  routes[["/repos/zeileis/sandwich"]] <- cb_answer(body = .e2e_cb_obj())
  d <- .e2e_cb_first_run("cb_two", acquire = .e2e_cb_acquire(c(sandwichold = "https://codeberg.org/zeileis/sandwich-old")),
                         routes = routes)
  ids <- .e2e_read(d$rel, "SELECT node_id FROM repos WHERE host = 'codeberg' ORDER BY repo_id")$node_id
  expect_identical(ids, rep("codeberg:codeberg.org:2371475", 2L))
  expect_identical(sum(grepl("/commits", d$seen$cb)), 1L)
  expect_identical(c(.e2e_summary(d$rel, "sandwich")$stars, .e2e_summary(d$rel, "sandwichold")$stars), c(1L, 1L))
})

test_that("a run by code without the Codeberg pass keeps every Codeberg id and value, and the next run collects again", {
  d <- .e2e_cb_first_run("cb_old")
  old_pass <- .codeberg_pass; old_hosts <- SUPPORTED_HOSTS
  assign(".codeberg_pass", function(io, con, deadline, now, today) list(snapshot = NULL, deferred = character(0),
    unresolvable = character(0), unreached = character(0), map = data.frame(node_id = character(), repo_id = character())),
    envir = globalenv())
  assign("SUPPORTED_HOSTS", c("github", "gitlab"), envir = globalenv())
  restored <- FALSE
  withr::defer(if (!restored) { assign(".codeberg_pass", old_pass, envir = globalenv()); assign("SUPPORTED_HOSTS", old_hosts, envir = globalenv()) })
  suppressMessages(capture.output(run_update(.e2e_cb_io(d$rel), d$out, list())))
  expect_identical(.e2e_summary(d$rel, "sandwich")$stars, 1L)
  expect_identical(.e2e_read(d$rel, "SELECT node_id, supported FROM repos WHERE host = 'codeberg'"),
                   data.frame(node_id = "codeberg:codeberg.org:2371475", supported = 0L, stringsAsFactors = FALSE))
  assign(".codeberg_pass", old_pass, envir = globalenv()); assign("SUPPORTED_HOSTS", old_hosts, envir = globalenv())
  restored <- TRUE
  io <- .e2e_cb_io(d$rel, routes = .e2e_cb_routes(stars = 4L))
  suppressMessages(capture.output(run_update(io, d$out, list())))
  expect_true("manifest.json" %in% io$uploaded())
  expect_identical(.e2e_summary(d$rel, "sandwich")$stars, 4L)
})

test_that("the real daily io reaches Codeberg through cb_get", {
  src <- paste(readLines(file.path(.repo_root, "scripts", "update.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("cb_get\\s*=\\s*cb_get", src))
})

# ---- Bitbucket after Codeberg --------------------------------------------------------
.e2e_bb_acquire <- function() data.frame(package = c("ggplot2", "depower"), origin = "cran",
  url_raw = c("https://github.com/tidyverse/ggplot2", "https://bitbucket.org/bklamer/depower"),
  bugreports_raw = NA, stringsAsFactors = FALSE)
.e2e_bb_routes <- function(by = "path", merged = 16L) {
  obj <- if (by == "path") bb_fields("/repositories/bklamer/depower") else bb_fields(bb_ids_path(NODE))
  c(setNames(list(bb_answer(200L, bb_obj())), obj), bb_routes(merged = bb_count(merged)))
}
# Bitbucket as it answers `routes`; each request is kept in seen$bb and seen$all.
.e2e_bitbucket <- function(seen, routes = .e2e_bb_routes()) {
  fake <- bb_fake(routes); seen$bb <- character(0)
  function(path) { seen$bb <- c(seen$bb, path); seen$all <- c(seen$all, paste("bitbucket", path)); fake(path) }
}
.e2e_bb_io <- function(rel, seen = new.env(), acquire = .e2e_bb_acquire, ...) {
  seen$all <- character(0)
  local_release_io(rel, acquire = acquire, graphql = .e2e_github(seen), bb_get = .e2e_bitbucket(seen, ...))
}
.e2e_bb_first_run <- function(tag) {
  out <- tempfile(paste0("out_", tag)); dir.create(out)
  rel <- tempfile(paste0("rel_", tag)); dir.create(rel)
  seen <- new.env()
  log <- capture.output(suppressMessages(run_update(.e2e_bb_io(rel, seen), out, list(force_full = TRUE))))
  list(out = out, rel = rel, seen = seen, log = log)
}

test_that("the daily run collects a Bitbucket repository after GitHub, writes its points and summary and stores its node id", {
  d <- .e2e_bb_first_run("bb")
  s <- .e2e_summary(d$rel, "depower")
  expect_identical(s$repo_id, "bitbucket.org/bklamer/depower")
  expect_identical(c(s$forks, s$prs_open), c(2L, 1L))
  expect_equal(s$pr_merge_ratio, 84)
  expect_identical(c(s$last_commit_date, s$repo_created_at), c("2026-06-15T02:37:55Z", "2024-02-18T22:04:37Z"))
  expect_true(is.na(s$stars)); expect_true(is.na(s$license)); expect_true(is.na(s$issues_open))
  expect_equal(.e2e_latest(d$rel, "bitbucket.org/bklamer/depower")[c("forks", "prs_open", "prs_merged", "prs_closed", "size_kb")],
               c(forks = 2, prs_open = 1, prs_merged = 16, prs_closed = 3, size_kb = 4631))
  expect_identical(.e2e_read(d$rel, "SELECT node_id, status FROM repos WHERE host = 'bitbucket'"),
                   data.frame(node_id = NODE, status = "active", stringsAsFactors = FALSE))
  expect_gt(min(grep("^bitbucket ", d$seen$all)), max(which(d$seen$all == "github gauges")))
  expect_length(d$seen$bb, 6L)
  expect_false(any(grepl("depower|bklamer", d$seen$gh)))
  expect_true(any(grepl("^Bitbucket: 1 read, 0 not located, 0 deferred, 0 left for later runs, 6 requests$", d$log)))
  expect_true(any(grepl("^Bitbucket gauges: collected 1 repos, 0 deferred, 0 not reached", d$log)))

  # The next run asks by id, and a 500 on the object keeps every value.
  seen <- new.env()
  routes <- .e2e_bb_routes("id", merged = 40L)
  routes[[bb_fields(bb_ids_path(NODE))]] <- bb_answer(500L)
  log <- capture.output(suppressMessages(run_update(.e2e_bb_io(d$rel, seen, routes = routes), d$out, list())))
  expect_identical(seen$bb, bb_fields(bb_ids_path(NODE)))
  expect_identical(.e2e_summary(d$rel, "depower")$forks, 2L)
  expect_equal(.e2e_latest(d$rel, "bitbucket.org/bklamer/depower")[["prs_merged"]], 16)
  expect_true(any(grepl("^Bitbucket: 0 read, 0 not located, 1 deferred", log)))

  # And a later one with every answer reads the new count.
  seen2 <- new.env()
  io <- .e2e_bb_io(d$rel, seen2, routes = .e2e_bb_routes("id", merged = 40L))
  suppressMessages(capture.output(run_update(io, d$out, list())))
  expect_equal(.e2e_latest(d$rel, "bitbucket.org/bklamer/depower")[["prs_merged"]], 40)
})

test_that("a fault in the Bitbucket pass leaves GitHub, GitLab and Codeberg as they were and the run publishes", {
  acquire <- function() rbind(.e2e_gl_acquire(), .e2e_cb_acquire()()[2, , drop = FALSE], .e2e_bb_acquire()[2, , drop = FALSE])
  mk <- function(rel, seen = new.env(), ...) {
    seen$all <- character(0)
    local_release_io(rel, acquire = acquire, graphql = .e2e_github(seen), gl_graphql = .e2e_gitlab(seen),
                     cb_get = .e2e_codeberg(seen), bb_get = .e2e_bitbucket(seen, ...))
  }
  out <- tempfile("out_bb_fault"); dir.create(out)
  rel <- tempfile("rel_bb_fault"); dir.create(rel)
  suppressMessages(capture.output(run_update(mk(rel), out, list(force_full = TRUE))))
  before <- lapply(c("ggplot2", "rurl", "inlpubs", "sandwich", "depower"), function(p) .e2e_summary(rel, p))
  expect_identical(.e2e_summary(rel, "depower")$forks, 2L)
  old <- .bitbucket_pass
  assign(".bitbucket_pass", function(...) stop("simulated Bitbucket fault"), envir = globalenv())
  withr::defer(assign(".bitbucket_pass", old, envir = globalenv()))
  io <- mk(rel, routes = .e2e_bb_routes("id", merged = 40L))
  log <- capture.output(suppressMessages(run_update(io, out, list())))
  expect_true(any(grepl("^Bitbucket: stopped by a fault.*simulated Bitbucket fault", log)))
  expect_true("manifest.json" %in% io$uploaded())
  expect_identical(.e2e_summary(rel, "depower")$forks, 2L)
  expect_identical(.e2e_summary(rel, "ggplot2")$stars, before[[1]]$stars)
  expect_identical(.e2e_summary(rel, "rurl")$stars, before[[2]]$stars)
  expect_identical(.e2e_summary(rel, "sandwich")$stars, before[[4]]$stars)
})

test_that("a day with no Bitbucket repository asks Bitbucket nothing and needs no Bitbucket io", {
  out <- tempfile("out_no_bb"); dir.create(out); rel <- tempfile("rel_no_bb"); dir.create(rel)
  seen <- new.env(); seen$all <- character(0)
  io <- local_release_io(rel, acquire = function() .e2e_bb_acquire()[1, , drop = FALSE], graphql = .e2e_github(seen))
  log <- capture.output(suppressMessages(run_update(io, out, list(force_full = TRUE))))
  expect_false(any(grepl("^Bitbucket", log)))
  expect_true("manifest.json" %in% io$uploaded())
})

test_that("the real daily io reaches Bitbucket through bb_get", {
  src <- paste(readLines(file.path(.repo_root, "scripts", "update.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("bb_get\\s*=\\s*bb_get", src))
})

test_that("two package rows for one Bitbucket repository give one value each and no duplicate key", {
  acquire <- function() data.frame(package = c("ggplot2", "depower", "depower2"), origin = "cran",
    url_raw = c("https://github.com/tidyverse/ggplot2", "https://bitbucket.org/bklamer/depower",
                "https://bitbucket.org/bklamer/depower"), bugreports_raw = NA, stringsAsFactors = FALSE)
  out <- tempfile("out_bb_two"); dir.create(out); rel <- tempfile("rel_bb_two"); dir.create(rel)
  seen <- new.env()
  io <- .e2e_bb_io(rel, seen, acquire = acquire)
  suppressMessages(capture.output(run_update(io, out, list(force_full = TRUE))))
  expect_true("manifest.json" %in% io$uploaded())
  expect_equal(.e2e_summary(rel, "depower")$forks, 2L)
  expect_length(seen$bb, 6L)
  n <- .e2e_read(rel, "SELECT COUNT(*) n FROM series_latest WHERE repo_id LIKE 'bitbucket.org/%' AND metric = 'forks'")$n
  expect_identical(n, 1L)
})
