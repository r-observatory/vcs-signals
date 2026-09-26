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

test_that("run_update carries last_release_date/median_days_between_releases forward for a repo collected again", {
  # last_release_date/median_days_between_releases have no fresh source in the
  # daily gauge snapshot (see update.R's Stage 4 comment above release_facts):
  # they are always read off the PRIOR vcs_signals_summary row and merged back
  # in via a separate rbind(attrs, descriptive_prev) + merge(..., release_facts)
  # step, independent of the descriptive fields (license/topics/is_archived)
  # that prefer this run's fresh attrs. The "floor" test above only exercises
  # a run whose gauges$snapshot is entirely empty, which skips that whole
  # Stage 4 block outright - it never actually runs the rbind/merge wiring.
  # This test seeds a prior published state by hand (instead of via a real
  # first run) so the two release-fact columns start at known, non-NA values,
  # then runs a repo that IS collected again on every pass, proving those two
  # columns survive the summary rebuild instead of being wiped to NA.
  out <- tempfile("out_release_facts"); dir.create(out)
  rel <- tempfile("rel_release_facts"); dir.create(rel)
  repo_id <- repo_slug("github.com", "tidyverse", "ggplot2")

  # Seed series_latest's releases_total at the same value gauges_one.json
  # reports (40), so the fixture's release count never looks "changed" on
  # any run below and no signals_series row is ever written for it; that is
  # what keeps last_release_date's own from-the-window recomputation (see
  # build_signals_summary's release_last_date) at NA every run, so the
  # seeded prior value is what must carry forward, not a value the window
  # happens to recompute today.
  seed_path <- file.path(rel, "vcs-signals-recent.db")
  scon <- DBI::dbConnect(RSQLite::SQLite(), seed_path)
  ensure_repo_schema(scon); ensure_series_schema(scon)
  DBI::dbExecute(scon, "INSERT INTO repos
    (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status)
    VALUES (?,?,?,?,?,?,?,?,?,?,?,?)",
    params = list(repo_id, "R_a", "github", "github.com", "tidyverse", "ggplot2", "tidyverse/ggplot2",
                  1L, 1L, "2020-01-01", "2020-01-01", "active"))
  DBI::dbExecute(scon, "INSERT INTO repo_packages (repo_id,package,origin,resolved_from) VALUES (?,?,?,?)",
    params = list(repo_id, "ggplot2", "cran", "url"))
  DBI::dbExecute(scon, "INSERT INTO series_latest (repo_id,metric,value) VALUES (?,?,?)",
    params = list(repo_id, "releases_total", 40L))
  DBI::dbExecute(scon, "INSERT INTO vcs_signals_summary
    (package,origin,repo_id,last_release_date,median_days_between_releases,first_seen,last_seen)
    VALUES (?,?,?,?,?,?,?)",
    params = list("ggplot2", "cran", repo_id, "2023-05-05", 30L, "2020-01-01", "2020-01-01"))
  DBI::dbDisconnect(scon)
  # A published recent shard always has a manifest beside it, and the publish-time
  # pull now stops rather than carrying on when one is missing.
  writeLines('{"summary":{"years":[]}}', file.path(rel, "manifest.json"))

  acquire_one <- function() data.frame(package = "ggplot2", origin = "cran",
    url_raw = "https://github.com/tidyverse/ggplot2", bugreports_raw = NA, stringsAsFactors = FALSE)
  io <- local_release_io(rel,
    acquire = acquire_one,
    graphql = function(query) {
      if (grepl("rateLimit", query)) return(list(data = list(nodes = list())))
      f <- if (grepl("followRenames", query)) "resolve_one.json"
           else if (grepl("history \\{ totalCount", query)) "commits.json" else "gauges_one.json"
      jsonlite::fromJSON(readLines(file.path("fixtures", f), warn = FALSE), simplifyVector = FALSE)
    })

  # Two passes, mirroring the floor test's two-call shape: the repo is
  # collected successfully both times (unlike the floor test's second call),
  # so Stage 4's rbind/merge carry-forward wiring actually runs on every pass.
  run_update(io, out, list(force_full = TRUE))
  run_update(io, out, list(force_full = FALSE))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-signals-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  facts <- DBI::dbGetQuery(con,
    "SELECT last_release_date, median_days_between_releases FROM vcs_signals_summary WHERE package='ggplot2'")
  stars <- DBI::dbGetQuery(con, "SELECT value FROM series_latest WHERE metric='stars'")$value

  expect_equal(stars, 6959)                              # sanity: really collected fresh, not deferred
  expect_equal(facts$last_release_date, "2023-05-05")
  expect_equal(facts$median_days_between_releases, 30L)
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
