test_that("run_update resolves, collects, materializes, and publishes with a fake io", {
  out <- tempfile("out"); dir.create(out)
  # fake io: SP1 acquisition returns a fixed 1-repo input; graphql returns fixtures;
  # release IO records uploads.
  uploaded <- new.env(); uploaded$paths <- character(0)
  io <- list(
    acquire = function() data.frame(package = "ggplot2", origin = "cran",
      url_raw = "https://github.com/tidyverse/ggplot2", bugreports_raw = NA, stringsAsFactors = FALSE),
    graphql = function(query) {
      f <- if (grepl("followRenames", query)) "resolve_one.json"
           else if (grepl("history \\{ totalCount", query)) "commits.json" else "gauges_one.json"
      jsonlite::fromJSON(readLines(file.path("fixtures", f), warn = FALSE), simplifyVector = FALSE)
    },
    release_exists = function() FALSE, download = function(pattern, dir) FALSE,
    upload = function(path) uploaded$paths <- c(uploaded$paths, basename(path)))
  run_update(io, out, list(force_full = TRUE))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-signals-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  expect_true(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM signals_series")$n >= 1)
  expect_equal(DBI::dbGetQuery(con, "SELECT value FROM series_latest WHERE metric='stars'")$value, 6959)
  expect_true(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM vcs_signals_summary WHERE package='ggplot2'")$n == 1)
  expect_true("manifest.json" %in% uploaded$paths)
})

test_that("run_update floor: a total collection failure on a later run leaves series_latest and summary unchanged", {
  # Same out_dir reused across both calls (mirrors a real daily run: the
  # second call's release_exists/download read back exactly what the first
  # call's publish() wrote directly into out_dir, so a no-op download that
  # just reports "yes, it's there" is a faithful fake for this repo/io
  # contract without needing a separate fake-remote directory).
  out <- tempfile("out_floor"); dir.create(out)
  acquire_one <- function() data.frame(package = "ggplot2", origin = "cran",
    url_raw = "https://github.com/tidyverse/ggplot2", bugreports_raw = NA, stringsAsFactors = FALSE)

  io1 <- list(
    acquire = acquire_one,
    graphql = function(query) {
      if (grepl("rateLimit", query)) return(list(data = list(nodes = list())))
      f <- if (grepl("followRenames", query)) "resolve_one.json"
           else if (grepl("history \\{ totalCount", query)) "commits.json" else "gauges_one.json"
      jsonlite::fromJSON(readLines(file.path("fixtures", f), warn = FALSE), simplifyVector = FALSE)
    },
    release_exists = function() FALSE, download = function(pattern, dir) FALSE,
    upload = function(path) invisible(NULL))
  run_update(io1, out, list(force_full = TRUE))

  con1 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-signals-recent.db"))
  stars1 <- DBI::dbGetQuery(con1, "SELECT value FROM series_latest WHERE metric='stars'")$value
  summary1 <- DBI::dbGetQuery(con1, "SELECT stars, license, is_archived FROM vcs_signals_summary WHERE package='ggplot2'")
  DBI::dbDisconnect(con1)
  expect_equal(stars1, 6959)   # sanity: run 1 actually collected

  # Run 2: rateLimit preflight reports unlimited (so I3's preflight does not
  # itself explain an empty run), but every gauge/commit query errors, so
  # stage-3 collection returns nothing at all.
  io2 <- list(
    acquire = acquire_one,
    graphql = function(query) {
      if (grepl("rateLimit", query)) return(list(data = list(nodes = list())))
      list(data = NULL, errors = list(list(message = "SERVICE_UNAVAILABLE")))
    },
    release_exists = function() TRUE, download = function(pattern, dir) TRUE,
    upload = function(path) invisible(NULL))
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
  repo_id <- repo_slug("github.com", "tidyverse", "ggplot2")

  # Seed series_latest's releases_total at the same value gauges_one.json
  # reports (40), so the fixture's release count never looks "changed" on
  # any run below and no signals_series row is ever written for it; that is
  # what keeps last_release_date's own from-the-window recomputation (see
  # build_signals_summary's release_last_date) at NA every run, so the
  # seeded prior value is what must carry forward, not a value the window
  # happens to recompute today.
  seed_path <- file.path(out, "vcs-signals-recent.db")
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

  acquire_one <- function() data.frame(package = "ggplot2", origin = "cran",
    url_raw = "https://github.com/tidyverse/ggplot2", bugreports_raw = NA, stringsAsFactors = FALSE)
  io <- list(
    acquire = acquire_one,
    graphql = function(query) {
      if (grepl("rateLimit", query)) return(list(data = list(nodes = list())))
      f <- if (grepl("followRenames", query)) "resolve_one.json"
           else if (grepl("history \\{ totalCount", query)) "commits.json" else "gauges_one.json"
      jsonlite::fromJSON(readLines(file.path("fixtures", f), warn = FALSE), simplifyVector = FALSE)
    },
    release_exists = function() TRUE, download = function(pattern, dir) TRUE,
    upload = function(path) invisible(NULL))

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
