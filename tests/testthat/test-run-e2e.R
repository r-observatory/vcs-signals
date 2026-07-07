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
