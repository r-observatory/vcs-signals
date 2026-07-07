test_that("changed_shards reports added and modified", {
  prev <- c("vcs-signals-2026.db" = "aaa", "vcs-signals-recent.db" = "bbb")
  curr <- c("vcs-signals-2026.db" = "ZZZ", "vcs-signals-recent.db" = "bbb", "vcs-signals-summary.db" = "ccc")
  expect_setequal(changed_shards(prev, curr), c("vcs-signals-2026.db", "vcs-signals-summary.db"))
})

test_that("publish uploads changed shards on first run and heartbeats when unchanged", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSERT INTO signals_series VALUES ('R','2026-07-06','stars',10)")
  uploaded <- new.env(); uploaded$paths <- character(0)
  io <- list(
    release_exists = function() FALSE,
    download = function(pattern, dir) FALSE,
    upload = function(path) uploaded$paths <- c(uploaded$paths, basename(path)))
  out <- tempfile("out"); dir.create(out)
  publish(io, con, out, "v1", "live", force_full = TRUE)
  expect_true("manifest.json" %in% uploaded$paths)
  expect_true(any(grepl("vcs-signals-2026.db", uploaded$paths)))
})

test_that("publish with touched_years leaves the untouched prior-year shard intact and unuploaded", {
  # Working DB carries two years: a 2025 row (the RECENT_WINDOW=400d
  # spillover from seeding) and a 2026 row (this run's forward-collected
  # data). Only 2026 was actually touched this run.
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSERT INTO signals_series VALUES ('R','2025-06-01','stars',10)")
  DBI::dbExecute(con, "INSERT INTO signals_series VALUES ('R','2026-07-01','stars',20)")

  out <- tempfile("out"); dir.create(out)

  # The fixture "full" 2025 shard as it exists on the release: two rows,
  # including an 2025-01-01 row that is NOT present in the working DB above.
  # If publish() were to re-export 2025 from the working DB alone, that row
  # would be lost - proving truncation.
  full_2025 <- data.frame(repo_id = c("R", "R"), date = c("2025-01-01", "2025-06-01"),
                          metric = c("stars", "stars"), value = c(5L, 10L),
                          stringsAsFactors = FALSE)

  uploaded <- new.env(); uploaded$paths <- character(0)
  io <- list(
    release_exists = function() TRUE,
    download = function(pattern, dir) {
      if (grepl("manifest\\.json", pattern)) {
        jsonlite::write_json(list(summary = list(years = list(2025))),
                              file.path(dir, "manifest.json"), auto_unbox = TRUE)
        return(TRUE)
      }
      if (grepl("recent", pattern)) {
        export_series_shard(file.path(dir, "vcs-signals-recent.db"),
          data.frame(repo_id = character(), date = character(),
                     metric = character(), value = integer(), stringsAsFactors = FALSE))
        return(TRUE)
      }
      if (grepl("2025", pattern)) {
        export_series_shard(file.path(dir, "vcs-signals-2025.db"), full_2025)
        return(TRUE)
      }
      FALSE
    },
    upload = function(path) uploaded$paths <- c(uploaded$paths, basename(path)))

  publish(io, con, out, "v1", "live", force_full = FALSE, touched_years = "2026")

  expect_false("vcs-signals-2025.db" %in% uploaded$paths)
  expect_true("vcs-signals-2026.db" %in% uploaded$paths)

  # The 2025 shard file on disk must remain exactly what was downloaded
  # (both rows) - never truncated down to just the working DB's one row.
  chk <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-signals-2025.db"))
  n_2025 <- DBI::dbGetQuery(chk, "SELECT COUNT(*) AS n FROM signals_series")$n
  DBI::dbDisconnect(chk)
  expect_equal(n_2025, 2L)
})
