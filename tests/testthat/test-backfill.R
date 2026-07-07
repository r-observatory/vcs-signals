# scripts/backfill.R uses repo-root-relative paths (source("scripts/config.R")
# etc.), same as scripts/update.R (see helper-setup.R), so it must be sourced
# with cwd temporarily chdir'd to the repo root.
.bf_wd <- setwd(.repo_root)
source(file.path(.repo_root, "scripts", "backfill.R"))
setwd(.bf_wd)

test_that("shard_rows partitions 1..n disjointly and covers every row across i=0..N-1", {
  n <- 17; N <- 4
  idx <- lapply(0:(N - 1), function(i) shard_rows(n, i, N))
  expect_equal(sort(unlist(idx)), seq_len(n))              # every row covered exactly once
  for (a in seq_along(idx)) for (b in seq_along(idx)) if (a != b)
    expect_length(intersect(idx[[a]], idx[[b]]), 0)         # pairwise disjoint
})

test_that("shard_rows handles the empty-roster case", {
  expect_equal(shard_rows(0, 0, 4), integer(0))
})

test_that("run_fetch_shard skips a repo whose pagination errors, without failing the whole shard", {
  out_dir <- tempfile("out"); dir.create(out_dir)
  roster <- data.frame(repo_id = c("github.com/a/ok", "github.com/b/bad"),
                       owner = c("a", "b"), name = c("ok", "bad"),
                       stars = c(2L, 5L), done = c(0L, 0L), stringsAsFactors = FALSE)
  roster_path <- file.path(out_dir, "vcs-signals-roster.db")
  write_roster(roster_path, roster)

  io <- list(graphql = function(query) {
    if (grepl('name: "bad"', query, fixed = TRUE)) stop("502")
    list(data = list(repository = list(stargazers = list(
      pageInfo = list(endCursor = NULL, hasNextPage = FALSE),
      edges = list(list(starredAt = "2021-01-01T00:00:00Z"),
                  list(starredAt = "2021-01-02T00:00:00Z"))))))
  })

  shard_path <- run_fetch_shard(io, out_dir, roster_path, 0, 1)
  con <- DBI::dbConnect(RSQLite::SQLite(), shard_path); on.exit(DBI::dbDisconnect(con))
  rows <- DBI::dbGetQuery(con, "SELECT * FROM signals_series")
  expect_equal(unique(rows$repo_id), "github.com/a/ok")     # "bad" repo skipped, not fatal
  rows <- rows[order(rows$date), ]
  expect_equal(rows$value, c(1L, 2L))                       # two distinct days, cumulative 1 then 2
})

test_that("run_merge inserts only historical stars rows, preserving the forward point and series_latest", {
  out_dir <- tempfile("out"); dir.create(out_dir)
  parts_dir <- tempfile("parts"); dir.create(parts_dir)
  released <- tempfile("released"); dir.create(released)

  # The "published" recent shard: one forward stars row for repo R at today,
  # with series_latest embedded exactly the way a real publish() run leaves it.
  today <- format(Sys.Date())
  recent_rows <- data.frame(repo_id = "R", date = today, metric = "stars", value = 42L,
                            stringsAsFactors = FALSE)
  recent_path <- file.path(released, "vcs-signals-recent.db")
  export_series_shard(recent_path, recent_rows)
  rcon <- DBI::dbConnect(RSQLite::SQLite(), recent_path)
  ensure_repo_schema(rcon); ensure_series_schema(rcon)
  DBI::dbExecute(rcon, "INSERT INTO series_latest VALUES ('R','stars',42)")
  DBI::dbDisconnect(rcon)

  jsonlite::write_json(list(summary = list(years = list())),
                        file.path(released, "manifest.json"), auto_unbox = TRUE)

  # A partial shard, as run_fetch_shard would write it: two historical rows,
  # both in a past year distinct from today's.
  hist_rows <- data.frame(repo_id = c("R", "R"), date = c("2018-01-01", "2018-06-01"),
                          metric = c("stars", "stars"), value = c(1L, 3L), stringsAsFactors = FALSE)
  export_series_shard(file.path(parts_dir, "vcs-signals-shard-0.db"), hist_rows)

  io <- list(
    release_exists = function() TRUE,
    download = function(pattern, dir) {
      src <- file.path(released, pattern)
      if (!file.exists(src)) return(FALSE)
      file.copy(src, file.path(dir, pattern), overwrite = TRUE)
      TRUE
    },
    upload = function(path) invisible(NULL))

  run_merge(io, out_dir, parts_dir)

  # Historical rows land in the touched 2018 year shard, untruncated.
  yr_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, "vcs-signals-2018.db"))
  yr_rows <- DBI::dbGetQuery(yr_con, "SELECT date, value FROM signals_series ORDER BY date")
  DBI::dbDisconnect(yr_con)
  expect_equal(yr_rows$date, c("2018-01-01", "2018-06-01"))
  expect_equal(yr_rows$value, c(1L, 3L))

  # The recent shard still carries exactly the forward point, untouched -
  # not overwritten by the backfill and not carrying the (out-of-window)
  # historical rows.
  rec_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, "vcs-signals-recent.db"))
  on.exit(DBI::dbDisconnect(rec_con))
  series <- DBI::dbGetQuery(rec_con, "SELECT date, value FROM signals_series")
  expect_equal(nrow(series), 1)
  expect_equal(series$value[series$date == today], 42)

  latest <- DBI::dbGetQuery(rec_con, "SELECT value FROM series_latest WHERE repo_id='R' AND metric='stars'")
  expect_equal(latest$value, 42)   # series_latest untouched by the merge
})
