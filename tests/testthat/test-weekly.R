# scripts/weekly.R uses repo-root-relative paths (source("scripts/config.R")
# etc.), same as scripts/backfill.R (see helper-setup.R), so it must be
# sourced with cwd temporarily chdir'd to the repo root.
.wk_wd <- setwd(.repo_root)
source(file.path(.repo_root, "scripts", "weekly.R"))
setwd(.wk_wd)

# ---- Task 1: batched commit-count query + parser ---------------------------
test_that("build_commits_batched_query aliases one repository block per repo asking for history totalCount", {
  repos <- data.frame(repo_id = c("github.com/a/one", "github.com/b/two"),
                      owner = c("a", "b"), name = c("one", "two"), stringsAsFactors = FALSE)
  q <- build_commits_batched_query(repos)
  expect_match(q, 'r0: repository\\(owner: "a", name: "one"\\)')
  expect_match(q, 'r1: repository\\(owner: "b", name: "two"\\)')
  expect_match(q, "history \\{ totalCount \\}")
})

test_that("fetch_commit_counts maps aliased counts back to repo_id, NA for a null default branch", {
  repos <- data.frame(repo_id = c("github.com/a/one", "github.com/b/two"),
                      owner = c("a", "b"), name = c("one", "two"), stringsAsFactors = FALSE)
  io <- list(graphql = function(query) list(data = list(
    r0 = list(defaultBranchRef = list(target = list(history = list(totalCount = 6200)))),
    r1 = list(defaultBranchRef = NULL))))
  out <- fetch_commit_counts(io, repos)
  expect_equal(out[["github.com/a/one"]], 6200L)
  expect_true(is.na(out[["github.com/b/two"]]))
})

test_that("fetch_commit_counts errors on a transport-level GraphQL error, letting the caller isolate the chunk", {
  repos <- data.frame(repo_id = "github.com/a/one", owner = "a", name = "one", stringsAsFactors = FALSE)
  io <- list(graphql = function(query) list(data = NULL, errors = list(list(message = "boom"))))
  expect_error(fetch_commit_counts(io, repos))
})

# ---- Task 2: contributor-count Link-header parsing --------------------------
test_that("parse_contributor_link_count reads the rel=\"last\" page as the count", {
  headers <- c("HTTP/2.0 200 OK", "content-type: application/json",
    'link: <https://api.github.com/repositories/1/contributors?per_page=1&anon=true&page=2>; rel="next", <https://api.github.com/repositories/1/contributors?per_page=1&anon=true&page=45>; rel="last"')
  expect_equal(parse_contributor_link_count(headers, 1), 45L)
})

test_that("parse_contributor_link_count falls back to body length with no Link header", {
  expect_equal(parse_contributor_link_count(character(0), 1), 1L)   # one-item, no Link
  expect_equal(parse_contributor_link_count(character(0), 0), 0L)   # empty body
})

# ---- Task 3: run_fetch_shard -------------------------------------------------
test_that("run_fetch_shard writes commits_total and contributors_total, NA where a fetch failed", {
  out_dir <- tempfile("out"); dir.create(out_dir)
  roster <- data.frame(repo_id = c("github.com/a/ok", "github.com/b/bad"),
                       owner = c("a", "b"), name = c("ok", "bad"),
                       stars = c(2L, 5L), done = c(0L, 0L), stringsAsFactors = FALSE)
  roster_path <- file.path(out_dir, "vcs-signals-roster.db")
  write_roster(roster_path, roster)

  io <- list(
    graphql = function(query) list(data = list(
      r0 = list(defaultBranchRef = list(target = list(history = list(totalCount = 100)))),
      r1 = list(defaultBranchRef = list(target = list(history = list(totalCount = 50)))))),
    contributors = function(owner, name) {
      if (identical(name, "bad")) stop("404")
      7L
    })

  shard_path <- run_fetch_shard(io, out_dir, roster_path, 0, 1, commit_delay = 0, contributor_delay = 0)
  con <- DBI::dbConnect(RSQLite::SQLite(), shard_path); on.exit(DBI::dbDisconnect(con))
  rows <- DBI::dbGetQuery(con, "SELECT * FROM snapshot ORDER BY repo_id")

  ok <- rows[rows$repo_id == "github.com/a/ok", ]
  bad <- rows[rows$repo_id == "github.com/b/bad", ]
  expect_equal(ok$commits_total, 100L); expect_equal(ok$contributors_total, 7L)
  expect_equal(bad$commits_total, 50L)          # commit fetch ok even though contributors failed
  expect_true(is.na(bad$contributors_total))
})

test_that("run_fetch_shard leaves commits_total NA for a whole failed batched chunk without aborting the shard", {
  out_dir <- tempfile("out"); dir.create(out_dir)
  roster <- data.frame(repo_id = "github.com/a/ok", owner = "a", name = "ok",
                       stars = 1L, done = 0L, stringsAsFactors = FALSE)
  roster_path <- file.path(out_dir, "vcs-signals-roster.db")
  write_roster(roster_path, roster)

  io <- list(graphql = function(query) stop("502"), contributors = function(owner, name) 3L)

  shard_path <- run_fetch_shard(io, out_dir, roster_path, 0, 1, commit_delay = 0, contributor_delay = 0)
  con <- DBI::dbConnect(RSQLite::SQLite(), shard_path); on.exit(DBI::dbDisconnect(con))
  rows <- DBI::dbGetQuery(con, "SELECT * FROM snapshot")
  expect_true(is.na(rows$commits_total))
  expect_equal(rows$contributors_total, 3L)
})

# ---- Task 3: run_merge -------------------------------------------------------
.weekly_merge_fixture <- function(released_dir, today) {
  repos_df <- data.frame(repo_id = "github.com/a/ok", node_id = "R_1", host = "github",
    host_domain = "github.com", owner = "a", name = "ok", name_with_owner = "a/ok",
    supported = 1L, n_packages = 1L, first_seen = "2026-01-01", last_seen = today,
    status = "active", stringsAsFactors = FALSE)
  rp_df <- data.frame(repo_id = "github.com/a/ok", package = "pkgA", origin = "cran",
    resolved_from = "url", stringsAsFactors = FALSE)
  summary_df <- data.frame(package = "pkgA", origin = "cran", repo_id = "github.com/a/ok",
    stars = 42L, forks = 3L, issues_open = 0L, prs_open = 0L, commits_total = NA_integer_,
    releases_total = 1L, last_commit_date = NA_character_, license = "MIT", topics = "r",
    is_archived = 0L, trend_30d = NA_real_, first_seen = "2026-01-01", last_seen = today,
    stringsAsFactors = FALSE)

  recent_path <- file.path(released_dir, "vcs-signals-recent.db")
  export_series_shard(recent_path,
    data.frame(repo_id = "github.com/a/ok", date = today, metric = "stars", value = 42L,
              stringsAsFactors = FALSE))
  rcon <- DBI::dbConnect(RSQLite::SQLite(), recent_path)
  ensure_repo_schema(rcon); ensure_series_schema(rcon)
  DBI::dbExecute(rcon, "INSERT INTO series_latest VALUES ('github.com/a/ok','stars',42)")
  DBI::dbWriteTable(rcon, "repos", repos_df, append = TRUE)
  DBI::dbWriteTable(rcon, "repo_packages", rp_df, append = TRUE)
  DBI::dbWriteTable(rcon, "vcs_signals_summary", summary_df, append = TRUE)
  DBI::dbDisconnect(rcon)

  jsonlite::write_json(list(summary = list(years = list())),
                        file.path(released_dir, "manifest.json"), auto_unbox = TRUE)
}

.weekly_merge_io <- function(released_dir) {
  list(
    release_exists = function() TRUE,
    download = function(pattern, dir) {
      src <- file.path(released_dir, pattern)
      if (!file.exists(src)) return(FALSE)
      file.copy(src, file.path(dir, pattern), overwrite = TRUE)
      TRUE
    },
    upload = function(path) invisible(NULL))
}

test_that("run_merge appends today's commits_total/contributors_total change-only rows and leaves stars untouched", {
  out_dir <- tempfile("out"); dir.create(out_dir)
  parts_dir <- tempfile("parts"); dir.create(parts_dir)
  released <- tempfile("released"); dir.create(released)
  today <- format(Sys.Date())

  .weekly_merge_fixture(released, today)
  export_snapshot_shard(file.path(parts_dir, "vcs-signals-shard-0.db"),
    data.frame(repo_id = "github.com/a/ok", commits_total = 500L, contributors_total = 10L,
              stringsAsFactors = FALSE))

  run_merge(.weekly_merge_io(released), out_dir, parts_dir)

  rec_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, "vcs-signals-recent.db"))
  on.exit(DBI::dbDisconnect(rec_con))
  series <- DBI::dbGetQuery(rec_con, "SELECT date, metric, value FROM signals_series ORDER BY metric")
  expect_equal(nrow(series), 3)   # stars (untouched) + the two new weekly metrics
  expect_equal(series$value[series$metric == "stars"], 42L)
  expect_equal(series$value[series$metric == "commits_total" & series$date == today], 500L)
  expect_equal(series$value[series$metric == "contributors_total" & series$date == today], 10L)

  latest <- DBI::dbGetQuery(rec_con, "SELECT metric, value FROM series_latest ORDER BY metric")
  expect_equal(latest$value[latest$metric == "stars"], 42L)             # daily metric untouched
  expect_equal(latest$value[latest$metric == "commits_total"], 500L)
  expect_equal(latest$value[latest$metric == "contributors_total"], 10L)

  summ <- DBI::dbGetQuery(rec_con, "SELECT commits_total FROM vcs_signals_summary WHERE package='pkgA'")
  expect_equal(summ$commits_total, 500L)
})

test_that("run_merge is change-only: an unchanged weekly value on a second run adds no new series row", {
  out_dir1 <- tempfile("out"); dir.create(out_dir1)
  parts_dir1 <- tempfile("parts"); dir.create(parts_dir1)
  released <- tempfile("released"); dir.create(released)
  today <- format(Sys.Date())

  .weekly_merge_fixture(released, today)
  export_snapshot_shard(file.path(parts_dir1, "vcs-signals-shard-0.db"),
    data.frame(repo_id = "github.com/a/ok", commits_total = 500L, contributors_total = 10L,
              stringsAsFactors = FALSE))
  run_merge(.weekly_merge_io(released), out_dir1, parts_dir1)

  # Second run: same repo, same values, against the release the first run
  # just published (out_dir1 stands in for "released" this time).
  out_dir2 <- tempfile("out2"); dir.create(out_dir2)
  parts_dir2 <- tempfile("parts2"); dir.create(parts_dir2)
  export_snapshot_shard(file.path(parts_dir2, "vcs-signals-shard-0.db"),
    data.frame(repo_id = "github.com/a/ok", commits_total = 500L, contributors_total = 10L,
              stringsAsFactors = FALSE))
  run_merge(.weekly_merge_io(out_dir1), out_dir2, parts_dir2)

  rec_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir2, "vcs-signals-recent.db"))
  on.exit(DBI::dbDisconnect(rec_con))
  series <- DBI::dbGetQuery(rec_con,
    "SELECT * FROM signals_series WHERE metric IN ('commits_total','contributors_total')")
  expect_equal(nrow(series), 2)   # still just the one row each from run 1, none added by run 2
})
