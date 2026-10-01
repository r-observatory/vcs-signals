# The top contributor's commit count and account type, read from the body of the
# weekly contributors call that was read only for its Link header before.

.tc_body <- function(txt) jsonlite::fromJSON(txt, simplifyVector = FALSE)
.tc_none <- list(top_commits = NA_integer_, top_type = NA_character_)

test_that("parse_contributor_top reads the top contributor's commits and type", {
  user <- .tc_body('[{"login":"gaborcsardi","id":660288,"type":"User","site_admin":false,"contributions":1537}]')
  expect_equal(parse_contributor_top(user), list(top_commits = 1537L, top_type = "User"))
  bot <- .tc_body('[{"login":"github-actions[bot]","type":"Bot","contributions":4210}]')
  expect_equal(parse_contributor_top(bot), list(top_commits = 4210L, top_type = "Bot"))
  anon <- .tc_body('[{"email":"someone@example.org","name":"Someone","type":"Anonymous","contributions":88}]')
  expect_equal(parse_contributor_top(anon), list(top_commits = 88L, top_type = "Anonymous"))
})

test_that("an empty body, an error object or a missing count has no top contributor count", {
  expect_equal(parse_contributor_top(.tc_body("[]")), .tc_none)
  expect_equal(parse_contributor_top(.tc_body(paste0('{"message":"The history or contributor list is too ',
                                                     'large to list contributors for this repository via the API."}'))),
               .tc_none)
  expect_equal(parse_contributor_top(NULL), .tc_none)
  expect_equal(parse_contributor_top(.tc_body('[{"login":"x","type":"User"}]')),
               list(top_commits = NA_integer_, top_type = "User"))
})

test_that("the bot flag is 1 for a bot account, 0 for a user account, and unknown otherwise", {
  expect_identical(contributor_bot_flag("Bot"), 1L)
  expect_identical(contributor_bot_flag("User"), 0L)
  expect_identical(contributor_bot_flag("Anonymous"), NA_integer_)
  expect_identical(contributor_bot_flag("Organization"), NA_integer_)
  expect_identical(contributor_bot_flag(NA_character_), NA_integer_)
})

.tc_weekly <- local({
  env <- new.env(parent = globalenv())
  withr::with_dir(.repo_root, sys.source(file.path("scripts", "weekly.R"), envir = env))
  env
})

test_that("both are weekly metrics, named within the viewer's metric width", {
  expect_true(all(c("top_contributor_commits", "top_contributor_bot") %in% WEEKLY_METRICS))
  expect_true(all(nchar(WEEKLY_METRICS) <= 64L))
})

test_that("a snapshot written without the new metrics stores them as NA", {
  p <- tempfile(fileext = ".db")
  .tc_weekly$export_snapshot_shard(p, data.frame(repo_id = "r", commits_total = 1L,
                                                 contributors_total = 2L, stringsAsFactors = FALSE))
  con <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(con))
  row <- DBI::dbReadTable(con, "snapshot")
  expect_true(all(WEEKLY_METRICS %in% names(row)))
  expect_true(is.na(row$top_contributor_commits))
  expect_true(is.na(row$top_contributor_bot))
})

test_that("run_fetch_shard stores the top contributor's commits and bot flag beside the count", {
  out_dir <- tempfile("tc_out_"); dir.create(out_dir)
  roster <- data.frame(repo_id = c("github.com/a/one", "github.com/b/two", "github.com/c/old"),
                       owner = c("a", "b", "c"), name = c("one", "two", "old"),
                       stars = 1L, done = 0L, stringsAsFactors = FALSE)
  roster_path <- file.path(out_dir, "vcs-signals-roster.db")
  write_roster(roster_path, roster)
  replies <- list(one = list(count = 45L, top_commits = 1537L, top_type = "User"),
                  two = list(count = 3L, top_commits = 900L, top_type = "Bot"),
                  old = 7L)   # a bare count, the older reply shape
  io <- list(
    graphql = function(query) list(data = list(
      r0 = list(defaultBranchRef = list(target = list(history = list(totalCount = 100)))),
      r1 = list(defaultBranchRef = list(target = list(history = list(totalCount = 50)))),
      r2 = list(defaultBranchRef = list(target = list(history = list(totalCount = 10)))))),
    contributors = function(owner, name) replies[[name]])
  shard <- suppressMessages(.tc_weekly$run_fetch_shard(io, out_dir, roster_path, 0, 1,
                                                       commit_delay = 0, contributor_delay = 0))
  con <- DBI::dbConnect(RSQLite::SQLite(), shard); on.exit(DBI::dbDisconnect(con))
  rows <- DBI::dbGetQuery(con, "SELECT * FROM snapshot ORDER BY repo_id")
  expect_equal(rows$contributors_total, c(45L, 3L, 7L))
  expect_equal(rows$top_contributor_commits, c(1537L, 900L, NA))
  expect_equal(rows$top_contributor_bot, c(0L, 1L, NA))
})

.tc_rid <- "github.com/a/ok"
.tc_weeks_ago <- function(n) format(Sys.Date() - 7L * n)
.tc_point <- function(weeks_ago, metric, value)
  data.frame(repo_id = .tc_rid, date = .tc_weeks_ago(weeks_ago), metric = metric, value = value,
             stringsAsFactors = FALSE)
# A release as earlier weekly runs left it: `points` are their series rows and
# `latest` the values they last stored, named by metric.
.tc_release <- function(points = NULL, latest = integer(0)) {
  rel <- tempfile("tc_rel_"); dir.create(rel)
  today <- format(Sys.Date())
  recent <- file.path(rel, "vcs-signals-recent.db")
  export_series_shard(recent, rbind(data.frame(repo_id = .tc_rid, date = today, metric = "stars",
                                               value = 42L, stringsAsFactors = FALSE), points))
  con <- DBI::dbConnect(RSQLite::SQLite(), recent); on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "INSERT INTO series_latest VALUES (?, 'stars', 42)", params = list(.tc_rid))
  for (m in names(latest))
    DBI::dbExecute(con, "INSERT INTO series_latest VALUES (?, ?, ?)", params = list(.tc_rid, m, latest[[m]]))
  DBI::dbWriteTable(con, "repos", data.frame(repo_id = .tc_rid, node_id = "R_1", host = "github",
    host_domain = "github.com", owner = "a", name = "ok", name_with_owner = "a/ok", supported = 1L,
    n_packages = 1L, first_seen = "2026-01-01", last_seen = today, status = "active",
    stringsAsFactors = FALSE), append = TRUE)
  DBI::dbWriteTable(con, "repo_packages", data.frame(repo_id = .tc_rid, package = "pkgA",
    origin = "cran", resolved_from = "url", stringsAsFactors = FALSE), append = TRUE)
  jsonlite::write_json(list(summary = list(years = list())), file.path(rel, "manifest.json"),
                       auto_unbox = TRUE)
  rel
}
.tc_parts <- function(commits, bot) {
  p <- tempfile("tc_parts_"); dir.create(p)
  .tc_weekly$export_snapshot_shard(file.path(p, "vcs-signals-shard-0.db"), data.frame(
    repo_id = .tc_rid, commits_total = 500L, contributors_total = 10L,
    top_contributor_commits = commits, top_contributor_bot = bot, stringsAsFactors = FALSE))
  p
}
.tc_published <- function(rel) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  list(series = DBI::dbGetQuery(con, "SELECT metric, date, value FROM signals_series
                                       WHERE metric LIKE 'top_contributor%' ORDER BY metric, date"),
       latest = DBI::dbGetQuery(con, "SELECT metric, value FROM series_latest
                                       WHERE metric LIKE 'top_contributor%' ORDER BY metric"),
       summary = DBI::dbGetQuery(con, "SELECT top_contributor_commits, top_contributor_bot
                                        FROM vcs_signals_summary"))
}
.tc_merge <- function(rel, parts)
  suppressMessages(.tc_weekly$run_merge(local_release_io(rel), tempfile("tc_m_"), parts))
# The shard a weekly fetch writes for the one repository when the contributors call answers `reply`.
.tc_fetched <- function(reply) {
  out_dir <- tempfile("tc_fetch_"); dir.create(out_dir)
  roster_path <- file.path(out_dir, "vcs-signals-roster.db")
  write_roster(roster_path, data.frame(repo_id = .tc_rid, owner = "a", name = "ok", stars = 1L,
                                       done = 0L, stringsAsFactors = FALSE))
  io <- list(
    graphql = function(query) list(data = list(
      r0 = list(defaultBranchRef = list(target = list(history = list(totalCount = 500)))))),
    contributors = function(owner, name) reply)
  suppressMessages(.tc_weekly$run_fetch_shard(io, out_dir, roster_path, 0, 1,
                                              commit_delay = 0, contributor_delay = 0))
  out_dir
}
# Last week's published state: the top contributor had 1537 commits and the given flag.
.tc_last_week <- function(bot) .tc_release(
  points = rbind(.tc_point(1, "top_contributor_commits", 1537L), .tc_point(1, "top_contributor_bot", bot)),
  latest = c(top_contributor_commits = 1537L, top_contributor_bot = bot))
.tc_flag_points <- function(got) got$series[got$series$metric == "top_contributor_bot", ]

test_that("the weekly merge writes both metrics change-only and the summary carries them", {
  rel <- .tc_release()
  suppressMessages(.tc_weekly$run_merge(local_release_io(rel), tempfile("tc_m1_"), .tc_parts(1537L, 0L)))
  suppressMessages(.tc_weekly$run_merge(local_release_io(rel), tempfile("tc_m2_"), .tc_parts(1537L, 0L)))
  got <- .tc_published(rel)
  expect_equal(got$series$metric, c("top_contributor_bot", "top_contributor_commits"))
  expect_equal(got$summary$top_contributor_commits, 1537L)
  expect_equal(got$summary$top_contributor_bot, 0L)
  # The top committer turns out to be a bot account: the flag moves, the count does not.
  suppressMessages(.tc_weekly$run_merge(local_release_io(rel), tempfile("tc_m3_"), .tc_parts(1537L, 1L)))
  got <- .tc_published(rel)
  expect_equal(got$series$value[got$series$metric == "top_contributor_bot"], 1L)
  expect_equal(got$summary$top_contributor_bot, 1L)
  expect_equal(sum(got$series$metric == "top_contributor_commits"), 1L)
})

test_that("untyped_top_contributors names the rows with commits and no flag", {
  snap <- data.frame(repo_id = c("user", "bot", "anon", "unread"),
                     top_contributor_commits = c(10L, 20L, 30L, NA),
                     top_contributor_bot = c(0L, 1L, NA, NA), stringsAsFactors = FALSE)
  expect_equal(.tc_weekly$untyped_top_contributors(snap), "anon")
  expect_equal(.tc_weekly$untyped_top_contributors(snap[0, ]), character(0))
  # A shard written before the two metrics existed has neither column.
  expect_equal(.tc_weekly$untyped_top_contributors(snap["repo_id"]), character(0))
})

test_that("a user account that gives way to an anonymous top contributor leaves no flag", {
  rel <- .tc_last_week(bot = 0L)
  .tc_merge(rel, .tc_fetched(list(count = 12L, top_commits = 1600L, top_type = "Anonymous")))
  got <- .tc_published(rel)
  expect_equal(got$summary$top_contributor_commits, 1600L)
  expect_true(is.na(got$summary$top_contributor_bot))
  expect_equal(got$latest$metric, "top_contributor_commits")
  expect_equal(got$latest$value, 1600L)
  # The series cannot hold an empty value: last week's point stays and this week adds none.
  expect_equal(.tc_flag_points(got)$date, .tc_weeks_ago(1))
  expect_equal(.tc_flag_points(got)$value, 0L)
})

test_that("a bot account that gives way to an anonymous top contributor leaves no flag", {
  rel <- .tc_last_week(bot = 1L)
  .tc_merge(rel, .tc_fetched(list(count = 12L, top_commits = 1600L, top_type = "Anonymous")))
  got <- .tc_published(rel)
  expect_equal(got$summary$top_contributor_commits, 1600L)
  expect_true(is.na(got$summary$top_contributor_bot))
  expect_equal(got$latest$metric, "top_contributor_commits")
  expect_equal(.tc_flag_points(got)$date, .tc_weeks_ago(1))
})

test_that("an account type that is neither Bot nor User leaves no flag", {
  for (type in list("Organization", NULL)) {
    rel <- .tc_last_week(bot = 0L)
    .tc_merge(rel, .tc_fetched(list(count = 12L, top_commits = 1600L, top_type = type)))
    got <- .tc_published(rel)
    expect_equal(got$summary$top_contributor_commits, 1600L)
    expect_true(is.na(got$summary$top_contributor_bot))
    expect_equal(got$latest$metric, "top_contributor_commits")
  }
})

test_that("a user account that follows an anonymous top contributor writes the flag again", {
  # Two weeks ago a user account, last week anonymous, so no flag is stored.
  rel <- .tc_release(
    points = rbind(.tc_point(2, "top_contributor_commits", 1537L), .tc_point(2, "top_contributor_bot", 0L),
                   .tc_point(1, "top_contributor_commits", 1600L)),
    latest = c(top_contributor_commits = 1600L))
  .tc_merge(rel, .tc_fetched(list(count = 12L, top_commits = 1650L, top_type = "User")))
  got <- .tc_published(rel)
  expect_equal(got$summary$top_contributor_commits, 1650L)
  expect_equal(got$summary$top_contributor_bot, 0L)
  expect_equal(got$latest$value[got$latest$metric == "top_contributor_bot"], 0L)
  expect_equal(.tc_flag_points(got)$date, c(.tc_weeks_ago(2), .tc_weeks_ago(0)))
  expect_equal(.tc_flag_points(got)$value, c(0L, 0L))
})

test_that("a contributors read that fails keeps the stored commits and flag", {
  rel <- .tc_last_week(bot = 1L)
  .tc_merge(rel, .tc_fetched(NULL))
  got <- .tc_published(rel)
  expect_equal(got$summary$top_contributor_commits, 1537L)
  expect_equal(got$summary$top_contributor_bot, 1L)
  expect_equal(got$latest$metric, c("top_contributor_bot", "top_contributor_commits"))
})
