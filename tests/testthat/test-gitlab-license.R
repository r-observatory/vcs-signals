# gitlab_license: each GitLab repository's licence as the weekly job last read it. GitLab's
# gauge query has no licence, so every summary build takes a GitLab row's licence from here.

# weekly.R, backfill.R and ai_backfill.R all define run_merge, so each is loaded into its own environment.
.gll_env <- function(script) {
  env <- new.env(parent = globalenv())
  withr::with_dir(.repo_root, sys.source(file.path("scripts", script), envir = env))
  env
}
.gll_weekly <- .gll_env("weekly.R")
.gll_ai <- .gll_env("ai_backfill.R")
.gll_backfill <- .gll_env("backfill.R")
.gll_rurl <- "gitlab.com/bart-turczynski/rurl"

# ggplot2 on GitHub, whose gauges carry MIT, and rurl on gitlab.com, whose gauges carry no licence
# and `stars` stars.
.gll_io <- function(rel, stars = 7L) local_release_io(rel,
  acquire = function() data.frame(package = c("ggplot2", "rurl"), origin = "cran",
    url_raw = c("https://github.com/tidyverse/ggplot2", "https://gitlab.com/bart-turczynski/rurl"),
    bugreports_raw = NA, stringsAsFactors = FALSE),
  graphql = function(query) {
    f <- if (grepl("followRenames", query)) "resolve_one.json"
         else if (grepl("history \\{ totalCount", query)) "commits.json" else "gauges_one.json"
    jsonlite::fromJSON(file.path("fixtures", f), simplifyVector = FALSE)
  },
  gl_graphql = function(domain, query, variables) {
    keys <- tolower(unlist(variables$keys))
    nodes <- jsonlite::fromJSON(file.path("fixtures", "gitlab", "gitlab_com.json"),
                                simplifyVector = FALSE)$data$projects$nodes
    hit <- Filter(function(n) tolower(n$fullPath) %in% keys || tolower(n$id) %in% keys, nodes)
    hit <- lapply(hit, function(n) { n$starCount <- stars; n })
    list(data = list(projects = list(nodes = hit)), errors = NULL, status = 200L, error = NA_character_)
  })

# The GitLab job's shard as run_fetch_gitlab writes it, with rurl's licence read as `license` on
# `read_on`, or not read at all.
.gll_parts <- function(license = NA_character_, read = TRUE, read_on = format(Sys.Date())) {
  p <- tempfile("gll_parts_"); dir.create(p)
  path <- file.path(p, GITLAB_SHARD_DB)
  export_snapshot_shard(path, data.frame(repo_id = .gll_rurl, commits_total = 240L, contributors_total = 3L,
                                         top_contributor_commits = 200L, stringsAsFactors = FALSE))
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE license (repo_id TEXT PRIMARY KEY, license TEXT, read_on TEXT NOT NULL)")
  if (read)
    DBI::dbWriteTable(con, "license", data.frame(repo_id = .gll_rurl, license = license, read_on = read_on,
                                                 stringsAsFactors = FALSE), append = TRUE)
  p
}

.gll_ai_parts <- function() {
  p <- tempfile("gll_ai_parts_"); dir.create(p)
  .gll_ai$export_ai_shard(file.path(p, "vcs-ai-shard-0.db"), data.frame(
    repo_id = "github.com/tidyverse/ggplot2", tool = "copilot", first_seen_date = "2026-02-01",
    first_seen_censored = 0L, evidence_tiers = "A", authored = 1L, last_confirmed_date = format(Sys.Date()),
    stringsAsFactors = FALSE))
  p
}

.gll_read <- function(rel, sql, db = "vcs-signals-summary.db") {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, db))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

# The published summary's licence for each package.
.gll_licences <- function(rel) {
  s <- .gll_read(rel, "SELECT package, license FROM vcs_signals_summary")
  stats::setNames(s$license, s$package)[c("ggplot2", "rurl")]
}

test_that("a GitLab licence read weekly survives the daily runs and the other merges, and changes only with a reading", {
  rel <- tempfile("gll_rel_"); dir.create(rel)
  out <- tempfile("gll_out_"); dir.create(out)
  quiet <- function(expr) invisible(suppressMessages(capture.output(expr)))
  daily <- function(stars, full = FALSE) quiet(run_update(.gll_io(rel, stars), out, list(force_full = full)))
  weekly <- function(parts) quiet(.gll_weekly$run_merge(.gll_io(rel), tempfile("gll_wk_"), parts))
  stored <- function(db = "vcs-signals-summary.db") .gll_read(rel, "SELECT * FROM gitlab_license", db)

  daily(7L, full = TRUE)
  expect_identical(.gll_licences(rel), c(ggplot2 = "MIT", rurl = NA))

  weekly(.gll_parts("MIT"))
  expect_identical(.gll_licences(rel), c(ggplot2 = "MIT", rurl = "MIT"))
  expect_identical(stored(), data.frame(repo_id = .gll_rurl, license = "MIT", read_on = format(Sys.Date()),
                                        stringsAsFactors = FALSE))

  # The daily run collects rurl again, with no licence in GitLab's answer.
  daily(9L)
  expect_identical(.gll_read(rel, "SELECT stars FROM vcs_signals_summary WHERE package = 'rurl'")$stars, 9L)
  expect_identical(.gll_licences(rel), c(ggplot2 = "MIT", rurl = "MIT"))
  expect_identical(stored("vcs-signals-recent.db"), stored())

  weekly(.gll_parts("GPL-3.0"))
  expect_identical(.gll_licences(rel), c(ggplot2 = "MIT", rurl = "GPL-3.0"))

  # A week that did not read rurl keeps what it has.
  weekly(.gll_parts(read = FALSE))
  expect_identical(.gll_licences(rel), c(ggplot2 = "MIT", rurl = "GPL-3.0"))

  # The AI merge rebuilds the summary and the backfill merge republishes it, both with the table.
  quiet(.gll_ai$run_merge(.gll_io(rel), tempfile("gll_ai_"), .gll_ai_parts()))
  expect_identical(.gll_licences(rel), c(ggplot2 = "MIT", rurl = "GPL-3.0"))
  expect_identical(stored()$license, "GPL-3.0")
  empty <- tempfile("gll_bf_parts_"); dir.create(empty)
  quiet(.gll_backfill$run_merge(.gll_io(rel), tempfile("gll_bf_"), empty))
  expect_identical(.gll_licences(rel), c(ggplot2 = "MIT", rurl = "GPL-3.0"))
  expect_identical(stored("vcs-signals-recent.db")$license, "GPL-3.0")

  # GitLab answering with no licence is a reading.
  weekly(.gll_parts(NA_character_))
  expect_identical(.gll_licences(rel), c(ggplot2 = "MIT", rurl = NA))
  expect_identical(stored()$repo_id, .gll_rurl)
})

test_that("a reading replaces the stored licence, NA included, and no reading or an older one leaves it", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  read <- function(id, license, on) data.frame(repo_id = id, license = license, read_on = on, stringsAsFactors = FALSE)
  upsert_gitlab_license(con, read(c("gitlab.com/a/one", "gitlab.com/b/two", "gitlab.com/c/three"),
                                  c("MIT", "GPL-3.0", "MIT"), "2026-10-11"))
  upsert_gitlab_license(con, read("gitlab.com/a/one", NA_character_, "2026-10-18"))
  upsert_gitlab_license(con, read("gitlab.com/b/two", "MIT", "2026-10-04"))
  upsert_gitlab_license(con, read("gitlab.com/c/three", "Apache-2.0", "2026-10-11"))
  upsert_gitlab_license(con, read(character(0), character(0), character(0)))
  upsert_gitlab_license(con, NULL)
  got <- DBI::dbGetQuery(con, "SELECT * FROM gitlab_license ORDER BY repo_id")
  expect_identical(got$license, c(NA, "GPL-3.0", "Apache-2.0"))
  expect_identical(got$read_on, c("2026-10-18", "2026-10-11", "2026-10-11"))
})

test_that("a GitLab row takes its licence from gitlab_license, NA while unread, and any other row keeps its own", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  ids <- c("github.com/a/gh", "gitlab.com/b/read", "gitlab.com/c/unread", "codeberg.org/d/cb")
  DBI::dbWriteTable(con, "repos", data.frame(repo_id = ids, node_id = NA_character_,
    host = c("github", "gitlab", "gitlab", "codeberg"), host_domain = sub("/.*$", "", ids), owner = "o",
    name = "n", name_with_owner = "o/n", supported = 1L, n_packages = 1L, first_seen = "2026-10-01",
    last_seen = "2026-10-18", status = "active", stringsAsFactors = FALSE), append = TRUE)
  upsert_gitlab_license(con, data.frame(repo_id = c("gitlab.com/b/read", "github.com/a/gh"),
    license = c("MIT", "GPL-3.0"), read_on = "2026-10-18", stringsAsFactors = FALSE))
  attrs <- data.frame(repo_id = ids, license = c("Apache-2.0", NA, "GPL-2.0", "MIT"), topics = "r",
                      stringsAsFactors = FALSE)
  got <- apply_gitlab_license(con, attrs)
  expect_identical(got$license, c("Apache-2.0", "MIT", NA, "MIT"))
  expect_identical(got[c("repo_id", "topics")], attrs[c("repo_id", "topics")])
})

# ---- the publish gate ---------------------------------------------------------------

# A summary whose repos table lists gitlab.com/g/p1 to p10 as GitLab repositories (those in
# `retired` retired) and whose gitlab_license rows are `ids`, each read as `license`.
.gll_gate <- function(ids, license = "MIT", retired = integer(0), with_table = TRUE) {
  all <- 1:10
  path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbWriteTable(con, "repos", data.frame(
    repo_id = sprintf("gitlab.com/g/p%d", all), node_id = sprintf("gitlab:gitlab.com:%d", all), host = "gitlab",
    host_domain = "gitlab.com", owner = "g", name = sprintf("p%d", all), name_with_owner = sprintf("g/p%d", all),
    supported = 1L, n_packages = 1L, first_seen = "2026-10-01", last_seen = "2026-10-18",
    status = ifelse(all %in% retired, "retired", "active"), stringsAsFactors = FALSE), append = TRUE)
  if (!with_table) DBI::dbExecute(con, "DROP TABLE gitlab_license")
  else if (length(ids)) DBI::dbWriteTable(con, "gitlab_license", data.frame(
    repo_id = sprintf("gitlab.com/g/p%d", ids), license = license, read_on = "2026-10-18",
    stringsAsFactors = FALSE), append = TRUE)
  path
}
.gll_reasons <- function(prev, nxt) grep("^gitlab_license:", summary_regressions(prev, nxt), value = TRUE)

test_that("the gate passes a changed licence, a new row and a first publish", {
  prev <- .gll_gate(1:5)
  expect_equal(.gll_reasons(prev, .gll_gate(1:5, license = c("GPL-3.0", NA, "MIT", "MIT", "MIT"))), character(0))
  expect_equal(.gll_reasons(prev, .gll_gate(1:6)), character(0))
  expect_equal(.gll_reasons(.gll_gate(integer(0), with_table = FALSE), prev), character(0))
  expect_equal(.gll_reasons(.gll_gate(integer(0)), prev), character(0))
})

test_that("the gate refuses a row gone while its repository is active, and names it", {
  r <- .gll_reasons(.gll_gate(1:5), .gll_gate(c(1:2, 4:5)))
  expect_identical(r, "gitlab_license: 1 row(s) are gone while the repository is still active: gitlab.com/g/p3")
  expect_match(.gll_reasons(.gll_gate(1:5), .gll_gate(integer(0))),
               "gitlab_license: 5 row(s) are gone while the repository is still active: gitlab.com/g/p1, gitlab.com/g/p2, gitlab.com/g/p3, and 2 more",
               fixed = TRUE)
})

test_that("a row may leave once its repository is retired or gone from repos", {
  prev <- .gll_gate(1:5)
  expect_equal(.gll_reasons(prev, .gll_gate(1:4, retired = 5L)), character(0))
  gone <- .gll_gate(1:4)
  con <- DBI::dbConnect(RSQLite::SQLite(), gone)
  DBI::dbExecute(con, "DELETE FROM repos WHERE repo_id = 'gitlab.com/g/p5'")
  DBI::dbDisconnect(con)
  expect_equal(.gll_reasons(prev, gone), character(0))
})

test_that("a table without the licence column is refused, and a repos table without status explains no lost row", {
  prev <- .gll_gate(1:5)
  no_col <- .gll_gate(1:5)
  con <- DBI::dbConnect(RSQLite::SQLite(), no_col)
  DBI::dbExecute(con, "ALTER TABLE gitlab_license DROP COLUMN license")
  DBI::dbDisconnect(con)
  expect_identical(.gll_reasons(prev, no_col),
                   "gitlab_license: published without license, so the gate cannot tell which licences were kept")
  no_status <- .gll_gate(1:4, retired = 5L)
  con <- DBI::dbConnect(RSQLite::SQLite(), no_status)
  DBI::dbExecute(con, "ALTER TABLE repos DROP COLUMN status")
  DBI::dbDisconnect(con)
  expect_identical(.gll_reasons(prev, no_status),
                   "gitlab_license: 1 row(s) are gone while the repository is still active: gitlab.com/g/p5")
})

test_that("a malformed GitLab shard is logged and skipped, and the GitHub shards still merge and publish", {
  rel <- tempfile("gll_rel_"); dir.create(rel)
  quiet <- function(expr) invisible(suppressMessages(capture.output(expr)))
  quiet(run_update(.gll_io(rel), tempfile("gll_out_"), list(force_full = TRUE)))
  p <- tempfile("gll_bad_"); dir.create(p)
  export_snapshot_shard(file.path(p, "vcs-signals-shard-0.db"), data.frame(
    repo_id = "github.com/tidyverse/ggplot2", commits_total = 4321L, contributors_total = 9L,
    top_contributor_commits = 99L, stringsAsFactors = FALSE))
  writeLines(strrep("not a database ", 200), file.path(p, GITLAB_SHARD_DB))
  msgs <- testthat::capture_messages(.gll_weekly$run_merge(.gll_io(rel), tempfile("gll_wk_"), p))
  expect_true(any(grepl("GitLab shard", msgs, fixed = TRUE)))
  s <- .gll_read(rel, "SELECT commits_total, contributors_total FROM vcs_signals_summary WHERE package = 'ggplot2'")
  expect_identical(s$commits_total, 4321L)
  expect_identical(s$contributors_total, 9L)
  expect_identical(.gll_read(rel, "SELECT COUNT(*) n FROM gitlab_license")$n, 0L)
})

test_that("a malformed Codeberg shard is logged and skipped, and the GitHub shards still merge and publish", {
  rel <- tempfile("gll_rel_"); dir.create(rel)
  quiet <- function(expr) invisible(suppressMessages(capture.output(expr)))
  quiet(run_update(.gll_io(rel), tempfile("gll_out_"), list(force_full = TRUE)))
  p <- tempfile("gll_bad_"); dir.create(p)
  export_snapshot_shard(file.path(p, "vcs-signals-shard-0.db"), data.frame(
    repo_id = "github.com/tidyverse/ggplot2", commits_total = 4321L, contributors_total = 9L,
    top_contributor_commits = 99L, stringsAsFactors = FALSE))
  writeLines(strrep("not a database ", 200), file.path(p, CODEBERG_SHARD_DB))
  msgs <- testthat::capture_messages(.gll_weekly$run_merge(.gll_io(rel), tempfile("gll_wk_"), p))
  expect_true(any(grepl("Codeberg shard vcs-signals-shard-codeberg.db not read, skipped", msgs, fixed = TRUE)))
  s <- .gll_read(rel, "SELECT commits_total, contributors_total FROM vcs_signals_summary WHERE package = 'ggplot2'")
  expect_identical(s$commits_total, 4321L)
  expect_identical(s$contributors_total, 9L)
  expect_identical(.gll_read(rel, "SELECT COUNT(*) n FROM gitlab_license")$n, 0L)
})

# A release as a publish by code older than the table leaves it: the recent shard says since when
# the licences were published but carries none, the summary has no table, and `prev` is what the
# previous summary holds.
.gll_lost_release <- function(prev, key = TRUE) {
  rel <- tempfile("gll_lost_"); dir.create(rel)
  mk <- function(name, rows = NULL, key = FALSE) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, name)); on.exit(DBI::dbDisconnect(con))
    ensure_repo_schema(con); ensure_series_schema(con)
    DBI::dbExecute(con, "DROP TABLE gitlab_license")
    if (!is.null(rows)) {
      DBI::dbExecute(con, "CREATE TABLE gitlab_license (repo_id TEXT PRIMARY KEY, license TEXT, read_on TEXT NOT NULL)")
      DBI::dbWriteTable(con, "gitlab_license", rows, append = TRUE)
    }
    if (key) DBI::dbExecute(con, "INSERT INTO pipeline_state VALUES (?, '2026-10-19')",
                            params = list(GITLAB_LICENSE_SINCE_KEY))
  }
  mk("vcs-signals-recent.db", key = key)
  mk("vcs-signals-summary.db")
  if (!is.null(prev)) mk("vcs-signals-summary-prev.db", prev)
  writeLines('{"summary":{"years":[]}}', file.path(rel, "manifest.json"))
  rel
}
.gll_seeded <- function(rel) {
  out <- tempfile("gll_seed_"); dir.create(out)
  work <- file.path(out, "_working.db")
  logged <- capture_messages(suppressWarnings(seed_working_db(local_release_io(rel), out, work)))
  con <- DBI::dbConnect(RSQLite::SQLite(), work); on.exit(DBI::dbDisconnect(con))
  list(rows = DBI::dbGetQuery(con, "SELECT repo_id, license, read_on FROM gitlab_license ORDER BY repo_id"),
       logged = logged)
}

test_that("a seed that lost the GitLab licences restores them from the previous summary", {
  saved <- data.frame(repo_id = c("gitlab.com/g/a", "gitlab.com/g/b"), license = c("MIT", NA),
                      read_on = "2026-10-20", stringsAsFactors = FALSE)
  s <- .gll_seeded(.gll_lost_release(saved))
  expect_identical(s$rows$repo_id, saved$repo_id)
  expect_identical(s$rows$license, saved$license)
  expect_true(any(grepl("restored 2 from vcs-signals-summary-prev.db", s$logged, fixed = TRUE)))
})

test_that("with no copy left the seed goes on with a warning, since the next weekly reads them again", {
  s <- .gll_seeded(.gll_lost_release(NULL))
  expect_identical(nrow(s$rows), 0L)
  expect_true(any(grepl("^::warning::.*gitlab_license since 2026-10-19", s$logged)))
})

test_that("before the licences were first published an empty table is not a loss", {
  s <- .gll_seeded(.gll_lost_release(NULL, key = FALSE))
  expect_identical(nrow(s$rows), 0L)
  expect_false(any(grepl("gitlab_license", s$logged, fixed = TRUE)))
})

test_that("a publish with GitLab licences marks the recent shard with since when they were published", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  .mark_gitlab_license_published(con, "2026-10-20")
  expect_length(DBI::dbGetQuery(con, "SELECT value FROM pipeline_state WHERE key = ?",
                                params = list(GITLAB_LICENSE_SINCE_KEY))$value, 0L)
  upsert_gitlab_license(con, data.frame(repo_id = "gitlab.com/g/a", license = "MIT", read_on = "2026-10-20"))
  .mark_gitlab_license_published(con, "2026-10-20")
  .mark_gitlab_license_published(con, "2026-10-27")
  expect_identical(DBI::dbGetQuery(con, "SELECT value FROM pipeline_state WHERE key = ?",
                                   params = list(GITLAB_LICENSE_SINCE_KEY))$value, "2026-10-20")
})
