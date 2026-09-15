# Every publisher seeds its working database from release "current" when its job
# starts and writes whole tables back 15 to 90 minutes later. The weekly merge and
# the AI merge share a Sunday cron and overlapped on 2026-08-30, 09-06 and 09-13.
# In one order the AI merge published last from a seed older than the weekly
# publish, and the weekly commit and contributor values in series_latest and the
# summary went back a week without a word. In the other order the weekly merge
# published last, and the regression gate refused it only because the AI merge's
# gains that week happened to exceed the 2% tolerance; it reported the stale build
# as "lost ground", which is not what had happened.
#
# These tests run both publishers for real against a release on local disk, with
# the second one started at an exact point inside the first one's window.

# Each script is loaded into its own environment. weekly.R, backfill.R and
# ai_backfill.R all define run_merge and main, so sourcing them side by side into
# the global environment would leave whichever came last under both names.
.script_env <- function(script) {
  env <- new.env(parent = globalenv())
  withr::with_dir(.repo_root, sys.source(file.path("scripts", script), envir = env))
  env
}
.weekly   <- .script_env("weekly.R")
.backfill <- .script_env("backfill.R")
.ai       <- .script_env("ai_backfill.R")
.update   <- .script_env("update.R")

.rid <- "github.com/a/ok"

# The state both publishers start from: one repository with a claude onset and a
# commits_total of 100, as the recent shard, summary and manifest a publish leaves.
# Written with the exporters rather than publish(), so the starting release does
# not depend on the check these tests are about.
.race_release <- function() {
  rel <- tempfile("release_"); dir.create(rel)
  today <- Sys.Date()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbWriteTable(con, "repos", data.frame(
    repo_id = .rid, node_id = "R_1", host = "github", host_domain = "github.com",
    owner = "a", name = "ok", name_with_owner = "a/ok", supported = 1L, n_packages = 1L,
    first_seen = "2026-01-01", last_seen = format(today), status = "active",
    stringsAsFactors = FALSE), append = TRUE)
  DBI::dbWriteTable(con, "repo_packages", data.frame(
    repo_id = .rid, package = "pkgA", origin = "cran", resolved_from = "url",
    stringsAsFactors = FALSE), append = TRUE)
  DBI::dbExecute(con, "INSERT INTO signals_series VALUES (?, ?, 'stars', 42)",
                 params = list(.rid, format(today - 30)))
  DBI::dbExecute(con, "INSERT INTO series_latest VALUES (?, 'stars', 42)", params = list(.rid))
  DBI::dbExecute(con, "INSERT INTO series_latest VALUES (?, 'commits_total', 100)", params = list(.rid))
  DBI::dbExecute(con, "INSERT INTO vcs_ai_signals
    (repo_id, tool, first_seen_date, first_seen_censored, evidence_tiers, authored, last_confirmed_date)
    VALUES (?, 'claude', '2026-01-05', 0, 'D', 0, '2026-01-05')", params = list(.rid))

  read <- function(nm) DBI::dbReadTable(con, nm)
  attrs <- data.frame(repo_id = .rid, first_seen = "2026-01-01", last_seen = format(today),
                      license = "MIT", topics = NA_character_, is_archived = 0L,
                      last_commit_date = NA_character_, last_release_date = NA_character_,
                      median_days_between_releases = NA_integer_, stringsAsFactors = FALSE)
  summary_df <- build_signals_summary(read("series_latest"), read("signals_series"), attrs,
                                      read("repo_packages"), format(today),
                                      ai_signals = read("vcs_ai_signals"))
  DBI::dbWriteTable(con, "vcs_signals_summary", summary_df, append = TRUE)

  recent <- file.path(rel, "vcs-signals-recent.db")
  export_series_shard(recent, extract_recent_rows(con, today, RECENT_WINDOW))
  .embed_recent_tables(con, recent)
  export_summary_shard(file.path(rel, "vcs-signals-summary.db"), summary_df, read("repos"),
                       read("repo_packages"), read("vcs_ai_signals"), read("vcs_dev_tooling"),
                       extra = stats::setNames(lapply(SUMMARY_EXTRA_TABLES, read), SUMMARY_EXTRA_TABLES))
  write_manifest(file.path(rel, "manifest.json"), character(0), "current",
                 list(source_kind = "live", years = list()))
  rel
}

# This week's weekly snapshot: commits_total went from 100 to 500.
.weekly_parts <- function() {
  p <- tempfile("weekly_parts_"); dir.create(p)
  .weekly$export_snapshot_shard(file.path(p, "vcs-signals-shard-0.db"), data.frame(
    repo_id = .rid, commits_total = 500L, contributors_total = 10L,
    median_days_to_close_issue = NA_integer_, median_days_to_close_pr = NA_integer_,
    median_open_issue_age_days = NA_integer_, stringsAsFactors = FALSE))
  p
}

# This week's deep AI shard: a copilot onset the published table does not have.
.ai_parts <- function() {
  p <- tempfile("ai_parts_"); dir.create(p)
  .ai$export_ai_shard(file.path(p, "vcs-ai-shard-0.db"), data.frame(
    repo_id = .rid, tool = "copilot", first_seen_date = "2026-02-01", first_seen_censored = 0L,
    evidence_tiers = "A", authored = 1L, last_confirmed_date = format(Sys.Date()),
    stringsAsFactors = FALSE))
  p
}

.run_weekly_merge <- function(rel) suppressMessages(
  .weekly$run_merge(local_release_io(rel), tempfile("weekly_out_"), .weekly_parts()))
.run_ai_merge <- function(rel) suppressMessages(
  .ai$run_merge(local_release_io(rel), tempfile("ai_out_"), .ai_parts()))

# Runs fn once, right after the first download of the recent shard. That download
# is the seed, so the other publisher then goes from seed to publish entirely
# inside this one's window.
.after_seed <- function(fn) {
  fired <- FALSE
  function(pattern) {
    if (!fired && identical(pattern, "vcs-signals-recent.db")) { fired <<- TRUE; fn() }
  }
}

# What the release says now: both merges' results, read back from the assets.
.published <- function(rel) {
  s <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-summary.db"))
  r <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-recent.db"))
  on.exit({ DBI::dbDisconnect(s); DBI::dbDisconnect(r) })
  list(
    ai_tools = sort(DBI::dbGetQuery(s, "SELECT tool FROM vcs_ai_signals")$tool),
    summary_commits = DBI::dbGetQuery(s, "SELECT commits_total FROM vcs_signals_summary")$commits_total,
    latest_commits = DBI::dbGetQuery(r,
      "SELECT value FROM series_latest WHERE metric = 'commits_total'")$value)
}

# ---- the two orders, refused -------------------------------------------------

test_that("a stale weekly build is refused as a conflict, never as lost ground, and uploads nothing", {
  # 2026-09-13: the weekly merge seeded, the AI merge published, the weekly merge
  # tried to publish. The refusal was right and its reason was wrong.
  rel <- .race_release()
  after_ai <- NULL
  weekly_io <- local_release_io(rel, on_download = .after_seed(function() {
    .run_ai_merge(rel)
    after_ai <<- local_release_snapshot(rel)
  }))
  err <- tryCatch(
    suppressMessages(.weekly$run_merge(weekly_io, tempfile("weekly_out_"), .weekly_parts())),
    error = function(e) e)

  expect_false(is.null(after_ai))
  expect_s3_class(err, "vcs_publish_conflict")
  expect_no_match(conditionMessage(err), "lost ground")
  expect_match(conditionMessage(err), "vcs-signals-summary.db", fixed = TRUE)
  expect_match(conditionMessage(err), "Nothing was uploaded", fixed = TRUE)
  expect_length(weekly_io$uploaded(), 0)
  expect_identical(local_release_snapshot(rel), after_ai)
  expect_equal(.published(rel)$ai_tools, c("claude", "copilot"))
})

test_that("a stale AI build is refused instead of reverting the weekly metrics", {
  # 2026-08-30 and 09-06: the AI merge seeded, the weekly merge published, the AI
  # merge published over it. No rule in the gate could fire, and series_latest
  # and the summary went back to the previous week's commit counts.
  rel <- .race_release()
  after_weekly <- NULL
  ai_io <- local_release_io(rel, on_download = .after_seed(function() {
    .run_weekly_merge(rel)
    after_weekly <<- local_release_snapshot(rel)
  }))
  err <- tryCatch(
    suppressMessages(.ai$run_merge(ai_io, tempfile("ai_out_"), .ai_parts())),
    error = function(e) e)

  expect_s3_class(err, "vcs_publish_conflict")
  expect_length(ai_io$uploaded(), 0)
  expect_identical(local_release_snapshot(rel), after_weekly)
  got <- .published(rel)
  expect_equal(got$latest_commits, 500L)
  expect_equal(got$summary_commits, 500L)
})

# ---- the two orders, end to end through each script's entry point ------------

test_that("a weekly merge that seeded before an AI merge published re-seeds and keeps both", {
  rel <- .race_release()
  weekly_io <- local_release_io(rel, on_download = .after_seed(function() .run_ai_merge(rel)))
  withr::local_envvar(VCS_PARTS = .weekly_parts())

  suppressMessages(expect_message(
    .weekly$main("merge", tempfile("weekly_out_"), io = weekly_io),
    "another publisher"))

  got <- .published(rel)
  expect_equal(got$ai_tools, c("claude", "copilot"))
  expect_equal(got$latest_commits, 500L)
  expect_equal(got$summary_commits, 500L)
})

test_that("an AI merge that seeded before a weekly merge published re-seeds and keeps both", {
  rel <- .race_release()
  ai_io <- local_release_io(rel, on_download = .after_seed(function() .run_weekly_merge(rel)))
  withr::local_envvar(VCS_PARTS = .ai_parts())

  suppressMessages(expect_message(
    .ai$main("merge", tempfile("ai_out_"), io = ai_io),
    "another publisher"))

  got <- .published(rel)
  expect_equal(got$latest_commits, 500L)
  expect_equal(got$summary_commits, 500L)
  expect_equal(got$ai_tools, c("claude", "copilot"))
})

test_that("a backfill merge dispatched over a Sunday merge re-seeds and keeps both", {
  rel <- .race_release()
  old_day <- format(Sys.Date() - 600)
  parts <- tempfile("backfill_parts_"); dir.create(parts)
  export_series_shard(file.path(parts, "vcs-signals-shard-0.db"), data.frame(
    repo_id = .rid, date = old_day, metric = "stars", value = 7L, stringsAsFactors = FALSE))
  io <- local_release_io(rel, on_download = .after_seed(function() .run_ai_merge(rel)))
  withr::local_envvar(VCS_PARTS = parts, VCS_PURGE_METRICS = "")

  suppressMessages(expect_message(
    .backfill$main("merge", tempfile("backfill_out_"), io = io),
    "another publisher"))

  expect_equal(.published(rel)$ai_tools, c("claude", "copilot"))
  yc <- DBI::dbConnect(RSQLite::SQLite(),
                       file.path(rel, sprintf("vcs-signals-%s.db", substr(old_day, 1, 4))))
  on.exit(DBI::dbDisconnect(yc), add = TRUE)
  expect_equal(DBI::dbGetQuery(yc, "SELECT value FROM signals_series WHERE date = ?",
                               params = list(old_day))$value, 7L)
})

test_that("the daily update fails on a conflict instead of retrying", {
  # A retry would have to collect every gauge again, which is the 90 minutes that
  # made the window wide in the first place. The run fails with the true reason.
  rel <- .race_release()
  acquired <- 0L
  io <- local_release_io(rel,
    on_download = .after_seed(function() .run_ai_merge(rel)),
    acquire = function() {
      acquired <<- acquired + 1L
      data.frame(package = "pkgA", origin = "cran", url_raw = "https://github.com/a/ok",
                 bugreports_raw = NA, stringsAsFactors = FALSE)
    },
    graphql = function(query) {
      if (grepl("rateLimit", query)) return(list(data = list(nodes = list())))
      f <- if (grepl("followRenames", query)) "resolve_one.json"
           else if (grepl("history \\{ totalCount", query)) "commits.json" else "gauges_one.json"
      jsonlite::fromJSON(readLines(file.path("fixtures", f), warn = FALSE), simplifyVector = FALSE)
    })
  withr::local_envvar(FORCE_FULL_REBUILD = "")

  err <- tryCatch(suppressMessages(capture.output(.update$main(tempfile("update_out_"), io = io))),
                  error = function(e) e)
  expect_s3_class(err, "vcs_publish_conflict")
  expect_equal(acquired, 1L)
  expect_length(io$uploaded(), 0)
})

# ---- where the checks sit ----------------------------------------------------

test_that("a publish landing between the pull and the first upload is caught before anything is uploaded", {
  # The first read after the publish-time pull passes; the weekly merge then
  # publishes in full before the AI merge reaches its uploads.
  rel <- .race_release()
  pulled <- FALSE
  landed <- FALSE
  ai_io <- local_release_io(rel,
    on_download = function(pattern) if (identical(pattern, "vcs-signals-summary.db")) pulled <<- TRUE,
    on_generation = function(n) if (pulled && !landed) { .run_weekly_merge(rel); landed <<- TRUE })
  err <- tryCatch(
    suppressMessages(.ai$run_merge(ai_io, tempfile("ai_out_"), .ai_parts())),
    error = function(e) e)

  expect_true(landed)
  expect_s3_class(err, "vcs_publish_conflict")
  expect_match(conditionMessage(err), "before uploading", fixed = TRUE)
  expect_length(ai_io$uploaded(), 0)
  expect_equal(.published(rel)$latest_commits, 500L)
})

test_that("the regression gate still refuses genuine loss when the release did not move", {
  rel <- .race_release()
  io <- local_release_io(rel)
  out <- tempfile("loss_"); dir.create(out)
  seed <- seed_working_db(io, out, file.path(out, "work.db"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "work.db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "DELETE FROM vcs_ai_signals")
  before <- local_release_snapshot(rel)

  err <- tryCatch(
    publish(io, con, out, "current", "live", touched_years = character(0),
            base_generation = attr(seed, "generation")),
    error = function(e) e)
  expect_match(conditionMessage(err), "lost ground", fixed = TRUE)
  expect_false(inherits(err, "vcs_publish_conflict"))
  expect_length(io$uploaded(), 0)
  expect_identical(local_release_snapshot(rel), before)
})

test_that("an upload that interleaves with another publisher is reported, not passed", {
  # Nothing closes the window between the last check and the last upload. What
  # the release shows afterwards is compared with what this run sent, so a
  # release left holding a mixture of two builds fails the run by name.
  rel <- .race_release()
  io0 <- local_release_io(rel)
  out <- tempfile("mix_"); dir.create(out)
  seed <- seed_working_db(io0, out, file.path(out, "work.db"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "work.db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  io <- local_release_io(rel, on_upload = function(name) {
    if (identical(name, "vcs-signals-summary.db")) {
      writeLines("another build", file.path(rel, "vcs-signals-recent.db"))
      writeLines("another build", file.path(rel, "vcs-signals-2019.db"))
    }
  })
  err <- tryCatch(
    publish(io, con, out, "current", "live", force_full = TRUE,
            base_generation = attr(seed, "generation")),
    error = function(e) e)

  expect_s3_class(err, "error")
  expect_false(inherits(err, "vcs_publish_conflict"))
  expect_match(conditionMessage(err), "interleaved", fixed = TRUE)
  expect_match(conditionMessage(err), "vcs-signals-recent.db", fixed = TRUE)
  expect_match(conditionMessage(err), "vcs-signals-2019.db", fixed = TRUE)
})

test_that("a release listing that lags the uploads is read again before it is called a mixture", {
  rel <- .race_release()
  io0 <- local_release_io(rel)
  out <- tempfile("lag_"); dir.create(out)
  seed <- seed_working_db(io0, out, file.path(out, "work.db"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "work.db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  stale <- local_release_generation(rel)
  lagging <- FALSE
  slept <- 0L
  base <- local_release_io(rel, on_upload = function(name)
    if (identical(name, "manifest.json")) lagging <<- TRUE)
  io <- utils::modifyList(base, list(
    generation = function() {
      if (lagging) { lagging <<- FALSE; return(stale) }
      base$generation()
    },
    sleep = function(seconds) slept <<- slept + 1L))

  expect_no_error(publish(io, con, out, "current", "live", force_full = TRUE,
                          base_generation = attr(seed, "generation")))
  expect_equal(slept, 1L)
})

test_that("a confirmation read that fails once after the uploads does not fail a publish that landed", {
  # The release API has served 502s here (the update of 2026-08-20). After the
  # manifest is up the data is out, and a red run would skip the release notes
  # and the year-tag mirror and send update.yml's catch-up round again.
  rel <- .race_release()
  manifest_up <- FALSE
  blipped <- FALSE
  slept <- 0L
  base <- local_release_io(rel, on_upload = function(name)
    if (identical(name, "manifest.json")) manifest_up <<- TRUE)
  io <- utils::modifyList(base, list(
    generation = function() {
      if (manifest_up && !blipped) { blipped <<- TRUE; stop("gh: Server Error (HTTP 502)") }
      base$generation()
    },
    sleep = function(seconds) slept <<- slept + 1L))

  expect_no_error(suppressMessages(.ai$run_merge(io, tempfile("ai_out_"), .ai_parts())))
  expect_true(blipped)
  expect_equal(slept, 1L)
  expect_equal(.published(rel)$ai_tools, c("claude", "copilot"))
})

test_that("a confirmation that can never be read says the uploads went through", {
  rel <- .race_release()
  manifest_up <- FALSE
  base <- local_release_io(rel, on_upload = function(name)
    if (identical(name, "manifest.json")) manifest_up <<- TRUE)
  io <- utils::modifyList(base, list(generation = function() {
    if (manifest_up) stop("gh: Server Error (HTTP 502)")
    base$generation()
  }))

  err <- tryCatch(suppressMessages(.ai$run_merge(io, tempfile("ai_out_"), .ai_parts())),
                  error = function(e) e)
  expect_s3_class(err, "error")
  expect_false(inherits(err, "vcs_publish_conflict"))
  expect_match(conditionMessage(err), "uploads went through", fixed = TRUE)
  expect_match(conditionMessage(err), "could not be read back", fixed = TRUE)
  expect_match(conditionMessage(err), "HTTP 502", fixed = TRUE)
  expect_true("manifest.json" %in% io$uploaded())
})

# ---- the generation itself ---------------------------------------------------

test_that("seed_working_db reads the generation before it downloads anything", {
  # Read after the download, a publish landing mid-seed would be absorbed into the
  # base and never noticed. Read before, it shows up later as a conflict.
  rel <- .race_release()
  before <- local_release_generation(rel)
  io <- local_release_io(rel, on_download = .after_seed(function()
    writeLines("published mid-seed", file.path(rel, "vcs-signals-2019.db"))))
  out <- tempfile("seed_"); dir.create(out)
  seed <- seed_working_db(io, out, file.path(out, "work.db"))
  calls <- io$calls()
  expect_equal(calls[1], "generation")
  expect_true("download vcs-signals-recent.db" %in% calls)
  expect_true(seed)
  expect_identical(attr(seed, "generation"), before)
  expect_false(identical(attr(seed, "generation"), local_release_generation(rel)))

  empty <- tempfile("empty_release_"); dir.create(empty)
  out2 <- tempfile("seed_"); dir.create(out2)
  first <- seed_working_db(local_release_io(empty), out2, file.path(out2, "work.db"))
  expect_false(first)
  expect_identical(attr(first, "generation"), "")
})

test_that("a release that exists but carries no assets seeds as a first run", {
  # update.yml creates the release before the first run publishes into it. The
  # generation says there is nothing to seed from, and a download that finds no
  # recent shard agrees, so the missing shard is not a failure.
  out <- tempfile("seed_"); dir.create(out)
  asked <- character(0)
  io <- list(release_exists = function() TRUE,
             generation = function() "",
             download = function(pattern, dir) { asked <<- c(asked, pattern); FALSE })
  seed <- seed_working_db(io, out, file.path(out, "work.db"))
  expect_false(seed)
  expect_identical(attr(seed, "generation"), "")
  expect_equal(asked, "vcs-signals-recent.db")
})

test_that("a generation that reads as empty over a release with history stops instead of starting cold", {
  # "current" as a draft: the REST lookup by tag is blind to drafts, while gh's
  # download and upload find them. Read as empty, the merge started cold, skipped
  # the pull and the gate, and published one run's rows over the draft's history.
  rel <- .race_release()
  before <- local_release_snapshot(rel)
  io <- local_release_io(rel, generation = function() "")

  err <- tryCatch(suppressMessages(.ai$run_merge(io, tempfile("ai_out_"), .ai_parts())),
                  error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "vcs-signals-recent.db", fixed = TRUE)
  expect_match(conditionMessage(err), "no assets", fixed = TRUE)
  expect_length(io$uploaded(), 0)
  expect_identical(local_release_snapshot(rel), before)
})

test_that("publish() refuses to run without the generation it was seeded from", {
  rel <- tempfile("release_"); dir.create(rel)
  io <- local_release_io(rel)
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  out <- tempfile("out_"); dir.create(out)
  expect_error(publish(io, con, out, "current", "live", force_full = TRUE), "generation")
  expect_error(publish(io, con, out, "current", "live", force_full = TRUE, base_generation = NA_character_),
               "generation")
  expect_length(io$uploaded(), 0)
})

test_that("an unreadable generation fails closed and uploads nothing", {
  rel <- .race_release()
  io0 <- local_release_io(rel)
  out <- tempfile("unreadable_"); dir.create(out)
  seed <- seed_working_db(io0, out, file.path(out, "work.db"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "work.db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  before <- local_release_snapshot(rel)
  base <- attr(seed, "generation")

  broken <- local_release_io(rel, generation = function() stop("gh: Server Error (HTTP 502)"))
  expect_error(publish(broken, con, out, "current", "live", touched_years = character(0),
                       base_generation = base), "HTTP 502")
  expect_length(broken$uploaded(), 0)

  shapeless <- local_release_io(rel, generation = function() character(0))
  expect_error(publish(shapeless, con, out, "current", "live", touched_years = character(0),
                       base_generation = base), "generation")
  expect_length(shapeless$uploaded(), 0)

  absent <- local_release_io(rel)
  absent$generation <- NULL
  expect_error(seed_working_db(absent, out, file.path(out, "work2.db")), "generation")
  expect_identical(local_release_snapshot(rel), before)
})

test_that("gh_release_generation: a missing release is empty and any other failure is fatal", {
  fake_gh <- function(out = character(0), err = "", status = NULL) {
    function(command, args, stdout, stderr) {
      if (is.character(stderr)) writeLines(err, stderr)
      if (!is.null(status)) attr(out, "status") <- status
      out
    }
  }

  got <- gh_release_generation("o/r", run = fake_gh(
    out = c("vcs-signals-summary.db\tsha256:bb", "manifest.json\tsha256:aa",
            "vcs-signals-2019.db\tRA_kwDO@2026-07-09T00:00:00Z"),
    err = "A new release of gh is available"))
  expect_identical(got, paste("manifest.json\tsha256:aa", "vcs-signals-2019.db\tRA_kwDO@2026-07-09T00:00:00Z",
                              "vcs-signals-summary.db\tsha256:bb", sep = "\n"))
  expect_no_match(got, "new release of gh", fixed = TRUE)

  expect_identical(gh_release_generation("o/r", run = fake_gh()), "")
  expect_identical(gh_release_generation("o/r", run = fake_gh(err = "release not found", status = 1L)), "")
  no_wait <- function(seconds) invisible(NULL)
  expect_error(gh_release_generation("o/r", sleep = no_wait, run = fake_gh(
    err = "gh: Server Error (HTTP 502)", status = 1L)), "HTTP 502")
  expect_error(gh_release_generation("o/r", sleep = no_wait, run = fake_gh(
    err = "gh: Bad credentials (HTTP 401)", status = 1L)), "HTTP 401")
  # Only gh's own "release not found" means there is no release. Any other 404
  # is something this function cannot interpret, and "" is the dangerous guess.
  expect_error(gh_release_generation("o/r", sleep = no_wait, run = fake_gh(
    err = "gh: Not Found (HTTP 404)", status = 1L)), "HTTP 404")

  # The same lookup gh release download and upload use, which finds a draft
  # "current" as well as a published one. GET releases/tags/<tag> is 404 for a
  # draft, and read as an empty release that started the merge cold.
  seen <- NULL
  gh_release_generation("r-observatory/vcs-signals", tag = "current",
    run = function(command, args, stdout, stderr) { seen <<- c(command, args); character(0) })
  expect_equal(seen[1:8], c("gh", "release", "view", "current", "--repo", "r-observatory/vcs-signals",
                            "--json", "assets"))
  expect_true(any(grepl(".digest", seen, fixed = TRUE)))
})

test_that("gh_release_generation reads again through a transient failure before giving up", {
  # Every publish reads the generation at least four times, and none of those
  # reads may throw away a merge or fail a publish over one 5xx.
  calls <- 0L
  slept <- numeric(0)
  flaky <- function(fails) function(command, args, stdout, stderr) {
    calls <<- calls + 1L
    if (calls <= fails) {
      writeLines("gh: Server Error (HTTP 502)", stderr)
      return(structure(character(0), status = 1L))
    }
    "manifest.json\tsha256:aa"
  }
  got <- gh_release_generation("o/r", run = flaky(2L), waits = c(5, 20),
                               sleep = function(s) slept <<- c(slept, s))
  expect_identical(got, "manifest.json\tsha256:aa")
  expect_equal(calls, 3L)
  expect_length(slept, 2L)

  calls <- 0L
  slept <- numeric(0)
  expect_error(gh_release_generation("o/r", run = flaky(99L), waits = c(5, 20),
                                     sleep = function(s) slept <<- c(slept, s)), "HTTP 502")
  expect_equal(calls, 3L)

  # A release that is not there is an answer, not a failure, and is not asked again.
  calls <- 0L
  expect_identical(gh_release_generation("o/r", waits = c(5, 20), sleep = function(s) stop("no wait"),
    run = function(command, args, stdout, stderr) {
      calls <<- calls + 1L
      writeLines("release not found", stderr)
      structure(character(0), status = 1L)
    }), "")
  expect_equal(calls, 1L)
})

test_that("every publisher's real io reads the release generation", {
  # A main() that builds its io without the member would fail at seed, which is
  # loud; this says which script and why before a run ever gets that far.
  for (script in c("update.R", "weekly.R", "backfill.R", "ai_backfill.R")) {
    src <- paste(readLines(file.path(.repo_root, "scripts", script), warn = FALSE), collapse = "\n")
    expect_true(grepl("generation\\s*=\\s*function\\(\\)\\s*gh_release_generation\\(RELEASE_REPO\\)", src),
                info = script)
  }
})

test_that("retry_on_publish_conflict retries a conflict once and nothing else", {
  conflict <- function() stop(publish_conflict("before uploading",
                                               "a.db\tsha256:1\nb.db\tsha256:2",
                                               "a.db\tsha256:9\nb.db\tsha256:2"))
  n <- 0L
  expect_error(retry_on_publish_conflict(function() { n <<- n + 1L; stop("boom") }), "boom")
  expect_equal(n, 1L)

  n <- 0L
  res <- suppressMessages(retry_on_publish_conflict(function() {
    n <<- n + 1L
    if (n == 1L) conflict()
    "published"
  }))
  expect_equal(res, "published")
  expect_equal(n, 2L)

  n <- 0L
  err <- tryCatch(suppressMessages(retry_on_publish_conflict(function() { n <<- n + 1L; conflict() })),
                  error = function(e) e)
  expect_s3_class(err, "vcs_publish_conflict")
  expect_equal(n, 2L)
  expect_match(conditionMessage(err), "a.db", fixed = TRUE)
  expect_no_match(conditionMessage(err), "b.db", fixed = TRUE)
})

# ---- the pull no longer switches the gate off --------------------------------

test_that("a first publish into an empty release still works", {
  rel <- tempfile("release_"); dir.create(rel)
  io <- local_release_io(rel, release_exists = function() TRUE)   # created, nothing uploaded yet
  out <- tempfile("first_"); dir.create(out)
  seed <- seed_working_db(io, out, file.path(out, "work.db"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "work.db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "INSERT INTO signals_series VALUES ('R', ?, 'stars', 1)",
                 params = list(format(Sys.Date())))

  publish(io, con, out, "current", "live", base_generation = attr(seed, "generation"))
  expect_setequal(list.files(rel), c("manifest.json", "vcs-signals-recent.db", "vcs-signals-summary.db",
                                     sprintf("vcs-signals-%s.db", format(Sys.Date(), "%Y"))))
})

test_that("a failed pull stops the publish instead of switching the gate off", {
  # protect_history_pull stops when a published asset cannot be downloaded, and
  # publish() used to catch that and carry on as if nothing had been published:
  # no summary to compare against, so no regression gate, and every shard counted
  # as changed and uploaded over the top.
  rel <- .race_release()
  before <- local_release_snapshot(rel)
  healthy <- local_release_io(rel)
  failing <- function(asset) local_release_io(rel, download = function(pattern, dir)
    if (identical(pattern, asset)) FALSE else healthy$download(pattern, dir))

  io <- failing("manifest.json")
  expect_error(suppressMessages(.ai$run_merge(io, tempfile("ai_out_"), .ai_parts())),
               "manifest.json could not be downloaded", fixed = TRUE)
  expect_length(io$uploaded(), 0)

  # The summary pull was best-effort, which switched the gate off the same way.
  io <- failing("vcs-signals-summary.db")
  expect_error(suppressMessages(.ai$run_merge(io, tempfile("ai_out_"), .ai_parts())),
               "vcs-signals-summary.db", fixed = TRUE)
  expect_length(io$uploaded(), 0)
  expect_identical(local_release_snapshot(rel), before)
})
