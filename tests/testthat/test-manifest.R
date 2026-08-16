# Integrity / completeness core for the primary published db
# (vcs-signals-summary.db) attached as top-level fields in manifest.json.

# Build a tiny, real summary DB on disk via the canonical export_summary_shard,
# so the tables/counts/bytes/hash are exercised against a genuine SQLite file.
build_summary_db <- function(n = 3L) {
  tmp <- tempfile(fileext = ".db")
  summ <- data.frame(
    package = paste0("pkg", seq_len(n)), origin = "cran",
    repo_id = paste0("R", seq_len(n)),
    stars = seq_len(n) * 5L, forks = seq_len(n), issues_open = 0L, prs_open = 0L,
    commits_total = seq_len(n) * 10L, releases_total = 0L,
    last_commit_date = "2026-07-01", license = "MIT", topics = "r", is_archived = 0L,
    trend_30d = NA_real_, first_seen = "2026-07-06", last_seen = "2026-07-06",
    stringsAsFactors = FALSE)
  repos <- data.frame(
    repo_id = paste0("R", seq_len(n)), node_id = NA_character_, host = "github",
    host_domain = "github.com", owner = "o", name = paste0("n", seq_len(n)),
    name_with_owner = paste0("o/n", seq_len(n)), supported = 1L, n_packages = 1L,
    first_seen = "2026-07-06", last_seen = "2026-07-06", status = "active",
    stringsAsFactors = FALSE)
  rp <- data.frame(
    repo_id = paste0("R", seq_len(n)), package = paste0("pkg", seq_len(n)),
    origin = "cran", resolved_from = "url", stringsAsFactors = FALSE)
  export_summary_shard(tmp, summ, repos, rp)
  tmp
}

test_that("summary_integrity_core reports filename, bytes, sha256, tables, complete", {
  db <- build_summary_db(3L)
  on.exit(unlink(db))

  core <- summary_integrity_core(db, complete = TRUE)

  expect_equal(core$db_filename, basename(db))
  # db_bytes is a double (not cast to integer) so files >= ~2 GiB do not
  # overflow to NA; compare against the uncast file.size() directly.
  expect_type(core$db_bytes, "double")
  expect_equal(core$db_bytes, file.size(db))
  # sha256 is lowercase 64-char hex of the exact file bytes
  expect_match(core$db_sha256, "^[0-9a-f]{64}$")
  # tables maps EVERY user table (populated and empty schema tables alike) to
  # its row count, ordered by name, excluding sqlite_% internals.
  expect_equal(core$tables, list(
    pipeline_state      = 0L,
    repo_packages       = 3L,
    repos               = 3L,
    series_latest       = 0L,
    signals_series      = 0L,
    vcs_ai_models       = 0L,
    vcs_ai_rule_inventory = 0L,
    vcs_ai_signals      = 0L,
    vcs_ai_silent_channels = 0L,
    vcs_dev_tooling     = 0L,
    vcs_signals_summary = 3L))
  expect_true(core$complete)
})

test_that("summary_integrity_core sha256 matches an independent digest of the bytes", {
  # Compute the expected hash via an external CLI tool, independent of
  # file_sha256()'s own preferred backend (digest/openssl), so this genuinely
  # cross-checks the code path instead of re-running the same library. Skip
  # only if neither tool is on PATH (both are expected on CI).
  sha256sum_bin <- Sys.which("sha256sum")
  shasum_bin    <- Sys.which("shasum")
  if (!nzchar(sha256sum_bin) && !nzchar(shasum_bin)) {
    skip("neither sha256sum nor shasum is on PATH")
  }

  db <- build_summary_db(2L)
  on.exit(unlink(db))

  core <- summary_integrity_core(db)

  if (nzchar(sha256sum_bin)) {
    out <- system2(sha256sum_bin, shQuote(db), stdout = TRUE)
  } else {
    out <- system2(shasum_bin, c("-a", "256", shQuote(db)), stdout = TRUE)
  }
  independent <- tolower(sub("\\s.*$", "", out[1]))

  expect_equal(core$db_sha256, independent)
})

test_that("write_manifest merges the integrity core as top-level fields, preserving existing ones", {
  db <- build_summary_db(4L)
  on.exit(unlink(db), add = TRUE)
  core <- summary_integrity_core(db, complete = TRUE)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_manifest(
    path           = tmp,
    changed_shards = c("vcs-signals-summary.db"),
    tag            = "v20260714-000000",
    summary        = list(source_kind = "live", packages = 4L),
    core           = core
  )

  parsed <- jsonlite::fromJSON(tmp)
  # existing fields preserved
  expect_equal(parsed$tag, "v20260714-000000")
  expect_equal(parsed$summary$source_kind, "live")
  expect_equal(parsed$summary$packages, 4L)
  expect_equal(parsed$changed_shards, "vcs-signals-summary.db")
  expect_true(nzchar(parsed$generated_at))
  # new top-level integrity/completeness core
  expect_equal(parsed$db_filename, basename(db))
  expect_equal(parsed$db_bytes, file.size(db))
  expect_match(parsed$db_sha256, "^[0-9a-f]{64}$")
  expect_equal(parsed$tables$vcs_signals_summary, 4L)
  expect_equal(parsed$tables$repos, 4L)
  expect_equal(parsed$tables$repo_packages, 4L)
  expect_true(parsed$complete)
})

test_that("publish attaches the integrity core to the uploaded manifest", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "INSERT INTO signals_series VALUES ('R','2026-07-06','stars',10)")

  out <- tempfile("out"); dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  io <- list(
    release_exists = function() FALSE,
    download = function(pattern, dir) FALSE,
    upload = function(path) invisible(NULL))

  publish(io, con, out, "v1", "live", force_full = TRUE)

  manifest <- jsonlite::fromJSON(file.path(out, "manifest.json"))
  expect_equal(manifest$db_filename, "vcs-signals-summary.db")
  expect_equal(manifest$db_bytes, file.size(file.path(out, "vcs-signals-summary.db")))
  expect_match(manifest$db_sha256, "^[0-9a-f]{64}$")
  expect_true("vcs_signals_summary" %in% names(manifest$tables))
  expect_true(manifest$complete)

  # The db_sha256 in the manifest matches the on-disk bytes that were uploaded.
  expect_equal(manifest$db_sha256, file_sha256(file.path(out, "vcs-signals-summary.db")))
})

test_that("every table the pipeline writes reaches the published summary with its rows", {
  # Three tables shipped as empty tables in the published database: created by
  # the schema step, never filled by the export step, because the export named
  # one argument per table and nobody added theirs. A consumer reading an empty
  # table cannot tell that from a table nothing has written to yet, so it looked
  # exactly like a feature that had not run.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con); ensure_series_schema(con)

  DBI::dbExecute(con, "INSERT INTO vcs_ai_models
    (repo_id, tool, provider, family, version, context_window, commits,
     first_seen, last_seen, window_complete)
    VALUES ('R1','claude',NULL,'Opus','4.8','1M',12,'2025-01-01','2025-06-01',1)")
  DBI::dbExecute(con, "INSERT INTO vcs_ai_rule_inventory (tier, tool, ruleset_version)
    VALUES ('D','claude','v1')")
  DBI::dbExecute(con, "INSERT INTO vcs_ai_silent_channels (tier, tool, status, reason, recorded_on)
    VALUES ('B','replit','open','only the commit-author trailer remains','2026-08-01')")

  out <- tempfile("pub_"); dir.create(out)
  io <- list(release_exists = function() FALSE,
             download = function(pattern, dir) FALSE,
             upload = function(path) invisible(NULL))
  publish(io, con, out, "v1", "live", force_full = TRUE)

  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-signals-summary.db"))
  on.exit(DBI::dbDisconnect(scon), add = TRUE)
  for (nm in SUMMARY_EXTRA_TABLES) {
    n <- DBI::dbGetQuery(scon, sprintf('SELECT COUNT(*) AS n FROM "%s"', nm))$n
    expect_true(n > 0, info = paste(nm, "shipped empty"))
  }
  expect_equal(DBI::dbGetQuery(scon, "SELECT family FROM vcs_ai_models")$family, "Opus")
})

test_that("the declared list matches the tables the schema creates", {
  # The list and the schema are two places that can disagree. A name here with
  # no table would be read as NULL and silently skipped.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  present <- DBI::dbGetQuery(con,
    "SELECT name FROM sqlite_master WHERE type='table'")$name
  expect_equal(setdiff(SUMMARY_EXTRA_TABLES, present), character(0))
})

# ---------------------------------------------------------------------------
# The published summary is one asset, clobbered. Losing ground must be refused.
# ---------------------------------------------------------------------------

.mk_summary <- function(path, n_rows, markers = "CLAUDE.md", counts = 5L) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  if (n_rows > 0) {
    df <- data.frame(
      repo_id = sprintf("github.com/o/r%d", seq_len(n_rows)), tool = "claude",
      first_seen_date = "2025-01-01", first_seen_censored = 0L, evidence_tiers = "D",
      markers = markers, authored = 0L,
      authored_commits = counts, assisted_commits = counts,
      last_confirmed_date = "2026-01-01", stringsAsFactors = FALSE)
    DBI::dbWriteTable(con, "vcs_ai_signals", df, append = TRUE)
  }
  path
}

test_that("a build that lost rows is refused rather than clobbering the only copy", {
  prev <- .mk_summary(tempfile(fileext = ".db"), 100L)
  nxt  <- .mk_summary(tempfile(fileext = ".db"), 50L)
  r <- summary_regressions(prev, nxt)
  expect_true(length(r) > 0)
  expect_match(paste(r, collapse = " "), "vcs_ai_signals: 50 rows, was 100")
})

test_that("a build that kept every row but emptied a column is refused too", {
  # The shape that actually happened. A read-modify-write over a subset of
  # columns leaves the row count untouched and nulls the rest, so a row-count
  # check on its own would have watched it happen and said nothing.
  prev <- .mk_summary(tempfile(fileext = ".db"), 100L)
  nxt  <- .mk_summary(tempfile(fileext = ".db"), 100L, markers = NA, counts = NA)
  r <- summary_regressions(prev, nxt)
  expect_true(any(grepl("markers", r)))
  expect_true(any(grepl("authored_commits", r)))
})

test_that("growth publishes, and so does a first build with nothing to compare", {
  prev <- .mk_summary(tempfile(fileext = ".db"), 100L)
  nxt  <- .mk_summary(tempfile(fileext = ".db"), 140L)
  expect_equal(summary_regressions(prev, nxt), character(0))
  expect_equal(summary_regressions(NULL, nxt), character(0))
  expect_equal(summary_regressions(tempfile(fileext = ".db"), nxt), character(0))
})

test_that("ordinary churn is tolerated so the gate is not noise", {
  prev <- .mk_summary(tempfile(fileext = ".db"), 1000L)
  nxt  <- .mk_summary(tempfile(fileext = ".db"), 995L)   # five repos gone: fine
  expect_equal(summary_regressions(prev, nxt), character(0))
})

test_that("the churn allowance is nothing at all on a small table", {
  # The claim above is true of the tables it was measured on and false of every
  # small one the gate also covers. A proportional 2% of eleven rows is 0.22 of
  # a row, so the refuse threshold sits at 10.78 and a single row leaving trips
  # it. Stated as a test because three derived tables of 10, 16 and 34 rows were
  # swept into this rule on the strength of that sentence.
  prev <- .mk_summary(tempfile(fileext = ".db"), 11L)
  nxt  <- .mk_summary(tempfile(fileext = ".db"), 10L)
  expect_true(length(summary_regressions(prev, nxt)) > 0)
})

# ---------------------------------------------------------------------------
# Three tables are rebuilt from scratch on every merge, and a row leaving them
# is how they report progress. They were swept into the no-decrease rule
# because the loop walks whatever tables happen to be in the summary shard.
# ---------------------------------------------------------------------------

.mk_extra <- function(path, silent = NULL, inventory = NULL, models = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  if (!is.null(silent) && nrow(silent) > 0)
    DBI::dbWriteTable(con, "vcs_ai_silent_channels", silent[, c(
      "tier", "tool", "status", "reason", "recorded_on")], append = TRUE)
  if (!is.null(inventory) && nrow(inventory) > 0) {
    inventory$ruleset_version <- "v1"
    DBI::dbWriteTable(con, "vcs_ai_rule_inventory",
                      inventory[, c("tier", "tool", "ruleset_version")], append = TRUE)
  }
  if (!is.null(models) && nrow(models) > 0)
    DBI::dbWriteTable(con, "vcs_ai_models", models, append = TRUE)
  path
}

.mk_models <- function(repos, rows_each, tool = "claude", complete = 1L) {
  do.call(rbind, lapply(seq_along(repos), function(i) data.frame(
    repo_id = repos[i], tool = tool, provider = NA_character_,
    family = sprintf("Opus%d", seq_len(rows_each[i])), version = "4.8",
    context_window = NA_character_, commits = 3L,
    first_seen = "2025-01-01", last_seen = "2026-01-01",
    window_complete = as.integer(complete),
    stringsAsFactors = FALSE)))
}

# The table as the last weekly merge wrote it, under the list that was in the
# config that week. B/devin was an open question then and is retired now.
.kn_before_devin_answered <- function() {
  rbind(AI_SILENT_CHANNELS_KNOWN, data.frame(
    tier = "B", tool = "devin", status = "open",
    reason = paste("rule added 2026-08-01, unscanned; and it can only fire on",
                   "a repo some OTHER tool already flagged"),
    recorded_on = "2026-08-01", stringsAsFactors = FALSE))
}

test_that("a silent channel that started detecting is progress, not a loss", {
  # The merge of 2026-08-16. devin's tier-B trailer matched for the first time,
  # on a repository the PR channel had already flagged, which is precisely the
  # event the recorded open question predicted. The table is rebuilt every run
  # from the channels still at zero, so answering one of them is the only shape
  # progress can take, and the gate read eleven rows becoming ten as data loss
  # and refused to publish a build in which nothing had been lost.
  kn <- AI_SILENT_CHANNELS_KNOWN
  prev <- .mk_extra(tempfile(fileext = ".db"), silent = kn)
  nxt  <- .mk_extra(tempfile(fileext = ".db"), silent = kn[-1, , drop = FALSE])
  expect_equal(summary_regressions(prev, nxt), character(0))
})

test_that("a silent-channel table published empty is still refused", {
  # The other direction. Every channel answering at once is a milestone nobody
  # should reach by accident, and a builder that wrote nothing looks the same.
  kn <- AI_SILENT_CHANNELS_KNOWN
  prev <- .mk_extra(tempfile(fileext = ".db"), silent = kn)
  nxt  <- .mk_extra(tempfile(fileext = ".db"))
  expect_match(paste(summary_regressions(prev, nxt), collapse = " "),
               "vcs_ai_silent_channels")
})

test_that("the table the last merge left republishes against a config that moved", {
  # The production shape, and none of the tests above reach it: both sides of
  # every one of them come out of the AI_SILENT_CHANNELS_KNOWN in force right
  # now, so the case where the published table was written by an EARLIER list
  # was never exercised.
  #
  # Only the weekly AI merge rebuilds this table. The daily update seeds every
  # SUMMARY_EXTRA_TABLE verbatim out of the recent shard and publishes it
  # again, so between a retirement in the config and the next merge, every
  # daily run hands the gate a table written under the older list. Here that is
  # B/devin, carried as an open question by the merge that ran before it
  # started detecting, and retired from the list in the same change that
  # rewrote this rule. Nothing about that build is wrong, and refusing it is
  # the same refusal-of-a-correct-build this gate was rewritten to stop.
  was  <- .kn_before_devin_answered()
  prev <- .mk_extra(tempfile(fileext = ".db"), silent = was)
  nxt  <- .mk_extra(tempfile(fileext = ".db"), silent = was)
  expect_equal(summary_regressions(prev, nxt), character(0))
})

test_that("the merge that retires an answered claim publishes against the older table", {
  # The other half of the same skew, one week later: the published table is the
  # one the previous merge wrote under the older list, and this merge rebuilds
  # it from the list as it stands, so the retired entry leaves. Eleven rows
  # become ten, which is the run that was refused, and every remaining row is
  # recorded in the list in force.
  prev <- .mk_extra(tempfile(fileext = ".db"), silent = .kn_before_devin_answered())
  nxt  <- .mk_extra(tempfile(fileext = ".db"), silent = AI_SILENT_CHANNELS_KNOWN)
  expect_equal(summary_regressions(prev, nxt), character(0))
})

test_that("a row the outgoing build introduced is checked against the list in force", {
  # The carry-forward above is an exemption for rows that were already
  # published, not an exemption for the table. A row this build put there for
  # the first time has no earlier authority to inherit.
  was  <- .kn_before_devin_answered()
  bad  <- was[1, , drop = FALSE]
  bad$tool <- "nosuchtool"; bad$status <- "genuine"
  prev <- .mk_extra(tempfile(fileext = ".db"), silent = was)
  nxt  <- .mk_extra(tempfile(fileext = ".db"), silent = rbind(was, bad))
  expect_match(paste(summary_regressions(prev, nxt), collapse = " "), "nosuchtool")
})

test_that("the gate does not stand down when the list it checks against is missing", {
  # A guard that quietly softens when a dependency is missing is how three
  # tables got the wrong rule in the first place. With no list in scope the
  # strict reading is that nothing is recorded, so every row this build
  # introduced has to say for itself that no claim exists.
  kn <- AI_SILENT_CHANNELS_KNOWN
  withr::defer(assign("AI_SILENT_CHANNELS_KNOWN", kn, envir = globalenv()))
  prev <- .mk_extra(tempfile(fileext = ".db"), silent = kn[1, , drop = FALSE])
  nxt  <- .mk_extra(tempfile(fileext = ".db"), silent = kn)
  rm("AI_SILENT_CHANNELS_KNOWN", envir = globalenv())
  expect_true(length(summary_regressions(prev, nxt)) > 0)
})

test_that("a silent-channel table the gate cannot read is refused, not waved through", {
  # Written before the status column existed, rows$status comes back NULL, and
  # the subset that asks which rows are unexplained is then empty for reasons
  # that have nothing to do with the rows. The gate passed a table it could not
  # read, silently, which is worse than the loss it was watching for.
  prev <- .mk_extra(tempfile(fileext = ".db"), silent = AI_SILENT_CHANNELS_KNOWN)
  nxt  <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), nxt)
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "DROP TABLE vcs_ai_silent_channels")
  DBI::dbExecute(con, "CREATE TABLE vcs_ai_silent_channels (
    tier TEXT NOT NULL, tool TEXT NOT NULL, reason TEXT, recorded_on TEXT)")
  DBI::dbExecute(con, "INSERT INTO vcs_ai_silent_channels
    VALUES ('B','replit','only the commit-author trailer remains','2026-08-01')")
  DBI::dbDisconnect(con)
  expect_match(paste(summary_regressions(prev, nxt), collapse = " "),
               "vcs_ai_silent_channels")
})

test_that("a silent-channel row nobody recorded and nothing marked unexplained is refused", {
  # The table is only worth publishing while every row is either a dated human
  # claim or an admission that no claim exists. A row that is neither is a
  # channel being reported silent on no authority at all.
  kn <- AI_SILENT_CHANNELS_KNOWN
  bad <- kn[1, , drop = FALSE]
  bad$tool <- "nosuchtool"; bad$status <- "genuine"
  prev <- .mk_extra(tempfile(fileext = ".db"), silent = kn)
  nxt  <- .mk_extra(tempfile(fileext = ".db"), silent = rbind(kn, bad))
  expect_match(paste(summary_regressions(prev, nxt), collapse = " "), "nosuchtool")
})

test_that("retiring a rule shrinks the published inventory without refusing the build", {
  # The inventory is a catalogue derived from the ruleset in source, republished
  # whole on every merge, and this project retires rules on purpose: .positai
  # and .idx were removed because listing markers the classifier never reads
  # made the canary report two channels as silent. Under a no-decrease rule the
  # next retirement takes the pipeline down with it.
  inv <- ai_rule_inventory()
  gone <- data.frame(tier = "D", tool = "retired_marker", stringsAsFactors = FALSE)
  prev <- .mk_extra(tempfile(fileext = ".db"), inventory = rbind(inv, gone))
  nxt  <- .mk_extra(tempfile(fileext = ".db"), inventory = inv)
  expect_equal(summary_regressions(prev, nxt), character(0))
})

test_that("an inventory that dropped a channel the ruleset still has is refused", {
  # The loss the row-count rule was catching here, kept.
  inv <- ai_rule_inventory()
  prev <- .mk_extra(tempfile(fileext = ".db"), inventory = inv)
  nxt  <- .mk_extra(tempfile(fileext = ".db"), inventory = inv[-1, , drop = FALSE])
  expect_match(paste(summary_regressions(prev, nxt), collapse = " "),
               "vcs_ai_rule_inventory")
})

test_that("an inventory published empty is refused, which is the bug that started this", {
  # It shipped as an empty table for weeks: created by the schema step, never
  # filled by the export step. Zero of thirty-four is not a retirement.
  inv <- ai_rule_inventory()
  prev <- .mk_extra(tempfile(fileext = ".db"), inventory = inv)
  nxt  <- .mk_extra(tempfile(fileext = ".db"))
  expect_match(paste(summary_regressions(prev, nxt), collapse = " "),
               "vcs_ai_rule_inventory")
})

test_that("a scan that stopped reading past the first hit is refused", {
  # The loss the row count was there for, and the reason a repository count
  # cannot replace it: every repository stays in the table, carrying one model
  # row where it carried its whole history. config.R records how close this
  # came: the scan asked for a single hit until the page size went in, and
  # "asking for a page rather than a single hit turns the same response into
  # the repository's model history". Put AI_SEARCH_PAGE back to 1 and this is
  # the shape the merge publishes, with the repository count untouched.
  repos <- sprintf("github.com/o/r%d", 1:20)
  prev <- .mk_extra(tempfile(fileext = ".db"), models = .mk_models(repos, rep(5L, 20)))
  nxt  <- .mk_extra(tempfile(fileext = ".db"), models = .mk_models(repos, rep(1L, 20)))
  expect_match(paste(summary_regressions(prev, nxt), collapse = " "), "vcs_ai_models")
})

test_that("a tool whose search was refused this run does not refuse the publish", {
  # The merge deletes every model row a repository has and writes back what the
  # shards brought, and a throttled search brings nothing for that tool: the
  # deep pass counts the refusal and moves on rather than recording a zero. So
  # a (repository, tool) pair leaving the table is the throttle, not a loss of
  # the scan's reach, and it happens often enough that refusing it would red
  # most weeks.
  repos <- sprintf("github.com/o/r%d", 1:20)
  both <- rbind(.mk_models(repos, rep(3L, 20)),
                .mk_models(repos, rep(2L, 20), tool = "codex"))
  prev <- .mk_extra(tempfile(fileext = ".db"), models = both)
  nxt  <- .mk_extra(tempfile(fileext = ".db"), models = .mk_models(repos, rep(3L, 20)))
  expect_equal(summary_regressions(prev, nxt), character(0))
})

test_that("a window the scan never saw the end of is not held to what it showed", {
  # window_complete is 0 when the search reported more hits than the page
  # carried, so those rows are a prefix of a history cut off at whatever page
  # size was in force when they were written. They are not a statement about
  # what the repository has, and nothing can be said to have been lost from
  # them.
  repos <- sprintf("github.com/o/r%d", 1:20)
  prev <- .mk_extra(tempfile(fileext = ".db"),
                    models = .mk_models(repos, rep(5L, 20), complete = 0L))
  nxt  <- .mk_extra(tempfile(fileext = ".db"),
                    models = .mk_models(repos, rep(2L, 20), complete = 0L))
  expect_equal(summary_regressions(prev, nxt), character(0))
})

test_that("model rows disappearing for whole repositories is still refused", {
  # Coverage, the other half. Every repository that carried model rows must
  # still carry them: the table has shipped empty before, and a seed step that
  # forgets it takes every repository out at once.
  repos <- sprintf("github.com/o/r%d", 1:20)
  prev <- .mk_extra(tempfile(fileext = ".db"), models = .mk_models(repos, rep(5L, 20)))
  nxt  <- .mk_extra(tempfile(fileext = ".db"),
                    models = .mk_models(repos[1:10], rep(20L, 10)))  # more rows, half the repos
  expect_match(paste(summary_regressions(prev, nxt), collapse = " "), "vcs_ai_models")
})

test_that("the per-table rules did not exempt the tables the gate was written for", {
  # The whole point of the gate. reconcile_ai_identity wrote back seven of ten
  # columns for months, so markers and both commit counts were destroyed on
  # every merge that folded a renamed repository and nothing compared. Neither
  # that loss nor a plain drop of onset rows may pass, whatever the derived
  # tables are now allowed to do.
  prev <- .mk_summary(tempfile(fileext = ".db"), 2891L)
  nxt  <- .mk_summary(tempfile(fileext = ".db"), 2891L, markers = NA, counts = NA)
  r <- summary_regressions(prev, nxt)
  expect_true(any(grepl("markers", r)))
  expect_true(any(grepl("authored_commits", r)))
  expect_true(any(grepl("assisted_commits", r)))

  dropped <- .mk_summary(tempfile(fileext = ".db"), 2000L)
  expect_match(paste(summary_regressions(prev, dropped), collapse = " "),
               "vcs_ai_signals: 2000 rows, was 2891")
})

test_that("the extra tables survive a publish, a reseed, and a second publish", {
  # They did not. The weekly AI merge published them populated, the recent shard
  # did not carry them, the next daily run seeded from that shard and got empty
  # tables, and the regression gate then refused every publish from that point
  # on. The gate was right; the round trip was missing.
  out <- tempfile("rt_"); dir.create(out)
  uploaded <- character(0)
  io <- list(release_exists = function() TRUE,
             download = function(pattern, dir) {
               src <- file.path(out, "vcs-signals-recent.db")
               if (!file.exists(src)) return(FALSE)
               file.copy(src, file.path(dir, "vcs-signals-recent.db"), overwrite = TRUE)
             },
             upload = function(path) { uploaded <<- c(uploaded, basename(path)); invisible(NULL) })

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "INSERT INTO signals_series VALUES ('R1','2026-07-06','stars',10)")
  DBI::dbExecute(con, "INSERT INTO vcs_ai_rule_inventory (tier, tool, ruleset_version)
    VALUES ('D','claude','v1'), ('B','codex','v1'), ('A','copilot','v1')")
  DBI::dbExecute(con, "INSERT INTO vcs_ai_silent_channels (tier, tool, status, reason, recorded_on)
    VALUES ('B','replit','open','only the author trailer remains','2026-08-01')")
  publish(io, con, out, "v1", "live", force_full = TRUE)
  DBI::dbDisconnect(con)

  # Everything a later run has to work from is the recent shard.
  recent <- file.path(out, "vcs-signals-recent.db")
  expect_true(file.exists(recent))
  rc <- DBI::dbConnect(RSQLite::SQLite(), recent)
  on.exit(DBI::dbDisconnect(rc), add = TRUE)
  for (nm in SUMMARY_EXTRA_TABLES) {
    expect_true(DBI::dbExistsTable(rc, nm), info = paste(nm, "absent from the recent shard"))
  }
  expect_equal(DBI::dbGetQuery(rc, "SELECT COUNT(*) AS n FROM vcs_ai_rule_inventory")$n, 3L)
})

test_that("a full rebuild from the recent window does not truncate published years", {
  # update.yml exposes FORCE_FULL_REBUILD against the live release. The working
  # database at that point holds only the RECENT_WINDOW tail, and every year was
  # re-exported from it and uploaded with --clobber, so a complete 2024 shard
  # was replaced by whatever fragment of 2024 fell inside the window. The
  # per-year archive tags are mirrored from the same run, so nothing survived.
  #
  # The published shard lives in a separate directory and reaches out_dir only
  # through io$download. A version of this test that pre-placed it in out_dir
  # passed with the pull removed, because the fold found the file either way.
  remote <- tempfile("remote_"); dir.create(remote)
  out    <- tempfile("ff_");     dir.create(out)

  pc <- DBI::dbConnect(RSQLite::SQLite(), file.path(remote, "vcs-signals-2024.db"))
  ensure_series_schema(pc)
  DBI::dbWriteTable(pc, "signals_series", data.frame(
    repo_id = "R1", date = sprintf("2024-%02d-01", 1:12), metric = "stars",
    value = 1:12, stringsAsFactors = FALSE), append = TRUE)
  DBI::dbDisconnect(pc)

  rc <- DBI::dbConnect(RSQLite::SQLite(), file.path(remote, "vcs-signals-recent.db"))
  ensure_series_schema(rc); DBI::dbDisconnect(rc)
  writeLines('{"summary":{"years":[2024]}}', file.path(remote, "manifest.json"))

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "INSERT INTO signals_series VALUES ('R1','2024-12-01','stars',12)")

  io <- list(
    release_exists = function() TRUE,
    download = function(pattern, dir) {
      src <- file.path(remote, pattern)
      if (!file.exists(src)) return(FALSE)
      file.copy(src, file.path(dir, basename(pattern)), overwrite = TRUE)
    },
    upload = function(path) invisible(NULL))
  publish(io, con, out, "current", "live", force_full = TRUE)

  yc <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-signals-2024.db"))
  on.exit(DBI::dbDisconnect(yc), add = TRUE)
  expect_equal(DBI::dbGetQuery(yc, "SELECT COUNT(*) AS n FROM signals_series")$n, 12L)
})

test_that("a deliberately purged metric is not resurrected by the fold", {
  # Retiring a mis-named metric is the one case where the published history is
  # meant to lose rows, and only the caller knows that.
  out <- tempfile("pg_"); dir.create(out)
  published <- file.path(out, "vcs-signals-2024.db")
  pc <- DBI::dbConnect(RSQLite::SQLite(), published)
  ensure_series_schema(pc)
  DBI::dbWriteTable(pc, "signals_series", data.frame(
    repo_id = "R1", date = "2024-05-01", metric = c("stars", "typo_metric"),
    value = c(5L, 9L), stringsAsFactors = FALSE), append = TRUE)
  DBI::dbDisconnect(pc)

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "INSERT INTO signals_series VALUES ('R1','2024-05-01','stars',5)")

  io <- list(release_exists = function() TRUE,
             download = function(pattern, dir) TRUE,
             upload = function(path) invisible(NULL))
  publish(io, con, out, "current", "live", force_full = TRUE,
          purged_metrics = "typo_metric")

  yc <- DBI::dbConnect(RSQLite::SQLite(), published)
  on.exit(DBI::dbDisconnect(yc), add = TRUE)
  got <- DBI::dbGetQuery(yc, "SELECT DISTINCT metric FROM signals_series")$metric
  expect_equal(got, "stars")
})

test_that("the build being replaced is kept as one generation of rollback", {
  # The summary is uploaded with --clobber, so a bad build overwrites the only
  # copy of the accumulated onset history. The regression gate stops a build
  # that visibly lost ground; this is for the one that gets past it.
  remote <- tempfile("rem_"); dir.create(remote)
  out    <- tempfile("ret_"); dir.create(out)
  uploaded <- character(0)

  pc <- DBI::dbConnect(RSQLite::SQLite(), file.path(remote, "vcs-signals-summary.db"))
  ensure_repo_schema(pc); ensure_series_schema(pc)
  DBI::dbExecute(pc, "INSERT INTO vcs_ai_signals
    (repo_id, tool, first_seen_date, evidence_tiers, markers, authored_commits)
    VALUES ('R1','claude','2024-01-01','A,D','CLAUDE.md',53)")
  DBI::dbDisconnect(pc)
  rc <- DBI::dbConnect(RSQLite::SQLite(), file.path(remote, "vcs-signals-recent.db"))
  ensure_series_schema(rc); DBI::dbDisconnect(rc)
  writeLines('{"summary":{"years":[]}}', file.path(remote, "manifest.json"))

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "INSERT INTO vcs_ai_signals
    (repo_id, tool, first_seen_date, evidence_tiers, markers, authored_commits)
    VALUES ('R1','claude','2024-01-01','A,D','CLAUDE.md',53),
           ('R2','codex','2025-01-01','B','B',4)")

  io <- list(release_exists = function() TRUE,
             download = function(pattern, dir) {
               src <- file.path(remote, pattern)
               if (!file.exists(src)) return(FALSE)
               file.copy(src, file.path(dir, basename(pattern)), overwrite = TRUE)
             },
             upload = function(path) { uploaded <<- c(uploaded, basename(path)); invisible(NULL) })
  publish(io, con, out, "current", "live")

  expect_true("vcs-signals-summary-prev.db" %in% uploaded)
  # And it holds the OLD build, not a second copy of the new one.
  kept <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-signals-summary-prev.db"))
  on.exit(DBI::dbDisconnect(kept), add = TRUE)
  expect_equal(DBI::dbGetQuery(kept, "SELECT COUNT(*) AS n FROM vcs_ai_signals")$n, 1L)
  expect_equal(DBI::dbGetQuery(kept, "SELECT authored_commits FROM vcs_ai_signals")$authored_commits, 53L)

  m <- jsonlite::fromJSON(file.path(out, "manifest.json"))
  expect_equal(m$summary$previous_summary$asset, "vcs-signals-summary-prev.db")
  expect_match(m$summary$previous_summary$sha256, "^[0-9a-f]{64}$")
})

test_that("a first publish keeps no rollback copy and claims none", {
  out <- tempfile("first_"); dir.create(out)
  uploaded <- character(0)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con); ensure_series_schema(con)
  io <- list(release_exists = function() FALSE,
             download = function(pattern, dir) FALSE,
             upload = function(path) { uploaded <<- c(uploaded, basename(path)); invisible(NULL) })
  publish(io, con, out, "v1", "live", force_full = TRUE)

  expect_false("vcs-signals-summary-prev.db" %in% uploaded)
  m <- jsonlite::fromJSON(file.path(out, "manifest.json"))
  expect_null(m$summary$previous_summary)
})

test_that("a column the published build never had is not read as a loss", {
  # This blocked the daily pipeline. SQLite resolves a double-quoted name that
  # is not a column as a STRING LITERAL rather than raising, so
  #   WHERE "authored_commits" IS NOT NULL
  # became 'authored_commits' IS NOT NULL, true for every row. Against a
  # published database written before the column existed that read as all 2,429
  # rows carrying a value, and the gate refused a publish over a loss that had
  # not happened. The tryCatch around the query was waiting for an error SQLite
  # does not raise.
  prev <- tempfile(fileext = ".db")
  pc <- DBI::dbConnect(RSQLite::SQLite(), prev)
  DBI::dbExecute(pc, "CREATE TABLE vcs_ai_signals (
    repo_id TEXT, tool TEXT, first_seen_date TEXT, evidence_tiers TEXT)")
  for (i in 1:100) {
    DBI::dbExecute(pc, sprintf(
      "INSERT INTO vcs_ai_signals VALUES ('r%d','claude','2025-01-01','D')", i))
  }
  DBI::dbDisconnect(pc)

  # The outgoing build has the columns, all NULL, because no scan has filled
  # them yet. That is the honest state of a freshly added column.
  nxt <- .mk_summary(tempfile(fileext = ".db"), 100L, markers = NA, counts = NA)

  expect_equal(summary_regressions(prev, nxt), character(0))
})

test_that("the old query really did count every row, so the guard is load-bearing", {
  # Pinning the SQLite behaviour itself: if a future version starts raising,
  # this test says so rather than the guard silently becoming redundant.
  p <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (a TEXT)")
  DBI::dbExecute(con, "INSERT INTO t VALUES ('x'), ('y')")
  quoted <- DBI::dbGetQuery(con, 'SELECT COUNT(*) AS n FROM t WHERE "nope" IS NOT NULL')$n
  expect_equal(quoted, 2L)   # the string literal, not an error
  expect_error(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM t WHERE [nope] IS NOT NULL"))
})

test_that("a real loss in a column both builds have is still refused", {
  # The gate must still do its job, or the fix would have removed it.
  prev <- .mk_summary(tempfile(fileext = ".db"), 100L)
  nxt  <- .mk_summary(tempfile(fileext = ".db"), 100L, markers = NA, counts = NA)
  r <- summary_regressions(prev, nxt)
  expect_true(any(grepl("authored_commits", r)))
  expect_true(any(grepl("markers", r)))
})

test_that("a publish and a reseed keep the extra tables, both ways round", {
  # .embed_recent_tables was fixed to carry these into the recent shard and
  # seed_working_db was not, so every run read them back as empty, rebuilt a
  # summary without them, and the gate refused the publish. The earlier test
  # checked only the outbound half, which is why the inbound half stayed broken.
  remote <- tempfile("rt_rem_"); dir.create(remote)
  out    <- tempfile("rt_out_"); dir.create(out)

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "INSERT INTO signals_series VALUES ('R1','2026-07-06','stars',10)")
  DBI::dbExecute(con, "INSERT INTO vcs_ai_rule_inventory (tier, tool, ruleset_version)
    VALUES ('D','claude','v1'), ('B','codex','v1'), ('A','copilot','v1')")
  DBI::dbExecute(con, "INSERT INTO vcs_ai_silent_channels (tier, tool, status, reason, recorded_on)
    VALUES ('B','replit','open','only the author trailer remains','2026-08-01')")
  DBI::dbExecute(con, "INSERT INTO vcs_ai_models
    (repo_id, tool, family, version, commits, window_complete)
    VALUES ('R1','claude','Opus','4.8',12,1)")

  io_pub <- list(release_exists = function() FALSE,
                 download = function(pattern, dir) FALSE,
                 upload = function(path) {
                   file.copy(path, file.path(remote, basename(path)), overwrite = TRUE)
                 })
  publish(io_pub, con, out, "v1", "live", force_full = TRUE)
  DBI::dbDisconnect(con)

  # Now the inbound half: a later run seeds its working DB from what was published.
  work <- tempfile(fileext = ".db")
  seed_dir <- tempfile("rt_seed_"); dir.create(seed_dir)
  io_seed <- list(release_exists = function() TRUE,
                  download = function(pattern, dir) {
                    src <- file.path(remote, pattern)
                    if (!file.exists(src)) return(FALSE)
                    file.copy(src, file.path(dir, basename(pattern)), overwrite = TRUE)
                  },
                  upload = function(path) invisible(NULL))
  seed_working_db(io_seed, seed_dir, work)

  wc <- DBI::dbConnect(RSQLite::SQLite(), work)
  on.exit(DBI::dbDisconnect(wc), add = TRUE)
  for (nm in SUMMARY_EXTRA_TABLES) {
    n <- DBI::dbGetQuery(wc, sprintf('SELECT COUNT(*) AS n FROM "%s"', nm))$n
    expect_true(n > 0, info = paste(nm, "came back empty after the round trip"))
  }
  expect_equal(DBI::dbGetQuery(wc, "SELECT COUNT(*) AS n FROM vcs_ai_rule_inventory")$n, 3L)
})
