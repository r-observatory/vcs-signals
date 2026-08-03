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
