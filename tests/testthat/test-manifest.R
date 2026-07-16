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
    vcs_ai_signals      = 0L,
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
