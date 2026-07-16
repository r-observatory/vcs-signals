test_that("export_summary_shard writes the ai_signals_df param to vcs_ai_signals", {
  tmp <- tempfile(fileext = ".db")
  ai <- data.frame(repo_id="github.com/o/r", tool="claude", first_seen_date="2024-03-01",
                   first_seen_censored=0L, evidence_tiers="A", authored=1L,
                   last_confirmed_date="2025-01-01", stringsAsFactors=FALSE)
  empty <- function(cols) do.call(data.frame, c(setNames(rep(list(character()), length(cols)), cols),
                                                list(stringsAsFactors = FALSE)))
  export_summary_shard(tmp,
    summary_df = empty(c("package","origin","repo_id")),
    repos_df = empty(c("repo_id","first_seen","last_seen")),
    repo_packages_df = empty(c("repo_id","package","origin")),
    ai_signals_df = ai)
  scon <- DBI::dbConnect(RSQLite::SQLite(), tmp); on.exit(DBI::dbDisconnect(scon))
  got <- DBI::dbReadTable(scon, "vcs_ai_signals")
  expect_equal(nrow(got), 1); expect_equal(got$tool, "claude")
})

test_that("vcs_ai_signals survives a full publish -> re-seed round trip", {
  # Fake io backed by a local 'release' dir: upload copies in, download copies out.
  # Model on the fake io in test-publisher.R; reuse its io builder if present.
  rel <- tempfile("rel_"); dir.create(rel)
  io <- list(
    release_exists = function() length(list.files(rel)) > 0,
    download = function(pattern, dir) {
      f <- list.files(rel, pattern = utils::glob2rx(pattern), full.names = TRUE)
      if (!length(f)) return(FALSE)
      file.copy(f, file.path(dir, basename(f)), overwrite = TRUE); TRUE },
    upload = function(path) { file.copy(path, file.path(rel, basename(path)), overwrite = TRUE); TRUE })

  out1 <- tempfile("o1_"); dir.create(out1)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out1, "w.db"))
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "INSERT INTO vcs_ai_signals (repo_id, tool, first_seen_date, first_seen_censored, evidence_tiers, authored, last_confirmed_date) VALUES ('github.com/o/r','claude','2024-03-01',0,'A',1,'2025-01-01')")
  publish(io, con, out1, tag = "current", source_kind = "live", force_full = TRUE)
  DBI::dbDisconnect(con)

  out2 <- tempfile("o2_"); dir.create(out2)
  w2 <- file.path(out2, "w.db")
  seed_working_db(io, out2, w2)
  scon <- DBI::dbConnect(RSQLite::SQLite(), w2); on.exit(DBI::dbDisconnect(scon))
  got <- DBI::dbReadTable(scon, "vcs_ai_signals")
  expect_equal(nrow(got), 1); expect_equal(got$tool, "claude")   # survived seed<->embed<->publish
})
