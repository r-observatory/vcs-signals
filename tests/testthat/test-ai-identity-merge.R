# scripts/ai_backfill.R uses repo-root-relative source() calls (a CLI-entry script), so
# it must be sourced with cwd temporarily chdir'd to the repo root, like test-ai-backfill.R.
.aiim_wd <- setwd(.repo_root)
source(file.path(.repo_root, "scripts", "ai_backfill.R"))
setwd(.aiim_wd)

# Two CRAN packages whose DESCRIPTION URLs name two slugs of ONE GitHub repository,
# both still listed, so both repo_ids stay active: the uchidamizuki/japanstat and
# uchidamizuki/jpstat shape. The fold used to collapse them onto japanstat on every
# merge, and the weekly confirmation pass then wrote jpstat back as rows with a date
# and nothing else, which cost jpstat its summary rollup.
.JAP <- "github.com/u/japanstat"
.JP  <- "github.com/u/jpstat"

# Local-release fake io (upload copies in, download copies out), from helper-setup.R.
.sibling_release_io <- function() {
  rel <- tempfile("rel_"); dir.create(rel)
  local_release_io(rel)
}

.seed_siblings <- function(io, ai_sql) {
  out0 <- tempfile("o0_"); dir.create(out0)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out0, "w.db"))
  on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, sprintf("INSERT INTO repos VALUES
    ('%s','R_kgDOGZZTdA','github','github.com','u','japanstat','u/japanstat',1,1,'2026-01-01','2026-09-14','active'),
    ('%s','R_kgDOGZZTdA','github','github.com','u','jpstat','u/jpstat',1,1,'2026-01-01','2026-09-14','active')",
    .JAP, .JP))
  DBI::dbExecute(con, sprintf("INSERT INTO repo_packages VALUES
    ('%s','japanstat','cran','both'), ('%s','jpstat','cran','both')", .JAP, .JP))
  for (q in ai_sql) DBI::dbExecute(con, q)
  suppressMessages(publish(io, con, out0, tag = "current", source_kind = "live", force_full = TRUE,
                           base_generation = ""))
}

.full_row <- function(repo, tool, date, tiers, markers, assisted, last)
  sprintf("INSERT INTO vcs_ai_signals VALUES ('%s','%s','%s',0,'%s','%s',0,%s,%s,'%s')",
          repo, tool, date, tiers, markers,
          if (tool == "agents-md") "NULL" else "0", assisted, last)
.hollow_row <- function(repo, tool, last)
  sprintf("INSERT INTO vcs_ai_signals (repo_id,tool,first_seen_censored,authored,last_confirmed_date)
           VALUES ('%s','%s',0,0,'%s')", repo, tool, last)

.published <- function(io) {
  chk <- tempfile("chk_"); dir.create(chk)
  io$download("vcs-signals-summary.db", chk)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(chk, "vcs-signals-summary.db"))
  on.exit(DBI::dbDisconnect(con))
  list(ai = DBI::dbReadTable(con, "vcs_ai_signals"),
       summary = DBI::dbGetQuery(con,
         "SELECT package, repo_id, ai_markers_detected, ai_tools FROM vcs_signals_summary"))
}

# The shard the incremental gate writes: a confirmation for every published key the
# cheap pass saw again this week. Both slugs are scanned, since both are active.
.confirm_parts <- function(io, today) {
  pub <- .published(io)$ai
  ev <- data.frame(repo_id = rep(c(.JAP, .JP), each = 2), tool = rep(c("claude", "agents-md"), 2),
                   tier = "D", marker = rep(c(".claude", "AGENTS.md"), 2),
                   agnostic = rep(c(0L, 1L), 2), stringsAsFactors = FALSE)
  parts <- tempfile("parts_"); dir.create(parts)
  export_ai_shard(file.path(parts, "vcs-ai-shard-confirm.db"),
                  select_confirmation_rows(ev, pub, today))
  list(parts = parts, ev = ev, pub = pub)
}

.merge_prior_count <- function(msgs) {
  line <- grep("^ai merge: [0-9]+ prior", msgs, value = TRUE)
  as.integer(sub("^ai merge: ([0-9]+) prior.*$", "\\1", trimws(line[1])))
}

.hollow <- function(ai) is.na(ai$first_seen_date) & is.na(ai$evidence_tiers)

test_that("two active slugs of one repository keep full rows through weekly confirmations", {
  io <- .sibling_release_io()
  .seed_siblings(io, c(
    .full_row(.JAP, "claude", "2026-06-22T12:21:43.000+09:00", "B,D", ".claude,B,rbuildignore:.claude", 12, "2026-09-06"),
    .full_row(.JP,  "claude", "2026-06-22T12:21:43.000+09:00", "B,D", ".claude,B,rbuildignore:.claude", 12, "2026-09-06"),
    .full_row(.JAP, "agents-md", "2026-07-15T09:06:09Z", "D", "AGENTS.md", "NULL", "2026-09-06"),
    .full_row(.JP,  "agents-md", "2026-07-15T09:06:09Z", "D", "AGENTS.md", "NULL", "2026-09-06")))

  for (today in c("2026-09-13", "2026-09-20")) {            # the fold repeated every week
    cp <- .confirm_parts(io, today)
    msgs <- testthat::capture_messages(run_merge(io, tempfile("m_"), cp$parts))
    got <- .published(io)

    expect_equal(.merge_prior_count(msgs), nrow(cp$pub), info = today)   # nothing folded away
    expect_equal(nrow(got$ai), nrow(cp$pub), info = today)
    expect_false(any(.hollow(got$ai)), info = today)
    expect_equal(sort(unique(got$ai$last_confirmed_date)), today, info = today)
    jp <- got$summary[got$summary$package == "jpstat", ]
    expect_identical(as.integer(jp$ai_markers_detected), 1L, info = today)
    expect_equal(jp$ai_tools, "claude", info = today)
  }
})

test_that("a hollow row already published on an active sibling heals on the next merge without a deep scan", {
  io <- .sibling_release_io()
  # The live state: japanstat carries the evidence, jpstat carries only dates.
  .seed_siblings(io, c(
    .full_row(.JAP, "claude", "2026-06-22T12:21:43.000+09:00", "B,D", ".claude,B,rbuildignore:.claude", 12, "2026-09-13"),
    .full_row(.JAP, "agents-md", "2026-07-15T09:06:09Z", "D", "AGENTS.md", "NULL", "2026-09-13"),
    .hollow_row(.JP, "claude", "2026-09-13"),
    .hollow_row(.JP, "agents-md", "2026-09-13")))

  cp <- .confirm_parts(io, "2026-09-20")
  # A hollow key counts as published, so the incremental gate never schedules the
  # deep scan that could rebuild it. Healing has to come from the sibling.
  flagged <- data.frame(repo_id = c(.JAP, .JP), stringsAsFactors = FALSE)
  expect_length(select_incremental_repos(flagged, cp$ev, cp$pub), 0L)

  msgs <- testthat::capture_messages(run_merge(io, tempfile("m_"), cp$parts))
  got <- .published(io)
  expect_equal(.merge_prior_count(msgs), 4L)
  expect_equal(nrow(got$ai), 4L)
  expect_false(any(.hollow(got$ai)))
  jap <- got$ai[got$ai$repo_id == .JAP, ]; jp <- got$ai[got$ai$repo_id == .JP, ]
  for (tl in c("claude", "agents-md")) {
    expect_equal(jp$first_seen_date[jp$tool == tl], jap$first_seen_date[jap$tool == tl], info = tl)
    expect_equal(jp$markers[jp$tool == tl], jap$markers[jap$tool == tl], info = tl)
    expect_equal(jp$evidence_tiers[jp$tool == tl], jap$evidence_tiers[jap$tool == tl], info = tl)
  }
  expect_equal(jp$assisted_commits[jp$tool == "claude"], 12L)
  expect_identical(as.integer(got$summary$ai_markers_detected[got$summary$package == "jpstat"]), 1L)
})

test_that("a confirmation for a key the merge holds no row for creates nothing", {
  io <- .sibling_release_io()
  .seed_siblings(io, .full_row(.JAP, "claude", "2026-06-22T12:21:43.000+09:00", "B,D", ".claude,B", 12, "2026-09-13"))
  # The gate read a published copy that still had a jpstat row; the release the
  # merge seeds from does not. The confirmation must not bring the key back empty.
  stale <- data.frame(repo_id = c(.JAP, .JP), tool = "claude", first_seen_date = "2026-06-22",
                      first_seen_censored = 0L, evidence_tiers = "D", authored = 0L,
                      last_confirmed_date = "2026-09-13", stringsAsFactors = FALSE)
  ev <- data.frame(repo_id = c(.JAP, .JP), tool = "claude", tier = "D", marker = ".claude",
                   agnostic = 0L, stringsAsFactors = FALSE)
  parts <- tempfile("parts_"); dir.create(parts)
  export_ai_shard(file.path(parts, "vcs-ai-shard-confirm.db"),
                  select_confirmation_rows(ev, stale, "2026-09-20"))

  suppressMessages(run_merge(io, tempfile("m_"), parts))
  got <- .published(io)$ai
  expect_equal(got$repo_id, .JAP)
  expect_equal(got$last_confirmed_date, "2026-09-20")
  expect_false(any(.hollow(got)))
  # Dropping it loses nothing for good: the key is not published, so the next gate
  # deep-scans jpstat, which is still active and still flagged, and dates it.
  flagged <- data.frame(repo_id = c(.JAP, .JP), stringsAsFactors = FALSE)
  expect_equal(select_incremental_repos(flagged, ev, got), .JP)
})
