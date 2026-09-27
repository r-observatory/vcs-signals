test_that("ensure_series_schema creates vcs_ai_signals with the (repo_id, tool) PK", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  ensure_series_schema(con)
  expect_true(DBI::dbExistsTable(con, "vcs_ai_signals"))
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(vcs_ai_signals)")
  expect_setequal(cols$name, c("repo_id","tool","first_seen_date","first_seen_censored",
                               "evidence_tiers","markers","authored",
                               "authored_commits","assisted_commits","last_confirmed_date",
                               "authored_measured_on","assisted_measured_on"))
  pk <- cols$name[cols$pk > 0][order(cols$pk[cols$pk > 0])]
  expect_equal(pk, c("repo_id","tool"))
})

test_that("a database written before markers existed gains the column, keeping its rows", {
  # The onset table is accumulated history: rebuilding it to add a column would
  # discard every date the pipeline has ever established. The column arrives empty
  # and fills on the next scan.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE vcs_ai_signals (
    repo_id TEXT NOT NULL, tool TEXT NOT NULL, first_seen_date TEXT,
    first_seen_censored INTEGER NOT NULL DEFAULT 0, evidence_tiers TEXT,
    authored INTEGER NOT NULL DEFAULT 0, last_confirmed_date TEXT,
    PRIMARY KEY (repo_id, tool))")
  DBI::dbExecute(con, "INSERT INTO vcs_ai_signals
    (repo_id, tool, first_seen_date, evidence_tiers) VALUES ('github.com/o/n','claude','2025-06-01','D')")

  ensure_series_schema(con)

  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(vcs_ai_signals)")$name
  expect_true("markers" %in% cols)
  got <- DBI::dbGetQuery(con, "SELECT * FROM vcs_ai_signals")
  expect_equal(nrow(got), 1L, info = "the prior onset survives the migration")
  expect_equal(got$first_seen_date, "2025-06-01")
  expect_true(is.na(got$markers))
})

test_that("the reducer publishes every marker that fired, not just the winner", {
  # A maintainer who commits CLAUDE.md AND gitignores .claude has said something
  # neither marker says alone, and the winning onset would have shown only one.
  prior <- .ai_empty_signals()
  incoming <- data.frame(
    repo_id = rep("github.com/o/n", 2), tool = rep("claude", 2),
    first_seen_date = c("2025-06-01", "2026-07-18T23:59:59Z"),
    first_seen_censored = c(0L, 1L), evidence_tiers = c("D", "D"),
    markers = c("CLAUDE.md", "gitignore:.claude"),
    authored = c(0L, 0L), last_confirmed_date = c("2026-07-30", "2026-07-30"),
    stringsAsFactors = FALSE)
  out <- ai_onset_reducer(prior, incoming)
  expect_equal(nrow(out), 1L)
  expect_equal(out$markers, "CLAUDE.md,gitignore:.claude")
  expect_equal(out$first_seen_date, "2025-06-01", info = "the committed marker still wins the onset")
})

test_that("a database written before the counts existed gains them, keeping its rows", {
  # Same reason markers arrived this way: the onset table is accumulated history,
  # and rebuilding it to add a column would discard every date established so far.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE vcs_ai_signals (
    repo_id TEXT NOT NULL, tool TEXT NOT NULL, first_seen_date TEXT,
    first_seen_censored INTEGER NOT NULL DEFAULT 0, evidence_tiers TEXT,
    markers TEXT, authored INTEGER NOT NULL DEFAULT 0, last_confirmed_date TEXT,
    PRIMARY KEY (repo_id, tool))")
  DBI::dbExecute(con, "INSERT INTO vcs_ai_signals
    (repo_id, tool, first_seen_date, evidence_tiers) VALUES ('R1','claude','2024-01-01','D')")
  ensure_series_schema(con)
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(vcs_ai_signals)")$name
  expect_true(all(c("authored_commits", "assisted_commits") %in% cols))
  got <- DBI::dbGetQuery(con, "SELECT * FROM vcs_ai_signals")
  expect_equal(nrow(got), 1L)
  expect_equal(got$first_seen_date, "2024-01-01")   # history survived
  expect_true(is.na(got$authored_commits))          # arrives as "nobody asked"
})

test_that("a published dev-tooling table gains a newly added marker column", {
  # Adding has_pkgdown to the ruleset would otherwise write rows carrying a
  # column the published table has no room for, because CREATE TABLE IF NOT
  # EXISTS does nothing to a table that already exists.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  old_cols <- setdiff(dev_tooling_columns(), c("has_pkgdown", "has_vignettes"))
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE vcs_dev_tooling (repo_id TEXT PRIMARY KEY, last_scanned TEXT, %s)",
    paste(paste(old_cols, "INTEGER"), collapse = ", ")))
  DBI::dbExecute(con, "INSERT INTO vcs_dev_tooling (repo_id, last_scanned)
                       VALUES ('R1', '2026-07-01')")
  ensure_series_schema(con)

  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(vcs_dev_tooling)")$name
  expect_true(all(dev_tooling_columns() %in% cols))
  got <- DBI::dbGetQuery(con, "SELECT * FROM vcs_dev_tooling")
  expect_equal(nrow(got), 1L)                  # the row survived
  expect_true(is.na(got$has_pkgdown))          # arrives unscanned, not as a 0

  # And a fresh scan's row can actually be written into it.
  fresh <- classify_dev_tooling(c("_pkgdown.yml", "vignettes"), character(0))
  fresh$repo_id <- "R2"; fresh$last_scanned <- "2026-08-02"
  expect_no_error(DBI::dbWriteTable(con, "vcs_dev_tooling",
    fresh[c("repo_id", "last_scanned", dev_tooling_columns())], append = TRUE))
})

test_that("the weekly read keeps its state and findings in seven tables of their own", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)
  have <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type = 'table'")$name
  new <- c("vcs_ai_repo_reads", "vcs_ai_account_counts", "vcs_ai_search_log",
           "vcs_ai_search_coverage", "vcs_ai_review_signals", "vcs_ai_outside_prs",
           "vcs_ai_ruleset_history")
  expect_equal(setdiff(new, have), character(0))
  cols <- function(t) DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", t))$name
  # The review table keeps the signals table's column names so one renderer serves both.
  expect_equal(cols("vcs_ai_review_signals"),
               c("repo_id", "tool", "first_seen_date", "first_seen_censored", "evidence_tiers",
                 "markers", "assisted_commits", "assisted_measured_on", "last_confirmed_date"))
  expect_equal(cols("vcs_ai_outside_prs"),
               c("repo_id", "pr_number", "tool", "found_via", "created_at", "from_fork",
                 "author_association", "last_confirmed_date"))
  expect_equal(tail(cols("vcs_ai_signals"), 2), c("authored_measured_on", "assisted_measured_on"))
  # The log only ever holds one of three outcomes from one of two sources.
  expect_error(DBI::dbExecute(con, "INSERT INTO vcs_ai_search_log VALUES
    ('r','msg.x',1,'v','2026-10-01','maybe',NULL,NULL,NULL,NULL,'search')"))
  expect_error(DBI::dbExecute(con, "INSERT INTO vcs_ai_search_log VALUES
    ('r','msg.x',1,'v','2026-10-01','hit',1,1,0,'2026-01-01','guess')"))
})

test_that("a published signals table from before the measured dates gains them and keeps its rows", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE vcs_ai_signals (
    repo_id TEXT NOT NULL, tool TEXT NOT NULL, first_seen_date TEXT,
    first_seen_censored INTEGER NOT NULL DEFAULT 0, evidence_tiers TEXT, markers TEXT,
    authored INTEGER NOT NULL DEFAULT 0, authored_commits INTEGER, assisted_commits INTEGER,
    last_confirmed_date TEXT, PRIMARY KEY (repo_id, tool))")
  DBI::dbExecute(con, "INSERT INTO vcs_ai_signals (repo_id, tool, first_seen_date, authored_commits)
    VALUES ('github.com/o/n', 'claude', '2025-06-01', 7)")
  ensure_series_schema(con)
  got <- DBI::dbGetQuery(con, "SELECT * FROM vcs_ai_signals")
  expect_equal(nrow(got), 1L)
  expect_equal(got$authored_commits, 7L)
  expect_true(is.na(got$authored_measured_on))
  expect_true(is.na(got$assisted_measured_on))
})

test_that("a shard written without the measured dates still folds", {
  prior <- .ai_empty_signals()
  prior[1, "repo_id"] <- "github.com/o/n"; prior$tool <- "claude"; prior$first_seen_date <- "2025-06-01"
  prior$first_seen_censored <- 0L; prior$evidence_tiers <- "D"; prior$markers <- "CLAUDE.md"
  prior$authored <- 0L; prior$authored_measured_on <- "2026-09-27"
  incoming <- data.frame(repo_id = "github.com/o/n", tool = "claude", first_seen_date = "2026-10-04T23:59:59Z",
                         first_seen_censored = 1L, evidence_tiers = "PR", authored = 0L,
                         last_confirmed_date = "2026-10-04", stringsAsFactors = FALSE)
  out <- ai_onset_reducer(prior, incoming)
  expect_equal(nrow(out), 1L)
  expect_equal(out$evidence_tiers, "D,PR")
  expect_equal(out$authored_measured_on, "2026-09-27")
  expect_equal(names(out), names(.ai_empty_signals()))
})
