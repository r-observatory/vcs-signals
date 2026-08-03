# Two commit counts, and the three states each of them can be in.
#
# "AI commits" is not a quantity this data has. A repository can have an agent
# that authored nothing while a person credited it 37 times, so one blended
# figure would describe neither. The counts are stored apart and never summed.

test_that("the empty frame carries both counts, so a consumer cannot miss one", {
  e <- .ai_empty_signals()
  expect_true(all(c("authored_commits", "assisted_commits") %in% names(e)))
  expect_type(e$authored_commits, "integer")
  expect_type(e$assisted_commits, "integer")
})

test_that("a measured zero is kept as zero and never becomes not-searched", {
  # The direction people forget. Claude is a bot-identity tool, so on a flagged
  # repository the author search really ran; returning NULL there would claim we
  # never looked.
  g <- data.frame(repo_id = "r", tool = "claude", first_seen_date = "2025-01-01",
                  first_seen_censored = 0L, evidence_tiers = "B", markers = "B",
                  authored = 0L, authored_commits = 0L, assisted_commits = 37L,
                  last_confirmed_date = "2026-01-01", stringsAsFactors = FALSE)
  out <- .ai_reduce_group(g)
  expect_equal(out$authored_commits, 0L)
  expect_equal(out$assisted_commits, 37L)
  expect_false(is.na(out$authored_commits))
})

test_that("a search that never ran stays NA rather than counting as zero", {
  g <- data.frame(repo_id = "r", tool = "cline", first_seen_date = "2025-01-01",
                  first_seen_censored = 0L, evidence_tiers = "D", markers = "CLAUDE.md",
                  authored = 0L, authored_commits = NA_integer_,
                  assisted_commits = NA_integer_,
                  last_confirmed_date = "2026-01-01", stringsAsFactors = FALSE)
  out <- .ai_reduce_group(g)
  expect_true(is.na(out$authored_commits))
  expect_true(is.na(out$assisted_commits))
})

test_that("folding several evidence rows takes the max, because summing double-counts", {
  # Two patterns for one tool can match the same commit, so a sum would count it
  # twice. Max is a floor and stays a floor.
  g <- data.frame(
    repo_id = "r", tool = "claude", first_seen_date = c("2025-01-01", "2025-02-01"),
    first_seen_censored = 0L, evidence_tiers = c("A", "B"), markers = c("A", "B"),
    authored = c(1L, 0L),
    authored_commits = c(28L, NA_integer_),
    assisted_commits = c(NA_integer_, 12L),
    last_confirmed_date = "2026-01-01", stringsAsFactors = FALSE)
  out <- .ai_reduce_group(g)
  expect_equal(out$authored_commits, 28L)
  expect_equal(out$assisted_commits, 12L)
  expect_equal(nrow(out), 1L)
})

test_that("a real count beats an absent one when the rows disagree about being asked", {
  g <- data.frame(
    repo_id = "r", tool = "claude", first_seen_date = "2025-01-01",
    first_seen_censored = 0L, evidence_tiers = "A", markers = "A",
    authored = 1L,
    authored_commits = c(NA_integer_, 5L, NA_integer_),
    assisted_commits = NA_integer_,
    last_confirmed_date = "2026-01-01", stringsAsFactors = FALSE)
  out <- .ai_reduce_group(g)
  expect_equal(out$authored_commits, 5L)
})

test_that("the authored flag is derived from the count rather than kept beside it", {
  # Two fields that can disagree will eventually disagree. Tier D throughout,
  # because tier A sets the flag on its own and would mask the derivation.
  g <- data.frame(repo_id = "r", tool = "claude", first_seen_date = "2025-01-01",
                  first_seen_censored = 0L, evidence_tiers = "D", markers = "CLAUDE.md",
                  authored = 0L, authored_commits = 4L, assisted_commits = NA_integer_,
                  last_confirmed_date = "2026-01-01", stringsAsFactors = FALSE)
  expect_equal(.ai_reduce_group(g)$authored, 1L)

  g2 <- g; g2$authored <- 1L; g2$authored_commits <- 0L
  expect_equal(.ai_reduce_group(g2)$authored, 0L)
})

test_that("build_ai_detail carries the counts off the evidence rows", {
  ev <- data.frame(tool = "claude", tier = "A", marker = "A", agnostic = 0L,
                   authored = 1L, authored_commits = 28L,
                   assisted_commits = NA_integer_, stringsAsFactors = FALSE)
  onsets <- data.frame(tool = "claude", marker = "A",
                       first_seen_date = "2025-01-01", first_seen_censored = 0L,
                       stringsAsFactors = FALSE)
  got <- build_ai_detail("github.com/o/r", ev, onsets, "2026-01-01")
  expect_equal(got$authored_commits, 28L)
  expect_true(is.na(got$assisted_commits))
})

test_that("evidence with no count columns still reduces, so old shards keep working", {
  ev <- data.frame(tool = "claude", tier = "D", marker = "CLAUDE.md", agnostic = 0L,
                   stringsAsFactors = FALSE)
  onsets <- data.frame(tool = "claude", marker = "CLAUDE.md",
                       first_seen_date = "2025-01-01", first_seen_censored = 0L,
                       stringsAsFactors = FALSE)
  got <- build_ai_detail("github.com/o/r", ev, onsets, "2026-01-01")
  expect_equal(nrow(got), 1L)
  expect_true(is.na(got$authored_commits))
})

test_that("run_deep writes both counts through to the shard, fibr-shaped", {
  # The spec's worked example: the agent authored nothing here and a person
  # credited it 37 times. A single blended figure would be wrong either way it
  # rounded, so both must arrive and neither may stand in for the other.
  out <- tempfile("out_"); dir.create(out)
  write_flagged_partial(file.path(out, "vcs-ai-flagged-roster.db"),
    data.frame(repo_id = "github.com/o/fibr", owner = "o", name = "fibr", node_id = "R_1",
               is_fork = 0L, parent = NA_character_, pr_onset_date = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(repo_id = "github.com/o/fibr", tool = "claude", tier = "D",
               marker = "CLAUDE.md", agnostic = 0L, stringsAsFactors = FALSE))
  io <- list(
    graphql = function(query) list(data = list(repository = list(defaultBranchRef = list(
      target = list(history = list(pageInfo = list(endCursor = "", hasNextPage = FALSE),
        nodes = list(list(committedDate = "2025-05-01T00:00:00Z")))))))),
    search_hit = function(owner, name, query, delay = 0) {
      if (grepl("^author(-email)?:", query))     # the agent authored nothing here
        return(list(date = NA_character_, message = NA_character_, author = NA_character_,
                    total_count = 0L, unavailable = FALSE))
      if (!grepl("Co-Authored-By: Claude", query, fixed = TRUE))
        return(list(date = NA_character_, message = NA_character_, author = NA_character_,
                    total_count = 0L, unavailable = FALSE))
      list(date = "2025-06-01T00:00:00Z",
           message = "feat: x\n\nCo-authored-by: Claude <noreply@anthropic.com>",
           author = "Jane", total_count = 37L, unavailable = FALSE)
    })
  suppressMessages(run_deep(io, out, file.path(out, "vcs-ai-flagged-roster.db"), 0, 1,
                            marker_delay = 0, search_delay = 0))
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-ai-shard-0.db"))
  on.exit(DBI::dbDisconnect(scon))
  got <- DBI::dbReadTable(scon, "vcs_ai_signals")
  got <- got[got$tool == "claude", ]
  expect_equal(got$assisted_commits, 37L)
  expect_equal(got$authored_commits, 0L)   # measured zero, not "never asked"
  expect_false(is.na(got$authored_commits))
  expect_equal(got$authored, 0L)           # derived: it authored none
})

test_that("a refused count is not stored as zero", {
  out <- tempfile("out_"); dir.create(out)
  write_flagged_partial(file.path(out, "vcs-ai-flagged-roster.db"),
    data.frame(repo_id = "github.com/o/r", owner = "o", name = "r", node_id = "R_1",
               is_fork = 0L, parent = NA_character_, pr_onset_date = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(repo_id = "github.com/o/r", tool = "claude", tier = "D",
               marker = "CLAUDE.md", agnostic = 0L, stringsAsFactors = FALSE))
  io <- list(
    graphql = function(query) list(data = list(repository = list(defaultBranchRef = list(
      target = list(history = list(pageInfo = list(endCursor = "", hasNextPage = FALSE),
        nodes = list(list(committedDate = "2025-05-01T00:00:00Z")))))))),
    search_hit = function(owner, name, query, delay = 0)
      list(date = NA_character_, message = NA_character_, author = NA_character_,
           total_count = NA_integer_, unavailable = TRUE))
  suppressMessages(run_deep(io, out, file.path(out, "vcs-ai-flagged-roster.db"), 0, 1,
                            marker_delay = 0, search_delay = 0))
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-ai-shard-0.db"))
  on.exit(DBI::dbDisconnect(scon))
  got <- DBI::dbReadTable(scon, "vcs_ai_signals")
  expect_true(is.na(got$authored_commits))
  expect_true(is.na(got$assisted_commits))
})

test_that("folding two repos onto one identity keeps every column it did not read", {
  # reconcile_ai_identity named seven of the table's ten columns, deleted the
  # rows, and wrote the seven back. Markers and both commit counts went with
  # them, and select_incremental_repos never revisits a published repo, so the
  # loss was permanent and accumulated on every merge.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con); ensure_series_schema(con)

  # Two repo_ids sharing one immutable node_id: a rename, which is exactly what
  # this function exists to fold together.
  DBI::dbExecute(con, "INSERT INTO repos (repo_id, node_id, host, host_domain, owner, name,
      name_with_owner, supported, n_packages, first_seen, last_seen, status)
    VALUES ('github.com/o/old','R_1','github','github.com','o','old','o/old',1,1,
            '2026-01-01','2026-06-01','renamed'),
           ('github.com/o/new','R_1','github','github.com','o','new','o/new',1,1,
            '2026-02-01','2026-07-01','active')")
  DBI::dbExecute(con, "INSERT INTO vcs_ai_signals
      (repo_id, tool, first_seen_date, first_seen_censored, evidence_tiers, markers,
       authored, authored_commits, assisted_commits, last_confirmed_date)
    VALUES ('github.com/o/old','claude','2025-01-01',0,'A,D','CLAUDE.md',1,53,12,'2026-06-01')")

  reconcile_ai_identity(con)
  got <- DBI::dbGetQuery(con, "SELECT * FROM vcs_ai_signals")

  expect_equal(nrow(got), 1L)
  expect_equal(got$repo_id, "github.com/o/new")     # folded onto the canonical id
  expect_equal(got$authored_commits, 53L)           # and the measurement survived
  expect_equal(got$assisted_commits, 12L)
  expect_equal(got$markers, "CLAUDE.md")
  expect_equal(got$first_seen_date, "2025-01-01")
})

test_that("a fresh zero does not retract standing tier-A evidence", {
  # Rows written before the counts existed carry authored = 1 and no number. A
  # full_gate re-scan measures the tier-A search honestly, and where it now
  # returns zero the fold published evidence_tiers "A,D" beside authored = 0: a
  # row that contradicts itself. Tiers are a union that never expires, so the
  # tier cannot be retracted by a later search either.
  g <- data.frame(repo_id = "r", tool = "claude", first_seen_date = "2025-01-01",
    first_seen_censored = 0L, evidence_tiers = c("A,D", "D"), markers = c("A", "CLAUDE.md"),
    authored = c(1L, 0L), authored_commits = c(NA_integer_, 0L),
    assisted_commits = NA_integer_, last_confirmed_date = "2026-01-01",
    stringsAsFactors = FALSE)
  out <- .ai_reduce_group(g)
  expect_true(grepl("A", out$evidence_tiers, fixed = TRUE))
  expect_equal(out$authored, 1L)
})

test_that("a measured zero with no tier-A evidence still reads as none", {
  # The flag must still be able to say no, or it is not derived from anything.
  g <- data.frame(repo_id = "r", tool = "claude", first_seen_date = "2025-01-01",
    first_seen_censored = 0L, evidence_tiers = "B,D", markers = "B",
    authored = 0L, authored_commits = 0L, assisted_commits = 37L,
    last_confirmed_date = "2026-01-01", stringsAsFactors = FALSE)
  expect_equal(.ai_reduce_group(g)$authored, 0L)
})

test_that("a positive count still wins outright", {
  g <- data.frame(repo_id = "r", tool = "copilot", first_seen_date = "2025-01-01",
    first_seen_censored = 0L, evidence_tiers = "D", markers = "copilot-instructions.md",
    authored = 0L, authored_commits = 7L, assisted_commits = NA_integer_,
    last_confirmed_date = "2026-01-01", stringsAsFactors = FALSE)
  expect_equal(.ai_reduce_group(g)$authored, 1L)
})
