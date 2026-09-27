# Seed two repos sharing a node_id: the old slug (retired) and the new slug (active).
seed_pair <- function(con, nid = "R_kgDO1") {
  DBI::dbExecute(con, "INSERT INTO repos (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status) VALUES
    ('github.com/old/name',?, 'github','github.com','old','name','old/name',1,1,'2020-01-01','2026-06-01','retired'),
    ('github.com/new/name',?, 'github','github.com','new','name','new/name',1,1,'2020-01-01','2026-07-10','active')",
    params = list(nid, nid))
}
ai_ins <- function(con, repo, tool, date, cens, tiers, auth, last)
  DBI::dbExecute(con, "INSERT INTO vcs_ai_signals (repo_id,tool,first_seen_date,first_seen_censored,evidence_tiers,authored,last_confirmed_date) VALUES (?,?,?,?,?,?,?)",
    params = list(repo, tool, date, cens, tiers, auth, last))

test_that("idx_repos_node_id is created by ensure_repo_schema", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  idx <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='index'")$name
  expect_true("idx_repos_node_id" %in% idx)
})

test_that("reconcile carries a stale repo's onset onto the canonical (active/newest) repo", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  seed_pair(con)
  ai_ins(con, "github.com/old/name", "claude", "2024-01-01", 0L, "A", 0L, "2024-01-01")
  reconcile_ai_identity(con)
  got <- DBI::dbReadTable(con, "vcs_ai_signals")
  expect_equal(nrow(got), 1)
  expect_equal(got$repo_id, "github.com/new/name")   # carried onto the active/newest slug
  expect_equal(got$first_seen_date, "2024-01-01")
})

test_that("a same-tool collision folds through the reducer, never violating the PK", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  seed_pair(con)
  ai_ins(con, "github.com/old/name", "claude", "2024-01-01", 0L, "D", 0L, "2024-01-01") # exact, earlier
  ai_ins(con, "github.com/new/name", "claude", "2024-06-01", 1L, "B", 0L, "2024-06-01") # floor, later
  expect_no_warning(expect_message(reconcile_ai_identity(con),
    "ai identity: vcs_ai_signals, 1 row(s) moved from an old name, 1 of them folded", fixed = TRUE))
  got <- DBI::dbReadTable(con, "vcs_ai_signals")
  expect_equal(nrow(got), 1)                          # one (canonical, claude) row, no PK error
  expect_equal(got$repo_id, "github.com/new/name")
  expect_equal(got$first_seen_date, "2024-01-01")     # exact dominates the later floor
  expect_equal(got$first_seen_censored, 0L)
  expect_setequal(strsplit(got$evidence_tiers, ",")[[1]], c("B", "D"))  # tier union
})

test_that("reconcile handles 3+ repos on one node_id and is a no-op when none are shared", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSERT INTO repos (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status) VALUES
    ('github.com/a/x','N','github','github.com','a','x','a/x',1,1,'2020-01-01','2026-01-01','retired'),
    ('github.com/b/x','N','github','github.com','b','x','b/x',1,1,'2020-01-01','2026-02-01','retired'),
    ('github.com/c/x','N','github','github.com','c','x','c/x',1,1,'2020-01-01','2026-07-01','active'),
    ('github.com/d/y','M','github','github.com','d','y','d/y',1,1,'2020-01-01','2026-07-01','active')")
  ai_ins(con, "github.com/a/x", "cursor", "2023-05-01", 0L, "D", 0L, "2023-05-01")
  ai_ins(con, "github.com/b/x", "aider",  "2023-08-01", 0L, "C", 0L, "2023-08-01")
  ai_ins(con, "github.com/d/y", "codex",  "2024-02-01", 0L, "D", 0L, "2024-02-01")   # distinct node_id
  reconcile_ai_identity(con)
  got <- DBI::dbReadTable(con, "vcs_ai_signals")
  expect_setequal(got$repo_id[got$tool %in% c("cursor","aider")], "github.com/c/x")  # both carried to canonical
  expect_equal(got$repo_id[got$tool == "codex"], "github.com/d/y")                   # untouched
})

# Two slugs GitHub resolves to ONE repository, each still named in the DESCRIPTION of
# a different listed package, so both repo_ids stay active. Live since 2026-09:
# uchidamizuki/japanstat and uchidamizuki/jpstat, doi-usgs/hydrogeofetch and
# doi-usgs/nhdplustools.
seed_active_pair <- function(con, nid = "R_kgDOGZZTdA") {
  DBI::dbExecute(con, "INSERT INTO repos (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status) VALUES
    ('github.com/u/japanstat',?,'github','github.com','u','japanstat','u/japanstat',1,1,'2026-01-01','2026-09-14','active'),
    ('github.com/u/jpstat',?,'github','github.com','u','jpstat','u/jpstat',1,1,'2026-01-01','2026-09-14','active')",
    params = list(nid, nid))
}
# Every column, so a test can see a value go missing rather than only a row.
ai_full <- function(con, repo, tool, date, tiers, markers, auth_n, assist_n, last)
  DBI::dbExecute(con, "INSERT INTO vcs_ai_signals (repo_id,tool,first_seen_date,first_seen_censored,evidence_tiers,markers,authored,authored_commits,assisted_commits,last_confirmed_date) VALUES (?,?,?,0,?,?,0,?,?,?)",
    params = list(repo, tool, date, tiers, markers, auth_n, assist_n, last))
# What a weekly confirmation row leaves when nothing else backs its key.
ai_hollow <- function(con, repo, tool, last)
  DBI::dbExecute(con, "INSERT INTO vcs_ai_signals (repo_id,tool,first_seen_censored,authored,last_confirmed_date) VALUES (?,?,0,0,?)",
    params = list(repo, tool, last))
ai_sorted <- function(con) {
  d <- DBI::dbReadTable(con, "vcs_ai_signals")
  d <- d[order(d$repo_id, d$tool), , drop = FALSE]
  rownames(d) <- NULL
  d
}

test_that("two ACTIVE repos sharing a node_id keep their own full rows", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  seed_active_pair(con)
  ai_full(con, "github.com/u/japanstat", "claude", "2026-06-22T12:21:43.000+09:00", "B,D",
          ".claude,B,rbuildignore:.claude", 0L, 12L, "2026-09-13")
  ai_full(con, "github.com/u/jpstat", "claude", "2026-06-22T12:21:43.000+09:00", "D",
          ".claude", NA_integer_, NA_integer_, "2026-09-06")
  ai_full(con, "github.com/u/jpstat", "agents-md", "2026-07-15T09:06:09Z", "D",
          "AGENTS.md", NA_integer_, NA_integer_, "2026-09-13")
  before <- ai_sorted(con)
  reconcile_ai_identity(con)
  expect_equal(ai_sorted(con), before)
})

test_that("a hollow row on an active sibling takes the full sibling's evidence", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  seed_active_pair(con)
  ai_full(con, "github.com/u/japanstat", "claude", "2026-06-22T12:21:43.000+09:00", "B,D",
          ".claude,B,rbuildignore:.claude", 0L, 12L, "2026-09-06")
  ai_full(con, "github.com/u/japanstat", "agents-md", "2026-07-15T09:06:09Z", "D",
          "AGENTS.md", NA_integer_, NA_integer_, "2026-09-13")
  ai_hollow(con, "github.com/u/jpstat", "claude", "2026-09-13")
  ai_hollow(con, "github.com/u/jpstat", "agents-md", "2026-09-06")
  donors <- ai_sorted(con)
  donors <- donors[donors$repo_id == "github.com/u/japanstat", , drop = FALSE]

  reconcile_ai_identity(con)
  got <- ai_sorted(con)
  expect_equal(nrow(got), 4L)
  kept <- got[got$repo_id == "github.com/u/japanstat", , drop = FALSE]
  rownames(kept) <- NULL; rownames(donors) <- NULL
  expect_equal(kept, donors)                          # the donor is not touched

  healed <- got[got$repo_id == "github.com/u/jpstat", , drop = FALSE]
  evidence <- c("first_seen_date", "first_seen_censored", "evidence_tiers", "markers",
                "authored", "authored_commits", "assisted_commits")
  for (tl in c("claude", "agents-md"))
    expect_equal(as.list(healed[healed$tool == tl, evidence]),
                 as.list(donors[donors$tool == tl, evidence]), info = tl)
  # Each row keeps the later of the two confirmations: its own for claude, the
  # sibling's for agents-md. Both slugs are the same repository.
  expect_equal(healed$last_confirmed_date[healed$tool == "claude"], "2026-09-13")
  expect_equal(healed$last_confirmed_date[healed$tool == "agents-md"], "2026-09-13")
})

test_that("an active sibling with no row for a tool is not given one, and the row is not moved", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  seed_active_pair(con)
  # jpstat sorts after japanstat, so the old fold moved this row onto japanstat.
  ai_full(con, "github.com/u/jpstat", "claude", "2026-06-22T12:21:43.000+09:00", "B,D",
          ".claude,B", 0L, 12L, "2026-09-13")
  before <- ai_sorted(con)
  reconcile_ai_identity(con)
  expect_equal(ai_sorted(con), before)
})

test_that("hollow rows with no full sibling stay as they are", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  seed_active_pair(con)
  ai_hollow(con, "github.com/u/japanstat", "claude", "2026-09-13")
  ai_hollow(con, "github.com/u/jpstat", "claude", "2026-09-13")
  before <- ai_sorted(con)
  reconcile_ai_identity(con)
  expect_equal(ai_sorted(con), before)
})

test_that("a row holding any one of onset, tiers or markers is not filled from a sibling", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  seed_active_pair(con)
  for (tl in c("claude", "cursor", "codex", "aider"))
    ai_full(con, "github.com/u/japanstat", tl, "2026-06-22T12:21:43.000+09:00", "B,D",
            ".claude,B", 0L, 12L, "2026-09-13")
  # Each of these says something the sibling's row does not, and filling would
  # overwrite it. Only aider's row is empty.
  ai_full(con, "github.com/u/jpstat", "claude", "2026-07-01T00:00:00Z", NA, NA,
          NA_integer_, NA_integer_, "2026-09-06")
  ai_full(con, "github.com/u/jpstat", "cursor", NA, "D", NA, NA_integer_, NA_integer_, "2026-09-06")
  ai_full(con, "github.com/u/jpstat", "codex", NA, NA, "AGENTS.md", NA_integer_, NA_integer_, "2026-09-06")
  ai_hollow(con, "github.com/u/jpstat", "aider", "2026-09-06")
  before <- ai_sorted(con)

  reconcile_ai_identity(con)
  got <- ai_sorted(con)
  own <- function(d) d[d$repo_id == "github.com/u/jpstat" & d$tool != "aider", , drop = FALSE]
  expect_equal(own(got), own(before))
  filled <- got[got$repo_id == "github.com/u/jpstat" & got$tool == "aider", , drop = FALSE]
  expect_equal(filled$markers, ".claude,B")
})

test_that("a retired slug still folds onto the canonical member when two are active", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  seed_active_pair(con)
  DBI::dbExecute(con, "INSERT INTO repos (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status) VALUES
    ('github.com/old/jpstat','R_kgDOGZZTdA','github','github.com','old','jpstat','old/jpstat',1,1,'2020-01-01','2026-06-01','retired')")
  ai_full(con, "github.com/old/jpstat", "cursor", "2025-10-13T12:20:28Z", "D",
          "gitignore:.cursor", NA_integer_, NA_integer_, "2026-06-01")
  ai_full(con, "github.com/old/jpstat", "claude", "2026-01-01T00:00:00Z", "D",
          "CLAUDE.md", NA_integer_, NA_integer_, "2026-06-01")
  ai_full(con, "github.com/u/japanstat", "claude", "2026-06-22T12:21:43.000+09:00", "B,D",
          ".claude,B", 0L, 12L, "2026-09-13")
  ai_full(con, "github.com/u/jpstat", "claude", "2026-06-22T12:21:43.000+09:00", "B,D",
          ".claude,B", 0L, 12L, "2026-09-13")
  jp_before <- ai_sorted(con)
  jp_before <- jp_before[jp_before$repo_id == "github.com/u/jpstat", , drop = FALSE]
  rownames(jp_before) <- NULL

  reconcile_ai_identity(con)
  got <- ai_sorted(con)
  expect_false(any(got$repo_id == "github.com/old/jpstat"))        # the retired slug is folded
  canon <- got[got$repo_id == "github.com/u/japanstat", , drop = FALSE]
  expect_setequal(canon$tool, c("claude", "cursor"))               # onto the canonical member
  expect_equal(canon$first_seen_date[canon$tool == "claude"], "2026-01-01T00:00:00Z")  # exact min
  expect_setequal(strsplit(canon$markers[canon$tool == "claude"], ",")[[1]], c(".claude", "B", "CLAUDE.md"))
  jp <- got[got$repo_id == "github.com/u/jpstat", , drop = FALSE]
  rownames(jp) <- NULL
  expect_equal(jp, jp_before)                                      # the other active one is left alone
})

# ---- every AI table follows a renamed repository --------------------------------

.rn_old <- "github.com/old/name"; .rn_new <- "github.com/new/name"
.rn_put <- function(con, t, df) DBI::dbWriteTable(con, t, df, append = TRUE)
.rn_get <- function(con, t) DBI::dbReadTable(con, t)
.rn_reads <- function(repo, ...)
  .ai_bind_like(.ai_empty_reads(), list(data.frame(repo_id = repo, ..., stringsAsFactors = FALSE)))
.rn_count <- function(repo, set, n, on)
  data.frame(repo_id = repo, tool = "claude", identity_set = set, commits = as.integer(n),
             newest_commit_date = "2026-09-01T00:00:00Z", measured_on = on, stringsAsFactors = FALSE)
.rn_asked <- function(repo, key, outcome, on)
  data.frame(repo_id = repo, rule_key = key, rule_rev = 1L, ruleset_version = "v", asked_on = on,
             outcome = outcome, total_count = if (outcome == "hit") 3L else 0L,
             verified = if (outcome == "hit") 1L else NA_integer_, incomplete = 0L,
             first_hit_on = if (outcome == "hit") "2025-06-01" else NA_character_,
             source = "search", stringsAsFactors = FALSE)
.rn_review <- function(repo, first, file, last)
  data.frame(repo_id = repo, tool = "coderabbit", first_seen_date = first, first_seen_censored = 0L,
             evidence_tiers = "D", markers = file, assisted_commits = NA_integer_,
             assisted_measured_on = NA_character_, last_confirmed_date = last, stringsAsFactors = FALSE)
.rn_outside <- function(repo, last)
  data.frame(repo_id = repo, pr_number = 927L, tool = "cursor", found_via = "pr.cursor.agent-branch",
             created_at = "2026-08-28T09:43:30Z", from_fork = 1L, author_association = "NONE",
             last_confirmed_date = last, stringsAsFactors = FALSE)
.rn_model <- function(repo, family, n, tool = "claude")
  data.frame(repo_id = repo, tool = tool, provider = NA_character_, family = family, version = "4.8",
             context_window = NA_character_, commits = as.integer(n), first_seen = "2025-01-01",
             last_seen = "2026-09-01", window_complete = 1L, stringsAsFactors = FALSE)
.rn_tables <- c("vcs_ai_review_signals", "vcs_ai_outside_prs", "vcs_ai_account_counts",
                "vcs_ai_search_log", "vcs_ai_repo_reads", "vcs_ai_models")

test_that("the canonical map takes the newest active name and leaves a second active name alone", {
  r <- function(id, node, status, seen)
    data.frame(repo_id = id, node_id = node, status = status, last_seen = seen, stringsAsFactors = FALSE)
  repos <- rbind(r("github.com/a/old", "N1", "retired", "2026-06-01"),
                 r("github.com/a/new", "N1", "active", "2026-09-01"),
                 r("github.com/a/twin", "N1", "active", "2026-08-01"),
                 r("github.com/b/one", "N2", "retired", "2026-05-01"),
                 r("github.com/b/two", "N2", "retired", "2026-07-01"),
                 r("github.com/e/b", "N4", "retired", "2026-07-01"),
                 r("github.com/e/a", "N4", "retired", "2026-07-01"),
                 r("github.com/c/alone", "N3", "active", "2026-09-01"),
                 r("github.com/d/x", NA, "active", "2026-09-01"),
                 r("github.com/d/y", NA, "active", "2026-09-01"))
  map <- ai_canonical_repo_map(repos)
  expect_equal(map[["github.com/a/old"]], "github.com/a/new")
  expect_false("github.com/a/twin" %in% names(map))            # a second active name keeps its rows
  expect_equal(map[["github.com/b/one"]], "github.com/b/two")    # none active: all but the newest
  expect_equal(map[["github.com/e/b"]], "github.com/e/a")        # a tie goes to the smaller repo_id
  expect_setequal(names(map), c("github.com/a/old", "github.com/b/one", "github.com/e/b"))
  expect_length(ai_canonical_repo_map(repos[repos$node_id %in% "N3", ]), 0L)
  expect_length(ai_canonical_repo_map(NULL), 0L)
})

test_that("every AI table's rows under the old name move to the current name and none stay", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  seed_pair(con)
  .rn_put(con, "vcs_ai_review_signals", .rn_review(.rn_old, "2025-01-01", ".coderabbit.yaml", "2026-09-27"))
  .rn_put(con, "vcs_ai_outside_prs", .rn_outside(.rn_old, "2026-09-27"))
  .rn_put(con, "vcs_ai_account_counts", .rn_count(.rn_old, "graphql", 7, "2026-09-27"))
  .rn_put(con, "vcs_ai_search_log", .rn_asked(.rn_old, "author.claude", "hit", "2026-09-28"))
  .rn_put(con, "vcs_ai_repo_reads", .rn_reads(.rn_old, commits_read_on = "2026-09-27",
                                              prs_read_on = "2026-09-27", prs_walk_cursor = "P9"))
  .rn_put(con, "vcs_ai_models", .rn_model(.rn_old, "Opus", 3))
  msgs <- testthat::capture_messages(reconcile_ai_identity(con))
  for (t in .rn_tables) {
    got <- .rn_get(con, t)
    expect_equal(nrow(got), 1L, info = t)
    expect_equal(got$repo_id, .rn_new, info = t)
    expect_true(any(grepl(sprintf("ai identity: %s, 1 row(s) moved", t), msgs, fixed = TRUE)), info = t)
  }
  expect_equal(.rn_get(con, "vcs_ai_repo_reads")$prs_walk_cursor, "P9")
})

test_that("rows under both names fold to one, and account counts keep the newest, not the sum", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  seed_pair(con)
  .rn_put(con, "vcs_ai_review_signals", rbind(
    .rn_review(.rn_old, "2025-01-01", ".coderabbit.yaml", "2026-06-01"),
    .rn_review(.rn_new, "2025-08-01", ".coderabbit.yml", "2026-09-27")))
  .rn_put(con, "vcs_ai_outside_prs", rbind(.rn_outside(.rn_old, "2026-06-01"), .rn_outside(.rn_new, "2026-09-27")))
  .rn_put(con, "vcs_ai_account_counts", rbind(
    .rn_count(.rn_old, "graphql", 7, "2026-09-20"),
    .rn_count(.rn_old, "41898282+claude[bot]@users.noreply.github.com", 2, "2026-09-22"),
    .rn_count(.rn_new, "graphql", 5, "2026-09-27")))
  .rn_put(con, "vcs_ai_search_log", rbind(
    .rn_asked(.rn_old, "msg.claude.coauthor", "hit", "2026-09-21"),
    .rn_asked(.rn_new, "msg.claude.coauthor", "none", "2026-09-21"),
    .rn_asked(.rn_old, "author.claude", "hit", "2026-09-28"),
    .rn_asked(.rn_new, "author.claude", "none", "2026-09-21"),
    .rn_asked(.rn_old, "author.aider", "none", "2026-09-20")))
  suppressMessages(reconcile_ai_identity(con))

  rv <- .rn_get(con, "vcs_ai_review_signals")
  expect_equal(nrow(rv), 1L); expect_equal(rv$repo_id, .rn_new)
  expect_setequal(strsplit(rv$markers, ",")[[1]], c(".coderabbit.yaml", ".coderabbit.yml"))
  expect_equal(rv$first_seen_date, "2025-01-01"); expect_equal(rv$last_confirmed_date, "2026-09-27")
  op <- .rn_get(con, "vcs_ai_outside_prs")
  expect_equal(nrow(op), 1L); expect_equal(op$last_confirmed_date, "2026-09-27")
  ac <- .rn_get(con, "vcs_ai_account_counts")
  expect_true(all(ac$repo_id == .rn_new))
  expect_equal(ac$commits[ac$identity_set == "graphql"], 5L)       # both names counted the same commits
  expect_equal(ac$commits[ac$identity_set != "graphql"], 2L)
  lg <- .rn_get(con, "vcs_ai_search_log")
  lg <- lg[order(lg$rule_key), , drop = FALSE]
  expect_true(all(lg$repo_id == .rn_new))
  expect_equal(lg$rule_key, c("author.aider", "author.claude", "msg.claude.coauthor"))
  expect_equal(lg$outcome, c("none", "hit", "none"))               # the latest wins, the current name on a tie
})

test_that("a new name with no read row takes the old name's watermarks, and a newer commit read keeps its own", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  seed_pair(con)
  .rn_put(con, "vcs_ai_repo_reads", .rn_reads(.rn_old, commits_read_on = "2026-09-20",
    commits_read_through = "2026-09-19T08:00:00Z", commits_ruleset = "2026-09-19", commits_read = 40L,
    commits_window_complete = 1L, commits_history_complete = 1L, prs_read_on = "2026-09-20",
    prs_newest_created_at = "2026-09-01T00:00:00Z", prs_walk_complete = 0L,
    prs_walk_started_on = "2026-09-06", prs_walk_cursor = "P9", accounts_counted_on = "2026-09-20"))
  suppressMessages(reconcile_ai_identity(con))
  got <- .rn_get(con, "vcs_ai_repo_reads")
  expect_equal(got$repo_id, .rn_new)
  expect_equal(got$commits_read_through, "2026-09-19T08:00:00Z")
  expect_equal(got$commits_history_complete, 1L)
  expect_equal(got$prs_walk_cursor, "P9")

  DBI::dbExecute(con, "DELETE FROM vcs_ai_repo_reads")
  .rn_put(con, "vcs_ai_repo_reads", rbind(
    .rn_reads(.rn_old, commits_read_on = "2026-09-20", commits_read_through = "2026-09-19T08:00:00Z",
              commits_history_complete = 1L, prs_read_on = "2026-09-20", prs_walk_cursor = "P9"),
    .rn_reads(.rn_new, commits_read_on = "2026-09-27", commits_read_through = "2026-09-26T08:00:00Z",
              commits_history_complete = 0L)))
  suppressMessages(reconcile_ai_identity(con))
  got <- .rn_get(con, "vcs_ai_repo_reads")
  expect_equal(nrow(got), 1L)
  expect_equal(got$commits_read_on, "2026-09-27")
  expect_equal(got$commits_read_through, "2026-09-26T08:00:00Z")
  expect_equal(got$commits_history_complete, 0L)    # never the old name's flag beside the new watermark
  expect_equal(got$prs_walk_cursor, "P9")           # a walk only the old name had begun
})

test_that("model tallies are never combined across the two names", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  seed_pair(con)
  .rn_put(con, "vcs_ai_models", rbind(.rn_model(.rn_old, "Opus", 3), .rn_model(.rn_old, "Sonnet", 2),
                                      .rn_model(.rn_new, "Opus", 10), .rn_model(.rn_old, "Sonnet", 4, "cursor")))
  suppressMessages(reconcile_ai_identity(con))
  got <- .rn_get(con, "vcs_ai_models")
  expect_true(all(got$repo_id == .rn_new))
  cl <- got[got$tool == "claude", , drop = FALSE]
  expect_equal(nrow(cl), 1L); expect_equal(cl$commits, 10L)       # its own tally, not 13
  expect_equal(got$commits[got$tool == "cursor"], 4L)              # a tool only the old name had
})

test_that("a working database without the signals table still reconciles the other tables", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  seed_pair(con)
  DBI::dbExecute(con, "DROP TABLE vcs_ai_signals")
  .rn_put(con, "vcs_ai_search_log", .rn_asked(.rn_old, "author.claude", "none", "2026-09-28"))
  ok <- suppressMessages(reconcile_ai_identity(con))
  expect_true(ok)
  expect_equal(.rn_get(con, "vcs_ai_search_log")$repo_id, .rn_new)
})

test_that("state no rename touches comes back as it came", {
  st <- list(vcs_ai_search_log = .rn_asked("github.com/o/other", "author.claude", "none", "2026-10-05"),
             vcs_ai_models = .rn_model("github.com/o/other", "Opus", 3))
  expect_identical(carry_renamed_state(st, c("github.com/old/name" = "github.com/new/name")), st)
  expect_identical(carry_renamed_state(st, ai_canonical_repo_map(NULL)), st)
})
