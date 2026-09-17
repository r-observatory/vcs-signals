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
  expect_silent(reconcile_ai_identity(con))
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
