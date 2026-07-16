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
