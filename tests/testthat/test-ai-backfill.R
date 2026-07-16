# scripts/ai_backfill.R uses repo-root-relative source() calls (a CLI-entry script), so
# it must be sourced with cwd temporarily chdir'd to the repo root, like test-backfill.R.
.aibf_wd <- setwd(.repo_root)
source(file.path(.repo_root, "scripts", "ai_backfill.R"))
setwd(.aibf_wd)

test_that("write_ai_roster / load_ai_roster round-trip a node_id-carrying, stars-free roster", {
  p <- tempfile(fileext = ".db")
  r <- data.frame(repo_id = "github.com/o/r", owner = "o", name = "r",
                  node_id = "R_1", done = 0L, stringsAsFactors = FALSE)
  write_ai_roster(p, r)
  got <- load_ai_roster(p)
  expect_equal(got$repo_id, "github.com/o/r")
  expect_equal(got$node_id, "R_1")
  expect_false("stars" %in% names(got))
})

test_that("run_enumerate_ai builds the FULL active github roster from the repos table", {
  # A fake summary DB with a repos table: one active github repo, one gone, one gitlab.
  rel <- tempfile("rel_"); dir.create(rel)
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-summary.db"))
  ensure_repo_schema(scon)
  DBI::dbExecute(scon, "INSERT INTO repos (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status) VALUES
    ('github.com/a/keep','R_a','github','github.com','a','keep','a/keep',1,1,'2024-01-01','2026-07-01','active'),
    ('github.com/b/gone','R_b','github','github.com','b','gone','b/gone',1,1,'2024-01-01','2026-07-01','gone'),
    ('gitlab.com/c/skip','R_c','gitlab','gitlab.com','c','skip','c/skip',0,1,'2024-01-01','2026-07-01','active')")
  DBI::dbDisconnect(scon)
  io <- list(
    download = function(pattern, dir) {
      f <- list.files(rel, pattern = utils::glob2rx(pattern), full.names = TRUE)
      if (!length(f)) return(FALSE)
      file.copy(f, file.path(dir, basename(f)), overwrite = TRUE); TRUE },
    # No renames to resolve in this fixture; an empty data payload makes the re-resolve
    # step's parse_resolve() see idx-less rows (node_id NA) and leave owner/name as-is.
    graphql = function(query) list(data = list()))
  out <- tempfile("out_"); dir.create(out)
  run_enumerate_ai(io, out)
  roster <- load_ai_roster(file.path(out, "vcs-ai-roster.db"))
  expect_equal(roster$repo_id, "github.com/a/keep")   # only the active github repo
  expect_equal(roster$node_id, "R_a")
})

test_that("run_enumerate_ai re-resolves owner/name from node_id for rows that already have one", {
  # A fake summary DB with one repo whose owner/name is stale (renamed since the last
  # resolve) but whose node_id is still current.
  rel <- tempfile("rel_"); dir.create(rel)
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-summary.db"))
  ensure_repo_schema(scon)
  DBI::dbExecute(scon, "INSERT INTO repos (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status) VALUES
    ('github.com/old/name','R_x','github','github.com','old','name','old/name',1,1,'2024-01-01','2026-07-01','active')")
  DBI::dbDisconnect(scon)
  io <- list(
    download = function(pattern, dir) {
      f <- list.files(rel, pattern = utils::glob2rx(pattern), full.names = TRUE)
      if (!length(f)) return(FALSE)
      file.copy(f, file.path(dir, basename(f)), overwrite = TRUE); TRUE },
    graphql = function(query) {
      # build_resolve_query(followRenames: true) reports the current slug for node R_x.
      list(data = list(r0 = list(id = "R_x", nameWithOwner = "new/name", isArchived = FALSE,
                                 isFork = FALSE, isMirror = FALSE, createdAt = "2024-01-01T00:00:00Z")))
    })
  out <- tempfile("out_"); dir.create(out)
  run_enumerate_ai(io, out)
  roster <- load_ai_roster(file.path(out, "vcs-ai-roster.db"))
  expect_equal(roster$owner, "new")                   # re-resolved via node_id, not the stale slug
  expect_equal(roster$name, "name")
  expect_equal(roster$repo_id, "github.com/old/name") # repo_id (the PK) is untouched
})
