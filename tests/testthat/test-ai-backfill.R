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

test_that("run_cheap keeps only flagged repos and writes evidence + flagged tables", {
  out <- tempfile("out_"); dir.create(out)
  roster <- data.frame(
    repo_id = c("github.com/a/hit", "github.com/b/clean"),
    owner = c("a", "b"), name = c("hit", "clean"), node_id = c("R_a", "R_b"),
    done = 0L, stringsAsFactors = FALSE)
  roster_path <- file.path(out, "vcs-ai-roster.db"); write_ai_roster(roster_path, roster)

  # Fake io: the tree query flags r0 (.claude) and returns nothing for r1; the PR query
  # returns no agent PRs for either.
  io <- list(graphql = function(query) {
    if (grepl("rootTree", query, fixed = TRUE)) {
      return(list(data = list(
        r0 = list(isFork = FALSE, parent = NULL,
                  rootTree = list(entries = list(list(name = ".claude", type = "tree"))),
                  githubTree = NULL, gitignore = NULL, rbuildignore = NULL),
        r1 = list(isFork = FALSE, parent = NULL, rootTree = NULL, githubTree = NULL,
                  gitignore = NULL, rbuildignore = NULL))))
    }
    list(data = list(
      r0 = list(pullRequests = list(pageInfo = list(endCursor = NA, hasNextPage = FALSE), nodes = list())),
      r1 = list(pullRequests = list(pageInfo = list(endCursor = NA, hasNextPage = FALSE), nodes = list()))))
  })
  run_cheap(io, out, roster_path, 0, 1)
  fr <- read_flagged(file.path(out, "vcs-ai-cheap-0.db"))
  expect_equal(fr$flagged$repo_id, "github.com/a/hit")            # only the flagged repo
  expect_equal(fr$flagged$is_fork, 0L)
  expect_true("claude" %in% fr$evidence$tool[fr$evidence$repo_id == "github.com/a/hit"])
  expect_false("github.com/b/clean" %in% fr$flagged$repo_id)      # clean repo not written
})

test_that("run_cheap pauses before spending a batch when rate remaining is below AI_POINT_RESERVE", {
  out <- tempfile("out_"); dir.create(out)
  roster <- data.frame(
    repo_id = c("github.com/a/hit", "github.com/b/late"),
    owner = c("a", "b"), name = c("hit", "late"), node_id = c("R_a", "R_b"),
    done = 0L, stringsAsFactors = FALSE)
  roster_path <- file.path(out, "vcs-ai-roster.db"); write_ai_roster(roster_path, roster)

  # Rate remaining is already below AI_POINT_RESERVE (1500L), so the preflight must pause
  # before the first batch: the tree/PR queries are never issued, and the partial is
  # written with whatever was scanned (nothing), never faulted into a silent drop.
  io <- list(graphql = function(query) {
    if (grepl("rateLimit", query, fixed = TRUE))
      return(list(data = list(rateLimit = list(remaining = 100, resetAt = "2026-07-16T00:00:00Z"))))
    stop("tree/PR query issued despite rate remaining below AI_POINT_RESERVE")
  })
  run_cheap(io, out, roster_path, 0, 1, batch_size = 1)
  fr <- read_flagged(file.path(out, "vcs-ai-cheap-0.db"))
  expect_equal(nrow(fr$flagged), 0)
})

test_that("run_gate unions and dedups cheap partials into one flagged roster", {
  parts <- tempfile("parts_"); dir.create(parts)
  write_flagged_partial(file.path(parts, "vcs-ai-cheap-0.db"),
    data.frame(repo_id = "github.com/a/x", owner = "a", name = "x", node_id = "R_a",
               is_fork = 0L, parent = NA_character_, pr_onset_date = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(repo_id = "github.com/a/x", tool = "claude", tier = "D", marker = "CLAUDE.md",
               agnostic = 0L, stringsAsFactors = FALSE))
  write_flagged_partial(file.path(parts, "vcs-ai-cheap-1.db"),
    data.frame(repo_id = "github.com/b/y", owner = "b", name = "y", node_id = "R_b",
               is_fork = 1L, parent = "up/y", pr_onset_date = "2024-04-01T00:00:00Z",
               stringsAsFactors = FALSE),
    data.frame(repo_id = "github.com/b/y", tool = "copilot", tier = "PR", marker = "PR",
               agnostic = 0L, stringsAsFactors = FALSE))
  out <- tempfile("out_"); dir.create(out)
  run_gate(out, parts)
  fr <- read_flagged(file.path(out, "vcs-ai-flagged-roster.db"))
  expect_setequal(fr$flagged$repo_id, c("github.com/a/x", "github.com/b/y"))
  expect_equal(nrow(fr$evidence), 2)
})

test_that("run_deep assembles marker + confirmed commit + PR onsets into a detail shard", {
  out <- tempfile("out_"); dir.create(out)
  write_flagged_partial(file.path(out, "vcs-ai-flagged-roster.db"),
    data.frame(repo_id = "github.com/o/r", owner = "o", name = "r", node_id = "R_1",
               is_fork = 0L, parent = NA_character_, pr_onset_date = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(repo_id = "github.com/o/r", tool = "claude", tier = "D", marker = "CLAUDE.md",
               agnostic = 0L, stringsAsFactors = FALSE))
  # Marker pager returns a single last page dated 2024-03-01; the author-email search
  # returns an earlier 2023-11-01 (a real bot-authored commit, structurally exact).
  io <- list(
    graphql = function(query) list(data = list(repository = list(defaultBranchRef = list(
      target = list(history = list(pageInfo = list(endCursor = "", hasNextPage = FALSE),
        nodes = list(list(committedDate = "2024-03-01T00:00:00Z")))))))),
    search = function(owner, name, query, delay = 0) "2023-11-01T00:00:00Z")
  run_deep(io, out, file.path(out, "vcs-ai-flagged-roster.db"), 0, 1,
           marker_delay = 0, search_delay = 0)
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-ai-shard-0.db"))
  on.exit(DBI::dbDisconnect(scon))
  got <- DBI::dbReadTable(scon, "vcs_ai_signals")
  expect_equal(got$tool, "claude")
  expect_equal(got$first_seen_date, "2023-11-01T00:00:00Z")   # commit onset earlier than marker onset
  expect_equal(got$first_seen_censored, 0L)                   # both exact
  expect_setequal(strsplit(got$evidence_tiers, ",")[[1]], c("A", "D"))
})

test_that("run_deep censors every Tier-D marker on a fork", {
  out <- tempfile("out_"); dir.create(out)
  write_flagged_partial(file.path(out, "vcs-ai-flagged-roster.db"),
    data.frame(repo_id = "github.com/o/f", owner = "o", name = "f", node_id = "R_2",
               is_fork = 1L, parent = "up/f", pr_onset_date = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(repo_id = "github.com/o/f", tool = "cursor", tier = "D", marker = ".cursor",
               agnostic = 0L, stringsAsFactors = FALSE))
  io <- list(
    graphql = function(query) list(data = list(repository = list(defaultBranchRef = list(
      target = list(history = list(pageInfo = list(endCursor = "", hasNextPage = FALSE),
        nodes = list(list(committedDate = "2022-01-01T00:00:00Z")))))))),
    search = function(owner, name, query, delay = 0) NA_character_)   # no bot identity for cursor markers
  run_deep(io, out, file.path(out, "vcs-ai-flagged-roster.db"), 0, 1,
           marker_delay = 0, search_delay = 0)
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-ai-shard-0.db"))
  on.exit(DBI::dbDisconnect(scon))
  got <- DBI::dbReadTable(scon, "vcs_ai_signals")
  expect_equal(got$first_seen_censored, 1L)                   # fork -> Tier-D onset is a floor
})

test_that("run_deep pauses before a repo when rate remaining is below AI_POINT_RESERVE", {
  out <- tempfile("out_"); dir.create(out)
  write_flagged_partial(file.path(out, "vcs-ai-flagged-roster.db"),
    data.frame(repo_id = "github.com/o/late", owner = "o", name = "late", node_id = "R_3",
               is_fork = 0L, parent = NA_character_, pr_onset_date = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(repo_id = "github.com/o/late", tool = "claude", tier = "D", marker = "CLAUDE.md",
               agnostic = 0L, stringsAsFactors = FALSE))
  # Rate remaining is already below AI_POINT_RESERVE, so the preflight must pause before
  # the first repo: fetch_marker_onset's graphql call and io$search are never issued.
  io <- list(
    graphql = function(query) list(data = list(rateLimit = list(remaining = 200, resetAt = "2026-07-16T00:00:00Z"))),
    search = function(owner, name, query, delay = 0) stop("search issued despite the rate pause"))
  run_deep(io, out, file.path(out, "vcs-ai-flagged-roster.db"), 0, 1,
           marker_delay = 0, search_delay = 0)
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-ai-shard-0.db"))
  on.exit(DBI::dbDisconnect(scon))
  got <- DBI::dbReadTable(scon, "vcs_ai_signals")
  expect_equal(nrow(got), 0)      # nothing scanned this run
})
