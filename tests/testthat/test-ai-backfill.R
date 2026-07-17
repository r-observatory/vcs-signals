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

test_that("run_enumerate_ai drops a roster row whose re-resolve returns a different node_id (squatted slug)", {
  # A fake summary DB with two repos that already carry a node_id: one genuinely renamed
  # (old/name -> R_x, still resolves to R_x at the new slug) and one whose old slug has
  # since been squatted by an unrelated repo (stale/squatted -> R_y, but the slug
  # stale/squatted now resolves to a DIFFERENT node_id, R_evil, at a different slug).
  rel <- tempfile("rel_"); dir.create(rel)
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-summary.db"))
  ensure_repo_schema(scon)
  DBI::dbExecute(scon, "INSERT INTO repos (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status) VALUES
    ('github.com/old/name','R_x','github','github.com','old','name','old/name',1,1,'2024-01-01','2026-07-01','active'),
    ('github.com/stale/squatted','R_y','github','github.com','stale','squatted','stale/squatted',1,1,'2024-01-01','2026-07-01','active')")
  DBI::dbDisconnect(scon)
  io <- list(
    download = function(pattern, dir) {
      f <- list.files(rel, pattern = utils::glob2rx(pattern), full.names = TRUE)
      if (!length(f)) return(FALSE)
      file.copy(f, file.path(dir, basename(f)), overwrite = TRUE); TRUE },
    graphql = function(query) {
      # r0 (old/name, node R_x) genuinely renamed -> same node_id at a new slug.
      # r1 (stale/squatted, node R_y) -> the slug now resolves to an UNRELATED repo's
      # node_id (R_evil): the old repo is gone and something else squatted the slug.
      list(data = list(
        r0 = list(id = "R_x", nameWithOwner = "new/name", isArchived = FALSE,
                  isFork = FALSE, isMirror = FALSE, createdAt = "2024-01-01T00:00:00Z"),
        r1 = list(id = "R_evil", nameWithOwner = "squatter/repo", isArchived = FALSE,
                  isFork = FALSE, isMirror = FALSE, createdAt = "2025-06-01T00:00:00Z")))
    })
  out <- tempfile("out_"); dir.create(out)
  run_enumerate_ai(io, out)
  roster <- load_ai_roster(file.path(out, "vcs-ai-roster.db"))

  # The squatted row is dropped from the roster entirely (never scanned by cheap/deep),
  # not merely flagged done=1: only the genuinely-renamed row survives.
  expect_equal(roster$repo_id, "github.com/old/name")
  expect_equal(roster$owner, "new")
  expect_equal(roster$name, "name")
  expect_equal(roster$node_id, "R_x")
  expect_false("github.com/stale/squatted" %in% roster$repo_id)
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
  expect_equal(got$authored, 1L)   # author-email commit hit means the bot itself authored commits
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

test_that("run_merge reduces prior onsets against incoming shard partials and republishes", {
  # Local-release fake io (upload copies in, download copies out), as in test-ai-persistence.R.
  rel <- tempfile("rel_"); dir.create(rel)
  io <- list(
    release_exists = function() length(list.files(rel)) > 0,
    download = function(pattern, dir) {
      f <- list.files(rel, pattern = utils::glob2rx(pattern), full.names = TRUE)
      if (!length(f)) return(FALSE)
      file.copy(f, file.path(dir, basename(f)), overwrite = TRUE); TRUE },
    upload = function(path) { file.copy(path, file.path(rel, basename(path)), overwrite = TRUE); TRUE })

  # Prior published state: one claude onset recorded as a censored floor at 2024-06-01.
  out1 <- tempfile("o1_"); dir.create(out1)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out1, "w.db"))
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "INSERT INTO vcs_ai_signals (repo_id,tool,first_seen_date,first_seen_censored,evidence_tiers,authored,last_confirmed_date) VALUES ('github.com/o/r','claude','2024-06-01',1,'D',0,'2024-06-01')")
  publish(io, con, out1, tag = "current", source_kind = "live", force_full = TRUE)
  DBI::dbDisconnect(con)

  # Incoming deep partial: an EXACT claude onset at 2024-03-01 (earlier than the floor).
  parts <- tempfile("parts_"); dir.create(parts)
  export_ai_shard(file.path(parts, "vcs-ai-shard-0.db"),
    data.frame(repo_id = "github.com/o/r", tool = "claude", first_seen_date = "2024-03-01",
               first_seen_censored = 0L, evidence_tiers = "A", authored = 0L,
               last_confirmed_date = "2026-07-16", stringsAsFactors = FALSE))

  out2 <- tempfile("o2_"); dir.create(out2)
  run_merge(io, out2, parts)

  # Read the republished summary shard back and assert the reduced onset.
  chk <- tempfile("chk_"); dir.create(chk)
  io$download("vcs-signals-summary.db", chk)
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(chk, "vcs-signals-summary.db"))
  on.exit(DBI::dbDisconnect(scon))
  got <- DBI::dbReadTable(scon, "vcs_ai_signals")
  expect_equal(nrow(got), 1)
  expect_equal(got$first_seen_date, "2024-03-01")          # exact dominates the earlier-published floor
  expect_equal(got$first_seen_censored, 0L)
  expect_setequal(strsplit(got$evidence_tiers, ",")[[1]], c("A", "D"))
})

test_that("run_gate_incremental narrows the flagged roster to new-tool repos", {
  parts <- tempfile("parts_"); dir.create(parts)
  # Cheap partials: A already-published claude (skip), B new cursor (keep),
  #                 C already-published claude PLUS a new copilot PR (keep - adopted a 2nd tool).
  write_flagged_partial(file.path(parts, "vcs-ai-cheap-0.db"),
    data.frame(repo_id = c("github.com/a/x", "github.com/b/y", "github.com/c/z"),
               owner = c("a", "b", "c"), name = c("x", "y", "z"),
               node_id = c("R_a", "R_b", "R_c"), is_fork = 0L,
               parent = NA_character_, pr_onset_date = NA_character_, stringsAsFactors = FALSE),
    data.frame(repo_id = c("github.com/a/x", "github.com/b/y", "github.com/c/z", "github.com/c/z"),
               tool = c("claude", "cursor", "claude", "copilot"),
               tier = c("D", "D", "D", "PR"),
               marker = c("CLAUDE.md", ".cursor", "CLAUDE.md", "PR"),
               agnostic = 0L, stringsAsFactors = FALSE))

  # Published baseline in a fake summary release: A/claude and C/claude already onset.
  rel <- tempfile("rel_"); dir.create(rel)
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-summary.db"))
  ensure_repo_schema(scon); ensure_series_schema(scon)
  DBI::dbExecute(scon, "INSERT INTO vcs_ai_signals (repo_id,tool,first_seen_date,first_seen_censored,evidence_tiers,authored,last_confirmed_date) VALUES
    ('github.com/a/x','claude','2024-01-01',0,'D',0,'2024-01-01'),
    ('github.com/c/z','claude','2024-02-01',0,'D',0,'2024-02-01')")
  DBI::dbDisconnect(scon)

  io <- list(download = function(pattern, dir) {
    f <- list.files(rel, pattern = utils::glob2rx(pattern), full.names = TRUE)
    if (!length(f)) return(FALSE)
    file.copy(f, file.path(dir, basename(f)), overwrite = TRUE); TRUE })

  out <- tempfile("out_"); dir.create(out)
  run_gate_incremental(io, out, parts)
  fr <- read_flagged(file.path(out, "vcs-ai-flagged-roster.db"))
  expect_setequal(fr$flagged$repo_id, c("github.com/b/y", "github.com/c/z"))  # A skipped
  expect_false("github.com/a/x" %in% fr$flagged$repo_id)
  # C survives with both its evidence rows so the deep pass re-onsets its new copilot.
  expect_true("copilot" %in% fr$evidence$tool[fr$evidence$repo_id == "github.com/c/z"])

  # A's skipped claude and C's already-published claude both get a confirmation row (their
  # last_confirmed_date must keep advancing even though A never reaches the deep matrix); B's
  # cursor and C's copilot are new adoptions, not confirmations, so they get none.
  ccon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-ai-shard-confirm.db"))
  on.exit(DBI::dbDisconnect(ccon), add = TRUE)
  confirm <- DBI::dbReadTable(ccon, "vcs_ai_signals")
  expect_setequal(paste(confirm$repo_id, confirm$tool),
                  c("github.com/a/x claude", "github.com/c/z claude"))
  expect_true(all(is.na(confirm$first_seen_date)))
})

test_that("run_gate_incremental keeps everything when no published detail exists (first weekly run)", {
  parts <- tempfile("parts_"); dir.create(parts)
  write_flagged_partial(file.path(parts, "vcs-ai-cheap-0.db"),
    data.frame(repo_id = "github.com/a/x", owner = "a", name = "x", node_id = "R_a",
               is_fork = 0L, parent = NA_character_, pr_onset_date = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(repo_id = "github.com/a/x", tool = "claude", tier = "D", marker = "CLAUDE.md",
               agnostic = 0L, stringsAsFactors = FALSE))
  io <- list(download = function(pattern, dir) FALSE)   # no published release yet
  out <- tempfile("out_"); dir.create(out)
  run_gate_incremental(io, out, parts)
  fr <- read_flagged(file.path(out, "vcs-ai-flagged-roster.db"))
  expect_equal(fr$flagged$repo_id, "github.com/a/x")    # nothing published -> everything is new

  ccon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-ai-shard-confirm.db"))
  on.exit(DBI::dbDisconnect(ccon), add = TRUE)
  expect_equal(nrow(DBI::dbReadTable(ccon, "vcs_ai_signals")), 0)  # nothing to confirm yet
})

test_that("run_deep dates a github-located marker via its .github/ real path", {
  out <- tempfile("out_"); dir.create(out)
  write_flagged_partial(file.path(out, "vcs-ai-flagged-roster.db"),
    data.frame(repo_id = "github.com/o/cop", owner = "o", name = "cop", node_id = "R_c",
               is_fork = 0L, parent = NA_character_, pr_onset_date = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(repo_id = "github.com/o/cop", tool = "copilot", tier = "D",
               marker = "copilot-instructions.md", agnostic = 0L, stringsAsFactors = FALSE))
  # The history query resolves ONLY at the real .github/ path; the bare entry name returns
  # 0 commits (the old bug). A dated onset therefore proves run_deep queried .github/.
  io <- list(
    graphql = function(query) {
      if (grepl("rateLimit", query, fixed = TRUE))
        return(list(data = list(rateLimit = list(remaining = 5000, resetAt = "2026-07-18T00:00:00Z"))))
      if (grepl(".github/copilot-instructions.md", query, fixed = TRUE))
        return(list(data = list(repository = list(defaultBranchRef = list(target = list(
          history = list(pageInfo = list(endCursor = "", hasNextPage = FALSE),
            nodes = list(list(committedDate = "2024-05-01T00:00:00Z")))))))))
      list(data = list(repository = list(defaultBranchRef = list(target = list(
        history = list(pageInfo = list(endCursor = "", hasNextPage = FALSE), nodes = list()))))))
    },
    search = function(owner, name, query, delay = 0) NA_character_)   # copilot has no author-email
  run_deep(io, out, file.path(out, "vcs-ai-flagged-roster.db"), 0, 1,
           marker_delay = 0, search_delay = 0)
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-ai-shard-0.db"))
  on.exit(DBI::dbDisconnect(scon))
  got <- DBI::dbReadTable(scon, "vcs_ai_signals")
  expect_equal(got$tool, "copilot")
  expect_equal(got$first_seen_date, "2024-05-01T00:00:00Z")   # dated via .github/ real path
  expect_equal(got$first_seen_censored, 0L)                   # committed marker -> exact
})

test_that("run_deep gives an ignore-token detection a censored today floor and spends no history call", {
  out <- tempfile("out_"); dir.create(out)
  write_flagged_partial(file.path(out, "vcs-ai-flagged-roster.db"),
    data.frame(repo_id = "github.com/o/ign", owner = "o", name = "ign", node_id = "R_i",
               is_fork = 0L, parent = NA_character_, pr_onset_date = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(repo_id = "github.com/o/ign", tool = "claude", tier = "D",
               marker = "ignore:.claude", agnostic = 0L, stringsAsFactors = FALSE))
  # A marker-history query for the non-existent ignore path would be wasted: fault it so the
  # test proves run_deep never issues one. The rateLimit preflight is the only graphql call.
  io <- list(
    graphql = function(query) {
      if (grepl("history", query, fixed = TRUE))
        stop("marker history queried for an ignore-token detection")
      list(data = list())   # rateLimit query -> remaining NULL -> Inf, so the preflight passes
    },
    search = function(owner, name, query, delay = 0) NA_character_)   # isolate the floor
  run_deep(io, out, file.path(out, "vcs-ai-flagged-roster.db"), 0, 1,
           marker_delay = 0, search_delay = 0)
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-ai-shard-0.db"))
  on.exit(DBI::dbDisconnect(scon))
  got <- DBI::dbReadTable(scon, "vcs_ai_signals")
  expect_equal(got$tool, "claude")
  expect_equal(got$first_seen_censored, 1L)                       # honest "<= today" floor
  expect_equal(got$first_seen_date, paste0(format(Sys.Date()), "T23:59:59Z"))  # end-of-day floor
  expect_equal(got$evidence_tiers, "D")
})

test_that("main dispatches gate-incremental to run_gate_incremental", {
  rec <- new.env()
  orig_fn <- run_gate_incremental
  orig_parts <- Sys.getenv("VCS_PARTS", unset = NA)
  on.exit({
    run_gate_incremental <<- orig_fn
    if (is.na(orig_parts)) Sys.unsetenv("VCS_PARTS") else Sys.setenv(VCS_PARTS = orig_parts)
  }, add = TRUE)

  run_gate_incremental <<- function(io, out_dir, parts_dir) {
    rec$hit <- TRUE; rec$out <- out_dir; rec$parts <- parts_dir; invisible(TRUE)
  }
  Sys.setenv(VCS_PARTS = "myparts")
  main("gate-incremental", "myout")

  expect_true(isTRUE(rec$hit))
  expect_equal(rec$out, "myout")
  expect_equal(rec$parts, "myparts")   # read from VCS_PARTS, same as the plain gate
})

test_that("ai-weekly.yml is the 5-job incremental pipeline (Sunday cron, incremental gate, serialized, no year-tag mirror)", {
  wf <- file.path(.repo_root, ".github", "workflows", "ai-weekly.yml")
  expect_true(file.exists(wf))
  txt <- paste(readLines(wf), collapse = "\n")

  # Sunday 07:00 UTC cron + manual dispatch.
  expect_match(txt, "schedule:", fixed = TRUE)
  expect_match(txt, 'cron:\\s*"0 7 \\* \\* 0"')
  expect_match(txt, "workflow_dispatch:", fixed = TRUE)

  # Its own concurrency group, not the backfill's.
  expect_match(txt, "group:\\s*vcs-signals-ai-weekly")

  # The same five jobs (2-space-indented job keys).
  for (job in c("\n  enumerate:", "\n  cheap:", "\n  gate:", "\n  deep:", "\n  merge:"))
    expect_match(txt, job, fixed = TRUE)

  # The gate runs INCREMENTALLY.
  expect_match(txt, "ai_backfill.R gate-incremental", fixed = TRUE)

  # CI test gate + release guard carried over from ai-backfill.yml.
  expect_match(txt, "Rscript tests/testthat.R", fixed = TRUE)
  expect_match(txt, "gh release view current", fixed = TRUE)

  # The deep matrix stays serialized on the shared GraphQL token.
  expect_match(txt, "max-parallel: 1", fixed = TRUE)

  # The gate's confirmation-row partial is uploaded from the gate job and downloaded into the
  # merge job's parts directory, so run_merge's unchanged vcs-ai-shard-*.db glob picks it up
  # and last_confirmed_date keeps advancing for already-published repos skipped from deep.
  expect_match(txt, "vcs-ai-shard-confirm.db", fixed = TRUE)
  expect_match(txt, "ai-confirm-shard", fixed = TRUE)

  # AI onsets have no year component, so the year-tag mirror must NOT be present.
  expect_false(grepl("mirror-year-tags", txt, fixed = TRUE))
})
