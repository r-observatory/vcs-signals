.amp <- setwd(.repo_root); source(file.path(.repo_root, "scripts", "ai_backfill.R")); setwd(.amp)

.A <- "github.com/o/a"; .B <- "github.com/o/b"
# `seed(con)` writes extra published state before the first publish.
.seed_release <- function(seed = NULL) {
  rel <- tempfile("rel_"); dir.create(rel)
  io <- local_release_io(rel)
  out0 <- tempfile("o0_"); dir.create(out0)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out0, "w.db"))
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "INSERT INTO repos (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status)
    VALUES ('github.com/o/a','R_a','github','github.com','o','a','o/a',1,1,'2024-01-01','2026-09-27','active')")
  DBI::dbExecute(con, "INSERT INTO vcs_ai_signals (repo_id,tool,first_seen_date,first_seen_censored,evidence_tiers,markers,authored,last_confirmed_date)
    VALUES ('github.com/o/a','claude','2024-06-01',1,'D','CLAUDE.md',0,'2026-09-27')")
  # A search the ruleset no longer has, which the rebuilt coverage leaves out.
  DBI::dbExecute(con, "INSERT INTO vcs_ai_search_coverage (rule_key,tool,channel,rule_rev,repos_asked,repos_hit,repos_refused,repos_read_whole,last_asked_on)
    VALUES ('msg.retired.rule','claude','commit-credit',1,3,1,0,0,'2026-09-20')")
  if (!is.null(seed)) seed(con)
  suppressMessages(publish(io, con, out0, tag = "current", source_kind = "live", force_full = TRUE, base_generation = ""))
  DBI::dbDisconnect(con)
  list(io = io, rel = rel)
}
.parts <- function(with_cheap = TRUE, failures = NULL) {
  parts <- tempfile("parts_"); dir.create(parts)
  today <- format(Sys.Date())
  export_ai_shard(file.path(parts, "vcs-ai-shard-0.db"),
    .ai_align_signals(data.frame(repo_id = .A, tool = "claude", first_seen_date = "2024-03-01", first_seen_censored = 0L,
      evidence_tiers = "A", markers = "A", authored = 1L, last_confirmed_date = today, stringsAsFactors = FALSE)),
    extra = list(
      search_log = data.frame(repo_id = .A, rule_key = "msg.claude.coauthor", rule_rev = 1L,
        ruleset_version = AI_RULESET_VERSION, asked_on = today, outcome = "hit", total_count = 5L, verified = 1L,
        incomplete = 0L, first_hit_on = "2024-05-01", source = "search", stringsAsFactors = FALSE),
      account_counts = data.frame(repo_id = .A, tool = "claude", identity_set = "41898282+claude[bot]@users.noreply.github.com",
        commits = 2L, newest_commit_date = NA_character_, measured_on = today, stringsAsFactors = FALSE),
      campaign = data.frame(since = NA_character_, stringsAsFactors = FALSE)))
  export_ai_shard(file.path(parts, "vcs-ai-shard-1.db"), .ai_empty_signals())
  if (with_cheap) {
    write_cheap_partial(file.path(parts, "vcs-ai-cheap-0.db"), list(
      flagged = data.frame(repo_id = .A, owner = "o", name = "a", node_id = "R_a", is_fork = 0L, parent = NA_character_,
                           pr_onset_date = NA_character_, stringsAsFactors = FALSE),
      evidence = cbind(repo_id = .A, .ai_found("claude", "D", "CLAUDE.md", onset = paste0(today, "T23:59:59Z"))),
      repo_reads = data.frame(repo_id = .A, commits_read_on = today, commits_ruleset = AI_RULESET_VERSION,
                              commits_history_complete = 1L, accounts_counted_on = today, reached_first = 1L,
                              stringsAsFactors = FALSE),
      account_counts = data.frame(repo_id = .A, tool = "claude", identity_set = "graphql", commits = 7L,
                                  newest_commit_date = "2026-09-30T00:00:00Z", measured_on = today, stringsAsFactors = FALSE),
      review = data.frame(repo_id = .A, tool = "coderabbit", first_seen_date = paste0(today, "T23:59:59Z"),
                          first_seen_censored = 1L, evidence_tiers = "D", markers = ".coderabbit.yaml",
                          last_confirmed_date = today, stringsAsFactors = FALSE),
      outside_prs = data.frame(repo_id = .A, pr_number = 927L, tool = "cursor", found_via = "pr.cursor.agent-branch",
                               created_at = "2026-08-28T09:43:30Z", from_fork = 1L, author_association = "NONE",
                               last_confirmed_date = today, stringsAsFactors = FALSE),
      models = data.frame(repo_id = .A, tool = "claude", provider = NA_character_, family = "Opus", version = "4.8",
                          context_window = NA_character_, commits = 3L, first_seen = "2025-01-01", last_seen = "2026-09-01",
                          window_complete = 1L, mode = "replace", stringsAsFactors = FALSE),
      search_log = data.frame(repo_id = .A, rule_key = "msg.claude.session", rule_rev = 1L,
                              ruleset_version = AI_RULESET_VERSION, asked_on = today, outcome = "hit", total_count = 3L,
                              verified = 1L, incomplete = 0L, first_hit_on = "2025-02-01", source = "read", mode = "replace",
                              stringsAsFactors = FALSE)))
    # A partial written by older code carries only some tables; the rest read as empty.
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(parts, "vcs-ai-cheap-1.db"))
    DBI::dbWriteTable(con, "repo_reads", data.frame(repo_id = .B, commits_read_on = today, stringsAsFactors = FALSE))
    DBI::dbDisconnect(con)
  }
  write_dev_tooling_partial(file.path(parts, "vcs-dev-tooling-0.db"), .devtool_empty_shard(), failures)
  parts
}
.pub <- function(io, asset = "vcs-signals-summary.db") {
  chk <- tempfile("chk_"); dir.create(chk)
  io$download(asset, chk)
  file.path(chk, asset)
}
.tbl <- function(path, t) { con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con)); DBI::dbReadTable(con, t) }

test_that("the merge takes every table the weekly reads and searches wrote", {
  r <- .seed_release()
  msgs <- testthat::capture_messages(run_merge(r$io, tempfile("m_"), .parts()))
  expect_true(any(grepl("2 search shard(s), 2 weekly read partial(s)", msgs, fixed = TRUE)))
  s <- .pub(r$io)
  rr <- .tbl(s, "vcs_ai_repo_reads")
  expect_setequal(rr$repo_id, c(.A, .B)); expect_false("reached_first" %in% names(rr))
  sig <- .tbl(s, "vcs_ai_signals")
  expect_equal(sig$authored_commits[sig$tool == "claude"], 9L)      # 7 by the account filter and 2 by REST
  expect_equal(sig$assisted_commits[sig$tool == "claude"], 5L)      # the largest usable credit count
  expect_equal(sig$first_seen_date[sig$tool == "claude"], "2024-03-01")
  expect_setequal(.tbl(s, "vcs_ai_search_log")$rule_key, c("msg.claude.coauthor", "msg.claude.session"))
  expect_equal(.tbl(s, "vcs_ai_review_signals")$tool, "coderabbit")
  expect_equal(.tbl(s, "vcs_ai_outside_prs")$pr_number, 927L)
  expect_equal(.tbl(s, "vcs_ai_models")$family, "Opus")
  cov <- .tbl(s, "vcs_ai_search_coverage")
  expect_setequal(cov$rule_key, .ai_coverage_rules()$rule_key)
  expect_true(all(cov$repos_read_whole == 1L))                      # .A read to its first commit this run
  expect_equal(.tbl(s, "vcs_ai_ruleset_history")$ruleset_version, AI_RULESET_VERSION)
  st <- .tbl(.pub(r$io, "vcs-signals-recent.db"), "pipeline_state")
  expect_equal(st$value[st$key == "ai_state_tables_since"], format(Sys.Date()))
})

test_that("the same parts without the weekly read partials advance no watermark", {
  r <- .seed_release()
  suppressMessages(run_merge(r$io, tempfile("m_"), .parts(with_cheap = FALSE)))
  expect_equal(nrow(.tbl(.pub(r$io), "vcs_ai_repo_reads")), 0L)
  # Nothing was read, so the next enumerate must not take the empty table for lost state.
  expect_false("ai_state_tables_since" %in% .tbl(.pub(r$io, "vcs-signals-recent.db"), "pipeline_state")$key)
})

test_that("the first merge over a summary without the new tables publishes", {
  r <- .seed_release()
  for (f in c("vcs-signals-summary.db", "vcs-signals-recent.db")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(r$rel, f))
    for (t in c("vcs_ai_repo_reads", "vcs_ai_account_counts", "vcs_ai_search_log", "vcs_ai_search_coverage",
                "vcs_ai_review_signals", "vcs_ai_outside_prs", "vcs_ai_ruleset_history"))
      DBI::dbExecute(con, sprintf('DROP TABLE IF EXISTS "%s"', t))
    for (col in c("authored_measured_on", "assisted_measured_on"))
      DBI::dbExecute(con, sprintf("ALTER TABLE vcs_ai_signals DROP COLUMN %s", col))
    DBI::dbDisconnect(con)
  }
  expect_no_error(suppressMessages(run_merge(r$io, tempfile("m_"), .parts())))
  expect_true("authored_measured_on" %in% names(.tbl(.pub(r$io), "vcs_ai_signals")))
})

test_that("too many repositories that could not be read fail the run after the data is out", {
  r <- .seed_release()
  f <- .fetch_failed_frame(.A, "activity", "Something went wrong", "2026-10-04T10:00:00Z")
  msgs <- character(0)
  err <- tryCatch(withCallingHandlers(run_merge(r$io, tempfile("m_"), .parts(failures = f)),
                                      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }),
                  error = function(e) conditionMessage(e))
  expect_match(err, "1 of 1 roster repositories failed a read this week", fixed = TRUE)
  expect_true(any(grepl("(contents 0, activity 1, accounts 0, walk 0)", msgs, fixed = TRUE)))
  expect_equal(nrow(.tbl(.pub(r$io), "vcs_ai_repo_reads")), 2L)   # published before the stop
})

test_that("weekly read partials from before this change add no rows", {
  # ai-merge-rerun over an older run: its cheap partial holds only two tables and no dates.
  r <- .seed_release()
  parts <- tempfile("parts_"); dir.create(parts)
  export_ai_shard(file.path(parts, "vcs-ai-shard-0.db"), .ai_empty_signals())
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(parts, "vcs-ai-cheap-0.db"))
  DBI::dbWriteTable(con, "flagged", data.frame(repo_id = .B, owner = "o", name = "b", node_id = "R_b", is_fork = 0L,
                                               parent = NA_character_, pr_onset_date = NA_character_,
                                               stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "evidence", data.frame(repo_id = c(.A, .B), tool = c("claude", "cursor"), tier = "D",
                                                marker = c("CLAUDE.md", ".cursor"), agnostic = 0L,
                                                stringsAsFactors = FALSE))
  DBI::dbDisconnect(con)
  write_dev_tooling_partial(file.path(parts, "vcs-dev-tooling-0.db"), .devtool_empty_shard())
  suppressMessages(run_merge(r$io, tempfile("m_"), parts))
  sig <- .tbl(.pub(r$io), "vcs_ai_signals")
  expect_false(any(sig$repo_id == .B))
  expect_equal(sig$last_confirmed_date[sig$repo_id == .A], "2026-09-27")
})

test_that("a merge run twice over the same parts adds a weekly read's commits once", {
  # The rerun reads the watermark the first run published, so the week's add is off its chain.
  through <- "2026-09-20T08:00:00Z"
  today <- format(Sys.Date())
  model <- function(n) data.frame(repo_id = .A, tool = "claude", provider = NA_character_, family = "Opus",
                                  version = "4.8", context_window = NA_character_, commits = n,
                                  first_seen = "2025-01-01", last_seen = "2026-09-01", window_complete = 1L,
                                  stringsAsFactors = FALSE)
  logged <- function(n) data.frame(repo_id = .A, rule_key = "msg.claude.session", rule_rev = 1L,
                                   ruleset_version = AI_RULESET_VERSION, asked_on = "2026-09-20", outcome = "hit",
                                   total_count = n, verified = 1L, incomplete = 0L, first_hit_on = "2025-02-01",
                                   source = "read", stringsAsFactors = FALSE)
  r <- .seed_release(function(con) {
    DBI::dbWriteTable(con, "vcs_ai_repo_reads", data.frame(repo_id = .A, commits_read_on = "2026-09-20",
      commits_read_through = through, commits_ruleset = AI_RULESET_VERSION, commits_history_complete = 1L,
      stringsAsFactors = FALSE), append = TRUE)
    DBI::dbWriteTable(con, "vcs_ai_models", model(10L), append = TRUE)
    DBI::dbWriteTable(con, "vcs_ai_search_log", logged(10L), append = TRUE)
  })
  parts <- tempfile("parts_"); dir.create(parts)
  export_ai_shard(file.path(parts, "vcs-ai-shard-0.db"), .ai_empty_signals())
  write_cheap_partial(file.path(parts, "vcs-ai-cheap-0.db"), list(
    repo_reads = data.frame(repo_id = .A, commits_read_on = today, commits_read_through = paste0(today, "T08:00:00Z"),
                            commits_ruleset = AI_RULESET_VERSION, commits_history_complete = 1L, reached_first = 0L,
                            stringsAsFactors = FALSE),
    models = cbind(model(2L), mode = "add", read_after = through),
    search_log = cbind(transform(logged(2L), asked_on = today), mode = "add", read_after = through)))
  write_dev_tooling_partial(file.path(parts, "vcs-dev-tooling-0.db"), .devtool_empty_shard())
  for (run in 1:2) {
    suppressMessages(run_merge(r$io, tempfile("m_"), parts))
    s <- .pub(r$io)
    expect_equal(nrow(.tbl(s, "vcs_ai_repo_reads")), 1L)
    expect_equal(.tbl(s, "vcs_ai_models")$commits, 12L)
    lg <- .tbl(s, "vcs_ai_search_log")
    expect_equal(lg$total_count[lg$rule_key == "msg.claude.session"], 12L)
  }
})

test_that("a full gate's campaign date is kept until every flagged repository has been asked since", {
  r <- .seed_release()
  today <- format(Sys.Date())
  since <- function() {
    st <- .tbl(.pub(r$io, "vcs-signals-recent.db"), "pipeline_state")
    st$value[st$key == "ai_campaign_since"]
  }
  parts <- function(whole) {
    p <- tempfile("parts_"); dir.create(p)
    export_ai_shard(file.path(p, "vcs-ai-shard-0.db"), .ai_empty_signals(),
                    extra = list(campaign = data.frame(since = "2026-09-20", stringsAsFactors = FALSE)))
    write_cheap_partial(file.path(p, "vcs-ai-cheap-0.db"), list(
      flagged = data.frame(repo_id = .A, owner = "o", name = "a", node_id = "R_a", is_fork = 0L,
                           parent = NA_character_, pr_onset_date = NA_character_, stringsAsFactors = FALSE),
      repo_reads = data.frame(repo_id = .A, commits_read_on = today, commits_ruleset = AI_RULESET_VERSION,
                              commits_history_complete = as.integer(whole), reached_first = as.integer(whole),
                              stringsAsFactors = FALSE)))
    write_dev_tooling_partial(file.path(p, "vcs-dev-tooling-0.db"), .devtool_empty_shard())
    p
  }
  suppressMessages(run_merge(r$io, tempfile("m_"), parts(whole = FALSE)))
  expect_equal(since(), "2026-09-20")
  msgs <- testthat::capture_messages(run_merge(r$io, tempfile("m_"), parts(whole = TRUE)))
  expect_length(since(), 0L)
  expect_true(any(grepl("the campaign since 2026-09-20 has asked every flagged repository", msgs, fixed = TRUE)))
})

test_that("a read to the first commit behind the stored watermark replaces no counts", {
  # ai-merge-rerun over an older run, after a newer one published: its whole-history read saw less.
  stored_through <- "2026-09-27T08:00:00Z"
  model <- function(n) data.frame(repo_id = .A, tool = "claude", provider = NA_character_, family = "Opus",
                                  version = "4.8", context_window = NA_character_, commits = n,
                                  first_seen = "2025-01-01", last_seen = "2026-09-01", window_complete = 1L,
                                  stringsAsFactors = FALSE)
  logged <- function(n, on) data.frame(repo_id = .A, rule_key = "msg.claude.session", rule_rev = 1L,
                                       ruleset_version = AI_RULESET_VERSION, asked_on = on, outcome = "hit",
                                       total_count = n, verified = 1L, incomplete = 0L, first_hit_on = "2025-02-01",
                                       source = "read", stringsAsFactors = FALSE)
  r <- .seed_release(function(con) {
    DBI::dbWriteTable(con, "vcs_ai_repo_reads", data.frame(repo_id = .A, commits_read_on = "2026-09-27",
      commits_read_through = stored_through, commits_ruleset = AI_RULESET_VERSION, commits_history_complete = 1L,
      stringsAsFactors = FALSE), append = TRUE)
    DBI::dbWriteTable(con, "vcs_ai_models", model(12L), append = TRUE)
    DBI::dbWriteTable(con, "vcs_ai_search_log", logged(12L, "2026-09-27"), append = TRUE)
  })
  parts <- tempfile("parts_"); dir.create(parts)
  export_ai_shard(file.path(parts, "vcs-ai-shard-0.db"), .ai_empty_signals())
  write_cheap_partial(file.path(parts, "vcs-ai-cheap-0.db"), list(
    repo_reads = data.frame(repo_id = .A, commits_read_on = "2026-09-20", commits_read_through = "2026-09-20T08:00:00Z",
                            commits_ruleset = AI_RULESET_VERSION, commits_history_complete = 1L, reached_first = 1L,
                            stringsAsFactors = FALSE),
    models = cbind(model(10L), mode = "replace", read_after = NA_character_),
    search_log = cbind(logged(10L, "2026-09-20"), mode = "replace", read_after = NA_character_)))
  write_dev_tooling_partial(file.path(parts, "vcs-dev-tooling-0.db"), .devtool_empty_shard())
  suppressMessages(run_merge(r$io, tempfile("m_"), parts))
  s <- .pub(r$io)
  expect_equal(.tbl(s, "vcs_ai_repo_reads")$commits_read_through, stored_through)
  expect_equal(.tbl(s, "vcs_ai_models")$commits, 12L)
  lg <- .tbl(s, "vcs_ai_search_log")
  expect_equal(lg$total_count[lg$rule_key == "msg.claude.session"], 12L)
})
