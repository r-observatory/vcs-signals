test_that("export_summary_shard writes the ai_signals_df param to vcs_ai_signals", {
  tmp <- tempfile(fileext = ".db")
  ai <- data.frame(repo_id="github.com/o/r", tool="claude", first_seen_date="2024-03-01",
                   first_seen_censored=0L, evidence_tiers="A", authored=1L,
                   last_confirmed_date="2025-01-01", stringsAsFactors=FALSE)
  empty <- function(cols) do.call(data.frame, c(setNames(rep(list(character()), length(cols)), cols),
                                                list(stringsAsFactors = FALSE)))
  export_summary_shard(tmp,
    summary_df = empty(c("package","origin","repo_id")),
    repos_df = empty(c("repo_id","first_seen","last_seen")),
    repo_packages_df = empty(c("repo_id","package","origin")),
    ai_signals_df = ai)
  scon <- DBI::dbConnect(RSQLite::SQLite(), tmp); on.exit(DBI::dbDisconnect(scon))
  got <- DBI::dbReadTable(scon, "vcs_ai_signals")
  expect_equal(nrow(got), 1); expect_equal(got$tool, "claude")
})

test_that("vcs_ai_signals survives a full publish -> re-seed round trip", {
  # Fake io backed by a local 'release' dir: upload copies in, download copies out.
  rel <- tempfile("rel_"); dir.create(rel)
  io <- local_release_io(rel)

  out1 <- tempfile("o1_"); dir.create(out1)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out1, "w.db"))
  ensure_repo_schema(con); ensure_series_schema(con)
  DBI::dbExecute(con, "INSERT INTO vcs_ai_signals (repo_id, tool, first_seen_date, first_seen_censored, evidence_tiers, authored, last_confirmed_date) VALUES ('github.com/o/r','claude','2024-03-01',0,'A',1,'2025-01-01')")
  publish(io, con, out1, tag = "current", source_kind = "live", force_full = TRUE,
          base_generation = "")
  DBI::dbDisconnect(con)

  out2 <- tempfile("o2_"); dir.create(out2)
  w2 <- file.path(out2, "w.db")
  seed_working_db(io, out2, w2)
  scon <- DBI::dbConnect(RSQLite::SQLite(), w2); on.exit(DBI::dbDisconnect(scon))
  got <- DBI::dbReadTable(scon, "vcs_ai_signals")
  expect_equal(nrow(got), 1); expect_equal(got$tool, "claude")   # survived seed<->embed<->publish
})

test_that("a failed history pull aborts instead of looking like a first run", {
  # seed_working_db used to return FALSE for three different situations, only one of
  # which was harmless. Once a release exists, the recent shard IS the accumulated
  # history: failing to pull it leaves an empty working DB, run_merge reads 0 prior
  # rows, and the published table is deleted and rewritten from one run's shards.
  out <- tempfile("seed_"); dir.create(out)
  io <- list(release_exists = function() TRUE,
             generation = function() "vcs-signals-recent.db\tsha256:0",
             download = function(pattern, dir) FALSE)
  expect_error(seed_working_db(io, out, file.path(out, "work.db")),
               "could not be downloaded")
})

test_that("a download that reports success but leaves no file also aborts", {
  out <- tempfile("seed_"); dir.create(out)
  io <- list(release_exists = function() TRUE,
             generation = function() "vcs-signals-recent.db\tsha256:0",
             download = function(pattern, dir) TRUE)   # lies: writes nothing
  expect_error(seed_working_db(io, out, file.path(out, "work.db")),
               "not on disk")
})

test_that("no release at all is still a legitimate first run", {
  # The one harmless case must stay harmless, or every bootstrap breaks.
  out <- tempfile("seed_"); dir.create(out)
  io <- list(release_exists = function() FALSE,
             generation = function() "",
             download = function(pattern, dir) stop("must not be called"))
  expect_false(seed_working_db(io, out, file.path(out, "work.db")))
})

test_that("a failed asset upload stops the run rather than publishing green", {
  # publish() writes release notes describing the assets immediately afterwards, and
  # the merger downstream reads whatever bytes are on the release. A discarded status
  # meant a 403 produced a green run whose notes described data that never landed.
  skip_if(nzchar(Sys.which("gh")) == FALSE, "gh not available")
  expect_error(
    gh_release_upload("r-observatory/definitely-not-a-real-repo-xyz",
                      tempfile(fileext = ".db")),
    "gh release upload failed")
})

.row <- function(n, on, col = "authored") {
  r <- .ai_align_signals(data.frame(repo_id = "github.com/o/r", tool = "claude", first_seen_date = "2026-01-01",
    first_seen_censored = 0L, evidence_tiers = "A", authored = 1L, last_confirmed_date = "2026-10-04",
    stringsAsFactors = FALSE))
  r[[paste0(col, "_commits")]] <- as.integer(n); r[[paste0(col, "_measured_on")]] <- on
  r
}
.log <- function(repo, key, outcome, n, src = "search", on = "2026-10-05", first = "2025-01-02", rev = 1L)
  data.frame(repo_id = repo, rule_key = key, rule_rev = rev, ruleset_version = AI_RULESET_VERSION,
             asked_on = on, outcome = outcome,
             total_count = if (outcome == "refused") NA_integer_ else as.integer(n),
             verified = if (outcome == "hit") 1L else NA_integer_, incomplete = 0L,
             first_hit_on = if (outcome == "hit") first else NA_character_, source = src,
             stringsAsFactors = FALSE)
.read_state <- function(on, through, repo = "github.com/o/r")
  .ai_bind_like(.ai_empty_reads(), list(data.frame(repo_id = repo, commits_read_on = on,
    commits_read_through = through, commits_ruleset = AI_RULESET_VERSION, commits_history_complete = 1L,
    accounts_counted_on = on, stringsAsFactors = FALSE)))
.add <- function(n, on, after, first = "2026-10-08", key = "msg.claude.coauthor") {
  a <- .log("github.com/o/r", key, "hit", n, src = "read", on = on, first = first)
  a$mode <- "add"; a$read_after <- after
  a
}

test_that("the latest measured count wins over a larger older one", {
  out <- ai_onset_reducer(.row(57, "2026-09-27"), .row(40, "2026-10-04"))
  expect_equal(out$authored_commits, 40L)
  expect_equal(out$authored_measured_on, "2026-10-04")
})

test_that("rows written before counts were dated still take the largest", {
  out <- ai_onset_reducer(.row(7, NA), .row(57, NA))
  expect_equal(out$authored_commits, 57L)
})

test_that("a credit count is the largest usable rule count, and zero only when every rule answered none", {
  r <- "github.com/o/r"
  sig <- .row(NA, NA, "assisted")
  sig$evidence_tiers <- "B"; sig$markers <- "msg.claude.coauthor"
  hit <- rbind(.log(r, "msg.claude.coauthor", "hit", 37, first = "2025-06-01"),
               .log(r, "msg.claude.generated", "hit", 12))
  got <- derive_assisted_counts(sig, hit, .ai_empty_reads())
  expect_equal(got$assisted_commits, 37L); expect_equal(got$assisted_measured_on, "2026-10-05")
  claude_always <- Filter(function(x) identical(x$tool, "claude") && identical(x$search, "always"),
                          AI_TRAILER_PATTERNS)
  none <- do.call(rbind, lapply(claude_always, function(x) .log(r, x$key, "none", 0, rev = x$rev)))
  expect_equal(derive_assisted_counts(sig, none, .ai_empty_reads())$assisted_commits, 0L)
  some_refused <- none; some_refused$outcome[1] <- "refused"; some_refused$total_count[1] <- NA_integer_
  expect_true(is.na(derive_assisted_counts(sig, some_refused, .ai_empty_reads())$assisted_commits))
})

test_that("Assisted-by counts come from a read of the whole history, never from its search", {
  r <- "github.com/o/r"
  cop <- .row(NA, NA, "assisted"); cop$tool <- "copilot"; cop$evidence_tiers <- "B"
  cla <- .row(NA, NA, "assisted"); cla$evidence_tiers <- "B"
  log <- rbind(.log(r, "msg.any.assisted-by.copilot", "hit", 50, src = "read"),
               .log(r, "msg.any.assisted-by.claude", "hit", 1, src = "read"))
  got <- derive_assisted_counts(rbind(cop, cla), log, .ai_empty_reads())
  expect_equal(got$assisted_commits[got$tool == "copilot"], 50L)
  expect_equal(got$assisted_commits[got$tool == "claude"], 1L)
  only_search <- .log(r, "msg.any.assisted-by", "hit", 51)
  expect_true(is.na(derive_assisted_counts(cop, only_search, .ai_empty_reads())$assisted_commits))
})

test_that("a VS Code Copilot count is used only when its earliest hit is after the false window", {
  r <- "github.com/o/r"
  cop <- .row(NA, NA, "assisted"); cop$tool <- "copilot"; cop$evidence_tiers <- "B"
  early <- .log(r, "msg.copilot.vscode", "hit", 9, first = "2026-04-25")
  late <- .log(r, "msg.copilot.vscode", "hit", 9, first = "2026-05-07")
  expect_true(is.na(derive_assisted_counts(cop, early, .ai_empty_reads())$assisted_commits))
  expect_equal(derive_assisted_counts(cop, late, .ai_empty_reads())$assisted_commits, 9L)
})

test_that("a whole-history read that matched no credit rule is a measured zero", {
  r <- "github.com/o/r"
  sig <- .row(NA, NA, "assisted"); sig$evidence_tiers <- "D"
  reads <- .ai_bind_like(.ai_empty_reads(), list(data.frame(repo_id = r, commits_read_on = "2026-10-04",
    commits_ruleset = AI_RULESET_VERSION, commits_history_complete = 1L, stringsAsFactors = FALSE)))
  got <- derive_assisted_counts(sig, .ai_empty_log(), reads)
  expect_equal(got$assisted_commits, 0L); expect_equal(got$assisted_measured_on, "2026-10-04")
})

test_that("the search log keeps the latest answer, and a weekly read adds to a whole-history count", {
  r <- "github.com/o/r"; t0 <- "2026-10-03T12:00:00Z"
  prior <- rbind(.log(r, "msg.claude.coauthor", "hit", 10, src = "read", on = "2026-10-04"),
                 .log(r, "msg.cursor.made-with", "refused", NA, on = "2026-10-04"))
  add <- .add(3, "2026-10-11", t0)
  new_key <- .add(1, "2026-10-11", t0, key = "msg.aider.coauthor")
  ask <- .log(r, "msg.cursor.made-with", "none", 0, on = "2026-10-12")
  ask$mode <- NA_character_; ask$read_after <- NA_character_
  got <- fold_search_log(prior, rbind(add, new_key, ask), rebuilt_repos = character(0),
                         reads = .read_state("2026-10-04", t0))
  expect_equal(got$total_count[got$rule_key == "msg.claude.coauthor"], 13L)
  expect_equal(got$first_hit_on[got$rule_key == "msg.claude.coauthor"], "2025-01-02")
  expect_equal(got$outcome[got$rule_key == "msg.cursor.made-with"], "none")
  expect_equal(got$total_count[got$rule_key == "msg.aider.coauthor"], 1L)
  rebuilt <- fold_search_log(prior, .ai_empty_log(), rebuilt_repos = r)
  expect_equal(nrow(rebuilt), 0L)
  expect_false(any(c("mode", "read_after") %in% names(got)))
})

test_that(
  "a weekly read folded twice, or two weekly reads from one watermark, never count their overlap twice", {
  t0 <- "2026-10-03T12:00:00Z"
  pub <- .log("github.com/o/r", "msg.claude.coauthor", "hit", 10, src = "read", on = "2026-10-04")
  pub_reads <- .read_state("2026-10-04", t0)
  sun <- .add(3, "2026-10-11", t0)
  once <- fold_search_log(pub, sun, reads = pub_reads)
  expect_equal(once$total_count, 13L)
  moved <- fold_repo_reads(pub_reads, .read_state("2026-10-11", "2026-10-10T20:00:00Z"))
  expect_equal(fold_search_log(once, sun, reads = moved)$total_count, 13L)
  sat <- .add(2, "2026-10-10", t0, first = "2026-10-06")
  sat_first <- fold_search_log(pub, sat, reads = pub_reads)
  sat_reads <- fold_repo_reads(pub_reads, .read_state("2026-10-10", "2026-10-09T08:00:00Z"))
  expect_equal(fold_search_log(sat_first, sun, reads = sat_reads)$total_count, 12L)
})

test_that("an add that meets a newer search row, or has no stored read row, is dropped", {
  r <- "github.com/o/r"; t0 <- "2026-10-03T12:00:00Z"
  pub <- .log(r, "msg.claude.coauthor", "hit", 10, src = "read", on = "2026-10-04")
  asked <- .log(r, "msg.claude.coauthor", "hit", 12, on = "2026-10-11")
  asked$mode <- NA_character_; asked$read_after <- NA_character_
  got <- fold_search_log(pub, rbind(asked, .add(3, "2026-10-11", t0)), reads = .read_state("2026-10-04", t0))
  expect_equal(nrow(got), 1L); expect_equal(got$source, "search"); expect_equal(got$total_count, 12L)
  elsewhere <- .read_state("2026-10-04", t0, repo = "github.com/o/other")
  expect_equal(fold_search_log(pub, .add(3, "2026-10-11", t0), reads = elsewhere)$total_count, 10L)
  expect_error(fold_search_log(pub, .add(3, "2026-10-11", t0)), "stored read state")
})

test_that("an older read merged after a newer one moves no watermark back", {
  sun <- .read_state("2026-10-11", "2026-10-10T20:00:00Z")
  got <- fold_repo_reads(sun, .read_state("2026-10-10", "2026-10-09T08:00:00Z"))
  expect_equal(got$commits_read_on, "2026-10-11")
  expect_equal(got$commits_read_through, "2026-10-10T20:00:00Z")
  expect_equal(got$accounts_counted_on, "2026-10-11")
  same_day <- fold_repo_reads(sun, .read_state("2026-10-11", "2026-10-10T22:00:00Z"))
  expect_equal(same_day$commits_read_through, "2026-10-10T22:00:00Z")
})

test_that("an author-name suffix count is not a count of commits crediting aider", {
  r <- "github.com/o/r"
  aid <- .row(NA, NA, "assisted"); aid$tool <- "aider"; aid$evidence_tiers <- "C"
  hit <- .log(r, "name.aider.suffix", "hit", 5)
  expect_true(is.na(derive_assisted_counts(aid, hit, .ai_empty_reads())$assisted_commits))
  read_hit <- .log(r, "name.aider.suffix", "hit", 5, src = "read")
  got <- derive_assisted_counts(aid, read_hit, .read_state("2026-10-04", "2026-10-03T12:00:00Z"))
  expect_equal(got$assisted_commits, 0L); expect_equal(got$assisted_measured_on, "2026-10-04")
})

test_that("review rows fold to one per tool and take their count and first date from the log", {
  r <- "github.com/o/r"
  rv <- review_rows(.ai_found(c("copilot-review", "copilot-review"), "B", "review.copilot.suggestion",
                              role = "review", rule_key = "review.copilot.suggestion",
                              onset = c("2026-09-01T00:00:00Z", "2026-08-01T00:00:00Z")), r, "2026-10-04")
  folded <- fold_review_rows(.ai_empty_review(), rv)
  expect_equal(nrow(folded), 1L); expect_equal(folded$first_seen_date, "2026-08-01T00:00:00Z")
  got <- derive_review_counts(folded, .log(r, "review.copilot.suggestion", "hit", 4, first = "2025-12-01"))
  expect_equal(got$assisted_commits, 4L); expect_equal(got$first_seen_date, "2025-12-01")
  expect_equal(got$first_seen_censored, 0L)
})

test_that("an outside pull request seen again keeps one row with the newest date", {
  o <- data.frame(repo_id = "g", pr_number = 927L, tool = "cursor", found_via = "pr.cursor.agent-branch",
                  created_at = "2026-08-28T09:43:30Z", from_fork = 1L, author_association = "NONE",
                  last_confirmed_date = "2026-10-04", stringsAsFactors = FALSE)
  o2 <- o; o2$last_confirmed_date <- "2026-10-11"
  got <- fold_outside_prs(o, o2)
  expect_equal(nrow(got), 1L); expect_equal(got$last_confirmed_date, "2026-10-11")
})
