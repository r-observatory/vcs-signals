# Which model, not just which tool.
#
# Parsed structurally rather than matched against a list of known models: an
# enumerated list silently drops the next model to ship, which is the same
# failure as discarding an unknown tool slug instead of showing it by name.
# Every string here was counted in a real message body during the trailer survey.

test_that("Claude's grammar yields family, version and context separately", {
  got <- extract_ai_model("claude",
    "feat: x\n\nCo-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>")
  expect_equal(got$family, "Opus")
  expect_equal(got$version, "4.8")
  expect_equal(got$context_window, "1M")
  expect_true(is.na(got$provider))
})

test_that("a stated model without a context window leaves the context silent", {
  # A NULL context does not mean the default window. It means the trailer said
  # nothing, and most of them say nothing.
  got <- extract_ai_model("claude",
    "Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>")
  expect_equal(got$family, "Sonnet")
  expect_equal(got$version, "4.6")
  expect_true(is.na(got$context_window))
})

test_that("a bare Claude trailer states no model at all", {
  # Two of 215 sampled trailers name nothing. That is "not stated": not an
  # unknown model, not an old one, not a default.
  got <- extract_ai_model("claude", "Co-Authored-By: Claude <noreply@anthropic.com>")
  expect_true(is.na(got$family))
  expect_true(is.na(got$version))
  expect_true(is.na(got$context_window))
})

test_that("an unfamiliar family is kept verbatim rather than dropped", {
  # Fable was not in anyone's list of Claude families until it shipped.
  got <- extract_ai_model("claude", "Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>")
  expect_equal(got$family, "Fable")
  expect_equal(got$version, "5")

  invented <- extract_ai_model("claude",
    "Co-Authored-By: Claude Quartet 9.1 <noreply@anthropic.com>")
  expect_equal(invented$family, "Quartet")
  expect_equal(invented$version, "9.1")
})

test_that("Aider names the provider it routed through, including a local one", {
  # The one fact on the site that says whether a maintainer ran a hosted model
  # or their own machine.
  a <- extract_ai_model("aider", "Co-authored-by: aider (openai/DeepSeek-R1) <aider@aider.chat>")
  expect_equal(a$provider, "openai")
  expect_equal(a$family, "DeepSeek-R1")

  local <- extract_ai_model("aider", "Co-authored-by: aider (ollama/gemma3:e4b-mlx) <aider@aider.chat>")
  expect_equal(local$provider, "ollama")
  expect_equal(local$family, "gemma3:e4b-mlx")
})

test_that("Aider's model keeps any further slashes, because that shape is upstream's", {
  # Splitting ollama/gemma3:e4b-mlx into parts would invent structure we do not
  # control. Only the provider is separated, on the first slash.
  got <- extract_ai_model("aider",
    "Co-authored-by: aider (openrouter/deepseek/deepseek-v3-flash-N) <aider@aider.chat>")
  expect_equal(got$provider, "openrouter")
  expect_equal(got$family, "deepseek/deepseek-v3-flash-N")
})

test_that("Gemini states a version and variant on some trailers and none on others", {
  named <- extract_ai_model("gemini", "Co-Authored-By: Gemini 2.5 Flash <noreply@google.com>")
  expect_equal(named$version, "2.5")
  expect_equal(named$family, "Flash")

  bare <- extract_ai_model("gemini", "Co-Authored-By: Gemini <gemini@google.com>")
  expect_true(is.na(bare$version))
  expect_true(is.na(bare$family))
})

test_that("a tool whose trailer carries no model yields no model row", {
  # A blank for these means their trailers carry no model, not that no model
  # was used.
  for (t in c("cursor", "devin", "openhands", "jules", "windsurf", "codex")) {
    got <- extract_ai_model(t, "Co-authored-by: Cursor <cursoragent@cursor.com>")
    expect_true(is.na(got$family), info = t)
    expect_true(is.na(got$version), info = t)
  }
})

test_that("model rows tally per repository and tool, with a first and last seen", {
  items <- data.frame(
    date = c("2025-01-01", "2025-03-01", "2025-02-01"),
    message = c("Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>",
                "Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>",
                "Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>"),
    stringsAsFactors = FALSE)
  got <- build_ai_model_rows("github.com/o/r", "claude", items, window_complete = TRUE)
  opus <- got[got$family == "Opus", ]
  expect_equal(opus$commits, 2L)
  expect_equal(opus$first_seen, "2025-01-01")
  expect_equal(opus$last_seen, "2025-03-01")
  expect_equal(nrow(got), 2L)
  expect_true(all(got$window_complete == 1L))
})

test_that("a partial window says so, so a reader can tell a tally from a sample", {
  items <- data.frame(date = "2025-01-01",
                      message = "Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>",
                      stringsAsFactors = FALSE)
  got <- build_ai_model_rows("github.com/o/r", "claude", items, window_complete = FALSE)
  expect_equal(got$window_complete, 0L)
})

test_that("trailers that state no model produce no rows rather than a blank one", {
  items <- data.frame(date = "2025-01-01",
                      message = "Co-Authored-By: Claude <noreply@anthropic.com>",
                      stringsAsFactors = FALSE)
  expect_equal(nrow(build_ai_model_rows("github.com/o/r", "claude", items, TRUE)), 0L)
})

test_that("a message with no trailer at all contributes nothing", {
  items <- data.frame(date = "2025-01-01", message = "fix: unrelated commit",
                      stringsAsFactors = FALSE)
  expect_equal(nrow(build_ai_model_rows("github.com/o/r", "claude", items, TRUE)), 0L)
})

test_that("the model parser is exercised against the observed corpus, not invented strings", {
  # Same discipline as the detection rules: these are strings the world emitted,
  # and the parser must agree with them rather than with my idea of them.
  path <- testthat::test_path("..", "fixtures", "observed-trailers.tsv")
  if (!file.exists(path)) path <- file.path("tests", "fixtures", "observed-trailers.tsv")
  cp <- utils::read.delim(path, comment.char = "#", quote = "", colClasses = "character")
  cp <- cp[!grepl("^NOT-", cp$tool), ]

  states_a_model <- function(tool, s) {
    m <- extract_ai_model(tool, s)
    any(!is.na(unlist(m)))
  }
  # The three tools that carry model detail, and only on the shapes that state it.
  expect_true(states_a_model("claude",
    "Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"))
  expect_true(states_a_model("gemini",
    "Co-Authored-By: Gemini 2.5 Flash <noreply@google.com>"))
  expect_true(states_a_model("aider",
    "Co-authored-by: aider (openai/DeepSeek-R1) <aider@aider.chat>"))

  # No tool outside those three may yield a model from any observed string, and
  # a bare trailer from those three must stay silent.
  quiet <- cp[!(cp$tool %in% c("claude", "gemini", "aider")), ]
  for (i in seq_len(nrow(quiet))) {
    expect_false(states_a_model(quiet$tool[i], quiet$string[i]),
                 info = sprintf("%s invented a model from: %s", quiet$tool[i], quiet$string[i]))
  }
  for (s in c("Co-Authored-By: Claude <noreply@anthropic.com>",
              "Co-Authored-By: Gemini <gemini@google.com>",
              "Co-authored-by: gemini-code-assist[bot] <1+x@users.noreply.github.com>")) {
    tool <- if (grepl("Claude", s)) "claude" else "gemini"
    expect_false(states_a_model(tool, s), info = s)
  }
})

test_that("run_deep writes the model named in a verified trailer through to the shard", {
  out <- tempfile("out_"); dir.create(out)
  write_flagged_partial(file.path(out, "vcs-ai-flagged-roster.db"),
    data.frame(repo_id = "github.com/o/r", owner = "o", name = "r", node_id = "R_1",
               is_fork = 0L, parent = NA_character_, pr_onset_date = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(repo_id = "github.com/o/r", tool = "claude", tier = "D",
               marker = "CLAUDE.md", agnostic = 0L, stringsAsFactors = FALSE))
  page <- data.frame(
    date = c("2025-06-01T00:00:00Z", "2025-07-01T00:00:00Z"),
    message = c("feat: a\n\nCo-authored-by: Claude Opus 4.8 (1M context) <noreply@anthropic.com>",
                "feat: b\n\nCo-authored-by: Claude Sonnet 4.6 <noreply@anthropic.com>"),
    stringsAsFactors = FALSE)
  io <- list(
    graphql = function(query) list(data = list(repository = list(defaultBranchRef = list(
      target = list(history = list(pageInfo = list(endCursor = "", hasNextPage = FALSE),
        nodes = list(list(committedDate = "2025-05-01T00:00:00Z")))))))),
    search_hit = function(owner, name, query, delay = 0) {
      if (!grepl("Co-Authored-By: Claude", query, fixed = TRUE))
        return(list(date = NA_character_, message = NA_character_, author = NA_character_,
                    total_count = 0L, items = page[0, ], unavailable = FALSE))
      list(date = page$date[1], message = page$message[1], author = "Jane",
           total_count = 2L, items = page, unavailable = FALSE)
    })
  suppressMessages(run_deep(io, out, file.path(out, "vcs-ai-flagged-roster.db"), 0, 1,
                            marker_delay = 0, search_delay = 0))
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-ai-shard-0.db"))
  on.exit(DBI::dbDisconnect(scon))
  got <- DBI::dbReadTable(scon, "vcs_ai_models")
  expect_equal(nrow(got), 2L)
  expect_setequal(got$family, c("Opus", "Sonnet"))
  expect_equal(got$context_window[got$family == "Opus"], "1M")
  expect_true(is.na(got$context_window[got$family == "Sonnet"]))
  expect_true(all(got$window_complete == 1L))   # 2 of 2 seen
})

test_that("a page that does not hold every hit is marked incomplete", {
  items <- data.frame(date = "2025-01-01",
                      message = "Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>",
                      stringsAsFactors = FALSE)
  # 40 matching commits, one page examined: the tally is of the window, not history.
  complete <- 40L <= nrow(items)
  got <- build_ai_model_rows("github.com/o/r", "claude", items, window_complete = complete)
  expect_equal(got$window_complete, 0L)
  expect_equal(got$commits, 1L)
})

test_that("a weekly read adds its new model commits to the stored tally", {
  m <- function(n, first, last, mode = NULL, after = NA_character_) {
    r <- data.frame(repo_id = "r", tool = "claude", provider = NA_character_, family = "Opus", version = "4.8",
                    context_window = NA_character_, commits = n, first_seen = first, last_seen = last,
                    window_complete = 1L, stringsAsFactors = FALSE)
    if (!is.null(mode)) { r$mode <- mode; r$read_after <- after }
    r
  }
  through <- "2026-09-20T08:00:00Z"
  reads <- data.frame(repo_id = "r", commits_read_through = through, stringsAsFactors = FALSE)
  got <- fold_models(m(10L, "2025-01-01", "2026-09-01"), .ai_empty_models(),
                     m(2L, "2026-10-02", "2026-10-03", "add", through), rebuilt_repos = character(0), reads = reads)
  expect_equal(got$commits, 12L); expect_equal(got$first_seen, "2025-01-01"); expect_equal(got$last_seen, "2026-10-03")
  replaced <- fold_models(m(10L, "2025-01-01", "2026-09-01"), .ai_empty_models(),
                          m(4L, "2025-02-01", "2026-10-03", "replace"), rebuilt_repos = "r", reads = reads)
  expect_equal(replaced$commits, 4L)
  # Counted from another watermark, or by a read to the first commit the read state did not keep: nothing moves.
  off_chain <- fold_models(m(10L, "2025-01-01", "2026-09-01"), .ai_empty_models(),
                           m(2L, "2026-10-02", "2026-10-03", "add", "2026-09-13T08:00:00Z"),
                           rebuilt_repos = character(0), reads = reads)
  expect_equal(off_chain$commits, 10L)
  not_kept <- fold_models(m(10L, "2025-01-01", "2026-09-01"), .ai_empty_models(),
                          m(4L, "2025-02-01", "2026-10-03", "replace"), rebuilt_repos = character(0), reads = reads)
  expect_equal(not_kept$commits, 10L)
  expect_error(fold_models(m(10L, "2025-01-01", "2026-09-01"), .ai_empty_models(),
                           m(2L, "2026-10-02", "2026-10-03", "add", through), rebuilt_repos = character(0)),
               "reads is NULL")
})

test_that("a read to the first commit that names no model clears the older tallies", {
  m <- function(repo, family) data.frame(repo_id = repo, tool = "claude", provider = NA_character_, family = family,
                                         version = "4.8", context_window = NA_character_, commits = 10L,
                                         first_seen = "2025-01-01", last_seen = "2026-09-01", window_complete = 1L,
                                         stringsAsFactors = FALSE)
  got <- fold_models(rbind(m("r", "Opus"), m("s", "Opus")), .ai_empty_models(), NULL, rebuilt_repos = "r")
  expect_equal(got$repo_id, "s")
  # This run's search rows are not older tallies, so they stay.
  expect_equal(fold_models(m("r", "Opus"), m("r", "Sonnet"), NULL, rebuilt_repos = "r")$family, "Sonnet")
})

.pm <- function(family, version, ctx, n, first, last, complete = 1L, tool = "claude", provider = NA_character_,
                repo = "github.com/adibender/pammtools")
  data.frame(repo_id = repo, tool = tool, provider = provider, family = family, version = version,
             context_window = ctx, commits = n, first_seen = first, last_seen = last,
             window_complete = complete, stringsAsFactors = FALSE)

test_that("Claude rules that match the same commits give one row per model", {
  out <- tempfile("out_"); dir.create(out)
  rid <- "github.com/o/r"
  work <- data.frame(repo_id = rid, tool = "claude",
                     rule_key = c("msg.claude.coauthor", "msg.claude.address", "msg.claude.session"),
                     reason = "rule-new", priority = unname(AI_WORK_PRIORITY["rule-new"]), stringsAsFactors = FALSE)
  write_flagged_partial(file.path(out, "vcs-ai-flagged-roster.db"),
    data.frame(repo_id = rid, owner = "o", name = "r", node_id = "R_1", is_fork = 0L, parent = NA_character_,
               pr_onset_date = NA_character_, stringsAsFactors = FALSE),
    cbind(repo_id = rid, .ai_found("claude", "D", "CLAUDE.md")), work = work,
    campaign = data.frame(since = NA_character_, stringsAsFactors = FALSE))
  opus <- "fix: a\n\nhttps://claude.ai/code/session_01A\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
  page <- data.frame(date = c("2026-06-12T12:55:59Z", "2026-07-03T13:12:05Z", "2026-07-08T13:36:40Z"),
                     message = c(opus, sub("session_01A", "session_01B", opus),
                                 "feat: b\n\nCo-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>"),
                     stringsAsFactors = FALSE)
  hit <- function(p) list(date = p$date[1], message = p$message[1], author = "Jane", total_count = nrow(p),
                          items = p, unavailable = FALSE, incomplete = 0L)
  # The session link rides the two Opus commits, and the other two rules match all three.
  io <- list(graphql = function(query) stop("no dating in this week"),
             search_hit = function(owner, name, query, delay = 0)
               if (grepl("session_", query, fixed = TRUE)) hit(page[1:2, ]) else hit(page))
  suppressMessages(run_deep(io, out, file.path(out, "vcs-ai-flagged-roster.db"), 0, 1,
                            marker_delay = 0, search_delay = 0))
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "vcs-ai-shard-0.db"))
  on.exit(DBI::dbDisconnect(scon))
  expect_equal(nrow(DBI::dbReadTable(scon, "search_log")), 3L)
  got <- DBI::dbReadTable(scon, "vcs_ai_models")
  expect_equal(nrow(got), 2L)
  expect_equal(got$commits[got$family == "Opus"], 2L)
  expect_equal(got$commits[got$family == "Sonnet"], 1L)
  expect_equal(got$first_seen[got$family == "Opus"], "2026-06-12T12:55:59Z")
  expect_true(all(got$window_complete == 1L))
})

test_that("a pooled tally counts a commit once and is whole only when every page it pools was", {
  msg <- "Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
  page <- function(date, whole, tool = "claude", message = msg)
    data.frame(repo_id = "github.com/o/r", tool = tool, date = date, message = message, whole = whole,
               stringsAsFactors = FALSE)
  got <- pool_model_rows(rbind(page(c("2026-01-01", "2026-01-02"), TRUE), page("2026-01-01", TRUE),
                               page("2026-01-03", TRUE, "gemini", "Co-Authored-By: Gemini 2.5 Flash <noreply@google.com>")))
  expect_equal(got$commits[got$tool == "claude"], 2L)
  expect_equal(got$commits[got$tool == "gemini"], 1L)
  expect_true(all(got$window_complete == 1L))
  # A page GitHub cut short may hold more, even when every commit it showed is on another page.
  cut <- pool_model_rows(rbind(page(c("2026-01-01", "2026-01-02"), TRUE), page("2026-01-01", FALSE)))
  expect_equal(cut$commits, 2L); expect_equal(cut$window_complete, 0L)
  expect_equal(nrow(pool_model_rows(NULL)), 0L)
})

test_that("a week that asks one Claude rule keeps what the others counted, and the gate reads it as kept", {
  prior <- rbind(.pm("Fable", "5", NA, 1L, "2026-06-10T16:08:25Z", "2026-06-10T16:08:25Z"),
                 .pm("Opus", "4.6", NA, 3L, "2026-02-23T10:50:54Z", "2026-02-23T13:09:10Z"),
                 .pm("Opus", "4.8", "1M", 5L, "2026-06-16T10:13:22Z", "2026-06-17T17:44:28Z"),
                 .pm("Opus", "4.8", NA, 3L, "2026-06-12T12:55:59Z", "2026-07-08T13:36:40Z"),
                 .pm("gpt-5", NA, NA, 2L, "2026-05-01T00:00:00Z", "2026-05-02T00:00:00Z", tool = "aider",
                     provider = "openai"))
  # A new commit carries only a session link, so the session rule alone answers, naming one Opus 4.8 commit.
  one <- .pm("Opus", "4.8", NA, 1L, "2026-07-03T13:12:05Z", "2026-07-03T13:12:05Z")
  got <- fold_models(prior, one, NULL, rebuilt_repos = character(0), reads = .ai_empty_reads())
  expect_equal(nrow(got), 5L)
  expect_equal(sum(got$commits[got$tool == "claude"]), 12L)
  expect_equal(got$commits[got$tool == "aider"], 2L)
  o48 <- got[got$family %in% "Opus" & got$version %in% "4.8" & is.na(got$context_window), ]
  expect_equal(o48$commits, 3L)
  expect_equal(o48$first_seen, "2026-06-12T12:55:59Z"); expect_equal(o48$last_seen, "2026-07-08T13:36:40Z")
  # A larger count raises the stored one, and a model the stored rows lack is added.
  more <- fold_models(prior, rbind(.pm("Opus", "4.8", NA, 4L, "2026-06-12T12:55:59Z", "2026-09-01T00:00:00Z"),
                                   .pm("Fable", "5.1", NA, 1L, "2026-09-02T00:00:00Z", "2026-09-02T00:00:00Z")),
                      NULL, rebuilt_repos = character(0))
  expect_equal(more$commits[more$family %in% "Opus" & more$version %in% "4.8" & is.na(more$context_window)], 4L)
  expect_equal(more$commits[more$version %in% "5.1"], 1L)
  expect_equal(nrow(more), 6L)
  db <- function(df) {
    p <- tempfile(fileext = ".db"); con <- DBI::dbConnect(RSQLite::SQLite(), p)
    DBI::dbWriteTable(con, "vcs_ai_models", df); DBI::dbDisconnect(con); p
  }
  expect_equal(summary_regressions(db(prior), db(got)), character(0))
})

test_that("rows one rule each counted for the same model fold into one, at the larger count", {
  prior <- rbind(.pm("Opus", "4.5", NA, 32L, "2026-01-07", "2026-02-04", complete = 0L),
                 .pm("Opus", "4.5", NA, 2L, "2026-01-07", "2026-01-17"),
                 .pm("Sonnet", "4.5", NA, 15L, "2025-12-12", "2026-01-18"),
                 .pm("Sonnet", "4.5", NA, 14L, "2025-12-11", "2026-01-18"))
  got <- fold_models(prior, .ai_empty_models(), NULL, rebuilt_repos = character(0))
  expect_equal(nrow(got), 2L)
  expect_equal(got$commits[got$family == "Opus"], 32L)
  expect_equal(got$window_complete[got$family == "Opus"], 0L)
  expect_equal(got$commits[got$family == "Sonnet"], 15L)
  expect_equal(got$first_seen[got$family == "Sonnet"], "2025-12-11")
  expect_equal(got$window_complete[got$family == "Sonnet"], 1L)
  # A weekly add lands once, on the folded row.
  through <- "2026-09-20T08:00:00Z"
  reads <- data.frame(repo_id = "github.com/adibender/pammtools", commits_read_through = through,
                      stringsAsFactors = FALSE)
  add <- cbind(.pm("Sonnet", "4.5", NA, 2L, "2026-10-02", "2026-10-03"), mode = "add", read_after = through)
  added <- fold_models(prior, .ai_empty_models(), add, rebuilt_repos = character(0), reads = reads)
  expect_equal(nrow(added), 2L)
  expect_equal(added$commits[added$family == "Sonnet"], 17L)
})
