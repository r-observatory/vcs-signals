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
