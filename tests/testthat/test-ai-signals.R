test_that("classify_tree_markers matches root and .github entries, tags agents-md agnostic", {
  out <- classify_tree_markers(c("CLAUDE.md", ".cursor", "README.md", "AGENTS.md"),
                               c("copilot-instructions.md", "workflows"))
  expect_setequal(out$tool, c("claude", "cursor", "copilot", "agents-md"))
  expect_true(all(out$tier == "D"))
  expect_true(out$agnostic[out$tool == "agents-md"])
  expect_false(any(out$agnostic[out$tool != "agents-md"]))
})

test_that("classify_tree_markers does not match a github-located marker at root", {
  out <- classify_tree_markers(c("copilot-instructions.md"), character(0))
  expect_equal(nrow(out), 0)   # copilot marker only counts inside .github
})

test_that("classify_tree_markers returns typed empty frame when nothing matches", {
  out <- classify_tree_markers(c("R", "man", "DESCRIPTION"), c("workflows"))
  expect_equal(nrow(out), 0)
  expect_true(all(c("tool","tier","marker","agnostic") %in% names(out)))
})

test_that("scan_ignore_tokens anchors whole entries and ignores comments and word-substrings", {
  gi <- c("# ai stuff", ".aiderignore", "*.o", "codex_output/")   # codex_output must NOT match codex
  rb <- c("^\\.cursor$", ".cursorignore")
  out <- scan_ignore_tokens(gi, rb)
  expect_true("aider" %in% out$tool)                         # .aiderignore is a real AI_MARKERS aider path
  expect_true("cursor" %in% out$tool)
  expect_false("codex" %in% out$tool)                        # substring collision rejected
  expect_true(all(grepl("^ignore:", out$marker)))
})

test_that("scan_ignore_tokens returns typed empty frame on no match", {
  out <- scan_ignore_tokens(c("*.tmp", "# nothing"), character(0))
  expect_equal(nrow(out), 0)
  expect_true(all(c("tool","tier","marker","agnostic") %in% names(out)))
})
