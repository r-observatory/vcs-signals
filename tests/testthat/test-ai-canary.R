test_that("a channel that detects nothing anywhere is reported, per tool", {
  # Tier A totalled 103 detections while four of its six identities had never
  # produced one, so a tier-level check saw a healthy channel. The granularity
  # is what makes this catch anything.
  rows <- data.frame(
    repo_id = c("a", "b", "c"),
    tool    = c("claude", "copilot", "claude"),
    evidence_tiers = c("A,D", "A,D", "B,D"),
    stringsAsFactors = FALSE)
  # cursor, devin, jules, openhands have bot identities and no tier-A row;
  # every trailer-ruled tool except claude has no tier-B row.
  # known = NULL isolates the detection logic: this test is about granularity,
  # and must not change its verdict every time someone records an allowlist entry.
  out <- ai_silent_channels(rows, known = NULL)
  expect_true(any(out$tier == "A" & out$tool == "cursor"))
  expect_true(any(out$tier == "A" & out$tool == "devin"))
  expect_false(any(out$tier == "A" & out$tool == "claude"))
  expect_false(any(out$tier == "A" & out$tool == "copilot"))
  expect_true(any(out$tier == "B" & out$tool == "codex"))
  expect_false(any(out$tier == "B" & out$tool == "claude"))
})

test_that("a channel with a recorded reason is not reported again", {
  rows <- data.frame(repo_id = "a", tool = "claude", evidence_tiers = "A",
                     stringsAsFactors = FALSE)
  known <- data.frame(
    tier = "A", tool = "devin",
    reason = "no config marker, so the tier-A gate never reaches it",
    recorded_on = "2026-08-01", stringsAsFactors = FALSE)
  out <- ai_silent_channels(rows, known)
  expect_false(any(out$tier == "A" & out$tool == "devin"))
  expect_true(any(out$tier == "A" & out$tool == "jules"))
})

test_that("an empty detection table reports every channel rather than none", {
  # The failure this guards against is a scan that produced nothing at all. It
  # must be loud, not silently satisfied by having no rows to check.
  out <- ai_silent_channels(.ai_empty_signals()[0, , drop = FALSE])
  expect_true(nrow(out) > 10)
})

test_that("every allowlist entry names a channel that actually has a rule", {
  # A typo'd tool name silences nothing and reads as though it did, which is the
  # allowlist quietly not working.
  inv <- paste(ai_rule_inventory()$tier, ai_rule_inventory()$tool, sep = "\t")
  kn  <- AI_SILENT_CHANNELS_KNOWN
  unknown <- kn[!(paste(kn$tier, kn$tool, sep = "\t") %in% inv), , drop = FALSE]
  expect_equal(nrow(unknown), 0L,
               info = paste("allowlisted but no such rule:",
                            paste(unknown$tier, unknown$tool, collapse = ", ")))
})

test_that("every allowlist entry carries a reason, a status and a date", {
  kn <- AI_SILENT_CHANNELS_KNOWN
  expect_true(all(nzchar(trimws(kn$reason))))
  expect_true(all(kn$status %in% c("genuine", "open")))
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", kn$recorded_on)))
  # "open" means unexplained-but-tracked, so it must say what would settle it.
  expect_true(all(nchar(kn$reason[kn$status == "open"]) > 30))
})

test_that("the canary stands down on a roster too small to mean anything", {
  # A fixture merges a handful of repos. Every channel is zero there, and none
  # of those zeros is evidence.
  expect_silent(suppressMessages(ai_canary_check(.ai_empty_signals(), roster_n = 5L)))
})

test_that("the canary gates on the roster, never on the detection count", {
  # A scan that collapsed to zero detections is the case that most needs
  # catching. Gating on rows-found would make it the case that skips.
  expect_error(suppressMessages(ai_canary_check(.ai_empty_signals(), roster_n = 15000L)),
               "detected nothing on the whole roster")
})
