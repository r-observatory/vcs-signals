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
  got <- suppressMessages(ai_canary_check(.ai_empty_signals(), roster_n = 15000L))
  expect_true(nrow(got) > 10)
})

test_that("the canary reports rather than throws, so the caller controls the ordering", {
  # It used to stop() where it stood, which was before publish(), so one quiet
  # tool channel also withheld that run's dev-tooling and summary rows. Those
  # are not implicated by a tool going quiet, and a week of collateral
  # staleness is worse than a red build standing beside fresh data.
  expect_no_error(suppressMessages(ai_canary_check(.ai_empty_signals(), roster_n = 15000L)))
})

test_that("the published table separates a measured zero from an unasked question", {
  # A page rendering "0 repos" for a channel nobody could reach is asserting
  # something we do not know.
  tbl <- ai_silent_channel_table(.ai_empty_signals())
  expect_true(all(c("tier", "tool", "status", "reason", "recorded_on") %in% names(tbl)))
  expect_true(any(tbl$status == "unexplained"))
  expect_true(any(tbl$status == "genuine"))
  # An unexplained row carries no reason, and must not borrow one.
  expect_true(all(is.na(tbl$reason[tbl$status == "unexplained"])))
})

test_that("a channel that started detecting leaves the silent table entirely", {
  # The allowlist is a claim about a zero. Once the zero is gone the claim is
  # spent, and leaving the row published would report a live channel as silent.
  rows <- data.frame(repo_id = "a", tool = "amazonq", evidence_tiers = "D",
                     stringsAsFactors = FALSE)
  tbl <- ai_silent_channel_table(rows)
  expect_false(any(tbl$tier == "D" & tbl$tool == "amazonq"))
})

test_that("the inventory states each tier's breadth, which the tiers do not share", {
  inv <- ai_rule_inventory()
  per_tier <- table(inv$tier)
  # The asymmetry is the point: presenting C beside D as comparable channels
  # overstates C, and this is the number that says so.
  expect_equal(as.integer(per_tier[["C"]]), 1L)
  expect_true(as.integer(per_tier[["D"]]) > 10L)
  expect_true(all(c("A", "B", "C", "D") %in% names(per_tier)))
  expect_equal(anyDuplicated(paste(inv$tier, inv$tool)), 0L)
})
