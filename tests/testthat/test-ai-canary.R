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

test_that("the canary counts the channels that are silent, not the rows in the file", {
  # It printed "16 recorded" every week by taking nrow() of a hand-written CSV
  # and calling it a measurement. Six of those sixteen claims had already been
  # answered by the data in front of it, five of them for weeks, and the line
  # read as a stable healthy invariant the whole time. The one table built to
  # stop a zero rotting into an assumed fact was reporting its own contents.
  kn <- data.frame(
    tier = c("D", "D"), tool = c("amazonq", "grok"), status = c("genuine", "genuine"),
    reason = c("scanned everywhere, absent from the roster",
               "scanned everywhere, absent from the roster"),
    recorded_on = c("2026-08-01", "2026-08-01"), stringsAsFactors = FALSE)
  # amazonq is detecting now; grok is not.
  rows <- data.frame(repo_id = "a", tool = "amazonq", evidence_tiers = "D",
                     stringsAsFactors = FALSE)
  msg <- paste(capture_messages(ai_canary_check(rows, kn, roster_n = 15000L)), collapse = "")
  measured <- nrow(ai_silent_channels(rows, NULL))
  expect_match(msg, sprintf("%d silent", measured), fixed = TRUE)
  expect_false(grepl("2 recorded", msg, fixed = TRUE))
  expect_match(msg, "1 recorded", fixed = TRUE)
})

test_that("a recorded zero the data has answered is named, so it can be retired", {
  # Nothing told anyone. The claim just sat in the file being re-printed as an
  # open question, which is the rot the file exists to prevent, happening inside
  # the file.
  kn <- data.frame(
    tier = "D", tool = "amazonq", status = "genuine",
    reason = "scanned everywhere, absent from the roster",
    recorded_on = "2026-08-01", stringsAsFactors = FALSE)
  rows <- data.frame(repo_id = "a", tool = "amazonq", evidence_tiers = "D",
                     stringsAsFactors = FALSE)
  msg <- paste(capture_messages(ai_canary_check(rows, kn, roster_n = 15000L)), collapse = "")
  expect_match(msg, "D/amazonq")
  expect_match(msg, "AI_SILENT_CHANNELS_KNOWN")
})

test_that("an answered question is not re-printed as an open one", {
  # The merge that recorded devin's first detection also printed "rule added
  # 2026-08-01, unscanned" about devin, in the same output, because the open
  # questions were read off the file rather than off the data.
  kn <- data.frame(
    tier = "B", tool = "devin", status = "open",
    reason = "rule added 2026-08-01, unscanned; and it can only fire on a repo some OTHER tool already flagged",
    recorded_on = "2026-08-01", stringsAsFactors = FALSE)
  rows <- data.frame(repo_id = "github.com/o/r", tool = "devin", evidence_tiers = "B,PR",
                     stringsAsFactors = FALSE)
  msg <- paste(capture_messages(ai_canary_check(rows, kn, roster_n = 15000L)), collapse = "")
  expect_false(grepl("open questions", msg, fixed = TRUE))
  # It is still named, once, under the heading that says what to do about it.
  expect_match(msg, "retire them from AI_SILENT_CHANNELS_KNOWN", fixed = TRUE)
  expect_match(msg, "B/devin")
})

test_that("a recorded zero whose rule was retired is not reported as detecting", {
  # "Detecting" is measured as absence from the silent set, and the silent set
  # is built from ai_rule_inventory(), so a channel whose rule is no longer in
  # the inventory is absent from it for the opposite reason: nothing scans for
  # it at all. .positai and .idx were removed from the inventory on purpose, so
  # a recorded claim outliving its rule is a shape this project has already
  # produced once. The advice is the same either way, retire the entry, but the
  # sentence under it is the one output this project treats as evidence about a
  # zero, and it would have said the tool was working.
  kn <- data.frame(
    tier = "D", tool = "positron", status = "genuine",
    reason = "the marker is ambient, written whether or not AI was used",
    recorded_on = "2026-07-01", stringsAsFactors = FALSE)
  rows <- data.frame(repo_id = "a", tool = "claude", evidence_tiers = "D",
                     stringsAsFactors = FALSE)
  msg <- paste(capture_messages(ai_canary_check(rows, kn, roster_n = 15000L)), collapse = "")
  expect_false(grepl("detecting; the claim recorded", msg, fixed = TRUE))
  expect_match(msg, "D/positron")
  expect_match(msg, "no rule")
})

test_that("a question that is still open is still re-printed", {
  kn <- data.frame(
    tier = "B", tool = "devin", status = "open",
    reason = "rule added 2026-08-01, unscanned; and it can only fire on a repo some OTHER tool already flagged",
    recorded_on = "2026-08-01", stringsAsFactors = FALSE)
  rows <- data.frame(repo_id = "github.com/o/r", tool = "claude", evidence_tiers = "B",
                     stringsAsFactors = FALSE)
  msg <- paste(capture_messages(ai_canary_check(rows, kn, roster_n = 15000L)), collapse = "")
  expect_match(msg, "unscanned", fixed = TRUE)
  expect_match(msg, "since 2026-08-01", fixed = TRUE)
})

test_that("a pkgdown site is recognised, in each shape maintainers actually use", {
  # The most common documentation site in the R ecosystem was not detectable at
  # all, while _quarto.yml beside it was. coatless-rpkg/livelink ships both a
  # root _pkgdown.yml and a pkgdown/ directory and registered neither.
  expect_equal(classify_dev_tooling(c("_pkgdown.yml"), character(0))$has_pkgdown, 1L)
  expect_equal(classify_dev_tooling(c("_pkgdown.yaml"), character(0))$has_pkgdown, 1L)
  expect_equal(classify_dev_tooling(c("pkgdown"), character(0))$has_pkgdown, 1L)
  expect_equal(classify_dev_tooling(c("DESCRIPTION"), character(0))$has_pkgdown, 0L)
})

test_that("altdoc is recognised from its config directory", {
  # Verified by code search: altdoc keeps its config in altdoc/ at the root,
  # as altdoc/mkdocs.yml, altdoc/pkgdown.yml, altdoc/quarto_website.yml.
  expect_equal(classify_dev_tooling(c("altdoc", "DESCRIPTION"), character(0))$has_altdoc, 1L)
  expect_equal(classify_dev_tooling(c("DESCRIPTION"), character(0))$has_altdoc, 0L)
})

test_that("altdoc wrapping pkgdown is not counted as a pkgdown site", {
  # altdoc/pkgdown.yml is altdoc driving pkgdown as a backend. The repository is
  # not running pkgdown itself, and conflating them would overstate pkgdown.
  r <- classify_dev_tooling(c("altdoc"), character(0))
  expect_equal(r$has_altdoc, 1L)
  expect_equal(r$has_pkgdown, 0L)
})

test_that("litedown is found where its config actually lives, not only at the root", {
  # _litedown.yml is not a root file. Every observed instance sits under site/ or
  # docs/ (Rdatatable/data.table uses site/_litedown.yml), so a root-only rule
  # would report litedown as unused everywhere.
  expect_equal(classify_dev_tooling(c("site/_litedown.yml"), character(0))$has_litedown, 1L)
  expect_equal(classify_dev_tooling(c("_litedown.yml"), character(0))$has_litedown, 1L)
  expect_equal(classify_dev_tooling(c("site"), character(0))$has_litedown, 0L)
})

test_that("livelink's real tree yields quarto vignettes and a pkgdown site", {
  root <- c(".aspell", ".github", "R", "_pkgdown.yml", "data-raw", "inst", "man",
            "pkgdown", "tests", "vignettes", "DESCRIPTION", "NAMESPACE",
            "cran-comments.md", "livelink.Rproj",
            "vignettes/decoding-links.qmd", "vignettes/getting-started.qmd",
            "vignettes/links-in-documents.qmd", "vignettes/teaching.qmd",
            "vignettes/webr-and-shinylive.qmd")
  r <- classify_dev_tooling(root, c("workflows"))
  expect_equal(r$has_pkgdown, 1L)
})

test_that("the inventory lists only channels the classifier can actually reach", {
  # Ambient markers are excluded from classification on purpose: .positai is
  # written by Positron whether or not AI was used. Listing them as tier-D rules
  # made the canary report them silent, and they were then recorded as measured
  # zeros carrying a reason that was false.
  inv <- ai_rule_inventory()
  reachable <- unique(vapply(ai_deliberate_markers(), function(m) m$tool, character(1)))
  expect_equal(setdiff(inv$tool[inv$tier == "D"], reachable), character(0))
  expect_false(any(inv$tier == "D" & inv$tool %in% c("positron", "idx")))
})

test_that("no allowlist entry explains a channel that has no rule", {
  # Covered by the integrity test above, but stated on its own because the two
  # false entries were written by hand and read as authoritative.
  kn <- AI_SILENT_CHANNELS_KNOWN
  expect_false(any(kn$tier == "D" & kn$tool %in% c("positron", "idx")))
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
