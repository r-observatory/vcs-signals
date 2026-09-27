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

test_that("the silent-search list parses into one row per search and tool", {
  # A reason missing a CSV double quote still parses: read.csv spills the rest of
  # the text into rows whose search and tool are fragments of a sentence.
  kn <- AI_SILENT_CHANNELS_KNOWN
  expect_equal(names(kn), c("tier", "tool", "status", "reason", "recorded_on"))
  expect_false(anyNA(kn))
  expect_true(all(kn$tier %in% c("A", "B", "C", "D", "PR", "PB")),
              info = paste(unique(kn$tier), collapse = ", "))
  expect_false(any(grepl("[\r\n]", unlist(kn))))
  # The published table is keyed on the search and the tool, so a repeat fails the merge's write.
  expect_equal(anyDuplicated(paste(kn$tier, kn$tool)), 0L)
})

test_that("silent-search reasons are written in the page's words", {
  # The page prints each reason as written, so these are the words a reader sees.
  kn <- AI_SILENT_CHANNELS_KNOWN
  key <- paste(kn$tier, kn$tool, sep = "/")
  which_rows <- function(bad) paste(key[bad], collapse = ", ")
  internal <- grepl(paste0("\\b(tiers?|evidence|markers?|traces?|channels?|flagged|",
                           "roster|shards?|gate|cheap|deep)\\b"), kn$reason, ignore.case = TRUE)
  expect_false(any(internal), info = which_rows(internal))
  letter <- grepl(paste0("\\btier[- ]?(A|B|C|D|PR|PB)\\b|\\((A|B|C|D|PR|PB)\\)|",
                         "\\b(A|B|C|D|PR|PB) (search|rule)\\b"), kn$reason)
  expect_false(any(letter), info = which_rows(letter))
  dash <- grepl("\u2014", kn$reason, fixed = TRUE)
  expect_false(any(dash), info = which_rows(dash))
  semi <- grepl(";", kn$reason, fixed = TRUE)
  expect_false(any(semi), info = which_rows(semi))
  quote <- grepl("\"", kn$reason, fixed = TRUE)
  expect_false(any(quote), info = which_rows(quote))
  long <- nchar(kn$reason) > 800
  expect_false(any(long), info = which_rows(long))
  unended <- !grepl("\\.$", kn$reason)
  expect_false(any(unended), info = which_rows(unended))
  # The page shows an open question as unresolved, so its last sentence says what settles it.
  unsettled <- kn$status == "open" & !grepl("\\bsettled?\\b", sub("^.*[.] ", "", kn$reason))
  expect_false(any(unsettled), info = which_rows(unsettled))
})

test_that("a vendor fact in a reason has its source written above the list", {
  # A rename, shutdown or successor told to readers needs a page someone can check,
  # and a source left behind after its sentence was dropped cites nothing.
  src <- readLines(file.path(.repo_root, "scripts", "config.R"), warn = FALSE)
  at <- grep("^AI_SILENT_CHANNELS_KNOWN <- read.csv", src)
  expect_length(at, 1L)
  top <- at
  while (top > 1L && startsWith(src[top - 1L], "#")) top <- top - 1L
  above <- src[seq_len(at - top) + top - 1L]
  sources <- c("Kiro since" = "kiro.dev/docs/upgrade-guides/migrating-from-q",
               "Devin Desktop" = "docs.devin.ai/desktop/devin-desktop-faq",
               "Grok Build" = "github.com/xai-org/grok-build",
               "Junie now reads" = "junie.jetbrains.com/docs/guidelines-and-memory",
               "Roo Code extension was shut down" = "github.com/RooCodeInc/Roo-Code")
  said <- vapply(names(sources), function(p)
    any(grepl(p, AI_SILENT_CHANNELS_KNOWN$reason, fixed = TRUE)), logical(1))
  cited <- vapply(sources, function(u) any(grepl(u, above, fixed = TRUE)), logical(1))
  expect_equal(unname(cited), unname(said),
               info = paste(names(sources)[cited != said], collapse = ", "))
})

test_that("pull requests opened by a tool's account are in the rule inventory", {
  inv <- ai_rule_inventory()
  expect_setequal(inv$tool[inv$tier == "PR"], unique(unname(AI_PR_AGENT_LOGINS)))
  # Copilot, Devin and Jules accounts have opened pull requests in scanned
  # repositories, and so have Claude's and Amazon Q's. The other four zeros must reach the check.
  rows <- data.frame(repo_id = c("a", "b", "c", "d", "e"),
                     tool = c("copilot", "devin", "jules", "claude", "amazonq"),
                     evidence_tiers = c("PR", "D,PR", "A,PR", "PR", "PR"), stringsAsFactors = FALSE)
  measured <- ai_silent_channels(rows, known = NULL)
  expect_setequal(measured$tool[measured$tier == "PR"], c("cursor", "openhands", "codex", "kiro"))
  # With the recorded list, ai_silent_channels() returns only the unexplained rows.
  out <- ai_silent_channels(rows)
  expect_false(any(out$tier == "PR"), info = paste(out$tool[out$tier == "PR"], collapse = ", "))
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

test_that("community-health files are detected in both locations", {
  # A tree hit is 1 without the contents read; a miss is NA, since an owner default is invisible to the tree.
  expect_equal(classify_dev_tooling(c("CODE_OF_CONDUCT.md"), character(0))$has_code_of_conduct, 1L)
  expect_equal(classify_dev_tooling(character(0), c("CODE_OF_CONDUCT.md"))$has_code_of_conduct, 1L)
  expect_equal(classify_dev_tooling(c("CODE_OF_CONDUCT"), character(0))$has_code_of_conduct, 1L)
  expect_true(is.na(classify_dev_tooling(c("DESCRIPTION"), character(0))$has_code_of_conduct))

  expect_equal(classify_dev_tooling(c("CONTRIBUTING.md"), character(0))$has_contributing, 1L)
  expect_equal(classify_dev_tooling(character(0), c("CONTRIBUTING.md"))$has_contributing, 1L)
  expect_equal(classify_dev_tooling(c("CONTRIBUTING.Rmd"), character(0))$has_contributing, 1L)
  expect_true(is.na(classify_dev_tooling(c("DESCRIPTION"), character(0))$has_contributing))

  # Neither is a CI system, so neither may move the has_ci rollup.
  expect_equal(classify_dev_tooling(c("CODE_OF_CONDUCT.md", "CONTRIBUTING.md"),
                                    character(0))$has_ci, 0L)
})

test_that("the two community columns join the derived column set", {
  cols <- dev_tooling_columns()
  expect_true("has_code_of_conduct" %in% cols)
  expect_true("has_contributing" %in% cols)
  expect_true(grepl("has_code_of_conduct INTEGER", dev_tooling_create_sql(), fixed = TRUE))
  expect_true(grepl("has_contributing INTEGER", dev_tooling_create_sql(), fixed = TRUE))
})

test_that("community-health files are caught in the spellings maintainers use", {
  # rlistings keeps .github/CODE_OF_CONDUCT.Rmd and partialling.out keeps .github/contributing.md.
  expect_equal(classify_dev_tooling(character(0), c("CODE_OF_CONDUCT.Rmd"))$has_code_of_conduct, 1L)
  expect_equal(classify_dev_tooling(character(0), c("contributing.md"))$has_contributing, 1L)
  expect_equal(classify_dev_tooling(c("code_of_conduct.md"), character(0))$has_code_of_conduct, 1L)
  expect_equal(classify_dev_tooling(c("Contributing.md"), character(0))$has_contributing, 1L)
  expect_equal(classify_dev_tooling(c("CONDUCT.md"), character(0))$has_code_of_conduct, 1L)
  expect_equal(classify_dev_tooling(c("CONTRIBUTING.MD"), character(0))$has_contributing, 1L)
  expect_true(is.na(classify_dev_tooling(c("CONDUCT_OF_CODE.md"), character(0))$has_code_of_conduct))
})

test_that("a contents read that found no community files reads 0, not unknown", {
  repo <- list(name_with_owner = "o/pkg", coc_url = NA_character_, contributing_url = NA_character_,
               pr_templates = data.frame(filename = character(0), repository = character(0)))
  r <- classify_dev_tooling(c("DESCRIPTION"), character(0), repo = repo)
  expect_equal(r$has_code_of_conduct, 0L); expect_equal(r$coc_source, "none")
  expect_equal(r$has_contributing, 0L);    expect_equal(r$contributing_source, "none")
  expect_equal(r$has_pr_template, 0L);     expect_equal(r$pr_template_source, "none")
})

test_that("the inventory lists pull requests a tool wrote and no review tool", {
  inv <- ai_rule_inventory()
  expect_true(all(c("PR", "PB") %in% inv$tier))
  expect_setequal(inv$tool[inv$tier == "PB"], c("cursor", "claude", "codex", "devin", "openhands", "amazonq"))
  expect_false(any(inv$tool %in% c("coderabbit", "gemini-code-assist", "copilot-review", "any")))
})

test_that("every search at zero on launch has a recorded row", {
  # The searches that matched something in the scanned repositories on 2026-09-25.
  detecting <- list(
    A  = c("claude", "copilot", "cursor", "jules", "openhands", "amazonq"),
    B  = c("claude", "codex", "cursor", "devin", "openhands", "jules", "gemini", "aider", "antigravity",
           "copilot", "corteza", "eca", "warp", "qwen", "crush"),
    C  = "aider",
    PR = c("copilot", "devin", "jules", "claude", "amazonq"),
    PB = c("cursor", "claude", "codex", "devin", "openhands"),
    D  = c("claude", "codex", "cursor", "copilot", "aider", "gemini", "windsurf", "cline", "continue",
           "agents-md", "agents-dir", "antigravity", "kiro", "devin", "posit-assistant", "opencode",
           "qwen", "kilo", "warp", "jules", "openhands"))
  rows <- do.call(rbind, lapply(names(detecting), function(t) data.frame(
    repo_id = paste0("r-", t, "-", detecting[[t]]), tool = detecting[[t]], evidence_tiers = t,
    stringsAsFactors = FALSE)))
  unexplained <- ai_silent_channels(rows)
  expect_equal(nrow(unexplained), 0L, info = paste(unexplained$tier, unexplained$tool, sep = "/", collapse = ", "))
})

test_that("commits by Cursor's and OpenHands's accounts are no longer recorded as silent", {
  kn <- AI_SILENT_CHANNELS_KNOWN
  expect_false(any(kn$tier == "A" & kn$tool %in% c("cursor", "openhands")))
  expect_equal(kn$status[kn$tier == "PR" & kn$tool == "cursor"], "genuine")
  # The rows whose reasons stop being true once every repository is read each week.
  says <- function(tier, tool, text) grepl(text, kn$reason[kn$tier == tier & kn$tool == tool], fixed = TRUE)
  expect_true(says("A", "devin", "counted in every repository each week"))
  expect_true(says("B", "replit", "every repository's new commits are now read each week"))
  expect_true(says("B", "windsurf", "which this page lists under Devin"))
  expect_true(says("D", "amazonq", "which this page lists as its own tool"))
  expect_true(says("PR", "cursor", "the note Cursor writes at the top of the description"))
})

test_that("an outside contributor's pull request shows the search works", {
  out <- data.frame(repo_id = "g", pr_number = 1:2, tool = c("kiro", "cursor"),
                    found_via = c("pr-author", "pr.cursor.agent-branch"), created_at = "2026-08-28T00:00:00Z",
                    from_fork = 1L, author_association = "NONE", last_confirmed_date = "2026-10-04",
                    stringsAsFactors = FALSE)
  s <- ai_silent_channels(.ai_empty_signals(), known = NULL, outside = out)
  expect_false(any(s$tier == "PR" & s$tool == "kiro"))
  expect_false(any(s$tier == "PB" & s$tool == "cursor"))
  expect_true(any(s$tier == "PB" & s$tool == "claude"))
})

test_that("a codex branch alone leaves the Codex pull request search unseen", {
  prs <- .ai_pr_nodes_frame(list(list(number = 275L, createdAt = "2026-03-11T08:43:55Z",
    author = list(login = "fabian-s", `__typename` = "User"), authorAssociation = "COLLABORATOR",
    isCrossRepository = FALSE, headRefName = "codex/fix-rcmdcheck-after-merge", body = "## Summary")))
  found <- assemble_repo_evidence(list(root_entries = "DESCRIPTION"), list(prs = prs))
  expect_true(repo_has_ai_signal(found))   # the week's searches may still ask about the repository
  found$repo_id <- "github.com/adibender/pammtools"
  rows <- build_cheap_rows(found, "2026-10-04")
  expect_equal(nrow(rows), 0L)
  expect_true(any(ai_silent_channels(rows, known = NULL)$tier == "PB" &
                  ai_silent_channels(rows, known = NULL)$tool == "codex"))
})

test_that("Claude's pull requests beside a codex branch name Claude alone", {
  node <- function(n, head, at) list(number = n, createdAt = at, author = list(login = "temuulene",
    `__typename` = "User"), authorAssociation = "OWNER", isCrossRepository = FALSE, headRefName = head, body = "x")
  prs <- .ai_pr_nodes_frame(list(node(2L, "claude/review-package-vignettes-8clehs", "2026-07-15T03:42:49Z"),
                                 node(3L, "codex/add-tests", "2026-07-20T00:00:00Z")))
  found <- assemble_repo_evidence(list(root_entries = "DESCRIPTION"), list(prs = prs))
  found$repo_id <- "github.com/temuulene/mongolstats"
  rows <- build_cheap_rows(found, "2026-10-04")
  expect_equal(rows$tool, "claude")
  roll <- build_ai_rollups(rows)
  expect_equal(roll$ai_tools, "claude"); expect_equal(roll$ai_tool_count, 1L)
})

test_that("each way a tool can be found names it or not, as the naming table says", {
  row <- function(tiers, markers, censored = 0L, credited = NA_integer_)
    data.frame(tool = "claude", first_seen_date = "2026-01-01", first_seen_censored = censored,
               evidence_tiers = tiers, markers = markers, authored = 0L, agnostic = FALSE,
               assisted_commits = credited, stringsAsFactors = FALSE)
  expect_true(meets_naming_threshold(row("D", "CLAUDE.md")))                 # a file in the repository
  expect_false(meets_naming_threshold(row("D", "gitignore:.claude")))         # a line in an ignore file
  expect_true(meets_naming_threshold(row("PR", "PR")))                        # opened by the tool's account
  expect_true(meets_naming_threshold(row("PB", "pr.claude.branch")))          # the tool wrote it, a maintainer opened it
  expect_false(meets_naming_threshold(row("PB", "pr.codex.branch")))          # a codex/ branch, never stored
  expect_true(meets_naming_threshold(row("A", "A")))                          # commits by the tool's account
  expect_true(meets_naming_threshold(row("B", "msg.claude.session", 1L)))     # commits crediting it, read weekly
  expect_true(meets_naming_threshold(row("C", "name.aider.suffix", 1L)))
  expect_false(meets_naming_threshold(row("B", "B", 1L)))                     # an unchecked search hit, alone
  expect_true(meets_naming_threshold(row("B,D", "B,CLAUDE.md", 1L)))          # the same, beside a file
  expect_equal(unname(TIER_PRIORITY[c("PR", "PB", "D")]), c(4L, 5L, 6L))
})
