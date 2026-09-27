test_that("a rule's pattern or query never changes without a new revision", {
  snap <- utils::read.delim(test_path("fixtures", "ai-rule-revs.tsv"), quote = "", na.strings = "NA",
                            colClasses = "character", comment.char = "")
  now <- ai_rule_rev_table()
  expect_equal(anyDuplicated(now$key), 0L)
  expect_setequal(now$key, snap$key)
  m <- match(now$key, snap$key)
  same_rev <- !is.na(m) & now$rev == as.integer(snap$rev[m])
  drift <- same_rev & (!mapply(identical, now$pattern, snap$pattern[m]) |
                       !mapply(identical, now$query, snap$query[m]))
  expect_false(any(drift), info = paste(now$key[drift], collapse = ", "))
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", now$since_ruleset)))
})

test_that("rules new in this ruleset carry its date and older ones the date the log began", {
  now <- ai_rule_rev_table()
  new <- c("msg.codex.coauthor", "msg.gemini.bot", "msg.claude.address", "msg.claude.session",
           "msg.cursor.made-with", "msg.copilot.cli", "msg.copilot.vscode", "msg.copilot.cloud-coauthor",
           "msg.antigravity.coauthor", "msg.devin.generated", "msg.devin.cognition",
           "msg.windsurf.cascade", "msg.any.assisted-by")
  expect_true(all(now$since_ruleset[now$key %in% new] == AI_RULESET_UNGATED))
  always <- vapply(c(AI_TRAILER_PATTERNS, AI_AUTHOR_SUFFIXES), function(r) identical(r$search, "always"), logical(1))
  keys <- vapply(c(AI_TRAILER_PATTERNS, AI_AUTHOR_SUFFIXES), `[[`, "", "key")
  expect_equal(sum(always & keys %in% new), 13L)
  expect_equal(sum(always & !(keys %in% new)), 14L)
  expect_true(all(now$since_ruleset[now$key %in% keys[always & !(keys %in% new)]] == "2026-09-19"))
})

test_that("the ruleset that began reading every repository carries its page note", {
  expect_equal(AI_RULESET_VERSION, AI_RULESET_UNGATED)
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", names(AI_RULESET_CHANGE_KEYS))))
  expect_true(all(names(AI_RULESET_CHANGE_KEYS) <= AI_RULESET_VERSION))
  expect_equal(anyDuplicated(unname(AI_RULESET_CHANGE_KEYS)), 0L)
  expect_true("ungated-weekly-read" %in% AI_RULESET_CHANGE_KEYS)
})
