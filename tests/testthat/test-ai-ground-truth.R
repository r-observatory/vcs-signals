# Detection rules against strings the world actually emits.
#
# Every other test in this suite builds its fixtures from the ruleset, which
# asserts that our list matches our list and cannot fail whatever the list
# says. Five channels shipped a confident zero across the whole roster while
# those tests stayed green. This file is the counterweight: it reads
# tests/fixtures/observed-trailers.tsv, every line of which was seen in a real
# commit, and asks whether the rules match reality rather than themselves.

corpus <- function() {
  path <- testthat::test_path("..", "fixtures", "observed-trailers.tsv")
  if (!file.exists(path)) path <- file.path("tests", "fixtures", "observed-trailers.tsv")
  df <- utils::read.delim(path, comment.char = "#", stringsAsFactors = FALSE,
                          quote = "", colClasses = "character")
  df$n_seen <- as.integer(df$n_seen)
  df
}

test_that("the corpus is present, dated, and carries its negatives", {
  cp <- corpus()
  expect_true(nrow(cp) > 20)
  expect_true(all(nzchar(cp$string)))
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", cp$observed_at)))
  expect_true(all(cp$n_seen >= 1))
  # Without negatives a name-matching rule passes: Devin's own search returns
  # Devin Logan, and a corpus of positives alone would never notice.
  expect_true(sum(grepl("^NOT-", cp$tool)) >= 3)
})

test_that("every observed trailer is attributed to the tool that wrote it", {
  cp <- corpus()
  pos <- cp[!grepl("^NOT-", cp$tool), ]
  for (i in seq_len(nrow(pos))) {
    hit <- scan_trailers(pos$string[i])
    expect_true(nrow(hit) > 0,
                info = sprintf("no rule matches (seen %d times on %s): %s",
                               pos$n_seen[i], pos$observed_at[i], pos$string[i]))
    if (nrow(hit) > 0) {
      expect_true(pos$tool[i] %in% hit$tool,
                  info = sprintf("attributed to %s, expected %s: %s",
                                 paste(unique(hit$tool), collapse = ","),
                                 pos$tool[i], pos$string[i]))
    }
  }
})

test_that("an observed lookalike is never attributed to a tool", {
  cp <- corpus()
  neg <- cp[grepl("^NOT-", cp$tool), ]
  for (i in seq_len(nrow(neg))) {
    hit <- scan_trailers(neg$string[i])
    expect_equal(nrow(hit), 0L,
                 info = sprintf("false positive on a real %s: %s",
                                sub("^NOT-", "", neg$tool[i]), neg$string[i]))
  }
})

test_that("every tool with a trailer rule has a string in the corpus", {
  # A rule nobody has ever seen fire is a rule written from documentation. It
  # may be right, but nothing here can tell, and that is the state this file
  # exists to make visible.
  cp <- corpus()
  ruled <- sort(unique(vapply(AI_TRAILER_PATTERNS, function(p) p$tool, character(1))))
  seen  <- sort(unique(cp$tool[!grepl("^NOT-", cp$tool)]))
  missing <- setdiff(ruled, seen)
  expect_equal(missing, character(0),
               info = paste("rules with no observed sample:", paste(missing, collapse = ", ")))
})

test_that("constructed lookalikes are rejected too", {
  # These were not observed, so they stay out of the corpus, but they are the
  # shapes a careless widening would start matching.
  constructed <- c(
    "Co-authored-by: Claudia Smith <noreply@anthropic.com>",   # name is not Claude
    "Co-authored-by: Claude <someone@example.com>",            # right name, wrong address
    "Co-authored-by: Codex <jane@example.com>",
    "Co-authored-by: Jules Verne <jules@example.org>",
    "we rewrote the cursor handling in the parser",            # prose, no trailer
    "Co-Authored-By: Claude\nSigned-off-by: x <noreply@anthropic.com>"  # split across lines
  )
  for (s in constructed) {
    expect_equal(nrow(scan_trailers(s)), 0L, info = s)
  }
})
