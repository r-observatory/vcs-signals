# The top contributor's commit count and account type, read from the body of the
# weekly contributors call that was read only for its Link header before.

.tc_body <- function(txt) jsonlite::fromJSON(txt, simplifyVector = FALSE)
.tc_none <- list(top_commits = NA_integer_, top_type = NA_character_)

test_that("parse_contributor_top reads the top contributor's commits and type", {
  user <- .tc_body('[{"login":"gaborcsardi","id":660288,"type":"User","site_admin":false,"contributions":1537}]')
  expect_equal(parse_contributor_top(user), list(top_commits = 1537L, top_type = "User"))
  bot <- .tc_body('[{"login":"github-actions[bot]","type":"Bot","contributions":4210}]')
  expect_equal(parse_contributor_top(bot), list(top_commits = 4210L, top_type = "Bot"))
  anon <- .tc_body('[{"email":"someone@example.org","name":"Someone","type":"Anonymous","contributions":88}]')
  expect_equal(parse_contributor_top(anon), list(top_commits = 88L, top_type = "Anonymous"))
})

test_that("an empty body, an error object or a missing count has no top contributor count", {
  expect_equal(parse_contributor_top(.tc_body("[]")), .tc_none)
  expect_equal(parse_contributor_top(.tc_body(paste0('{"message":"The history or contributor list is too ',
                                                     'large to list contributors for this repository via the API."}'))),
               .tc_none)
  expect_equal(parse_contributor_top(NULL), .tc_none)
  expect_equal(parse_contributor_top(.tc_body('[{"login":"x","type":"User"}]')),
               list(top_commits = NA_integer_, top_type = "User"))
})

test_that("the bot flag is 1 for a bot, 0 for a person, and unknown otherwise", {
  expect_identical(contributor_bot_flag("Bot"), 1L)
  expect_identical(contributor_bot_flag("User"), 0L)
  expect_identical(contributor_bot_flag("Anonymous"), NA_integer_)
  expect_identical(contributor_bot_flag("Organization"), NA_integer_)
  expect_identical(contributor_bot_flag(NA_character_), NA_integer_)
})
