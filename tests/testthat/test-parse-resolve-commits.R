test_that("parse_resolve maps aliases and flags 404 as NA node_id", {
  j <- jsonlite::fromJSON(readLines("fixtures/resolve.json", warn = FALSE), simplifyVector = FALSE)
  df <- parse_resolve(j$data, 3)
  expect_equal(nrow(df), 3)
  expect_equal(df$node_id[df$idx == 0], "R_a")
  expect_true(is.na(df$node_id[df$idx == 1]))                 # 404
  expect_equal(df$name_with_owner[df$idx == 2], "new-owner/renamed")
})

test_that("parse_commits reads totalCount + latest date, tolerates empty default branch", {
  j <- jsonlite::fromJSON(readLines("fixtures/commits.json", warn = FALSE), simplifyVector = FALSE)
  df <- parse_commits(j$data$nodes)
  expect_equal(nrow(df), 2)                                   # null node dropped
  a <- df[df$node_id == "R_a", ]
  expect_equal(a$commits_total, 6200); expect_equal(a$last_commit_date, "2026-07-01T10:00:00Z")
  b <- df[df$node_id == "R_b", ]
  expect_true(is.na(b$commits_total)); expect_true(is.na(b$last_commit_date))
})
