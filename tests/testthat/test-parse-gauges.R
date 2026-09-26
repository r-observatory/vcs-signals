test_that("parse_gauges extracts fields and drops null nodes", {
  j <- jsonlite::fromJSON(readLines("fixtures/gauges.json", warn = FALSE),
                          simplifyVector = FALSE)
  df <- parse_gauges(j$data$nodes)
  expect_equal(nrow(df), 2)                       # the null node dropped
  a <- df[df$node_id == "R_a", ]
  expect_equal(a$stars, 6959); expect_equal(a$prs_merged, 1885)
  expect_equal(a$license, "MIT"); expect_equal(a$topics, "r,ggplot2")
  expect_equal(a$size_kb, 41000); expect_equal(a$last_release_at, "2026-01-10T00:00:00Z")
  b <- df[df$node_id == "R_b", ]
  expect_true(is.na(b$license)); expect_equal(b$topics, "")
  expect_true(is.na(b$size_kb)); expect_true(is.na(b$pushed_at)); expect_true(is.na(b$last_release_at))
  expect_equal(b$is_fork, 1L)
})

test_that("parse_gauges reads each repository's current owner", {
  j <- jsonlite::fromJSON(readLines("fixtures/gauges.json", warn = FALSE),
                          simplifyVector = FALSE)
  df <- parse_gauges(j$data$nodes)
  a <- df[df$node_id == "R_a", ]
  expect_equal(a$owner_login, "tidyverse")
  expect_equal(a$owner_type, "Organization")
  expect_equal(a$owner_node_id, "O_1")
  b <- df[df$node_id == "R_b", ]
  expect_equal(b$owner_login, "someone")
  expect_equal(b$owner_type, "User")
  expect_equal(b$owner_node_id, "U_1")
})

test_that("a repository whose owner comes back null keeps its gauge row with no owner", {
  j <- jsonlite::fromJSON(readLines("fixtures/gauges_null_owner.json", warn = FALSE),
                          simplifyVector = FALSE)
  df <- parse_gauges(j$data$nodes)
  expect_equal(nrow(df), 1L)
  expect_equal(df$stars, 3L)
  expect_true(is.na(df$owner_login))
  expect_true(is.na(df$owner_type))
  expect_true(is.na(df$owner_node_id))
  expect_type(df$owner_login, "character")
})

test_that("the empty gauge frame has the parsed frame's columns", {
  j <- jsonlite::fromJSON(readLines("fixtures/gauges.json", warn = FALSE),
                          simplifyVector = FALSE)
  empty <- rows_df_empty_gauges()
  expect_equal(names(empty), names(parse_gauges(j$data$nodes)))
  for (col in c("owner_login", "owner_type", "owner_node_id"))
    expect_type(empty[[col]], "character")
  expect_equal(names(parse_gauges(list(NULL))), names(empty))
})
