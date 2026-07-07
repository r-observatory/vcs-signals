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
