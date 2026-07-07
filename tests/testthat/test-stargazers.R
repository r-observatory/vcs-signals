test_that("build_stargazers_query embeds owner/name/page size/STARRED_AT/ASC and after: null without a cursor", {
  q <- build_stargazers_query("tidyverse", "ggplot2")
  expect_match(q, 'repository\\(owner: "tidyverse", name: "ggplot2"\\)')
  expect_match(q, sprintf("first: %d", STARGAZER_PAGE))
  expect_match(q, "STARRED_AT")
  expect_match(q, "ASC")
  expect_match(q, "after: null")
})

test_that("build_stargazers_query embeds the cursor as a quoted after: when given one", {
  q <- build_stargazers_query("tidyverse", "ggplot2", after = "Y3Vyc29yOnYyOpHOAA==")
  expect_match(q, 'after: "Y3Vyc29yOnYyOpHOAA=="', fixed = TRUE)
})

test_that("parse_stargazers extracts timestamps, endCursor, and hasNextPage", {
  resp <- list(data = list(repository = list(stargazers = list(
    pageInfo = list(endCursor = "CURSOR1", hasNextPage = TRUE),
    edges = list(list(starredAt = "2020-01-01T00:00:00Z"), list(starredAt = "2020-01-02T00:00:00Z"))))))
  out <- parse_stargazers(resp)
  expect_equal(out$starred_at, c("2020-01-01T00:00:00Z", "2020-01-02T00:00:00Z"))
  expect_equal(out$end_cursor, "CURSOR1")
  expect_true(out$has_next)
})

test_that("parse_stargazers degrades to empty, has_next FALSE on a null stargazers node", {
  resp <- list(data = list(repository = list(stargazers = NULL)))
  out <- parse_stargazers(resp)
  expect_equal(out$starred_at, character(0))
  expect_true(is.na(out$end_cursor))
  expect_false(out$has_next)
})

test_that("paginate_stargazers loops after cursors until has_next is FALSE", {
  pages <- list(
    list(data = list(repository = list(stargazers = list(
      pageInfo = list(endCursor = "C1", hasNextPage = TRUE),
      edges = list(list(starredAt = "2020-01-01T00:00:00Z")))))),
    list(data = list(repository = list(stargazers = list(
      pageInfo = list(endCursor = "C2", hasNextPage = FALSE),
      edges = list(list(starredAt = "2020-01-02T00:00:00Z")))))))
  calls <- new.env(); calls$n <- 0L; calls$cursors <- character(0)
  io <- list(graphql = function(query) {
    calls$n <- calls$n + 1L
    calls$cursors <- c(calls$cursors,
      if (grepl("after: null", query, fixed = TRUE)) "null" else "cursor")
    pages[[calls$n]]
  })
  out <- paginate_stargazers(io, "o", "n", delay = 0)
  expect_equal(out, c("2020-01-01T00:00:00Z", "2020-01-02T00:00:00Z"))
  expect_equal(calls$n, 2L)
  expect_equal(calls$cursors, c("null", "cursor"))   # first page has no cursor, second carries C1
})

test_that("paginate_stargazers returns character(0) for a repo with no stargazers", {
  io <- list(graphql = function(query) list(data = list(repository = list(stargazers = list(
    pageInfo = list(endCursor = NULL, hasNextPage = FALSE), edges = list())))))
  expect_equal(paginate_stargazers(io, "o", "n", delay = 0), character(0))
})

test_that("paginate_stargazers errors when a mid-pagination page carries GraphQL errors (no silent truncation)", {
  pages <- list(
    list(data = list(repository = list(stargazers = list(
      pageInfo = list(endCursor = "C1", hasNextPage = TRUE),
      edges = list(list(starredAt = "2019-01-01T00:00:00Z")))))),
    # HTTP-200-with-errors shape: data present but stargazers null AND errors set
    list(data = list(repository = list(stargazers = NULL)),
         errors = list(list(message = "SECONDARY_RATE_LIMIT"))))
  calls <- new.env(); calls$n <- 0L
  io <- list(graphql = function(query) { calls$n <- calls$n + 1L; pages[[calls$n]] })
  expect_error(paginate_stargazers(io, "o", "n", delay = 0), "stargazers page error")
})

test_that("paginate_stargazers errors on a null-data response instead of degrading to empty", {
  io <- list(graphql = function(query) list(data = NULL, errors = list(list(message = "BAD_CREDENTIALS"))))
  expect_error(paginate_stargazers(io, "o", "n", delay = 0), "stargazers page error")
})

test_that("paginate_stargazers stops (does not loop forever) when has_next is TRUE but the cursor is null", {
  io <- list(graphql = function(query) list(data = list(repository = list(stargazers = list(
    pageInfo = list(endCursor = NULL, hasNextPage = TRUE),
    edges = list(list(starredAt = "2019-01-01T00:00:00Z")))))))
  expect_error(paginate_stargazers(io, "o", "n", delay = 0), "no cursor")
})

test_that("a repository-null response with NO errors still degrades to empty (repo gone / private)", {
  io <- list(graphql = function(query) list(data = list(repository = NULL)))
  expect_equal(paginate_stargazers(io, "o", "n", delay = 0), character(0))
})
