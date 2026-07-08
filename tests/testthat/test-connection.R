test_that("build_connection_query embeds owner/name/page size/order/ASC and after: null without a cursor (stars, edges)", {
  q <- build_connection_query("tidyverse", "ggplot2", "stars")
  expect_match(q, 'repository\\(owner: "tidyverse", name: "ggplot2"\\)')
  expect_match(q, "stargazers\\(")
  expect_match(q, sprintf("first: %d", STARGAZER_PAGE))
  expect_match(q, "STARRED_AT")
  expect_match(q, "ASC")
  expect_match(q, "after: null")
  expect_match(q, "edges \\{ starredAt \\}")
})

test_that("build_connection_query embeds the forks connection/order/selection (nodes)", {
  q <- build_connection_query("tidyverse", "ggplot2", "forks")
  expect_match(q, "forks\\(")
  expect_match(q, "CREATED_AT")
  expect_match(q, "ASC")
  expect_match(q, "nodes \\{ createdAt \\}")
})

test_that("build_connection_query embeds the releases connection/order/selection (nodes)", {
  q <- build_connection_query("tidyverse", "ggplot2", "releases_total")
  expect_match(q, "releases\\(")
  expect_match(q, "CREATED_AT")
  expect_match(q, "nodes \\{ createdAt \\}")
})

test_that("build_connection_query embeds the cursor as a quoted after: when given one", {
  q <- build_connection_query("tidyverse", "ggplot2", "stars", after = "Y3Vyc29yOnYyOpHOAA==")
  expect_match(q, 'after: "Y3Vyc29yOnYyOpHOAA=="', fixed = TRUE)
})

test_that("build_connection_query errors on an unknown metric", {
  expect_error(build_connection_query("o", "n", "watchers"), "unknown metric")
})

test_that("build_stargazers_query wrapper matches build_connection_query(..., \"stars\")", {
  expect_identical(build_stargazers_query("tidyverse", "ggplot2"),
                   build_connection_query("tidyverse", "ggplot2", "stars"))
  expect_identical(build_stargazers_query("tidyverse", "ggplot2", after = "C1"),
                   build_connection_query("tidyverse", "ggplot2", "stars", after = "C1"))
})

test_that("parse_connection extracts timestamps, endCursor, and hasNextPage from an edges-shaped (stars) response", {
  resp <- list(data = list(repository = list(stargazers = list(
    pageInfo = list(endCursor = "CURSOR1", hasNextPage = TRUE),
    edges = list(list(starredAt = "2020-01-01T00:00:00Z"), list(starredAt = "2020-01-02T00:00:00Z"))))))
  out <- parse_connection(resp, "stars")
  expect_equal(out$timestamps, c("2020-01-01T00:00:00Z", "2020-01-02T00:00:00Z"))
  expect_equal(out$end_cursor, "CURSOR1")
  expect_true(out$has_next)
})

test_that("parse_connection extracts timestamps, endCursor, and hasNextPage from a nodes-shaped (forks) response", {
  resp <- list(data = list(repository = list(forks = list(
    pageInfo = list(endCursor = "CURSOR2", hasNextPage = FALSE),
    nodes = list(list(createdAt = "2021-06-01T00:00:00Z"), list(createdAt = "2021-06-02T00:00:00Z"))))))
  out <- parse_connection(resp, "forks")
  expect_equal(out$timestamps, c("2021-06-01T00:00:00Z", "2021-06-02T00:00:00Z"))
  expect_equal(out$end_cursor, "CURSOR2")
  expect_false(out$has_next)
})

test_that("parse_connection extracts timestamps from a nodes-shaped (releases) response", {
  resp <- list(data = list(repository = list(releases = list(
    pageInfo = list(endCursor = NA, hasNextPage = FALSE),
    nodes = list(list(createdAt = "2022-01-01T00:00:00Z"))))))
  out <- parse_connection(resp, "releases_total")
  expect_equal(out$timestamps, "2022-01-01T00:00:00Z")
  expect_false(out$has_next)
})

test_that("parse_connection degrades to empty, has_next FALSE on a null connection node (edges and nodes shapes)", {
  resp <- list(data = list(repository = list(stargazers = NULL)))
  out <- parse_connection(resp, "stars")
  expect_equal(out$timestamps, character(0))
  expect_true(is.na(out$end_cursor))
  expect_false(out$has_next)

  resp2 <- list(data = list(repository = list(forks = NULL)))
  out2 <- parse_connection(resp2, "forks")
  expect_equal(out2$timestamps, character(0))
  expect_true(is.na(out2$end_cursor))
  expect_false(out2$has_next)
})

test_that("parse_stargazers wrapper matches parse_connection(..., \"stars\")", {
  resp <- list(data = list(repository = list(stargazers = list(
    pageInfo = list(endCursor = "CURSOR1", hasNextPage = TRUE),
    edges = list(list(starredAt = "2020-01-01T00:00:00Z"))))))
  out <- parse_stargazers(resp)
  expect_equal(out$starred_at, "2020-01-01T00:00:00Z")
  expect_equal(out$end_cursor, "CURSOR1")
  expect_true(out$has_next)
})

test_that("paginate_connection loops after cursors until has_next is FALSE (stars, edges)", {
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
  out <- paginate_connection(io, "o", "n", "stars", delay = 0)
  expect_equal(out, c("2020-01-01T00:00:00Z", "2020-01-02T00:00:00Z"))
  expect_equal(calls$n, 2L)
  expect_equal(calls$cursors, c("null", "cursor"))   # first page has no cursor, second carries C1
})

test_that("paginate_connection loops after cursors until has_next is FALSE (forks, nodes)", {
  pages <- list(
    list(data = list(repository = list(forks = list(
      pageInfo = list(endCursor = "C1", hasNextPage = TRUE),
      nodes = list(list(createdAt = "2021-01-01T00:00:00Z")))))),
    list(data = list(repository = list(forks = list(
      pageInfo = list(endCursor = "C2", hasNextPage = FALSE),
      nodes = list(list(createdAt = "2021-01-02T00:00:00Z")))))))
  calls <- new.env(); calls$n <- 0L
  io <- list(graphql = function(query) { calls$n <- calls$n + 1L; pages[[calls$n]] })
  out <- paginate_connection(io, "o", "n", "forks", delay = 0)
  expect_equal(out, c("2021-01-01T00:00:00Z", "2021-01-02T00:00:00Z"))
  expect_equal(calls$n, 2L)
})

test_that("paginate_connection returns character(0) for a repo with no items on the connection", {
  io <- list(graphql = function(query) list(data = list(repository = list(releases = list(
    pageInfo = list(endCursor = NULL, hasNextPage = FALSE), nodes = list())))))
  expect_equal(paginate_connection(io, "o", "n", "releases_total", delay = 0), character(0))
})

test_that("paginate_connection errors when a mid-pagination page carries GraphQL errors (no silent truncation)", {
  pages <- list(
    list(data = list(repository = list(stargazers = list(
      pageInfo = list(endCursor = "C1", hasNextPage = TRUE),
      edges = list(list(starredAt = "2019-01-01T00:00:00Z")))))),
    # HTTP-200-with-errors shape: data present but stargazers null AND errors set
    list(data = list(repository = list(stargazers = NULL)),
         errors = list(list(message = "SECONDARY_RATE_LIMIT"))))
  calls <- new.env(); calls$n <- 0L
  io <- list(graphql = function(query) { calls$n <- calls$n + 1L; pages[[calls$n]] })
  expect_error(paginate_connection(io, "o", "n", "stars", delay = 0), "stargazers page error")
})

test_that("paginate_connection errors on a null-data response instead of degrading to empty", {
  io <- list(graphql = function(query) list(data = NULL, errors = list(list(message = "BAD_CREDENTIALS"))))
  expect_error(paginate_connection(io, "o", "n", "stars", delay = 0), "stargazers page error")
  expect_error(paginate_connection(io, "o", "n", "forks", delay = 0), "forks page error")
})

test_that("paginate_connection stops (does not loop forever) when has_next is TRUE but the cursor is null", {
  io <- list(graphql = function(query) list(data = list(repository = list(stargazers = list(
    pageInfo = list(endCursor = NULL, hasNextPage = TRUE),
    edges = list(list(starredAt = "2019-01-01T00:00:00Z")))))))
  expect_error(paginate_connection(io, "o", "n", "stars", delay = 0), "no cursor")
})

test_that("a repository-null response with NO errors still degrades to empty (repo gone / private)", {
  io <- list(graphql = function(query) list(data = list(repository = NULL)))
  expect_equal(paginate_connection(io, "o", "n", "stars", delay = 0), character(0))
  expect_equal(paginate_connection(io, "o", "n", "releases_total", delay = 0), character(0))
})

test_that("paginate_stargazers wrapper behaves identically to paginate_connection(..., \"stars\")", {
  io <- list(graphql = function(query) list(data = list(repository = list(stargazers = list(
    pageInfo = list(endCursor = NULL, hasNextPage = FALSE),
    edges = list(list(starredAt = "2021-01-01T00:00:00Z")))))))
  expect_equal(paginate_stargazers(io, "o", "n", delay = 0),
              paginate_connection(io, "o", "n", "stars", delay = 0))
})
