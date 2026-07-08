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

test_that("build_connection_query includes closedAt in the selection for issues_open/prs_open but not for cumulative metrics", {
  qi <- build_connection_query("tidyverse", "ggplot2", "issues_open")
  expect_match(qi, "issues\\(")
  expect_match(qi, "nodes \\{ createdAt closedAt \\}")

  qp <- build_connection_query("tidyverse", "ggplot2", "prs_open")
  expect_match(qp, "pullRequests\\(")
  expect_match(qp, "nodes \\{ createdAt closedAt \\}")

  qs <- build_connection_query("tidyverse", "ggplot2", "stars")
  expect_false(grepl("closedAt", qs, fixed = TRUE))
  qf <- build_connection_query("tidyverse", "ggplot2", "forks")
  expect_false(grepl("closedAt", qf, fixed = TRUE))
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

test_that("build_batched_query emits one aliased repository block per repo, keeping the metric's connection/order/selection", {
  repos <- data.frame(repo_id = c("github.com/a/one", "github.com/b/two"),
                      owner = c("a", "b"), name = c("one", "two"), stringsAsFactors = FALSE)
  q <- build_batched_query(repos, "stars")
  expect_match(q, 'r0: repository\\(owner: "a", name: "one"\\)')
  expect_match(q, 'r1: repository\\(owner: "b", name: "two"\\)')
  expect_match(q, "stargazers\\(")
  expect_match(q, "STARRED_AT")
  expect_match(q, "edges \\{ starredAt \\}")
  expect_false(grepl("after:", q, fixed = TRUE))  # batched query is always page 1, no cursor arg
})

test_that("build_batched_query includes closedAt for issues_open", {
  repos <- data.frame(repo_id = "github.com/a/one", owner = "a", name = "one", stringsAsFactors = FALSE)
  q <- build_batched_query(repos, "issues_open")
  expect_match(q, 'r0: repository\\(owner: "a", name: "one"\\)')
  expect_match(q, "issues\\(")
  expect_match(q, "nodes \\{ createdAt closedAt \\}")
})

test_that("build_batched_query errors on an unknown metric", {
  repos <- data.frame(repo_id = "x", owner = "o", name = "n", stringsAsFactors = FALSE)
  expect_error(build_batched_query(repos, "watchers"), "unknown metric")
})

test_that("parse_connection extracts a ts-column nodes frame, endCursor, and hasNextPage from an edges-shaped (stars) response", {
  resp <- list(data = list(repository = list(stargazers = list(
    pageInfo = list(endCursor = "CURSOR1", hasNextPage = TRUE),
    edges = list(list(starredAt = "2020-01-01T00:00:00Z"), list(starredAt = "2020-01-02T00:00:00Z"))))))
  out <- parse_connection(resp, "stars")
  expect_equal(out$nodes$ts, c("2020-01-01T00:00:00Z", "2020-01-02T00:00:00Z"))
  expect_equal(out$end_cursor, "CURSOR1")
  expect_true(out$has_next)
})

test_that("parse_connection extracts a ts-column nodes frame, endCursor, and hasNextPage from a nodes-shaped (forks) response", {
  resp <- list(data = list(repository = list(forks = list(
    pageInfo = list(endCursor = "CURSOR2", hasNextPage = FALSE),
    nodes = list(list(createdAt = "2021-06-01T00:00:00Z"), list(createdAt = "2021-06-02T00:00:00Z"))))))
  out <- parse_connection(resp, "forks")
  expect_equal(out$nodes$ts, c("2021-06-01T00:00:00Z", "2021-06-02T00:00:00Z"))
  expect_equal(out$end_cursor, "CURSOR2")
  expect_false(out$has_next)
})

test_that("parse_connection extracts a ts-column nodes frame from a nodes-shaped (releases) response", {
  resp <- list(data = list(repository = list(releases = list(
    pageInfo = list(endCursor = NA, hasNextPage = FALSE),
    nodes = list(list(createdAt = "2022-01-01T00:00:00Z"))))))
  out <- parse_connection(resp, "releases_total")
  expect_equal(out$nodes$ts, "2022-01-01T00:00:00Z")
  expect_false(out$has_next)
})

test_that("parse_connection returns a created/closed nodes frame for an issues fixture (one closed, one still-open)", {
  resp <- list(data = list(repository = list(issues = list(
    pageInfo = list(endCursor = "C3", hasNextPage = FALSE),
    nodes = list(
      list(createdAt = "2022-01-01T00:00:00Z", closedAt = "2022-01-03T00:00:00Z"),
      list(createdAt = "2022-01-02T00:00:00Z", closedAt = NULL))))))
  out <- parse_connection(resp, "issues_open")
  expect_equal(out$nodes$created, c("2022-01-01T00:00:00Z", "2022-01-02T00:00:00Z"))
  expect_equal(out$nodes$closed, c("2022-01-03T00:00:00Z", NA_character_))
  expect_false(out$has_next)
})

test_that("parse_connection returns a created/closed nodes frame for prs_open (merged PRs are closed)", {
  resp <- list(data = list(repository = list(pullRequests = list(
    pageInfo = list(endCursor = NA, hasNextPage = FALSE),
    nodes = list(list(createdAt = "2023-01-01T00:00:00Z", closedAt = "2023-01-02T00:00:00Z"))))))
  out <- parse_connection(resp, "prs_open")
  expect_equal(out$nodes$created, "2023-01-01T00:00:00Z")
  expect_equal(out$nodes$closed, "2023-01-02T00:00:00Z")
})

test_that("parse_connection degrades to empty nodes, has_next FALSE on a null connection node (cumulative and open shapes)", {
  resp <- list(data = list(repository = list(stargazers = NULL)))
  out <- parse_connection(resp, "stars")
  expect_equal(nrow(out$nodes), 0)
  expect_true(is.na(out$end_cursor))
  expect_false(out$has_next)

  resp2 <- list(data = list(repository = list(forks = NULL)))
  out2 <- parse_connection(resp2, "forks")
  expect_equal(nrow(out2$nodes), 0)
  expect_true(is.na(out2$end_cursor))
  expect_false(out2$has_next)

  resp3 <- list(data = list(repository = list(issues = NULL)))
  out3 <- parse_connection(resp3, "issues_open")
  expect_equal(nrow(out3$nodes), 0)
  expect_equal(names(out3$nodes), c("created", "closed"))
  expect_false(out3$has_next)
})

test_that("connection_timestamps extracts the ts vector from a parsed cumulative result", {
  resp <- list(data = list(repository = list(forks = list(
    pageInfo = list(endCursor = NA, hasNextPage = FALSE),
    nodes = list(list(createdAt = "2021-01-01T00:00:00Z"))))))
  out <- parse_connection(resp, "forks")
  expect_equal(connection_timestamps(out), "2021-01-01T00:00:00Z")
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

test_that("fetch_batched_page maps a canned 2-alias response back to the two repo_ids with their nodes + has_next", {
  repos <- data.frame(repo_id = c("github.com/a/one", "github.com/b/two"),
                      owner = c("a", "b"), name = c("one", "two"), stringsAsFactors = FALSE)
  io <- list(graphql = function(query) {
    list(data = list(
      r0 = list(stargazers = list(
        pageInfo = list(endCursor = "C1", hasNextPage = TRUE),
        edges = list(list(starredAt = "2020-01-01T00:00:00Z")))),
      r1 = list(stargazers = list(
        pageInfo = list(endCursor = NA, hasNextPage = FALSE),
        edges = list(list(starredAt = "2021-01-01T00:00:00Z"))))))
  })
  out <- fetch_batched_page(io, repos, "stars")
  expect_named(out, c("github.com/a/one", "github.com/b/two"))
  expect_equal(out[["github.com/a/one"]]$nodes$ts, "2020-01-01T00:00:00Z")
  expect_true(out[["github.com/a/one"]]$has_next)
  expect_equal(out[["github.com/a/one"]]$end_cursor, "C1")
  expect_equal(out[["github.com/b/two"]]$nodes$ts, "2021-01-01T00:00:00Z")
  expect_false(out[["github.com/b/two"]]$has_next)
})

test_that("fetch_batched_page stops on a batched response carrying errors or null data", {
  repos <- data.frame(repo_id = "x", owner = "o", name = "n", stringsAsFactors = FALSE)
  io_err <- list(graphql = function(query) list(data = NULL, errors = list(list(message = "boom"))))
  expect_error(fetch_batched_page(io_err, repos, "stars"), "batched page error")
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
  expect_equal(out$ts, c("2020-01-01T00:00:00Z", "2020-01-02T00:00:00Z"))
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
  expect_equal(out$ts, c("2021-01-01T00:00:00Z", "2021-01-02T00:00:00Z"))
  expect_equal(calls$n, 2L)
})

test_that("paginate_connection returns a zero-row nodes frame for a repo with no items on the connection", {
  io <- list(graphql = function(query) list(data = list(repository = list(releases = list(
    pageInfo = list(endCursor = NULL, hasNextPage = FALSE), nodes = list())))))
  out <- paginate_connection(io, "o", "n", "releases_total", delay = 0)
  expect_equal(nrow(out), 0)
})

test_that("paginate_connection with after/first_nodes resumes from a cursor and prepends the already-fetched page", {
  # Simulates completing a repo whose batched first page reported hasNextPage:
  # first_nodes is that already-fetched page 1, and the mock returns page 2.
  first_nodes <- data.frame(ts = "2020-01-01T00:00:00Z", stringsAsFactors = FALSE)
  calls <- new.env(); calls$n <- 0L; calls$afters <- character(0)
  io <- list(graphql = function(query) {
    calls$n <- calls$n + 1L
    calls$afters <- c(calls$afters,
      if (grepl('after: "C1"', query, fixed = TRUE)) "C1" else "other")
    list(data = list(repository = list(forks = list(
      pageInfo = list(endCursor = NA, hasNextPage = FALSE),
      nodes = list(list(createdAt = "2020-02-01T00:00:00Z"))))))
  })
  out <- paginate_connection(io, "o", "n", "forks", delay = 0, after = "C1", first_nodes = first_nodes)
  expect_equal(out$ts, c("2020-01-01T00:00:00Z", "2020-02-01T00:00:00Z"))
  expect_equal(calls$n, 1L)               # only the continuation page was fetched, not page 1 again
  expect_equal(calls$afters, "C1")
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

test_that("a repository-null response with NO errors still degrades to a zero-row nodes frame (repo gone / private)", {
  io <- list(graphql = function(query) list(data = list(repository = NULL)))
  expect_equal(nrow(paginate_connection(io, "o", "n", "stars", delay = 0)), 0)
  expect_equal(nrow(paginate_connection(io, "o", "n", "releases_total", delay = 0)), 0)
})

test_that("paginate_stargazers wrapper behaves identically to paginate_connection(..., \"stars\")", {
  io <- list(graphql = function(query) list(data = list(repository = list(stargazers = list(
    pageInfo = list(endCursor = NULL, hasNextPage = FALSE),
    edges = list(list(starredAt = "2021-01-01T00:00:00Z")))))))
  expect_equal(paginate_stargazers(io, "o", "n", delay = 0),
              paginate_connection(io, "o", "n", "stars", delay = 0))
})
