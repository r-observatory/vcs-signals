test_that("gauges_to_long maps node_id->repo_id and drops NA metrics", {
  snap <- data.frame(node_id = "R_a", stars = 10L, forks = 2L, commits_total = NA_integer_,
                     watchers = 3L, issues_open = 1L, issues_closed = 0L, prs_open = 0L,
                     prs_closed = 0L, prs_merged = 0L, releases_total = 0L, size_kb = 100L,
                     stringsAsFactors = FALSE)
  rmap <- data.frame(node_id = "R_a", repo_id = "github.com/o/n", stringsAsFactors = FALSE)
  long <- gauges_to_long(snap, rmap)
  expect_false("commits_total" %in% long$metric)         # NA dropped
  expect_equal(long$value[long$metric == "stars"], 10L)
  expect_true(all(long$repo_id == "github.com/o/n"))
})

test_that("materialize_series emits only changed/new metrics", {
  prev <- data.frame(repo_id = "R", metric = c("stars", "forks"), value = c(10L, 2L), stringsAsFactors = FALSE)
  snap <- data.frame(repo_id = "R", metric = c("stars", "forks", "watchers"),
                     value = c(11L, 2L, 5L), stringsAsFactors = FALSE)   # stars changed, forks same, watchers new
  out <- materialize_series(prev, snap, "2026-07-06")
  expect_setequal(out$series_rows$metric, c("stars", "watchers"))
  expect_true(all(out$series_rows$date == "2026-07-06"))
  expect_equal(nrow(out$new_latest), 3)                  # new_latest is the full snapshot
})
