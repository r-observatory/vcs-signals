# Fake io: returns success from a lookup table keyed by the sorted id set; a
# batch whose id-set is in `fail_sets` throws (simulating a 502).
fake_io <- function(node_map, fail_sets = list()) {
  list(graphql = function(query) {
    ids <- regmatches(query, gregexpr('"(R_[^"]+)"', query))[[1]]
    ids <- gsub('"', "", ids)
    key <- paste(sort(ids), collapse = ",")
    if (key %in% vapply(fail_sets, function(s) paste(sort(s), collapse = ","), "")) stop("502")
    nodes <- lapply(ids, function(i) node_map[[i]])
    list(data = list(nodes = nodes))
  })
}

test_that("collect_batched halves on failure and defers a persistently-failing single repo", {
  nm <- list(R_1 = list(id = "R_1", nameWithOwner = "a/1"),
             R_2 = list(id = "R_2", nameWithOwner = "b/2"))
  # the full 2-id batch fails; each singleton: R_1 ok, R_2 fails
  io <- fake_io(nm, fail_sets = list(c("R_1", "R_2"), c("R_2")))
  parse_ids <- function(nodes) do.call(rbind, lapply(Filter(Negate(is.null), nodes),
    function(n) data.frame(node_id = n$id, stringsAsFactors = FALSE)))
  res <- collect_batched(io, c("R_1", "R_2"), 2, function(b) build_gauge_query(b), parse_ids)
  expect_equal(res$records$node_id, "R_1")
  expect_equal(res$deferred, "R_2")
})

.ids_in <- function(query) gsub('"', "", regmatches(query, gregexpr('"(R_[^"]+)"', query))[[1]])
.parse_ids <- function(nodes) {
  keep <- Filter(Negate(is.null), nodes)
  if (!length(keep)) return(NULL)
  data.frame(node_id = vapply(keep, function(n) n$id, ""), stringsAsFactors = FALSE)
}
# Answers like GitHub: an id in `dead` comes back null with a NOT_FOUND on nodes[i].
.dead_io <- function(dead, calls) {
  list(graphql = function(query) {
    ids <- .ids_in(query)
    calls(ids)
    errs <- lapply(which(ids %in% dead), function(i) list(type = "NOT_FOUND",
      path = list("nodes", i - 1L),
      message = sprintf("Could not resolve to a node with the global id of '%s'", ids[i])))
    nodes <- lapply(ids, function(i) if (i %in% dead) NULL else list(id = i))
    if (length(errs)) list(data = list(nodes = nodes), errors = errs) else list(data = list(nodes = nodes))
  })
}

test_that("an unresolvable node id is reported once and its batch is kept, not halved", {
  seen <- list()
  io <- .dead_io("R_3", function(ids) seen[[length(seen) + 1L]] <<- ids)
  res <- collect_batched(io, paste0("R_", 1:6), 6, build_gauge_query, .parse_ids)
  expect_length(seen, 1L)
  expect_equal(sort(res$records$node_id), paste0("R_", c(1, 2, 4, 5, 6)))
  expect_equal(res$unresolvable, "R_3")
  expect_length(res$deferred, 0L)
  expect_length(res$unreached, 0L)
})

test_that("the dead id is found from the path when the message does not name it", {
  res <- list(data = list(nodes = list(list(id = "R_1"), NULL)),
              errors = list(list(type = "NOT_FOUND", path = list("nodes", 1L), message = "gone")))
  expect_equal(unresolvable_node_ids(res, c("R_1", "R_2")), "R_2")
})

test_that("a NOT_FOUND beside any other error still halves the batch", {
  res <- list(data = list(nodes = list(NULL, NULL)),
              errors = list(list(type = "NOT_FOUND", path = list("nodes", 0L), message = "x"),
                            list(type = "RATE_LIMITED", message = "slow down")))
  expect_null(unresolvable_node_ids(res, c("R_1", "R_2")))
  expect_null(unresolvable_node_ids(list(data = NULL, errors = res$errors[1]), c("R_1", "R_2")))
  expect_null(unresolvable_node_ids(list(data = list(nodes = list(NULL)),
    errors = list(list(type = "NOT_FOUND", message = "no path or id"))), "R_1"))
})

test_that("collection stops at the deadline and returns the rest as unreached", {
  clock <- 0
  io <- .dead_io(character(0), function(ids) clock <<- clock + 10)
  res <- collect_batched(io, paste0("R_", 1:10), 2, build_gauge_query, .parse_ids,
                         deadline = 25, now = function() clock)
  expect_equal(res$records$node_id, paste0("R_", 1:6))
  expect_equal(res$unreached, paste0("R_", 7:10))
  expect_length(res$deferred, 0L)
})

test_that("a deadline already passed queries nothing", {
  n <- 0L
  io <- .dead_io(character(0), function(ids) n <<- n + 1L)
  res <- collect_batched(io, paste0("R_", 1:4), 2, build_gauge_query, .parse_ids,
                         deadline = 0, now = function() 1)
  expect_equal(n, 0L)
  expect_null(res$records)
  expect_equal(res$unreached, paste0("R_", 1:4))
})

test_that("progress is logged every log_every queries", {
  lines <- character(0)
  io <- .dead_io(character(0), function(ids) NULL)
  collect_batched(io, paste0("R_", 1:10), 2, build_gauge_query, .parse_ids,
                  log = function(m) lines <<- c(lines, m), log_every = 2L)
  expect_length(lines, 2L)
  expect_match(lines[1], "^2 queries, 4 collected, 0 deferred, 6 ids left")
})

test_that("rotate_by_day starts at a different tenth each day and keeps every id", {
  ids <- sprintf("R_%02d", 1:20)
  d0 <- as.Date("2026-09-20")
  starts <- vapply(0:9, function(k) rotate_by_day(ids, d0 + k)[1], "")
  expect_length(unique(starts), 10L)
  for (k in 0:9) expect_setequal(rotate_by_day(ids, d0 + k), ids)
  expect_equal(rotate_by_day("R_1", d0), "R_1")
})

test_that("more unresolvable ids than the cap are not looked up or changed", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  ensure_repo_schema(con)
  calls <- 0L
  io <- list(graphql = function(query) { calls <<- calls + 1L; stop("should not be called") })
  ids <- paste0("R_", 1:101)
  expect_true(repoint_dead_node_ids(con, io, ids, 100000)$skipped)
  expect_true(repoint_dead_node_ids(con, io, ids[1:3], 200)$skipped)
  expect_equal(calls, 0L)
  expect_false(repoint_dead_node_ids(con, io, character(0), 10)$skipped)
})

test_that("a deadline cut warns whenever repos are left and fails past the threshold", {
  expect_length(gauge_cut_verdict(0, 100)$lines, 0L)
  small <- gauge_cut_verdict(10, 100)
  expect_false(small$fail)
  expect_match(small$lines, "^::warning::gauges: 10 of 100 repos \\(10.0%\\)")
  big <- gauge_cut_verdict(26, 100)
  expect_true(big$fail)
  expect_match(big$lines[2], "^::error::")
})
