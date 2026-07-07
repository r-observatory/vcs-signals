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
