test_that("changed_shards reports added and modified", {
  prev <- c("vcs-signals-2026.db" = "aaa", "vcs-signals-recent.db" = "bbb")
  curr <- c("vcs-signals-2026.db" = "ZZZ", "vcs-signals-recent.db" = "bbb", "vcs-signals-summary.db" = "ccc")
  expect_setequal(changed_shards(prev, curr), c("vcs-signals-2026.db", "vcs-signals-summary.db"))
})

test_that("publish uploads changed shards on first run and heartbeats when unchanged", {
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSERT INTO signals_series VALUES ('R','2026-07-06','stars',10)")
  uploaded <- new.env(); uploaded$paths <- character(0)
  io <- list(
    release_exists = function() FALSE,
    download = function(pattern, dir) FALSE,
    upload = function(path) uploaded$paths <- c(uploaded$paths, basename(path)))
  out <- tempfile("out"); dir.create(out)
  publish(io, con, out, "v1", "live", force_full = TRUE)
  expect_true("manifest.json" %in% uploaded$paths)
  expect_true(any(grepl("vcs-signals-2026.db", uploaded$paths)))
})
