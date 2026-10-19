test_that("every publishing path reproduces the golden record", {
  for (p in golden_paths()) {
    got <- golden_snapshot_text(golden_run(p))
    dir <- file.path("fixtures", "golden", p)
    want_files <- sort(list.files(dir))
    expect_identical(sort(names(got)), want_files, info = p)
    for (f in want_files) expect_identical(got[[f]], paste(readLines(file.path(dir, f)), collapse = "\n"), info = paste(p, f))
  }
})

test_that("the harness notices a changed published value", {
  run <- golden_run("daily")
  k <- grep("series_latest$", names(run$tables), value = TRUE)[1]
  run$tables[[k]]$value[1] <- run$tables[[k]]$value[1] + 1
  got <- golden_snapshot_text(run)
  want <- paste(readLines(file.path("fixtures", "golden", "daily", paste0(sub("/", "__", k), ".tsv"))), collapse = "\n")
  expect_false(identical(got[[paste0(sub("/", "__", k), ".tsv")]], want))
})

test_that("each path publishes its own tables", {
  expect_true(any(grepl("vcs_signals_summary", names(golden_run("daily")$tables))))
  expect_true(any(grepl("vcs_ai_signals", names(golden_run("ai")$tables))))
  expect_true(any(grepl("series_latest", names(golden_run("weekly")$tables))))
  expect_true(any(grepl("signals_series", names(golden_run("backfill")$tables))))
})
