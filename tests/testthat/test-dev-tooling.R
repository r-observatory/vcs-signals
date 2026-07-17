test_that("DEV_TOOLING_MARKERS is well-formed: unique cols, valid location and match", {
  cols <- vapply(DEV_TOOLING_MARKERS, function(m) m$col, character(1))
  expect_false(any(duplicated(cols)))
  for (m in DEV_TOOLING_MARKERS) {
    expect_true(is.character(m$paths) && length(m$paths) >= 1 && all(nzchar(m$paths)))
    expect_true((m$location %||% "root") %in% c("root", "github", "both"))
    expect_true((m$match %||% "exact") %in% c("exact", "suffix"))
  }
  # The ambient .positai marker lands here as has_positron (its dev-tooling home).
  expect_true("has_positron" %in% cols)
  # has_ci and readme_source are COMPUTED, never marker cols.
  expect_false(any(c("has_ci", "readme_source") %in% cols))
  # At least one CI marker exists so the has_ci rollup has inputs.
  expect_true(any(grepl("^ci_", cols)))
  # Exactly one suffix marker: the *.Rproj case.
  suffix <- Filter(function(m) identical(m$match %||% "exact", "suffix"), DEV_TOOLING_MARKERS)
  expect_equal(vapply(suffix, function(m) m$col, character(1)), "has_rproj")
})
