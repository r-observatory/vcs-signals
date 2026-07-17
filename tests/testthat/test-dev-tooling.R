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

test_that("classify_dev_tooling detects root, github, and both-location tokens", {
  out <- classify_dev_tooling(
    root_entries   = c("renv.lock", ".lintr", "CODEOWNERS", "DESCRIPTION"),
    github_entries = c("workflows", "SECURITY.md"))
  expect_equal(nrow(out), 1)
  expect_equal(out$has_renv, 1L)            # root token
  expect_equal(out$has_lintr, 1L)           # root token
  expect_equal(out$ci_github_actions, 1L)   # github: workflows dir under .github
  expect_equal(out$has_security, 1L)        # both: satisfied by the .github copy
  expect_equal(out$has_codeowners, 1L)      # both: satisfied by the root copy
  expect_equal(out$has_dependabot, 0L)      # github-only token, absent
})

test_that("classify_dev_tooling matches *.Rproj by suffix, not by exact name", {
  expect_equal(classify_dev_tooling(c("mypkg.Rproj"), character(0))$has_rproj, 1L)
  expect_equal(classify_dev_tooling(c("Rproj"), character(0))$has_rproj, 0L)  # not "*.Rproj"
})

test_that("classify_dev_tooling reads directory tokens (renv, data-raw, .circleci)", {
  out <- classify_dev_tooling(c("renv", "data-raw", ".circleci"), character(0))
  expect_equal(out$has_renv, 1L)      # renv dir also satisfies the renv.lock-or-renv flag
  expect_equal(out$has_data_raw, 1L)
  expect_equal(out$ci_circleci, 1L)
})

test_that("classify_dev_tooling README source enum prefers qmd, then rmd, then md, then none", {
  expect_equal(classify_dev_tooling(c("README.qmd", "README.Rmd", "README.md"), character(0))$readme_source, "qmd")
  expect_equal(classify_dev_tooling(c("README.Rmd", "README.md"), character(0))$readme_source, "rmd")
  expect_equal(classify_dev_tooling(c("README.md"), character(0))$readme_source, "md")
  expect_equal(classify_dev_tooling(c("DESCRIPTION"), character(0))$readme_source, "none")
})

test_that("classify_dev_tooling has_ci is the OR of the ci_* systems", {
  expect_equal(classify_dev_tooling(c("renv.lock"), character(0))$has_ci, 0L)
  expect_equal(classify_dev_tooling(character(0), c("workflows"))$has_ci, 1L)  # gha
  expect_equal(classify_dev_tooling(c(".travis.yml"), character(0))$has_ci, 1L)
})

test_that("classify_dev_tooling maps the ambient .positai marker to has_positron", {
  expect_equal(classify_dev_tooling(c(".positai"), character(0))$has_positron, 1L)
})

test_that("classify_dev_tooling returns an all-zero, none row when nothing matches", {
  out <- classify_dev_tooling(c("R", "man", "DESCRIPTION", "NAMESPACE"), character(0))
  expect_equal(nrow(out), 1)
  expect_true(all(as.integer(out[1, dev_tooling_marker_cols()]) == 0L))
  expect_equal(out$readme_source, "none")
  expect_equal(out$has_ci, 0L)
})

test_that("classify_dev_tooling guards NULL inputs like classify_tree_markers", {
  out <- classify_dev_tooling(NULL, NULL)
  expect_equal(nrow(out), 1)
  expect_true(all(as.integer(out[1, dev_tooling_marker_cols()]) == 0L))
})

test_that("classifier, empty helper, and DDL share one config-derived column set (drift guard)", {
  expect_identical(names(classify_dev_tooling(character(0), character(0))), dev_tooling_columns())
  expect_identical(names(.devtool_empty()), dev_tooling_columns())
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, dev_tooling_create_sql())
  expect_identical(DBI::dbListFields(con, "vcs_dev_tooling"),
                   c("repo_id", "last_scanned", dev_tooling_columns()))
  # WITHOUT ROWID is a deliberate departure; assert it survives in the stored DDL so the
  # merger (which copies the CREATE TABLE text verbatim) reproduces it downstream.
  sql <- DBI::dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE name = 'vcs_dev_tooling'")$sql
  expect_true(grepl("WITHOUT ROWID", sql, ignore.case = TRUE))
})
