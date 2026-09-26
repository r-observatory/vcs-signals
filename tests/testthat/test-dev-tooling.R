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
                   c("repo_id", "last_scanned", "ruleset_version", dev_tooling_columns()))
  info <- DBI::dbGetQuery(con, "PRAGMA table_info(vcs_dev_tooling)")
  expect_identical(stats::setNames(info$type, info$name)[dev_tooling_columns()], dev_tooling_column_types())
  expect_identical(vapply(.devtool_empty(), class, ""),
                   ifelse(dev_tooling_column_types() == "TEXT", "character", "integer"))
  row <- classify_dev_tooling(character(0), character(0))
  expect_identical(vapply(row, class, ""), vapply(.devtool_empty(), class, ""))
  # WITHOUT ROWID is a deliberate departure; assert it survives in the stored DDL so the
  # merger (which copies the CREATE TABLE text verbatim) reproduces it downstream.
  sql <- DBI::dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE name = 'vcs_dev_tooling'")$sql
  expect_true(grepl("WITHOUT ROWID", sql, ignore.case = TRUE))
})

test_that("ensure_series_schema creates vcs_dev_tooling with the config-derived columns", {
  con <- new_test_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(DBI::dbExistsTable(con, "vcs_dev_tooling"))
  expect_identical(DBI::dbListFields(con, "vcs_dev_tooling"),
                   c("repo_id", "last_scanned", "ruleset_version", dev_tooling_columns()))
})

test_that("ai-weekly.yml uploads and downloads the dev-tooling shards", {
  yml <- readLines(file.path(.repo_root, ".github", "workflows", "ai-weekly.yml"))
  # The cheap job uploads the new shard glob, and the merge job downloads the artifact.
  expect_true(any(grepl("vcs-dev-tooling-\\*\\.db", yml)))
  expect_true(any(grepl("dev-tooling-\\$\\{\\{ matrix.shard \\}\\}", yml)))  # cheap upload name
  expect_true(any(grepl("pattern: dev-tooling-\\*", yml)))                    # merge download
})

test_that("Air is detected under both spellings it ships", {
  # air.toml and .air.toml are both in wide use (roughly 11k and 9k repositories),
  # and only the undotted one was listed, so the dotted half went uncounted.
  expect_equal(classify_dev_tooling(c("air.toml"), character(0))$has_air, 1L)
  expect_equal(classify_dev_tooling(c(".air.toml"), character(0))$has_air, 1L)
  expect_equal(classify_dev_tooling(c("DESCRIPTION"), character(0))$has_air, 0L)
})

test_that("documentation written for models is a practice, never an AI marker", {
  # llms.txt is the package describing itself TO a model. Counting it as evidence a
  # model worked on the package would be a different claim entirely.
  flags <- classify_dev_tooling(c("llms.txt"), character(0))
  expect_equal(flags$has_llms_txt, 1L)
  expect_equal(classify_dev_tooling(c("llms-full.txt"), character(0))$has_llms_txt, 1L)
  expect_equal(nrow(classify_tree_markers(c("llms.txt", "llms-full.txt"), character(0))), 0L)
})

test_that("skills a package ships are a practice, and skills used to build it are not", {
  # inst/ is installed, so inst/skills is a deliverable for the package's users. The
  # skills under .claude/ are what the maintainer used, and belong to the AI signal.
  ship <- classify_dev_tooling(c("inst/skills", "DESCRIPTION"), character(0))
  expect_equal(ship$has_agent_skills, 1L)
  expect_equal(nrow(classify_tree_markers(c("inst/skills"), character(0))), 0L)

  build <- classify_dev_tooling(c(".claude/skills"), character(0))
  expect_equal(build$has_agent_skills, 0L)
  expect_true("claude" %in% classify_tree_markers(c(".claude/skills"), character(0))$tool)
})

test_that("a directory named skills anywhere else is not a shipped skill", {
  expect_equal(classify_dev_tooling(c("skills"), character(0))$has_agent_skills, 0L)
  expect_equal(classify_dev_tooling(c("dev/skills"), character(0))$has_agent_skills, 0L)
})

test_that("a published table gains every new column with its declared type", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE vcs_dev_tooling (repo_id TEXT PRIMARY KEY, last_scanned TEXT, has_lintr INTEGER)")
  ensure_series_schema(con)
  info <- DBI::dbGetQuery(con, "PRAGMA table_info(vcs_dev_tooling)")
  types <- stats::setNames(info$type, info$name)
  expect_equal(types[["ruleset_version"]], "TEXT")
  expect_identical(types[dev_tooling_columns()], dev_tooling_column_types())
})

test_that("the ruleset version names v3 and the day it landed", {
  expect_match(DEV_TOOLING_RULESET_VERSION, "^v3 \\(\\d{4}-\\d{2}-\\d{2}\\)$")
  for (d in DEV_TOOLING_DERIVED) {
    expect_true(d$type %in% c("INTEGER", "TEXT"), info = d$col)
    expect_true(d$source %in% c("tree", "graphql", "workflow_text", "derived"), info = d$col)
    expect_true(nzchar(d$rule), info = d$col)
  }
  expect_false(any(duplicated(dev_tooling_columns())))
})

test_that("every subtree a rule names is one the contents query lists", {
  for (m in DEV_TOOLING_MARKERS)
    expect_identical(unfetched_rule_paths(m$paths, m$location %||% "root"), character(0), info = m$col)
  for (paths in list(COC_TREE_PATHS, CONTRIBUTING_TREE_PATHS, PR_TEMPLATE_TREE_PATHS))
    expect_identical(unfetched_rule_paths(paths, "both"), character(0))
  # The retired litedown path is the case this check exists for.
  expect_identical(unfetched_rule_paths(c("site/_litedown.yml", "docs/_litedown.yml"), "root"),
                   "docs/_litedown.yml")
})

test_that("a rule path below a listed subtree's first level is reported, since only that level is listed", {
  expect_identical(unfetched_rule_paths(c("inst/skills", "inst/skills/SKILL.md"), "root"),
                   "inst/skills/SKILL.md")
  expect_identical(unfetched_rule_paths(c("workflows/check.yml", "workflows/sub/x.yml"), "github"),
                   "workflows/sub/x.yml")
  expect_identical(unfetched_rule_paths("inst/skills/SKILL.md", "both"), "inst/skills/SKILL.md")
})

test_that("the community and pkgdown lists are the ones the analyzer shares", {
  expect_identical(COC_TREE_PATHS, c("CODE_OF_CONDUCT.md", "CODE_OF_CONDUCT", "CODE_OF_CONDUCT.Rmd",
    "CODE_OF_CONDUCT.rst", "code_of_conduct.md", "Code_of_conduct.md", "CODE-OF-CONDUCT.md", "CONDUCT.md"))
  expect_identical(CONTRIBUTING_TREE_PATHS, c("CONTRIBUTING.md", "CONTRIBUTING", "CONTRIBUTING.Rmd",
    "CONTRIBUTING.rst", "contributing.md", "Contributing.md", "CONTRIBUTING.MD"))
  expect_identical(PR_TEMPLATE_TREE_PATHS, c("pull_request_template.md", "PULL_REQUEST_TEMPLATE.md",
                                             "PULL_REQUEST_TEMPLATE"))
  pk <- Find(function(m) m$col == "has_pkgdown", DEV_TOOLING_MARKERS)$paths
  expect_setequal(pk, c(PKGDOWN_CONFIG_TREE_PATHS, "pkgdown"))
})

test_that("the v3 tree rules read what the repository holds", {
  expect_equal(classify_dev_tooling(c("inst/_pkgdown.yml"), character(0))$has_pkgdown, 1L)
  expect_equal(classify_dev_tooling(c("inst/_pkgdown.yaml"), character(0))$has_pkgdown, 1L)
  expect_equal(classify_dev_tooling(c("docs/_litedown.yml"), character(0))$has_litedown, 0L)
  expect_equal(classify_dev_tooling(c("issue_template.md"), character(0))$has_issue_template, 1L)
  expect_equal(classify_dev_tooling(character(0), c("issue_template.md"))$has_issue_template, 1L)
  expect_equal(classify_dev_tooling(c("ISSUE_TEMPLATE"), character(0))$has_issue_template, 1L)
  expect_equal(classify_dev_tooling(c("tests"), character(0))$has_tests_dir, 1L)
  expect_equal(classify_dev_tooling(c("jarl.toml"), character(0))$has_jarl, 1L)
  expect_false("has_pr_template" %in% dev_tooling_marker_cols())
})

test_that("package_at_root says whether DESCRIPTION is at the root", {
  expect_equal(classify_dev_tooling(c("DESCRIPTION"), character(0))$package_at_root, 1L)
  expect_equal(classify_dev_tooling(c("pkg", "README.md"), character(0))$package_at_root, 0L)
})

test_that("Travis alone is told apart from Travis beside another CI system", {
  alone <- classify_dev_tooling(c(".travis.yml"), character(0))
  expect_equal(alone$ci_travis_only, 1L)
  expect_equal(alone$has_ci, 1L)
  both <- classify_dev_tooling(c(".travis.yml"), c("workflows"))
  expect_equal(both$ci_travis_only, 0L)
  expect_equal(both$has_ci, 1L)
  expect_equal(classify_dev_tooling(c("DESCRIPTION"), character(0))$ci_travis_only, 0L)
})

test_that("Travis beside any CI system has_ci counts is not Travis alone", {
  # has_ci reads every ci_ tree rule, so a new one must also clear ci_travis_only.
  for (m in Filter(function(m) startsWith(m$col, "ci_") && m$col != "ci_travis", DEV_TOOLING_MARKERS)) {
    other <- m$paths[[1]]
    in_github <- identical(m$location %||% "root", "github")
    row <- classify_dev_tooling(c(".travis.yml", if (!in_github) other),
                                if (in_github) other else character(0))
    expect_equal(row[[m$col]], 1L, info = m$col)
    expect_equal(row$has_ci, 1L, info = m$col)
    expect_equal(row$ci_travis_only, 0L, info = m$col)
  }
})
