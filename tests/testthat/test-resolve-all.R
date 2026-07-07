test_that("resolve_all resolves an input frame and drops non-repo packages", {
  input <- data.frame(
    package = c("ggplot2", "nourl", "tximeta"),
    origin  = c("cran", "cran", "bioc"),
    url_raw = c("https://github.com/tidyverse/ggplot2",
                "https://doi.org/10.1/x",
                "https://thelovelab.github.io/tximeta, https://github.com/thelovelab/tximeta"),
    bugreports_raw = c("https://github.com/tidyverse/ggplot2/issues", NA, NA),
    stringsAsFactors = FALSE)
  resolved <- resolve_all(input)
  expect_equal(nrow(resolved), 2)
  expect_setequal(resolved$package, c("ggplot2", "tximeta"))
  expect_equal(resolved$resolved_from[resolved$package == "ggplot2"], "both")
})

test_that("resolve_all -> build_repo_index -> write_repo_tables is a clean end-to-end", {
  input <- data.frame(
    package = c("ggplot2", "scales"), origin = c("cran", "cran"),
    url_raw = c("https://github.com/tidyverse/ggplot2", "https://github.com/r-lib/scales"),
    bugreports_raw = c(NA, NA), stringsAsFactors = FALSE)
  idx <- build_repo_index(resolve_all(input))
  con <- new_test_db(); on.exit(DBI::dbDisconnect(con))
  write_repo_tables(con, idx$repos, idx$repo_packages, "2026-07-06")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM repos")$n, 2)
  expect_true("github.com/r-lib/scales" %in% DBI::dbGetQuery(con, "SELECT repo_id FROM repos")$repo_id)
})
