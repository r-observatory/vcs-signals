test_that("print_coverage reports denylist-drop and mirror-exclusion counts", {
  input <- data.frame(
    package = c("p1", "p2", "p3", "p4"),
    origin  = c("cran", "cran", "bioc", "bioc"),
    url_raw = c("https://github.com/tidyverse/ggplot2",
                "https://doi.org/10.1/x",
                "https://github.com/cran/foo",
                NA),
    bugreports_raw = c(NA, NA, "https://doi.org/10.2/y", NA),
    stringsAsFactors = FALSE)
  resolved <- resolve_all(input)
  idx <- build_repo_index(resolved)

  out <- capture.output(print_coverage(input, resolved, idx))
  denied_line <- grep("candidates dropped \\(denylist\\)", out, value = TRUE)
  mirror_line <- grep("candidates excluded \\(mirror\\)", out, value = TRUE)

  expect_length(denied_line, 1)
  expect_length(mirror_line, 1)
  expect_match(denied_line, "2$")
  expect_match(mirror_line, "1$")
})
