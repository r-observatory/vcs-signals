# scripts/hosts.R - SQL filters on a host column. The GitHub collectors pass GITHUB_HOSTS (config.R).

# SQL fragment matching rows whose host column is one of `hosts`.
host_predicate <- function(hosts, col = "host") {
  q <- vapply(hosts, function(h) paste0("'", gsub("'", "''", h, fixed = TRUE), "'"), "")
  sprintf("%s IN (%s)", col, paste(q, collapse = ", "))
}
