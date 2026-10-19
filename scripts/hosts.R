# scripts/hosts.R - which hosts the pipeline collects. Depends on SUPPORTED_HOSTS (config.R).

# SQL fragment matching rows whose host column is one of the supported hosts.
supported_host_predicate <- function(col = "host", hosts = SUPPORTED_HOSTS) {
  q <- vapply(hosts, function(h) paste0("'", gsub("'", "''", h, fixed = TRUE), "'"), "")
  sprintf("%s IN (%s)", col, paste(q, collapse = ", "))
}
