# scripts/dev_tooling.R - pure rules that turn one repository's contents read into
# vcs_dev_tooling columns. No I/O.

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

#' TRUE when a github.com url's path sits inside the repository `slug` (owner/name), without case.
url_points_into <- function(url, slug) {
  if (length(url) != 1L || is.na(url) || length(slug) != 1L || is.na(slug)) return(FALSE)
  path <- sub("^[A-Za-z][A-Za-z0-9+.-]*://[^/]*", "", url)
  startsWith(tolower(path), paste0("/", tolower(slug), "/"))
}
