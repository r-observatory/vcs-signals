# scripts/dev_tooling.R - pure rules that turn one repository's contents read into
# vcs_dev_tooling columns. No I/O.

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

#' TRUE when a github.com url's path sits inside the repository `slug` (owner/name), without case.
url_points_into <- function(url, slug) {
  if (length(url) != 1L || is.na(url) || length(slug) != 1L || is.na(slug)) return(FALSE)
  path <- sub("^[A-Za-z][A-Za-z0-9+.-]*://[^/]*", "", url)
  startsWith(tolower(path), paste0("/", tolower(slug), "/"))
}

#' Every derived vcs_dev_tooling column classify_dev_tooling does not compute itself.
#' `flags` is the named vector of tree-rule columns; `repo` the parsed contents read or NULL.
dev_tooling_derive <- function(root_entries, github_entries, flags, repo) {
  out <- tree_derived_columns(root_entries, flags)
  out
}

#' Columns read from the tree alone, so they need no contents read beyond the listing.
tree_derived_columns <- function(root_entries, flags) {
  others <- c("ci_github_actions", "ci_gitlab", "ci_appveyor", "ci_circleci", "ci_tic",
              "ci_jenkins", "ci_azure", "ci_drone")
  list(package_at_root = as.integer("DESCRIPTION" %in% root_entries),
       ci_travis_only = as.integer(flags[["ci_travis"]] == 1L && all(flags[others] == 0L)))
}

#' Rule paths with a "/" whose folder is not a subtree the contents query lists there. The query
#' lists one level of each subtree, so a path deeper inside one can never match either.
unfetched_rule_paths <- function(paths, location, prefixes = tree_subtree_prefixes()) {
  nested <- paths[grepl("/", paths, fixed = TRUE)]
  allowed <- switch(location, root = prefixes$root, github = prefixes$github,
                    both = c(prefixes$root, prefixes$github))
  nested[!sub("/[^/]*$", "", nested) %in% allowed]
}
