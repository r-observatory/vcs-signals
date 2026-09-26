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
  out <- c(out, community_columns(root_entries, github_entries, repo))
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

.dev_has <- function(repo, el) !is.null(repo) && el %in% names(repo)
.presence <- function(src) if (is.na(src)) NA_integer_ else as.integer(src %in% c("repo", "account_default"))

#' Where a community file GitHub shows comes from. A tree hit wins: GitHub ignores some
#' spellings (CONDUCT.md) and can show the owner's default over the repository's own file.
community_source <- function(tree_hit, repo, url_el) {
  if (tree_hit) return("repo")
  if (!.dev_has(repo, url_el)) return(NA_character_)
  url <- repo[[url_el]]
  if (is.na(url)) return("none")
  nwo <- if (.dev_has(repo, "name_with_owner")) repo$name_with_owner else NA_character_
  if (url_points_into(url, nwo)) return("repo")
  if (!is.na(nwo) && url_points_into(url, paste0(sub("/.*$", "", nwo), "/.github"))) return("account_default")
  NA_character_
}

#' Where a pull request template comes from; GitHub names the repository that holds it.
pr_template_source <- function(tree_hit, repo) {
  if (tree_hit) return("repo")
  if (!.dev_has(repo, "pr_templates")) return(NA_character_)
  held <- tolower(repo$pr_templates$repository)
  nwo <- if (.dev_has(repo, "name_with_owner")) tolower(repo$name_with_owner) else NA_character_
  if (!is.na(nwo) && any(held == nwo, na.rm = TRUE)) return("repo")
  if (!is.na(nwo) && any(held == paste0(sub("/.*$", "", nwo), "/.github"), na.rm = TRUE)) return("account_default")
  if (!nrow(repo$pr_templates)) return("none")
  NA_character_
}

community_columns <- function(root_entries, github_entries, repo) {
  hay <- c(root_entries, github_entries)
  coc <- community_source(any(COC_TREE_PATHS %in% hay), repo, "coc_url")
  con <- community_source(any(CONTRIBUTING_TREE_PATHS %in% hay), repo, "contributing_url")
  prt <- pr_template_source(any(PR_TEMPLATE_TREE_PATHS %in% hay), repo)
  list(has_code_of_conduct = .presence(coc), coc_source = coc,
       has_contributing = .presence(con), contributing_source = con,
       has_pr_template = .presence(prt), pr_template_source = prt)
}
