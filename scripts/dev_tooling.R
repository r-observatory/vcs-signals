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
  rbi <- rbuildignore_columns(root_entries, repo)
  out <- c(out, rbi)
  out <- c(out, workflow_columns(github_entries, repo))
  out <- c(out, pages_columns(repo))
  site <- pkgdown_site_columns(repo)
  out <- c(out, list(site_generator = site_generator_for(site$site_pkgdown_source, flags,
                                                          .dev_has(repo, "pkgdown_yml"))), site)
  out <- c(out, repo_desc_fields(repo), repo_scalar_columns(repo))
  attr(out, "rbuildignore_bad_lines") <- attr(rbi, "bad_lines")
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

#' Which `paths` a .Rbuildignore text leaves out, as tools:::inRbuildignore reads it: split like
#' readLines, not trimmed, empty lines dropped, each line a Perl pattern without case.
rbuildignore_path_matches <- function(text, paths) {
  lines <- strsplit(text, "\r\n|\r|\n")[[1]]
  lines <- lines[nzchar(lines)]
  hit <- rep(FALSE, length(paths))
  bad <- 0L
  for (line in lines) {
    # R CMD build stops on a line that does not compile; the scan skips it and counts it.
    m <- tryCatch(suppressWarnings(grepl(line, paths, perl = TRUE, ignore.case = TRUE)),
                  error = function(e) NULL)
    if (is.null(m)) { bad <- bad + 1L; next }
    hit <- hit | m
  }
  list(hit = hit, bad_lines = bad)
}

#' The RBUILDIGNORE_ITEMS the repository holds and R CMD build would leave out, as a JSON array;
#' NA when the package is not at the root or the file is present but its text was not returned.
rbuildignore_excluded <- function(text, root_entries) {
  if (!("DESCRIPTION" %in% root_entries)) return(structure(NA_character_, bad_lines = 0L))
  if (!(".Rbuildignore" %in% root_entries)) return(structure("[]", bad_lines = 0L))
  if (is.na(text)) return(structure(NA_character_, bad_lines = 0L))
  present <- function(item) switch(item,
    "_pkgdown.yml"       = intersect(PKGDOWN_CONFIG_TREE_PATHS, root_entries),
    "CODE_OF_CONDUCT.md" = intersect(COC_TREE_PATHS, root_entries),
    "CONTRIBUTING.md"    = intersect(CONTRIBUTING_TREE_PATHS, root_entries),
    intersect(item, root_entries))
  item_paths <- lapply(stats::setNames(RBUILDIGNORE_ITEMS, RBUILDIGNORE_ITEMS), present)
  sources <- root_entries[startsWith(root_entries, "vignettes/") &
                          grepl(VIGNETTE_SOURCE_PATTERN, root_entries, ignore.case = TRUE)]
  # A path is left out when it or a directory above it matches: R CMD build drops the directory.
  above <- function(p) { parts <- strsplit(p, "/", fixed = TRUE)[[1]]
    if (length(parts) < 2L) character(0) else vapply(seq_len(length(parts) - 1L),
      function(k) paste(parts[seq_len(k)], collapse = "/"), character(1)) }
  tested <- unique(c(unlist(item_paths), sources, unlist(lapply(c(unlist(item_paths), sources), above))))
  m <- rbuildignore_path_matches(text, tested)
  gone <- function(p) any(m$hit[tested %in% c(p, above(p))])
  out <- vapply(RBUILDIGNORE_ITEMS, function(item) {
    ps <- item_paths[[item]]
    if (!length(ps)) return(FALSE)
    if (any(vapply(ps, gone, logical(1)))) return(TRUE)
    # vignettes also counts when every vignette source is left out, even if a .bib survives.
    identical(item, "vignettes") && length(sources) > 0L && all(vapply(sources, gone, logical(1)))
  }, logical(1))
  structure(as.character(jsonlite::toJSON(unname(RBUILDIGNORE_ITEMS[out]))), bad_lines = m$bad_lines)
}

rbuildignore_columns <- function(root_entries, repo) {
  if (!.dev_has(repo, "rbuildignore_text"))
    return(structure(list(rbuildignore_excluded = NA_character_, rbuildignore_text = NA_character_),
                     bad_lines = 0L))
  txt <- repo$rbuildignore_text
  ex <- rbuildignore_excluded(txt, root_entries)
  keep <- if (!is.na(txt) && nchar(txt, type = "bytes") <= RBUILDIGNORE_TEXT_MAX_BYTES) txt else NA_character_
  structure(list(rbuildignore_excluded = as.vector(ex), rbuildignore_text = keep),
            bad_lines = attr(ex, "bad_lines"))
}

#' What the GitHub Actions workflows run. A NULL `workflows` element means the directory
#' was read and is absent, which reads 0; a repo without the element reads NA.
workflow_columns <- function(github_entries, repo) {
  cols <- c("ci_workflow_files", "ci_rcmdcheck", "ci_platforms", "ci_r_devel", "ci_coverage",
            "ci_site_deploy", "ci_lint")
  if (!.dev_has(repo, "workflows")) return(stats::setNames(as.list(rep(NA, length(cols))), cols))
  rules <- WORKFLOW_TEXT_RULES
  names <- sub("^workflows/", "", github_entries[startsWith(github_entries, "workflows/")])
  yml <- names[grepl("\\.ya?ml$", names, ignore.case = TRUE)]
  wf <- repo$workflows
  texts <- if (is.null(wf)) character(0) else wf$text[wf$name %in% yml]
  textless <- setdiff(yml, if (is.null(wf)) character(0) else wf$name)
  any_text <- function(pattern, t = texts) length(t) > 0L && any(grepl(pattern, t, perl = TRUE))
  check_texts <- texts[grepl(rules$rcmdcheck, texts, perl = TRUE)]
  rcmd <- length(check_texts) > 0L || any(grepl(rules$rcmdcheck_name, textless, perl = TRUE))
  platforms <- NA_character_
  r_devel <- NA_integer_
  if (rcmd) {
    found <- unlist(regmatches(check_texts, gregexpr(rules$platforms, check_texts, perl = TRUE)))
    seen <- unique(sub("-.*$", "", tolower(found)))
    family <- c(linux = "ubuntu", macos = "macos", windows = "windows")
    named <- names(family)[family %in% seen]
    if (length(named)) platforms <- as.character(jsonlite::toJSON(named))
    r_devel <- as.integer(any_text(rules$r_devel, check_texts))
  }
  list(ci_workflow_files = as.character(jsonlite::toJSON(yml)),
       ci_rcmdcheck = as.integer(rcmd),
       ci_platforms = platforms,
       ci_r_devel = r_devel,
       ci_coverage = as.integer(any_text(rules$coverage)),
       ci_site_deploy = as.integer(any_text(rules$site_deploy)),
       ci_lint = as.integer(any_text(rules$lint, texts[!grepl(rules$lint_skip, texts, fixed = TRUE)])))
}

#' GitHub Pages: whether a site is deployed, when, and where. environmentUrl goes stale after a
#' transfer, so a github.io url is rebuilt from the repository's current name.
pages_columns <- function(repo) {
  none <- list(has_pages = NA_integer_, pages_last_deploy = NA_character_, pages_url = NA_character_)
  if (!.dev_has(repo, "pages")) return(none)
  p <- repo$pages
  has <- as.integer("github-pages" %in% p$environments || !is.na(p$last_deploy_at))
  url <- NA_character_
  if (has == 1L) {
    env <- p$environment_url
    host <- if (is.na(env) || !nzchar(env)) NA_character_
            else tolower(sub("^[A-Za-z][A-Za-z0-9+.-]*://([^/:]+).*$", "\\1", env))
    nwo <- if (.dev_has(repo, "name_with_owner")) repo$name_with_owner else NA_character_
    if (!is.na(host) && grepl("^https?://", env, ignore.case = TRUE) && !endsWith(host, ".github.io")) {
      url <- env
    } else if (!is.na(nwo)) {
      owner <- tolower(sub("/.*$", "", nwo)); name <- sub("^[^/]*/", "", nwo)
      url <- if (identical(tolower(name), paste0(owner, ".github.io"))) sprintf("https://%s.github.io/", owner)
             else sprintf("https://%s.github.io/%s/", owner, name)
    }
  }
  list(has_pages = has, pages_last_deploy = if (has == 1L) p$last_deploy_at else NA_character_,
       pages_url = url)
}

#' What a built pkgdown site's pkgdown.yml says: where it was found, the pkgdown version,
#' when it was built, and the site's base url. Regexes only, no YAML parser.
pkgdown_site_columns <- function(repo) {
  out <- list(site_pkgdown_source = NA_character_, site_pkgdown_version = NA_character_,
              site_pkgdown_last_built = NA_character_, site_url = NA_character_)
  if (!.dev_has(repo, "pkgdown_yml")) return(out)
  y <- repo$pkgdown_yml
  src <- if (!is.na(y[["gh_pages"]])) "gh-pages" else if (!is.na(y[["docs"]])) "docs" else return(out)
  txt <- if (src == "gh-pages") y[["gh_pages"]] else y[["docs"]]
  first <- function(pattern) {
    m <- regmatches(txt, regexec(pattern, txt, perl = TRUE))[[1]]
    if (length(m) >= 2L) m[2] else NA_character_
  }
  lines <- strsplit(txt, "\r\n|\r|\n")[[1]]
  at <- which(grepl("^urls:\\s*$", lines))
  block <- character(0)
  if (length(at)) {
    rest <- lines[-seq_len(at[1])]
    stop_at <- which(!grepl("^\\s+", rest))
    block <- if (length(stop_at)) rest[seq_len(stop_at[1] - 1L)] else rest
  }
  url_of <- function(key, tail) {
    l <- grep(sprintf("^\\s+%s:", key), block, value = TRUE)
    if (!length(l)) return(NA_character_)
    v <- gsub("[\"']", "", trimws(sub(sprintf("^\\s+%s:", key), "", l[1])))
    if (nzchar(v)) sub(tail, "", v) else NA_character_
  }
  site <- url_of("reference", "/reference/?$")
  if (is.na(site)) site <- url_of("article", "/articles/?$")
  list(site_pkgdown_source = src,
       site_pkgdown_version = first("(?m)^pkgdown:\\s*['\"]?([0-9]+(\\.[0-9]+)*)"),
       site_pkgdown_last_built = first("(?m)^last_built:\\s*['\"]?([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}(:[0-9]{2})?Z?)"),
       site_url = site)
}

#' The tool that builds the site: a deployed pkgdown.yml first, then a chosen tool's config
#' (altdoc and litedown before pkgdown), Quarto last, else unknown.
site_generator_for <- function(pkgdown_source, flags, read) {
  if (!read) return(NA_character_)
  if (!is.na(pkgdown_source)) return("pkgdown")
  for (tool in c("altdoc", "litedown", "pkgdown", "quarto"))
    if (identical(unname(flags[[paste0("has_", tool)]]), 1L)) return(tool)
  "unknown"
}

#' Package and Version of the default branch's DESCRIPTION; NA when absent or unreadable.
repo_desc_fields <- function(repo) {
  out <- list(repo_desc_package = NA_character_, repo_desc_version = NA_character_)
  txt <- if (.dev_has(repo, "desc_text")) repo$desc_text else NA_character_
  if (is.na(txt)) return(out)
  d <- tryCatch(suppressWarnings(read.dcf(textConnection(txt), fields = c("Package", "Version"))),
                error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(out)
  val <- function(f) { v <- trimws(d[1, f]); if (is.na(v) || !nzchar(v)) NA_character_ else v }
  list(repo_desc_package = val("Package"), repo_desc_version = val("Version"))
}

#' Fork, support and funding facts GitHub reports for the repository itself.
repo_scalar_columns <- function(repo) {
  pick <- function(el, f) if (.dev_has(repo, el)) f(repo[[el]]) else NA
  list(
    funding_links = pick("funding_links", function(x) as.character(jsonlite::toJSON(x))),
    owner_sponsorable = pick("owner_sponsorable", as.integer),
    is_fork = pick("is_fork", as.integer),
    parent_name_with_owner = pick("parent", as.character),
    has_issues_enabled = pick("has_issues_enabled", as.integer),
    homepage_url = pick("homepage_url", function(x) { x <- trimws(x); if (is.na(x) || !nzchar(x)) NA_character_ else x }),
    has_discussions = pick("has_discussions", as.integer),
    discussions_total = pick("discussions_total", as.integer))
}

#' The default branch's version against CRAN's, only when the DESCRIPTION's Package is one of
#' the repository's CRAN packages (case-sensitive). `cran_links` is data.frame(package, cran_version).
compare_repo_version <- function(repo_desc_package, repo_desc_version, cran_links) {
  none <- list(cran_version_at_scan = NA_character_, repo_version_vs_cran = NA_character_)
  if (is.na(repo_desc_package) || is.na(repo_desc_version) || is.null(cran_links) || !nrow(cran_links))
    return(none)
  cv <- cran_links$cran_version[cran_links$package == repo_desc_package]
  if (!length(cv) || is.na(cv[1])) return(none)
  cmp <- tryCatch(suppressWarnings(utils::compareVersion(repo_desc_version, cv[1])), error = function(e) NA)
  list(cran_version_at_scan = cv[1],
       repo_version_vs_cran = if (is.na(cmp)) NA_character_ else c("behind", "equal", "ahead")[cmp + 2L])
}
