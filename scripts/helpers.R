# scripts/helpers.R - pure resolver + repo-model helpers for vcs-signals.
# Depends on constants from scripts/config.R. No network. The only side effect is
# write_repo_tables (SQLite). Functions are added task-by-task below.

# ---- URL splitting ---------------------------------------------------------
split_urls <- function(field) {
  if (is.null(field) || length(field) == 0) return(character(0))
  x <- field[[1]]
  if (is.na(x) || !nzchar(trimws(x))) return(character(0))
  parts <- trimws(strsplit(x, "[,[:space:]]+")[[1]])
  parts <- sub("^<+", "", parts)
  parts <- sub(">+$", "", parts)
  parts[nzchar(parts)]
}

# ---- internal URL helpers --------------------------------------------------
.domain_of <- function(url) {
  m <- regmatches(url, regexec("^[a-z0-9+.-]+://([^/]+)(/.*)?$", url, ignore.case = TRUE))[[1]]
  if (length(m) < 2) return(list(domain = NA_character_, path = ""))
  host <- sub(":.*$", "", sub("^[^@]*@", "", m[2]))          # strip userinfo and port
  host <- tolower(sub("^www\\.", "", host))
  path <- if (length(m) >= 3) sub("[?#].*$", "", m[3]) else ""
  list(domain = host, path = path)
}

.looks_like_forge <- function(domain) {
  labels <- strsplit(domain, ".", fixed = TRUE)[[1]]
  if (any(labels %in% FORGE_LABEL_TOKENS)) return(TRUE)
  any(vapply(FORGE_SUBSTRINGS, function(s) grepl(s, domain, fixed = TRUE), logical(1)))
}

.is_denied <- function(domain) {
  if (domain %in% NON_REPO_DENYLIST) return(TRUE)
  any(vapply(NON_REPO_SUFFIXES, function(s) endsWith(domain, s), logical(1)))
}

.path_segs <- function(path) {
  segs <- strsplit(path, "/", fixed = TRUE)[[1]]
  segs[nzchar(segs)]
}

# ---- VCS URL parsing -------------------------------------------------------
parse_vcs_url <- function(u) {
  if (is.null(u) || length(u) == 0 || is.na(u) || !nzchar(trimws(u))) return(NULL)
  s <- sub(">+$", "", sub("^<+", "", trimws(u)))
  ssh <- regmatches(s, regexec("^git@([^:]+):(.+)$", s))[[1]]
  if (length(ssh) == 3) {
    domain <- tolower(ssh[2]); path <- paste0("/", ssh[3])
  } else {
    s <- sub("^git\\+", "", s, ignore.case = TRUE)
    if (grepl("^//", s)) s <- paste0("https:", s)
    if (!grepl("^[a-z0-9+.-]+://", s, ignore.case = TRUE)) {
      if (grepl("^[a-z0-9.-]+\\.[a-z]{2,}(/|$)", tolower(s))) s <- paste0("https://", s) else return(NULL)
    }
    dp <- .domain_of(s); domain <- dp$domain; path <- dp$path
  }
  if (is.na(domain) || !nzchar(domain)) return(NULL)
  domain <- sub("^www\\.", "", tolower(domain))
  if (endsWith(domain, PAGES_SUFFIX)) return(NULL)            # pages handled by parse_pages_url

  host <- unname(KNOWN_FORGES[domain])
  if (is.na(host)) {
    if (.is_denied(domain)) return(NULL)
    if (.looks_like_forge(domain)) host <- "other" else return(NULL)
  }

  if (host == "gitlab") {
    p <- sub("/-/.*$", "", path)
    p <- sub("/(issues|merge_requests|wiki)(/.*)?$", "", p)
    segs <- .path_segs(p)
    if (length(segs) < 2) return(NULL)
    name <- sub("\\.git$", "", segs[length(segs)])
    owner <- paste(segs[-length(segs)], collapse = "/")
  } else {
    segs <- .path_segs(path)
    if (length(segs) < 2) return(NULL)
    owner <- segs[1]; name <- sub("\\.git$", "", segs[2])
  }
  if (!nzchar(owner) || !nzchar(name)) return(NULL)
  list(host = host, host_domain = domain, owner = owner, name = name)
}

# ---- github.io pages last-resort ------------------------------------------
parse_pages_url <- function(u) {
  if (is.null(u) || length(u) == 0 || is.na(u) || !nzchar(trimws(u))) return(NULL)
  s <- sub(">+$", "", sub("^<+", "", trimws(u)))
  if (!grepl("^[a-z0-9+.-]+://", s, ignore.case = TRUE)) {
    if (grepl("^[a-z0-9.-]+\\.[a-z]{2,}(/|$)", tolower(s))) s <- paste0("https://", s) else return(NULL)
  }
  dp <- .domain_of(s); domain <- dp$domain
  if (is.na(domain) || !endsWith(domain, PAGES_SUFFIX)) return(NULL)
  owner <- sub("\\.github\\.io$", "", domain)
  if (!nzchar(owner)) return(NULL)
  segs <- .path_segs(dp$path)
  name <- if (length(segs) >= 1) sub("\\.git$", "", segs[1]) else paste0(owner, ".github.io")
  list(host = "github", host_domain = "github.com", owner = owner, name = name)
}

# ---- mirror exclusion ------------------------------------------------------
is_mirror <- function(host, owner, name, host_domain) {
  if (!is.null(host_domain) && host_domain %in% MIRROR_DOMAINS) return(TRUE)
  if (!is.null(host) && host == "github" && tolower(owner) %in% MIRROR_GITHUB_OWNERS) return(TRUE)
  FALSE
}

# ---- slug + per-package resolution ----------------------------------------
repo_slug <- function(host_domain, owner, name) {
  tolower(paste(host_domain, owner, name, sep = "/"))
}

.as_repo_row <- function(p, resolved_from) {
  data.frame(host = p$host, host_domain = p$host_domain, owner = p$owner,
             name = p$name, resolved_from = resolved_from, stringsAsFactors = FALSE)
}

resolve_repo_for_package <- function(url_raw, bugreports_raw) {
  gather <- function(field, tag) {
    out <- list()
    for (u in split_urls(field)) {
      p <- parse_vcs_url(u)
      if (!is.null(p) && !is_mirror(p$host, p$owner, p$name, p$host_domain)) {
        p$resolved_from <- tag; out[[length(out) + 1L]] <- p
      }
    }
    out
  }
  url_c <- gather(url_raw, "url")
  bug_c <- gather(bugreports_raw, "bugreports")
  direct <- c(url_c, bug_c)

  if (length(direct) == 0) {
    pages <- c(lapply(split_urls(url_raw), parse_pages_url),
               lapply(split_urls(bugreports_raw), parse_pages_url))
    pages <- Filter(function(p) !is.null(p) && !is_mirror(p$host, p$owner, p$name, p$host_domain), pages)
    if (length(pages) == 0) return(NULL)
    return(.as_repo_row(pages[[1]], "pages"))
  }

  url_slugs <- vapply(url_c, function(p) repo_slug(p$host_domain, p$owner, p$name), "")
  bug_slugs <- vapply(bug_c, function(p) repo_slug(p$host_domain, p$owner, p$name), "")
  both <- intersect(url_slugs, bug_slugs)
  if (length(both) > 0) {
    p <- url_c[[which(url_slugs == both[1])[1]]]
    return(.as_repo_row(p, "both"))
  }
  gh <- Filter(function(p) p$host == "github", direct)
  p <- if (length(gh) > 0) gh[[1]] else direct[[1]]
  .as_repo_row(p, p$resolved_from)
}
