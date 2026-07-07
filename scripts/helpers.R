# scripts/helpers.R - pure resolver + repo-model helpers for vcs-signals SP1.
# Depends on constants from scripts/config.R. No network. The only side effect is write_repo_tables (SQLite).
# Functions are added task-by-task below.

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

# Shared scheme/ssh/bare-domain normalization: given a raw candidate URL string,
# returns list(domain, path) with domain lowercased and www-stripped, or
# list(domain = NA_character_, path = "") if it cannot be normalized to a domain.
.url_domain_path <- function(u) {
  empty <- list(domain = NA_character_, path = "")
  if (is.null(u) || length(u) == 0 || is.na(u) || !nzchar(trimws(u))) return(empty)
  s <- sub(">+$", "", sub("^<+", "", trimws(u)))
  ssh <- regmatches(s, regexec("^git@([^:]+):(.+)$", s))[[1]]
  if (length(ssh) == 3) {
    domain <- tolower(ssh[2]); path <- paste0("/", ssh[3])
  } else {
    s <- sub("^git\\+", "", s, ignore.case = TRUE)
    if (grepl("^//", s)) s <- paste0("https:", s)
    if (!grepl("^[a-z0-9+.-]+://", s, ignore.case = TRUE)) {
      if (grepl("^[a-z0-9.-]+\\.[a-z]{2,}(/|$)", tolower(s))) s <- paste0("https://", s) else return(empty)
    }
    dp <- .domain_of(s); domain <- dp$domain; path <- dp$path
  }
  if (is.na(domain) || !nzchar(domain)) return(empty)
  domain <- sub("^www\\.", "", tolower(domain))
  list(domain = domain, path = path)
}

# Domain-only accessor shared by parse_vcs_url and classify_url.
.url_domain <- function(u) .url_domain_path(u)$domain

# ---- VCS URL parsing -------------------------------------------------------
parse_vcs_url <- function(u) {
  dp <- .url_domain_path(u)
  domain <- dp$domain; path <- dp$path
  if (is.na(domain) || !nzchar(domain)) return(NULL)
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

# ---- URL classification (for coverage reporting) ---------------------------
# One of "repo" (parsed and not a mirror), "mirror" (parsed but a read-only
# mirror), "denied" (rejected by the non-repo denylist), or "other" (anything
# else: malformed, non-VCS, NA/empty).
classify_url <- function(u) {
  p <- parse_vcs_url(u)
  if (!is.null(p)) {
    if (is_mirror(p$host, p$owner, p$name, p$host_domain)) return("mirror")
    return("repo")
  }
  domain <- .url_domain(u)
  if (!is.na(domain) && .is_denied(domain)) return("denied")
  "other"
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

# ---- repo index ------------------------------------------------------------
build_repo_index <- function(resolved_df) {
  empty_repos <- data.frame(repo_id = character(), host = character(), host_domain = character(),
    owner = character(), name = character(), name_with_owner = character(),
    supported = integer(), n_packages = integer(), stringsAsFactors = FALSE)
  empty_rp <- data.frame(repo_id = character(), package = character(), origin = character(),
    resolved_from = character(), stringsAsFactors = FALSE)
  if (is.null(resolved_df) || nrow(resolved_df) == 0) return(list(repos = empty_repos, repo_packages = empty_rp))

  slug <- repo_slug(resolved_df$host_domain, resolved_df$owner, resolved_df$name)
  first <- !duplicated(slug)
  repos <- data.frame(
    repo_id = slug[first], host = resolved_df$host[first], host_domain = resolved_df$host_domain[first],
    owner = resolved_df$owner[first], name = resolved_df$name[first],
    name_with_owner = paste(resolved_df$owner[first], resolved_df$name[first], sep = "/"),
    supported = as.integer(resolved_df$host[first] %in% SUPPORTED_HOSTS),
    stringsAsFactors = FALSE)
  pk <- paste(resolved_df$package, resolved_df$origin, sep = "\r")
  n <- tapply(pk, slug, function(v) length(unique(v)))
  repos$n_packages <- as.integer(n[repos$repo_id])
  repo_packages <- unique(data.frame(repo_id = slug, package = resolved_df$package,
    origin = resolved_df$origin, resolved_from = resolved_df$resolved_from, stringsAsFactors = FALSE))
  list(repos = repos, repo_packages = repo_packages)
}

# ---- schema + persistence --------------------------------------------------
ensure_repo_schema <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS repos (
    repo_id TEXT PRIMARY KEY, node_id TEXT, host TEXT NOT NULL, host_domain TEXT NOT NULL,
    owner TEXT NOT NULL, name TEXT NOT NULL, name_with_owner TEXT NOT NULL,
    supported INTEGER NOT NULL, n_packages INTEGER NOT NULL,
    first_seen TEXT NOT NULL, last_seen TEXT NOT NULL, status TEXT NOT NULL DEFAULT 'active')")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS repo_packages (
    repo_id TEXT NOT NULL, package TEXT NOT NULL, origin TEXT NOT NULL, resolved_from TEXT NOT NULL,
    PRIMARY KEY (repo_id, package, origin))")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_rp_package ON repo_packages(package)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_repos_host ON repos(host)")
  invisible(TRUE)
}

write_repo_tables <- function(con, repos_df, repo_packages_df, today) {
  ensure_repo_schema(con)
  existing <- DBI::dbGetQuery(con, "SELECT repo_id, status FROM repos")
  DBI::dbBegin(con)
  ok <- FALSE
  on.exit(if (!ok) tryCatch(DBI::dbRollback(con), error = function(e) NULL), add = TRUE)
  for (i in seq_len(nrow(repos_df))) {
    r <- repos_df[i, ]
    ex <- existing[existing$repo_id == r$repo_id, ]
    if (nrow(ex) == 0) {
      DBI::dbExecute(con, "INSERT INTO repos
        (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?)",
        params = list(r$repo_id, NA_character_, r$host, r$host_domain, r$owner, r$name, r$name_with_owner,
                      r$supported, r$n_packages, today, today, "active"))
    } else {
      new_status <- if (ex$status %in% c("gone", "moved")) ex$status else "active"
      DBI::dbExecute(con, "UPDATE repos SET
        host=?,host_domain=?,owner=?,name=?,name_with_owner=?,supported=?,n_packages=?,last_seen=?,status=?
        WHERE repo_id=?",
        params = list(r$host, r$host_domain, r$owner, r$name, r$name_with_owner, r$supported,
                      r$n_packages, today, new_status, r$repo_id))
    }
  }
  gone <- setdiff(existing$repo_id, repos_df$repo_id)
  for (id in gone) {
    DBI::dbExecute(con, "UPDATE repos SET status='retired' WHERE repo_id=? AND status NOT IN ('gone')",
                   params = list(id))
  }
  DBI::dbExecute(con, "DELETE FROM repo_packages")
  if (nrow(repo_packages_df) > 0) {
    DBI::dbExecute(con, "INSERT INTO repo_packages (repo_id,package,origin,resolved_from) VALUES (?,?,?,?)",
      params = list(repo_packages_df$repo_id, repo_packages_df$package,
                    repo_packages_df$origin, repo_packages_df$resolved_from))
  }
  DBI::dbCommit(con); ok <- TRUE
  invisible(TRUE)
}

#' Persist resolve_node_ids' lifecycle frame (repo_id, node_id, owner, name,
#' name_with_owner, status) onto the repos dimension: an UPSERT keyed on the
#' frozen repo_id (never re-derived), covering both a fresh id attachment, a
#' rename (owner/name/name_with_owner updated), and a gone repo (node_id
#' NA, status='gone').
update_repo_node_ids <- function(con, resolved) {
  if (is.null(resolved) || nrow(resolved) == 0) return(invisible(TRUE))
  DBI::dbBegin(con)
  DBI::dbExecute(con,
    "UPDATE repos SET node_id=?, owner=?, name=?, name_with_owner=?, status=? WHERE repo_id=?",
    params = list(resolved$node_id, resolved$owner, resolved$name,
                  resolved$name_with_owner, resolved$status, resolved$repo_id))
  DBI::dbCommit(con)
  invisible(TRUE)
}

# ---- universe guard --------------------------------------------------------
universe_guard <- function(prev_pkgs, prev_repos, curr_pkgs, curr_repos, threshold = 0.10) {
  if (is.null(prev_pkgs) || is.na(prev_pkgs) || prev_pkgs == 0) return(invisible(TRUE))
  if (curr_pkgs < prev_pkgs * (1 - threshold))
    stop(sprintf("universe guard: resolved packages dropped %d -> %d (> %.0f%%); aborting",
                 prev_pkgs, curr_pkgs, threshold * 100))
  if (!is.null(prev_repos) && !is.na(prev_repos) && prev_repos > 0 &&
      curr_repos < prev_repos * (1 - threshold))
    stop(sprintf("universe guard: unique repos dropped %d -> %d (> %.0f%%); aborting",
                 prev_repos, curr_repos, threshold * 100))
  invisible(TRUE)
}

# ---- orchestration helpers (pure) -----------------------------------------
resolve_all <- function(input) {
  rows <- list()
  for (i in seq_len(nrow(input))) {
    r <- resolve_repo_for_package(input$url_raw[i], input$bugreports_raw[i])
    if (!is.null(r)) { r$package <- input$package[i]; r$origin <- input$origin[i]
      rows[[length(rows) + 1L]] <- r }
  }
  if (length(rows) == 0)
    return(data.frame(host = character(), host_domain = character(), owner = character(),
      name = character(), resolved_from = character(), package = character(),
      origin = character(), stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

print_coverage <- function(input, resolved, idx) {
  cat("=== vcs-signals SP1 coverage (candidate repos: parsed, unvalidated) ===\n")
  for (org in c("cran", "bioc")) {
    tot <- sum(input$origin == org)
    res <- sum(resolved$origin == org)
    cat(sprintf("  %-5s: %d packages, %d resolved (%.1f%%)\n", org, tot, res, 100 * res / max(tot, 1)))
  }
  cat(sprintf("  unique repos: %d\n", nrow(idx$repos)))
  hb <- table(idx$repos$host)
  if (length(hb)) cat("  by host:", paste(sprintf("%s=%d", names(hb), as.integer(hb)), collapse = ", "), "\n")
  rf <- table(idx$repo_packages$resolved_from)
  if (length(rf)) cat("  resolved_from:", paste(sprintf("%s=%d", names(rf), as.integer(rf)), collapse = ", "), "\n")

  denied_n <- 0L; mirror_n <- 0L
  for (i in seq_len(nrow(input))) {
    cands <- c(split_urls(input$url_raw[i]), split_urls(input$bugreports_raw[i]))
    if (length(cands) == 0) next
    cls <- vapply(cands, classify_url, character(1))
    denied_n <- denied_n + sum(cls == "denied")
    mirror_n <- mirror_n + sum(cls == "mirror")
  }
  cat(sprintf("  candidates dropped (denylist): %d\n", denied_n))
  cat(sprintf("  candidates excluded (mirror): %d\n", mirror_n))
  invisible(NULL)
}

# ---- SP2 batching --------------------------------------------------------
chunk <- function(x, n) {
  if (length(x) == 0) return(list())
  split(x, ceiling(seq_along(x) / n))
}

# ---- SP2 series schema + materialization ----------------------------------
ensure_series_schema <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS signals_series (
    repo_id TEXT NOT NULL, date TEXT NOT NULL, metric TEXT NOT NULL, value INTEGER NOT NULL,
    PRIMARY KEY (repo_id, date, metric))")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_series_date ON signals_series(date)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS series_latest (
    repo_id TEXT NOT NULL, metric TEXT NOT NULL, value INTEGER NOT NULL,
    PRIMARY KEY (repo_id, metric))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS vcs_signals_summary (
    package TEXT NOT NULL, origin TEXT NOT NULL, repo_id TEXT,
    stars INTEGER, forks INTEGER, issues_open INTEGER, prs_open INTEGER,
    commits_total INTEGER, releases_total INTEGER, last_commit_date TEXT,
    license TEXT, topics TEXT, is_archived INTEGER, trend_30d REAL,
    first_seen TEXT, last_seen TEXT, PRIMARY KEY (package, origin))")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS pipeline_state (key TEXT PRIMARY KEY, value TEXT)")
  invisible(TRUE)
}

gauges_to_long <- function(snapshot, repo_map) {
  if (is.null(snapshot) || nrow(snapshot) == 0)
    return(data.frame(repo_id = character(), metric = character(), value = integer(), stringsAsFactors = FALSE))
  m <- merge(snapshot, repo_map[, c("node_id", "repo_id")], by = "node_id")
  out <- list()
  for (metric in FORWARD_METRICS) {
    if (!metric %in% names(m)) next
    v <- m[[metric]]
    keep <- !is.na(v)
    if (any(keep)) out[[metric]] <- data.frame(repo_id = m$repo_id[keep], metric = metric,
                                               value = as.integer(v[keep]), stringsAsFactors = FALSE)
  }
  if (length(out) == 0)
    return(data.frame(repo_id = character(), metric = character(), value = integer(), stringsAsFactors = FALSE))
  do.call(rbind, out)
}

materialize_series <- function(prev_latest, snapshot_long, date) {
  k <- function(rid, met) paste(rid, met, sep = "\r")
  prev <- if (nrow(prev_latest)) setNames(as.integer(prev_latest$value), k(prev_latest$repo_id, prev_latest$metric)) else integer(0)
  changed <- vapply(seq_len(nrow(snapshot_long)), function(i) {
    pv <- prev[k(snapshot_long$repo_id[i], snapshot_long$metric[i])]
    is.null(pv) || is.na(pv) || pv != snapshot_long$value[i]
  }, logical(1))
  r <- snapshot_long[changed, , drop = FALSE]
  list(series_rows = data.frame(repo_id = r$repo_id, date = rep(date, nrow(r)), metric = r$metric,
                                value = as.integer(r$value), stringsAsFactors = FALSE),
       new_latest = snapshot_long)
}

build_signals_summary <- function(latest, series, repos, repo_packages, today) {
  if (nrow(repo_packages) == 0)
    return(data.frame(package = character(), origin = character(), repo_id = character(),
      stars = integer(), forks = integer(), issues_open = integer(), prs_open = integer(),
      commits_total = integer(), releases_total = integer(), last_commit_date = character(),
      license = character(), topics = character(), is_archived = integer(), trend_30d = double(),
      first_seen = character(), last_seen = character(), stringsAsFactors = FALSE))
  val <- function(rid, met) {
    v <- latest$value[latest$repo_id == rid & latest$metric == met]
    if (length(v)) as.integer(v[1]) else NA_integer_
  }
  trend30 <- function(rid) {
    s <- series[series$repo_id == rid & series$metric == "stars", ]
    if (nrow(s) < 2) return(NA_real_)
    s <- s[order(s$date), ]
    cutoff <- as.character(as.Date(today) - 30)
    prior <- s$value[s$date <= cutoff]
    base <- if (length(prior)) prior[length(prior)] else s$value[1]
    now <- s$value[nrow(s)]
    if (is.na(base) || base == 0) return(NA_real_)
    (now - base) / base * 100
  }
  rows <- lapply(seq_len(nrow(repo_packages)), function(i) {
    rid <- repo_packages$repo_id[i]
    ra <- repos[repos$repo_id == rid, ]
    data.frame(package = repo_packages$package[i], origin = repo_packages$origin[i], repo_id = rid,
      stars = val(rid, "stars"), forks = val(rid, "forks"), issues_open = val(rid, "issues_open"),
      prs_open = val(rid, "prs_open"), commits_total = val(rid, "commits_total"),
      releases_total = val(rid, "releases_total"),
      last_commit_date = if (nrow(ra)) ra$last_commit_date[1] else NA_character_,
      license = if (nrow(ra)) ra$license[1] else NA_character_,
      topics = if (nrow(ra)) ra$topics[1] else NA_character_,
      is_archived = if (nrow(ra)) as.integer(ra$is_archived[1]) else NA_integer_,
      trend_30d = trend30(rid),
      first_seen = if (nrow(ra)) ra$first_seen[1] else NA_character_,
      last_seen = if (nrow(ra)) ra$last_seen[1] else NA_character_,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# ---- SP2 shard + manifest helpers (ported from cran-downloads) ------------

#' Extract all signals_series rows for a single year.
#'
#' @param con  SQLite connection (working DB with signals_series table)
#' @param year integer
#' @return data.frame(repo_id, date, metric, value)
extract_year_rows <- function(con, year) {
  year_prefix <- sprintf("%04d", as.integer(year))
  DBI::dbGetQuery(
    con,
    "SELECT repo_id, date, metric, value
       FROM signals_series
      WHERE substr(date, 1, 4) = ?
      ORDER BY repo_id, date, metric",
    params = list(year_prefix)
  )
}

#' Extract the rolling N-day window of signals_series rows.
#'
#' @param con         SQLite connection
#' @param today       Date - reference "now"
#' @param window_days integer - how many days back, inclusive of cutoff
#' @return data.frame(repo_id, date, metric, value)
extract_recent_rows <- function(con, today, window_days) {
  cutoff <- format(today - as.integer(window_days), "%Y-%m-%d")
  DBI::dbGetQuery(
    con,
    "SELECT repo_id, date, metric, value
       FROM signals_series
      WHERE date >= ?
      ORDER BY repo_id, date, metric",
    params = list(cutoff)
  )
}

#' Write the given rows into a fresh SQLite file at `path`.
#'
#' Overwrites any existing file. Always creates the signals_series table
#' with the canonical schema and idx_series_date index. Runs VACUUM at end
#' so the file is minimal.
export_series_shard <- function(path, rows) {
  if (file.exists(path)) unlink(path)

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")  # no WAL in published shards

  DBI::dbExecute(con, "CREATE TABLE signals_series (
    repo_id TEXT NOT NULL, date TEXT NOT NULL, metric TEXT NOT NULL, value INTEGER NOT NULL,
    PRIMARY KEY (repo_id, date, metric))")
  DBI::dbExecute(con, "CREATE INDEX idx_series_date ON signals_series(date)")

  if (nrow(rows) > 0) {
    DBI::dbBegin(con)
    DBI::dbExecute(
      con,
      "INSERT INTO signals_series (repo_id, date, metric, value) VALUES (?, ?, ?, ?)",
      params = list(rows$repo_id, rows$date, rows$metric, rows$value)
    )
    DBI::dbCommit(con)
  }

  DBI::dbExecute(con, "VACUUM")
  invisible(NULL)
}

#' Write a minimal SQLite file containing the vcs_signals_summary, repos,
#' and repo_packages tables - the published "summary" shard.
export_summary_shard <- function(path, summary_df, repos_df, repo_packages_df) {
  if (file.exists(path)) unlink(path)

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  ensure_series_schema(con)
  ensure_repo_schema(con)

  if (nrow(summary_df) > 0) DBI::dbWriteTable(con, "vcs_signals_summary", summary_df, append = TRUE)
  if (nrow(repos_df) > 0) DBI::dbWriteTable(con, "repos", repos_df, append = TRUE)
  if (nrow(repo_packages_df) > 0) DBI::dbWriteTable(con, "repo_packages", repo_packages_df, append = TRUE)

  DBI::dbExecute(con, "VACUUM")
  invisible(NULL)
}

#' Write the manifest.json describing which shards changed this run.
#'
#' Empty arrays are preserved (jsonlite default is to drop them - we force them).
write_manifest <- function(path, changed_shards, tag, summary) {
  obj <- list(
    tag            = tag,
    generated_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    changed_shards = as.list(changed_shards),
    summary        = summary
  )
  json <- jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(json, path)
}

# ---- SP2 publisher: change-gate, protect-history, heartbeat ---------------
# Ported from the bioconductor-downloads safety kit (scripts/helpers.R and
# scripts/update.R), adapted to this pipeline's shard names
# (vcs-signals-<YYYY>.db, vcs-signals-recent.db, vcs-signals-summary.db) and
# the signals_series schema (repo_id, date, metric, value).

#' Content hash of a single shard file, for change detection across runs.
#'
#' Base R only (no digest dependency): tools::md5sum(path). Returns NA when
#' the file does not exist, so a missing shard never crashes the change-gate.
shard_hash <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  unname(tools::md5sum(path))
}

#' Which shard basenames (present in curr_hashes) are added or modified
#' relative to prev_hashes. A name absent from prev, or present with a
#' different hash, counts as changed; an unchanged name is omitted so a
#' no-op run uploads nothing but the manifest.
changed_shards <- function(prev_hashes, curr_hashes) {
  nm <- names(curr_hashes)
  if (is.null(nm)) return(character(0))
  keep <- vapply(nm, function(n) {
    prior <- unname(prev_hashes[n])
    is.na(prior) || !identical(prior, unname(curr_hashes[n]))
  }, logical(1))
  nm[keep]
}

#' Pull the prior release's manifest, recent shard, and year shards into
#' `dir`, so the change-gate has something to hash against and accumulated
#' history is never silently discarded by a bad run. A no-op (returns
#' character(0)) when no release exists yet (first ever run). Once the
#' release exists, every listed asset is required: if any cannot be
#' downloaded, stop() rather than proceed with a partial or empty history.
#' The set of year shards to pull is read back from the manifest's own
#' summary$years (written by publish() on every prior run), since this
#' pipeline keeps no separate shard-coverage registry.
protect_history_pull <- function(io, dir) {
  if (!isTRUE(io$release_exists())) return(character(0))

  pull_or_stop <- function(pattern) {
    ok <- isTRUE(io$download(pattern, dir))
    if (!ok) stop(sprintf(
      "release 'current' exists but %s could not be downloaded; aborting to protect accumulated history",
      pattern))
    pattern
  }

  pulled <- character(0)
  pulled <- c(pulled, pull_or_stop("manifest.json"))
  pulled <- c(pulled, pull_or_stop("vcs-signals-recent.db"))

  manifest_path <- file.path(dir, "manifest.json")
  manifest <- tryCatch(
    jsonlite::fromJSON(manifest_path, simplifyVector = FALSE),
    error = function(e) NULL)
  years <- tryCatch(as.integer(unlist(manifest$summary$years)), error = function(e) integer(0))
  years <- years[!is.na(years)]

  for (yr in years) {
    pulled <- c(pulled, pull_or_stop(sprintf("vcs-signals-%04d.db", yr)))
  }

  # The summary shard holds no irreplaceable accumulated history (it is
  # fully rebuilt from the working DB every run), so its pull is
  # best-effort: on failure it is simply left out of `pulled`, which makes
  # the change-gate treat it as added (re-uploaded) this run, not fatal.
  if (isTRUE(io$download("vcs-signals-summary.db", dir))) {
    pulled <- c(pulled, "vcs-signals-summary.db")
  }

  pulled
}

#' Copy the working DB's non-series tables into the just-exported recent
#' shard: vcs_signals_summary, repos, repo_packages, series_latest, and
#' pipeline_state. Mirrors bioconductor-downloads' embed_summary, extended
#' to the extra tables this pipeline's recent shard carries.
.embed_recent_tables <- function(con, recent_path) {
  rcon <- DBI::dbConnect(RSQLite::SQLite(), recent_path)
  on.exit(DBI::dbDisconnect(rcon), add = TRUE)
  ensure_series_schema(rcon)
  ensure_repo_schema(rcon)

  copy_table <- function(name) {
    DBI::dbExecute(rcon, sprintf("DELETE FROM %s", name))
    if (DBI::dbExistsTable(con, name)) {
      df <- DBI::dbReadTable(con, name)
      if (nrow(df) > 0) DBI::dbWriteTable(rcon, name, df, append = TRUE)
    }
  }
  for (nm in c("vcs_signals_summary", "repos", "repo_packages",
               "series_latest", "pipeline_state")) copy_table(nm)
  invisible(NULL)
}

#' Export, change-gate, upload, and heartbeat: the full publish step.
#'
#' Pulls prior history (protect_history_pull) so the change-gate has
#' something to hash against, exports every year shard present in
#' signals_series plus the recent window (with the non-series tables
#' embedded) plus the summary shard, hashes all of them, and uploads only
#' the shards whose content actually changed (or every shard when
#' force_full is TRUE). A run with no changed shards still rewrites and
#' uploads manifest.json, so the release always has a fresh heartbeat even
#' when nothing else changed.
publish <- function(io, con, out_dir, tag, source_kind, force_full = FALSE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  pulled <- protect_history_pull(io, out_dir)
  prev_names <- setdiff(pulled, "manifest.json")
  prev_hashes <- stats::setNames(
    vapply(prev_names, function(nm) shard_hash(file.path(out_dir, nm)), character(1)),
    prev_names)

  year_rows <- DBI::dbGetQuery(con, "SELECT DISTINCT substr(date, 1, 4) AS yr FROM signals_series ORDER BY yr")
  years <- as.integer(year_rows$yr)

  shard_names <- character(0)
  for (yr in years) {
    shard <- sprintf("vcs-signals-%04d.db", yr)
    export_series_shard(file.path(out_dir, shard), extract_year_rows(con, yr))
    shard_names <- c(shard_names, shard)
  }

  today <- Sys.Date()
  recent_shard <- "vcs-signals-recent.db"
  recent_path <- file.path(out_dir, recent_shard)
  recent_rows <- extract_recent_rows(con, today, RECENT_WINDOW)
  export_series_shard(recent_path, recent_rows)
  .embed_recent_tables(con, recent_path)
  shard_names <- c(shard_names, recent_shard)

  summary_df <- if (DBI::dbExistsTable(con, "vcs_signals_summary")) DBI::dbReadTable(con, "vcs_signals_summary") else data.frame()
  repos_df   <- if (DBI::dbExistsTable(con, "repos")) DBI::dbReadTable(con, "repos") else data.frame()
  rp_df      <- if (DBI::dbExistsTable(con, "repo_packages")) DBI::dbReadTable(con, "repo_packages") else data.frame()

  summary_shard <- "vcs-signals-summary.db"
  export_summary_shard(file.path(out_dir, summary_shard), summary_df, repos_df, rp_df)
  shard_names <- c(shard_names, summary_shard)

  curr_hashes <- stats::setNames(
    vapply(shard_names, function(nm) shard_hash(file.path(out_dir, nm)), character(1)),
    shard_names)

  changed <- if (isTRUE(force_full)) shard_names else changed_shards(prev_hashes, curr_hashes)

  for (nm in changed) io$upload(file.path(out_dir, nm))

  data_through <- if (nrow(recent_rows) > 0) max(recent_rows$date) else NULL
  summary_block <- list(
    source_kind  = source_kind,
    last_checked = format(today),
    data_through = data_through,
    years        = as.list(years),
    packages     = nrow(summary_df),
    repos        = nrow(repos_df))

  manifest_path <- file.path(out_dir, "manifest.json")
  write_manifest(manifest_path, changed, tag, summary_block)
  io$upload(manifest_path)

  invisible(list(changed_shards = changed, shards = shard_names))
}
