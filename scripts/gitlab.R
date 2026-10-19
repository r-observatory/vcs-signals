# scripts/gitlab.R - GitLab GraphQL collectors: project paths to ids, and the daily gauges.
# Pure functions behind an injected io: io$gl_graphql(domain, query, variables) returns an
# http_graphql() result. The frames have the GitHub collectors' columns (github.R).

gl_api_base <- function(domain) {
  if (domain %in% names(GITLAB_DOMAINS)) GITLAB_DOMAINS[[domain]] else paste0("https://", domain)
}

# GITLAB_TOKEN_<DOMAIN>, upper case with every other character as _, or NA when unset or empty.
gl_token <- function(domain) {
  v <- Sys.getenv(paste0("GITLAB_TOKEN_", gsub("[^A-Z0-9]", "_", toupper(domain))), unset = "")
  if (nzchar(v)) v else NA_character_
}

# The real io member: one POST to the domain's GraphQL endpoint.
gl_graphql <- function(domain, query, variables = NULL, run = system2) {
  http_graphql(paste0(gl_api_base(domain), "/api/graphql"), query, variables,
               token = gl_token(domain), run = run, timeout = HTTP_TIMEOUT_GITLAB_S)
}

# One pacer per run, passed to both gl_resolve and gl_collect_gauges so the request cap covers both.
gl_pacer <- function(now = function() as.numeric(Sys.time()), sleep = Sys.sleep) {
  host_pacer(min_interval = c(default = GITLAB_MIN_INTERVAL_S), run_budget = c(default = GITLAB_MAX_REQUESTS),
             breaker_after = GITLAB_BREAKER_AFTER, now = now, sleep = sleep)
}

gl_projects_query <- function(by = c("paths", "ids")) {
  by <- match.arg(by)
  keys <- if (by == "paths") c("[String!]", "fullPaths") else c("[ID!]", "ids")
  sprintf('query($keys: %s) {
  queryComplexity { score limit }
  projects(%s: $keys, first: %d) {
    count
    pageInfo { hasNextPage }
    nodes {
      id fullPath nameWithNamespace
      group { fullPath }
      starCount forksCount archived isForked forkedFrom { fullPath }
      topics createdAt lastActivityAt
      issuesEnabled mergeRequestsEnabled
      openIssuesCount openMergeRequestsCount
      issueStatusCounts { closed }
      mrClosed: mergeRequests(state: closed) { count }
      mrMerged: mergeRequests(state: merged) { count }
      releases(first: 1, sort: RELEASED_AT_DESC) { count nodes { tagName releasedAt } }
      repository { rootRef empty commitCount tree { lastCommit { committedDate } } }
    }
  }
}', keys[1], keys[2], GITLAB_BATCH)
}

# ---- parsing -------------------------------------------------------------------
# x[[k1]][[k2]]..., or NULL where the path breaks.
.gl_at <- function(x, ...) {
  for (k in list(...)) {
    if (!is.list(x)) return(NULL)
    if (is.character(k)) { if (!k %in% names(x)) return(NULL) } else if (k > length(x)) return(NULL)
    x <- x[[k]]
  }
  x
}
.gl_int <- function(x) if (is.numeric(x) && length(x) == 1L && !is.na(x)) as.integer(x) else NA_integer_
.gl_chr <- function(x) if (is.character(x) && length(x) == 1L && !is.na(x)) x else NA_character_
# as.integer(isTRUE(x)) for a value, NA for a null.
.gl_flag <- function(x) if (is.logical(x) && length(x) == 1L && !is.na(x)) as.integer(x) else NA_integer_

# UTC with Z, as GitHub's dates are; GitLab keeps the committer's offset.
.gl_utc <- function(x) {
  x <- .gl_chr(x)
  if (is.na(x)) return(NA_character_)
  m <- regmatches(x, regexec(paste0("^([0-9]{4}-[0-9]{2}-[0-9]{2})T([0-9]{2}:[0-9]{2}:[0-9]{2})(\\.[0-9]+)?",
                                    "(Z|([+-])([0-9]{2}):?([0-9]{2}))$"), x))[[1]]
  if (!length(m)) return(NA_character_)
  t <- as.POSIXct(paste(m[2], m[3]), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  if (m[5] != "Z") t <- t - (if (m[6] == "-") -1 else 1) * (as.numeric(m[7]) * 3600 + as.numeric(m[8]) * 60)
  if (is.na(t)) NA_character_ else format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

.gl_nodes <- function(resp) {
  nodes <- .gl_at(resp, "data", "projects", "nodes")
  if (is.list(nodes)) nodes else NULL
}

.gl_row <- function(n, domain) {
  gid <- .gl_chr(.gl_at(n, "id"))
  if (is.na(gid) || !grepl("^gid://gitlab/Project/[0-9]+$", gid)) return(NULL)
  # A disabled feature is unknown, not zero.
  issues <- !isFALSE(.gl_at(n, "issuesEnabled"))
  mrs <- !isFALSE(.gl_at(n, "mergeRequestsEnabled"))
  topics <- .gl_at(n, "topics")
  release <- .gl_at(n, "releases", "nodes", 1L)
  data.frame(
    node_id = paste0("gitlab:", domain, ":", sub("^.*/", "", gid)),
    name_with_owner = .gl_chr(.gl_at(n, "fullPath")),
    owner_login = NA_character_, owner_type = NA_character_, owner_node_id = NA_character_,
    stars = .gl_int(.gl_at(n, "starCount")), forks = .gl_int(.gl_at(n, "forksCount")),
    watchers = NA_integer_,
    issues_open = if (issues) .gl_int(.gl_at(n, "openIssuesCount")) else NA_integer_,
    issues_closed = if (issues) .gl_int(.gl_at(n, "issueStatusCounts", "closed")) else NA_integer_,
    prs_open = if (mrs) .gl_int(.gl_at(n, "openMergeRequestsCount")) else NA_integer_,
    prs_closed = if (mrs) .gl_int(.gl_at(n, "mrClosed", "count")) else NA_integer_,
    prs_merged = if (mrs) .gl_int(.gl_at(n, "mrMerged", "count")) else NA_integer_,
    releases_total = .gl_int(.gl_at(n, "releases", "count")),
    size_kb = NA_integer_, license = NA_character_,
    topics = if (is.list(topics) || is.character(topics))
      paste(unlist(Filter(is.character, topics)), collapse = ",") else NA_character_,
    is_archived = .gl_flag(.gl_at(n, "archived")),
    is_fork = .gl_flag(.gl_at(n, "isForked")),
    is_mirror = NA_integer_,
    created_at = .gl_utc(.gl_at(n, "createdAt")),
    pushed_at = .gl_utc(.gl_at(n, "lastActivityAt")),
    last_release_at = .gl_utc(.gl_at(release, "releasedAt")),
    last_release_tag = .gl_chr(.gl_at(release, "tagName")),
    head_committed_at = .gl_utc(.gl_at(n, "repository", "tree", "lastCommit", "committedDate")),
    full_path = .gl_chr(.gl_at(n, "fullPath")),
    gl_id = gid,
    stringsAsFactors = FALSE)
}

# The gauge frame of an answer, plus full_path and gl_id (the GraphQL global id) for matching.
gl_parse_projects <- function(resp, domain) {
  rows <- Filter(Negate(is.null), lapply(.gl_nodes(resp), .gl_row, domain = domain))
  if (length(rows)) return(do.call(rbind, rows))
  df <- rows_df_empty_gauges()
  df$full_path <- character()
  df$gl_id <- character()
  df
}

# ---- paced requests ------------------------------------------------------------
.gl_log <- function(log, msg) if (!is.null(log)) log(msg)

.gl_why <- function(res) {
  e <- .gl_chr(.gl_at(res, "error"))
  if (is.na(e)) e <- .gl_chr(.gl_at(res, "errors", 1L, "message"))
  if (is.na(e)) "no projects in the answer" else substr(e, 1L, 200L)
}

# One request for one batch. state is "refused" when the pacer says no, "failed", or "ok" with
# the parsed rows; clean is FALSE when an answer with data also carried errors.
.gl_ask <- function(io, pacer, domain, by, keys, log) {
  if (!isTRUE(pacer$before(domain))) return(list(state = "refused"))
  res <- tryCatch(io$gl_graphql(domain, gl_projects_query(by), list(keys = as.list(unname(keys)))),
                  error = function(e) list(error = conditionMessage(e)))
  e <- .gl_at(res, "error")
  ok <- is.list(res) && (is.null(e) || is.na(.gl_chr(e))) && !is.null(.gl_nodes(res))
  pacer$after(domain, ok)
  if (!ok) {
    .gl_log(log, sprintf("GitLab %s: %d %s not read: %s", domain, length(keys),
                         if (by == "paths") "paths" else "projects", .gl_why(res)))
    return(list(state = "failed"))
  }
  list(state = "ok", rows = gl_parse_projects(res, domain), clean = !length(.gl_at(res, "errors")))
}

.gl_stop_note <- function(pacer, domain, left) {
  why <- if (domain %in% pacer$deferred()) "deferred after repeated failures" else "out of requests for this run"
  sprintf("GitLab %s: %s, %d left for the next run", domain, why, left)
}

# ---- path resolution -----------------------------------------------------------
#' Resolve GitLab paths to project ids. `repos` has repo_id, domain and path (the full path).
#' Returns resolve_node_ids' shape for the projects found, all status 'active'. A path the answer
#' leaves out is not returned and is never marked gone: path lookup does not follow renames, so it
#' is asked again next run. A failed batch contributes no rows.
gl_resolve <- function(io, repos, pacer, log = NULL) {
  out <- list(data.frame(repo_id = character(), node_id = character(), owner = character(),
    name = character(), name_with_owner = character(), status = character(), stringsAsFactors = FALSE))
  repos <- repos[!is.na(repos$path) & nzchar(repos$path), , drop = FALSE]
  for (d in unique(repos$domain)) {
    sub <- repos[repos$domain == d, , drop = FALSE]
    batches <- unname(chunk(unique(sub$path), GITLAB_BATCH))
    for (i in seq_along(batches)) {
      a <- .gl_ask(io, pacer, d, "paths", batches[[i]], log)
      if (a$state == "refused") {
        .gl_log(log, .gl_stop_note(pacer, d, length(unlist(batches[i:length(batches)]))))
        break
      }
      if (a$state == "failed") next
      asked <- sub[sub$path %in% batches[[i]], , drop = FALSE]
      # GitLab answers in any order and matches paths without case.
      hit <- match(tolower(asked$path), tolower(a$rows$full_path))
      if (all(is.na(hit))) next
      r <- a$rows[hit[!is.na(hit)], , drop = FALSE]
      parts <- strsplit(r$full_path, "/", fixed = TRUE)
      out[[length(out) + 1L]] <- data.frame(
        repo_id = asked$repo_id[!is.na(hit)], node_id = r$node_id,
        owner = vapply(parts, function(p) if (length(p) > 1L) paste(p[-length(p)], collapse = "/") else NA_character_, ""),
        name = vapply(parts, function(p) p[length(p)], ""),
        name_with_owner = r$full_path, status = "active", stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}

# ---- daily gauges --------------------------------------------------------------
.gl_gid <- function(node_ids) paste0("gid://gitlab/Project/", sub("^.*:", "", node_ids))

#' Daily gauges for resolved GitLab projects, `GITLAB_BATCH` ids per request, grouped by domain.
#' `repo_map` has node_id, repo_id and domain. Returns collect_gauges' list: `snapshot` (the gauge
#' frame plus full_path and gl_id, or NULL), `deferred` (a failed batch, or a host the pacer stopped),
#' `unresolvable` (asked and left out of a clean answer) and `unreached` (not asked by `deadline`).
gl_collect_gauges <- function(io, repo_map, pacer, deadline = Inf,
                              now = function() as.numeric(Sys.time()), log = NULL) {
  records <- list(); deferred <- character(0); unresolvable <- character(0); unreached <- character(0)
  batches <- list()
  for (d in unique(repo_map$domain))
    for (b in unname(chunk(unique(repo_map$node_id[repo_map$domain == d]), GITLAB_BATCH)))
      batches[[length(batches) + 1L]] <- list(domain = d, ids = b)
  stopped <- character(0)
  for (i in seq_along(batches)) {
    d <- batches[[i]]$domain; ids <- batches[[i]]$ids
    if (d %in% stopped) { deferred <- c(deferred, ids); next }
    if (now() >= deadline) { unreached <- c(unreached, ids); next }
    a <- .gl_ask(io, pacer, d, "ids", .gl_gid(ids), log)
    if (a$state == "refused") {
      stopped <- c(stopped, d)
      left <- unlist(lapply(batches[i:length(batches)], function(x) if (x$domain == d) x$ids))
      .gl_log(log, .gl_stop_note(pacer, d, length(left)))
      deferred <- c(deferred, ids)
      next
    }
    if (a$state == "failed") { deferred <- c(deferred, ids); next }
    rows <- a$rows[a$rows$node_id %in% ids, , drop = FALSE]
    if (nrow(rows)) records[[length(records) + 1L]] <- rows
    # GitLab leaves out an id it cannot find; a missing id is unresolvable only in a clean answer.
    missing <- setdiff(ids, rows$node_id)
    if (a$clean) unresolvable <- c(unresolvable, missing) else deferred <- c(deferred, missing)
  }
  list(snapshot = if (length(records)) do.call(rbind, records) else NULL, deferred = deferred,
       unresolvable = unique(unresolvable), unreached = unreached)
}
