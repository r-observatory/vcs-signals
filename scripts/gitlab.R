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

# Per-run token state: domains whose token was refused, domains already logged as accepted, and
# the REST pacer whose budget a refusal lowers. The caller makes one per run.
gl_run_state <- function(pacer = NULL, log = NULL) {
  st <- new.env(parent = emptyenv())
  st$dropped <- character(); st$accepted <- character(); st$pacer <- pacer; st$log <- log
  st
}

# The anonymous hourly REST budget of a domain's host.
.gl_rest_anon <- function(domain) {
  key <- .gl_key(domain)
  if (key %in% names(GITLAB_REST_HOURLY)) GITLAB_REST_HOURLY[[key]] else GITLAB_REST_HOURLY[["default"]]
}

# Sends with the domain's token unless this run dropped it. `send(token)` returns a result with
# `status`. A 401 while the token is set drops it, lowers the REST budget and retries once without it.
.gl_with_token <- function(domain, state, send) {
  if (is.null(state)) state <- gl_run_state()
  tok <- if (domain %in% state$dropped) NA_character_ else gl_token(domain)
  r <- send(tok)
  if (!.has_token(tok)) return(r)
  if (identical(r$status, 401L)) {
    state$dropped <- c(state$dropped, domain)
    if (!is.null(state$pacer)) state$pacer$set_hourly_budget(.gl_key(domain), .gl_rest_anon(domain))
    .gl_log(state$log, sprintf("gitlab %s: token refused (401), continuing without it", domain))
    return(send(NA_character_))
  }
  if (!is.na(r$status) && r$status < 300L && !domain %in% state$accepted) {
    state$accepted <- c(state$accepted, domain)
    .gl_log(state$log, sprintf("gitlab %s: token accepted, REST budget %d per hour", domain, GITLAB_REST_HOURLY_TOKEN))
  }
  r
}

# The real io member: one POST to the domain's GraphQL endpoint.
gl_graphql <- function(domain, query, variables = NULL, run = system2, state = NULL) {
  .gl_with_token(domain, state, function(tok)
    http_graphql(paste0(gl_api_base(domain), "/api/graphql"), query, variables,
                 token = tok, run = run, timeout = HTTP_TIMEOUT_GITLAB_S))
}

# The real io member: GET <base>/api/v4<path>, an http_request() result.
gl_rest <- function(domain, path, run = system2, state = NULL) {
  .gl_with_token(domain, state, function(tok)
    http_request("GET", paste0(gl_api_base(domain), "/api/v4", path), run = run, timeout = HTTP_TIMEOUT_GITLAB_S,
                 secret_headers = if (.has_token(tok)) paste("PRIVATE-TOKEN:", tok) else character()))
}

# The pacer's key: the API base's host, so two domains served by one server share its budget.
.gl_key <- function(domain) sub("^[a-z]+://([^/:]+).*$", "\\1", gl_api_base(domain))

# One pacer per run, passed to both gl_resolve and gl_collect_gauges so the request cap covers both.
gl_pacer <- function(now = function() as.numeric(Sys.time()), sleep = Sys.sleep, run_budget = GITLAB_MAX_REQUESTS) {
  host_pacer(min_interval = c(default = GITLAB_MIN_INTERVAL_S), run_budget = c(default = run_budget),
             breaker_after = GITLAB_BREAKER_AFTER, now = now, sleep = sleep)
}

# Only the fields gl_parse_projects reads: older instances reject forkedFrom and commitCount.
gl_projects_query <- function(by = c("paths", "ids")) {
  by <- match.arg(by)
  keys <- if (by == "paths") c("[String!]", "fullPaths") else c("[ID!]", "ids")
  sprintf('query($keys: %s) {
  projects(%s: $keys, first: %d) {
    nodes {
      id fullPath
      starCount forksCount archived isForked
      topics createdAt lastActivityAt
      issuesEnabled mergeRequestsEnabled
      openIssuesCount openMergeRequestsCount
      issueStatusCounts { closed }
      mrClosed: mergeRequests(state: closed) { count }
      mrMerged: mergeRequests(state: merged) { count }
      releases(first: 5, sort: RELEASED_AT_DESC) { count nodes { tagName releasedAt upcomingRelease } }
      repository { tree { lastCommit { committedDate } } }
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

# A release flagged upcoming, or dated after `now`. GitLab sorts these first; GitHub's
# latestRelease is never one, and its count does not include them.
.gl_upcoming <- function(r, now) {
  if (isTRUE(.gl_at(r, "upcomingRelease"))) return(TRUE)
  at <- .gl_utc(.gl_at(r, "releasedAt"))
  !is.na(at) && as.numeric(as.POSIXct(at, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")) > now
}

# The newest release that is out, or NULL when every fetched one is upcoming.
.gl_released <- function(nodes, now) {
  for (r in if (is.list(nodes)) nodes) if (!.gl_upcoming(r, now)) return(r)
  NULL
}

# GitLab's count less the upcoming releases among the newest fetched. NA when every fetched one
# is upcoming and more exist, since how many of the rest are out is unknown.
.gl_released_count <- function(count, nodes, now) {
  if (is.na(count)) return(NA_integer_)
  nodes <- if (is.list(nodes)) nodes else list()
  up <- sum(vapply(nodes, .gl_upcoming, NA, now = now))
  if (length(nodes) && up == length(nodes) && count > length(nodes)) return(NA_integer_)
  as.integer(max(count - up, 0L))
}

.gl_nodes <- function(resp) {
  nodes <- .gl_at(resp, "data", "projects", "nodes")
  if (is.list(nodes)) nodes else NULL
}

.gl_row <- function(n, domain, now) {
  gid <- .gl_chr(.gl_at(n, "id"))
  if (is.na(gid) || !grepl("^gid://gitlab/Project/[0-9]+$", gid)) return(NULL)
  # A disabled feature is unknown, not zero.
  issues <- !isFALSE(.gl_at(n, "issuesEnabled"))
  mrs <- !isFALSE(.gl_at(n, "mergeRequestsEnabled"))
  topics <- .gl_at(n, "topics")
  rel_nodes <- .gl_at(n, "releases", "nodes")
  release <- .gl_released(rel_nodes, now)
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
    releases_total = .gl_released_count(.gl_int(.gl_at(n, "releases", "count")), rel_nodes, now),
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
# `now` (seconds) is the run's time: a release dated after it is upcoming.
gl_parse_projects <- function(resp, domain, now = as.numeric(Sys.time())) {
  rows <- Filter(Negate(is.null), lapply(.gl_nodes(resp), .gl_row, domain = domain, now = now))
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

# One request for one batch, paced by the domain's server. state is "refused" when the pacer
# says no, "failed", or "ok" with the parsed rows; clean is FALSE when an answer with data also
# carried errors.
.gl_ask <- function(io, pacer, domain, by, keys, log, now = as.numeric(Sys.time())) {
  key <- .gl_key(domain)
  if (!isTRUE(pacer$before(key))) return(list(state = "refused"))
  res <- tryCatch(io$gl_graphql(domain, gl_projects_query(by), list(keys = as.list(unname(keys)))),
                  error = function(e) list(error = conditionMessage(e)))
  e <- .gl_at(res, "error")
  ok <- is.list(res) && (is.null(e) || is.na(.gl_chr(e))) && !is.null(.gl_nodes(res))
  pacer$after(key, ok)
  if (!ok) {
    .gl_log(log, sprintf("GitLab %s: %d %s not read: %s", domain, length(keys),
                         if (by == "paths") "paths" else "projects", .gl_why(res)))
    return(list(state = "failed"))
  }
  list(state = "ok", rows = gl_parse_projects(res, domain, now), clean = !length(.gl_at(res, "errors")))
}

.gl_stop_note <- function(pacer, domain, left) {
  why <- if (.gl_key(domain) %in% pacer$deferred()) "deferred after repeated failures" else "out of requests for this run"
  sprintf("GitLab %s: %s, %d left for the next run", domain, why, left)
}

# ---- path resolution -----------------------------------------------------------
#' Resolve GitLab paths to project ids. `repos` has repo_id, domain and path (the full path).
#' Returns resolve_node_ids' shape for the projects found, all status 'active'. A path the answer
#' leaves out is not returned and is never marked gone: path lookup does not follow renames, so it
#' is asked again next run. A failed batch, or one not asked by `deadline`, contributes no rows.
gl_resolve <- function(io, repos, pacer, log = NULL, deadline = Inf,
                       now = function() as.numeric(Sys.time())) {
  out <- list(data.frame(repo_id = character(), node_id = character(), owner = character(),
    name = character(), name_with_owner = character(), status = character(), stringsAsFactors = FALSE))
  repos <- repos[!is.na(repos$path) & nzchar(repos$path), , drop = FALSE]
  for (d in unique(repos$domain)) {
    sub <- repos[repos$domain == d, , drop = FALSE]
    batches <- unname(chunk(unique(sub$path), GITLAB_BATCH))
    for (i in seq_along(batches)) {
      if (now() >= deadline) {
        .gl_log(log, sprintf("GitLab %s: %d paths not asked before the deadline, left for the next run",
                             d, length(unlist(batches[i:length(batches)]))))
        break
      }
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
.GL_NODE_RE <- "^gitlab:([^:]+):[0-9]+$"
is_gitlab_node <- function(node_id) !is.na(node_id) & grepl(.GL_NODE_RE, node_id)

#' The active GitLab repositories with a GitLab node id, by repo_id: the domain and project id the
#' id carries, and the owner and name the summary holds. `con` is the published summary.
gitlab_roster_rows <- function(con) {
  rows <- DBI::dbGetQuery(con, sprintf(paste0(
    "SELECT repo_id, node_id, owner, name FROM repos WHERE %s AND status = 'active' AND node_id IS NOT NULL ",
    "ORDER BY repo_id"), host_predicate(GITLAB_HOSTS)))
  rows <- rows[is_gitlab_node(rows$node_id), , drop = FALSE]
  data.frame(repo_id = rows$repo_id, node_id = rows$node_id, domain = sub(.GL_NODE_RE, "\\1", rows$node_id),
             project_id = as.integer(sub("^.*:", "", rows$node_id)), owner = rows$owner, name = rows$name,
             stringsAsFactors = FALSE)
}

#' Daily gauges for resolved GitLab projects, `GITLAB_BATCH` ids per request, grouped by domain.
#' `repo_map` has node_id and repo_id; each project's domain is read from its node id. Returns
#' collect_gauges' list: `snapshot` (the gauge frame plus full_path and gl_id, or NULL), `deferred`
#' (a failed batch, a host the pacer stopped, or an id that is not a GitLab node id), `unresolvable`
#' (asked and left out of a clean answer) and `unreached` (not asked by `deadline`).
gl_collect_gauges <- function(io, repo_map, pacer, deadline = Inf,
                              now = function() as.numeric(Sys.time()), log = NULL) {
  records <- list(); deferred <- character(0); unresolvable <- character(0); unreached <- character(0)
  ids_all <- unique(repo_map$node_id)
  odd <- ids_all[is.na(ids_all) | !grepl(.GL_NODE_RE, ids_all)]
  if (length(odd)) {
    .gl_log(log, sprintf("GitLab: %d ids are not GitLab node ids and were not asked", length(odd)))
    deferred <- odd
  }
  ids_all <- setdiff(ids_all, odd)
  dom <- sub(.GL_NODE_RE, "\\1", ids_all)
  batches <- list()
  for (d in unique(dom))
    for (b in unname(chunk(ids_all[dom == d], GITLAB_BATCH)))
      batches[[length(batches) + 1L]] <- list(domain = d, ids = b)
  stopped <- character(0)
  for (i in seq_along(batches)) {
    d <- batches[[i]]$domain; ids <- batches[[i]]$ids
    if (d %in% stopped) { deferred <- c(deferred, ids); next }
    if (now() >= deadline) { unreached <- c(unreached, ids); next }
    a <- .gl_ask(io, pacer, d, "ids", .gl_gid(ids), log, now())
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

# ---- weekly: commit totals, contributors, licence ------------------------------
# REST pacer: hourly budgets by host, 500 where a token is set. GraphQL uses gl_pacer().
gl_rest_pacer <- function(now = function() as.numeric(Sys.time()), sleep = Sys.sleep) {
  hourly <- GITLAB_REST_HOURLY
  for (d in names(GITLAB_DOMAINS)) if (!is.na(gl_token(d))) hourly[[.gl_key(d)]] <- GITLAB_REST_HOURLY_TOKEN
  host_pacer(min_interval = c(default = GITLAB_MIN_INTERVAL_S), hourly_budget = hourly,
             breaker_after = GITLAB_BREAKER_AFTER, now = now, sleep = sleep)
}

.gl_commits_query <- sprintf("query($ids: [ID!]) {
  projects(ids: $ids, first: %d) { nodes { id repository { commitCount } } }
}", GITLAB_BATCH)

.gl_commit_schema_error <- function(res)
  any(vapply(Filter(is.list, .gl_at(res, "errors")), function(e)
    grepl("commitCount", .gl_chr(.gl_at(e, "message")), fixed = TRUE), NA))

#' Commit totals by repo_id for GitLab repos (node_id, repo_id, domain), 50 ids per GraphQL request.
#' NA where a project is missing, empty, in a refused or failed batch, or its host rejects commitCount
#' (that host is not asked again).
gl_commit_counts <- function(io, repos, pacer) {
  out <- data.frame(repo_id = repos$repo_id, commits_total = rep(NA_integer_, nrow(repos)), stringsAsFactors = FALSE)
  for (d in unique(repos$domain)) {
    rows <- which(repos$domain == d & !is.na(repos$node_id) & grepl(.GL_NODE_RE, repos$node_id))
    key <- .gl_key(d)
    for (b in unname(chunk(rows, GITLAB_BATCH))) {
      if (!isTRUE(pacer$before(key))) break
      res <- tryCatch(io$gl_graphql(d, .gl_commits_query, list(ids = as.list(.gl_gid(repos$node_id[b])))),
                      error = function(e) list(error = conditionMessage(e)))
      if (.gl_commit_schema_error(res)) { pacer$after(key, TRUE); break }
      nodes <- .gl_nodes(res)
      e <- .gl_at(res, "error")
      pacer$after(key, is.list(res) && (is.null(e) || is.na(.gl_chr(e))) && !is.null(nodes))
      if (is.null(nodes)) next
      got <- vapply(nodes, function(n) .gl_int(.gl_at(n, "repository", "commitCount")), 1L)
      ids <- vapply(nodes, function(n) .gl_chr(.gl_at(n, "id")), "")
      hit <- match(.gl_gid(repos$node_id[b]), ids)
      out$commits_total[b[!is.na(hit)]] <- got[hit[!is.na(hit)]]
    }
  }
  out
}

# One paced REST GET. state is "stopped" (budget spent, or the host said 429 or ratelimit-remaining 0
# earlier), "failed" or "ok". `last` is TRUE when this answer says no more requests this run: the host
# is then closed for the run by setting its budget to 0. Neither case counts toward the breaker.
.gl_rest_get <- function(io, pacer, domain, path) {
  key <- .gl_key(domain)
  if (!isTRUE(pacer$before(key))) return(list(state = "stopped"))
  r <- tryCatch(io$gl_rest(domain, path), error = function(e) list(status = NA_integer_, error = conditionMessage(e)))
  if (identical(r$status, 429L)) { pacer$set_hourly_budget(key, 0L); return(list(state = "stopped")) }
  ok <- is.list(r) && is.na(.gl_chr(r$error)) && !is.na(r$status) && r$status < 300L
  # A 403, 404 or 410 is an answer about the project: the host is fine, the values are NA.
  gone <- is.list(r) && !is.na(r$status) && r$status %in% c(403L, 404L, 410L)
  pacer$after(key, ok || gone)
  if (!ok) return(list(state = "failed"))
  last <- identical(unname(r$headers["ratelimit-remaining"]), "0")
  if (last) pacer$set_hourly_budget(key, 0L)
  list(state = "ok", res = r, last = last)
}

.gl_body <- function(r) tryCatch(jsonlite::fromJSON(r$body, simplifyVector = FALSE), error = function(e) NULL)

#' Contributor count (x-total) and the top contributor's commits for one project, two REST GETs.
#' No name or email is read from the rows. stopped is TRUE when the host's REST calls ended this run.
#' answered is TRUE when every call it needed got an answer of any kind, a 403, 404, 410, 5xx or
#' network error included (a failed first call needs no second); FALSE when the pacer refused a call
#' or the host said 429 or ratelimit-remaining 0 before the calls were done.
gl_contributors <- function(io, domain, project_id, pacer) {
  out <- list(total = NA_integer_, top_commits = NA_integer_, stopped = FALSE, answered = FALSE)
  base <- sprintf("/projects/%s/repository/contributors", format(project_id, scientific = FALSE, trim = TRUE))
  a <- .gl_rest_get(io, pacer, domain, paste0(base, "?per_page=1"))
  if (a$state == "stopped") { out$stopped <- TRUE; return(out) }
  if (a$state == "failed") { out$answered <- TRUE; return(out) }
  out$total <- suppressWarnings(as.integer(unname(a$res$headers["x-total"])))
  if (a$last) { out$stopped <- TRUE; return(out) }
  b <- .gl_rest_get(io, pacer, domain, paste0(base, "?order_by=commits&sort=desc&per_page=1"))
  if (b$state == "stopped") { out$stopped <- TRUE; return(out) }
  out$answered <- TRUE
  if (b$state == "failed") return(out)
  body <- .gl_body(b$res)
  out$top_commits <- .gl_int(.gl_at(body, 1L, "commits"))
  out$stopped <- b$last
  out
}

#' The licence key of one project (license.key, NA when none), one REST GET. answered as for
#' gl_contributors; read only for a 200, where a null licence is a reading of NA.
gl_license <- function(io, domain, project_id, pacer) {
  a <- .gl_rest_get(io, pacer, domain, sprintf("/projects/%s?license=true", format(project_id, scientific = FALSE, trim = TRUE)))
  if (a$state == "stopped") return(list(key = NA_character_, stopped = TRUE, answered = FALSE, read = FALSE))
  if (a$state == "failed") return(list(key = NA_character_, stopped = FALSE, answered = TRUE, read = FALSE))
  body <- .gl_body(a$res)
  # A 200 that is not a project object is no reading, so it cannot replace a known licence.
  if (is.null(.gl_at(body, "id"))) return(list(key = NA_character_, stopped = a$last, answered = TRUE, read = FALSE))
  list(key = .gl_chr(.gl_at(body, "license", "key")), stopped = a$last, answered = TRUE, read = TRUE)
}

# GitLab licence keys to the SPDX ids GitHub reports; any other key is NA.
.GL_SPDX <- c(mit = "MIT", `apache-2.0` = "Apache-2.0", `gpl-2.0` = "GPL-2.0", `gpl-3.0` = "GPL-3.0",
  `lgpl-2.1` = "LGPL-2.1", `lgpl-3.0` = "LGPL-3.0", `agpl-3.0` = "AGPL-3.0", `bsd-2-clause` = "BSD-2-Clause",
  `bsd-3-clause` = "BSD-3-Clause", `mpl-2.0` = "MPL-2.0", `cc0-1.0` = "CC0-1.0", unlicense = "Unlicense",
  `epl-2.0` = "EPL-2.0", isc = "ISC", `cc-by-4.0` = "CC-BY-4.0", `cc-by-sa-4.0` = "CC-BY-SA-4.0",
  `afl-3.0` = "AFL-3.0", `artistic-2.0` = "Artistic-2.0", `bsd-3-clause-clear` = "BSD-3-Clause-Clear",
  `bsl-1.0` = "BSL-1.0", `cecill-2.1` = "CECILL-2.1", `ecl-2.0` = "ECL-2.0", `epl-1.0` = "EPL-1.0",
  `eupl-1.1` = "EUPL-1.1", `eupl-1.2` = "EUPL-1.2", `lppl-1.3c` = "LPPL-1.3c", `ms-pl` = "MS-PL",
  `ms-rl` = "MS-RL", ncsa = "NCSA", `ofl-1.1` = "OFL-1.1", `osl-3.0` = "OSL-3.0", postgresql = "PostgreSQL",
  `upl-1.0` = "UPL-1.0", vim = "Vim", wtfpl = "WTFPL", zlib = "Zlib",
  # GitHub reports an or-later licence by its plain id.
  `gpl-2.0+` = "GPL-2.0", `gpl-3.0+` = "GPL-3.0", `lgpl-2.1+` = "LGPL-2.1", `lgpl-3.0+` = "LGPL-3.0",
  `agpl-3.0+` = "AGPL-3.0")
gl_license_spdx <- function(key) {
  key <- as.character(key)
  out <- unname(.GL_SPDX[key])
  out[is.na(key)] <- NA_character_
  out
}
