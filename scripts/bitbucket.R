# scripts/bitbucket.R - Bitbucket Cloud collectors: the transport, the pacer, the ask and the
# repository identity. Pure functions behind an injected io: io$bb_get(path) returns an
# http_request() result for GET BITBUCKET_API<path>. Anonymous, no token. Bitbucket sends no
# rate-limit header, so only a 429 closes it for the run. Uses .gl_at, .gl_int, .gl_chr (gitlab.R)
# and cb_run_state (codeberg.R).

# The real io member. curl runs without -L, so a redirect comes back as a status.
bb_get <- function(path, run = system2) {
  http_request("GET", paste0(BITBUCKET_API, path),
               headers = c(paste("User-Agent:", CODEBERG_USER_AGENT), "Accept: application/json"),
               timeout = BITBUCKET_TIMEOUT_S, run = run)
}

# One pacer per run, keyed BITBUCKET_DOMAIN, shared by every Bitbucket request of the run.
bb_pacer <- function(now = function() as.numeric(Sys.time()), sleep = Sys.sleep, run_budget = BITBUCKET_MAX_REQUESTS) {
  host_pacer(min_interval = c(default = BITBUCKET_MIN_INTERVAL_S), run_budget = c(default = run_budget),
             breaker_after = BITBUCKET_BREAKER_AFTER, now = now, sleep = sleep)
}

# One paced GET. state is "ok" (200 with a JSON object), "absent" (403, 404 or 410), "failed" (no
# answer, 5xx, any 3xx, any other 4xx, or a 200 that is not an object) or "stopped" (nothing sent).
# Only no answer and 5xx count against the breaker. A 429 closes Bitbucket for the run, logs the
# one warning and counts toward neither. `status` is the HTTP status, for log lines.
.bb_ask <- function(io, st, path) {
  out <- list(state = "stopped", json = NULL, status = NA_integer_)
  if (!is.na(st$closed)) return(out)
  if (!isTRUE(st$pacer$before(BITBUCKET_DOMAIN))) { st$refused <- TRUE; return(out) }
  st$asked <- st$asked + 1L
  r <- tryCatch(io$bb_get(path), error = function(e) list(status = NA_integer_, error = conditionMessage(e)))
  status <- out$status <- .gl_int(.gl_at(r, "status"))
  if (identical(status, 429L)) {
    st$pacer$set_hourly_budget(BITBUCKET_DOMAIN, 0)
    st$closed <- "429"
    if (!is.null(st$log)) st$log("::warning::Bitbucket: HTTP 429, no more requests this run")
    return(out)
  }
  if (is.na(status) || status >= 500L) {
    st$pacer$after(BITBUCKET_DOMAIN, FALSE)
    out$state <- "failed"
    return(out)
  }
  st$pacer$after(BITBUCKET_DOMAIN, TRUE)
  if (status == 200L) {
    # An error beside a 200 is a cut or unreadable body. parse_json never reads a body as a URL or file.
    body <- .gl_chr(.gl_at(r, "body"))
    json <- if (is.na(.gl_chr(.gl_at(r, "error"))) && !is.na(body))
      tryCatch(jsonlite::parse_json(body, simplifyVector = FALSE), error = function(e) NULL)
    ok <- is.list(json) && !is.null(names(json))
    out$state <- if (ok) "ok" else "failed"
    if (ok) out$json <- json
    return(out)
  }
  out$state <- if (status %in% c(403L, 404L, 410L)) "absent" else "failed"
  out
}

.BB_UUID <- "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"
.bb_uuid <- function(x) {
  x <- .gl_chr(x)
  if (is.na(x)) return(NA_character_)
  m <- regmatches(x, regexec(paste0("^(\\{)?(", .BB_UUID, ")(\\})?$"), x))[[1]]
  if (length(m) != 4L || (m[2] == "{") != (m[4] == "}")) NA_character_ else tolower(m[3])
}

# "bitbucket:<workspace uuid>:<repository uuid>", braces stripped, or NA when either is not a uuid.
bb_node_id <- function(workspace_uuid, repo_uuid) {
  ws <- .bb_uuid(workspace_uuid); rp <- .bb_uuid(repo_uuid)
  if (is.na(ws) || is.na(rp)) NA_character_ else paste0("bitbucket:", ws, ":", rp)
}
.BB_NODE_RE <- paste0("^bitbucket:", .BB_UUID, ":", .BB_UUID, "$")
is_bitbucket_node <- function(node_id) !is.na(node_id) & grepl(.BB_NODE_RE, node_id)

# The API path that reads a repository by its two uuids.
bb_ids_path <- function(node_id) {
  p <- strsplit(sub("^bitbucket:", "", node_id), ":", fixed = TRUE)[[1]]
  sprintf("/repositories/%%7B%s%%7D/%%7B%s%%7D", p[1], p[2])
}

# The API path that reads a repository by owner and name, or NA for a segment that is empty or
# holds a slash, whitespace or a control character.
bb_path <- function(owner, name) {
  seg <- c(owner, name)
  if (anyNA(seg) || any(!nzchar(seg)) || any(grepl("[/[:space:][:cntrl:]]", seg))) return(NA_character_)
  sprintf("/repositories/%s/%s", utils::URLencode(tolower(owner), reserved = TRUE),
          utils::URLencode(tolower(name), reserved = TRUE))
}

#' The active Bitbucket repositories by repo_id, with their node id (NA until located), owner and
#' name. `con` holds the repos table.
bitbucket_active_repos <- function(con) {
  rows <- DBI::dbGetQuery(con, sprintf(
    "SELECT repo_id, node_id, owner, name FROM repos WHERE %s AND status = 'active' ORDER BY repo_id",
    host_predicate(BITBUCKET_HOSTS)))
  data.frame(repo_id = as.character(rows$repo_id), node_id = as.character(rows$node_id),
             owner = as.character(rows$owner), name = as.character(rows$name), stringsAsFactors = FALSE)
}
