# scripts/bitbucket.R - Bitbucket Cloud collectors: the transport, the pacer, the ask and the
# repository identity. Pure functions behind an injected io: io$bb_get(path) returns an
# http_request() result for GET BITBUCKET_API<path>. Anonymous, no token. Bitbucket sends no
# rate-limit header, so only a 429 closes it for the run. Uses .gl_at, .gl_int, .gl_chr, .gl_utc
# (gitlab.R), cb_run_state, .cb_log, .cb_status, .cb_enc (codeberg.R), http_request (http.R),
# host_pacer (pacing.R), host_predicate (hosts.R), rows_df_empty_gauges and update_repo_node_ids
# (helpers.R).

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

#' One gauge row from a repository object and its counts, `list(forks, prs_open, prs_merged,
#' prs_closed, head_committed_at)`. is_fork is NA when the object does not carry `parent` at all.
bb_gauge_row <- function(obj, counts) {
  at <- function(k) .gl_at(obj, k)
  ct <- function(k) .gl_at(counts, k)
  size <- at("size")
  row <- rows_df_empty_gauges()[1, ]
  row[1, ] <- NA
  row$node_id <- bb_node_id(.gl_at(obj, "workspace", "uuid"), at("uuid"))
  row$name_with_owner <- .gl_chr(at("full_name"))
  row$forks <- .gl_int(ct("forks"))
  row$prs_open <- .gl_int(ct("prs_open"))
  row$prs_merged <- .gl_int(ct("prs_merged"))
  row$prs_closed <- .gl_int(ct("prs_closed"))
  row$size_kb <- if (is.numeric(size) && length(size) == 1L && !is.na(size)) as.integer(round(size / 1024)) else NA_integer_
  row$is_fork <- if (!"parent" %in% names(obj)) NA_integer_ else as.integer(is.list(obj[["parent"]]))
  row$created_at <- .gl_utc(at("created_on"))
  row$head_committed_at <- .gl_chr(ct("head_committed_at"))
  rownames(row) <- NULL
  row
}

#' Read one repository: its object, three counts and its main branch's date, each through
#' .bb_ask. `repo` is one row of bitbucket_active_repos(). state is "ok" (row set), "not_located",
#' "deferred" or "stopped"; node_id is the id the repository now has, NA unless it was located;
#' moved is TRUE when a stored node id was rewritten. The first failed call defers the repository
#' and nothing more is asked for it.
bb_read_repo <- function(io, repo, st) {
  out <- function(state, row = NULL, node_id = NA_character_, moved = FALSE)
    list(state = state, row = row, node_id = node_id, moved = moved)
  fail <- function(path, a) {
    .cb_log(st, sprintf("Bitbucket %s: %s, the repository is deferred", path, .cb_status(a)))
    out("deferred")
  }
  # A repository's identity from its object, or NA when a part is missing or not a uuid.
  identity <- function(obj) {
    nid <- bb_node_id(.gl_at(obj, "workspace", "uuid"), .gl_at(obj, "uuid"))
    fn <- .gl_chr(.gl_at(obj, "full_name"))
    if (is.na(nid) || is.na(fn) || length(strsplit(fn, "/", fixed = TRUE)[[1]]) != 2L) NA_character_ else nid
  }

  node <- repo$node_id
  moved <- FALSE
  byid <- is_bitbucket_node(node)
  path <- if (byid) bb_ids_path(node) else bb_path(repo$owner, repo$name)
  if (is.na(path)) return(out("not_located"))
  p <- paste0(path, "?fields=", BITBUCKET_REPO_FIELDS)
  a <- .bb_ask(io, st, p)
  if (a$state == "absent" && byid) {
    path <- bb_path(repo$owner, repo$name)
    if (is.na(path)) return(out("not_located"))
    p <- paste0(path, "?fields=", BITBUCKET_REPO_FIELDS)
    a <- .bb_ask(io, st, p)
    if (a$state == "ok") {
      now <- identity(a$json)
      if (is.na(now)) return(fail(p, a))
      if (sub("^.*:", "", now) != sub("^.*:", "", node)) return(out("not_located"))
      moved <- TRUE
    }
  }
  if (a$state == "stopped") return(out("stopped"))
  if (a$state == "absent") return(out("not_located"))
  if (a$state == "failed") return(fail(p, a))
  obj <- a$json
  nid <- identity(obj)
  if (is.na(nid)) return(fail(p, a))

  parts <- strsplit(tolower(.gl_chr(obj$full_name)), "/", fixed = TRUE)[[1]]
  base <- paste0("/repositories/", paste(.cb_enc(parts), collapse = "/"))
  counts <- list(forks = NA_integer_, prs_open = NA_integer_, prs_merged = NA_integer_,
                 prs_closed = NA_integer_, head_committed_at = NA_character_)
  asks <- c(forks = "/forks?pagelen=1&fields=size",
            prs_open = "/pullrequests?state=OPEN&pagelen=1&fields=size",
            prs_merged = "/pullrequests?state=MERGED&pagelen=1&fields=size",
            prs_closed = "/pullrequests?state=DECLINED&pagelen=1&fields=size")
  for (k in names(asks)) {
    p <- paste0(base, asks[[k]])
    a <- .bb_ask(io, st, p)
    if (a$state == "stopped") return(out("stopped", node_id = nid, moved = moved))
    n <- if (a$state == "ok") .gl_int(a$json$size) else NA_integer_
    if (is.na(n)) return(fail(p, a))
    counts[[k]] <- n
  }
  branch <- .gl_chr(.gl_at(obj, "mainbranch", "name"))
  if (!is.na(branch) && nzchar(branch)) {
    p <- paste0(base, "/refs/branches/", .cb_enc(branch), "?fields=target.date")
    a <- .bb_ask(io, st, p)
    if (a$state == "stopped") return(out("stopped", node_id = nid, moved = moved))
    if (a$state == "ok") counts$head_committed_at <- .gl_utc(.gl_at(a$json, "target", "date"))
    else if (!(a$state == "absent" && identical(a$status, 404L))) return(fail(p, a))
  }
  out("ok", bb_gauge_row(obj, counts), nid, moved)
}

#' The repo_ids to read today, in order: every repository without a Bitbucket node id, then one
#' part of the located ones, chosen by the day. A budget that reads `per_run` repositories a day
#' needs `parts` days to turn over every located repository once. Inside the part the order turns
#' by cycle, so a day that runs out of budget does not skip the same tail every cycle. Pure.
bb_slice <- function(repos, today, budget = BITBUCKET_MAX_REQUESTS) {
  if (!nrow(repos)) return(character(0))
  repos <- repos[order(repos$repo_id, method = "radix"), , drop = FALSE]
  located <- is_bitbucket_node(repos$node_id)
  ids <- repos$repo_id[located]
  per_run <- max(1L, floor(budget / BITBUCKET_CALLS_PER_REPO))
  parts <- max(1L, ceiling(length(ids) / per_run))
  sel <- ids[(seq_along(ids) - 1L) %% parts == as.integer(today) %% parts]
  if (length(sel)) {
    k <- (as.integer(today) %/% parts) %% length(sel)
    sel <- sel[(seq_along(sel) + k - 1L) %% length(sel) + 1L]
  }
  c(repos$repo_id[!located], sel)
}

#' Read today's slice. Stops at `deadline` (the rest is `unreached`), when fewer than
#' BITBUCKET_CALLS_PER_REPO requests are left or when the run is stopped (the rest is `later`).
#' `deferred` and `unreached` are node ids of located repositories. `ids` is the frame for
#' update_repo_node_ids: every repository read whose node id is not the stored one.
bb_collect <- function(io, repos, st, deadline = Inf, now = function() as.numeric(Sys.time()),
                       today = Sys.Date(), budget = BITBUCKET_MAX_REQUESTS) {
  slice <- bb_slice(repos, today, budget)
  stored <- stats::setNames(repos$node_id, repos$repo_id)
  rows <- list(); ids <- list()
  deferred <- unreached <- not_located <- character(0)
  later <- n_deferred <- 0L
  tried <- character(0)  # node ids already read or deferred this run: one read per repository
  for (i in seq_along(slice)) {
    rest <- slice[i:length(slice)]
    if (now() >= deadline) {
      isloc <- is_bitbucket_node(stored[rest])
      unreached <- unique(stored[rest][isloc])
      later <- sum(!isloc)
      break
    }
    if (budget - st$asked < BITBUCKET_CALLS_PER_REPO) { later <- length(rest); break }
    id <- slice[i]
    if (stored[[id]] %in% tried) next
    r <- bb_read_repo(io, repos[match(id, repos$repo_id), , drop = FALSE], st)
    if (r$state == "stopped") { later <- length(rest); break }
    if (r$state == "ok") {
      if (!r$node_id %in% tried) rows[[length(rows) + 1L]] <- r$row
      tried <- c(tried, r$node_id)
      if (is.na(stored[[id]]) || !identical(stored[[id]], r$node_id)) {
        fn <- strsplit(r$row$name_with_owner, "/", fixed = TRUE)[[1]]
        ids[[length(ids) + 1L]] <- data.frame(repo_id = id, node_id = r$node_id, owner = fn[1], name = fn[2],
          name_with_owner = r$row$name_with_owner, status = "active", stringsAsFactors = FALSE)
      }
    } else if (r$state == "deferred") {
      n_deferred <- n_deferred + 1L
      if (is_bitbucket_node(stored[[id]])) { deferred <- c(deferred, stored[[id]]); tried <- c(tried, stored[[id]]) }
    } else {
      not_located <- c(not_located, id)
      if (is_bitbucket_node(stored[[id]])) tried <- c(tried, stored[[id]])
    }
  }
  list(snapshot = if (length(rows)) do.call(rbind, rows) else NULL, deferred = deferred,
       unresolvable = character(0), unreached = unname(unreached),
       ids = if (length(ids)) do.call(rbind, ids) else NULL,
       not_located = not_located, read = length(rows), later = later, n_deferred = n_deferred)
}
