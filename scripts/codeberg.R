# scripts/codeberg.R - Codeberg (Forgejo) collectors: the transport, the pacer and locating repositories.
# Pure functions behind an injected io: io$cb_get(path) returns an http_request() result for
# GET CODEBERG_API<path>. Anonymous, no token. Uses .gl_at, .gl_int and .gl_chr (gitlab.R).

# The real io member. curl runs without -L, so a 301 comes back as a status with its location.
cb_get <- function(path, run = system2) {
  http_request("GET", paste0(CODEBERG_API, path),
               headers = c(paste("User-Agent:", CODEBERG_USER_AGENT), "Accept: application/json"),
               timeout = CODEBERG_TIMEOUT_S, run = run)
}

# One pacer per run, keyed CODEBERG_DOMAIN, shared by every Codeberg request of the run.
cb_pacer <- function(now = function() as.numeric(Sys.time()), sleep = Sys.sleep, run_budget = CODEBERG_MAX_REQUESTS) {
  host_pacer(min_interval = c(default = CODEBERG_MIN_INTERVAL_S), run_budget = c(default = run_budget),
             breaker_after = CODEBERG_BREAKER_AFTER, now = now, sleep = sleep)
}

# Per-run state: the lowest ratelimit r seen, why Codeberg closed ("429" or "ratelimit") and the
# number of requests actually sent.
cb_run_state <- function(pacer, log = NULL) {
  st <- new.env(parent = emptyenv())
  st$pacer <- pacer; st$log <- log
  st$low_r <- NA_integer_; st$closed <- NA_character_; st$asked <- 0L
  st
}

.cb_log <- function(st, msg) if (!is.null(st$log)) st$log(msg)

.cb_header <- function(headers, name) {
  if (!is.character(headers) || !name %in% names(headers)) return(NA_character_)
  unname(headers[[name]])
}

# r in `ratelimit: "baseline";r=1999;t=600`, or NA.
cb_ratelimit_r <- function(headers) {
  h <- .cb_header(headers, "ratelimit")
  if (is.na(h)) return(NA_integer_)
  m <- regmatches(h, regexec("(^|;)[[:space:]]*r=([0-9]+)", h))[[1]]
  if (length(m) < 3L) NA_integer_ else suppressWarnings(as.integer(m[3]))
}

# Forgejo's list count. x-total, which only the commits list sends, is not read.
cb_total_count <- function(headers) suppressWarnings(as.integer(.cb_header(headers, "x-total-count")))

# The API path of a redirect location on codeberg.org under /repos/ or /users/, or NA.
cb_api_path <- function(location) {
  location <- .gl_chr(location)
  if (is.na(location)) return(NA_character_)
  m <- regmatches(location, regexec("^(https://codeberg\\.org)?/api/v1(/(repos|users)/.*)$", location))[[1]]
  if (!length(m)) return(NA_character_)
  path <- m[3]
  if (grepl("[[:space:][:cntrl:]\\\\]", path) || grepl("(^|/)\\.\\.?(/|[?#]|$)", path)) return(NA_character_)
  path
}

# Closes Codeberg for the run: no request is sent after this.
.cb_close <- function(st, why, msg) {
  if (!is.na(st$closed)) return(invisible(NULL))
  st$pacer$set_hourly_budget(CODEBERG_DOMAIN, 0)
  st$closed <- why
  .cb_log(st, msg)
}

# One paced GET. state is "ok" (200 with JSON), "absent" (404 or 409), "moved" (301, with
# `location`), "failed" or "stopped" (nothing sent). A 4xx or 301 is an answer, never a breaker
# failure; a 429 or an r under the floor closes Codeberg for the run and counts toward neither.
.cb_ask <- function(io, st, path) {
  out <- list(state = "stopped", json = NULL, headers = character(), location = NA_character_)
  if (!isTRUE(st$pacer$before(CODEBERG_DOMAIN))) return(out)
  st$asked <- st$asked + 1L
  r <- tryCatch(io$cb_get(path), error = function(e) list(status = NA_integer_, error = conditionMessage(e)))
  status <- .gl_int(.gl_at(r, "status"))
  hdr <- .gl_at(r, "headers")
  out$headers <- if (is.character(hdr)) hdr else character()
  rr <- cb_ratelimit_r(out$headers)
  if (!is.na(rr)) st$low_r <- if (is.na(st$low_r)) rr else min(st$low_r, rr)
  if (identical(status, 429L)) {
    .cb_close(st, "429", "Codeberg: HTTP 429, no more requests this run")
    return(out)
  }
  if (!is.na(rr) && rr < CODEBERG_RATELIMIT_FLOOR)
    .cb_close(st, "ratelimit", sprintf("Codeberg: ratelimit r=%d is under %d, no more requests this run",
                                       rr, CODEBERG_RATELIMIT_FLOOR))
  if (is.na(status) || status >= 500L) {
    st$pacer$after(CODEBERG_DOMAIN, FALSE)
    out$state <- "failed"
    return(out)
  }
  if (status == 200L) {
    # An error beside a 200 is a cut or unreadable body. parse_json never reads a body as a URL or file.
    body <- .gl_chr(.gl_at(r, "body"))
    json <- if (is.na(.gl_chr(.gl_at(r, "error"))) && !is.na(body))
      tryCatch(jsonlite::parse_json(body, simplifyVector = FALSE), error = function(e) NULL)
    st$pacer$after(CODEBERG_DOMAIN, !is.null(json))
    out$state <- if (is.null(json)) "failed" else "ok"
    out$json <- json
    return(out)
  }
  st$pacer$after(CODEBERG_DOMAIN, TRUE)
  if (status == 301L) {
    out$state <- "moved"
    out$location <- .cb_header(out$headers, "location")
  } else {
    out$state <- if (status %in% c(404L, 409L)) "absent" else "failed"
  }
  out
}

# A repository object with a whole numeric id and a full name, or NULL.
cb_repo_object <- function(x) {
  if (!is.list(x)) return(NULL)
  id <- x[["id"]]; fn <- x[["full_name"]]
  ok <- is.numeric(id) && length(id) == 1L && is.finite(id) && id >= 0 && id == round(id) &&
    is.character(fn) && length(fn) == 1L && !is.na(fn) && nzchar(fn)
  if (ok) x else NULL
}

cb_node_id <- function(id) paste0("codeberg:", CODEBERG_DOMAIN, ":", format(id, scientific = FALSE, trim = TRUE))
.CB_NODE_RE <- "^codeberg:([^:]+):[0-9]+$"
is_codeberg_node <- function(node_id)
  !is.na(node_id) & grepl(.CB_NODE_RE, node_id) & sub(.CB_NODE_RE, "\\1", node_id) == CODEBERG_DOMAIN

#' The active Codeberg repositories by repo_id, with their node id (NA until located), owner and
#' name. `con` holds the repos table.
codeberg_active_repos <- function(con) {
  rows <- DBI::dbGetQuery(con, sprintf(
    "SELECT repo_id, node_id, owner, name FROM repos WHERE %s AND status = 'active' ORDER BY repo_id",
    host_predicate(CODEBERG_HOSTS)))
  data.frame(repo_id = as.character(rows$repo_id), node_id = as.character(rows$node_id),
             owner = as.character(rows$owner), name = as.character(rows$name), stringsAsFactors = FALSE)
}

#' Finds each repository's object, and so its id, from its owner's listing; a repository missing
#' there is asked by path, following one 301 on codeberg.org when the id still matches. `repos` has
#' repo_id, node_id (NA when not yet known), owner and name. Returns `objects` (by node id), `found`
#' (repo_id, node_id), `ids` (resolve_node_ids' shape, for repositories that had no node id) and the
#' repo_ids `not_located` (asked and not found: never marked gone), `deferred` (a failed answer, or
#' Codeberg stopped for the run) and `unreached` (not asked by `deadline`).
cb_locate <- function(io, repos, st, deadline = Inf, now = function() as.numeric(Sys.time())) {
  enc <- function(x) utils::URLencode(x, reserved = TRUE)
  n <- nrow(repos)
  s <- new.env(parent = emptyenv())
  s$got <- rep(NA_character_, n); s$fate <- rep(NA_character_, n); s$objects <- list(); s$halt <- NA_character_
  # Every request goes through here: nothing is sent once the deadline passed or Codeberg stopped.
  ask <- function(path) {
    if (!is.na(s$halt)) return(list(state = "halt"))
    if (now() >= deadline) { s$halt <- "unreached"; return(list(state = "halt")) }
    a <- .cb_ask(io, st, path)
    if (a$state == "stopped") { s$halt <- "deferred"; return(list(state = "halt")) }
    a
  }
  take <- function(i, obj) {
    id <- cb_node_id(obj[["id"]])
    s$got[i] <- id
    if (is.null(s$objects[[id]])) s$objects[[id]] <- obj
  }
  not_located <- function(i, why) {
    s$fate[i] <- "not_located"
    .cb_log(st, sprintf("Codeberg %s: not located (%s), asked again next run", repos$repo_id[i], why))
  }
  # TRUE when obj may stand for repository i: i has no node id yet, or obj carries it.
  same_id <- function(i, obj) !is.null(obj) && (is.na(repos$node_id[i]) || identical(cb_node_id(obj[["id"]]), repos$node_id[i]))

  key <- tolower(repos$owner)
  by_path <- integer(0)
  for (k in sort(unique(key), method = "radix")) {
    rows <- which(key == k)
    login <- repos$owner[rows[1]]
    renamed <- FALSE; page <- 1L; pool <- list(); outcome <- "listed"
    repeat {
      if (page > CODEBERG_OWNER_PAGES) break
      a <- ask(sprintf("/users/%s/repos?limit=%d&page=%d", enc(login), CODEBERG_PAGE, page))
      if (a$state %in% c("halt", "failed")) { outcome <- a$state; break }
      if (a$state == "moved") {
        p <- cb_api_path(a$location)
        to <- if (is.na(p)) character() else regmatches(p, regexec("^/users/([A-Za-z0-9_.-]+)/repos([?].*)?$", p))[[1]]
        if (renamed || !length(to)) break
        login <- to[2]; renamed <- TRUE; page <- 1L; pool <- list()
        next
      }
      if (a$state == "absent") break
      items <- a$json
      # A listing is a JSON array; anything else is a failed answer.
      if (!is.list(items) || !is.null(names(items))) { outcome <- "failed"; break }
      pool <- c(pool, items)
      total <- cb_total_count(a$headers)
      more <- length(items) > 0L && if (is.na(total)) length(items) == CODEBERG_PAGE else length(pool) < total
      if (!more) break
      page <- page + 1L
    }
    if (outcome == "failed") {
      s$fate[rows] <- "deferred"
      .cb_log(st, sprintf("Codeberg %s: listing failed, %d repositories deferred", login, length(rows)))
      next
    }
    objs <- lapply(pool, cb_repo_object)
    bad <- vapply(objs, is.null, NA)
    if (any(bad)) .cb_log(st, sprintf("Codeberg %s: %d listed objects without an id or full name skipped", login, sum(bad)))
    objs <- objs[!bad]
    ids <- vapply(objs, function(o) cb_node_id(o[["id"]]), "")
    names_lc <- tolower(vapply(objs, function(o) o[["full_name"]], ""))
    for (i in rows) {
      # A repository with a node id matches only by it, so a new repository under an old name never does.
      hit <- if (!is.na(repos$node_id[i])) match(repos$node_id[i], ids)
             else match(tolower(paste(repos$owner[i], repos$name[i], sep = "/")), names_lc)
      if (!is.na(hit)) take(i, objs[[hit]]) else if (outcome != "halt") by_path <- c(by_path, i)
    }
    # Pages read before a stop are still used; nothing more is asked.
    if (outcome == "halt") break
  }

  # In the caller's order, so a deadline cut falls where its rotation puts it.
  for (i in sort(by_path)) {
    a <- ask(sprintf("/repos/%s/%s", enc(repos$owner[i]), enc(repos$name[i])))
    if (a$state == "halt") break
    if (a$state == "moved") {
      p <- cb_api_path(a$location)
      if (is.na(p) || !startsWith(p, "/repos/")) { not_located(i, "moved off the Codeberg API"); next }
      a <- ask(p)
      if (a$state == "halt") break
      if (a$state == "moved") { not_located(i, "moved twice"); next }
    }
    if (a$state == "failed") { s$fate[i] <- "deferred"; next }
    if (a$state == "absent") { not_located(i, "404"); next }
    obj <- cb_repo_object(a$json)
    if (is.null(obj)) { s$fate[i] <- "deferred"; next }
    if (same_id(i, obj)) take(i, obj) else not_located(i, "the path now holds another repository")
  }

  left <- is.na(s$got) & is.na(s$fate)
  if (any(left)) {
    s$fate[left] <- if (is.na(s$halt)) "deferred" else s$halt
    why <- if (identical(s$halt, "unreached")) "the deadline passed"
           else if (!is.na(st$closed)) "Codeberg closed for the run"
           else if (CODEBERG_DOMAIN %in% st$pacer$deferred()) "deferred after repeated failures"
           else "out of requests for this run"
    .cb_log(st, sprintf("Codeberg: %s, %d repositories left for the next run", why, sum(left)))
  }
  hit <- which(!is.na(s$got))
  fresh <- hit[is.na(repos$node_id[hit])]
  nwo <- unname(vapply(s$objects[s$got[fresh]], function(o) o[["full_name"]], ""))
  list(
    objects = s$objects,
    found = data.frame(repo_id = repos$repo_id[hit], node_id = s$got[hit], stringsAsFactors = FALSE),
    ids = data.frame(repo_id = repos$repo_id[fresh], node_id = s$got[fresh], owner = sub("/.*$", "", nwo),
                     name = sub("^[^/]*/", "", nwo), name_with_owner = nwo,
                     status = rep("active", length(fresh)), stringsAsFactors = FALSE),
    not_located = repos$repo_id[s$fate %in% "not_located"],
    deferred = repos$repo_id[s$fate %in% "deferred"],
    unreached = repos$repo_id[s$fate %in% "unreached"])
}
