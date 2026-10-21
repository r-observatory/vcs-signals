# scripts/codeberg.R - Codeberg (Forgejo) collectors: the transport, the pacer, locating repositories
# and the daily gauges. Pure functions behind an injected io: io$cb_get(path) returns an
# http_request() result for GET CODEBERG_API<path>. Anonymous, no token. Uses .gl_at, .gl_int,
# .gl_chr, .gl_flag and .gl_utc (gitlab.R), rows_df_empty_gauges and rotate_by_day (github.R).

# The real io member. curl runs without -L, so a 301 or 308 comes back as a status with its location.
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
# An answer's status for a log line.
.cb_status <- function(a) if (is.na(a$status)) "no answer" else sprintf("HTTP %d", a$status)
.cb_enc <- function(x) utils::URLencode(x, reserved = TRUE)

.cb_header <- function(headers, name) {
  if (!is.character(headers) || !name %in% names(headers)) return(NA_character_)
  unname(headers[[name]])
}

# r of the "baseline" policy in `ratelimit: "baseline";r=1999;t=600`, else of the first ratelimit
# header, or NA. A web filter answer also carries `"search";r=29`, which says nothing about our window.
cb_ratelimit_r <- function(headers) {
  if (!is.character(headers)) return(NA_integer_)
  h <- unname(headers[names(headers) %in% "ratelimit"])
  if (!length(h)) return(NA_integer_)
  base <- h[grepl('^[[:space:]]*"baseline"', h)]
  h <- if (length(base)) base[1] else h[1]
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

# One paced GET. state is "ok" (200 with JSON), "absent" (403, 404, 409 or 410: an answer about the
# repository, though cb_repo_extras takes fewer), "moved" (301 or 308, both permanent, with
# `location`), "failed" or "stopped" (nothing sent). A 4xx or 3xx is an answer, never a breaker failure; a 429 or an r under the floor closes
# Codeberg for the run and counts toward neither. `status` is the HTTP status, for log lines.
.cb_ask <- function(io, st, path) {
  out <- list(state = "stopped", json = NULL, headers = character(), location = NA_character_,
              status = NA_integer_)
  if (!isTRUE(st$pacer$before(CODEBERG_DOMAIN))) return(out)
  st$asked <- st$asked + 1L
  r <- tryCatch(io$cb_get(path), error = function(e) list(status = NA_integer_, error = conditionMessage(e)))
  status <- out$status <- .gl_int(.gl_at(r, "status"))
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
  if (status %in% c(301L, 308L)) {
    out$state <- "moved"
    out$location <- .cb_header(out$headers, "location")
  } else {
    out$state <- if (status %in% c(403L, 404L, 409L, 410L)) "absent" else "failed"
  }
  out
}

.cb_id_ok <- function(id) is.numeric(id) && length(id) == 1L && is.finite(id) && id >= 0 && id == round(id)

# A repository object with a whole numeric id and a full name, or NULL.
cb_repo_object <- function(x) {
  if (!is.list(x)) return(NULL)
  fn <- x[["full_name"]]
  ok <- .cb_id_ok(x[["id"]]) && is.character(fn) && length(fn) == 1L && !is.na(fn) && nzchar(fn)
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
#' there is asked by path, following one 301 or 308 on codeberg.org when the id still matches.
#' `repos` has repo_id, node_id (NA when not yet known), owner and name. Returns `objects` (by node
#' id), `found` (repo_id, node_id), `ids` (resolve_node_ids' shape, for repositories that had no node
#' id) and the repo_ids `not_located` (asked and not found: never marked gone), `deferred` (a failed
#' answer, or Codeberg stopped for the run) and `unreached` (not asked by `deadline`).
cb_locate <- function(io, repos, st, deadline = Inf, now = function() as.numeric(Sys.time())) {
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
  deferred <- function(i, why) {
    s$fate[i] <- "deferred"
    .cb_log(st, sprintf("Codeberg %s: deferred (%s), asked again next run", repos$repo_id[i], why))
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
      a <- ask(sprintf("/users/%s/repos?limit=%d&page=%d", .cb_enc(login), CODEBERG_PAGE, page))
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

  # In the caller's order, which is repo_id order: nothing rotates here.
  for (i in sort(by_path)) {
    a <- ask(sprintf("/repos/%s/%s", .cb_enc(repos$owner[i]), .cb_enc(repos$name[i])))
    if (a$state == "halt") break
    if (a$state == "moved") {
      p <- cb_api_path(a$location)
      if (is.na(p) || !startsWith(p, "/repos/")) { not_located(i, "moved off the Codeberg API"); next }
      a <- ask(p)
      if (a$state == "halt") break
      if (a$state == "moved") { not_located(i, "moved twice"); next }
    }
    if (a$state == "failed") { deferred(i, .cb_status(a)); next }
    if (a$state == "absent") { not_located(i, sprintf("HTTP %d", a$status)); next }
    obj <- cb_repo_object(a$json)
    if (is.null(obj)) { deferred(i, "HTTP 200, not a repository"); next }
    if (same_id(i, obj)) take(i, obj) else not_located(i, "the path now holds another repository")
  }

  left <- is.na(s$got) & is.na(s$fate)
  if (any(left)) {
    s$fate[left] <- if (is.na(s$halt)) "deferred" else s$halt
    why <- if (identical(s$halt, "unreached")) "the deadline passed" else .cb_stop_why(st)
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

# ---- daily gauges --------------------------------------------------------------
# "/repos/<owner>/<name>" for a full name, each segment encoded.
.cb_repo_path <- function(full_name)
  sprintf("/repos/%s/%s", .cb_enc(sub("/.*$", "", full_name)), .cb_enc(sub("^[^/]*/", "", full_name)))
.cb_is_array <- function(x) is.list(x) && is.null(names(x))

# Why Codeberg sends nothing more this run, for log lines.
.cb_stop_why <- function(st) {
  if (!is.na(st$closed)) "Codeberg closed for the run"
  else if (CODEBERG_DOMAIN %in% st$pacer$deferred()) "deferred after repeated failures"
  else "out of requests for this run"
}

# A unit counts only when the object says it is on; issues also need the internal tracker.
.cb_issues_on <- function(obj) isTRUE(.gl_at(obj, "has_issues")) && is.null(.gl_at(obj, "external_tracker"))
.cb_prs_on <- function(obj) isTRUE(.gl_at(obj, "has_pull_requests"))
.cb_releases_on <- function(obj) isTRUE(.gl_at(obj, "has_releases"))

# The first CODEBERG_TOPICS_MAX topics joined with ",", "" for none, NA unless an array of strings.
.cb_topics <- function(x) {
  if (!.cb_is_array(x) || !all(vapply(x, function(t) is.character(t) && length(t) == 1L && !is.na(t), NA)))
    return(NA_character_)
  paste(utils::head(as.character(unlist(x)), CODEBERG_TOPICS_MAX), collapse = ",")
}

#' What a repository's extra calls fill, all NA until read.
cb_extras_na <- function()
  list(issues_closed = NA_integer_, prs_closed = NA_integer_, prs_merged = NA_integer_,
       last_release_at = NA_character_, last_release_tag = NA_character_, head_committed_at = NA_character_)

#' One gauge row (the columns of rows_df_empty_gauges) from a repository object and its extras.
#' Pure. A field absent or of the wrong type is NA, and so is every value of a disabled unit.
cb_gauge_row <- function(obj, extras = cb_extras_na()) {
  at <- function(k) .gl_at(obj, k)
  ex <- function(k) .gl_at(extras, k)
  issues <- .cb_issues_on(obj); prs <- .cb_prs_on(obj)
  id <- at("id")
  # A migrated or mirrored repository's created_at is the day it came to Codeberg, so the date is
  # kept only when both fields are readable and say it was not moved.
  url <- .gl_chr(at("original_url")); mir <- .gl_flag(at("mirror"))
  native <- !is.na(url) && !is.na(mir) && !nzchar(url) && mir == 0L
  data.frame(
    node_id = if (.cb_id_ok(id)) cb_node_id(id) else NA_character_,
    name_with_owner = .gl_chr(at("full_name")),
    owner_login = NA_character_, owner_type = NA_character_, owner_node_id = NA_character_,
    stars = .gl_int(at("stars_count")), forks = .gl_int(at("forks_count")), watchers = .gl_int(at("watchers_count")),
    issues_open = if (issues) .gl_int(at("open_issues_count")) else NA_integer_,
    issues_closed = if (issues) .gl_int(ex("issues_closed")) else NA_integer_,
    prs_open = if (prs) .gl_int(at("open_pr_counter")) else NA_integer_,
    prs_closed = if (prs) .gl_int(ex("prs_closed")) else NA_integer_,
    prs_merged = if (prs) .gl_int(ex("prs_merged")) else NA_integer_,
    releases_total = if (.cb_releases_on(obj)) .gl_int(at("release_counter")) else NA_integer_,
    size_kb = .gl_int(at("size")),
    license = NA_character_,
    topics = .cb_topics(at("topics")),
    is_archived = .gl_flag(at("archived")), is_fork = .gl_flag(at("fork")), is_mirror = .gl_flag(at("mirror")),
    created_at = if (native) .gl_utc(at("created_at")) else NA_character_,
    pushed_at = NA_character_,
    last_release_at = .gl_chr(ex("last_release_at")), last_release_tag = .gl_chr(ex("last_release_tag")),
    head_committed_at = .gl_chr(ex("head_committed_at")),
    stringsAsFactors = FALSE)
}

#' A repository's values beyond its object: closed issues, the closed and merged PR split, the
#' newest release and the newest commit date, each asked only when its unit is on. state is "ok",
#' or the "failed" or "stopped" of the first call that was, after which nothing more is asked.
#' Besides a 200, only a 404 from releases/latest (every release is a pre-release) and a 409 from
#' the commit list (an empty repository) are answers, and they leave their values NA.
cb_repo_extras <- function(io, obj, st) {
  ex <- cb_extras_na()
  end <- function(a) list(state = a$state, extras = ex)
  if (is.null(cb_repo_object(obj))) return(end(list(state = "failed")))
  base <- .cb_repo_path(obj[["full_name"]])
  # One call. A move, a 200 of the wrong shape, a list without its count or a 4xx other than
  # `answer` is a failed answer, so a 403 never publishes NA over a known value.
  ask <- function(path, shape, answer = NA_integer_) {
    a <- .cb_ask(io, st, path)
    if (a$state == "ok" && !switch(shape,
          count = .cb_is_array(a$json) && !is.na(cb_total_count(a$headers)),
          array = .cb_is_array(a$json),
          object = is.list(a$json) && !is.null(names(a$json))))
      a$state <- "failed"
    if (a$state == "moved" || (a$state == "absent" && !identical(a$status, answer))) a$state <- "failed"
    if (a$state == "failed")
      .cb_log(st, sprintf("Codeberg %s: %s, the repository is deferred", path, .cb_status(a)))
    a
  }
  halts <- function(a) a$state %in% c("failed", "stopped")

  if (.cb_issues_on(obj)) {
    a <- ask(paste0(base, "/issues?state=closed&type=issues&limit=1"), "count")
    if (halts(a)) return(end(a))
    ex$issues_closed <- cb_total_count(a$headers)
  }
  if (.cb_prs_on(obj)) {
    merged <- 0L; read <- 0L; total <- NA_integer_
    for (page in seq_len(CODEBERG_PR_PAGES)) {
      a <- ask(sprintf("%s/issues?state=closed&type=pulls&limit=%d&page=%d", base, CODEBERG_PAGE, page), "count")
      if (halts(a)) return(end(a))
      total <- cb_total_count(a$headers)
      flags <- vapply(a$json, function(r) .gl_flag(.gl_at(r, "pull_request", "merged")), NA_integer_)
      # Past the page cap, or a row without its merged flag: no split rather than a partial one.
      if (total > CODEBERG_PAGE * CODEBERG_PR_PAGES || anyNA(flags)) { total <- NA_integer_; break }
      merged <- merged + sum(flags); read <- read + length(flags)
      if (read >= total || !length(flags)) break
    }
    if (!is.na(total) && read == total) { ex$prs_merged <- merged; ex$prs_closed <- total - merged }
  }
  if (.cb_releases_on(obj) && isTRUE(.gl_int(.gl_at(obj, "release_counter")) > 0L)) {
    a <- ask(paste0(base, "/releases/latest"), "object", answer = 404L)
    if (halts(a)) return(end(a))
    if (a$state == "ok") {
      ex$last_release_tag <- .gl_chr(.gl_at(a$json, "tag_name"))
      ex$last_release_at <- .gl_utc(.gl_at(a$json, "published_at"))
    }
  }
  if (!isTRUE(.gl_at(obj, "empty"))) {
    a <- ask(paste0(base, "/commits?limit=1&stat=false&verification=false&files=false"), "array", answer = 409L)
    if (halts(a)) return(end(a))
    if (a$state == "ok") ex$head_committed_at <- .gl_utc(.gl_at(a$json, 1L, "commit", "committer", "date"))
  }
  list(state = "ok", extras = ex)
}

#' Daily gauges for located repositories. `objects` is cb_locate's objects, by node id. Visits them
#' in rotate_by_day order and checks the deadline before each repository, never inside one.
#' Returns collect_gauges' list: `snapshot` (the gauge frame, or NULL), `deferred` (a call failed,
#' or Codeberg stopped for the run: no row, so the repository keeps its last values),
#' `unresolvable` (always empty: a Codeberg repository is never marked gone) and `unreached`.
cb_collect_gauges <- function(io, objects, st, deadline = Inf, now = function() as.numeric(Sys.time()),
                              today = Sys.Date()) {
  ids <- rotate_by_day(sort(as.character(names(objects)), method = "radix"), today)
  rows <- list(); deferred <- character(0); unreached <- character(0); held <- 0L
  for (i in seq_along(ids)) {
    if (held) { deferred <- c(deferred, ids[i]); held <- held + 1L; next }
    if (now() >= deadline) { unreached <- ids[i:length(ids)]; break }
    a <- cb_repo_extras(io, objects[[ids[i]]], st)
    if (a$state == "ok") { rows[[length(rows) + 1L]] <- cb_gauge_row(objects[[ids[i]]], a$extras); next }
    deferred <- c(deferred, ids[i])
    if (a$state == "stopped") held <- 1L
  }
  if (held) .cb_log(st, sprintf("Codeberg: %s, %d repositories left for the next run", .cb_stop_why(st), held))
  if (length(unreached))
    .cb_log(st, sprintf("Codeberg: the deadline passed, %d repositories left for the next run", length(unreached)))
  list(snapshot = if (length(rows)) do.call(rbind, rows) else NULL, deferred = deferred,
       unresolvable = character(0), unreached = unreached)
}

#' NA count per gauge column Codeberg can fill, for the log: a renamed Forgejo field shows as a jump
#' in one count. The columns that are always NA are left out.
cb_na_counts <- function(snapshot) {
  never <- c("owner_login", "owner_type", "owner_node_id", "license", "pushed_at")
  vapply(setdiff(names(rows_df_empty_gauges()), never), function(k)
    if (k %in% names(snapshot)) sum(is.na(snapshot[[k]])) else NROW(snapshot), integer(1))
}
