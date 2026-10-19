# scripts/gitlab_weekly.R - the weekly GitLab pass (weekly.R fetch-gitlab): commit totals over GraphQL,
# contributors and licences over REST in turn under each host's hourly budget, from a cursor per domain
# that rides on the release as one more asset. The collectors are in gitlab.R.

GITLAB_SHARD_DB <- "vcs-signals-shard-gitlab.db"
GITLAB_STATE_DB <- "vcs-signals-gitlab-state.db"

#' Each domain's last repository visited, named by domain, from the published state. A download that
#' fails or finds none, or a file that cannot be read, starts every host at its first repository.
read_gitlab_cursors <- function(io, state_path, log = message) {
  none <- stats::setNames(character(0), character(0))
  got <- tryCatch(isTRUE(io$download(basename(state_path), dirname(state_path))), error = function(e) FALSE)
  if (!got || !file.exists(state_path)) {
    log("GitLab: no cursor state downloaded, every host starts at its first repository")
    return(none)
  }
  cur <- tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), state_path)
    tryCatch(DBI::dbReadTable(con, "cursor"), finally = DBI::dbDisconnect(con))
  }, error = function(e) NULL)
  if (is.null(cur)) {
    log("GitLab: the cursor state could not be read, every host starts at its first repository")
    return(none)
  }
  cur <- cur[!is.na(cur$domain) & !is.na(cur$last_repo_id), , drop = FALSE]
  stats::setNames(as.character(cur$last_repo_id), cur$domain)
}

write_gitlab_cursors <- function(path, cursors) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, "CREATE TABLE cursor (domain TEXT PRIMARY KEY, last_repo_id TEXT NOT NULL)")
  if (length(cursors))
    DBI::dbWriteTable(con, "cursor", data.frame(domain = names(cursors), last_repo_id = unname(cursors),
                                                stringsAsFactors = FALSE), append = TRUE)
  invisible(path)
}

#' One host's repo_ids in visiting order: those after `cursor`, then from the first. The cursor's
#' repository need not still be on the roster. Pure.
gitlab_rotation <- function(ids, cursor = NA_character_) {
  ids <- sort(unique(ids), method = "radix")
  if (is.na(cursor) || !length(ids)) return(ids)
  ranks <- sort(unique(c(ids, cursor)), method = "radix")
  k <- sum(match(ids, ranks) <= match(cursor, ranks))
  c(ids[seq_along(ids) > k], ids[seq_len(k)])
}

# Why a host can take no further repository now: "closed" by a 429 or ratelimit-remaining 0,
# "deferred" by the breaker, "hour" when this hour's budget cannot cover one more, or NA.
.gl_rest_block <- function(pacer, key) {
  if (key %in% pacer$deferred()) return("deferred")
  limit <- pacer$hourly_budget(key)
  if (limit <= 0) return("closed")
  if (limit - pacer$spent(key) < min(GITLAB_REST_CALLS_PER_REPO, limit)) return("hour")
  NA_character_
}

# Visits one host's queue from acc$pos[[d]] until the host cannot go on, and says why: "done",
# "hour", "closed", "deferred" or "deadline". Values go into acc by roster row.
.gl_rotate <- function(io, d, queue, roster, pacer, deadline, now, acc) {
  key <- .gl_key(d)
  while (acc$pos[[d]] <= length(queue)) {
    block <- .gl_rest_block(pacer, key)
    if (!is.na(block)) return(block)
    if (now() >= deadline) return("deadline")
    i <- match(queue[acc$pos[[d]]], roster$repo_id)
    cn <- gl_contributors(io, d, roster$project_id[i], pacer)
    li <- if (cn$stopped) NULL else gl_license(io, d, roster$project_id[i], pacer)
    if (!is.na(cn$total)) acc$total[i] <- cn$total
    if (!is.na(cn$top_commits)) acc$top[i] <- cn$top_commits
    if (isTRUE(li$read)) {
      acc$license[i] <- gl_license_spdx(li$key); acc$license_read[i] <- TRUE
      if (!is.na(li$key) && is.na(acc$license[i])) acc$unmapped <- c(acc$unmapped, li$key)
    }
    # Visited when every call got an answer of any kind. No repository starts on a deferred host, so a
    # call the breaker refused here came after this repository's own failures, and does not hold it back.
    visited <- cn$answered && !is.null(li) && (li$answered || key %in% pacer$deferred())
    if (visited) { acc$cursors[[d]] <- roster$repo_id[i]; acc$n_visited[[d]] <- acc$n_visited[[d]] + 1L }
    # A repository this hour's budget cut short is asked again after the hour.
    if (visited || !identical(.gl_rest_block(pacer, key), "hour")) acc$pos[[d]] <- acc$pos[[d]] + 1L
  }
  "done"
}

.gl_rotation_note <- function(d, why, n_visited, n) sprintf("GitLab %s: %d of %d repositories visited, %s",
  d, n_visited, n,
  switch(why, done = "end of the roster", closed = "rate limited", deferred = "deferred after repeated failures",
         deadline = "past the deadline", hour = "out of requests for this hour and no time for the next",
         fault = "stopped by a fault"))

# An error's text for the log, without anything shaped like an address.
.gl_redact <- function(x) substr(gsub("[^[:space:]]*@[^[:space:]]*", "<redacted>", x), 1L, 200L)

.gl_load_roster <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbReadTable(con, "roster")
}

#' The weekly GitLab job. Commit totals come over GraphQL for every roster row. Contributors and the
#' licence are read per host in repo_id order from its cursor, wrapping once, until the host is closed
#' by a 429 or ratelimit-remaining 0, deferred by the breaker, through its repositories, or past the
#' deadline. A repository is visited when each of its calls got an answer of any kind, and the cursor
#' moves to the last one visited; values from one partly read are kept. Only when every host left has
#' spent this hour's budget does the run sleep to the next UTC hour, if the deadline allows. A fault in
#' one host's collection ends only that host. Writes the snapshot shard with a `license` table of the
#' licences a 200 answered this run, and the cursors to state_path, where the published state is
#' downloaded first.
run_fetch_gitlab <- function(io, out_dir, roster_path, state_path,
                             now = function() as.numeric(Sys.time()), sleep = Sys.sleep,
                             rest_pacer = gl_rest_pacer(now, sleep), graphql_pacer = gl_pacer(now, sleep),
                             deadline_min = GITLAB_WEEKLY_DEADLINE_MIN, log = message) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(state_path), showWarnings = FALSE, recursive = TRUE)
  deadline <- now() + 60 * deadline_min
  roster <- .gl_load_roster(roster_path)
  roster <- roster[order(roster$repo_id, method = "radix"), , drop = FALSE]
  n <- nrow(roster)
  hosts <- sort(unique(roster$domain), method = "radix")
  log(sprintf("weekly fetch gitlab: %d repos on %d hosts", n, length(hosts)))

  commits <- tryCatch(
    gl_commit_counts(io, roster[c("node_id", "repo_id", "domain")], graphql_pacer)$commits_total,
    error = function(e) {
      log(paste("GitLab: commit totals not read:", .gl_redact(conditionMessage(e))))
      rep(NA_integer_, n)
    })

  acc <- new.env(parent = emptyenv())
  acc$total <- rep(NA_integer_, n); acc$top <- rep(NA_integer_, n)
  acc$license <- rep(NA_character_, n); acc$license_read <- rep(FALSE, n)
  acc$unmapped <- character()
  acc$cursors <- read_gitlab_cursors(io, state_path, log)
  acc$pos <- stats::setNames(rep(1L, length(hosts)), hosts)
  acc$n_visited <- stats::setNames(rep(0L, length(hosts)), hosts)
  queues <- lapply(stats::setNames(hosts, hosts), function(d)
    gitlab_rotation(roster$repo_id[roster$domain == d], acc$cursors[d]))

  open <- hosts
  while (length(open)) {
    waiting <- character(0)
    for (d in open) {
      why <- tryCatch(.gl_rotate(io, d, queues[[d]], roster, rest_pacer, deadline, now, acc), error = function(e) {
        log(sprintf("GitLab %s: collection stopped by a fault: %s", d, .gl_redact(conditionMessage(e))))
        "fault"
      })
      if (identical(why, "hour")) waiting <- c(waiting, d)
      else log(.gl_rotation_note(d, why, acc$n_visited[[d]], length(queues[[d]])))
    }
    open <- waiting
    if (!length(open)) break
    # A host whose hour turned while the others ran goes again before anyone sleeps.
    if (any(vapply(open, function(d) !identical(.gl_rest_block(rest_pacer, .gl_key(d)), "hour"), NA))) next
    wake <- (floor(now() / 3600) + 1) * 3600 + 1
    if (wake >= deadline) {
      for (d in open) log(.gl_rotation_note(d, "hour", acc$n_visited[[d]], length(queues[[d]])))
      break
    }
    log(sprintf("GitLab: %s out of requests for this hour, waiting %d s for the next",
                paste(open, collapse = ", "), as.integer(ceiling(wake - now()))))
    sleep(wake - now())
  }

  rows <- data.frame(repo_id = roster$repo_id, commits_total = as.integer(commits),
                     contributors_total = acc$total, top_contributor_commits = acc$top,
                     top_contributor_bot = rep(NA_integer_, n), stringsAsFactors = FALSE)
  shard_path <- file.path(out_dir, GITLAB_SHARD_DB)
  export_snapshot_shard(shard_path, rows)
  read <- acc$license_read
  con <- DBI::dbConnect(RSQLite::SQLite(), shard_path)
  DBI::dbExecute(con, "CREATE TABLE license (repo_id TEXT PRIMARY KEY, license TEXT, read_on TEXT NOT NULL)")
  if (any(read))
    DBI::dbWriteTable(con, "license", data.frame(repo_id = roster$repo_id[read], license = acc$license[read],
                                                 read_on = format(Sys.Date()), stringsAsFactors = FALSE), append = TRUE)
  DBI::dbDisconnect(con)
  write_gitlab_cursors(state_path, acc$cursors)
  # A licence key names no one, so it can be logged.
  for (k in sort(unique(acc$unmapped), method = "radix"))
    log(paste("gitlab license key not mapped:", substr(gsub("[^[:alnum:]._+-]", "?", k), 1L, 60L)))
  log(sprintf("weekly fetch gitlab: commits %d, contributors %d, licences %d of %d repos",
              sum(!is.na(rows$commits_total)), sum(!is.na(acc$total)), sum(read), n))
  shard_path
}
