# scripts/codeberg_clone.R - the weekly Codeberg pass (weekly.R fetch-codeberg): commit totals and commit
# identity counts from a treeless bare clone of each repository's default branch. The clone is the io
# member io$cb_clone(url, dir), returning list(status, output); local git commands are never io members.
# Uses cb_run_state, cb_pacer, cb_locate, .cb_log, .cb_close and .cb_stop_why (codeberg.R), .gl_at,
# .gl_chr and .gl_int (gitlab.R), and .gl_redact and .gl_load_roster (gitlab_weekly.R).

CODEBERG_ROSTER_DB <- "vcs-signals-roster-codeberg.db"
CODEBERG_SHARD_DB <- "vcs-signals-shard-codeberg.db"

#' The roster fetch-codeberg reads, as weekly.R's enumerate writes it.
write_codeberg_roster <- function(path, roster_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, "CREATE TABLE roster (repo_id TEXT PRIMARY KEY, node_id TEXT NOT NULL,
    owner TEXT NOT NULL, name TEXT NOT NULL)")
  if (nrow(roster_df) > 0)
    DBI::dbWriteTable(con, "roster", roster_df[c("repo_id", "node_id", "owner", "name")], append = TRUE)
  invisible(path)
}

# Every git command runs with no prompt, no lazy fetch and no config file.
.CB_GIT_ENV <- c("GIT_TERMINAL_PROMPT=0", "GIT_NO_LAZY_FETCH=1", "GIT_CONFIG_GLOBAL=/dev/null", "GIT_CONFIG_NOSYSTEM=1")
.CB_CLONE_URL_RE <- "^https://codeberg\\.org/[^/]+/[^/]+\\.git$"
# Git's words for a missing, private or refused repository: an answer about it, never a breaker failure.
.CB_GIT_ANSWER_RE <- "repository '[^']*' not found|could not read Username|Authentication failed|returned error: (403|404|410)"

# Runs git under .CB_GIT_ENV. Returns list(status, output); a run that throws or times out (system2
# gives 124) has a non-zero status.
.cb_git <- function(run, args, stderr, timeout) {
  out <- tryCatch(suppressWarnings(run("git", args, stdout = TRUE, stderr = stderr, env = .CB_GIT_ENV, timeout = timeout)),
                  error = function(e) structure(conditionMessage(e), status = 1L))
  status <- attr(out, "status")
  status <- if (is.null(status)) 0L else suppressWarnings(as.integer(status[1]))
  list(status = if (is.na(status)) 1L else status, output = as.character(out))
}

#' The real io member: a treeless bare clone of the default branch under the project's User-Agent,
#' with no credential helper and no redirect followed. `output` is git's stdout and stderr.
cb_git_clone <- function(url, dir, run = system2) .cb_partial_clone(url, dir, CODEBERG_CLONE_FILTER, run)

# The clone both hosts use; only the filter is a parameter.
.cb_partial_clone <- function(url, dir, filter, run) {
  args <- c("-c", shQuote(paste0("http.userAgent=", CODEBERG_USER_AGENT)), "-c", "credential.helper=",
            "-c", "protocol.version=2", "-c", "http.followRedirects=false",
            "clone", "--bare", paste0("--filter=", filter), "--single-branch", "--no-tags", "--quiet",
            shQuote(url), shQuote(dir))
  .cb_git(run, args, stderr = TRUE, timeout = CODEBERG_CLONE_TIMEOUT_S)
}

#' git --git-dir <dir> <args> on a local clone. Returns list(status, lines), lines being stdout only.
cb_git_local <- function(dir, args, run = system2) {
  r <- .cb_git(run, c("--git-dir", shQuote(dir), args), stderr = FALSE, timeout = CODEBERG_LOCAL_GIT_TIMEOUT_S)
  list(status = r$status, lines = r$output)
}

#' The number of distinct `name<US>address` lines and the largest count of one, both NA for no
#' lines. Pure; only the two integers are returned.
cb_identity_counts <- function(lines) {
  if (!length(lines)) return(list(contributors_total = NA_integer_, top_contributor_commits = NA_integer_))
  n <- table(lines)
  list(contributors_total = length(n), top_contributor_commits = as.integer(max(n)))
}

.cb_clone_result <- function(state, commits = NA_integer_, ids = cb_identity_counts(character()))
  list(state = state, commits_total = commits, contributors_total = ids$contributors_total,
       top_contributor_commits = ids$top_contributor_commits)

#' One located repository's commit total and identity counts, from a clone of its default branch into
#' `scratch`, removed whatever happens. Returns list(state, commits_total, contributors_total,
#' top_contributor_commits), the values NA unless state is "ok". "failed" logs one line. "stopped"
#' is a git 429, which closes Codeberg for the run, or a run already closed, deferred or out of
#' requests, when nothing is cloned. A clone the server did not filter is counted and closes Codeberg
#' for the run.
cb_clone_counts <- function(io, obj, st, scratch) {
  name <- .gl_chr(.gl_at(obj, "full_name"))
  url <- .gl_chr(.gl_at(obj, "clone_url"))
  if (is.na(url) || !grepl(.CB_CLONE_URL_RE, url)) {
    .cb_log(st, sprintf("Codeberg %s: the clone URL is not on codeberg.org, not cloned", name))
    return(.cb_clone_result("failed"))
  }
  # A clone costs Codeberg about three requests.
  for (k in seq_len(CODEBERG_CLONE_SLOTS))
    if (!isTRUE(st$pacer$before(CODEBERG_DOMAIN))) { st$refused <- TRUE; return(.cb_clone_result("stopped")) }
  st$asked <- st$asked + CODEBERG_CLONE_SLOTS
  dir <- tempfile("clone-", tmpdir = scratch)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  tryCatch(.cb_clone_read(io, st, name, url, dir), error = function(e) {
    .cb_log(st, sprintf("Codeberg %s: not counted: %s", name, .gl_redact(conditionMessage(e))))
    .cb_clone_result("failed")
  })
}

.cb_clone_read <- function(io, st, name, url, dir) {
  r <- tryCatch(io$cb_clone(url, dir), error = function(e) list(status = 1L, output = conditionMessage(e)))
  out <- paste(as.character(r$output), collapse = " ")
  status <- .gl_int(r$status)
  if (grepl("returned error: 429", out, fixed = TRUE)) {
    .cb_close(st, "429", sprintf("Codeberg %s: git clone got HTTP 429, no more requests this run", name))
    return(.cb_clone_result("stopped"))
  }
  # Without the filter every clone is a whole repository: this one is still counted, and no more are made.
  if (grepl("filtering not recognized by server", out, fixed = TRUE))
    .cb_close(st, "filter", sprintf(
      "::warning::Codeberg %s: the server ignored the clone filter, so this clone was a full one and no more are made this run", name))
  if (!identical(status, 0L)) {
    st$pacer$after(CODEBERG_DOMAIN, grepl(.CB_GIT_ANSWER_RE, out))
    .cb_log(st, sprintf("Codeberg %s: clone failed (exit %s): %s", name, status, .gl_redact(out)))
    return(.cb_clone_result("failed"))
  }
  st$pacer$after(CODEBERG_DOMAIN, TRUE)
  got <- .cb_local_counts(dir)
  if (!is.null(got$why)) {
    .cb_log(st, sprintf("Codeberg %s: %s", name, got$why))
    return(.cb_clone_result("failed"))
  }
  .cb_clone_result("ok", got$total, got$ids)
}

# The commit total and identity counts of a cloned directory, list(total, ids), or list(why) naming
# what failed. The identity lines are counted here and dropped; nothing returned carries them.
.cb_local_counts <- function(dir) {
  n <- cb_git_local(dir, c("rev-list", "--count", "HEAD"))
  ok <- n$status == 0L && length(n$lines) == 1L && grepl("^[0-9]+$", n$lines)
  if (!ok) return(list(why = sprintf("git rev-list failed (exit %d)", n$status)))
  total <- as.integer(n$lines)
  who <- cb_git_local(dir, c("log", shQuote("--format=%an%x1f%ae"), "HEAD"))
  if (who$status != 0L || length(who$lines) != total)
    return(list(why = sprintf("git log failed (exit %d, %d of %d commits read)", who$status, length(who$lines), total)))
  list(total = total, ids = cb_identity_counts(who$lines))
}

# Clones the located repositories in roster order, into acc by roster row, until the deadline (checked
# before each clone) or until Codeberg stops for the run. A fault in one repository leaves it NA.
.cb_clone_all <- function(io, roster, objects, ids, st, scratch, deadline, now, acc) {
  todo <- which(!is.na(ids))
  for (i in todo) if (isTRUE(.gl_at(objects[[ids[i]]], "empty"))) acc$state[i] <- "empty"
  todo <- todo[is.na(acc$state[todo])]
  for (k in seq_along(todo)) {
    i <- todo[k]
    left <- length(todo) - k + 1L
    if (now() >= deadline) {
      .cb_log(st, sprintf("Codeberg: the deadline passed, %d repositories left for the next run", left))
      break
    }
    r <- tryCatch(cb_clone_counts(io, objects[[ids[i]]], st, scratch), error = function(e) {
      .cb_log(st, sprintf("Codeberg %s: not counted, stopped by a fault: %s", roster$repo_id[i], .gl_redact(conditionMessage(e))))
      .cb_clone_result("failed")
    })
    if (identical(r$state, "stopped")) {
      .cb_log(st, sprintf("Codeberg: %s, %d repositories left for the next run", .cb_stop_why(st), left))
      break
    }
    acc$state[i] <- r$state
    acc$commits[i] <- r$commits_total
    acc$contributors[i] <- r$contributors_total
    acc$top[i] <- r$top_contributor_commits
  }
  invisible(NULL)
}

# Why Codeberg was closed for the rest of the run, or NA when it never was.
.cb_closed_why <- function(st) {
  if (!is.na(st$closed))
    switch(st$closed, "429" = "HTTP 429", ratelimit = sprintf("ratelimit under %d", CODEBERG_RATELIMIT_FLOOR),
           filter = "the server ignored the clone filter", st$closed)
  else if (CODEBERG_DOMAIN %in% st$pacer$deferred()) sprintf("%d failures in a row", CODEBERG_BREAKER_AFTER)
  else if (isTRUE(st$refused)) "the run's request budget is spent"
  else NA_character_
}

#' The weekly Codeberg job. Locates the roster's repositories, then clones each located one that is not
#' empty, in repo_id order, until the deadline or until Codeberg stops for the run. A fault while
#' locating, while cloning or in one repository is logged and leaves NA where nothing was read, which
#' the merge skips. Codeberg closing for the run, or a run that reads none of a non-empty roster, is a
#' workflow warning. Always writes the snapshot shard with every roster repository, and removes the
#' scratch root on every path. Keeps no state. Returns the shard path, invisibly.
run_fetch_codeberg <- function(io, out_dir, roster_path, now = function() as.numeric(Sys.time()), sleep = Sys.sleep,
                               deadline_min = CODEBERG_WEEKLY_DEADLINE_MIN, scratch = tempfile("cb-clones-"),
                               log = message, run_budget = CODEBERG_WEEKLY_MAX_REQUESTS) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(scratch, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
  start <- now()
  deadline <- start + 60 * deadline_min
  st <- cb_run_state(cb_pacer(now, sleep, run_budget = run_budget), log)

  roster <- tryCatch({
    if (!file.exists(roster_path)) stop("no roster file")
    .gl_load_roster(roster_path)[c("repo_id", "node_id", "owner", "name")]
  }, error = function(e) {
    log(sprintf("::warning::Codeberg: the roster could not be read, so no repository is read this run: %s",
                .gl_redact(conditionMessage(e))))
    data.frame(repo_id = character(), node_id = character(), owner = character(), name = character(),
               stringsAsFactors = FALSE)
  })
  roster <- roster[order(roster$repo_id, method = "radix"), , drop = FALSE]
  n <- nrow(roster)
  log(sprintf("weekly fetch codeberg: %d repos", n))
  v <- .cb_git(system2, "--version", stderr = TRUE, timeout = CODEBERG_LOCAL_GIT_TIMEOUT_S)
  log(if (identical(v$status, 0L)) paste("weekly fetch codeberg:", .gl_redact(paste(v$output, collapse = " ")))
      else sprintf("weekly fetch codeberg: git --version failed (exit %d)", v$status))

  acc <- new.env(parent = emptyenv())
  acc$state <- rep(NA_character_, n)
  acc$commits <- acc$contributors <- acc$top <- rep(NA_integer_, n)
  loc <- tryCatch(cb_locate(io, roster, st, deadline = deadline, now = now), error = function(e) {
    log(sprintf("::warning::Codeberg: locating stopped by a fault, no repository is read this run: %s",
                .gl_redact(conditionMessage(e))))
    NULL
  })
  if (!is.null(loc))
    tryCatch(.cb_clone_all(io, roster, loc$objects, loc$found$node_id[match(roster$repo_id, loc$found$repo_id)],
                           st, scratch, deadline, now, acc),
             error = function(e) log(sprintf(
               "::warning::Codeberg: cloning stopped by a fault, the repositories not read keep their last values: %s",
               .gl_redact(conditionMessage(e)))))

  rows <- data.frame(repo_id = roster$repo_id, commits_total = acc$commits, contributors_total = acc$contributors,
                     top_contributor_commits = acc$top, top_contributor_bot = rep(NA_integer_, n), stringsAsFactors = FALSE)
  shard_path <- file.path(out_dir, CODEBERG_SHARD_DB)
  export_snapshot_shard(shard_path, rows)
  count <- function(s) sum(acc$state %in% s)
  lost <- if (is.null(loc)) 0L else sum(roster$repo_id %in% loc$not_located)
  unreached <- n - count(c("ok", "failed", "empty")) - lost
  why <- .cb_closed_why(st)
  if (!is.na(why))
    log(sprintf("::warning::Codeberg: closed for the run (%s), %d repositories not reached keep last week's values",
                why, unreached))
  if (n > 0L && count("ok") == 0L)
    log(sprintf("::warning::Codeberg: 0 of %d repositories read this run, every one keeps last week's values", n))
  log(sprintf(paste("weekly fetch codeberg: %d read, %d failed, %d empty, %d not located, %d not reached of %d repos,",
                    "%d requests and clone slots, lowest r %s, %d s"),
              count("ok"), count("failed"), count("empty"), lost, unreached, n,
              st$asked, format(st$low_r), as.integer(round(now() - start))))
  invisible(shard_path)
}
