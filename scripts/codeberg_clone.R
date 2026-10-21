# scripts/codeberg_clone.R - the weekly Codeberg pass: commit totals and commit identity counts from a
# treeless bare clone of each repository's default branch. The clone is the io member
# io$cb_clone(url, dir), returning list(status, output); local git commands are never io members.
# Uses cb_run_state, cb_pacer, .cb_log and .cb_close (codeberg.R), .gl_at, .gl_chr and .gl_int
# (gitlab.R) and .gl_redact (gitlab_weekly.R).

CODEBERG_ROSTER_DB <- "vcs-signals-roster-codeberg.db"
CODEBERG_SHARD_DB <- "vcs-signals-shard-codeberg.db"

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
cb_git_clone <- function(url, dir, run = system2) {
  args <- c("-c", shQuote(paste0("http.userAgent=", CODEBERG_USER_AGENT)), "-c", "credential.helper=",
            "-c", "protocol.version=2", "-c", "http.followRedirects=false",
            "clone", "--bare", paste0("--filter=", CODEBERG_CLONE_FILTER), "--single-branch", "--no-tags", "--quiet",
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
#' requests, when nothing is cloned.
cb_clone_counts <- function(io, obj, st, scratch) {
  name <- .gl_chr(.gl_at(obj, "full_name"))
  url <- .gl_chr(.gl_at(obj, "clone_url"))
  if (is.na(url) || !grepl(.CB_CLONE_URL_RE, url)) {
    .cb_log(st, sprintf("Codeberg %s: the clone URL is not on codeberg.org, not cloned", name))
    return(.cb_clone_result("failed"))
  }
  # A clone costs Codeberg about three requests.
  for (k in seq_len(CODEBERG_CLONE_SLOTS))
    if (!isTRUE(st$pacer$before(CODEBERG_DOMAIN))) return(.cb_clone_result("stopped"))
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
  if (!identical(status, 0L)) {
    st$pacer$after(CODEBERG_DOMAIN, grepl(.CB_GIT_ANSWER_RE, out))
    .cb_log(st, sprintf("Codeberg %s: clone failed (exit %s): %s", name, status, .gl_redact(out)))
    return(.cb_clone_result("failed"))
  }
  st$pacer$after(CODEBERG_DOMAIN, TRUE)
  if (grepl("filtering not recognized by server", out, fixed = TRUE))
    .cb_log(st, sprintf("Codeberg %s: the server ignored the clone filter, so this clone was a full one", name))
  n <- cb_git_local(dir, c("rev-list", "--count", "HEAD"))
  ok <- n$status == 0L && length(n$lines) == 1L && grepl("^[0-9]+$", n$lines)
  if (!ok) {
    .cb_log(st, sprintf("Codeberg %s: git rev-list failed (exit %d)", name, n$status))
    return(.cb_clone_result("failed"))
  }
  total <- as.integer(n$lines)
  # The identity lines are counted here and dropped; no log line ever carries them.
  who <- cb_git_local(dir, c("log", shQuote("--format=%an%x1f%ae"), "HEAD"))
  if (who$status != 0L || length(who$lines) != total) {
    .cb_log(st, sprintf("Codeberg %s: git log failed (exit %d, %d of %d commits read)",
                        name, who$status, length(who$lines), total))
    return(.cb_clone_result("failed"))
  }
  .cb_clone_result("ok", total, cb_identity_counts(who$lines))
}
