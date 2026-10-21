# scripts/bitbucket_clone.R - the weekly Bitbucket pass: commit totals and commit identity counts from
# a treeless bare clone of each repository's default branch. The clone is the io member
# io$bb_clone(url, dir), returning list(status, output); local git commands are never io members.
# Anonymous git requests do not count toward Bitbucket's API limit, so clones need no budget; only
# the not-found fallback ask does. Uses .bb_ask, bb_ids_path, bb_path, .bb_uuid, bb_pacer (bitbucket.R),
# the git helpers of codeberg_clone.R, cb_run_state and .cb_log (codeberg.R), .gl_chr, .gl_int, .gl_at
# (gitlab.R), and .gl_redact and .gl_load_roster (gitlab_weekly.R).

BITBUCKET_ROSTER_DB <- "vcs-signals-roster-bitbucket.db"
BITBUCKET_SHARD_DB <- "vcs-signals-shard-bitbucket.db"
.BB_CLONE_URL_RE <- "^https://bitbucket\\.org/[^/]+/[^/]+\\.git$"

#' The roster fetch-bitbucket reads, in the Codeberg roster's schema.
write_bitbucket_roster <- function(path, roster_df) write_codeberg_roster(path, roster_df)

#' The real io member: the Codeberg clone with Bitbucket's filter. A URL off bitbucket.org is refused
#' without running git.
bb_git_clone <- function(url, dir, run = system2) {
  if (!grepl(.BB_CLONE_URL_RE, url)) return(list(status = 1L, output = "the clone URL is not on bitbucket.org"))
  .cb_partial_clone(url, dir, BITBUCKET_CLONE_FILTER, run)
}

.bb_close <- function(st, why, msg) {
  if (!is.na(st$closed)) return(invisible(NULL))
  st$pacer$set_hourly_budget(BITBUCKET_DOMAIN, 0)
  st$closed <- why
  .cb_log(st, msg)
}

.bb_clone_result <- function(state, commits = NA_integer_, ids = cb_identity_counts(character()))
  list(state = state, commits_total = commits, contributors_total = ids$contributors_total,
       top_contributor_commits = ids$top_contributor_commits)

# One clone into `dir`, counted. kind is "ok" (counts set), "gone" (git's answer about the repository:
# not found, private or refused), "failed" or "stopped" (a git 429 closed Bitbucket).
.bb_clone_once <- function(io, st, label, url, dir) {
  unlink(dir, recursive = TRUE, force = TRUE)
  r <- tryCatch(io$bb_clone(url, dir), error = function(e) list(status = 1L, output = conditionMessage(e)))
  out <- paste(as.character(r$output), collapse = " ")
  status <- .gl_int(r$status)
  if (grepl("returned error: 429", out, fixed = TRUE)) {
    .bb_close(st, "429", sprintf("::warning::Bitbucket %s: git clone got HTTP 429, no more requests this run", label))
    return(list(kind = "stopped"))
  }
  if (grepl("filtering not recognized by server", out, fixed = TRUE))
    .bb_close(st, "filter", sprintf(
      "::warning::Bitbucket %s: the server ignored the clone filter, so this clone was a full one and no more are made this run", label))
  if (!identical(status, 0L)) {
    gone <- grepl(.CB_GIT_ANSWER_RE, out)
    st$pacer$after(BITBUCKET_DOMAIN, gone)
    .cb_log(st, sprintf("Bitbucket %s: clone failed (exit %s): %s", label, status, .gl_redact(out)))
    return(list(kind = if (gone) "gone" else "failed"))
  }
  st$pacer$after(BITBUCKET_DOMAIN, TRUE)
  got <- .cb_local_counts(dir)
  if (!is.null(got$why)) {
    .cb_log(st, sprintf("Bitbucket %s: %s", label, got$why))
    return(list(kind = "failed"))
  }
  list(kind = "ok", total = got$total, ids = got$ids)
}

# The clone URL of "owner/name", or NA when it is not a plain pair.
.bb_clone_url <- function(owner, name) {
  if (is.na(bb_path(owner, name))) return(NA_character_)
  url <- sprintf("https://bitbucket.org/%s/%s.git", tolower(owner), tolower(name))
  if (grepl(.BB_CLONE_URL_RE, url)) url else NA_character_
}

#' One repository's commit total and identity counts. state is "ok" (counts set), "failed",
#' "not_located" (git said not found and the stored node id did not find it again), "stopped"
#' (Bitbucket is closed or deferred for the run, nothing cloned) or "skipped" (the fallback ask was
#' refused by the request cap). The clone directory is removed whatever happens.
bb_clone_counts <- function(io, repo, st, scratch) {
  label <- paste0(repo$owner, "/", repo$name)
  if (!is.na(st$closed) || BITBUCKET_DOMAIN %in% st$pacer$deferred()) return(.bb_clone_result("stopped"))
  url <- .bb_clone_url(repo$owner, repo$name)
  if (is.na(url)) {
    .cb_log(st, sprintf("Bitbucket %s: not a clone URL on bitbucket.org, not cloned", label))
    return(.bb_clone_result("not_located"))
  }
  dir <- tempfile("clone-", tmpdir = scratch)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  tryCatch({
    done <- function(c) if (c$kind == "ok") .bb_clone_result("ok", c$total, c$ids) else .bb_clone_result(c$kind)
    c1 <- .bb_clone_once(io, st, label, url, dir)
    if (c1$kind != "gone") return(done(c1))
    if (!is_bitbucket_node(repo$node_id)) return(.bb_clone_result("not_located"))
    a <- .bb_ask(io, st, paste0(bb_ids_path(repo$node_id), "?fields=full_name,uuid"))
    if (a$state == "stopped")
      return(.bb_clone_result(if (!is.na(st$closed) || BITBUCKET_DOMAIN %in% st$pacer$deferred()) "stopped" else "skipped"))
    if (a$state == "failed") {
      .cb_log(st, sprintf("Bitbucket %s: the id lookup got %s, not read", label, .cb_status(a)))
      return(.bb_clone_result("failed"))
    }
    same <- a$state == "ok" && identical(.bb_uuid(.gl_at(a$json, "uuid")), sub("^.*:", "", repo$node_id))
    url2 <- if (same) .bb_clone_url(sub("/.*$", "", .gl_chr(.gl_at(a$json, "full_name"))),
                                    sub("^.*/", "", .gl_chr(.gl_at(a$json, "full_name")))) else NA_character_
    if (is.na(url2)) return(.bb_clone_result("not_located"))
    done(.bb_clone_once(io, st, label, url2, dir))
  }, error = function(e) {
    .cb_log(st, sprintf("Bitbucket %s: not counted: %s", label, .gl_redact(conditionMessage(e))))
    .bb_clone_result("failed")
  })
}

# Why Bitbucket was closed for the rest of the run, or NA when it never was.
.bb_closed_why <- function(st) {
  if (!is.na(st$closed)) NA_character_
  else if (BITBUCKET_DOMAIN %in% st$pacer$deferred()) sprintf("%d failures in a row", BITBUCKET_BREAKER_AFTER)
  else NA_character_
}

#' The weekly Bitbucket job: clones each roster repository in repo_id order until the deadline or until
#' Bitbucket closes for the run. NA is written where nothing was read, which the merge skips. A git 429,
#' an ignored filter or repeated clone failures close Bitbucket with a workflow warning. Always writes
#' the snapshot shard with every roster repository, and removes the scratch root on every path. Keeps
#' no state. Returns the shard path, invisibly.
run_fetch_bitbucket <- function(io, out_dir, roster_path, now = function() as.numeric(Sys.time()), sleep = Sys.sleep,
                                deadline_min = BITBUCKET_WEEKLY_DEADLINE_MIN, scratch = tempfile("bb-clones-"),
                                log = message, run_budget = BITBUCKET_WEEKLY_MAX_REQUESTS) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(scratch, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
  start <- now()
  deadline <- start + 60 * deadline_min
  st <- cb_run_state(bb_pacer(now, sleep, run_budget = run_budget), log)

  roster <- tryCatch({
    if (!file.exists(roster_path)) stop("no roster file")
    .gl_load_roster(roster_path)[c("repo_id", "node_id", "owner", "name")]
  }, error = function(e) {
    log(sprintf("::warning::Bitbucket: the roster could not be read, so no repository is read this run: %s",
                .gl_redact(conditionMessage(e))))
    data.frame(repo_id = character(), node_id = character(), owner = character(), name = character(),
               stringsAsFactors = FALSE)
  })
  roster <- roster[order(roster$repo_id, method = "radix"), , drop = FALSE]
  n <- nrow(roster)
  log(sprintf("weekly fetch bitbucket: %d repos", n))
  v <- .cb_git(system2, "--version", stderr = TRUE, timeout = CODEBERG_LOCAL_GIT_TIMEOUT_S)
  log(if (identical(v$status, 0L)) paste("weekly fetch bitbucket:", .gl_redact(paste(v$output, collapse = " ")))
      else sprintf("weekly fetch bitbucket: git --version failed (exit %d)", v$status))

  acc <- new.env(parent = emptyenv())
  acc$state <- rep(NA_character_, n)
  acc$commits <- acc$contributors <- acc$top <- rep(NA_integer_, n)
  tryCatch({
    for (i in seq_len(n)) {
      if (now() >= deadline) {
        log(sprintf("Bitbucket: the deadline passed, %d repositories left for the next run", n - i + 1L))
        break
      }
      r <- tryCatch(bb_clone_counts(io, roster[i, , drop = FALSE], st, scratch), error = function(e) {
        log(sprintf("Bitbucket %s: not counted, stopped by a fault: %s", roster$repo_id[i], .gl_redact(conditionMessage(e))))
        .bb_clone_result("failed")
      })
      if (identical(r$state, "stopped")) break
      acc$state[i] <- r$state
      acc$commits[i] <- r$commits_total
      acc$contributors[i] <- r$contributors_total
      acc$top[i] <- r$top_contributor_commits
    }
  }, error = function(e) log(sprintf(
    "::warning::Bitbucket: cloning stopped by a fault, the repositories not read keep their last values: %s",
    .gl_redact(conditionMessage(e)))))

  rows <- data.frame(repo_id = roster$repo_id, commits_total = acc$commits, contributors_total = acc$contributors,
                     top_contributor_commits = acc$top, top_contributor_bot = rep(NA_integer_, n), stringsAsFactors = FALSE)
  shard_path <- file.path(out_dir, BITBUCKET_SHARD_DB)
  export_snapshot_shard(shard_path, rows)
  count <- function(s) sum(acc$state %in% s)
  unreached <- n - count(c("ok", "failed", "not_located"))
  why <- .bb_closed_why(st)
  if (!is.na(why))
    log(sprintf("::warning::Bitbucket: closed for the run (%s), %d repositories not reached keep last week's values",
                why, unreached))
  log(sprintf("weekly fetch bitbucket: %d read, %d failed, %d not located, %d not reached of %d repos, %d API requests, %d s",
              count("ok"), count("failed"), count("not_located"), unreached, n, st$asked,
              as.integer(round(now() - start))))
  invisible(shard_path)
}
