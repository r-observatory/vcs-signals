#!/usr/bin/env Rscript
# scripts/weekly.R - sharded weekly commit-count + contributor-count
# collection for vcs-signals.
#
# Same three sub-commands as scripts/backfill.R, wired together by CI:
#   enumerate -> reuse backfill's roster build (every github repo with any
#                signal) - one job
#   fetch     -> for one even mod-N shard of the roster, collect
#                commits_total (GraphQL history.totalCount, batched),
#                contributors_total (REST /contributors count), and the
#                three responsiveness medians (median_days_to_close_issue,
#                median_days_to_close_pr, median_open_issue_age_days) -
#                matrix job
#   fetch-gitlab -> the GitLab roster's commit totals, contributors and
#                licences, read in turn under each host's REST budget - one job
#   merge     -> fold every shard's snapshot into the published series as a
#                change-only weekly point dated today, rebuild the summary,
#                and republish - one job
#
# Both collected values are current-value snapshots, not reconstructed
# history: commit history is too large to page and contributor history is
# not exposed by any API, so these two metrics only ever accrue forward from
# whenever a weekly run first sees a repo.
if (!exists("STARGAZER_PAGE"))      source("scripts/config.R")
if (!exists("host_predicate"))   source("scripts/hosts.R")
if (!exists("http_request"))   source("scripts/http.R")
if (!exists("host_pacer"))     source("scripts/pacing.R")
if (!exists("ensure_series_schema")) source("scripts/helpers.R")
if (!exists("build_gauge_query"))    source("scripts/github.R")
if (!exists("gl_collect_gauges"))    source("scripts/gitlab.R")
if (!exists("gh_release_exists"))    source("scripts/update.R")  # default_io, gh_release_*, seed_working_db
if (!exists("run_enumerate"))        source("scripts/backfill.R") # run_enumerate, write_roster, load_roster
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

SNAPSHOT_TABLE <- "snapshot"

# ---- snapshot shard IO ------------------------------------------------------
#' Write a shard's collected (repo_id, commits_total, contributors_total)
#' rows to a fresh SQLite file. Either numeric column may be NA for a given
#' repo (a fetch that failed or was skipped this run).
export_snapshot_shard <- function(path, rows) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  # A metric the rows do not carry is stored NA, as a fetch that failed would be.
  for (m in setdiff(WEEKLY_METRICS, names(rows))) rows[[m]] <- rep(NA_integer_, nrow(rows))
  cols <- paste(sprintf("%s INTEGER", WEEKLY_METRICS), collapse = ", ")
  DBI::dbExecute(con, sprintf("CREATE TABLE %s (repo_id TEXT PRIMARY KEY, %s)", SNAPSHOT_TABLE, cols))
  if (nrow(rows) > 0) {
    DBI::dbWriteTable(con, SNAPSHOT_TABLE, rows[c("repo_id", WEEKLY_METRICS)], append = TRUE)
  }
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

# ---- fetch ------------------------------------------------------------------
#' One contributors reply as list(count, top_commits, top_type). A bare count, the
#' older reply shape, has no top contributor. Pure.
contributor_reply <- function(v) {
  none <- list(count = NA_integer_, top_commits = NA_integer_, top_type = NA_character_)
  if (is.null(v)) return(none)
  one <- function(x, cast, na) { x <- suppressWarnings(cast(x)); if (length(x) == 1L) x else na }
  if (!is.list(v)) return(utils::modifyList(none, list(count = one(v, as.integer, NA_integer_))))
  list(count = one(v$count, as.integer, NA_integer_),
       top_commits = one(v$top_commits, as.integer, NA_integer_),
       top_type = one(v$top_type, as.character, NA_character_))
}

#' Repositories whose snapshot row read a top contributor's commits but no Bot or
#' User account type: an anonymous contributor, or a type GitHub did not give. Pure.
untyped_top_contributors <- function(snapshot) {
  snapshot$repo_id[!is.na(snapshot$top_contributor_commits) & is.na(snapshot$top_contributor_bot)]
}

#' Collect commits_total, contributors_total, and the three responsiveness
#' medians (median_days_to_close_issue, median_days_to_close_pr,
#' median_open_issue_age_days) for one even mod-N shard of the roster.
#' Commits are swept in COMMIT_HISTORY_BATCH-sized chunks through one
#' aliased GraphQL query per chunk (fetch_commit_counts): a chunk whose
#' query itself fails leaves every repo in that chunk NA for commits_total
#' this run (left for a re-run), while every other chunk is unaffected.
#' Contributors are fetched one REST request per repo (io$contributors,
#' paced by contributor_delay): a failing repo is NA for contributors_total
#' only, independent of whether its commit count succeeded. The three
#' medians are swept in MEDIAN_BATCH-sized chunks through fetch_responsiveness,
#' the same way commits are: a chunk whose query fails leaves every repo in
#' that chunk NA for all three medians this run. No failure mode ever aborts
#' the shard. Writes a partial `snapshot` table to out/vcs-signals-shard-<i>.db.
run_fetch_shard <- function(io, out_dir, roster_path, i, N,
                            commit_delay = BACKFILL_DELAY_S,
                            contributor_delay = CONTRIBUTOR_DELAY_S,
                            batch_size = COMMIT_HISTORY_BATCH) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  roster <- load_roster(roster_path)
  mine <- roster[shard_rows(nrow(roster), i, N), , drop = FALSE]
  total <- nrow(mine)
  message(sprintf("weekly fetch shard %d/%d: %d of %d repos", i, N, total, nrow(roster)))

  commits_total <- rep(NA_integer_, total)
  n_commit_ok <- 0L; n_commit_skipped <- 0L
  for (idx in unname(chunk(seq_len(total), batch_size))) {
    repos <- mine[idx, , drop = FALSE]
    got <- tryCatch(fetch_commit_counts(io, repos), error = function(e) NULL)
    if (commit_delay > 0) Sys.sleep(commit_delay)
    if (is.null(got)) { n_commit_skipped <- n_commit_skipped + length(idx); next }
    n_commit_ok <- n_commit_ok + length(idx)
    commits_total[idx] <- as.integer(unlist(got[repos$repo_id]))
  }

  contributors_total <- rep(NA_integer_, total)
  top_commits <- rep(NA_integer_, total)
  top_bot <- rep(NA_integer_, total)
  n_contrib_ok <- 0L; n_contrib_skipped <- 0L
  for (r in seq_len(total)) {
    v <- tryCatch(io$contributors(mine$owner[r], mine$name[r]), error = function(e) NULL)
    if (contributor_delay > 0) Sys.sleep(contributor_delay)
    reply <- contributor_reply(v)
    if (is.na(reply$count)) n_contrib_skipped <- n_contrib_skipped + 1L else n_contrib_ok <- n_contrib_ok + 1L
    contributors_total[r] <- reply$count
    top_commits[r] <- reply$top_commits
    top_bot[r] <- contributor_bot_flag(reply$top_type)
  }

  resp_cols <- data.frame(
    median_days_to_close_issue = rep(NA_integer_, total),
    median_days_to_close_pr    = rep(NA_integer_, total),
    median_open_issue_age_days = rep(NA_integer_, total))
  today <- format(Sys.Date())
  for (idx in unname(chunk(seq_len(total), MEDIAN_BATCH))) {
    repos <- mine[idx, , drop = FALSE]
    got <- tryCatch(fetch_responsiveness(io, repos, today), error = function(e) NULL)
    if (commit_delay > 0) Sys.sleep(commit_delay)
    if (is.null(got)) next
    m <- match(repos$repo_id, got$repo_id)
    resp_cols$median_days_to_close_issue[idx] <- got$median_days_to_close_issue[m]
    resp_cols$median_days_to_close_pr[idx]    <- got$median_days_to_close_pr[m]
    resp_cols$median_open_issue_age_days[idx] <- got$median_open_issue_age_days[m]
  }

  rows <- data.frame(repo_id = mine$repo_id, commits_total = commits_total,
                     contributors_total = contributors_total,
                     top_contributor_commits = top_commits, top_contributor_bot = top_bot,
                     resp_cols, stringsAsFactors = FALSE)

  shard_path <- file.path(out_dir, sprintf("vcs-signals-shard-%d.db", i))
  export_snapshot_shard(shard_path, rows)
  message(sprintf("weekly fetch shard %d/%d: commits %d ok/%d skipped, contributors %d ok/%d skipped",
                  i, N, n_commit_ok, n_commit_skipped, n_contrib_ok, n_contrib_skipped))
  shard_path
}

# ---- GitLab -----------------------------------------------------------------------
# Commit totals over GraphQL, contributors and licences over REST under each host's hourly budget,
# in turn from a cursor per domain. The cursors ride on the release as one more asset.
GITLAB_ROSTER_DB <- "vcs-signals-roster-gitlab.db"
GITLAB_SHARD_DB  <- "vcs-signals-shard-gitlab.db"
GITLAB_STATE_DB  <- "vcs-signals-gitlab-state.db"

write_gitlab_roster <- function(path, roster_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, sprintf("CREATE TABLE %s (repo_id TEXT PRIMARY KEY, node_id TEXT NOT NULL,
    domain TEXT NOT NULL, project_id INTEGER NOT NULL)", ROSTER_TABLE))
  if (nrow(roster_df) > 0)
    DBI::dbWriteTable(con, ROSTER_TABLE, roster_df[c("repo_id", "node_id", "domain", "project_id")], append = TRUE)
  invisible(path)
}

#' The GitLab roster from the summary run_enumerate downloaded: every active GitLab repository with a
#' node id, with the domain and project id that id carries.
enumerate_gitlab <- function(summary_path, path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), summary_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, sprintf(
    "SELECT repo_id, node_id FROM repos WHERE %s AND status = 'active' AND node_id IS NOT NULL ORDER BY repo_id",
    host_predicate(GITLAB_HOSTS)))
  rows <- rows[grepl(.GL_NODE_RE, rows$node_id), , drop = FALSE]
  roster <- data.frame(repo_id = rows$repo_id, node_id = rows$node_id,
                       domain = sub(.GL_NODE_RE, "\\1", rows$node_id),
                       project_id = as.integer(sub("^.*:", "", rows$node_id)), stringsAsFactors = FALSE)
  message(sprintf("enumerate: %d gitlab repos on %d hosts", nrow(roster), length(unique(roster$domain))))
  write_gitlab_roster(path, roster)
}

#' weekly.yml's enumerate: the GitHub roster, then the GitLab one. A GitLab roster that cannot be
#' written leaves fetch-gitlab without one and the GitHub shards untouched.
run_weekly_enumerate <- function(io, out_dir) {
  res <- run_enumerate(io, out_dir)
  tryCatch(enumerate_gitlab(file.path(out_dir, "vcs-signals-summary.db"), file.path(out_dir, GITLAB_ROSTER_DB)),
           error = function(e) message("enumerate: no gitlab roster: ", conditionMessage(e)))
  invisible(res)
}

#' Each domain's last repository read, named by domain, from the published state. A download that
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
    if (isTRUE(li$read)) { acc$license[i] <- gl_license_spdx(li$key); acc$license_read[i] <- TRUE }
    read <- cn$read && isTRUE(li$read)
    if (read) { acc$cursors[[d]] <- roster$repo_id[i]; acc$n_read[[d]] <- acc$n_read[[d]] + 1L }
    # A repository this hour's budget cut short is asked again after the hour.
    if (read || !(cn$stopped || isTRUE(li$stopped)) || !identical(.gl_rest_block(pacer, key), "hour"))
      acc$pos[[d]] <- acc$pos[[d]] + 1L
  }
  "done"
}

.gl_rotation_note <- function(d, why, n_read, n) sprintf("GitLab %s: %d of %d repositories read, %s", d, n_read, n,
  switch(why, done = "all visited", closed = "rate limited", deferred = "deferred after repeated failures",
         deadline = "past the deadline", hour = "out of requests for this hour and no time for the next",
         fault = "stopped by a fault"))

# An error's text for the log, without anything shaped like an address.
.gl_redact <- function(x) substr(gsub("[^[:space:]]*@[^[:space:]]*", "<redacted>", x), 1L, 200L)

#' The weekly GitLab job. Commit totals come over GraphQL for every roster row. Contributors and the
#' licence are read per host in repo_id order from its cursor, wrapping once, until the host is closed
#' by a 429 or ratelimit-remaining 0, deferred by the breaker, through its repositories, or past the
#' deadline. When every host left has only spent this hour's budget, the run sleeps to the next UTC
#' hour if the deadline allows. A repository is read when all its calls were answered, and the cursor
#' moves to the last one read; values from one partly read are kept. A fault in one host's collection
#' ends only that host. Writes the snapshot shard with a `license` table of the licences read this
#' run, and the cursors to state_path, where the published state is downloaded first.
run_fetch_gitlab <- function(io, out_dir, roster_path, state_path,
                             now = function() as.numeric(Sys.time()), sleep = Sys.sleep,
                             rest_pacer = gl_rest_pacer(now, sleep), graphql_pacer = gl_pacer(now, sleep),
                             deadline_min = GITLAB_WEEKLY_DEADLINE_MIN, log = message) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(state_path), showWarnings = FALSE, recursive = TRUE)
  deadline <- now() + 60 * deadline_min
  roster <- load_roster(roster_path)
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
  acc$cursors <- read_gitlab_cursors(io, state_path, log)
  acc$pos <- stats::setNames(rep(1L, length(hosts)), hosts)
  acc$n_read <- stats::setNames(rep(0L, length(hosts)), hosts)
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
      else log(.gl_rotation_note(d, why, acc$n_read[[d]], length(queues[[d]])))
    }
    open <- waiting
    if (!length(open)) break
    wake <- (floor(now() / 3600) + 1) * 3600 + 1
    if (wake >= deadline) {
      for (d in open) log(.gl_rotation_note(d, "hour", acc$n_read[[d]], length(queues[[d]])))
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
  log(sprintf("weekly fetch gitlab: commits %d, contributors %d, licences %d of %d repos",
              sum(!is.na(rows$commits_total)), sum(!is.na(acc$total)), sum(read), n))
  shard_path
}

# ---- merge --------------------------------------------------------------------
#' Fold every shard's snapshot of every WEEKLY_METRICS value into the published
#' series as a change-only point dated today, rebuild the summary, and
#' republish. Mirrors backfill.R::run_merge's seed + complete-history-load
#' pattern (seed_working_db for the recent window, then protect_history_pull
#' plus a year-shard fold so publish()'s re-export of the touched (current)
#' year never truncates other metrics' rows already in that shard).
#'
#' materialize_series is fed prev_latest restricted to WEEKLY_METRICS only,
#' so a daily metric (stars, forks, ...) already sitting in series_latest can
#' never be clobbered or spuriously re-emitted by this pass; snapshot_long is
#' likewise built from WEEKLY_METRICS alone. A repo unchanged since the last
#' weekly run contributes no new signals_series row for that metric. The GitLab
#' cursor state in parts_dir, when fetch-gitlab supplied it, is uploaded after
#' the publish.
run_merge <- function(io, out_dir, parts_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  working_path <- file.path(out_dir, "_weekly_working.db")
  seed <- seed_working_db(io, out_dir, working_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), working_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con)
  ensure_series_schema(con)

  # A pull that fails while another publisher is replacing assets is a conflict,
  # which main() retries, and not lost history.
  .pull_or_conflict(io, attr(seed, "generation"), "while pulling the published history",
                    function() protect_history_pull(io, out_dir))
  year_shards <- list.files(out_dir, pattern = "^vcs-signals-[0-9]{4}\\.db$", full.names = TRUE)
  for (ys in year_shards) {
    ycon <- DBI::dbConnect(RSQLite::SQLite(), ys)
    yrows <- tryCatch(
      if (DBI::dbExistsTable(ycon, "signals_series")) DBI::dbReadTable(ycon, "signals_series") else NULL,
      error = function(e) NULL)
    DBI::dbDisconnect(ycon)
    if (!is.null(yrows) && nrow(yrows) > 0)
      DBI::dbExecute(con,
        "INSERT OR IGNORE INTO signals_series (repo_id, date, metric, value) VALUES (?,?,?,?)",
        params = list(yrows$repo_id, yrows$date, yrows$metric, yrows$value))
  }

  today <- format(Sys.Date())

  parts <- list.files(parts_dir, pattern = "^vcs-signals-shard-.*\\.db$", full.names = TRUE)
  empty_snapshot <- setNames(
    data.frame(repo_id = character(),
               matrix(integer(0), nrow = 0, ncol = length(WEEKLY_METRICS)),
               stringsAsFactors = FALSE),
    c("repo_id", WEEKLY_METRICS))
  snap_rows <- lapply(parts, function(p) {
    pcon <- DBI::dbConnect(RSQLite::SQLite(), p)
    on.exit(DBI::dbDisconnect(pcon), add = TRUE)
    if (!DBI::dbExistsTable(pcon, SNAPSHOT_TABLE)) return(empty_snapshot)
    DBI::dbReadTable(pcon, SNAPSHOT_TABLE)
  })
  snapshot <- if (length(snap_rows)) do.call(rbind, snap_rows) else empty_snapshot
  snapshot <- snapshot[!duplicated(snapshot$repo_id), , drop = FALSE]

  snapshot_long <- data.frame(repo_id = character(), metric = character(),
                              value = integer(), stringsAsFactors = FALSE)
  for (metric in WEEKLY_METRICS) {
    v <- snapshot[[metric]]
    keep <- !is.na(v)
    if (any(keep))
      snapshot_long <- rbind(snapshot_long, data.frame(
        repo_id = snapshot$repo_id[keep], metric = metric,
        value = as.integer(v[keep]), stringsAsFactors = FALSE))
  }

  ph <- paste(sprintf("'%s'", WEEKLY_METRICS), collapse = ", ")
  prev_latest <- DBI::dbGetQuery(con, sprintf(
    "SELECT repo_id, metric, value FROM series_latest WHERE metric IN (%s)", ph))
  mat <- materialize_series(prev_latest, snapshot_long, today)

  n_inserted <- 0L
  if (nrow(mat$series_rows) > 0) {
    before <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM signals_series")$n
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO signals_series (repo_id, date, metric, value) VALUES (?,?,?,?)",
      params = list(mat$series_rows$repo_id, mat$series_rows$date,
                    mat$series_rows$metric, mat$series_rows$value))
    after <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM signals_series")$n
    n_inserted <- after - before
  }
  if (nrow(mat$new_latest) > 0) {
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO series_latest (repo_id, metric, value) VALUES (?,?,?)",
      params = list(mat$new_latest$repo_id, mat$new_latest$metric, mat$new_latest$value))
  }
  # The stored flag described an earlier top contributor, so it goes and the summary
  # reads NA. signals_series has no NA value: its earlier points stay and none is added.
  untyped <- untyped_top_contributors(snapshot)
  if (length(untyped) > 0)
    DBI::dbExecute(con, "DELETE FROM series_latest WHERE metric = 'top_contributor_bot' AND repo_id = ?",
                   params = list(untyped))

  # Rebuild the summary so commits_total/contributors_total populate. No
  # repository is gauged this run, so every REPO_ATTR_COLS value is carried
  # forward from the prior summary row, as run_update does for one it did not reach.
  repos_all <- DBI::dbReadTable(con, "repos")
  rp_all <- DBI::dbReadTable(con, "repo_packages")
  series_all <- DBI::dbGetQuery(con, "SELECT repo_id, date, metric, value FROM signals_series")
  latest_all <- DBI::dbGetQuery(con, "SELECT repo_id, metric, value FROM series_latest")
  repo_attrs <- merge(repos_all[, c("repo_id", "first_seen", "last_seen")], prior_repo_attrs(con),
                      by = "repo_id", all.x = TRUE)

  ai_all <- if (DBI::dbExistsTable(con, "vcs_ai_signals")) DBI::dbReadTable(con, "vcs_ai_signals") else NULL
  go_live <- DBI::dbGetQuery(con, "SELECT value FROM pipeline_state WHERE key = 'go_live'")$value
  # Full history is loaded this run (protect_history_pull + year shards), so
  # release cadence is recomputed from scratch rather than carried forward.
  summary_df <- build_signals_summary(latest_all, series_all, repo_attrs, rp_all, today,
                                      compute_release_facts = TRUE, ai_signals = ai_all,
                                      go_live = if (length(go_live)) go_live[1] else NA_character_)
  DBI::dbExecute(con, "DELETE FROM vcs_signals_summary")
  if (nrow(summary_df) > 0) DBI::dbWriteTable(con, "vcs_signals_summary", summary_df, append = TRUE)

  touched_years <- unique(substr(mat$series_rows$date, 1, 4))
  message(sprintf("weekly merge: %d shard partials, %d repos snapshotted, %d changed rows, %d year(s) touched",
                  length(parts), nrow(snapshot), n_inserted, length(touched_years)))

  res <- publish(io, con, out_dir, tag = "current", source_kind = "live", touched_years = touched_years,
                 base_generation = attr(seed, "generation"))
  # The cursors go up only once the values read with them are published. A failed upload costs the
  # next GitLab run a repeat of this run's repositories.
  state <- file.path(parts_dir, GITLAB_STATE_DB)
  if (file.exists(state))
    tryCatch(io$upload(state), error = function(e)
      message(sprintf("weekly merge: %s not uploaded, the next GitLab run starts from the previous cursors: %s",
                      GITLAB_STATE_DB, conditionMessage(e))))
  invisible(res)
}

# ---- CLI dispatch -----------------------------------------------------------------
# io is built here unless a test passes one, so the suite can drive the same entry
# point CI does.
main <- function(mode, out_dir, io = NULL) {
  token <- Sys.getenv("VCS_SIGNALS_TOKEN")
  # One GitLab REST pacer per run: the token state lowers its budget on a 401 and the rotation reads it.
  gl_rest_p <- gl_rest_pacer()
  if (is.null(io)) {
    gl_state <- gl_run_state(pacer = gl_rest_p, log = message)
    io <- list(
      graphql        = default_io(token)$graphql,
      contributors   = function(owner, name) fetch_contributor_count(token, owner, name),
      gl_graphql     = function(domain, query, variables) gl_graphql(domain, query, variables, state = gl_state),
      gl_rest        = function(domain, path) gl_rest(domain, path, state = gl_state),
      release_exists = function() gh_release_exists(RELEASE_REPO),
      generation     = function() gh_release_generation(RELEASE_REPO),
      download       = function(pattern, dir) gh_release_download(RELEASE_REPO, pattern, dir),
      upload         = function(path) gh_release_upload(RELEASE_REPO, path))
  }

  if (mode == "enumerate") {
    run_weekly_enumerate(io, out_dir)
  } else if (mode == "fetch") {
    i <- suppressWarnings(as.integer(Sys.getenv("VCS_SHARD_I", "0")))
    N <- suppressWarnings(as.integer(Sys.getenv("VCS_SHARD_N", "1")))
    if (is.na(i) || is.na(N) || N < 1L || i < 0L || i >= N)
      stop("fetch: VCS_SHARD_I must be in [0, VCS_SHARD_N)")
    roster_dir <- Sys.getenv("VCS_ROSTER", out_dir)
    run_fetch_shard(io, out_dir, file.path(roster_dir, "vcs-signals-roster.db"), i, N)
  } else if (mode == "fetch-gitlab") {
    roster_dir <- Sys.getenv("VCS_ROSTER", out_dir)
    run_fetch_gitlab(io, out_dir, file.path(roster_dir, GITLAB_ROSTER_DB), file.path(out_dir, GITLAB_STATE_DB),
                     rest_pacer = gl_rest_p)
  } else if (mode == "merge") {
    # If another publisher replaced the release between this merge's seed and its
    # publish (the AI merge shares this Sunday cron), the merge waits for that
    # publisher to finish, seeds again from what it left, and rebuilds.
    retry_on_publish_conflict(io, function() run_merge(io, out_dir, Sys.getenv("VCS_PARTS", "parts")))
  } else {
    stop("usage: weekly.R [enumerate|fetch|fetch-gitlab|merge]")
  }
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  mode <- if (length(args) >= 1) args[1] else ""
  out_dir <- Sys.getenv("VCS_OUT", "out")
  main(mode, out_dir)
}
