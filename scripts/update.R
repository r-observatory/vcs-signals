#!/usr/bin/env Rscript
# scripts/update.R - vcs-signals orchestration.
# run_update(io, out_dir, opts) drives five ordered stages behind an injected
# io: (1) resolve, (2) node-id resolution + lifecycle, (3) forward gauge
# collection, (4) series/summary materialization + go-live watermark, (5)
# publish. main() builds the real io (network acquisition, GitHub GraphQL,
# gh-release download/upload) and calls run_update(); the hermetic test
# drives run_update() directly with a fake io, so no live network runs in CI.
#
# Sourced from tests/testthat/helper-setup.R (with cwd temporarily chdir'd to
# the repo root) as well as run directly via Rscript from the repo root, so
# these paths are always repo-root-relative.
source("scripts/config.R")
source("scripts/helpers.R")
source("scripts/github.R")
if (!exists("build_ai_rollups")) source("scripts/ai_signals.R")
if (!exists("url_points_into")) source("scripts/dev_tooling.R")
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

# ---- acquisition ------------------------------------------------------------
acquire_cran <- function() {
  pdb <- tools::CRAN_package_db()
  pdb <- pdb[!duplicated(pdb$Package), ]
  data.frame(package = pdb$Package, origin = "cran",
             url_raw = pdb$URL, bugreports_raw = pdb$BugReports, stringsAsFactors = FALSE)
}

# Retry `fn` on error, sleeping VIEWS_RETRY_WAITS_S between attempts. One more
# attempt is made than there are waits, and the final attempt's error is the one
# that propagates, so the caller sees the real cause rather than a retry wrapper.
# sleep and rand are injected so the suite asserts the schedule without waiting.
with_retry <- function(fn, waits = VIEWS_RETRY_WAITS_S, sleep = Sys.sleep,
                       rand = function() stats::runif(1, 1, 1.25)) {
  for (w in waits) {
    val <- tryCatch(fn(), error = function(e) e)
    if (!inherits(val, "error")) return(val)
    sleep(w * rand())
  }
  fn()
}

.read_url <- function(u) paste(readLines(url(u), warn = FALSE), collapse = "\n")

# A 504 arrives as a read error, but a gateway can also answer 200 with an HTML
# error page, which read.dcf would then parse into a zero-package data frame and
# silently shrink the universe. Both shapes are treated as a failed attempt and
# retried; only the URL-naming error escapes.
fetch_views <- function(u, read = .read_url, ...) {
  attempt <- function() {
    txt <- read(u)
    if (length(txt) != 1L || is.na(txt) || !grepl("(^|\n)Package:", txt))
      stop("response body is not VIEWS content")
    txt
  }
  tryCatch(
    with_retry(attempt, ...),
    error = function(e)
      stop(sprintf("VIEWS fetch failed or empty: %s (%s)", u, conditionMessage(e))))
}

acquire_bioc <- function() {
  parts <- lapply(VIEWS_URLS, function(u) {
    m <- read.dcf(textConnection(fetch_views(u)))
    g <- function(f) if (f %in% colnames(m)) as.character(m[, f]) else rep(NA_character_, nrow(m))
    data.frame(package = g("Package"), origin = "bioc",
               url_raw = g("URL"), bugreports_raw = g("BugReports"), stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, parts)
  df[!duplicated(df$package), ]
}

# ---- working-DB bootstrap --------------------------------------------------
# Seeds a fresh out_dir/_working.db from the previously-published recent
# shard (repos, repo_packages, series_latest, pipeline_state, and the
# RECENT_WINDOW=400d tail of signals_series) when a "current" release
# already exists, so a run in a fresh CI checkout still preserves
# first_seen/node_id/series history instead of starting cold every day.
# signals_series matters here beyond change-detection: publish() re-exports
# every year present in the working DB's signals_series on every run, and
# RECENT_WINDOW (400d) is deliberately > 365d so this seed always carries a
# complete current-year history forward; without it, a fresh daily checkout
# would silently re-publish year/recent shards containing only that day's
# changed rows, discarding all prior accumulated history. Older, fully-past
# years are correctly left untouched (out of the 400d window, immutable).
# A no-op (empty working DB, first-run shape) when no release exists yet or
# the prior recent shard cannot be pulled.
# Two of the three ways this used to return FALSE were not no-ops at all. Once a
# release exists, the recent shard IS the accumulated history, and failing to pull it
# leaves an empty working DB that every caller then treats as "there was no prior
# data": run_merge reads 0 prior rows, deletes the published table and rewrites it
# from one run's shards. So a missing release is still a legitimate first-run no-op,
# but a release that exists and cannot be pulled now aborts, matching the fail-closed
# contract gh_release_exists and protect_history_pull already follow.
#
# Returns TRUE or FALSE carrying attr "generation": what the release held when this
# seed began, which the caller hands to publish() as base_generation. It is read
# before the first download, not after. A publish landing mid-seed then shows up at
# publish time as a conflict and the merge seeds again; read afterwards, that publish
# would be absorbed into the base, and the stale build would go out as if current.
# A generation of "" is a release with no assets at all (update.yml creates the
# release before the first run uploads into it), which is a first run too, and a
# missing recent shard is then not a failure. But "" also switches off publish()'s
# pull and its regression gate, so it is not taken on the generation's word alone:
# if the recent shard downloads after all, the digests came from a different
# release than the one downloads and uploads reach, and nothing may start cold.
# A generation that lists assets already proves the release exists, and it is not
# put to release_exists() again: that asks the same gh lookup, which can word a
# transient failure as "release not found", and a FALSE believed there started the
# run cold over a real base until the regression gate refused it as lost ground.
seed_working_db <- function(io, out_dir, working_path) {
  generation <- .release_generation(io)
  seeded <- function(ok) invisible(structure(ok, generation = generation))
  if (file.exists(working_path)) unlink(working_path)
  if (!nzchar(generation)) {
    if (isTRUE(io$release_exists()) && isTRUE(io$download("vcs-signals-recent.db", out_dir)))
      stop("the release generation lists no assets, yet vcs-signals-recent.db downloaded from release ",
           "'current'; the digests describe a different release than downloads and uploads reach, ",
           "so this run neither starts cold nor publishes", call. = FALSE)
    return(seeded(FALSE))
  }
  # A download that fails because another publisher has the recent shard deleted
  # for its --clobber is a conflict, so a merge's retry seeds again once that
  # publisher is done (see .pull_or_conflict).
  prior_path <- file.path(out_dir, "vcs-signals-recent.db")
  .pull_or_conflict(io, generation, "while seeding", function() {
    if (!isTRUE(io$download("vcs-signals-recent.db", out_dir)))
      stop("release 'current' exists but vcs-signals-recent.db could not be downloaded; ",
           "aborting rather than treating accumulated history as absent.",
           lost_asset_hint("vcs-signals-recent.db"))
    if (!file.exists(prior_path))
      stop("vcs-signals-recent.db reported a successful download but is not on disk; ",
           "aborting rather than treating accumulated history as absent")
  })

  pcon <- DBI::dbConnect(RSQLite::SQLite(), prior_path)
  on.exit(DBI::dbDisconnect(pcon), add = TRUE)
  wcon <- DBI::dbConnect(RSQLite::SQLite(), working_path)
  on.exit(DBI::dbDisconnect(wcon), add = TRUE)
  ensure_repo_schema(wcon)
  ensure_series_schema(wcon)
  # vcs_signals_summary is included so that I4's stage-4 carry-forward has a
  # prior summary row to read for a repo not collected this run: it is
  # embedded into the published recent shard by .embed_recent_tables, so it
  # must be seeded back the same way the other four tables are.
  # SUMMARY_EXTRA_TABLES belongs here for the same reason it belongs in
  # .embed_recent_tables, which was fixed and this was not. The recent shard
  # carries the three AI tables; this is what reads them back out of it. Left
  # off, every run starts with them empty, rebuilds a summary without them, and
  # the regression gate refuses the publish.
  for (nm in c("repos", "repo_packages", "series_latest", "pipeline_state",
               "signals_series", "vcs_signals_summary", "vcs_ai_signals", "vcs_dev_tooling",
               SUMMARY_EXTRA_TABLES)) {
    if (DBI::dbExistsTable(pcon, nm)) {
      df <- DBI::dbReadTable(pcon, nm)
      if (nrow(df) > 0) DBI::dbWriteTable(wcon, nm, df, append = TRUE)
    }
  }
  # Every publisher seeds here, and a table the seed brings back empty is what
  # each of them then publishes. A link whose package has left cannot be
  # resolved again, so an empty link table has to be caught here or not at all.
  # The restore downloads the summary and its previous copy after the generation
  # was read, as the recent shard's download above does, so one that finds its
  # copy deleted for another publisher's --clobber is the same conflict and not
  # a history with nothing left to restore from.
  .pull_or_conflict(io, generation, "while restoring the link table", function()
    restore_package_links(io, wcon, file.path(out_dir, "_links_restore")))
  seeded(TRUE)
}

# ---- the five-stage orchestrator -------------------------------------------
#' Run one full vcs-signals update pass behind an injected io.
#'
#' io must expose: acquire() -> data.frame(package, origin, url_raw,
#' bugreports_raw); graphql(query) -> parsed GraphQL response (list, with
#' $data/$errors, or throws on transport error); release_exists() ->
#' logical; generation() -> the release generation string (see
#' gh_release_generation); download(pattern, dir) -> logical; upload(path) ->
#' invisible.
#' opts$force_full re-exports and re-uploads every shard regardless of the
#' change-gate; opts$tag overrides the release tag (default "current");
#' opts$links_backfill names the link backfill file (default
#' LINKS_BACKFILL_PATH, the committed one).
#' A publish refused because another publisher moved the release in the meantime
#' is not retried here: see retry_on_publish_conflict for why.
run_update <- function(io, out_dir, opts = list()) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  today <- Sys.Date()
  today_s <- format(today)
  force_full <- isTRUE(opts$force_full)
  tag <- if (!is.null(opts$tag)) opts$tag else "current"

  working_path <- file.path(out_dir, "_working.db")
  seed <- seed_working_db(io, out_dir, working_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), working_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ensure_repo_schema(con)
  ensure_series_schema(con)

  # ---- Stage 1: resolve (acquire -> resolve_all -> build_repo_index ->
  # write_repo_tables), guarded by the universe guard against a
  # catastrophic drop in resolved packages/repos. -------------------------
  input <- io$acquire()
  resolved <- resolve_all(input)
  idx <- build_repo_index(resolved)

  prev_pkgs <- DBI::dbGetQuery(con, "SELECT COUNT(DISTINCT package || origin) n FROM repo_packages")$n
  prev_repos <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM repos WHERE status IN ('active','moved')")$n
  curr_pkgs <- length(unique(paste(idx$repo_packages$package, idx$repo_packages$origin)))
  universe_guard(prev_pkgs, prev_repos, curr_pkgs, nrow(idx$repos))

  # Read and checked in full before anything is written, so a malformed file
  # stops the run here rather than landing the rows that parsed before it.
  links_backfill <- read_links_backfill(
    if (is.null(opts$links_backfill)) LINKS_BACKFILL_PATH else opts$links_backfill)
  write_repo_tables(con, idx$repos, idx$repo_packages, today_s, links_backfill = links_backfill)
  print_coverage(input, resolved, idx)

  # ---- Rate-limit preflight (I3): below-reserve skips resolve + collection
  # + materialization entirely, publishing a clean heartbeat instead of
  # spending down points on a run that cannot safely complete. -------------
  rl <- graphql_rate_remaining(io)
  if (rl < POINT_RESERVE) {
    message(sprintf(
      "graphql rate remaining (%s) below reserve (%d); skipping node-id resolution and collection this run",
      rl, POINT_RESERVE))
    return(invisible(publish(io, con, out_dir, tag, source_kind = "live",
                              force_full = force_full, touched_years = character(0),
                              base_generation = attr(seed, "generation"))))
  }

  # ---- Stage 2: node-id resolution + lifecycle (rename, gone) ------------
  needing <- DBI::dbGetQuery(con,
    "SELECT repo_id, owner, name FROM repos WHERE host = 'github' AND node_id IS NULL AND status = 'active'")
  resolved_ids <- resolve_node_ids(io, needing)
  update_repo_node_ids(con, resolved_ids)
  n_id_resolved <- sum(resolved_ids$status == "active")
  n_id_gone <- sum(resolved_ids$status == "gone")
  n_id_deferred <- nrow(needing) - nrow(resolved_ids)
  cat(sprintf("node ids: %d resolved, %d deferred, %d gone\n", n_id_resolved, n_id_deferred, n_id_gone))

  # ---- Stage 3: forward gauge collection over active github repos -------
  repo_map <- DBI::dbGetQuery(con,
    "SELECT node_id, repo_id FROM repos WHERE host = 'github' AND status = 'active' AND node_id IS NOT NULL")
  gauges <- collect_gauges(io, repo_map$node_id)
  snapshot_long <- gauges_to_long(gauges$snapshot, repo_map)
  n_gauges_collected <- if (!is.null(gauges$snapshot)) nrow(gauges$snapshot) else 0L
  cat(sprintf("gauges: collected %d repos, %d deferred\n", n_gauges_collected, length(gauges$deferred)))

  # ---- Stage 4: materialize series + summary + go-live watermark --------
  # I4 floor: when this run collected nothing at all (every repo deferred -
  # a sustained outage, or the last surviving repo 502ing on every retry),
  # gauges$snapshot is NULL/empty. Skip every stage-4 write outright (no
  # series_latest/vcs_signals_summary rebuild) and fall through to a
  # publish() heartbeat, rather than rebuild from an empty snapshot and
  # publish an all-NA dataset over the top of good accumulated history.
  if (!is.null(gauges$snapshot) && nrow(gauges$snapshot) > 0) {
    write_repo_owner(con, gauges$snapshot, repo_map, today_s)
    prev_latest <- DBI::dbGetQuery(con, "SELECT repo_id, metric, value FROM series_latest")
    mat <- materialize_series(prev_latest, snapshot_long, today_s)

    if (nrow(mat$series_rows) > 0) {
      DBI::dbExecute(con,
        "INSERT OR REPLACE INTO signals_series (repo_id, date, metric, value) VALUES (?,?,?,?)",
        params = list(mat$series_rows$repo_id, mat$series_rows$date,
                      mat$series_rows$metric, mat$series_rows$value))
    }
    # Upsert, never a blanket delete: a repo not collected this run (deferred,
    # rate-limited, still 502ing) keeps its prior series_latest value instead
    # of vanishing from the published snapshot. materialize_series() above
    # was still called with prev_latest (the pre-upsert values).
    if (nrow(mat$new_latest) > 0) {
      DBI::dbExecute(con,
        "INSERT OR REPLACE INTO series_latest (repo_id, metric, value) VALUES (?,?,?)",
        params = list(mat$new_latest$repo_id, mat$new_latest$metric, mat$new_latest$value))
    }

    repos_all <- DBI::dbReadTable(con, "repos")
    rp_all <- DBI::dbReadTable(con, "repo_packages")
    series_all <- DBI::dbGetQuery(con, "SELECT repo_id, date, metric, value FROM signals_series")

    # Descriptive repo attributes (license/topics/is_archived/last_commit_date)
    # are not columns on the repos table (that schema is frozen per the
    # design). For a repo collected this run they come from this run's
    # gauge snapshot, joined onto repo_id via repo_map; for a repo NOT
    # collected this run they are carried forward from the prior
    # vcs_signals_summary row (read here, before that table is rebuilt
    # below), so a deferred repo keeps its last-known descriptive attributes
    # instead of going NA. A repo with neither (never collected) gets NA.
    attrs <- data.frame(repo_id = character(), license = character(), topics = character(),
                        is_archived = integer(), last_commit_date = character(), stringsAsFactors = FALSE)
    if (nrow(repo_map) > 0) {
      sn <- merge(gauges$snapshot, repo_map, by = "node_id")
      pick <- function(col, default) if (col %in% names(sn)) sn[[col]] else rep(default, nrow(sn))
      attrs <- data.frame(repo_id = sn$repo_id,
                          license = pick("license", NA_character_),
                          topics = pick("topics", NA_character_),
                          is_archived = as.integer(pick("is_archived", NA_integer_)),
                          last_commit_date = pick("pushed_at", NA_character_),
                          stringsAsFactors = FALSE)
    }
    prev_summary_attrs <- DBI::dbGetQuery(con,
      "SELECT repo_id, license, topics, is_archived, last_commit_date,
              last_release_date, median_days_between_releases
         FROM vcs_signals_summary WHERE repo_id IS NOT NULL")
    if (nrow(prev_summary_attrs) > 0) {
      prev_summary_attrs <- prev_summary_attrs[!duplicated(prev_summary_attrs$repo_id), ]
      prev_summary_attrs$is_archived <- as.integer(prev_summary_attrs$is_archived)
    }
    # last_release_date/median_days_between_releases have no fresh source in
    # this run's gauge snapshot (attrs), so - unlike the descriptive fields
    # below, which prefer this run's fresh attrs when available - they always
    # carry forward from the prior summary for every repo, collected this run
    # or not; build_signals_summary(compute_release_facts = FALSE) uses these
    # as the carry-forward floor.
    release_facts <- prev_summary_attrs[, c("repo_id", "last_release_date", "median_days_between_releases")]
    descriptive_prev <- prev_summary_attrs[!(prev_summary_attrs$repo_id %in% attrs$repo_id),
                                            c("repo_id", "license", "topics", "is_archived", "last_commit_date")]
    combined_attrs <- rbind(attrs, descriptive_prev)
    repo_attrs <- merge(repos_all[, c("repo_id", "first_seen", "last_seen")], combined_attrs,
                        by = "repo_id", all.x = TRUE)
    repo_attrs <- merge(repo_attrs, release_facts, by = "repo_id", all.x = TRUE)

    # Built from the FULL post-upsert series_latest (every repo, including
    # ones deferred this run), not just this run's snapshot, so a deferred
    # repo keeps its numeric values in the summary too.
    latest_all <- DBI::dbGetQuery(con, "SELECT repo_id, metric, value FROM series_latest")
    ai_all <- if (DBI::dbExistsTable(con, "vcs_ai_signals")) DBI::dbReadTable(con, "vcs_ai_signals") else NULL
    # Recent-window collection only (no full history), so release cadence is
    # never recomputed here - it is carried forward via repo_attrs above.
    summary_df <- build_signals_summary(latest_all, series_all, repo_attrs, rp_all, today_s,
                                        compute_release_facts = FALSE, ai_signals = ai_all)
    DBI::dbExecute(con, "DELETE FROM vcs_signals_summary")
    if (nrow(summary_df) > 0) DBI::dbWriteTable(con, "vcs_signals_summary", summary_df, append = TRUE)

    go_live <- DBI::dbGetQuery(con, "SELECT value FROM pipeline_state WHERE key = 'go_live'")
    if (nrow(go_live) == 0) {
      DBI::dbExecute(con, "INSERT INTO pipeline_state (key, value) VALUES ('go_live', ?)", params = list(today_s))
    }

    # touched_years is derived from mat$series_rows (the change-only rows
    # just materialized into signals_series this run), never from the
    # working DB's full signals_series - so a forward run only re-exports
    # the current year's shard. See publish()'s touched_years documentation
    # in scripts/helpers.R for why this matters.
    touched_years <- unique(substr(mat$series_rows$date, 1, 4))
  } else {
    message("stage-4 floor: collection returned nothing this run; skipping series/summary rebuild, publishing heartbeat")
    touched_years <- character(0)
  }

  # ---- Stage 5: publish --------------------------------------------------
  invisible(publish(io, con, out_dir, tag, source_kind = "live", force_full = force_full,
                     touched_years = touched_years, base_generation = attr(seed, "generation")))
}

# ---- gh-release IO for the real run ----------------------------------------
#' Whether the "current" (or `tag`) release exists on `repo` - fail CLOSED.
#'
#' A plain exit-status check cannot distinguish "release not found" from any
#' other `gh` failure (auth expired, rate limited, network blip, GitHub
#' outage), and this function's FALSE answer is read by both
#' protect_history_pull and seed_working_db as "no prior release, start
#' cold" - which would silently clobber accumulated history if returned for
#' a merely-transient error. So: exit 0 -> TRUE; exit non-zero AND the
#' captured output names a genuine not-found -> FALSE; any other non-zero
#' exit -> stop(), aborting the run rather than guessing.
#'
#' Since gh 2.96 a genuine not-found is not the only thing worded that way: one
#' failed lookup of a published release is too (see gh_release_generation, which
#' reads through the same lookup). So every failure, that one included, is asked
#' again after each of `waits` (run and sleep injected the same way), and FALSE
#' needs the last attempt to still say not found. Taken on one answer, it told
#' publish()'s pull there was nothing to fetch, and the run stopped over a summary
#' it had never tried to download.
gh_release_exists <- function(repo, tag = "current", run = system2,
                              waits = RELEASE_READ_RETRY_WAITS_S, sleep = Sys.sleep) {
  last <- length(waits) + 1L
  attempt <- 0L
  ask <- function() {
    attempt <<- attempt + 1L
    out <- suppressWarnings(run("gh", c("release", "view", tag, "--repo", repo),
                                stdout = TRUE, stderr = TRUE))
    status <- attr(out, "status")
    status <- if (is.null(status)) 0L else as.integer(status)
    if (identical(status, 0L)) return(TRUE)
    text <- paste(out, collapse = "\n")
    not_found <- grepl("release not found", text, ignore.case = TRUE) ||
      grepl("HTTP 404", text, ignore.case = TRUE)
    if (not_found && attempt == last) return(FALSE)
    stop(sprintf("gh release view failed ambiguously, aborting to avoid clobbering history: %s", text))
  }
  with_retry(ask, waits = waits, sleep = sleep)
}

#' The release generation of `tag` on `repo`: one "name<TAB>digest" line per asset,
#' sorted, joined by newlines. GitHub reports a sha256 digest on every asset; one
#' without falls back to "<node id>@<updatedAt>", which --clobber also changes, since
#' the asset is deleted and created again. "" when the release has no assets or
#' gh reports "release not found", which is the one state with nothing to overwrite.
#'
#' Read through gh release view, the lookup gh release download and upload use.
#' It finds a draft "current" as well as a published one, where GET
#' releases/tags/<tag> answers 404 for a draft (for instance after the tag is
#' deleted). Asked that way, a draft read as an empty release while its downloads
#' and uploads still worked, so the merge started cold, skipped the pull and the
#' regression gate, and published one run's rows over the draft's history.
#'
#' Any other failure stops, rather than returning something a caller could read
#' as a generation: a 502 turned into "" would look like an empty release, and
#' publish() would skip its pull and its regression gate on the strength of it.
#' stderr goes to its own file, so a gh upgrade notice or warning never becomes
#' part of the string and a false conflict. `run` is system2, injected so the
#' suite can answer for gh without a network.
#'
#' A failed read is tried again after each of `waits` (sleep injected like run),
#' and only the last failure stops. "release not found" is tried again as well, and
#' reads as "" only when the last attempt still says it. gh 2.96 and later look the
#' tag up as a published release and as a draft at once, and when both lookups fail
#' they report the release as not found. The draft lookup gives that answer for
#' every published release, so one 502 on the published lookup comes back in exactly
#' those words. Believed the first time, it stopped the seed over a release it took
#' for empty, and at a check in publish() it became a conflict naming every asset
#' with no other publisher involved. A release that really is missing costs the
#' waits, which only a first-ever run can pay.
gh_release_generation <- function(repo, tag = "current", run = system2,
                                  waits = RELEASE_READ_RETRY_WAITS_S, sleep = Sys.sleep) {
  jq <- '.assets[] | [.name, (.digest // (.id + "@" + .updatedAt))] | join("\t")'
  last <- length(waits) + 1L
  attempt <- 0L
  read_once <- function() {
    attempt <<- attempt + 1L
    err_file <- tempfile("gh-release-generation-")
    on.exit(unlink(err_file), add = TRUE)
    out <- suppressWarnings(run("gh", c("release", "view", tag, "--repo", repo, "--json", "assets",
                                        "--jq", shQuote(jq)),
                                stdout = TRUE, stderr = err_file))
    status <- attr(out, "status")
    status <- if (is.null(status)) 0L else as.integer(status)
    err <- if (file.exists(err_file)) paste(readLines(err_file, warn = FALSE), collapse = "\n") else ""
    if (!identical(status, 0L)) {
      if (grepl("release not found", err, fixed = TRUE) && attempt == last) return("")
      stop(sprintf("could not read the asset digests of release '%s' on %s (gh exit %s): %s",
                   tag, repo, status, err), call. = FALSE)
    }
    lines <- out[nzchar(out)]
    paste(sort(lines, method = "radix"), collapse = "\n")
  }
  with_retry(read_once, waits = waits, sleep = sleep)
}

gh_release_download <- function(repo, pattern, dir, tag = "current") {
  st <- suppressWarnings(system2("gh", c("release", "download", tag, "--repo", repo,
    "--pattern", pattern, "--dir", dir, "--clobber"), stdout = TRUE, stderr = TRUE))
  code <- attr(st, "status")
  is.null(code) || identical(as.integer(code), 0L)
}

#' Upload one asset, failing closed. Every caller treats an upload as must-succeed:
#' publish() writes release notes describing the assets straight afterwards, and the
#' merger downstream reads whatever bytes are on the release. Discarding the status
#' here meant a 403 or a network fault produced a green run whose notes described data
#' that never landed, with the previous day's asset still in place. Its two siblings
#' above both read the status; only this one did not.
gh_release_upload <- function(repo, path, tag = "current") {
  out <- suppressWarnings(system2("gh", c("release", "upload", tag, "--repo", repo,
                                          path, "--clobber"), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    stop(sprintf("gh release upload failed for %s (exit %s): %s",
                 path, status, paste(out, collapse = "\n")))
  }
  invisible(NULL)
}

# io is built here unless a test passes one, so the suite can drive the same entry
# point CI does.
main <- function(out_dir, io = NULL) {
  token <- Sys.getenv("VCS_SIGNALS_TOKEN")
  if (is.null(io)) io <- list(
    acquire = function() rbind(acquire_cran(), acquire_bioc()),
    graphql = default_io(token)$graphql,
    release_exists = function() gh_release_exists(RELEASE_REPO),
    generation = function() gh_release_generation(RELEASE_REPO),
    download = function(pattern, dir) gh_release_download(RELEASE_REPO, pattern, dir),
    upload = function(path) gh_release_upload(RELEASE_REPO, path))
  force_full <- tolower(Sys.getenv("FORCE_FULL_REBUILD", "")) %in% c("true", "1", "yes")
  # Not wrapped in retry_on_publish_conflict. A conflict fails the run with that
  # message; update.yml's 11:30 catch-up runs the update again if the day has no
  # successful run by then, and a conflicted catch-up gets no further attempt.
  res <- run_update(io, out_dir, list(force_full = force_full))
  cat("Changed shards:",
      if (length(res$changed_shards)) paste(res$changed_shards, collapse = ", ") else "(none)", "\n")
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1) args[1] else "out"
  main(out_dir)
}
