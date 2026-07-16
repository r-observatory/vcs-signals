#!/usr/bin/env Rscript
# scripts/ai_backfill.R - gated deep-scan AI-tooling-detection backfill for vcs-signals.
#
# Five sub-commands, wired together by CI (.github/workflows/ai-backfill.yml):
#   enumerate -> full active github roster from the published summary's repos table (one job)
#   cheap     -> Tier-D marker + PR-agent pass over one mod-N shard, write a flagged partial
#                (matrix job)
#   gate      -> union every cheap shard's flagged partials into one flagged-roster (one job)
#   deep      -> commit-history onset scan over one mod-N shard of the flagged roster,
#                build vcs_ai_signals detail rows (matrix job)
#   merge     -> reconcile node_id identity, reduce prior+incoming onsets, rebuild the
#                summary rollups, and republish (one job)
if (!exists("STARGAZER_PAGE"))       source("scripts/config.R")
if (!exists("ensure_series_schema")) source("scripts/helpers.R")
if (!exists("build_tree_query"))     source("scripts/github.R")
if (!exists("gh_release_exists"))    source("scripts/update.R")   # default_io, gh_release_*, seed_working_db
if (!exists("build_ai_detail"))      source("scripts/ai_signals.R")
if (!exists("write_roster"))         source("scripts/backfill.R") # shard_rows via helpers, roster idiom
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

AI_ROSTER_TABLE <- "roster"

# ---- roster IO --------------------------------------------------------------
write_ai_roster <- function(path, roster_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, sprintf("CREATE TABLE %s (
    repo_id TEXT PRIMARY KEY, owner TEXT NOT NULL, name TEXT NOT NULL,
    node_id TEXT, done INTEGER NOT NULL DEFAULT 0)", AI_ROSTER_TABLE))
  if (nrow(roster_df) > 0)
    DBI::dbWriteTable(con, AI_ROSTER_TABLE,
                      roster_df[c("repo_id", "owner", "name", "node_id", "done")], append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

load_ai_roster <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbReadTable(con, AI_ROSTER_TABLE)
}

# ---- enumerate --------------------------------------------------------------
#' Build the FULL active github roster from the published summary's embedded repos
#' table (NOT the star-filtered vcs_signals_summary that run_enumerate uses): the
#' zero-signal long tail is exactly where solo maintainers quietly adopt an AI tool.
#' Uses the native owner/name/node_id columns, so no slug split and node_id rides
#' through for the identity reconcile. Re-resolves owner/name from node_id for every row
#' that already carries one (mirrors resolve_node_ids's build_resolve_query/parse_resolve
#' pair, github.R:107/204, followRenames:true already baked in), so a rename since the
#' row's node_id was first attached does not leave a stale slug flowing into Task 7/9's
#' owner/name-keyed queries. Same download as backfill.R's enumerate.
run_enumerate_ai <- function(io, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  if (!isTRUE(io$download("vcs-signals-summary.db", out_dir)))
    stop("could not download vcs-signals-summary.db from the published release; nothing to enumerate")
  summary_path <- file.path(out_dir, "vcs-signals-summary.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), summary_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con,
    "SELECT repo_id, owner, name, node_id FROM repos WHERE host = 'github' AND status = 'active'")
  roster <- data.frame(repo_id = rows$repo_id, owner = rows$owner, name = rows$name,
                       node_id = rows$node_id, done = 0L, stringsAsFactors = FALSE)

  # Re-resolve owner/name from the immutable node_id for rows that already have one, so a
  # rename since the last resolve does not leave a stale slug: an unrelated repo that
  # later squats the old slug would otherwise have its markers/PRs/commits misattributed
  # into the immutable vcs_ai_signals table under this node_id's repo_id. A batch that
  # still faults after the retry keeps its pre-existing owner/name (retried on the next
  # enumerate), never dropped from the roster.
  have_id <- !is.na(roster$node_id) & nzchar(roster$node_id)
  for (rowset in chunk(which(have_id), CHEAP_BATCH)) {
    sub <- roster[rowset, , drop = FALSE]
    res <- tryCatch(io$graphql(build_resolve_query(sub$owner, sub$name)),
                    error = function(e) list(.err = TRUE))
    Sys.sleep(BATCH_DELAY_S)
    ok <- is.list(res) && is.null(res$.err) && !is.null(res$data) &&
      (is.null(res$errors) || errors_are_alias_not_found(res$errors))
    if (!ok) next
    pr <- parse_resolve(res$data, nrow(sub))
    for (j in seq_len(nrow(sub))) {
      r <- pr[pr$idx == (j - 1L), ]
      if (is.na(r$node_id) || is.na(r$name_with_owner)) next
      parts <- strsplit(r$name_with_owner, "/", fixed = TRUE)[[1]]
      roster$owner[rowset[j]] <- parts[1]
      roster$name[rowset[j]]  <- paste(parts[-1], collapse = "/")
    }
  }

  message(sprintf("ai enumerate: %d active github repos", nrow(roster)))
  write_ai_roster(file.path(out_dir, "vcs-ai-roster.db"), roster)
}

# ---- CLI dispatch -----------------------------------------------------------
main <- function(mode, out_dir) {
  token <- Sys.getenv("VCS_SIGNALS_TOKEN")
  io <- list(
    graphql        = default_io(token)$graphql,
    search         = function(owner, name, query, delay = SEARCH_DELAY_S)
                       search_earliest_commit(token, owner, name, query, delay),
    release_exists = function() gh_release_exists(RELEASE_REPO),
    download       = function(pattern, dir) gh_release_download(RELEASE_REPO, pattern, dir),
    upload         = function(path) gh_release_upload(RELEASE_REPO, path))

  if (mode == "enumerate") {
    run_enumerate_ai(io, out_dir)
  } else {
    stop("usage: ai_backfill.R [enumerate|cheap|gate|deep|merge]")
  }
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  mode <- if (length(args) >= 1) args[1] else ""
  out_dir <- Sys.getenv("VCS_OUT", "out")
  main(mode, out_dir)
}
