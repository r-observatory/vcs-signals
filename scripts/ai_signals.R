# scripts/ai_signals.R - pure AI-tooling detection: classifiers, naming
# threshold, tool ordering, onset reducer, summary rollups. No I/O.

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

.ai_empty_evidence <- function()
  data.frame(tool = character(), tier = character(), marker = character(),
             agnostic = logical(), stringsAsFactors = FALSE)

#' Tier-D config markers present in the repo's root tree entry names and its
#' .github tree entry names (both files and dirs appear as entry names). One
#' row per matched marker; agnostic flags the tool-agnostic AGENTS.md.
classify_tree_markers <- function(root_entries, github_entries) {
  root_entries <- root_entries %||% character(0)
  github_entries <- github_entries %||% character(0)
  rows <- lapply(AI_MARKERS, function(m) {
    present <- if (identical(m$location, "github")) m$path %in% github_entries
               else m$path %in% root_entries
    if (!present) return(NULL)
    data.frame(tool = m$tool, tier = "D", marker = m$path,
               agnostic = isTRUE(m$agnostic), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(.ai_empty_evidence())
  do.call(rbind, rows)
}

#' Normalize one ignore-file line to a bare token: strip inline comment, leading
#' "./", regex anchors, surrounding whitespace, and one trailing "/" or "*".
.ai_norm_ignore <- function(line) {
  x <- sub("#.*$", "", line)
  x <- trimws(x)
  x <- sub("^\\^", "", x); x <- sub("\\$$", "", x)
  x <- gsub("\\\\", "", x)              # drop regex escapes (\.cursor -> .cursor)
  x <- sub("^\\./", "", x)
  x <- sub("[/*]$", "", x)
  x
}

#' Tier-D evidence from a whole-entry match of an ignore-file token against a
#' marker path. Anchored equality only (never a substring), so tokens like
#' codex_output or a gemini-protocol path do not collide.
scan_ignore_tokens <- function(gitignore_lines, rbuildignore_lines) {
  toks <- unique(vapply(c(gitignore_lines %||% character(0),
                          rbuildignore_lines %||% character(0)),
                        .ai_norm_ignore, character(1)))
  toks <- toks[nzchar(toks)]
  rows <- lapply(AI_MARKERS, function(m) {
    if (isTRUE(m$agnostic)) return(NULL)      # AGENTS.md token is too generic to trust in ignore files
    if (!(m$path %in% toks)) return(NULL)
    data.frame(tool = m$tool, tier = "D", marker = paste0("ignore:", m$path),
               agnostic = FALSE, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(.ai_empty_evidence())
  do.call(rbind, rows)
}

.ai_rows <- function(tools, tier) {
  tools <- unique(tools)
  if (!length(tools)) return(.ai_empty_evidence())
  data.frame(tool = tools, tier = tier, marker = tier, agnostic = FALSE,
             stringsAsFactors = FALSE)
}

#' Tier A: commit author email/login exactly (case-normalized) in the bot
#' allowlist and not in the denylist.
match_bot_identity <- function(emails, logins) {
  ids <- tolower(c(emails %||% character(0), logins %||% character(0)))
  ids <- ids[!ids %in% tolower(AI_BOT_DENYLIST)]
  hit <- ids[ids %in% tolower(names(AI_BOT_ALLOWLIST))]
  if (!length(hit)) return(.ai_empty_evidence())
  .ai_rows(unname(AI_BOT_ALLOWLIST[match(hit, tolower(names(AI_BOT_ALLOWLIST)))]), "A")
}

#' Tier B: a commit message matches a canonical AI trailer pattern.
scan_trailers <- function(messages) {
  msgs <- tolower(messages %||% character(0))
  tools <- character(0)
  for (p in AI_TRAILER_PATTERNS)
    if (any(grepl(p$pattern, msgs, perl = TRUE))) tools <- c(tools, p$tool)
  .ai_rows(tools, "B")
}

#' Tier C: an author display name ends with a known agent suffix.
match_author_suffix <- function(author_names) {
  nm <- author_names %||% character(0)
  tools <- character(0)
  for (s in AI_AUTHOR_SUFFIXES)
    if (any(endsWith(trimws(nm), s$suffix))) tools <- c(tools, s$tool)
  .ai_rows(tools, "C")
}

#' PR channel: a PR was opened by an allowlisted agent login (exact, lowercase).
detect_pr_agents <- function(pr_logins) {
  lg <- tolower(pr_logins %||% character(0))
  hit <- lg[lg %in% tolower(names(AI_PR_AGENT_LOGINS))]
  if (!length(hit)) return(.ai_empty_evidence())
  .ai_rows(unname(AI_PR_AGENT_LOGINS[match(hit, tolower(names(AI_PR_AGENT_LOGINS)))]), "PR")
}

#' Post-cutoff agent-PR logins: an allowlisted agent login whose PR was created at or
#' after AI_PR_CUTOFF. A pre-cutoff match predates the agent era (a login collision) and
#' is dropped. Internal.
.ai_agent_pr_logins <- function(pr, cutoff) {
  prs <- if (is.null(pr) || is.null(pr$prs)) NULL else pr$prs
  if (is.null(prs) || nrow(prs) == 0) return(character(0))
  intra <- !is.na(prs$created_at) & prs$created_at >= cutoff
  prs$login[intra]
}

#' Combine one repo's cheap-pass parsed tree + PR results into a raw-evidence frame
#' (tool, tier, marker, agnostic): classify_tree_markers + scan_ignore_tokens on the
#' tree, detect_pr_agents on the post-cutoff agent logins. Pure. A NULL tree or NULL pr
#' (that channel errored or is empty) contributes no rows from that channel, never a
#' "no AI" verdict. Returns the typed 0-row frame when nothing matches.
assemble_repo_evidence <- function(tree, pr, cutoff = AI_PR_CUTOFF) {
  tree <- tree %||% list()
  markers <- classify_tree_markers(tree$root_entries, tree$github_entries)
  ign <- scan_ignore_tokens(tree$gitignore_lines, tree$rbuildignore_lines)
  pr_ev <- detect_pr_agents(.ai_agent_pr_logins(pr, cutoff))
  do.call(rbind, list(markers, ign, pr_ev))
}

#' The gate: TRUE iff a repo shows ANY AI evidence (a marker, an ignore token, or a
#' post-cutoff agent PR). Absence of evidence is never gated in, so a repo that errored
#' in the cheap pass (no evidence frame) is deferred, never recorded as clean.
repo_has_ai_signal <- function(evidence) {
  !is.null(evidence) && nrow(evidence) > 0
}

#' PR-channel onset: the earliest createdAt among the repo's post-cutoff agent PRs, or
#' NA. A PR createdAt is a real dated event, so this is recorded exact by build_onset_map.
earliest_agent_pr_date <- function(pr, cutoff = AI_PR_CUTOFF) {
  prs <- if (is.null(pr) || is.null(pr$prs)) NULL else pr$prs
  if (is.null(prs) || nrow(prs) == 0) return(NA_character_)
  intra <- !is.na(prs$created_at) & prs$created_at >= cutoff
  agent <- intra & tolower(prs$login) %in% tolower(names(AI_PR_AGENT_LOGINS))
  d <- prs$created_at[agent]
  if (!length(d)) return(NA_character_)
  min(d)
}

.ai_split_tiers <- function(s) {
  if (is.na(s) || !nzchar(s)) return(character(0))
  trimws(strsplit(s, ",", fixed = TRUE)[[1]])
}

#' A repo is nameable iff some NON-agnostic tool has Tier A, or >=2 distinct tiers.
meets_naming_threshold <- function(ai_rows) {
  if (is.null(ai_rows) || nrow(ai_rows) == 0) return(FALSE)
  keep <- !as.logical(ai_rows$agnostic)
  if (!any(keep)) return(FALSE)
  sub <- ai_rows[keep, , drop = FALSE]
  any(vapply(seq_len(nrow(sub)), function(i) {
    tiers <- .ai_split_tiers(sub$evidence_tiers[i])
    ("A" %in% tiers) || (length(unique(tiers)) >= 2)
  }, logical(1)))
}

#' Strongest (lowest) tier priority among a comma tier string.
.ai_tier_rank <- function(s) {
  tiers <- .ai_split_tiers(s)
  if (!length(tiers)) return(99L)
  r <- suppressWarnings(min(TIER_PRIORITY[tiers], na.rm = TRUE))
  if (is.finite(r)) as.integer(r) else 99L
}

#' Non-agnostic tools ordered by (date ASC, censored ASC, tier ASC, tool ASC).
order_ai_tools <- function(ai_rows) {
  if (is.null(ai_rows) || nrow(ai_rows) == 0) return(character(0))
  sub <- ai_rows[!as.logical(ai_rows$agnostic), , drop = FALSE]
  if (nrow(sub) == 0) return(character(0))
  rank <- vapply(sub$evidence_tiers, .ai_tier_rank, integer(1))
  d <- sub$first_seen_date; d[is.na(d)] <- "9999-99-99"   # NA dates sort last
  ord <- order(d, sub$first_seen_censored, rank, sub$tool)
  sub$tool[ord]
}

#' Reduce one (repo_id, tool) group's rows to a single row by the onset rules.
.ai_reduce_group <- function(g) {
  dates <- g$first_seen_date; cens <- as.integer(g$first_seen_censored)
  ok <- !is.na(dates)
  dates <- dates[ok]; cens <- cens[ok]
  exact <- dates[cens == 0L]; floors <- dates[cens == 1L]
  if (!length(dates)) { fs <- NA_character_; fc <- 0L }
  else if (!length(exact)) { fs <- min(floors); fc <- 1L }         # floors only
  else if (!length(floors)) { fs <- min(exact); fc <- 0L }         # exacts only
  else if (min(exact) <= min(floors)) { fs <- min(exact); fc <- 0L } # exact consistent with floor
  else {                                                            # exact later than a floor
    warning(sprintf("ai onset contradiction for %s/%s: exact %s later than floor %s; keeping floor",
                    g$repo_id[1], g$tool[1], min(exact), min(floors)))
    fs <- min(floors); fc <- 1L
  }
  tiers <- sort(unique(unlist(lapply(g$evidence_tiers, .ai_split_tiers))))
  lc <- g$last_confirmed_date[!is.na(g$last_confirmed_date)]
  data.frame(repo_id = g$repo_id[1], tool = g$tool[1], first_seen_date = fs,
             first_seen_censored = fc,
             evidence_tiers = if (length(tiers)) paste(tiers, collapse = ",") else NA_character_,
             authored = as.integer(any(as.integer(g$authored) == 1L, na.rm = TRUE)),
             last_confirmed_date = if (length(lc)) max(lc) else NA_character_,
             stringsAsFactors = FALSE)
}

.ai_empty_signals <- function()
  data.frame(repo_id = character(), tool = character(), first_seen_date = character(),
             first_seen_censored = integer(), evidence_tiers = character(),
             authored = integer(), last_confirmed_date = character(),
             stringsAsFactors = FALSE)

#' Merge prior + incoming vcs_ai_signals rows per (repo_id, tool) by the six
#' column rules. Read-modify-write: callers write the returned set wholesale.
ai_onset_reducer <- function(prior_rows, incoming_rows) {
  all_rows <- rbind(prior_rows, incoming_rows)
  if (is.null(all_rows) || nrow(all_rows) == 0) return(.ai_empty_signals())
  key <- paste(all_rows$repo_id, all_rows$tool, sep = "\r")
  parts <- lapply(split(all_rows, key), .ai_reduce_group)
  do.call(rbind, parts)
}

#' New-tool gate for the weekly incremental. Returns the subset of flagged repo_ids that
#' carry at least one (repo_id, tool) pair in THIS week's cheap-pass evidence that is NOT
#' already present in the published vcs_ai_signals detail for that repo. A repo whose current
#' tools are all already published (their onsets are immutable and done) is skipped, so the
#' deep matrix only re-onsets genuinely new adoptions. agents-md is not special-cased: it is a
#' stored tool row like any other (build_ai_detail / ai_onset_reducer treat it uniformly; only
#' the summary rollup excludes agnostic tools), so a newly-adopted agents-md selects its repo
#' and an already-published one does not. published_detail may be empty (the first weekly run
#' before any onset has been published), in which case every flagged repo is new. Pure.
select_incremental_repos <- function(flagged, evidence, published_detail) {
  if (is.null(flagged) || nrow(flagged) == 0) return(character(0))
  if (is.null(evidence) || nrow(evidence) == 0) return(character(0))
  cur_key <- paste(evidence$repo_id, evidence$tool, sep = "\r")
  pub_key <- if (is.null(published_detail) || nrow(published_detail) == 0) character(0)
             else paste(published_detail$repo_id, published_detail$tool, sep = "\r")
  new_repos <- unique(evidence$repo_id[!(cur_key %in% pub_key)])
  flagged$repo_id[flagged$repo_id %in% new_repos]
}

#' Confirmation rows for the weekly incremental: for every (repo_id, tool) pair present in
#' BOTH this week's cheap-pass evidence and the published vcs_ai_signals detail (an
#' already-published tool the roster still shows this week), emit a lightweight row that
#' carries only last_confirmed_date = today, in the exact 7-col vcs_ai_signals shape
#' ai_onset_reducer consumes. This reuses the cheap-pass data already fetched for the gate -
#' no new API calls. The returned row's first_seen_date is NA (dropped by the reducer's
#' non-NA date filter), evidence_tiers is NA (contributes nothing to the tier union), and
#' authored is 0 (OR'd with the prior value) - so the prior row's exact onset, tiers, and
#' authored flag all survive untouched through ai_onset_reducer; only last_confirmed_date
#' advances via max(). Pure.
select_confirmation_rows <- function(evidence, published_detail, today) {
  if (is.null(evidence) || nrow(evidence) == 0) return(.ai_empty_signals())
  if (is.null(published_detail) || nrow(published_detail) == 0) return(.ai_empty_signals())
  cur_key <- paste(evidence$repo_id, evidence$tool, sep = "\r")
  pub_key <- paste(published_detail$repo_id, published_detail$tool, sep = "\r")
  hit <- !duplicated(cur_key) & (cur_key %in% pub_key)
  if (!any(hit)) return(.ai_empty_signals())
  data.frame(repo_id = evidence$repo_id[hit], tool = evidence$tool[hit],
             first_seen_date = NA_character_, first_seen_censored = 0L,
             evidence_tiers = NA_character_, authored = 0L,
             last_confirmed_date = today, stringsAsFactors = FALSE)
}

#' Fork / template guard for Tier-D marker onsets. A forked repo (is_fork) or a
#' marker present in the repo's first commit (template seeding, first_commit_touches)
#' inherits that marker rather than adopting it, so its Tier-D onset is censored to a
#' "<=" floor (first_seen_censored = 1). Only Tier D is guarded - commit-message and
#' author tiers carry their own dated, repo-specific evidence. `parent` is reserved
#' for a future parent-tree uniqueness check; here a fork conservatively censors every
#' Tier-D marker (over-censoring is the safe direction for an immutable onset, and any
#' genuine commit-tier onset later dominates the censored floor in the reducer).
apply_fork_guard <- function(evidence, is_fork, parent, first_commit_touches) {
  if (is.null(evidence) || nrow(evidence) == 0) return(evidence)
  if (!"first_seen_censored" %in% names(evidence)) evidence$first_seen_censored <- 0L
  templated <- evidence$marker %in% (first_commit_touches %||% character(0))
  inherited <- templated | isTRUE(is_fork)
  guarded <- inherited & (evidence$tier == "D")
  evidence$first_seen_censored[guarded] <- 1L
  evidence
}

#' Build this scan's vcs_ai_signals detail rows for one repo. Groups the raw tier
#' evidence (classify_tree_markers / scan_ignore_tokens / scan_trailers / match_* /
#' detect_pr_agents rows, optionally fork-guarded) by (repo_id, tool), attaches each
#' signal's onset from `onsets` (keyed by tool + marker), then collapses per tool
#' through ai_onset_reducer: evidence_tiers set-union, onset by the
#' exact-dominates-floor rules, authored logical-OR, last_confirmed max. Output is
#' exactly the 7-col shape ai_onset_reducer consumes cross-run, so B2 feeds it straight
#' into the prior-vs-incoming merge.
#'
#' `onsets` is data.frame(tool, marker, first_seen_date, first_seen_censored) (NULL or
#' 0-row is allowed). A signal with no matching onset keeps first_seen_date NA and
#' whatever censoring apply_fork_guard already put on its evidence row. Any censoring is
#' the max of the onset's and the fork-guard's, so an inherited marker with an exact
#' onset is still recorded as a floor. `last_confirmed` (the scan date) is stamped on
#' every row, since a HEAD marker is confirmed present now.
build_ai_detail <- function(repo_id, raw_evidence, onsets, last_confirmed) {
  if (is.null(raw_evidence) || nrow(raw_evidence) == 0) return(.ai_empty_signals())
  if (is.null(onsets) || nrow(onsets) == 0)
    onsets <- data.frame(tool = character(0), marker = character(0),
                         first_seen_date = character(0), first_seen_censored = integer(0),
                         stringsAsFactors = FALSE)
  ev <- raw_evidence
  if (!"authored" %in% names(ev)) ev$authored <- 0L
  guard_c <- if ("first_seen_censored" %in% names(ev)) as.integer(ev$first_seen_censored)
             else rep(0L, nrow(ev))
  m <- match(paste(ev$tool, ev$marker, sep = "\r"),
             paste(onsets$tool, onsets$marker, sep = "\r"))
  onset_c <- as.integer(onsets$first_seen_censored[m]); onset_c[is.na(onset_c)] <- 0L
  candidates <- data.frame(
    repo_id = repo_id, tool = ev$tool,
    first_seen_date = onsets$first_seen_date[m],
    first_seen_censored = pmax(onset_c, guard_c),
    evidence_tiers = ev$tier,
    authored = as.integer(ev$authored),
    last_confirmed_date = last_confirmed,
    stringsAsFactors = FALSE)
  ai_onset_reducer(.ai_empty_signals(), candidates)
}

#' Assemble the per-(tool, marker) onset frame build_ai_detail consumes, from a repo's
#' evidence plus the fetched onset dates. Each evidence row is matched to its onset source
#' by tier/marker shape: a Tier-D row (marker is a path, or "ignore:<path>") takes
#' marker_dates[[<path>]] exact; a PR row (marker == "PR") takes pr_date exact; a Tier
#' A/B/C row (marker == tier) takes commit_onsets for (tool, tier), exact when that onset's
#' `confirmed` is TRUE (a structured author match or a scan_trailers confirm) else a
#' censored floor - a fuzzy message-search candidate is never written as an exact immutable
#' onset. Pure: all dates/confirms are passed in. A row with no resolved onset keeps
#' first_seen_date NA (build_ai_detail leaves it NA in the detail row).
build_onset_map <- function(evidence, marker_dates = list(),
                            commit_onsets = NULL, pr_date = NA_character_) {
  empty <- data.frame(tool = character(), marker = character(),
                      first_seen_date = character(), first_seen_censored = integer(),
                      stringsAsFactors = FALSE)
  if (is.null(evidence) || nrow(evidence) == 0) return(empty)
  co_key <- if (is.null(commit_onsets) || nrow(commit_onsets) == 0) character(0)
            else paste(commit_onsets$tool, commit_onsets$tier, sep = "\r")
  rows <- lapply(seq_len(nrow(evidence)), function(i) {
    tool <- evidence$tool[i]; tier <- evidence$tier[i]; marker <- evidence$marker[i]
    fs <- NA_character_; fc <- 0L
    if (identical(tier, "D")) {
      path <- sub("^ignore:", "", marker)
      fs <- if (path %in% names(marker_dates)) marker_dates[[path]] else NA_character_
    } else if (identical(tier, "PR")) {
      fs <- pr_date
    } else {
      j <- match(paste(tool, tier, sep = "\r"), co_key)
      if (!is.na(j)) {
        fs <- commit_onsets$first_seen_date[j]
        fc <- if (isTRUE(as.logical(commit_onsets$confirmed[j]))) 0L else 1L
      }
    }
    data.frame(tool = tool, marker = marker, first_seen_date = fs,
               first_seen_censored = as.integer(fc), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[!duplicated(paste(out$tool, out$marker, sep = "\r")), , drop = FALSE]
}

.ai_empty_rollups <- function()
  data.frame(repo_id = character(), ai_markers_detected = logical(),
             ai_first_tool = character(), ai_first_date = character(),
             ai_tool_count = integer(), ai_tools = character(),
             ai_latest_tool = character(), ai_latest_date = character(),
             stringsAsFactors = FALSE)

#' Per-repo AI rollups for the summary. Only repos meeting the naming threshold
#' get a row (so the summary join yields NULL, never FALSE, for the rest).
#' agents-md is excluded from count/tools/first/latest.
build_ai_rollups <- function(ai_signals) {
  if (is.null(ai_signals) || nrow(ai_signals) == 0) return(.ai_empty_rollups())
  if (!"agnostic" %in% names(ai_signals))
    ai_signals$agnostic <- ai_signals$tool == "agents-md"
  parts <- lapply(split(ai_signals, ai_signals$repo_id), function(g) {
    if (!meets_naming_threshold(g)) return(NULL)
    ordered <- order_ai_tools(g)               # non-agnostic, chronological
    if (!length(ordered)) return(NULL)
    counted <- g[g$tool %in% ordered, , drop = FALSE]
    first_tool <- ordered[1]
    last_tool <- ordered[length(ordered)]
    data.frame(repo_id = g$repo_id[1], ai_markers_detected = TRUE,
               ai_first_tool = first_tool,
               ai_first_date = counted$first_seen_date[counted$tool == first_tool][1],
               ai_tool_count = length(ordered),
               ai_tools = paste(ordered, collapse = ","),
               ai_latest_tool = last_tool,
               ai_latest_date = counted$first_seen_date[counted$tool == last_tool][1],
               stringsAsFactors = FALSE)
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(.ai_empty_rollups())
  do.call(rbind, parts)
}
