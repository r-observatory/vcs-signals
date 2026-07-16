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
