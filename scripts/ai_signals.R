# scripts/ai_signals.R - pure AI-tooling detection: classifiers, naming
# threshold, tool ordering, onset reducer, summary rollups. No I/O.

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

.ai_empty_evidence <- function()
  data.frame(tool = character(), tier = character(), marker = character(),
             agnostic = logical(), stringsAsFactors = FALSE)

#' The deliberate-adoption subset of AI_MARKERS: every marker whose class is not "ambient".
#' Ambient markers (an IDE writes them regardless of AI use, e.g. .positai) are excluded from
#' the AI evidence entirely - they must never reach the naming threshold, the rollups, or the
#' tool set. A marker with no `class` field defaults to "deliberate", so the existing markers
#' are unaffected. Recording ambient markers as a dev-tooling datum is a separate signal. Pure.
ai_deliberate_markers <- function(markers = AI_MARKERS)
  Filter(function(m) !identical(m$class %||% "deliberate", "ambient"), markers)

#' Config paths present in the repo's root and .github entry names (files and folders
#' both appear as names, subfolder entries under their prefix). One row per matched
#' path; agnostic flags AGENTS.md and .agents, which name no tool.
classify_tree_markers <- function(root_entries, github_entries) {
  root_entries <- root_entries %||% character(0)
  github_entries <- github_entries %||% character(0)
  rows <- lapply(ai_deliberate_markers(), function(m) {
    hay <- if (identical(m$location, "github")) github_entries else root_entries
    if (!(m$path %in% hay)) return(NULL)
    # A folder counts only when it holds the file the tool reads.
    if (!is.null(m$requires) && !any(grepl(m$requires, hay, perl = TRUE))) return(NULL)
    # A .gemini folder of review settings belongs to Gemini Code Assist.
    if (identical(m$path, ".gemini") && identical(classify_gemini_dir(root_entries), "review"))
      return(NULL)
    data.frame(tool = m$tool, tier = "D", marker = m$path,
               agnostic = isTRUE(m$agnostic), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(.ai_empty_evidence())
  do.call(rbind, rows)
}

#' "review" when a .gemini folder's listing is non-empty and holds only the files
#' Gemini Code Assist reads, else "authoring". Pure.
classify_gemini_dir <- function(root_entries) {
  rule <- Find(function(r) identical(r$path, ".gemini"), AI_REVIEW_FILES)
  inside <- sub("^\\.gemini/", "", grep("^\\.gemini/[^/]+$", root_entries %||% character(0), value = TRUE))
  if (length(inside) && all(inside %in% rule$only)) "review" else "authoring"
}

#' Review tools' configuration files present at the root, one row each. Pure.
match_review_files <- function(root_entries) {
  root_entries <- root_entries %||% character(0)
  rows <- lapply(AI_REVIEW_FILES, function(r) {
    if (!(r$path %in% root_entries)) return(NULL)
    if (!is.null(r$only) && !identical(classify_gemini_dir(root_entries), "review")) return(NULL)
    data.frame(tool = r$tool, marker = r$path, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame(tool = character(), marker = character(), stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

# ---- Development-tooling classifier (pure, no I/O) --------------------------
# Parallel to classify_tree_markers, but the output shape is DIFFERENT: one WIDE row (one 0/1
# INTEGER per DEV_TOOLING_MARKERS col, plus the DEV_TOOLING_DERIVED columns), not one row per
# match. The column set is derived from those two lists so the classifier,
# the DDL (dev_tooling_create_sql), and the empty helper (.devtool_empty) cannot drift. This
# reads DEV_TOOLING_MARKERS directly and does NOT route through ai_deliberate_markers(): that
# ambient filter is AI-only, and ambient markers (.positai) ARE dev-tooling evidence.

#' The flag column names, in DEV_TOOLING_MARKERS order. Config-derived.
dev_tooling_marker_cols <- function() vapply(DEV_TOOLING_MARKERS, function(m) m$col, character(1))

#' The derived column names, in DEV_TOOLING_DERIVED order.
dev_tooling_derived_cols <- function() vapply(DEV_TOOLING_DERIVED, function(d) d$col, character(1))

#' The full classifier output column order: the flag columns, then the derived ones.
dev_tooling_columns <- function() c(dev_tooling_marker_cols(), dev_tooling_derived_cols())

#' SQLite type of every column, in dev_tooling_columns() order; the DDL, ALTER and empty frame read it.
dev_tooling_column_types <- function() {
  c(stats::setNames(rep("INTEGER", length(DEV_TOOLING_MARKERS)), dev_tooling_marker_cols()),
    stats::setNames(vapply(DEV_TOOLING_DERIVED, function(d) d$type, character(1)), dev_tooling_derived_cols()))
}

#' Typed 0-row frame with the column set and types classify_dev_tooling produces.
.devtool_empty <- function() {
  cols <- lapply(dev_tooling_column_types(), function(t) if (identical(t, "TEXT")) character(0) else integer(0))
  do.call(data.frame, c(cols, list(stringsAsFactors = FALSE, check.names = FALSE)))
}

#' One wide row from a repo's root and .github entry names, plus the parsed contents read
#' when supplied (`repo`). A column whose source is not in the inputs is NA, never 0.
classify_dev_tooling <- function(root_entries, github_entries, repo = NULL) {
  root_entries <- root_entries %||% character(0)
  github_entries <- github_entries %||% character(0)
  hit <- function(m) {
    hay <- switch(m$location %||% "root",
                  root   = root_entries,
                  github = github_entries,
                  both   = c(root_entries, github_entries))
    if (identical(m$match %||% "exact", "suffix"))
      any(vapply(m$paths, function(p) any(endsWith(hay, p)), logical(1)))
    else any(m$paths %in% hay)
  }
  flags <- vapply(DEV_TOOLING_MARKERS, function(m) as.integer(isTRUE(hit(m))), integer(1))
  names(flags) <- dev_tooling_marker_cols()
  row <- as.data.frame(as.list(flags), stringsAsFactors = FALSE)
  row$readme_source <-
    if ("README.qmd" %in% root_entries) "qmd"
    else if ("README.Rmd" %in% root_entries) "rmd"
    else if ("README.md" %in% root_entries) "md"
    else "none"
  # Only the CI configuration flags, never the workflow-text ci_ columns.
  ci_cols <- grep("^ci_", names(flags), value = TRUE)
  row$has_ci <- as.integer(any(flags[ci_cols] == 1L))

  derived <- dev_tooling_derive(root_entries, github_entries, flags, repo)
  for (cn in names(derived)) row[[cn]] <- derived[[cn]] %||% NA
  types <- dev_tooling_column_types()
  for (cn in names(types)) {
    v <- if (cn %in% names(row)) row[[cn]] else NA
    row[[cn]] <- if (identical(types[[cn]], "TEXT")) as.character(v) else as.integer(v)
  }
  out <- row[, dev_tooling_columns(), drop = FALSE]
  attr(out, "rbuildignore_bad_lines") <- attr(derived, "rbuildignore_bad_lines") %||% 0L
  out
}

#' The CREATE TABLE text for vcs_dev_tooling, typed from dev_tooling_column_types(). WITHOUT
#' ROWID because the hot path is a repo_id point lookup; the merger copies this text verbatim.
dev_tooling_create_sql <- function() {
  types <- dev_tooling_column_types()
  ddl <- paste(sprintf("    %s %s", names(types), types), collapse = ",\n")
  sprintf("CREATE TABLE IF NOT EXISTS vcs_dev_tooling (
    repo_id TEXT NOT NULL,
    last_scanned TEXT,
    ruleset_version TEXT,
%s,
    PRIMARY KEY (repo_id)) WITHOUT ROWID", ddl)
}

#' The real repository path for a Tier-D config marker's entry name. AI_MARKERS records the
#' bare entry NAME (matched against tree entry names in classify_tree_markers), but a marker
#' whose location is "github" actually lives under .github/ in the repo
#' (copilot-instructions.md -> .github/copilot-instructions.md), so paging its commit history
#' must use that real path. A root-located marker's path is its name unchanged, and a
#' directory needs no special handling (GraphQL history(path:) resolves a directory path
#' directly). An unknown marker (not in AI_MARKERS) is returned verbatim. Pure.
marker_repo_path <- function(marker) {
  m <- Find(function(x) identical(x$path, marker), AI_MARKERS)
  if (is.null(m)) return(marker)
  if (identical(m$location, "github")) paste0(".github/", m$path) else m$path
}

#' Normalise one ignore-file line to a bare lowercase path: drop an inline comment, a
#' leading "!" (a negated line still says the tool's files are kept), regex anchors and
#' escapes, a leading "./", "/" or "**/", and any trailing "/**", "/*", "/", "*",
#' "($|/)" or "(/|$)". Pure.
.ai_norm_ignore <- function(line) {
  x <- trimws(sub("#.*$", "", line))
  x <- sub("^!", "", x)
  x <- sub("^\\^", "", x); x <- sub("\\$$", "", x)
  x <- gsub("[.]", ".", x, fixed = TRUE); x <- gsub("\\.", ".", x, fixed = TRUE)
  x <- gsub("\\", "", x, fixed = TRUE)
  x <- sub("^(\\./|/|\\*\\*/)", "", x)
  repeat {
    y <- sub("(/\\*\\*|/\\*|/|\\*|\\(\\$\\|/\\)|\\(/\\|\\$\\))$", "", x)
    if (identical(y, x)) break
    x <- y
  }
  tolower(x)
}

#' TRUE where a normalised ignore line names `path` itself or something inside it. Pure.
ai_ignore_line_matches <- function(norm, path) {
  p <- tolower(path)
  norm == p | startsWith(norm, paste0(p, "/"))
}

#' TRUE when a Tier-D marker names an ignore-file entry rather than a committed path.
#' Two decisions hang on this: such a marker has no path whose history can be walked,
#' so it costs no fetch, and its date can only ever be a censored floor. Defined once
#' so neither can drift from the format scan_ignore_tokens produces.
ai_is_ignore_marker <- function(marker) {
  grepl("^(gitignore|rbuildignore):", marker)
}

#' Tier-D evidence from a whole-entry match of an ignore-file token against a
#' marker path. Anchored equality only (never a substring), so tokens like
#' codex_output or a gemini-protocol path do not collide.
#' The two ignore files are scanned SEPARATELY and a marker names the one it came
#' from: "gitignore:.claude", not the old "ignore:.claude", which unioned both
#' and left no way to tell a build-time exclusion from a source-control one. A path in
#' both files yields two rows on purpose; they collapse per (repo, tool) downstream and
#' the pair is a real fact about the repository, not a duplicate.
#'
#' Ambient markers stay OUT of this frame. .positai belongs in the dev-tooling signal,
#' not here: the AI gate flags a repository on any row this returns, so emitting an
#' ambient one would pull a repository whose only marker is an editor artifact into the
#' AI roster. Recording a gitignored .positai needs the dev-tooling classifier to see
#' the ignore lines, which is a separate change to its signature.
scan_ignore_tokens <- function(gitignore_lines, rbuildignore_lines) {
  sources <- list(gitignore = gitignore_lines, rbuildignore = rbuildignore_lines)
  rows <- list()
  add <- function(tool, value) rows[[length(rows) + 1L]] <<-
    data.frame(tool = tool, tier = "D", marker = value, agnostic = FALSE, stringsAsFactors = FALSE)
  for (src in names(sources)) {
    lines <- sources[[src]] %||% character(0)
    # Aider writes this exact line itself, a glob no path rule would match.
    if (any(trimws(lines) == ".aider*")) add("aider", paste0(src, ":.aider*"))
    toks <- unique(vapply(lines, .ai_norm_ignore, character(1), USE.NAMES = FALSE))
    toks <- toks[nzchar(toks)]
    if (!length(toks)) next
    for (m in ai_deliberate_markers()) {
      # AGENTS.md and .agents name no tool, and some editors write a tool's line themselves.
      if (isTRUE(m$agnostic) || identical(m$ignore_line, FALSE)) next
      if (is.character(m$ignore_line) && !(src %in% m$ignore_line)) next
      if (any(ai_ignore_line_matches(toks, m$path))) add(m$tool, paste0(src, ":", m$path))
    }
  }
  if (!length(rows)) return(.ai_empty_evidence())
  do.call(rbind, rows)
}

.ai_rows <- function(tools, tier) {
  tools <- unique(tools)
  if (!length(tools)) return(.ai_empty_evidence())
  data.frame(tool = tools, tier = tier, marker = tier, agnostic = FALSE,
             stringsAsFactors = FALSE)
}

#' Every account address and bot name, lowercased, with its tool and kind. Pure.
.ai_account_index <- function() {
  do.call(rbind, lapply(AI_ACCOUNTS, function(a) {
    kinds <- list(graphql = a$graphql, rest_only = a$rest_only, linked = a$linked, names = a$names)
    do.call(rbind, lapply(names(kinds), function(k) if (length(kinds[[k]]))
      data.frame(value = tolower(kinds[[k]]), tool = a$tool, kind = k, stringsAsFactors = FALSE)))
  }))
}

#' Commits by a tool's account: an author address or bot name, exact and case-normalised,
#' that belongs to a tool's accounts and not to the denylist.
match_bot_identity <- function(emails, logins) {
  ids <- tolower(c(emails %||% character(0), logins %||% character(0)))
  ids <- ids[!ids %in% tolower(AI_BOT_DENYLIST)]
  idx <- .ai_account_index()
  hit <- idx$tool[idx$value %in% ids]
  if (!length(hit)) return(.ai_empty_evidence())
  .ai_rows(unique(hit), "A")
}

#' The tools each message's Assisted-by lines name, per AI_ASSISTED_BY_TOOLS. Pure.
ai_assisted_by_tools <- function(messages) {
  lapply(tolower(messages %||% character(0)), function(m) {
    if (is.na(m)) return(character(0))
    vals <- regmatches(m, gregexpr("(^|\\n)assisted-by:[^\\n]*", m, perl = TRUE))[[1]]
    vals <- sub("^\\n?assisted-by:\\s*", "", vals, perl = TRUE)
    unique(as.character(unlist(lapply(vals, function(v) {
      hit <- Filter(function(p) grepl(p, v, perl = TRUE), names(AI_ASSISTED_BY_TOOLS))
      if (length(hit)) unname(AI_ASSISTED_BY_TOOLS[hit[[1]]]) else character(0)
    }))))
  })
}

#' TRUE for dates inside the VS Code Copilot false window, inclusive. Pure.
.ai_in_false_window <- function(dates) {
  d <- substr(as.character(dates), 1, 10)
  !is.na(d) & d >= AI_COPILOT_VSCODE_FALSE_WINDOW[1] & d <= AI_COPILOT_VSCODE_FALSE_WINDOW[2]
}

#' Commit messages crediting a tool, one row per tool named: match_commit_findings on the
#' messages alone, so the corpus tests the weekly read's matcher. Pure.
scan_trailers <- function(messages) {
  msgs <- as.character(messages %||% character(0))
  if (!length(msgs)) return(.ai_empty_evidence())
  h <- match_commit_findings(data.frame(oid = as.character(seq_along(msgs)), committed_at = NA_character_,
    message = msgs, author_name = "", author_email = "", author_login = "", stringsAsFactors = FALSE))
  .ai_rows(h$tool[h$code == "B" & h$role == "authoring"], "B")
}

#' Check a commit-search hit against the rule it was searched for. Commit search does no
#' regex, so an unchecked hit dates only a floor. The Assisted-by rule takes its tool from
#' the line, and a VS Code Copilot hit inside the false window names no tool. Pure.
verify_search_hit <- function(rule, tier, hit) {
  msg <- .nn(hit$message, NA_character_)
  aut <- .nn(hit$author, NA_character_)
  if (identical(rule$key, "msg.any.assisted-by")) {
    mapped <- if (is.na(msg)) character(0) else ai_assisted_by_tools(msg)[[1]]
    return(list(tool = if (length(mapped)) mapped[1] else NA_character_, tier = tier,
                confirmed = length(mapped) > 0))
  }
  tool <- rule$tool
  confirmed <- if (identical(tier, "B")) {
    !is.na(msg) && any(grepl(rule$pattern, tolower(msg), perl = TRUE))
  } else {
    !is.na(aut) && any(endsWith(trimws(aut), rule$suffix))
  }
  if (identical(rule$key, "msg.copilot.vscode") && isTRUE(.ai_in_false_window(.nn(hit$date, NA_character_)))) {
    confirmed <- FALSE
    tool <- NA_character_
  }
  list(tool = tool, tier = tier, confirmed = isTRUE(confirmed))
}

.ai_empty_commit_hits <- function()
  data.frame(oid = character(), committed_at = character(), tool = character(), code = character(),
             rule_key = character(), role = character(), stringsAsFactors = FALSE)

#' Every rule a commit matches, read locally: commits by a tool's account (from the author
#' address or bot name, never the GraphQL login), commits crediting it, an author-name suffix,
#' and review credits (role review). Pure.
match_commit_findings <- function(commits) {
  if (is.null(commits) || !nrow(commits)) return(.ai_empty_commit_hits())
  lower <- function(x) { x <- tolower(as.character(x)); x[is.na(x)] <- ""; x }
  em <- lower(commits$author_email); nm <- trimws(lower(commits$author_name)); msg <- lower(commits$message)
  hits <- list()
  put <- function(rows, tool, code, key, role = "authoring") {
    rows <- !is.na(rows) & rows
    if (any(rows)) hits[[length(hits) + 1L]] <<- data.frame(
      oid = commits$oid[rows], committed_at = commits$committed_at[rows], tool = tool, code = code,
      rule_key = key, role = role, stringsAsFactors = FALSE)
  }
  for (a in AI_ACCOUNTS) {
    rest <- tolower(a$rest_only)
    put((em %in% tolower(c(a$graphql, a$linked)) | nm %in% tolower(a$names)) & !(em %in% rest),
        a$tool, "A", NA_character_)
    for (addr in a$rest_only) put(em == tolower(addr), a$tool, "A", paste0("account.", addr))
  }
  for (r in AI_TRAILER_PATTERNS) {
    if (identical(r$key, "msg.any.assisted-by")) next
    m <- grepl(r$pattern, msg, perl = TRUE)
    if (identical(r$key, "msg.copilot.vscode")) m <- m & !.ai_in_false_window(commits$committed_at)
    put(m, r$tool, "B", r$key)
  }
  named <- ai_assisted_by_tools(msg)
  for (tl in unique(unlist(named)))
    put(vapply(named, function(x) tl %in% x, logical(1)), tl, "B", "msg.any.assisted-by")
  for (s in AI_AUTHOR_SUFFIXES) put(endsWith(nm, tolower(s$suffix)), s$tool, "C", s$key)
  for (r in AI_REVIEW_RULES) put(grepl(r$pattern, msg, perl = TRUE), r$tool, "B", r$key, role = "review")
  if (!length(hits)) return(.ai_empty_commit_hits())
  do.call(rbind, hits)
}

#' Every keyed rule with its revision and the text a revision pins. Pure.
ai_rule_rev_table <- function() {
  one <- function(r, pattern) data.frame(key = r$key, rev = as.integer(r$rev), since_ruleset = r$since_ruleset,
                                         pattern = pattern, query = r$query %||% NA_character_,
                                         stringsAsFactors = FALSE)
  pr_rules <- if (exists("AI_PR_RULES")) AI_PR_RULES else list()
  do.call(rbind, c(lapply(AI_TRAILER_PATTERNS, function(r) one(r, r$pattern)),
                   lapply(AI_AUTHOR_SUFFIXES, function(r) one(r, r$suffix)),
                   lapply(AI_REVIEW_RULES, function(r) one(r, r$pattern)),
                   lapply(pr_rules, function(r) one(r, r$pattern))))
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

.ai_empty_found <- function()
  data.frame(tool = character(), tier = character(), marker = character(), agnostic = logical(),
             role = character(), rule_key = character(), onset = character(), onset_censored = integer(),
             newest_at = character(), stringsAsFactors = FALSE)

.ai_found <- function(tool, tier, marker, role = "authoring", rule_key = NA_character_,
                      onset = NA_character_, onset_censored = 1L, newest_at = NA_character_, agnostic = FALSE) {
  if (!length(tool)) return(.ai_empty_found())
  data.frame(tool = tool, tier = tier, marker = marker, agnostic = as.logical(agnostic), role = role,
             rule_key = rule_key, onset = onset, onset_censored = as.integer(onset_censored),
             newest_at = newest_at, stringsAsFactors = FALSE)
}

#' Commits by a tool's accounts, dated by the newest one: the tool was in use by then.
found_from_accounts <- function(counts) {
  if (is.null(counts) || !nrow(counts)) return(.ai_empty_found())
  .ai_found(counts$tool, "A", "A", onset = counts$newest_commit_date, newest_at = counts$newest_commit_date)
}

#' One row per tool and rule a commit read matched, dated by the oldest match. A read that
#' covered the whole default branch dates exactly, any other only a floor.
found_from_commits <- function(hits, whole_history = FALSE) {
  if (is.null(hits) || !nrow(hits)) return(.ai_empty_found())
  groups <- split(hits, paste(hits$tool, hits$code, hits$rule_key, hits$role, sep = "\r"))
  do.call(rbind, unname(lapply(groups, function(g) .ai_found(
    g$tool[1], g$code[1], if (identical(g$code[1], "A")) "A" else g$rule_key[1], role = g$role[1],
    rule_key = g$rule_key[1], onset = min(g$committed_at), onset_censored = if (whole_history) 0L else 1L,
    newest_at = max(g$committed_at)))))
}

#' One row per tool and rule a pull request showed, dated exactly by the earliest one.
found_from_prs <- function(cls) {
  if (is.null(cls) || !nrow(cls)) return(.ai_empty_found())
  parts <- lapply(seq_len(nrow(cls)), function(i) {
    keys <- if (identical(cls$code[i], "PR")) "PR" else strsplit(cls$rule_key[i], ",", fixed = TRUE)[[1]]
    data.frame(tool = cls$tool[i], tier = cls$code[i], marker = keys, role = cls$role[i],
               at = cls$created_at[i], stringsAsFactors = FALSE)
  })
  r <- do.call(rbind, parts)
  groups <- split(r, paste(r$tool, r$tier, r$marker, r$role, sep = "\r"))
  do.call(rbind, unname(lapply(groups, function(g) .ai_found(
    g$tool[1], g$tier[1], g$marker[1], role = g$role[1],
    rule_key = if (identical(g$marker[1], "PR")) NA_character_ else g$marker[1],
    onset = min(g$at), onset_censored = 0L, newest_at = max(g$at)))))
}

#' Everything one repository showed this week. Files and ignore lines get the end of the scan day,
#' a floor the search pass can tighten. A NULL input adds nothing, never a verdict of "no AI". Pure.
assemble_repo_evidence <- function(tree, activity = NULL, accounts = NULL, cutoff = AI_PR_CUTOFF,
                                   scanned_on = format(Sys.Date()), whole_history = FALSE) {
  tree <- tree %||% list()
  at_scan <- paste0(scanned_on, "T23:59:59Z")
  files <- rbind(classify_tree_markers(tree$root_entries, tree$github_entries),
                 scan_ignore_tokens(tree$gitignore_lines, tree$rbuildignore_lines))
  review <- match_review_files(tree$root_entries)
  out <- rbind(
    if (nrow(files)) .ai_found(files$tool, files$tier, files$marker, onset = at_scan,
                               agnostic = files$agnostic) else .ai_empty_found(),
    if (nrow(review)) .ai_found(review$tool, "D", review$marker, role = "review", onset = at_scan)
    else .ai_empty_found(),
    found_from_prs(classify_prs(activity$prs, cutoff)),
    found_from_commits(match_commit_findings(activity$commits), whole_history),
    found_from_accounts(accounts))
  rownames(out) <- NULL
  out
}

#' TRUE when the repository showed a tool the package itself used. Review tools and
#' outside contributors' pull requests never admit it on their own.
repo_has_ai_signal <- function(evidence) {
  if (is.null(evidence) || !nrow(evidence)) return(FALSE)
  if (!"role" %in% names(evidence)) return(TRUE)
  any(evidence$role == "authoring")
}

.ai_empty_review <- function()
  data.frame(repo_id = character(), tool = character(), first_seen_date = character(),
             first_seen_censored = integer(), evidence_tiers = character(), markers = character(),
             assisted_commits = integer(), assisted_measured_on = character(),
             last_confirmed_date = character(), stringsAsFactors = FALSE)

#' A week's review findings in the vcs_ai_review_signals shape, one row each. Pure.
review_rows <- function(found, repo_id, today) {
  r <- found[found$role == "review", , drop = FALSE]
  if (!nrow(r)) return(.ai_empty_review())
  data.frame(repo_id = repo_id, tool = r$tool, first_seen_date = r$onset,
             first_seen_censored = r$onset_censored, evidence_tiers = r$tier, markers = r$marker,
             assisted_commits = NA_integer_, assisted_measured_on = NA_character_,
             last_confirmed_date = today, stringsAsFactors = FALSE)
}

#' vcs_ai_signals rows from a week's findings, one per repository and tool: the package's
#' own use only, and never a rule that does not name its tool. Pure.
build_cheap_rows <- function(found, today) {
  if (is.null(found) || !nrow(found)) return(.ai_empty_signals())
  role <- if ("role" %in% names(found)) found$role else rep("authoring", nrow(found))
  f <- found[role == "authoring" & !(found$marker %in% ai_non_naming_pr_keys()), , drop = FALSE]
  if (!nrow(f)) return(.ai_empty_signals())
  ai_onset_reducer(.ai_empty_signals(), data.frame(
    repo_id = f$repo_id, tool = f$tool, first_seen_date = f$onset,
    first_seen_censored = as.integer(f$onset_censored), evidence_tiers = f$tier, markers = f$marker,
    authored = as.integer(f$tier == "A"), authored_commits = NA_integer_, assisted_commits = NA_integer_,
    last_confirmed_date = today, stringsAsFactors = FALSE))
}

#' The pull request rule keys that name no tool, kept only on the package's own pull requests. Pure.
ai_non_naming_pr_keys <- function()
  vapply(Filter(function(r) !isTRUE(r$names), AI_PR_RULES), `[[`, "", "key")

.ai_empty_pr_findings <- function()
  data.frame(number = integer(), created_at = character(), tool = character(), code = character(),
             rule_key = character(), role = character(), from_fork = integer(), association = character(),
             stringsAsFactors = FALSE)

#' Which tool each pull request shows and whether the package's own people used it: a tool's account
#' not from a fork, or an owner, member or collaborator on a branch in the repository. Pure.
classify_prs <- function(prs, cutoff = AI_PR_CUTOFF) {
  if (is.null(prs)) return(.ai_empty_pr_findings())
  lost <- setdiff(vapply(AI_PR_RULES, `[[`, "", "key"), names(prs))
  # Some rule columns but not all means a rebuild renamed them; stop rather than find nothing.
  if (length(lost) && length(lost) < length(AI_PR_RULES))
    stop("pull request frame lacks rule columns: ", paste(lost, collapse = ", "), call. = FALSE)
  if (!nrow(prs)) return(.ai_empty_pr_findings())
  col <- function(n, d) if (n %in% names(prs)) prs[[n]] else rep(d, nrow(prs))
  number <- as.integer(col("number", NA_integer_)); created <- col("created_at", NA_character_)
  login <- tolower(col("login", NA_character_)); assoc <- col("association", NA_character_)
  cross <- as.logical(col("cross_repo", FALSE)); cross[is.na(cross)] <- FALSE
  insider <- !is.na(assoc) & assoc %in% c("OWNER", "MEMBER", "COLLABORATOR")
  agents <- stats::setNames(unname(AI_PR_AGENT_LOGINS), tolower(names(AI_PR_AGENT_LOGINS)))
  rows <- list()
  put <- function(i, tool, code, key, role) rows[[length(rows) + 1L]] <<- data.frame(
    number = number[i], created_at = created[i], tool = tool, code = code, rule_key = key, role = role,
    from_fork = as.integer(cross[i]), association = if (is.na(assoc[i])) "NONE" else assoc[i],
    stringsAsFactors = FALSE)
  for (i in seq_len(nrow(prs))) {
    if (is.na(created[i]) || created[i] < cutoff) next
    if (!is.na(login[i]) && login[i] %in% names(agents)) {
      put(i, agents[[login[i]]], "PR", "PR", if (cross[i]) "outside" else "authoring")
      next
    }
    matched <- Filter(function(r) isTRUE(prs[[r$key]][i]) &&
                        (is.null(r$min_created) || created[i] >= r$min_created), AI_PR_RULES)
    for (tl in unique(vapply(matched, `[[`, "", "tool"))) {
      mine <- Filter(function(r) identical(r$tool, tl), matched)
      naming <- vapply(Filter(function(r) isTRUE(r$names), mine), `[[`, "", "key")
      if (length(naming)) {
        put(i, tl, "PB", paste(naming, collapse = ","),
            if (!cross[i] && insider[i]) "authoring" else "outside")
      } else if (!cross[i] && insider[i]) {
        put(i, tl, "PB", paste(vapply(mine, `[[`, "", "key"), collapse = ","), "authoring")
      }
    }
  }
  if (!length(rows)) return(.ai_empty_pr_findings())
  do.call(rbind, rows)
}

#' Outside contributors' pull requests in the vcs_ai_outside_prs shape. Pure.
outside_pr_rows <- function(cls, repo_id, today) {
  o <- cls[cls$role == "outside", , drop = FALSE]
  if (!nrow(o)) return(data.frame(repo_id = character(), pr_number = integer(), tool = character(),
    found_via = character(), created_at = character(), from_fork = integer(),
    author_association = character(), last_confirmed_date = character(), stringsAsFactors = FALSE))
  out <- data.frame(repo_id = repo_id, pr_number = o$number, tool = o$tool,
                    found_via = ifelse(o$code == "PR", "pr-author", o$rule_key), created_at = o$created_at,
                    from_fork = o$from_fork, author_association = o$association,
                    last_confirmed_date = today, stringsAsFactors = FALSE)
  out[!duplicated(paste(out$pr_number, out$tool)), , drop = FALSE]
}

#' The earliest pull request that names a tool for the package, or NA. Exact.
earliest_agent_pr_date <- function(pr, cutoff = AI_PR_CUTOFF) {
  cls <- classify_prs(if (is.null(pr)) NULL else pr$prs, cutoff)
  own <- cls$created_at[cls$role == "authoring" & !(cls$rule_key %in% ai_non_naming_pr_keys())]
  if (!length(own)) return(NA_character_)
  min(own)
}

.ai_split_tiers <- function(s) {
  if (is.na(s) || !nzchar(s)) return(character(0))
  trimws(strsplit(s, ",", fixed = TRUE)[[1]])
}

#' Named by a file, a pull request by the tool or one it wrote for a maintainer, or commits by or crediting
#' it. Never by a codex/ branch or an ignore line alone: RStudio 2026.04 and later add .claude to .Rbuildignore.
meets_naming_threshold <- function(ai_rows) {
  if (is.null(ai_rows) || nrow(ai_rows) == 0) return(FALSE)
  col <- function(n, default) if (n %in% names(ai_rows)) ai_rows[[n]] else rep(default, nrow(ai_rows))
  marks <- col("markers", NA_character_)
  floor_only <- col("first_seen_censored", 0L)
  credited <- col("assisted_commits", NA_integer_)
  any(vapply(which(!as.logical(ai_rows$agnostic)), function(i) {
    tiers <- .ai_split_tiers(ai_rows$evidence_tiers[i])
    if (any(c("A", "PR") %in% tiers)) return(TRUE)
    m <- .ai_split_tiers(marks[i])
    if (any(!ai_is_ignore_marker(m) & !(m %in% c("A", "B", "C", "PR", ai_non_naming_pr_keys())))) return(TRUE)
    # A commit search hit that failed its check keeps only a floor date and no
    # count; it names the repo only beside another finding, as before.
    checked <- isTRUE(floor_only[i] == 0L) || !is.na(credited[i]) || "D" %in% tiers
    any(c("B", "C") %in% tiers) && checked
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
#' Max of a count column, keeping "not searched" apart from "searched, found 0".
#' NULL or an absent column means the shard predates the counts.
.ai_max_count <- function(x) {
  if (is.null(x)) return(NA_integer_)
  x <- suppressWarnings(as.integer(x))
  if (!length(x) || all(is.na(x))) return(NA_integer_)
  max(x, na.rm = TRUE)
}

# The latest of a date column, NA when no row carries one.
.ai_latest_chr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) max(x) else NA_character_
}

# The earliest of a date column, NA when no row carries one.
.ai_earliest_chr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) min(x) else NA_character_
}

# A count column reduced by date: the value measured most recently wins, ties take the
# larger, and rows written before counts were dated fall back to the largest. Pure.
.ai_latest_count <- function(counts, dates) {
  if (is.null(counts)) return(NA_integer_)
  counts <- suppressWarnings(as.integer(counts))
  dates <- if (is.null(dates) || length(dates) != length(counts)) rep(NA_character_, length(counts))
           else as.character(dates)
  dated <- !is.na(dates) & !is.na(counts)
  if (any(dated)) return(max(counts[dated & dates == max(dates[dated])]))
  .ai_max_count(counts)
}

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
  # Every marker that fired for this repository and tool, not just the one that won
  # the onset: a maintainer who commits CLAUDE.md AND gitignores .claude has told you
  # something neither marker says alone.
  marks <- sort(unique(unlist(lapply(g$markers, .ai_split_tiers))))
  lc <- g$last_confirmed_date[!is.na(g$last_confirmed_date)]
  data.frame(repo_id = g$repo_id[1], tool = g$tool[1], first_seen_date = fs,
             first_seen_censored = fc,
             evidence_tiers = if (length(tiers)) paste(tiers, collapse = ",") else NA_character_,
             markers = if (length(marks)) paste(marks, collapse = ",") else NA_character_,
             # Derived from the count where there is one, so the two cannot drift
             # apart. Where there is none the stored flag stands: every row written
             # before the counts existed has authored = 1 and no number, and
             # deriving strictly would silently reset all of them to 0.
             authored = {
               n <- .ai_latest_count(g$authored_commits, g$authored_measured_on)
               if (!is.na(n) && n > 0L) 1L
               # Tier A IS the authorship channel: it is recorded when an author
               # search matched this identity in this repository. Evidence tiers
               # are a union that never expires, so a later search returning zero
               # does not retract it, and letting the zero win published rows
               # reading "A,D" with authored = 0, which contradict themselves.
               else if ("A" %in% tiers) 1L
               else if (!is.na(n)) 0L
               # No count anywhere: the stored flag stands, because every row
               # written before the counts existed carries it and nothing else.
               else as.integer(any(as.integer(g$authored) == 1L, na.rm = TRUE))
             },
             # The latest measurement wins so a wrong high value can be corrected.
             authored_commits = .ai_latest_count(g$authored_commits, g$authored_measured_on),
             assisted_commits = .ai_latest_count(g$assisted_commits, g$assisted_measured_on),
             last_confirmed_date = if (length(lc)) max(lc) else NA_character_,
             authored_measured_on = .ai_latest_chr(g$authored_measured_on),
             assisted_measured_on = .ai_latest_chr(g$assisted_measured_on),
             stringsAsFactors = FALSE)
}

#' Every (tier, tool) that has a detection rule, and could therefore detect.
#'
#' Each search keeps its rules in a differently shaped table, so this is the one
#' place that lists every rule as one row per search and tool.
ai_rule_inventory <- function() {
  tool_of <- function(xs) vapply(xs, function(x) x$tool, character(1))
  inv <- rbind(
    data.frame(tier = "A", tool = vapply(AI_ACCOUNTS, `[[`, "", "tool"), stringsAsFactors = FALSE),
    data.frame(tier = "B", tool = c(unname(Filter(function(t) t != "any", tool_of(AI_TRAILER_PATTERNS))),
                                    unname(AI_ASSISTED_BY_TOOLS)), stringsAsFactors = FALSE),
    data.frame(tier = "C", tool = unname(tool_of(AI_AUTHOR_SUFFIXES)), stringsAsFactors = FALSE),
    # ai_deliberate_markers(), not AI_MARKERS: the classifier drops ambient
    # markers, so .positai and .idx can never produce a tier-D detection. Listing
    # them as rules made the canary report two channels as silent, and they were
    # then recorded as measured zeros with a reason that was not true. The one
    # table built to tell a measured zero from an unasked question must not
    # invent channels nobody scans.
    data.frame(tier = "D", tool = unname(tool_of(ai_deliberate_markers())),
               stringsAsFactors = FALSE),
    # Pull requests opened by a tool's account. Without these rows a login that
    # never matches sits at zero where the silent-search check cannot see it.
    data.frame(tier = "PR", tool = unname(AI_PR_AGENT_LOGINS), stringsAsFactors = FALSE),
    data.frame(tier = "PB", tool = vapply(Filter(function(r) isTRUE(r$names), AI_PR_RULES), `[[`, "", "tool"),
               stringsAsFactors = FALSE))
  inv <- unique(inv)
  inv[order(inv$tier, inv$tool), , drop = FALSE]
}

#' Channels that detected nothing across the entire roster.
#'
#' Five detection bugs shipped a confident zero and survived a green suite,
#' because a zero from a broken query is indistinguishable from a zero from an
#' unused tool. Nothing looked at the published counts.
#'
#' The granularity is the whole point and it is measured: tier A totals 103
#' detections, so a tier-level check sees a healthy channel, while per tool it
#' reads claude 50, copilot 53, and cursor/devin/jules/openhands 0. Four of six
#' identities have never produced a detection. So this is per (tier, tool),
#' never per tier.
#'
#' A genuine zero is possible. It goes in AI_SILENT_CHANNELS_KNOWN with a reason
#' and a date, which is a claim someone made and can be re-examined, rather than
#' a blank that reads as absence.
ai_silent_channels <- function(signals, known = AI_SILENT_CHANNELS_KNOWN, outside = NULL) {
  inv <- ai_rule_inventory()
  seen <- character(0)
  if (!is.null(signals) && nrow(signals) > 0) {
    seen <- unique(unlist(lapply(seq_len(nrow(signals)), function(i) {
      tiers <- .ai_split_tiers(signals$evidence_tiers[i])
      if (!length(tiers)) return(character(0))
      paste(tiers, signals$tool[i], sep = "\t")
    }), use.names = FALSE))
  }
  # An outside contributor's pull request still shows the search works.
  if (!is.null(outside) && nrow(outside) > 0)
    seen <- c(seen, paste(ifelse(outside$found_via == "pr-author", "PR", "PB"), outside$tool, sep = "\t"))
  silent <- inv[!(paste(inv$tier, inv$tool, sep = "\t") %in% seen), , drop = FALSE]
  if (!is.null(known) && nrow(known) > 0) {
    silent <- silent[!(paste(silent$tier, silent$tool, sep = "\t") %in%
                       paste(known$tier, known$tool, sep = "\t")), , drop = FALSE]
  }
  rownames(silent) <- NULL
  silent
}

#' Report the canary at merge time, and stop when a channel is silent with
#' nothing said about it.
#'
#' Silence that someone has examined is recorded in AI_SILENT_CHANNELS_KNOWN and
#' printed every run: an "open" entry is a question with a date on it, and
#' printing it is what keeps it from rotting into an assumed absence. Silence
#' nobody has examined stops the merge, because a zero published as fact is the
#' failure this exists to prevent.
#'
#' Every count below is measured against the detections in front of it. The
#' first version was not: it reported the silent total as nrow(known) plus the
#' unexplained rows, so the recorded half was the length of a hand-written file
#' and nothing ever re-checked whether those claims were still true. It printed
#' "16 recorded, 0 unexplained" every week for a month, reading like a stable
#' healthy invariant, while six of the sixteen recorded zeros had already been
#' answered by the data underneath it: cursor, gemini, jules and openhands were
#' detecting, five of them for weeks. The merge that recorded devin's first
#' tier-B detection printed "rule added 2026-08-01, unscanned" about devin in
#' the same output, because the open questions were read off the file rather
#' than off the run. That is the rot this whole table exists to prevent,
#' happening inside the table.
ai_canary_check <- function(signals, known = AI_SILENT_CHANNELS_KNOWN,
                            roster_n = NA_integer_,
                            min_roster = AI_CANARY_MIN_ROSTER, outside = NULL) {
  # "Nothing anywhere on the roster" is only evidence when there is a roster. On
  # a handful of repos a zero means nothing, so the check would be noise. The
  # gate reads the roster, never the detection count: a scan that collapsed to
  # zero detections must still be caught, and gating on detections would make
  # the worst case the one that skips silently.
  if (!is.na(roster_n) && roster_n < min_roster) {
    message(sprintf("AI detection canary: skipped, roster of %d is below %d",
                    roster_n, min_roster))
    return(invisible(ai_silent_channels(signals, known, outside = outside)[0, , drop = FALSE]))
  }
  unexplained <- ai_silent_channels(signals, known, outside = outside)
  inv <- ai_rule_inventory()
  # Every channel still at zero, recorded or not, read from this run's onsets.
  measured <- ai_silent_channels(signals, NULL, outside = outside)
  recorded_key <- paste(known$tier, known$tool, sep = "\t")
  still <- recorded_key %in% paste(measured$tier, measured$tool, sep = "\t")
  # A channel absent from the silent set is not necessarily detecting. The
  # silent set is derived from ai_rule_inventory(), so a recorded claim whose
  # rule has left the inventory is absent from it for the opposite reason:
  # nothing scans for that channel at all. This project retires rules on
  # purpose, .positai and .idx among them, so an entry outliving its rule is a
  # live shape rather than a hypothetical, and the first version of this report
  # printed one as "detecting". Retiring the entry is the right advice either
  # way, and the sentence under it is the one line this project reads as
  # evidence about a zero, so it has to be the true one.
  has_rule <- recorded_key %in% paste(inv$tier, inv$tool, sep = "\t")
  message(sprintf(paste0("AI detection canary: %d channels with a rule, %d silent ",
                         "(%d recorded, %d unexplained), %d detecting"),
                  nrow(inv), nrow(measured), sum(still), nrow(unexplained),
                  nrow(inv) - nrow(measured)))
  answered <- known[has_rule & !still, , drop = FALSE]
  if (nrow(answered) > 0) {
    message("  recorded zeros the data has answered, retire them from AI_SILENT_CHANNELS_KNOWN:")
    for (i in seq_len(nrow(answered)))
      message(sprintf("    %s/%-9s detecting; the claim recorded %s was: %s",
                      answered$tier[i], answered$tool[i], answered$recorded_on[i],
                      answered$reason[i]))
  }
  orphaned <- known[!has_rule, , drop = FALSE]
  if (nrow(orphaned) > 0) {
    message("  recorded zeros with no rule left, retire them from AI_SILENT_CHANNELS_KNOWN:")
    for (i in seq_len(nrow(orphaned)))
      message(sprintf(paste0("    %s/%-9s no rule in the inventory scans for it, so its zero ",
                             "is not a measurement; the claim recorded %s was: %s"),
                      orphaned$tier[i], orphaned$tool[i], orphaned$recorded_on[i],
                      orphaned$reason[i]))
  }
  # Only the questions the data has not settled. Re-printing a claim about a
  # zero that is gone is not visibility, it is the file talking to itself.
  open <- known[still & known$status == "open", , drop = FALSE]
  if (nrow(open) > 0) {
    message("  open questions, re-reported so they stay visible:")
    for (i in seq_len(nrow(open)))
      message(sprintf("    %s/%-9s %s (since %s)", open$tier[i], open$tool[i],
                      open$reason[i], open$recorded_on[i]))
  }
  if (nrow(unexplained) > 0) {
    message(sprintf("  UNEXPLAINED, and the run will fail once the data is out: %s",
                    paste(unexplained$tier, unexplained$tool, sep = "/", collapse = ", ")))
  }
  invisible(unexplained)
}

#' The canary's finding, shaped for publication.
#'
#' Every silent channel, whether recorded or not, so a consumer can say which
#' zeros were measured and which were never asked. A page that shows "0 repos"
#' beside a channel nobody could reach is asserting something we do not know.
ai_silent_channel_table <- function(signals, known = AI_SILENT_CHANNELS_KNOWN, outside = NULL) {
  unexplained <- ai_silent_channels(signals, known, outside = outside)
  rows <- NULL
  if (nrow(known) > 0) {
    rows <- rbind(rows, data.frame(tier = known$tier, tool = known$tool,
      status = known$status, reason = known$reason,
      recorded_on = known$recorded_on, stringsAsFactors = FALSE))
  }
  if (nrow(unexplained) > 0) {
    rows <- rbind(rows, data.frame(tier = unexplained$tier, tool = unexplained$tool,
      status = "unexplained", reason = NA_character_,
      recorded_on = NA_character_, stringsAsFactors = FALSE))
  }
  if (is.null(rows)) rows <- data.frame(tier = character(), tool = character(),
    status = character(), reason = character(), recorded_on = character(),
    stringsAsFactors = FALSE)
  # A recorded channel that started detecting is no longer silent, so it is not
  # in the published table at all: the allowlist is a claim about a zero, and
  # the zero is gone.
  live <- paste(rows$tier, rows$tool, sep = "\t") %in%
          paste(ai_silent_channels(signals, NULL, outside = outside)$tier,
                ai_silent_channels(signals, NULL, outside = outside)$tool, sep = "\t")
  rows[live, , drop = FALSE]
}

# ---- model detail ----------------------------------------------------------
#
# The trailer already names the model and we read past it. Three tools state
# one, in three genuinely different grammars, and the rest state nothing beyond
# identity. A blank for those means their trailers carry no model, not that no
# model was used.
#
# Parsed structurally, never against a list of known models: an enumerated list
# silently drops the next model to ship. Fable was not on anyone's list until it
# shipped, and an unrecognised family is stored verbatim for exactly that reason.

.ai_no_model <- function()
  list(provider = NA_character_, family = NA_character_,
       version = NA_character_, context_window = NA_character_)

#' The model a single commit message states, for the tool that wrote the trailer.
#'
#' Returns the four fields with NA where the trailer was silent. Absent family,
#' version and context are three separate silences, none of them a value: a
#' trailer saying only "Claude" is not Opus, not old, and not standard.
extract_ai_model <- function(tool, message) {
  out <- .ai_no_model()
  if (is.null(message) || length(message) != 1 || is.na(message)) return(out)

  if (identical(tool, "claude")) {
    # Claude <family> <version> [(<ctx> context)] -- family is any capitalised
    # word, so a family we have never seen still lands in the column.
    m <- regmatches(message,
      regexpr("Claude\\s+([A-Z][A-Za-z]*)\\s+([0-9]+(?:\\.[0-9]+)?)", message, perl = TRUE))
    if (length(m) && nzchar(m)) {
      g <- regmatches(m, regexec("Claude\\s+([A-Z][A-Za-z]*)\\s+([0-9]+(?:\\.[0-9]+)?)", m))[[1]]
      out$family <- g[2]; out$version <- g[3]
    }
    ctx <- regmatches(message, regexec("\\(([0-9]+[KM])\\s+context\\)", message))[[1]]
    if (length(ctx) == 2) out$context_window <- ctx[2]
    return(out)
  }

  if (identical(tool, "aider")) {
    # aider (<provider>/<model>) -- split on the FIRST slash only. The model id
    # belongs to the provider and splitting it further would invent structure we
    # do not control.
    g <- regmatches(message, regexec("aider\\s*\\(([^)]+)\\)", message))[[1]]
    if (length(g) == 2 && grepl("/", g[2], fixed = TRUE)) {
      out$provider <- sub("/.*$", "", g[2])
      out$family   <- sub("^[^/]*/", "", g[2])
    }
    return(out)
  }

  if (identical(tool, "gemini")) {
    # Gemini <version> <variant>
    g <- regmatches(message,
      regexec("Gemini\\s+([0-9]+(?:\\.[0-9]+)?)\\s+([A-Za-z][A-Za-z0-9-]*)", message))[[1]]
    if (length(g) == 3) { out$version <- g[2]; out$family <- g[3] }
    return(out)
  }

  out
}

.ai_empty_models <- function()
  data.frame(repo_id = character(), tool = character(), provider = character(),
             family = character(), version = character(), context_window = character(),
             commits = integer(), first_seen = character(), last_seen = character(),
             window_complete = integer(), stringsAsFactors = FALSE)

#' Tally the models a repository's trailers name, one row per distinct model.
#'
#' `commits` counts commits in the examined window, not in history, and
#' `window_complete` is 0 when the search reported more hits than the page
#' returned, so a reader can tell a full tally from the first hundred of many.
build_ai_model_rows <- function(repo_id, tool, items, window_complete = TRUE) {
  if (is.null(items) || nrow(items) == 0) return(.ai_empty_models())
  parsed <- lapply(seq_len(nrow(items)), function(i) {
    m <- extract_ai_model(tool, items$message[i])
    # A trailer that states no model produces no row. A blank row would read as
    # a model we could not identify, which is a different claim.
    if (all(is.na(unlist(m)))) return(NULL)
    data.frame(provider = m$provider, family = m$family, version = m$version,
               context_window = m$context_window, date = items$date[i],
               stringsAsFactors = FALSE)
  })
  parsed <- do.call(rbind, Filter(Negate(is.null), parsed))
  if (is.null(parsed) || nrow(parsed) == 0) return(.ai_empty_models())
  key <- paste(parsed$provider, parsed$family, parsed$version,
               parsed$context_window, sep = "\r")
  rows <- lapply(split(parsed, key), function(g) {
    d <- g$date[!is.na(g$date)]
    data.frame(repo_id = repo_id, tool = tool,
               provider = g$provider[1], family = g$family[1],
               version = g$version[1], context_window = g$context_window[1],
               commits = nrow(g),
               first_seen = if (length(d)) min(d) else NA_character_,
               last_seen  = if (length(d)) max(d) else NA_character_,
               window_complete = as.integer(isTRUE(window_complete)),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.ai_empty_signals <- function()
  data.frame(repo_id = character(), tool = character(), first_seen_date = character(),
             first_seen_censored = integer(), evidence_tiers = character(),
             markers = character(),
             authored = integer(),
             authored_commits = integer(), assisted_commits = integer(),
             last_confirmed_date = character(),
             authored_measured_on = character(), assisted_measured_on = character(),
             stringsAsFactors = FALSE)

#' Any signals frame on the full column set, a missing column as NA of its type.
#' Shards written by older code carry fewer columns and must still fold. Pure.
.ai_align_signals <- function(df) {
  proto <- .ai_empty_signals()
  if (is.null(df)) return(proto)
  for (cn in setdiff(names(proto), names(df)))
    df[[cn]] <- if (nrow(df)) rep(proto[[cn]][NA_integer_], nrow(df)) else proto[[cn]]
  df[, names(proto), drop = FALSE]
}

#' Merge prior + incoming vcs_ai_signals rows per (repo_id, tool) by the six
#' column rules. Read-modify-write: callers write the returned set wholesale.
ai_onset_reducer <- function(prior_rows, incoming_rows) {
  all_rows <- rbind(.ai_align_signals(prior_rows), .ai_align_signals(incoming_rows))
  if (nrow(all_rows) == 0) return(.ai_empty_signals())
  key <- paste(all_rows$repo_id, all_rows$tool, sep = "\r")
  parts <- lapply(split(all_rows, key), .ai_reduce_group)
  do.call(rbind, parts)
}

.ai_empty_log <- function()
  data.frame(repo_id = character(), rule_key = character(), rule_rev = integer(),
             ruleset_version = character(), asked_on = character(), outcome = character(),
             total_count = integer(), verified = integer(), incomplete = integer(),
             first_hit_on = character(), source = character(), stringsAsFactors = FALSE)
.ai_empty_reads <- function()
  data.frame(repo_id = character(), commits_read_on = character(), commits_read_through = character(),
             commits_ruleset = character(), commits_read = integer(), commits_window_complete = integer(),
             commits_history_complete = integer(), prs_read_on = character(),
             prs_newest_created_at = character(), prs_walk_complete = integer(),
             prs_walk_started_on = character(), prs_walk_cursor = character(),
             accounts_counted_on = character(), last_failed_on = character(), last_failure = character(),
             stringsAsFactors = FALSE)
.ai_empty_counts <- function()
  data.frame(repo_id = character(), tool = character(), identity_set = character(), commits = integer(),
             newest_commit_date = character(), measured_on = character(), stringsAsFactors = FALSE)
.ai_empty_outside <- function()
  data.frame(repo_id = character(), pr_number = integer(), tool = character(), found_via = character(),
             created_at = character(), from_fork = integer(), author_association = character(),
             last_confirmed_date = character(), stringsAsFactors = FALSE)

#' Stack frames on the column set of `empty`: a missing column is NA of its type, an
#' extra one is dropped. Partials written by older code carry fewer columns. Pure.
.ai_bind_like <- function(empty, frames) {
  frames <- Filter(function(f) !is.null(f) && nrow(f) > 0, frames)
  if (!length(frames)) return(empty)
  do.call(rbind, lapply(frames, function(f) {
    for (cn in setdiff(names(empty), names(f))) f[[cn]] <- rep(empty[[cn]][NA_integer_], nrow(f))
    f[, names(empty), drop = FALSE]
  }))
}

# Compare two ISO dates, NA before any date: -1 earlier, 0 the same, 1 later.
.ai_date_cmp <- function(a, b)
  if (is.na(a) || is.na(b)) (!is.na(a)) - (!is.na(b)) else (a > b) - (a < b)

# The same for a commit read against the stored one: its watermark first, then the day it was read.
.ai_commit_read_cmp <- function(through, on, stored_through, stored_on) {
  by <- .ai_date_cmp(through, stored_through)
  if (by == 0L) .ai_date_cmp(on, stored_on) else by
}

#' Fold this run's read state over the stored state, one column group at a time: a group moves
#' only when this run read it and is not behind, the commit group judged by its watermark first. Pure.
fold_repo_reads <- function(prior, incoming) {
  empty <- .ai_empty_reads()
  prior <- .ai_bind_like(empty, list(prior)); incoming <- .ai_bind_like(empty, list(incoming))
  groups <- list(
    commits_read_on = c("commits_read_on", "commits_read_through", "commits_ruleset", "commits_read",
                        "commits_window_complete", "commits_history_complete"),
    prs_read_on = c("prs_read_on", "prs_newest_created_at", "prs_walk_complete", "prs_walk_started_on",
                    "prs_walk_cursor"),
    accounts_counted_on = "accounts_counted_on",
    last_failed_on = c("last_failed_on", "last_failure"))
  for (i in seq_len(nrow(incoming))) {
    k <- match(incoming$repo_id[i], prior$repo_id)
    if (is.na(k)) { prior <- rbind(prior, incoming[i, , drop = FALSE]); next }
    for (g in names(groups)) {
      if (is.na(incoming[[g]][i])) next
      # The commit group goes by its watermark first: moved back, it would let a later read count a match twice.
      by <- if (g == "commits_read_on")
              .ai_commit_read_cmp(incoming$commits_read_through[i], incoming[[g]][i],
                                  prior$commits_read_through[k], prior[[g]][k])
            else .ai_date_cmp(incoming[[g]][i], prior[[g]][k])
      if (by >= 0L) prior[k, groups[[g]]] <- incoming[i, groups[[g]]]
    }
  }
  prior
}

#' The repositories whose read to the first commit this run is the read the fold kept (`reads`, after the fold).
#' A read behind it rebuilds nothing, since the log already counts the commits past its watermark. Pure.
rebuilt_log_repos <- function(reads, incoming) {
  inc <- .ai_bind_like(cbind(.ai_empty_reads(), reached_first = integer()), list(incoming))
  inc <- inc[inc$reached_first %in% 1L & !is.na(inc$commits_read_on), , drop = FALSE]
  rd <- .ai_bind_like(.ai_empty_reads(), list(reads))
  k <- match(inc$repo_id, rd$repo_id)
  kept <- vapply(seq_len(nrow(inc)), function(i) is.na(k[i]) ||
    .ai_commit_read_cmp(inc$commits_read_through[i], inc$commits_read_on[i],
                        rd$commits_read_through[k[i]], rd$commits_read_on[k[i]]) >= 0L, logical(1))
  unique(inc$repo_id[kept])
}

#' Keep the newest count per repository, tool and address set. A repository counted on day d (counted_repos,
#' repo_id to d) loses a GraphQL count dated d or earlier for a tool with none now; REST counts stand. Pure.
fold_account_counts <- function(prior, incoming, counted_repos) {
  if (is.null(counted_repos) || (length(counted_repos) && is.null(names(counted_repos))))
    stop("fold_account_counts: counted_repos must name each repository with the day it was counted")
  empty <- .ai_empty_counts()
  inc <- .ai_bind_like(empty, list(incoming))
  pri <- .ai_bind_like(empty, list(prior))
  inc_graphql <- paste(inc$repo_id, inc$tool)[inc$identity_set %in% "graphql"]
  on <- unname(counted_repos[pri$repo_id])
  older <- !is.na(on) & (is.na(pri$measured_on) | pri$measured_on <= on)
  stale <- pri$identity_set %in% "graphql" & older & !(paste(pri$repo_id, pri$tool) %in% inc_graphql)
  all <- rbind(inc, pri[!stale, , drop = FALSE])
  all <- all[order(all$measured_on, decreasing = TRUE), , drop = FALSE]
  all[!duplicated(paste(all$repo_id, all$tool, all$identity_set, sep = "\r")), , drop = FALSE]
}

#' Fold this run's searches and reads into the log. A weekly add counts only from the stored watermark, and only
#' a read to the first commit in rebuilt_repos replaces the repository's msg., name. and review. rows. Pure.
fold_search_log <- function(prior, incoming, rebuilt_repos = NULL, reads = NULL) {
  empty <- .ai_empty_log()
  pri <- .ai_bind_like(empty, list(prior))
  local_key <- grepl("^(msg|name|review)\\.", pri$rule_key)
  pri <- pri[!(pri$repo_id %in% rebuilt_repos & local_key), , drop = FALSE]
  if (is.null(incoming) || !nrow(incoming)) return(pri)
  col <- function(cn)
    if (cn %in% names(incoming)) as.character(incoming[[cn]]) else rep(NA_character_, nrow(incoming))
  is_add <- col("mode") %in% "add"
  is_replace <- col("mode") %in% "replace"
  after <- col("read_after")[is_add]
  if (any(is_add) && is.null(reads))
    stop("fold_search_log: a weekly read's counts need the stored read state, and reads is NULL")
  if (any(is_replace) && is.null(rebuilt_repos))
    stop("fold_search_log: a read to the first commit needs rebuilt_repos, and it is NULL")
  inc <- .ai_bind_like(empty, list(incoming))
  add <- inc[is_add, , drop = FALSE]
  # A read to the first commit that the read state did not keep saw less than the log already counts.
  all <- rbind(inc[!is_add & !(is_replace & !(inc$repo_id %in% rebuilt_repos)), , drop = FALSE], pri)
  all <- all[order(all$asked_on, decreasing = TRUE), , drop = FALSE]
  all <- all[!duplicated(paste(all$repo_id, all$rule_key, sep = "\r")), , drop = FALSE]
  rd <- .ai_bind_like(.ai_empty_reads(), list(reads))
  through <- rd$commits_read_through[match(add$repo_id, rd$repo_id)]
  same <- (is.na(after) & is.na(through)) | (!is.na(after) & !is.na(through) & after == through)
  for (i in which(add$repo_id %in% rd$repo_id & same)) {
    k <- which(all$repo_id == add$repo_id[i] & all$rule_key == add$rule_key[i])
    if (!length(k)) { all <- rbind(all, add[i, , drop = FALSE]); next }
    if (!identical(all$source[k], "read")) next
    all$total_count[k] <- all$total_count[k] + add$total_count[i]
    all$first_hit_on[k] <- .ai_earliest_chr(c(all$first_hit_on[k], add$first_hit_on[i]))
    all[k, c("asked_on", "rule_rev", "ruleset_version")] <- add[i, c("asked_on", "rule_rev", "ruleset_version")]
  }
  rownames(all) <- NULL
  all
}

#' One review row per repository and tool: codes and values united, the earliest date
#' kept (an exact one wins a tie), the latest confirmation kept. Pure.
fold_review_rows <- function(prior, incoming) {
  all <- .ai_bind_like(.ai_empty_review(), list(prior, incoming))
  if (!nrow(all)) return(.ai_empty_review())
  uni <- function(x) {
    v <- sort(unique(unlist(lapply(x, .ai_split_tiers))))
    if (length(v)) paste(v, collapse = ",") else NA_character_
  }
  do.call(rbind, unname(lapply(split(all, paste(all$repo_id, all$tool, sep = "\r")), function(g) {
    ok <- !is.na(g$first_seen_date)
    first <- if (any(ok)) {
      o <- order(g$first_seen_date[ok], g$first_seen_censored[ok]); g[ok, , drop = FALSE][o[1], ]
    } else g[1, ]
    data.frame(repo_id = g$repo_id[1], tool = g$tool[1],
               first_seen_date = if (any(ok)) first$first_seen_date else NA_character_,
               first_seen_censored = if (any(ok)) as.integer(first$first_seen_censored) else 0L,
               evidence_tiers = uni(g$evidence_tiers), markers = uni(g$markers),
               assisted_commits = .ai_latest_count(g$assisted_commits, g$assisted_measured_on),
               assisted_measured_on = .ai_latest_chr(g$assisted_measured_on),
               last_confirmed_date = .ai_latest_chr(g$last_confirmed_date), stringsAsFactors = FALSE)
  })))
}

#' One row per outside pull request and tool; the newest sighting wins. Pure.
fold_outside_prs <- function(prior, incoming) {
  all <- .ai_bind_like(.ai_empty_outside(), list(incoming, prior))
  all <- all[order(all$last_confirmed_date, decreasing = TRUE), , drop = FALSE]
  all[!duplicated(paste(all$repo_id, all$pr_number, all$tool, sep = "\r")), , drop = FALSE]
}

#' Commit-credit (msg.) rules with their tool, revision and search mode. Pure.
.ai_credit_rules <- function()
  do.call(rbind, lapply(AI_TRAILER_PATTERNS, function(r)
    data.frame(key = r$key, tool = r$tool, rev = as.integer(r$rev), search = r$search,
               stringsAsFactors = FALSE)))

#' authored_commits: the sum of a tool's account counts where its accounts were counted, 0 when counted
#' with none, NA for a tool with no accounts. A repository never counted keeps what it had. Pure.
derive_authored_counts <- function(signals, counts, reads) {
  if (is.null(signals) || !nrow(signals)) return(signals)
  counts <- .ai_bind_like(.ai_empty_counts(), list(counts))
  reads <- .ai_bind_like(.ai_empty_reads(), list(reads))
  acct <- vapply(AI_ACCOUNTS, `[[`, "", "tool")
  key <- paste(counts$repo_id, counts$tool, sep = "\r")
  n <- if (nrow(counts)) tapply(counts$commits, key, sum) else integer(0)
  on <- if (nrow(counts)) tapply(counts$measured_on, key, max) else character(0)
  counted_on <- reads$accounts_counted_on[match(signals$repo_id, reads$repo_id)]
  for (i in seq_len(nrow(signals))) {
    if (!(signals$tool[i] %in% acct)) {
      signals$authored_commits[i] <- NA_integer_; signals$authored_measured_on[i] <- NA_character_; next
    }
    if (is.na(counted_on[i])) next
    k <- paste(signals$repo_id[i], signals$tool[i], sep = "\r")
    signals$authored_commits[i] <- if (k %in% names(n)) as.integer(n[[k]]) else 0L
    signals$authored_measured_on[i] <- if (k %in% names(on)) on[[k]] else counted_on[i]
    codes <- .ai_split_tiers(signals$evidence_tiers[i])
    signals$authored[i] <- if (signals$authored_commits[i] > 0L || "A" %in% codes) 1L else 0L
  }
  signals
}

#' assisted_commits: the largest usable commit-credit count of the tool, a floor. 0 only where a whole-history
#' read matched none of its rules or each always rule answered none at its current revision. Pure.
derive_assisted_counts <- function(signals, log, reads, ruleset = AI_RULESET_VERSION) {
  if (is.null(signals) || !nrow(signals)) return(signals)
  log <- .ai_bind_like(.ai_empty_log(), list(log))
  reads <- .ai_bind_like(.ai_empty_reads(), list(reads))
  rules <- .ai_credit_rules()
  log$tool <- rules$tool[match(log$rule_key, rules$key)]
  ab <- startsWith(log$rule_key, "msg.any.assisted-by.")
  log$tool[ab] <- sub("^msg\\.any\\.assisted-by\\.", "", log$rule_key[ab])
  usable <- log$outcome == "hit" & log$verified %in% 1L & log$source %in% c("search", "read") &
            !(log$rule_key == "msg.any.assisted-by" & log$source == "search") &
            !(log$rule_key == "msg.copilot.vscode" & log$source == "search" &
              !is.na(log$first_hit_on) & log$first_hit_on <= AI_COPILOT_VSCODE_FALSE_WINDOW[2])
  use <- log[usable & !is.na(log$tool), , drop = FALSE]
  whole <- reads$repo_id[reads$commits_history_complete %in% 1L & reads$commits_ruleset %in% ruleset]
  credit_tools <- unique(c(rules$tool[rules$tool != "any"], unname(AI_ASSISTED_BY_TOOLS)))
  for (i in seq_len(nrow(signals))) {
    rid <- signals$repo_id[i]; tl <- signals$tool[i]
    mine <- use[use$repo_id == rid & use$tool == tl, , drop = FALSE]
    if (nrow(mine)) {
      signals$assisted_commits[i] <- as.integer(max(mine$total_count))
      signals$assisted_measured_on[i] <- max(mine$asked_on)
      next
    }
    if (!(tl %in% credit_tools)) next
    read_rows <- log$repo_id == rid & log$source == "read" & log$tool %in% tl
    if (rid %in% whole && !any(read_rows)) {
      signals$assisted_commits[i] <- 0L
      signals$assisted_measured_on[i] <- reads$commits_read_on[match(rid, reads$repo_id)]
      next
    }
    always <- rules[rules$tool == tl & rules$search == "always", , drop = FALSE]
    if (!nrow(always)) next
    asked <- log[log$repo_id == rid & log$rule_key %in% always$key & log$source == "search", , drop = FALSE]
    at_rev <- asked$rule_rev == always$rev[match(asked$rule_key, always$key)]
    if (nrow(asked) == nrow(always) && all(at_rev) && all(asked$outcome == "none")) {
      signals$assisted_commits[i] <- 0L
      signals$assisted_measured_on[i] <- max(asked$asked_on)
    }
  }
  signals
}

#' A review tool's accepted-suggestion count and first date from its rule's latest
#' checked hit. Pure.
derive_review_counts <- function(review, log) {
  if (is.null(review) || !nrow(review)) return(review)
  log <- .ai_bind_like(.ai_empty_log(), list(log))
  for (i in seq_len(nrow(review))) {
    keys <- vapply(Filter(function(r) identical(r$tool, review$tool[i]), AI_REVIEW_RULES), `[[`, "", "key")
    h <- log[log$repo_id == review$repo_id[i] & log$rule_key %in% keys & log$outcome == "hit" &
             log$verified %in% 1L, , drop = FALSE]
    if (!nrow(h)) next
    review$assisted_commits[i] <- as.integer(max(h$total_count))
    review$assisted_measured_on[i] <- max(h$asked_on)
    first <- .ai_earliest_chr(h$first_hit_on)
    if (!is.na(first) && (is.na(review$first_seen_date[i]) || first < review$first_seen_date[i])) {
      review$first_seen_date[i] <- first; review$first_seen_censored[i] <- 0L
    }
  }
  review
}

#' Every search the coverage table reports on, one per rule, author searches per tool. Pure.
.ai_coverage_rules <- function() {
  row <- function(key, tool, channel, rev) data.frame(rule_key = key, tool = tool, channel = channel,
                                                      rule_rev = as.integer(rev), stringsAsFactors = FALSE)
  do.call(rbind, c(
    lapply(AI_TRAILER_PATTERNS, function(r) row(r$key, r$tool, "commit-credit", r$rev)),
    lapply(AI_AUTHOR_SUFFIXES, function(r) row(r$key, r$tool, "commit-author-name", r$rev)),
    lapply(AI_REVIEW_RULES, function(r) row(r$key, r$tool, "review-credit", r$rev)),
    lapply(AI_ACCOUNTS, function(a) row(paste0("author.", a$tool), a$tool, "commit-author", 1L))))
}

#' How far each search has reached, one row per rule and zeros for one never asked. A repository
#' whose whole history was read under this ruleset counts apart from those asked, dated by its read. Pure.
build_search_coverage <- function(log, reads, ruleset = AI_RULESET_VERSION) {
  rules <- .ai_coverage_rules()
  log <- .ai_bind_like(.ai_empty_log(), list(log))
  reads <- .ai_bind_like(.ai_empty_reads(), list(reads))
  wr <- reads[reads$commits_history_complete %in% 1L & reads$commits_ruleset %in% ruleset, , drop = FALSE]
  whole <- unique(wr$repo_id)
  whole_on <- .ai_latest_chr(wr$commits_read_on)
  by_addr <- do.call(rbind, lapply(AI_ACCOUNTS, function(a) data.frame(
    key = paste0("author.", c(a$graphql, a$linked)), tool_key = paste0("author.", a$tool),
    stringsAsFactors = FALSE)))
  hit <- match(log$rule_key, by_addr$key)
  log$rolled <- ifelse(is.na(hit), log$rule_key, by_addr$tool_key[hit])
  do.call(rbind, lapply(seq_len(nrow(rules)), function(i) {
    r <- rules[i, ]
    g <- log[log$rolled %in% r$rule_key, , drop = FALSE]
    asked <- g[g$source %in% "search" & g$rule_rev %in% r$rule_rev & g$outcome %in% c("hit", "none") &
               !(g$repo_id %in% whole), , drop = FALSE]
    refused <- g[g$outcome %in% "refused", , drop = FALSE]
    # The read checks every commit's author address and review credit too, so it answers every channel.
    data.frame(rule_key = r$rule_key, tool = r$tool, channel = r$channel, rule_rev = r$rule_rev,
               repos_asked = length(unique(asked$repo_id)),
               repos_hit = length(unique(asked$repo_id[asked$outcome == "hit"])),
               repos_refused = length(unique(refused$repo_id)),
               repos_read_whole = length(whole),
               last_asked_on = .ai_latest_chr(c(asked$asked_on, refused$asked_on, whole_on)),
               stringsAsFactors = FALSE)
  }))
}

# The stored values that name a file or an ignore line, not an account, a credit or a pull request.
.ai_file_values <- function(m)
  m[!(m %in% c("A", "B", "C", "PR")) & !grepl("^(msg|name|pr|account|review)\\.", m)]

#' Moves published rows the current rules place in the review table, or drop, out of the authoring table.
#' Idempotent, so it runs on every merge. A row that loses a code loses its date: no per-code date was kept.
ai_reclassify_rows <- function(prior, cheap, log, reads, outside) {
  prior <- .ai_align_signals(prior)
  log <- .ai_bind_like(.ai_empty_log(), list(log))
  reads <- .ai_bind_like(.ai_empty_reads(), list(reads))
  outside <- .ai_bind_like(.ai_empty_outside(), list(outside))
  moved <- c(coderabbit = 0L, gemini_review_folder = 0L, gemini_review_credit = 0L, pr_superseded = 0L)
  review <- list()
  keep <- rep(TRUE, nrow(prior)); lost <- rep(FALSE, nrow(prior))
  to_review <- function(i, tool, codes, values) review[[length(review) + 1L]] <<- data.frame(
    repo_id = prior$repo_id[i], tool = tool, first_seen_date = prior$first_seen_date[i],
    first_seen_censored = as.integer(prior$first_seen_censored[i]), evidence_tiers = codes, markers = values,
    assisted_commits = NA_integer_, assisted_measured_on = NA_character_,
    last_confirmed_date = prior$last_confirmed_date[i], stringsAsFactors = FALSE)
  set_row <- function(i, codes, values) {
    had <- .ai_split_tiers(prior$evidence_tiers[i])
    prior$evidence_tiers[i] <<- if (length(codes)) paste(sort(codes), collapse = ",") else NA_character_
    prior$markers[i] <<- if (length(values)) paste(sort(values), collapse = ",") else NA_character_
    if (!length(codes)) keep[i] <<- FALSE else if (length(codes) < length(had)) lost[i] <<- TRUE
  }
  for (i in which(prior$tool == "coderabbit")) {
    to_review(i, "coderabbit", prior$evidence_tiers[i], prior$markers[i])
    keep[i] <- FALSE; moved[["coderabbit"]] <- moved[["coderabbit"]] + 1L
  }
  review_folder <- if (is.null(cheap) || !nrow(cheap)) character(0) else unique(cheap$repo_id[
    cheap$role %in% "review" & cheap$tool == "gemini-code-assist" & cheap$marker == ".gemini"])
  gem_revs <- vapply(c("msg.gemini.coauthor", "msg.gemini.bot"), function(k)
    Find(function(r) r$key == k, AI_TRAILER_PATTERNS)$rev, integer(1))
  whole <- reads$repo_id[reads$commits_history_complete %in% 1L &
                         reads$commits_ruleset %in% AI_RULESET_VERSION]
  log_key <- paste(log$repo_id, log$rule_key, sep = "\r")
  gem_none <- function(repo) all(vapply(names(gem_revs), function(k) {
    j <- match(paste(repo, k, sep = "\r"), log_key)
    if (is.na(j)) repo %in% whole else log$rule_rev[j] %in% gem_revs[[k]] && log$outcome[j] %in% "none"
  }, logical(1)))
  for (i in which(keep & prior$tool == "gemini")) {
    codes <- .ai_split_tiers(prior$evidence_tiers[i]); m <- .ai_split_tiers(prior$markers[i])
    if (prior$repo_id[i] %in% review_folder && ".gemini" %in% m) {
      m <- setdiff(m, ".gemini")
      if (!length(.ai_file_values(m))) codes <- setdiff(codes, "D")
      to_review(i, "gemini-code-assist", "D", ".gemini")
      set_row(i, codes, m); moved[["gemini_review_folder"]] <- moved[["gemini_review_folder"]] + 1L
    }
    # Both Gemini searches answering none, or a read to the first commit, undo only an unkeyed credit.
    unkeyed <- "B" %in% m || !any(startsWith(m, "msg."))
    if (!keep[i] || !("B" %in% codes) || !unkeyed || !gem_none(prior$repo_id[i])) next
    m <- setdiff(m, "B")
    set_row(i, if (any(startsWith(m, "msg."))) codes else setdiff(codes, "B"), m)
    moved[["gemini_review_credit"]] <- moved[["gemini_review_credit"]] + 1L
  }
  for (i in which(keep & prior$evidence_tiers %in% "PR")) {
    rd <- match(prior$repo_id[i], reads$repo_id)
    if (is.na(rd) || !isTRUE(reads$prs_walk_complete[rd] == 1L) || is.na(reads$prs_walk_started_on[rd])) next
    confirmed <- prior$last_confirmed_date[i]
    if (is.na(confirmed) || confirmed >= reads$prs_walk_started_on[rd]) next
    if (!any(outside$repo_id == prior$repo_id[i] & outside$tool == prior$tool[i] &
             grepl("pr-author", outside$found_via, fixed = TRUE))) next
    keep[i] <- FALSE; moved[["pr_superseded"]] <- moved[["pr_superseded"]] + 1L
  }
  prior$first_seen_date[lost & keep] <- NA_character_
  prior$first_seen_censored[lost & keep] <- 0L
  for (k in names(moved))
    message(sprintf("ai merge: moved or trimmed %d row(s): %s", moved[[k]], gsub("_", " ", k)))
  out <- prior[keep, , drop = FALSE]; rownames(out) <- NULL
  list(signals = out, review = .ai_bind_like(.ai_empty_review(), review), moved = moved)
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

#' TRUE for each vcs_ai_signals row that says nothing but when it was confirmed: no
#' onset, no evidence tier and no marker. That is the shape of a confirmation row, and
#' of the published row the reducer made out of one. A frame without a markers column
#' (select_confirmation_rows writes seven columns) reads as carrying no markers. Pure.
.ai_is_hollow <- function(rows) {
  blank <- function(cn) {
    x <- rows[[cn]]
    if (is.null(x)) return(rep(TRUE, nrow(rows)))
    x <- as.character(x)
    is.na(x) | !nzchar(x)
  }
  blank("first_seen_date") & blank("evidence_tiers") & blank("markers")
}

#' Drop the incoming confirmation rows that have no row to confirm.
#'
#' A confirmation only advances last_confirmed_date on a row that exists. With no prior
#' row for its (repo_id, tool), and no full row for that key arriving in the same merge,
#' ai_onset_reducer used to write it out as a row of its own carrying only the date. That
#' is how github.com/uchidamizuki/jpstat and github.com/doi-usgs/nhdplustools came to be
#' published with no onset, tiers or markers: reconcile_ai_identity had folded their rows
#' onto a sibling slug, and the gate had read a published copy that still held the keys.
#' Such a row cannot repair itself either, because select_incremental_repos treats its
#' key as published and never schedules the deep scan that would date it. Rows with any
#' evidence pass through, confirmation or not.
#'
#' Dropping loses nothing for good. The key is left out of what this merge publishes, so
#' if its slug is still active and its cheap pass still sees the tool, the next gate's
#' select_incremental_repos schedules the deep scan that dates it. A slug retired in the
#' meantime is not scanned again, and reconcile_ai_identity has already carried its rows
#' onto the canonical slug. Nor is the row copied from an active sibling slug here, for
#' the reason heal_hollow_siblings gives for not creating rows. Pure.
drop_unanchored_confirmations <- function(prior, incoming) {
  if (is.null(incoming) || nrow(incoming) == 0) return(incoming)
  key <- function(d) if (is.null(d) || nrow(d) == 0) character(0)
                     else paste(d$repo_id, d$tool, sep = "\r")
  hollow <- .ai_is_hollow(incoming)
  inc_key <- key(incoming)
  anchored <- inc_key %in% c(key(prior), inc_key[!hollow])
  incoming[!hollow | anchored, , drop = FALSE]
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
  # An evidence frame written before the counts existed carries neither column;
  # NA is right for it, because no search of that kind ran.
  if (!"authored_commits" %in% names(ev)) ev$authored_commits <- NA_integer_
  if (!"assisted_commits" %in% names(ev)) ev$assisted_commits <- NA_integer_
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
    markers = ev$marker,
    authored = as.integer(ev$authored),
    authored_commits = as.integer(ev$authored_commits),
    assisted_commits = as.integer(ev$assisted_commits),
    last_confirmed_date = last_confirmed,
    authored_measured_on = NA_character_, assisted_measured_on = NA_character_,
    stringsAsFactors = FALSE)
  ai_onset_reducer(.ai_empty_signals(), candidates)
}

#' Assemble the per-(tool, marker) onset frame build_ai_detail consumes, from a repo's
#' evidence plus the fetched onset dates. Each evidence row is matched to its onset source
#' by tier/marker shape: a Tier-D row is keyed by its FULL marker string and takes
#' marker_dates[[<marker>]] - a committed marker (an entry name) exact, an "ignore:<path>"
#' token as a censored "<=" floor (a .gitignore/.Rbuildignore entry names no committed path
#' to date). A PR row (marker == "PR") takes pr_date exact; a Tier
#' A/B/C row (marker == tier) takes commit_onsets for (tool, tier), exact when that onset's
#' `confirmed` is TRUE (a structured author match or a scan_trailers confirm) else a
#' censored floor - a fuzzy message-search candidate is never written as an exact immutable
#' onset. Pure: all dates/confirms are passed in. A row with no resolved onset keeps
#' first_seen_date NA (build_ai_detail leaves it NA in the detail row).
build_onset_map <- function(evidence, marker_dates = list(),
                            commit_onsets = NULL, pr_date = NA_character_,
                            exact_markers = character(0)) {
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
      # Keyed by the FULL marker string, so a committed marker and an ignore token for the
      # same tool never collide on a shared bare path. A committed marker is exact. An
      # ignore-token marker is a floor UNLESS the caller bisected its ignore file's history
      # and proved the line absent before that commit, which is what exact_markers carries.
      fs <- if (marker %in% names(marker_dates)) marker_dates[[marker]] else NA_character_
      fc <- if (ai_is_ignore_marker(marker) && !(marker %in% exact_markers)) 1L else 0L
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

#' Per-repo AI rollups for the summary. Only named repos get a row, so the join
#' gives NULL, never FALSE, for the rest. AGENTS.md and .agents stay out of the
#' count, the tool list and first and latest.
build_ai_rollups <- function(ai_signals) {
  if (is.null(ai_signals) || nrow(ai_signals) == 0) return(.ai_empty_rollups())
  if (!"agnostic" %in% names(ai_signals)) {
    agnostic_tools <- unique(unlist(lapply(Filter(function(m) isTRUE(m$agnostic), AI_MARKERS), `[[`, "tool")))
    ai_signals$agnostic <- ai_signals$tool %in% agnostic_tools
  }
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

#' Locate the earliest revision in `commits` (oldest first) at which `token` is present,
#' by bisection over a `present(i)` predicate.
#'
#' Returns list(index, exact). `index` is NA when the token is absent from every
#' revision tested. `exact` is TRUE only when the token is ABSENT at the oldest
#' revision and present at the found one, which together prove the line was added
#' inside the history we can see. When the oldest revision already carries it, the
#' true addition is at or before the start of history and the date is only a floor.
#'
#' Pure: the caller supplies `present`, so this is testable without any network, and
#' the count of probes is asserted in the tests because the whole point is log2(n).
#'
#' A token added, removed and re-added would fool a bisect. That is why `exact` rests
#' on the absent-at-oldest check rather than on the boundary alone: a clean absent to
#' present transition across the whole visible history is a single addition.
bisect_ignore_onset <- function(n, present) {
  if (n <= 0) return(list(index = NA_integer_, exact = FALSE))
  if (!isTRUE(present(n))) return(list(index = NA_integer_, exact = FALSE))  # never present
  if (isTRUE(present(1L))) return(list(index = 1L, exact = FALSE))           # present at the start
  lo <- 1L; hi <- n                       # present(lo) FALSE, present(hi) TRUE
  while (hi - lo > 1L) {
    mid <- lo + (hi - lo) %/% 2L
    if (isTRUE(present(mid))) hi <- mid else lo <- mid
  }
  list(index = hi, exact = TRUE)
}

#' Whether an ignore file's text holds a line the scanner would turn into `token`.
ignore_text_has_token <- function(text, token) {
  if (is.null(text) || is.na(text) || !nzchar(text)) return(FALSE)
  lines <- strsplit(text, "\r\n|\r|\n")[[1]]
  if (identical(token, ".aider*")) return(any(trimws(lines) == ".aider*"))
  toks <- vapply(lines, .ai_norm_ignore, character(1), USE.NAMES = FALSE)
  any(ai_ignore_line_matches(toks[nzchar(toks)], token))
}
