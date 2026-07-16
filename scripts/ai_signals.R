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
