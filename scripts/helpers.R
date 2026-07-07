# scripts/helpers.R - pure resolver + repo-model helpers for vcs-signals SP1.
# Depends on constants from scripts/config.R. No network. The only side effect is
# write_repo_tables (SQLite). Functions are added task-by-task below.

# ---- URL splitting ---------------------------------------------------------
split_urls <- function(field) {
  if (is.null(field) || length(field) == 0) return(character(0))
  x <- field[[1]]
  if (is.na(x) || !nzchar(trimws(x))) return(character(0))
  parts <- trimws(strsplit(x, "[,[:space:]]+")[[1]])
  parts <- sub("^<+", "", parts)
  parts <- sub(">+$", "", parts)
  parts[nzchar(parts)]
}
