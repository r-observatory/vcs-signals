# Trimmed real GitHub responses captured for the weekly AI read, under fixtures/ai-2026-09/.
ai_fixture <- function(name)
  jsonlite::fromJSON(paste(readLines(file.path("fixtures", "ai-2026-09", name), warn = FALSE,
                                     encoding = "UTF-8"), collapse = "\n"),
                     simplifyVector = FALSE)

# One commit per case, as GraphQL returns it, keyed by the short oid.
ai_commit <- function(short) {
  nodes <- ai_fixture("commits.json")
  hit <- Filter(function(n) startsWith(n$oid, short), nodes)
  if (length(hit) != 1L) stop("no single fixture commit ", short)
  .ai_commit_nodes_frame(hit)
}
