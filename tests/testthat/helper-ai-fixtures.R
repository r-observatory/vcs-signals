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

# Answers GitHub gave for the weekly documents' known repositories. Pass a response to
# replace the account-count, activity or fixed-object answer.
ai_canary_io <- function(accounts = NULL, activity = NULL, fixed = NULL) {
  prs <- ai_fixture("activity-prs.json")$data
  cm <- ai_fixture("commits.json")
  pick <- function(oid) Find(function(n) identical(n$oid, oid), cm)
  good_fixed <- list(data = list(p0 = list(pullRequest = prs$r0$pullRequests$nodes[[1]]),
                                 p1 = list(pullRequest = prs$r4$pullRequests$nodes[[1]]),
                                 c0 = list(object = pick("fe5c63c197db1fe8ce70eb740bb5ffe4af0f4e99")),
                                 c1 = list(object = pick("ab1da7b7b32be605c5f291a7ace100a0e65e08f6"))))
  good_accounts <- list(data = list(
    r0 = list(defaultBranchRef = list(target = list(
      a_claude = list(totalCount = 0L, nodes = list()),
      a_amazonq = list(totalCount = 2L, nodes = list(list(committedDate = "2025-05-09T00:00:00Z")))))),
    r1 = list(defaultBranchRef = list(target = list(
      a_cursor = list(totalCount = 19L, nodes = list(list(committedDate = "2026-08-13T09:47:23Z"))))))))
  one <- list(pullRequests = list(totalCount = 0L, pageInfo = list(endCursor = NULL, hasNextPage = FALSE), nodes = list()),
              defaultBranchRef = list(target = list(recent = list(pageInfo = list(endCursor = NULL, hasNextPage = FALSE),
                                                                  nodes = list()))))
  good_activity <- list(data = list(r0 = one, r1 = one))
  list(graphql = function(q) {
    if (grepl("pullRequest(number:", q, fixed = TRUE)) return(fixed %||% good_fixed)
    if (grepl("a_claude:", q, fixed = TRUE)) return(accounts %||% good_accounts)
    activity %||% good_activity
  })
}
