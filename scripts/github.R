# scripts/github.R - GitHub GraphQL adapter for vcs-signals.
# Pure query builders + response parsers are unit-tested against fixtures.
# Only gh_graphql (added later) touches the network, behind an injected io object.

# ---- query builders --------------------------------------------------------
build_gauge_query <- function(node_ids) {
  ids <- paste(sprintf('"%s"', node_ids), collapse = ", ")
  sprintf('query { nodes(ids: [%s]) { ... on Repository {
    id nameWithOwner stargazerCount forkCount
    watchers { totalCount }
    issues_open: issues(states: OPEN) { totalCount }
    issues_closed: issues(states: CLOSED) { totalCount }
    prs_open: pullRequests(states: OPEN) { totalCount }
    prs_closed: pullRequests(states: CLOSED) { totalCount }
    mergedPRs: pullRequests(states: MERGED) { totalCount }
    releases { totalCount }
    latestRelease { tagName publishedAt isPrerelease }
    licenseInfo { spdxId }
    repositoryTopics(first: 20) { nodes { topic { name } } }
    isArchived isFork isMirror isDisabled
    createdAt pushedAt updatedAt diskUsage
    primaryLanguage { name }
  } } }', ids)
}

build_commit_query <- function(node_ids) {
  ids <- paste(sprintf('"%s"', node_ids), collapse = ", ")
  sprintf('query { nodes(ids: [%s]) { ... on Repository {
    id nameWithOwner
    defaultBranchRef { target { ... on Commit {
      history { totalCount }
      last: history(first: 1) { nodes { committedDate } }
    } } }
  } } }', ids)
}

build_resolve_query <- function(owners, names) {
  parts <- vapply(seq_along(owners), function(i) sprintf(
    'r%d: repository(owner: "%s", name: "%s", followRenames: true) { id nameWithOwner isArchived isFork isMirror isDisabled createdAt }',
    i - 1L, owners[i], names[i]), "")
  sprintf('query { %s }', paste(parts, collapse = "\n"))
}

# ---- response parsers ------------------------------------------------------
.nn <- function(x, default) if (is.null(x)) default else x

parse_gauges <- function(nodes) {
  rows <- lapply(nodes, function(n) {
    if (is.null(n)) return(NULL)
    topics <- vapply(.nn(n$repositoryTopics$nodes, list()),
                     function(t) .nn(t$topic$name, ""), "")
    data.frame(
      node_id = n$id, name_with_owner = n$nameWithOwner,
      stars = .nn(n$stargazerCount, NA_integer_), forks = .nn(n$forkCount, NA_integer_),
      watchers = .nn(n$watchers$totalCount, NA_integer_),
      issues_open = .nn(n[["issues_open"]]$totalCount, NA_integer_),
      issues_closed = .nn(n[["issues_closed"]]$totalCount, NA_integer_),
      prs_open = .nn(n[["prs_open"]]$totalCount, NA_integer_),
      prs_closed = .nn(n[["prs_closed"]]$totalCount, NA_integer_),
      prs_merged = .nn(n[["mergedPRs"]]$totalCount, NA_integer_),
      releases_total = .nn(n$releases$totalCount, NA_integer_),
      size_kb = .nn(n$diskUsage, NA_integer_),
      license = .nn(n$licenseInfo$spdxId, NA_character_),
      topics = paste(topics, collapse = ","),
      is_archived = as.integer(isTRUE(n$isArchived)),
      is_fork = as.integer(isTRUE(n$isFork)),
      is_mirror = as.integer(isTRUE(n$isMirror)),
      created_at = .nn(n$createdAt, NA_character_),
      pushed_at = .nn(n$pushedAt, NA_character_),
      last_release_at = .nn(n$latestRelease$publishedAt, NA_character_),
      stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(rows_df_empty_gauges())
  do.call(rbind, rows)
}

rows_df_empty_gauges <- function() {
  data.frame(node_id = character(), name_with_owner = character(), stars = integer(),
    forks = integer(), watchers = integer(), issues_open = integer(), issues_closed = integer(),
    prs_open = integer(), prs_closed = integer(), prs_merged = integer(), releases_total = integer(),
    size_kb = integer(), license = character(), topics = character(), is_archived = integer(),
    is_fork = integer(), is_mirror = integer(), created_at = character(), pushed_at = character(),
    last_release_at = character(), stringsAsFactors = FALSE)
}

parse_resolve <- function(data, n) {
  do.call(rbind, lapply(seq_len(n) - 1L, function(i) {
    r <- data[[sprintf("r%d", i)]]
    data.frame(idx = i,
      node_id = if (is.null(r)) NA_character_ else r$id,
      name_with_owner = if (is.null(r)) NA_character_ else r$nameWithOwner,
      is_archived = if (is.null(r)) NA_integer_ else as.integer(isTRUE(r$isArchived)),
      is_fork = if (is.null(r)) NA_integer_ else as.integer(isTRUE(r$isFork)),
      is_mirror = if (is.null(r)) NA_integer_ else as.integer(isTRUE(r$isMirror)),
      created_at = if (is.null(r)) NA_character_ else .nn(r$createdAt, NA_character_),
      stringsAsFactors = FALSE)
  }))
}

parse_commits <- function(nodes) {
  rows <- lapply(nodes, function(n) {
    if (is.null(n)) return(NULL)
    tgt <- n$defaultBranchRef$target
    total <- .nn(tgt$history$totalCount, NA_integer_)
    last <- NA_character_
    ln <- tgt$last$nodes
    if (!is.null(ln) && length(ln) >= 1) last <- .nn(ln[[1]]$committedDate, NA_character_)
    data.frame(node_id = n$id, commits_total = total, last_commit_date = last, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0)
    return(data.frame(node_id = character(), commits_total = integer(),
                      last_commit_date = character(), stringsAsFactors = FALSE))
  do.call(rbind, rows)
}
