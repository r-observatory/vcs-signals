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
