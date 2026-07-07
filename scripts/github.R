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

build_stargazers_query <- function(owner, name, after = NULL) {
  after_arg <- if (is.null(after) || is.na(after)) "null" else sprintf('"%s"', after)
  sprintf('query { repository(owner: "%s", name: "%s") {
    stargazers(first: %d, orderBy: {field: STARRED_AT, direction: ASC}, after: %s) {
      pageInfo { endCursor hasNextPage }
      edges { starredAt }
    }
  } }', owner, name, STARGAZER_PAGE, after_arg)
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

#' Extract starredAt timestamps + pagination state from one stargazers query
#' response. Degrades to an empty, has_next=FALSE result when the
#' `stargazers` node itself is null (e.g. repository not found).
parse_stargazers <- function(resp) {
  sg <- resp$data$repository$stargazers
  if (is.null(sg)) return(list(starred_at = character(0), end_cursor = NA_character_, has_next = FALSE))
  edges <- .nn(sg$edges, list())
  starred_at <- vapply(edges, function(e) .nn(e$starredAt, NA_character_), character(1))
  list(starred_at = starred_at,
       end_cursor = .nn(sg$pageInfo$endCursor, NA_character_),
       has_next = isTRUE(sg$pageInfo$hasNextPage))
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

# ---- transport (not unit-tested) ------------------------------------------
gh_graphql <- function(token, query) {
  # Route through gh (handles auth, TLS, and JSON cleanly). The query is passed via
  # a JSON request-body file (--input), never on the command line, so system2's shell
  # invocation cannot mangle its braces. The token is set via the environment (restored
  # afterward so release ops keep theirs); only stdout is captured and parsed.
  old <- Sys.getenv("GH_TOKEN", unset = NA)
  Sys.setenv(GH_TOKEN = token)
  bf <- tempfile(fileext = ".json")
  on.exit({
    unlink(bf)
    if (is.na(old)) Sys.unsetenv("GH_TOKEN") else Sys.setenv(GH_TOKEN = old)
  }, add = TRUE)
  writeLines(jsonlite::toJSON(list(query = query), auto_unbox = TRUE), bf)
  out <- suppressWarnings(system2("gh", c("api", "graphql", "--input", bf), stdout = TRUE))
  txt <- paste(out, collapse = "\n")
  if (!nzchar(trimws(txt))) stop("gh api graphql returned no output")
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

default_io <- function(token) {
  list(graphql = function(query) gh_graphql(token, query))
}

#' Page through a repo's full stargazers connection (ASC by starredAt),
#' looping the `after` cursor until `hasNextPage` is FALSE, pausing `delay`
#' between pages. Returns character(0) for a repo with no stargazers.
paginate_stargazers <- function(io, owner, name, delay = BACKFILL_DELAY_S) {
  starred_at <- character(0)
  after <- NULL
  repeat {
    resp <- io$graphql(build_stargazers_query(owner, name, after))
    parsed <- parse_stargazers(resp)
    starred_at <- c(starred_at, parsed$starred_at)
    if (!isTRUE(parsed$has_next)) break
    after <- parsed$end_cursor
    Sys.sleep(delay)
  }
  starred_at
}

# ---- batched collection with the 502/partial failure contract -------------
collect_batched <- function(io, ids, batch_size, build_query, parse_nodes) {
  records <- list(); deferred <- character(0)
  queue <- unname(chunk(ids, batch_size))
  while (length(queue) > 0) {
    b <- queue[[1]]; queue <- queue[-1]
    res <- tryCatch(io$graphql(build_query(b)), error = function(e) list(.err = TRUE))
    Sys.sleep(BATCH_DELAY_S)
    ok <- is.list(res) && is.null(res$.err) && is.null(res$errors) && !is.null(res$data)
    if (ok) {
      df <- parse_nodes(res$data$nodes)
      if (!is.null(df) && nrow(df) > 0) records[[length(records) + 1L]] <- df
    } else if (length(b) > 1) {
      queue <- c(unname(chunk(b, ceiling(length(b) / 2))), queue)   # halve and retry
    } else {
      deferred <- c(deferred, b)                                     # single repo still failing
    }
  }
  list(records = if (length(records)) do.call(rbind, records) else NULL, deferred = deferred)
}

collect_gauges <- function(io, node_ids) {
  # Daily forward pass = the fast cheap gauges only. Commit count (history.totalCount)
  # is too slow to fetch for every repo daily, so it is collected on the weekly heavy
  # pass. build_commit_query / parse_commits remain for that pass to use.
  cheap <- collect_batched(io, node_ids, CHEAP_BATCH, build_gauge_query, parse_gauges)
  list(snapshot = cheap$records, deferred = cheap$deferred)
}

# ---- node-id resolution stage ----------------------------------------------
#' Resolve node ids for repos needing them, one CHEAP_BATCH-sized query at a
#' time. A batch whose response is unusable (io$graphql throws, res$errors is
#' non-null, or res$data is NULL - e.g. a rate limit or transient 502) is
#' DEFERRED: it contributes no rows at all, so update_repo_node_ids never
#' touches those repos and they stay node_id=NULL/status='active' for retry
#' on the next run. Only a genuinely successful batch is parsed, where a
#' per-alias null (repo actually gone/renamed-away) becomes status='gone'.
resolve_node_ids <- function(io, repos_needing) {
  empty <- data.frame(repo_id = character(), node_id = character(), owner = character(),
    name = character(), name_with_owner = character(), status = character(), stringsAsFactors = FALSE)
  if (nrow(repos_needing) == 0) return(empty)
  idx <- chunk(seq_len(nrow(repos_needing)), CHEAP_BATCH)
  out <- lapply(idx, function(rowset) {
    sub <- repos_needing[rowset, , drop = FALSE]
    res <- tryCatch(io$graphql(build_resolve_query(sub$owner, sub$name)),
                     error = function(e) list(.err = TRUE))
    Sys.sleep(BATCH_DELAY_S)
    ok <- is.list(res) && is.null(res$.err) && is.null(res$errors) && !is.null(res$data)
    if (!ok) return(NULL)
    pr <- parse_resolve(res$data, nrow(sub))
    do.call(rbind, lapply(seq_len(nrow(sub)), function(j) {
      r <- pr[pr$idx == (j - 1L), ]
      if (is.na(r$node_id)) {
        data.frame(repo_id = sub$repo_id[j], node_id = NA_character_, owner = sub$owner[j],
          name = sub$name[j], name_with_owner = paste(sub$owner[j], sub$name[j], sep = "/"),
          status = "gone", stringsAsFactors = FALSE)
      } else {
        parts <- strsplit(r$name_with_owner, "/", fixed = TRUE)[[1]]
        data.frame(repo_id = sub$repo_id[j], node_id = r$node_id,
          owner = parts[1], name = paste(parts[-1], collapse = "/"),
          name_with_owner = r$name_with_owner, status = "active", stringsAsFactors = FALSE)
      }
    }))
  })
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) return(empty)
  do.call(rbind, out)
}

# ---- rate-limit preflight ---------------------------------------------------
#' Remaining GraphQL rate-limit points, or Inf when the response carries no
#' rateLimit field at all (transport error, or a fake io in existing tests
#' that does not mock rateLimit - both treated as "unlimited", so this never
#' makes an unrelated test start skipping stages it didn't intend to skip).
graphql_rate_remaining <- function(io) {
  res <- tryCatch(io$graphql("query { rateLimit { remaining resetAt } }"), error = function(e) NULL)
  rem <- res$data$rateLimit$remaining
  if (is.null(rem)) return(Inf)
  as.integer(rem)
}
