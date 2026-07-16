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

#' The `<selection>` block for a connection metric: `edges { starredAt }` for
#' stars, `nodes { createdAt }` for the other cumulative metrics, and
#' `nodes { createdAt closedAt }` for open metrics (whose ts_close is set),
#' so the open-count reconstruction can see when (if ever) each item closed.
.connection_selection <- function(mc) {
  fields <- if (!is.null(mc$ts_close)) paste(mc$ts, mc$ts_close) else mc$ts
  sprintf("%s { %s }", mc$sel, fields)
}

#' Build a GraphQL query paging one connection (stargazers/forks/releases/
#' issues/pullRequests) for a repo, per METRIC_CONNECTIONS' per-metric shape.
#' `after` is embedded as `null` (no quotes) when absent, or as a quoted cursor.
build_connection_query <- function(owner, name, metric, after = NULL) {
  mc <- METRIC_CONNECTIONS[[metric]]
  if (is.null(mc)) stop(sprintf("unknown metric: %s", metric))
  after_arg <- if (is.null(after) || is.na(after)) "null" else sprintf('"%s"', after)
  sprintf('query { repository(owner: "%s", name: "%s") {
    %s(first: %d, orderBy: {field: %s, direction: ASC}, after: %s) {
      pageInfo { endCursor hasNextPage }
      %s
    }
  } }', owner, name, mc$conn, STARGAZER_PAGE, mc$order, after_arg, .connection_selection(mc))
}

#' Thin wrapper preserving the stars-only query builder for existing callers.
build_stargazers_query <- function(owner, name, after = NULL) {
  build_connection_query(owner, name, "stars", after)
}

#' Build one aliased multi-repo query batching a metric's FIRST connection
#' page across many repos into a single GraphQL request (a multi-repo
#' aliased query costs ~1 point regardless of repo count, since only the
#' repository lookup and one page per repo are requested - no `after`, this
#' is always page 1). `repos` is a data.frame with repo_id, owner, name; the
#' alias `r<idx>` (0-based, by row order) is mapped back to repo_id by
#' fetch_batched_page.
build_batched_query <- function(repos, metric) {
  mc <- METRIC_CONNECTIONS[[metric]]
  if (is.null(mc)) stop(sprintf("unknown metric: %s", metric))
  sel <- .connection_selection(mc)
  parts <- vapply(seq_len(nrow(repos)), function(j) {
    sprintf('r%d: repository(owner: "%s", name: "%s") {
      %s(first: %d, orderBy: {field: %s, direction: ASC}) {
        pageInfo { endCursor hasNextPage }
        %s
      }
    }', j - 1L, repos$owner[j], repos$name[j], mc$conn, STARGAZER_PAGE, mc$order, sel)
  }, character(1))
  sprintf('query { %s }', paste(parts, collapse = "\n"))
}

#' Build one aliased multi-repo query batching the weekly commit-count
#' collection (`defaultBranchRef.target.history.totalCount`) across a chunk
#' of repos into a single request. Unlike build_batched_query's connection
#' pages, `history.totalCount` alone costs ~1 GraphQL point regardless of
#' chunk size, but is expensive for GitHub to *compute* server-side, so
#' chunks are kept small (COMMIT_HISTORY_BATCH) to avoid execution-time
#' errors rather than to manage point budget. The alias `r<idx>` (0-based, by
#' row order) is mapped back to repo_id by fetch_commit_counts.
build_commits_batched_query <- function(repos) {
  parts <- vapply(seq_len(nrow(repos)), function(j) {
    sprintf('r%d: repository(owner: "%s", name: "%s") {
      defaultBranchRef { target { ... on Commit {
        history { totalCount }
      } } }
    }', j - 1L, repos$owner[j], repos$name[j])
  }, character(1))
  sprintf('query { %s }', paste(parts, collapse = "\n"))
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

#' Typed zero-row nodes frame for a metric's kind: ts for cumulative metrics,
#' created/closed for open metrics. Shared by parse_connection's null-connection
#' branch and paginate_connection's never-looped (impossible) empty case.
.empty_connection_nodes <- function(mc) {
  if (identical(mc$kind, "open"))
    data.frame(created = character(0), closed = character(0), stringsAsFactors = FALSE)
  else
    data.frame(ts = character(0), stringsAsFactors = FALSE)
}

#' Extract the connection's items + pagination state from one connection-query
#' response, per the metric's connection field and selection shape (edges for
#' stars, nodes for the rest). Returns list(nodes, end_cursor, has_next):
#' `nodes` is a data.frame with column `ts` for cumulative metrics, or columns
#' `created`/`closed` for open metrics (`closed` is NA for a still-open item).
#' Degrades to empty nodes + has_next=FALSE when the connection node itself is
#' null (e.g. repository not found).
parse_connection <- function(resp, metric) {
  mc <- METRIC_CONNECTIONS[[metric]]
  if (is.null(mc)) stop(sprintf("unknown metric: %s", metric))
  conn <- resp$data$repository[[mc$conn]]
  if (is.null(conn))
    return(list(nodes = .empty_connection_nodes(mc), end_cursor = NA_character_, has_next = FALSE))
  items <- .nn(conn[[mc$sel]], list())
  if (identical(mc$kind, "open")) {
    created <- vapply(items, function(e) .nn(e[[mc$ts]], NA_character_), character(1))
    closed  <- vapply(items, function(e) .nn(e[[mc$ts_close]], NA_character_), character(1))
    nodes <- data.frame(created = created, closed = closed, stringsAsFactors = FALSE)
  } else {
    ts <- vapply(items, function(e) .nn(e[[mc$ts]], NA_character_), character(1))
    nodes <- data.frame(ts = ts, stringsAsFactors = FALSE)
  }
  list(nodes = nodes,
       end_cursor = .nn(conn$pageInfo$endCursor, NA_character_),
       has_next = isTRUE(conn$pageInfo$hasNextPage))
}

#' Extract the cumulative timestamp vector from a parsed connection result
#' (cumulative metrics only, whose nodes frame carries a single `ts` column).
connection_timestamps <- function(parsed) parsed$nodes$ts

#' Thin wrapper preserving the stars-only parser for existing callers.
parse_stargazers <- function(resp) {
  out <- parse_connection(resp, "stars")
  list(starred_at = connection_timestamps(out), end_cursor = out$end_cursor, has_next = out$has_next)
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

#' Parse the `Link` response header GitHub's REST API returns on the
#' contributors endpoint (called with per_page=1&anon=true, see
#' fetch_contributor_count) into a contributor count. When the response is
#' paginated, the `rel="last"` link's `page` query parameter IS the total
#' contributor count, since each page holds exactly one item. When there is
#' no `Link` header at all (fewer than 2 contributors), the count is simply
#' the number of items in the parsed response body (0 or 1). Pure: `headers`
#' is the vector of raw header lines from a `gh api ... -i` response (or
#' character(0)/no matching line), so this is unit-testable without a
#' network call.
parse_contributor_link_count <- function(headers, body_len) {
  link_line <- grep("^link:", headers, ignore.case = TRUE, value = TRUE)
  if (length(link_line) == 0) return(as.integer(body_len))
  m <- regmatches(link_line[1],
                  regexec('[?&]page=([0-9]+)[^,]*>;\\s*rel="last"', link_line[1]))[[1]]
  if (length(m) < 2) return(as.integer(body_len))
  as.integer(m[2])
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

#' Fetch a repo's contributor count via GitHub's REST contributors endpoint,
#' called with per_page=1&anon=true so the response is minimal and its
#' `Link` header's rel="last" page number directly gives the count (see
#' parse_contributor_link_count). Same transport style as gh_graphql: routed
#' through `gh` (auth, TLS), the token set via the environment and restored
#' afterward, response headers captured via `-i`. Returns NA on any
#' transport error, non-2xx status (e.g. a 404'd/renamed repo), or
#' unparseable output, so one bad repo never aborts its caller's chunk.
fetch_contributor_count <- function(token, owner, name) {
  old <- Sys.getenv("GH_TOKEN", unset = NA)
  Sys.setenv(GH_TOKEN = token)
  on.exit({
    if (is.na(old)) Sys.unsetenv("GH_TOKEN") else Sys.setenv(GH_TOKEN = old)
  }, add = TRUE)

  # Pass the query as -X GET fields (not a "?a=1&b=2" URL) so the ampersand is
  # never handed to a shell; capture stdout only so stderr cannot corrupt the
  # header block we parse.
  endpoint <- sprintf("repos/%s/%s/contributors", owner, name)
  out <- suppressWarnings(system2("gh", c("api", "-X", "GET", endpoint,
                                          "-f", "per_page=1", "-f", "anon=true", "-i"),
                                  stdout = TRUE))
  status <- attr(out, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) return(NA_integer_)

  blank <- which(!nzchar(trimws(out)))
  if (length(blank) == 0) return(NA_integer_)
  header_lines <- out[seq_len(blank[1] - 1L)]
  body_txt <- paste(out[(blank[1] + 1L):length(out)], collapse = "\n")
  body <- tryCatch(jsonlite::fromJSON(body_txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) return(NA_integer_)

  parse_contributor_link_count(header_lines, length(body))
}

#' Run one batched first-page query (build_batched_query) for a chunk of
#' repos and parse it into a named list keyed by repo_id, each value the same
#' list(nodes, end_cursor, has_next) shape parse_connection returns for a
#' single repo. Reading each alias `r<idx>` back to its repo_id is purely
#' positional (by row order in `repos`), matching build_batched_query.
#'
#' Errors on the same errors/null-data contract paginate_connection enforces
#' (rather than degrading silently), so the caller's per-chunk handling turns
#' a bad batched response into a skip/fallback, never a partial page treated
#' as complete. A per-alias null (repo gone/renamed) is not an error here -
#' parse_connection already degrades that one repo to empty/has_next=FALSE.
fetch_batched_page <- function(io, repos, metric) {
  mc <- METRIC_CONNECTIONS[[metric]]
  if (is.null(mc)) stop(sprintf("unknown metric: %s", metric))
  resp <- io$graphql(build_batched_query(repos, metric))
  if (!is.null(resp$errors) || is.null(resp$data)) stop(sprintf("%s batched page error", mc$conn))
  out <- vector("list", nrow(repos))
  names(out) <- repos$repo_id
  for (j in seq_len(nrow(repos))) {
    alias_resp <- list(data = list(repository = resp$data[[sprintf("r%d", j - 1L)]]))
    out[[j]] <- parse_connection(alias_resp, metric)
  }
  out
}

#' Run one batched commit-count query (build_commits_batched_query) for a
#' chunk of repos and return a named list keyed by repo_id, each value an
#' integer commit count - NA when `defaultBranchRef`/`target`/`history` is
#' null (e.g. an empty repo with no default branch). Errors on a
#' transport-level failure or an in-body GraphQL error/null data (the same
#' contract fetch_batched_page enforces), so the caller's per-chunk handling
#' turns a bad batched response into a skip (NA for the whole chunk) rather
#' than silently treating it as "every repo has zero commits".
fetch_commit_counts <- function(io, repos) {
  resp <- io$graphql(build_commits_batched_query(repos))
  if (!is.null(resp$errors) || is.null(resp$data)) stop("commits batched page error")
  out <- vector("list", nrow(repos))
  names(out) <- repos$repo_id
  for (j in seq_len(nrow(repos))) {
    r <- resp$data[[sprintf("r%d", j - 1L)]]
    total <- NA_integer_
    if (!is.null(r)) total <- .nn(r$defaultBranchRef$target$history$totalCount, NA_integer_)
    out[[j]] <- as.integer(total)
  }
  out
}

#' Page through a repo's full connection for the given metric (ASC by the
#' metric's order field: STARRED_AT for stars, CREATED_AT for the rest),
#' looping the `after` cursor until `hasNextPage` is FALSE. Sleeps `delay`
#' after every request (each page, including the last) for rate-limit pacing.
#' Returns a `nodes` data.frame (all pages, ts or created/closed columns per
#' parse_connection). Empty for a repo with genuinely no items on this connection.
#'
#' When `after` and `first_nodes` are supplied, resumes from that cursor
#' instead of starting at page 1, prepending `first_nodes` (typically the
#' already-fetched batched first page) to what this call fetches - so a repo
#' whose first page reported hasNextPage is completed without re-fetching it.
#'
#' Errors (rather than silently truncating) on any response that carries
#' `errors` or a null `data` - GitHub commonly returns HTTP 200 with
#' `{"data":{"repository":{"stargazers":null}},"errors":[...]}` on a transient
#' fault or in-body secondary-limit for a large connection, which
#' parse_connection alone would read as a clean end-of-pages and yield a
#' truncated curve. Same gate collect_batched/resolve_node_ids use. Also
#' stops on a malformed page that claims a next page but gives no cursor,
#' which would otherwise re-fetch page 1 forever. The caller's per-repo,
#' per-metric tryCatch turns any of these into a skip (that metric left for a
#' re-run), never a persisted partial curve.
paginate_connection <- function(io, owner, name, metric, delay = BACKFILL_DELAY_S,
                                after = NULL, first_nodes = NULL) {
  mc <- METRIC_CONNECTIONS[[metric]]
  if (is.null(mc)) stop(sprintf("unknown metric: %s", metric))
  parts <- if (!is.null(first_nodes)) list(first_nodes) else list()
  repeat {
    resp <- io$graphql(build_connection_query(owner, name, metric, after))
    if (!is.null(resp$errors) || is.null(resp$data)) stop(sprintf("%s page error", mc$conn))
    if (delay > 0) Sys.sleep(delay)
    parsed <- parse_connection(resp, metric)
    parts[[length(parts) + 1L]] <- parsed$nodes
    if (!isTRUE(parsed$has_next)) break
    if (is.na(parsed$end_cursor) || !nzchar(parsed$end_cursor))
      stop(sprintf("%s page claims a next page but returned no cursor", mc$conn))
    after <- parsed$end_cursor
  }
  do.call(rbind, parts)
}

#' Thin wrapper preserving the stars-only paginator for existing callers.
paginate_stargazers <- function(io, owner, name, delay = BACKFILL_DELAY_S) {
  paginate_connection(io, owner, name, "stars", delay = delay)
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
#' TRUE when every error in a GraphQL response is a per-alias NOT_FOUND: GitHub
#' answered the batch, but one aliased repo is deleted, renamed away, or gone
#' private. It reports that as HTTP 200 carrying partial `data` (that alias null,
#' the rest intact) plus an errors[] entry scoped to the alias via `path`. Such a
#' response is USABLE - the surviving aliases hold real ids. Any other error (rate
#' limit, 502, timeout) is not alias-scoped and could have nulled a live alias, so
#' the batch must be deferred instead of read as "these repos are all gone".
errors_are_alias_not_found <- function(errs) {
  if (is.null(errs) || length(errs) == 0) return(FALSE)
  all(vapply(errs, function(e) identical(e$type, "NOT_FOUND") && length(e$path) > 0, logical(1)))
}

#' Resolve node ids for repos needing them, one CHEAP_BATCH-sized query at a
#' time. A batch whose response is unusable (io$graphql throws, res$data is NULL,
#' or res$errors carries anything that is not an alias-scoped NOT_FOUND - e.g. a
#' rate limit or transient 502) is DEFERRED: it contributes no rows at all, so
#' update_repo_node_ids never touches those repos and they stay
#' node_id=NULL/status='active' for retry on the next run.
#'
#' A batch is still parsed when its only errors are alias-scoped NOT_FOUNDs, where
#' each per-alias null (repo actually gone/renamed-away) becomes status='gone'.
#' Deferring those would be permanent, not transient: the batch's live repos would
#' stay node_id=NULL, be re-selected next run, be re-batched with the same dead
#' repo, and be discarded again forever - so one deleted repo would silently strand
#' every repo it happens to share a batch with, and they would never gain a signal.
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
    ok <- is.list(res) && is.null(res$.err) && !is.null(res$data) &&
      (is.null(res$errors) || errors_are_alias_not_found(res$errors))
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

# ---- weekly responsiveness (bounded single-page medians) --------------------
#' One aliased query per batch: the 50 most-recently-updated closed issues and
#' resolved (merged|closed) PRs, plus one oldest-first page of open issues, per
#' repo. Costs ~1 GraphQL point regardless of batch size (aliased single query).
build_responsiveness_query <- function(repos) {
  parts <- vapply(seq_len(nrow(repos)), function(j) {
    sprintf('r%d: repository(owner: "%s", name: "%s") {
      closedIssues: issues(states: CLOSED, first: 50, orderBy: {field: UPDATED_AT, direction: DESC}) {
        nodes { createdAt closedAt } }
      resolvedPRs: pullRequests(states: [MERGED, CLOSED], first: 50, orderBy: {field: UPDATED_AT, direction: DESC}) {
        nodes { createdAt closedAt } }
      openIssues: issues(states: OPEN, first: 50, orderBy: {field: CREATED_AT, direction: ASC}) {
        nodes { createdAt } }
    }', j - 1L, repos$owner[j], repos$name[j])
  }, character(1))
  sprintf('query { %s }', paste(parts, collapse = "\n"))
}

#' Reduce a parsed responsiveness response to one row of three integer medians
#' per repo. A null alias (repo deleted/renamed away) yields all-NA for that
#' repo rather than dropping it.
parse_responsiveness <- function(resp, repos, today) {
  ts <- function(nodes, field) vapply(nodes, function(n) {
    v <- n[[field]]; if (is.null(v)) NA_character_ else v }, character(1))
  do.call(rbind, lapply(seq_len(nrow(repos)), function(j) {
    r <- resp$data[[sprintf("r%d", j - 1L)]]
    ci <- if (is.null(r)) list() else r$closedIssues$nodes
    pr <- if (is.null(r)) list() else r$resolvedPRs$nodes
    oi <- if (is.null(r)) list() else r$openIssues$nodes
    data.frame(repo_id = repos$repo_id[j],
      median_days_to_close_issue = median_days_to_close(ts(ci, "createdAt"), ts(ci, "closedAt")),
      median_days_to_close_pr    = median_days_to_close(ts(pr, "createdAt"), ts(pr, "closedAt")),
      median_open_issue_age_days = median_open_issue_age(ts(oi, "createdAt"), today),
      stringsAsFactors = FALSE)
  }))
}

#' Fetch + parse responsiveness for one batch of repos.
fetch_responsiveness <- function(io, repos, today) {
  resp <- io$graphql(build_responsiveness_query(repos))
  if (!is.null(resp$errors) && is.null(resp$data)) stop("responsiveness batch error")
  parse_responsiveness(resp, repos, today)
}

# ---- AI-tooling detection collection -------------------------------------
# Pure query builders + response parsers for the cheap Tier-D marker pass, the
# PR-agent pass, and the onset lookups. Unit-tested against inline fixtures like
# build_responsiveness_query / parse_responsiveness above. The two impure
# transports at the end (marked) are not unit-tested, like fetch_contributor_count.

#' One aliased multi-repo query returning, per repo: the root-tree entry names
#' (expression "HEAD:"), the .github-tree entry names ("HEAD:.github"), isFork,
#' and parent.nameWithOwner. object() is null when a tree is absent (empty repo,
#' no .github), so the parser guards it. Alias r<idx> (0-based) maps back to
#' repo_id by row order. The entry-name vectors feed classify_tree_markers.
build_tree_query <- function(repos) {
  parts <- vapply(seq_len(nrow(repos)), function(j) {
    sprintf('r%d: repository(owner: "%s", name: "%s") {
      isFork parent { nameWithOwner }
      rootTree: object(expression: "HEAD:") { ... on Tree { entries { name type } } }
      githubTree: object(expression: "HEAD:.github") { ... on Tree { entries { name type } } }
      gitignore: object(expression: "HEAD:.gitignore") { ... on Blob { text } }
      rbuildignore: object(expression: "HEAD:.Rbuildignore") { ... on Blob { text } }
    }', j - 1L, repos$owner[j], repos$name[j])
  }, character(1))
  sprintf('query { %s }', paste(parts, collapse = "\n"))
}

#' Demux a build_tree_query response into a named list keyed by repo_id, each
#' value list(root_entries, github_entries, is_fork, parent). A null alias (repo
#' gone) or a null object() (absent tree) degrades to empty entries, so the cheap
#' pass never reads "could not fetch the tree" as "no markers".
parse_tree_markers <- function(resp, repos) {
  entry_names <- function(tree) {
    ns <- vapply(.nn(tree$entries, list()), function(e) .nn(e$name, ""), character(1))
    ns[nzchar(ns)]
  }
  blob_lines <- function(blob) {
    txt <- .nn(blob$text, NA_character_)
    if (is.na(txt) || !nzchar(txt)) return(character(0))
    strsplit(txt, "\n", fixed = TRUE)[[1]]
  }
  out <- vector("list", nrow(repos))
  names(out) <- repos$repo_id
  for (j in seq_len(nrow(repos))) {
    r <- resp$data[[sprintf("r%d", j - 1L)]]
    if (is.null(r)) {
      out[[j]] <- list(root_entries = character(0), github_entries = character(0),
                       is_fork = NA, parent = NA_character_,
                       gitignore_lines = character(0), rbuildignore_lines = character(0))
      next
    }
    out[[j]] <- list(
      root_entries = entry_names(r$rootTree),
      github_entries = entry_names(r$githubTree),
      is_fork = isTRUE(r$isFork),
      parent = .nn(r$parent$nameWithOwner, NA_character_),
      gitignore_lines = blob_lines(r$gitignore),
      rbuildignore_lines = blob_lines(r$rbuildignore))
  }
  out
}

#' One aliased multi-repo query for the oldest 50 PRs per repo (CREATED_AT ASC),
#' each with author { login __typename } and createdAt, plus pageInfo so the
#' orchestrator can decide which repos need further paging toward the agent era.
#' Always page 1: a single shared `after` cursor across aliases is meaningless,
#' so per-repo follow-up paging is the orchestrator's job (Plan B2).
build_pr_agent_query <- function(repos) {
  parts <- vapply(seq_len(nrow(repos)), function(j) {
    sprintf('r%d: repository(owner: "%s", name: "%s") {
      pullRequests(first: 50, orderBy: {field: CREATED_AT, direction: ASC}) {
        pageInfo { endCursor hasNextPage }
        nodes { author { login __typename } createdAt }
      }
    }', j - 1L, repos$owner[j], repos$name[j])
  }, character(1))
  sprintf('query { %s }', paste(parts, collapse = "\n"))
}

#' Demux a build_pr_agent_query response into a named list keyed by repo_id, each
#' value list(prs = data.frame(login, typename, created_at), has_next). A null
#' author (deleted account) yields login = NA. __typename is surfaced for
#' provenance only and never trusted alone - detection is detect_pr_agents(login)
#' against the allowlist, so Dependabot/renovate/github-actions never flag.
parse_pr_agents <- function(resp, repos) {
  empty <- data.frame(login = character(0), typename = character(0),
                      created_at = character(0), stringsAsFactors = FALSE)
  out <- vector("list", nrow(repos))
  names(out) <- repos$repo_id
  for (j in seq_len(nrow(repos))) {
    r <- resp$data[[sprintf("r%d", j - 1L)]]
    if (is.null(r) || is.null(r$pullRequests)) { out[[j]] <- list(prs = empty, has_next = FALSE); next }
    nodes <- .nn(r$pullRequests$nodes, list())
    out[[j]] <- list(
      prs = data.frame(
        login      = vapply(nodes, function(n) .nn(n$author$login, NA_character_), character(1)),
        typename   = vapply(nodes, function(n) .nn(n$author[["__typename"]], NA_character_), character(1)),
        created_at = vapply(nodes, function(n) .nn(n$createdAt, NA_character_), character(1)),
        stringsAsFactors = FALSE),
      has_next = isTRUE(r$pullRequests$pageInfo$hasNextPage))
  }
  out
}

#' Pure: the earliest-match commit date from a search/commits JSON body, or NA when
#' total_count is 0, items is empty, or the body does not parse. The match is FUZZY
#' (substring-ish), so the caller treats this date as a CANDIDATE onset.
parse_search_commit <- function(body_txt) {
  body <- tryCatch(jsonlite::fromJSON(body_txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) return(NA_character_)
  items <- .nn(body$items, list())
  if (isTRUE(.nn(body$total_count, length(items)) == 0) || length(items) == 0) return(NA_character_)
  .nn(items[[1]]$commit$committer$date, NA_character_)
}

#' A marker path plus any known predecessor paths (AI_MARKER_PREDECESSORS), probed
#' together so a rename (e.g. .cursorrules -> .cursor) does not reset the onset.
expand_marker_paths <- function(path) {
  preds <- unname(AI_MARKER_PREDECESSORS[names(AI_MARKER_PREDECESSORS) == path])
  unique(c(path, preds[!is.na(preds)]))
}

#' One page of a path's commit history on the default branch (DESC, newest first).
#' `after` is embedded as null (unquoted) when absent, else as a quoted cursor.
build_marker_history_query <- function(owner, name, path, after = NULL) {
  after_arg <- if (is.null(after) || is.na(after)) "null" else sprintf('"%s"', after)
  sprintf('query { repository(owner: "%s", name: "%s") {
    defaultBranchRef { target { ... on Commit {
      history(first: 100, path: "%s", after: %s) {
        pageInfo { endCursor hasNextPage }
        nodes { committedDate }
      }
    } } }
  } }', owner, name, path, after_arg)
}

#' Parse one marker-history page to list(dates, end_cursor, has_next). Degrades to
#' empty when defaultBranchRef / target / history is null (empty repo, or the path
#' never existed on the default branch).
parse_marker_history <- function(resp) {
  h <- resp$data$repository$defaultBranchRef$target$history
  if (is.null(h)) return(list(dates = character(0), end_cursor = NA_character_, has_next = FALSE))
  list(dates = vapply(.nn(h$nodes, list()), function(n) .nn(n$committedDate, NA_character_), character(1)),
       end_cursor = .nn(h$pageInfo$endCursor, NA_character_),
       has_next = isTRUE(h$pageInfo$hasNextPage))
}

#' Earliest commit date touching `path` (or a known predecessor path) on the default
#' branch. Pages history(path:) to exhaustion (DESC, so the oldest touch is on the
#' last page; marker files have few touching commits) and returns the min committedDate
#' across all probed paths, or NA when nothing touches them OR a page faults mid-scan
#' (fails closed - never folds a partial newest-page min, which would write a too-recent
#' onset; left for a re-run, never written as "no marker"). Uses io$graphql (GraphQL
#' budget), so it paces with BACKFILL_DELAY_S, not SEARCH_DELAY_S.
fetch_marker_onset <- function(io, owner, name, path, delay = BACKFILL_DELAY_S) {
  earliest <- NA_character_
  for (p in expand_marker_paths(path)) {
    after <- NULL; oldest <- NA_character_
    repeat {
      resp <- io$graphql(build_marker_history_query(owner, name, p, after))
      # history(path:) is DESC, so the true earliest touch is on the LAST page. A
      # mid-pagination fault means we never reached it; discarding this path's partial
      # (newest-page) min is the only correct choice - folding it would write a
      # too-recent onset into the immutable table. Fail closed, leave it for a re-run.
      if (!is.null(resp$errors) || is.null(resp$data)) return(NA_character_)
      if (delay > 0) Sys.sleep(delay)
      pg <- parse_marker_history(resp)
      d <- pg$dates[!is.na(pg$dates)]
      if (length(d)) oldest <- if (is.na(oldest)) min(d) else min(oldest, min(d))
      if (!isTRUE(pg$has_next) || is.na(pg$end_cursor) || !nzchar(pg$end_cursor)) break
      after <- pg$end_cursor
    }
    if (!is.na(oldest)) earliest <- if (is.na(earliest)) oldest else min(earliest, oldest)
  }
  earliest
}

# --- transport (not unit-tested) ---

#' Earliest commit in owner/name whose message matches `query`, via the REST
#' commit-search API. One request returns the server-side earliest match. `sort` and
#' `order` MUST be paired or GitHub silently returns best-match desc. Same transport
#' style as fetch_contributor_count: routed through gh, GH_TOKEN set/restored, NA on
#' any transport error or non-2xx so one bad repo never aborts a scan. Sleeps `delay`
#' after the request because the search budget (~30/min) is separate from and tighter
#' than GraphQL, so pacing at the transport keeps the caller's loop simple. FUZZY: the
#' returned date is a CANDIDATE the caller verifies (scan_trailers) or records as a
#' censored floor.
search_earliest_commit <- function(token, owner, name, query, delay = SEARCH_DELAY_S) {
  old <- Sys.getenv("GH_TOKEN", unset = NA)
  Sys.setenv(GH_TOKEN = token)
  on.exit({ if (is.na(old)) Sys.unsetenv("GH_TOKEN") else Sys.setenv(GH_TOKEN = old) }, add = TRUE)
  q <- sprintf("repo:%s/%s %s", owner, name, query)
  out <- suppressWarnings(system2("gh", c("api", "-X", "GET", "search/commits",
    "-f", paste0("q=", q), "-f", "sort=committer-date", "-f", "order=asc", "-f", "per_page=1"),
    stdout = TRUE))
  if (delay > 0) Sys.sleep(delay)
  status <- attr(out, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) return(NA_character_)
  parse_search_commit(paste(out, collapse = "\n"))
}
