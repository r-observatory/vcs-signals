# scripts/gitlab_contents.R - GitLab repository contents: one batched GraphQL read per host, parsed
# into the element shape parse_tree_markers gives GitHub repositories. Pure over an injected io.

GITLAB_CONTENTS_BATCH <- 20L  # projects per contents request; projects(ids:) is capped by query complexity
# Project fields an older host may lack; a rejected one is left out of that host's later asks.
GITLAB_OPTIONAL_FIELDS <- c("forkedFrom", "pagesDeployments")

# No pageInfo: it costs most of the complexity. A page of exactly 100 names is the only sign of a cut.
gl_contents_query <- function(drop = character(0)) {
  listing <- "trees(first: 100) { nodes { name } } blobs(first: 100) { nodes { name } }"
  trees <- vapply(names(GITLAB_TREE_ALIASES), function(a)
    sprintf('%s: tree(path: "%s") { %s }', a, GITLAB_TREE_ALIASES[[a]], listing), "")
  files <- paste(sprintf('"%s"', GITLAB_CONTENT_FILES), collapse = ", ")
  fields <- c(id = "id", fullPath = "fullPath", isForked = "isForked", forkedFrom = "forkedFrom { fullPath }",
              ciConfigPathOrDefault = "ciConfigPathOrDefault", pagesDeployments = "pagesDeployments { count }",
              issuesEnabled = "issuesEnabled")
  sprintf('query($ids: [ID!]) {
  queryComplexity { score limit }
  projects(ids: $ids, first: %d) { nodes {
    %s
    repository { rootRef empty
      root: tree { %s }
      %s
      blobs(paths: [%s]) { nodes { path rawTextBlob } }
    }
  } }
}', GITLAB_CONTENTS_BATCH, paste(fields[!names(fields) %in% drop], collapse = " "), listing,
    paste(trees, collapse = "\n      "), files)
}

gl_workflows_query <- function()
  sprintf('query($ids: [ID!], $paths: [String!]!) {
  projects(ids: $ids, first: %d) { nodes { id repository { blobs(paths: $paths) { nodes { path rawTextBlob } } } } }
}', GITLAB_CONTENTS_BATCH)

# The fields a schema error names, unique; character(0) when the answer has none.
gl_schema_fields <- function(res) {
  out <- vapply(Filter(is.list, .gl_at(res, "errors")), function(e) {
    if (identical(.gl_at(e, "extensions", "code"), "undefinedField") &&
        !is.na(.gl_chr(.gl_at(e, "extensions", "fieldName"))))
      return(.gl_at(e, "extensions", "fieldName"))
    m <- regmatches(.gl_chr(.gl_at(e, "message")), regexec("Field '([^']+)' doesn't exist on type", .gl_chr(.gl_at(e, "message"))))[[1]]
    if (length(m)) m[2] else NA_character_
  }, "")
  unique(out[!is.na(out)])
}

# ---- parsing -------------------------------------------------------------------
.gl_names <- function(tree, kind) {
  ns <- vapply(.nn(.gl_at(tree, kind, "nodes"), list()), function(n) .gl_chr(.gl_at(n, "name")), "")
  ns[!is.na(ns) & nzchar(ns)]
}
.gl_page_full <- function(tree)
  length(.nn(.gl_at(tree, "trees", "nodes"), list())) >= 100L || length(.nn(.gl_at(tree, "blobs", "nodes"), list())) >= 100L

.gl_lines <- function(txt) if (is.na(txt) || !nzchar(txt)) character(0) else strsplit(txt, "\r\n|\r|\n")[[1]]

# TRUE or FALSE for a flag the answer gave, NA for a null or a dropped field.
.gl_lgl <- function(x) if (is.logical(x) && length(x) == 1L && !is.na(x)) x else NA

#' Demux a contents answer into a named list by repo_id, in the shape parse_tree_markers gives GitHub
#' repositories. `roster` has repo_id and node_id. A roster row the answer leaves out is no element
#' and no failure; a node whose root listing is cut off or whose repository is null is no element,
#' and its repo_id is in attr(, "unreadable").
gl_parse_contents <- function(res, roster) {
  nodes <- .gl_nodes(res)
  ids <- vapply(nodes, function(n) .gl_chr(.gl_at(n, "id")), "")
  out <- list(); unreadable <- character(0)
  for (j in seq_len(nrow(roster))) {
    hit <- match(.gl_gid(roster$node_id[j]), ids)
    if (is.na(hit)) next
    n <- nodes[[hit]]
    repo <- .gl_at(n, "repository")
    if (!is.list(repo) || .gl_page_full(.gl_at(repo, "root"))) { unreadable <- c(unreadable, roster$repo_id[j]); next }
    blobs <- .nn(.gl_at(repo, "blobs", "nodes"), list())
    paths <- vapply(blobs, function(b) .gl_chr(.gl_at(b, "path")), "")
    text <- function(p) { i <- match(p, paths); if (is.na(i)) NA_character_ else .gl_chr(.gl_at(blobs[[i]], "rawTextBlob")) }
    root <- c(.gl_names(repo$root, "trees"), .gl_names(repo$root, "blobs"))
    gh <- character(0); truncated <- character(0)
    for (a in names(GITLAB_TREE_ALIASES)) {
      path <- GITLAB_TREE_ALIASES[[a]]
      tree <- .gl_at(repo, a)
      ns <- c(.gl_names(tree, "trees"), .gl_names(tree, "blobs"))
      if (.gl_page_full(tree)) truncated <- c(truncated, a)
      if (identical(path, ".github")) gh <- c(gh, ns)
      else if (startsWith(path, ".github/")) gh <- c(gh, if (length(ns)) paste0(sub("^\\.github/", "", path), "/", ns))
      else root <- c(root, if (length(ns)) paste0(path, "/", ns))
    }
    # Only the named Duo files are read, so an unknown file next to a known one is not seen.
    root <- c(root, paths[!is.na(paths) & startsWith(paths, ".gitlab/duo/")])
    rbi <- text(".Rbuildignore")
    el <- list(
      root_entries = root, github_entries = gh,
      is_fork = .gl_lgl(.gl_at(n, "isForked")),
      parent = .gl_chr(.gl_at(n, "forkedFrom", "fullPath")),
      name_with_owner = .gl_chr(.gl_at(n, "fullPath")),
      has_issues_enabled = .gl_lgl(.gl_at(n, "issuesEnabled")),
      gl_pages_count = .gl_int(.gl_at(n, "pagesDeployments", "count")),
      gitignore_lines = .gl_lines(text(".gitignore")), rbuildignore_lines = .gl_lines(rbi),
      rbuildignore_text = rbi, desc_text = text("DESCRIPTION"),
      pkgdown_yml = c(gh_pages = NA_character_, docs = text("docs/pkgdown.yml")),
      gitlab_ci = list(text = text(".gitlab-ci.yml"), config_path = .gl_chr(.gl_at(n, "ciConfigPathOrDefault"))),
      coc_url = NA_character_, contributing_url = NA_character_)
    yml <- grep("[.]ya?ml$", .gl_names(.gl_at(repo, "workflows"), "blobs"), ignore.case = TRUE)
    if (!length(yml)) el["workflows"] <- list(NULL)
    el$truncated <- truncated
    out[[roster$repo_id[j]]] <- el
  }
  attr(out, "unreadable") <- unreadable
  out
}

#' Workflow texts by repo_id from a gl_workflows_query answer. `wanted` has repo_id, node_id and
#' name, one row per workflow file; each project keeps only its own files, null texts are dropped.
gl_parse_workflows <- function(res, wanted) {
  nodes <- .gl_nodes(res)
  ids <- vapply(nodes, function(n) .gl_chr(.gl_at(n, "id")), "")
  out <- list()
  for (r in unique(wanted$repo_id)) {
    w <- wanted[wanted$repo_id == r, , drop = FALSE]
    hit <- match(.gl_gid(w$node_id[1]), ids)
    if (is.na(hit)) next
    blobs <- .nn(.gl_at(nodes[[hit]], "repository", "blobs", "nodes"), list())
    p <- vapply(blobs, function(b) .gl_chr(.gl_at(b, "path")), "")
    tx <- vapply(blobs, function(b) .gl_chr(.gl_at(b, "rawTextBlob")), "")
    keep <- !is.na(tx) & p %in% paste0(".github/workflows/", w$name)
    out[[r]] <- data.frame(name = sub("^.*/", "", p[keep]), text = tx[keep], stringsAsFactors = FALSE)
  }
  out
}

# ---- collection ----------------------------------------------------------------
# One paced ask; returns the answer, or NULL when the pacer refused.
.gl_contents_ask <- function(io, pacer, domain, query, variables) {
  key <- .gl_key(domain)
  if (!isTRUE(pacer$before(key))) return(NULL)
  tryCatch(io$gl_graphql(domain, query, variables), error = function(e) list(error = conditionMessage(e)))
}

.gl_answered <- function(res) {
  e <- .gl_at(res, "error")
  is.list(res) && (is.null(e) || is.na(.gl_chr(e))) && !is.null(.gl_nodes(res))
}

# The query complexity an answer reported, or NA.
.gl_score <- function(res) {
  s <- .gl_at(res, "data", "queryComplexity", "score")
  if (is.numeric(s) && length(s) == 1L && !is.na(s)) as.integer(s) else NA_integer_
}

# One batch, asked again in halves down to single projects while its answer fails. `ask(rows)` returns
# an answer, NULL when the pacer refused, or "done" when it settled the rows itself; `take(res, rows)`
# reads an answer; `lost(rows, why, res)` records rows left unread (res is NULL for a refusal).
# Only a one-project failure counts toward the breaker; a failed larger batch may hold one bad project.
.gl_halving <- function(pacer, key, rows, ask, take, lost) {
  res <- ask(rows)
  if (identical(res, "done")) return(invisible())
  if (is.null(res)) return(lost(rows, "request cap", NULL))
  if (.gl_answered(res)) { pacer$after(key, TRUE); return(take(res, rows)) }
  if (nrow(rows) == 1L) { pacer$after(key, FALSE); return(lost(rows, .gl_why(res), res)) }
  h <- ceiling(nrow(rows) / 2)
  .gl_halving(pacer, key, rows[seq_len(h), , drop = FALSE], ask, take, lost)
  .gl_halving(pacer, key, rows[-seq_len(h), , drop = FALSE], ask, take, lost)
}

#' Reads every roster project's contents, `GITLAB_CONTENTS_BATCH` per request, grouped by the node id's
#' domain. `roster` has repo_id and node_id. Returns trees (elements by repo_id), failed (a
#' fetch-failure frame), dropped (optional fields a host rejected, by domain) and max_score (the
#' highest query complexity an answer reported).
gl_collect_contents <- function(io, roster, pacer, log = NULL) {
  trees <- list(); failed <- list(); dropped <- list(); max_score <- 0L
  fail <- function(rows, why)
    failed[[length(failed) + 1L]] <<- .fetch_failed_frame(rows$repo_id, "gitlab-contents", why, .utc_now())
  stopped <- list()
  odd <- is.na(roster$node_id) | !grepl(.GL_NODE_RE, roster$node_id)
  if (any(odd)) {
    .gl_log(log, sprintf("GitLab contents: %d ids are not GitLab node ids and were not asked", sum(odd)))
    fail(roster[odd, , drop = FALSE], "not a GitLab node id")
  }
  roster <- roster[!odd, , drop = FALSE]
  dom <- sub(.GL_NODE_RE, "\\1", roster$node_id)

  for (d in unique(dom)) {
    key <- .gl_key(d)
    # Asks, and again without optional fields the host rejects; a required field it lacks stops the host.
    ask <- function(rows) {
      if (!is.null(stopped[[d]])) { fail(rows, stopped[[d]]); return("done") }
      repeat {
        res <- .gl_contents_ask(io, pacer, d, gl_contents_query(dropped[[d]]), list(ids = as.list(.gl_gid(rows$node_id))))
        fields <- gl_schema_fields(res)
        if (!length(fields)) return(res)
        pacer$after(key, TRUE)
        if (all(fields %in% GITLAB_OPTIONAL_FIELDS)) {
          dropped[[d]] <<- union(dropped[[d]], fields)
          .gl_log(log, sprintf("GitLab %s: no %s, asked again without", d, paste(fields, collapse = " or ")))
          next
        }
        stopped[[d]] <<- paste("schema lacks", paste(fields, collapse = ", "))
        .gl_log(log, sprintf("GitLab %s: contents not read, the schema lacks %s", d, paste(fields, collapse = ", ")))
        fail(rows, stopped[[d]]); return("done")
      }
    }
    take <- function(res, rows) {
      max_score <<- max(max_score, .gl_score(res), na.rm = TRUE)
      got <- gl_parse_contents(res, rows)
      trees[names(got)] <<- got
      bad <- rows[rows$repo_id %in% attr(got, "unreadable"), , drop = FALSE]
      if (nrow(bad)) fail(bad, "root listing cut off or repository unreadable")
    }
    lost <- function(rows, why, res) {
      if (!is.null(res)) .gl_log(log, sprintf("GitLab %s: contents of %s not read: %s", d, rows$repo_id, why))
      fail(rows, why)
    }
    for (b in unname(chunk(which(dom == d), GITLAB_CONTENTS_BATCH)))
      .gl_halving(pacer, key, roster[b, , drop = FALSE], ask, take, lost)
  }

  # GitHub workflow files, a second request per batch for the projects that list YAML files.
  for (d in unique(dom)) {
    rows <- roster[dom == d & roster$repo_id %in% names(trees), , drop = FALSE]
    want <- do.call(rbind, c(list(data.frame(repo_id = character(), node_id = character(), name = character())),
      lapply(seq_len(nrow(rows)), function(j) {
        el <- trees[[rows$repo_id[j]]]
        nm <- sub("^workflows/", "", grep("^workflows/.*[.]ya?ml$", el$github_entries, value = TRUE, ignore.case = TRUE))
        if (!"workflows" %in% names(el) && length(nm))
          data.frame(repo_id = rows$repo_id[j], node_id = rows$node_id[j], name = nm, stringsAsFactors = FALSE)
      })))
    for (b in unname(chunk(unique(want$repo_id), GITLAB_CONTENTS_BATCH))) {
      w <- want[want$repo_id %in% b, , drop = FALSE]
      ids <- unique(w$node_id)
      res <- .gl_contents_ask(io, pacer, d, gl_workflows_query(),
                              list(ids = as.list(.gl_gid(ids)), paths = as.list(unique(paste0(".github/workflows/", w$name)))))
      if (is.null(res) || !.gl_answered(res)) {
        if (!is.null(res)) pacer$after(.gl_key(d), FALSE)
        .gl_log(log, sprintf("GitLab %s: workflow files of %d projects not read", d, length(b)))
        next
      }
      pacer$after(.gl_key(d), TRUE)
      got <- gl_parse_workflows(res, w)
      for (r in names(got)) trees[[r]]["workflows"] <- list(got[[r]])
    }
  }
  list(trees = trees, failed = if (length(failed)) do.call(rbind, failed) else .fetch_failed_frame(),
       dropped = dropped, max_score = max_score)
}

# ---- commit-message search -----------------------------------------------------
# A word escaped so both regexp syntaxes GitLab applies read it literally and in either case: RE2
# validates the query, then git's basic grep runs it. A letter becomes a class of its two cases, other
# metacharacters a one-character class, or a backslash for the four a class cannot hold.
.gl_re_literal <- function(x) {
  ch <- strsplit(x, "")[[1]]
  ch <- ifelse(grepl("^[A-Za-z]$", ch), paste0("[", toupper(ch), tolower(ch), "]"),
        ifelse(ch %in% c(".", "+", "*", "?", "(", ")", "{", "}", "|", "$"), paste0("[", ch, "]"),
        ifelse(ch %in% c("\\", "^", "[", "]"), paste0("\\", ch), ch)))
  paste(ch, collapse = "")
}

#' The prefilter GitLab runs over commit messages: each rule's search phrase and each of `extra`, any
#' run of characters other than letters and digits between words, phrases joined by \| as git's basic
#' grep reads them (measured on gitlab.com 2026-10-20: a (?i) group or a bare | matched nothing there).
#' The query cannot ask the host to ignore case, so every letter spells both. It holds no lookaround;
#' each rule's own pattern decides locally what counts.
gl_message_regex <- function(rules = c(AI_TRAILER_PATTERNS, AI_REVIEW_RULES), extra = GITLAB_MESSAGE_EXTRA_PHRASES) {
  phrases <- vapply(c(vapply(rules, function(r) gsub('"', "", r$query), ""), extra), function(q) {
    w <- strsplit(q, " +")[[1]]
    paste(vapply(w[nzchar(w)], .gl_re_literal, ""), collapse = "[^A-Za-z0-9]*")
  }, "")
  paste(unique(phrases), collapse = "\\|")
}

#' The message search over up to GITLAB_MESSAGE_BATCH projects, or with `after` one project's next
#' page. It asks for no author.
gl_message_query <- function(after = FALSE)
  sprintf(paste0('query($ids: [ID!], $re: UntrustedRegexp!%s) { queryComplexity { score limit } ',
                 'projects(ids: $ids, first: %d) { nodes { id repository { ',
                 'commits(ref: "HEAD", query: $re, first: 100%s) { pageInfo { hasNextPage endCursor } ',
                 'nodes { sha committedDate message } } } } } }'),
          if (after) ", $after: String" else "", GITLAB_MESSAGE_BATCH, if (after) ", after: $after" else "")

#' TRUE when a host's schema has no message search: no data, and an error naming Repository.commits,
#' its query argument or the UntrustedRegexp type. A transport error, timeout or complexity refusal is FALSE.
gl_message_unsupported <- function(res) {
  if (!is.list(res) || !is.null(.gl_nodes(res))) return(FALSE)
  said <- function(e) {
    ext <- function(k) .gl_chr(.gl_at(e, "extensions", k))
    code <- ext("code"); msg <- .gl_chr(.gl_at(e, "message"))
    (code %in% "undefinedField" && ext("fieldName") %in% "commits") ||
      (code %in% "argumentNotAccepted" && ext("argumentName") %in% "query") ||
      (code %in% "undefinedType" && ext("typeName") %in% "UntrustedRegexp") ||
      (!is.na(msg) && grepl(paste0("Field 'commits' doesn't exist on type 'Repository'|",
                                   "Field 'commits' doesn't accept argument 'query'|UntrustedRegexp isn't a defined"), msg))
  }
  any(vapply(Filter(is.list, .gl_at(res, "errors")), said, NA))
}

#' Commit nodes of a message search in match_commit_findings' shape: dates in UTC, author columns NA,
#' so no account or author-name rule can match.
gl_message_commits <- function(nodes) {
  nodes <- Filter(function(n) is.list(n) && !is.na(.gl_chr(.gl_at(n, "sha"))), nodes %||% list())
  chr <- function(f) vapply(nodes, f, "")
  data.frame(oid = chr(function(n) .gl_chr(.gl_at(n, "sha"))),
             committed_at = chr(function(n) .gl_utc(.gl_at(n, "committedDate"))),
             message = chr(function(n) .gl_chr(.gl_at(n, "message"))),
             author_name = rep(NA_character_, length(nodes)), author_email = rep(NA_character_, length(nodes)),
             stringsAsFactors = FALSE)
}

#' Searches every roster project's commit messages: first pages GITLAB_MESSAGE_BATCH per request, then
#' later pages one project per request up to GITLAB_MESSAGE_PAGE_CAP. A host without the search is
#' asked once and its projects get no entry. Returns reads (by repo_id: commits, and complete when the
#' last page said there was no next), unsupported (domains), failed (a fetch-failure frame) and max_score.
gl_collect_messages <- function(io, roster, pacer, log = NULL) {
  reads <- list(); failed <- list(); unsupported <- character(0); max_score <- 0L
  fail <- function(rows, why)
    failed[[length(failed) + 1L]] <<- .fetch_failed_frame(rows$repo_id, "gitlab-messages", why, .utc_now())
  roster <- roster[!is.na(roster$node_id) & grepl(.GL_NODE_RE, roster$node_id), , drop = FALSE]
  dom <- sub(.GL_NODE_RE, "\\1", roster$node_id)
  re <- gl_message_regex()
  for (d in unique(dom)) {
    key <- .gl_key(d)
    page <- function(rows, after = NULL)
      .gl_contents_ask(io, pacer, d, gl_message_query(!is.null(after)),
                       c(list(ids = as.list(.gl_gid(rows$node_id)), re = re), if (!is.null(after)) list(after = after)))
    ask <- function(rows) {
      if (d %in% unsupported) return("done")
      res <- page(rows)
      if (is.null(res) || !gl_message_unsupported(res)) return(res)
      pacer$after(key, TRUE)
      unsupported <<- c(unsupported, d)
      .gl_log(log, sprintf("GitLab %s: no commit message search, its projects are not searched", d))
      "done"
    }
    commits_of <- function(res, row) {
      nodes <- .gl_nodes(res)
      hit <- match(.gl_gid(row$node_id), vapply(nodes, function(n) .gl_chr(.gl_at(n, "id")), ""))
      if (is.na(hit)) NULL else .gl_at(nodes[[hit]], "repository", "commits")
    }
    # One project from its first page on; any page not read leaves it with no entry.
    follow <- function(row, cm) {
      got <- list()
      repeat {
        if (!is.list(cm)) return(fail(row, "commit search unreadable"))
        got[[length(got) + 1L]] <- gl_message_commits(.gl_at(cm, "nodes"))
        more <- isTRUE(.gl_at(cm, "pageInfo", "hasNextPage"))
        if (!more || length(got) >= GITLAB_MESSAGE_PAGE_CAP) break
        cursor <- .gl_chr(.gl_at(cm, "pageInfo", "endCursor"))
        if (is.na(cursor)) return(fail(row, "no cursor for the next page"))
        res <- page(row, cursor)
        if (is.null(res)) return(fail(row, "request cap"))
        if (!.gl_answered(res)) {
          pacer$after(key, FALSE)
          .gl_log(log, sprintf("GitLab %s: commit messages of %s not read: %s", d, row$repo_id, .gl_why(res)))
          return(fail(row, .gl_why(res)))
        }
        pacer$after(key, TRUE)
        max_score <<- max(max_score, .gl_score(res), na.rm = TRUE)
        cm <- commits_of(res, row)
      }
      cms <- do.call(rbind, got)
      reads[[row$repo_id]] <<- list(commits = cms[!duplicated(cms$oid), , drop = FALSE], complete = !more)
    }
    take <- function(res, rows) {
      max_score <<- max(max_score, .gl_score(res), na.rm = TRUE)
      ids <- vapply(.gl_nodes(res), function(n) .gl_chr(.gl_at(n, "id")), "")
      for (j in which(.gl_gid(rows$node_id) %in% ids)) {
        row <- rows[j, , drop = FALSE]
        follow(row, commits_of(res, row))
      }
    }
    lost <- function(rows, why, res) {
      if (!is.null(res)) .gl_log(log, sprintf("GitLab %s: commit messages of %s not read: %s", d, rows$repo_id, why))
      fail(rows, why)
    }
    for (b in unname(chunk(which(dom == d), GITLAB_MESSAGE_BATCH)))
      .gl_halving(pacer, key, roster[b, , drop = FALSE], ask, take, lost)
  }
  list(reads = reads, unsupported = unsupported,
       failed = if (length(failed)) do.call(rbind, failed) else .fetch_failed_frame(), max_score = max_score)
}

#' Search-log rows of one project's message search, in the cheap partial's search_log shape. A whole
#' search answers every message and review rule, hit or none, and replaces the project's rows; the
#' Assisted-by rule also gets a hit row per tool its lines named. A cut-off search gives its hits only,
#' incomplete and undated, since the oldest match was not reached.
gl_message_log_rows <- function(hits, repo_id, today, complete) {
  rules <- c(AI_TRAILER_PATTERNS, AI_REVIEW_RULES)
  keys <- vapply(rules, `[[`, "", "key")
  rev <- vapply(rules, function(r) as.integer(r$rev), 1L)
  h <- .ai_bind_like(.ai_empty_commit_hits(), list(hits))
  h <- h[h$rule_key %in% keys, , drop = FALSE]
  ab <- "msg.any.assisted-by"
  tools <- sort(unique(h$tool[h$rule_key == ab]))
  groups <- c(lapply(keys, function(k) h[h$rule_key == k, , drop = FALSE]),
              lapply(tools, function(t) h[h$rule_key == ab & h$tool == t, , drop = FALSE]))
  n <- vapply(groups, function(g) length(unique(g$oid)), 1L)
  first <- vapply(groups, function(g) substr(.ai_earliest_chr(g$committed_at), 1, 10), "")
  out <- data.frame(repo_id = rep(repo_id, length(n)), rule_key = c(keys, paste0(ab, ".", tools, recycle0 = TRUE)),
    rule_rev = c(rev, rep(rev[keys == ab], length(tools))), ruleset_version = AI_RULESET_VERSION,
    asked_on = today, outcome = ifelse(n > 0L, "hit", "none"), total_count = n,
    verified = ifelse(n > 0L, 1L, NA_integer_), incomplete = if (complete) 0L else 1L,
    first_hit_on = if (complete) ifelse(n > 0L, first, NA_character_) else NA_character_,
    source = "search", mode = if (complete) "replace" else NA_character_, read_after = NA_character_,
    stringsAsFactors = FALSE)
  if (!complete) out <- out[n > 0L, , drop = FALSE]
  rownames(out) <- NULL
  out
}
