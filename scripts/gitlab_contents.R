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

  # One batch: asks, retries once without rejected optional fields, halves on any other failure.
  read <- function(d, rows) {
    if (!is.null(stopped[[d]])) { fail(rows, stopped[[d]]); return(invisible()) }
    key <- .gl_key(d)
    repeat {
      res <- .gl_contents_ask(io, pacer, d, gl_contents_query(dropped[[d]]), list(ids = as.list(.gl_gid(rows$node_id))))
      if (is.null(res)) { fail(rows, "request cap"); return(invisible()) }
      fields <- gl_schema_fields(res)
      if (!length(fields)) break
      pacer$after(key, TRUE)
      if (all(fields %in% GITLAB_OPTIONAL_FIELDS)) {
        dropped[[d]] <<- union(dropped[[d]], fields)
        .gl_log(log, sprintf("GitLab %s: no %s, asked again without", d, paste(fields, collapse = " or ")))
        next
      }
      stopped[[d]] <<- paste("schema lacks", paste(fields, collapse = ", "))
      .gl_log(log, sprintf("GitLab %s: contents not read, the schema lacks %s", d, paste(fields, collapse = ", ")))
      fail(rows, stopped[[d]]); return(invisible())
    }
    if (!.gl_answered(res)) {
      pacer$after(key, FALSE)
      if (nrow(rows) > 1L) {
        h <- ceiling(nrow(rows) / 2)
        read(d, rows[seq_len(h), , drop = FALSE]); read(d, rows[-seq_len(h), , drop = FALSE])
      } else {
        .gl_log(log, sprintf("GitLab %s: contents of %s not read: %s", d, rows$repo_id, .gl_why(res)))
        fail(rows, .gl_why(res))
      }
      return(invisible())
    }
    pacer$after(key, TRUE)
    s <- .gl_at(res, "data", "queryComplexity", "score")
    if (is.numeric(s) && length(s) == 1L && !is.na(s)) max_score <<- max(max_score, as.integer(s))
    got <- gl_parse_contents(res, rows)
    trees[names(got)] <<- got
    bad <- rows[rows$repo_id %in% attr(got, "unreadable"), , drop = FALSE]
    if (nrow(bad)) fail(bad, "root listing cut off or repository unreadable")
    invisible()
  }
  for (d in unique(dom))
    for (b in unname(chunk(which(dom == d), GITLAB_CONTENTS_BATCH)))
      read(d, roster[b, , drop = FALSE])

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
