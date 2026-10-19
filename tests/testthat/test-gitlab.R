# Recorded GitLab GraphQL answers, trimmed from the recon samples, behind a fake io and a fake clock.
gl_fixture <- function(name) jsonlite::fromJSON(file.path("fixtures", "gitlab", name), simplifyVector = FALSE)
gl_answer <- function(body) list(data = body$data, errors = body$errors, status = 200L, error = NA_character_)
gl_pool_of <- function(name) gl_fixture(name)$data$projects$nodes
gl_frame_names <- function() c(names(rows_df_empty_gauges()), "full_path", "gl_id")

# Answers a batch as GitLab does: unknown keys left out, paths matched without case, newest id first.
# `answer(domain, variables)` may return a whole answer instead. Every call is kept in seen$calls.
fake_gl_io <- function(pool, seen = new.env(), answer = NULL, clock = NULL, cost = 0) {
  seen$calls <- list()
  list(gl_graphql = function(domain, query, variables) {
    seen$calls[[length(seen$calls) + 1L]] <- list(domain = domain, query = query, variables = variables,
                                                  at = if (is.null(clock)) NA_real_ else clock$t)
    if (!is.null(clock)) clock$t <- clock$t + cost
    if (!is.null(answer)) {
      a <- answer(domain, variables)
      if (!is.null(a)) return(a)
    }
    keys <- unlist(variables$keys)
    hit <- Filter(function(n) tolower(n$fullPath) %in% tolower(keys) || n$id %in% keys, pool[[domain]])
    num <- vapply(hit, function(n) as.numeric(sub(".*/", "", n$id)), 0)
    hit <- hit[order(-num)]
    list(data = list(projects = list(count = length(hit), pageInfo = list(hasNextPage = FALSE), nodes = hit)),
         errors = NULL, status = 200L, error = NA_character_)
  })
}

# Bare projects for batches too large to record.
gl_synthetic <- function(n, from = 1L) lapply(seq_len(n) + from - 1L, function(i)
  list(id = sprintf("gid://gitlab/Project/%d", i), fullPath = sprintf("g/p%d", i), starCount = i))

# The GitLab pacer on a fake clock; the real Sys.sleep is never called.
fake_gl_pacer <- function(start = 3600 * 100) {
  clock <- new.env()
  clock$t <- start
  clock$slept <- numeric()
  p <- gl_pacer(now = function() clock$t,
                sleep = function(s) { clock$slept <- c(clock$slept, s); clock$t <- clock$t + s })
  list(p = p, clock = clock)
}

# Wraps a pacer and keeps what after() was told, per call.
recording_pacer <- function(p, seen = new.env()) {
  seen$after <- character()
  list(before = p$before,
       after = function(host, ok) { seen$after <- c(seen$after, paste(host, ok)); p$after(host, ok) },
       deferred = p$deferred, spent = p$spent)
}

gl_repos <- function(...) {
  ids <- c(...)
  data.frame(repo_id = ids, domain = sub("/.*$", "", ids), path = sub("^[^/]+/", "", ids),
             stringsAsFactors = FALSE)
}
gl_map <- function(node_ids, repo_ids = sprintf("r%d", seq_along(node_ids))) {
  data.frame(node_id = node_ids, repo_id = repo_ids,
             domain = sub("^gitlab:([^:]+):.*$", "\\1", node_ids), stringsAsFactors = FALSE)
}

test_that("the query asks by path or by id, 50 at a time, with the feature flags", {
  qp <- gl_projects_query("paths")
  expect_match(qp, "query($keys: [String!])", fixed = TRUE)
  expect_match(qp, "projects(fullPaths: $keys, first: 50)", fixed = TRUE)
  qi <- gl_projects_query("ids")
  expect_match(qi, "query($keys: [ID!])", fixed = TRUE)
  expect_match(qi, "projects(ids: $keys, first: 50)", fixed = TRUE)
  expect_false(grepl("fullPaths", qi, fixed = TRUE))
  for (f in c("starCount", "forksCount", "archived", "isForked", "topics", "createdAt", "lastActivityAt",
              "issuesEnabled", "mergeRequestsEnabled", "openIssuesCount", "openMergeRequestsCount",
              "issueStatusCounts { closed }", "mrClosed: mergeRequests(state: closed) { count }",
              "mrMerged: mergeRequests(state: merged) { count }",
              "releases(first: 5, sort: RELEASED_AT_DESC) { count nodes { tagName releasedAt upcomingRelease } }",
              "tree { lastCommit { committedDate } }"))
    expect_match(qp, f, fixed = TRUE, info = f)
  expect_identical(gl_projects_query(), qp)
  expect_error(gl_projects_query("names"))
})

test_that("the API base comes from GITLAB_DOMAINS, else the domain itself", {
  expect_identical(gl_api_base("gitlab.com"), "https://gitlab.com")
  expect_identical(gl_api_base("forgemia.inra.fr"), "https://forge.inrae.fr")
  expect_identical(gl_api_base("git.example.org"), "https://git.example.org")
})

test_that("a token is read per domain and is NA when unset or empty", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = "t0k", GITLAB_TOKEN_GIT_NOMICS_WORLD = "",
                      GITLAB_TOKEN_CODE_USGS_GOV = NA)
  expect_identical(gl_token("gitlab.com"), "t0k")
  expect_identical(gl_token("git.nomics.world"), NA_character_)
  expect_identical(gl_token("code.usgs.gov"), NA_character_)
})

# A fake curl that keeps its arguments and token header lines and answers with `body`.
gl_fake_curl <- function(body, seen, status = 200L) {
  function(cmd, args, stdout = TRUE, ...) {
    plain <- gsub("^'|'$", "", args)
    seen$args <- plain
    files <- sub("^@", "", grep("^@", plain[which(plain == "-H") + 1L], value = TRUE))
    seen$header_lines <- unlist(lapply(files, readLines, warn = FALSE))
    writeLines(c(sprintf("HTTP/2 %d", status), ""), plain[match("-D", plain) + 1L])
    writeLines(body, plain[match("-o", plain) + 1L], sep = "")
    as.character(status)
  }
}

test_that("the io member posts to the domain's API base with the GitLab timeout and its token", {
  withr::local_envvar(GITLAB_TOKEN_FORGEMIA_INRA_FR = "s3cret")
  seen <- new.env()
  r <- gl_graphql("forgemia.inra.fr", "q", list(keys = list("a/b")),
                  run = gl_fake_curl('{"data": {"projects": null}}', seen))
  expect_identical(utils::tail(seen$args, 1), "https://forge.inrae.fr/api/graphql")
  expect_identical(seen$args[match("--max-time", seen$args) + 1L], as.character(HTTP_TIMEOUT_GITLAB_S))
  expect_identical(HTTP_TIMEOUT_GITLAB_S, 30)
  expect_identical(seen$header_lines, "Authorization: Bearer s3cret")
  expect_false(any(grepl("s3cret", seen$args)))
  expect_true(is.na(r$error))
})

test_that("an HTML answer comes back as an error", {
  withr::local_envvar(GITLAB_TOKEN_CODE_USGS_GOV = NA)
  seen <- new.env()
  r <- gl_graphql("code.usgs.gov", "q", list(keys = list("a/b")),
                  run = gl_fake_curl("<html><body>Sign in</body></html>", seen))
  expect_null(r$data)
  expect_identical(r$error, "response is not JSON")
  expect_length(seen$header_lines, 0L)
})

test_that("gl_parse_projects gives exactly the gauge columns plus full_path and gl_id", {
  df <- gl_parse_projects(gl_answer(gl_fixture("gitlab_com.json")), "gitlab.com")
  expect_identical(names(df), gl_frame_names())
  empty <- rows_df_empty_gauges()
  for (col in names(empty)) expect_identical(class(df[[col]]), class(empty[[col]]), info = col)
  none <- gl_parse_projects(list(data = list(projects = list(nodes = list()))), "gitlab.com")
  expect_identical(names(none), gl_frame_names())
  expect_equal(nrow(none), 0L)
  expect_identical(names(gl_parse_projects(list(data = NULL, error = "HTTP 502"), "gitlab.com")), gl_frame_names())
  for (col in names(empty)) expect_identical(class(none[[col]]), class(empty[[col]]), info = col)
})

test_that("gl_parse_projects maps each GitLab field to its gauge column", {
  df <- gl_parse_projects(gl_answer(gl_fixture("gitlab_com.json")), "gitlab.com")
  expect_equal(nrow(df), 3L)
  a <- df[df$full_path == "bart-turczynski/rurl", ]
  expect_identical(a$node_id, "gitlab:gitlab.com:85027322")
  expect_identical(a$gl_id, "gid://gitlab/Project/85027322")
  expect_identical(a$name_with_owner, "bart-turczynski/rurl")
  for (col in c("owner_login", "owner_type", "owner_node_id", "license")) expect_identical(a[[col]], NA_character_, info = col)
  for (col in c("watchers", "size_kb", "is_mirror")) expect_identical(a[[col]], NA_integer_, info = col)
  expect_identical(a$stars, 7L); expect_identical(a$forks, 3L)
  expect_identical(a$issues_open, 0L); expect_identical(a$issues_closed, 0L)
  expect_identical(a$prs_open, 2L); expect_identical(a$prs_closed, 3L); expect_identical(a$prs_merged, 238L)
  expect_identical(a$releases_total, 2L)
  expect_identical(a$topics, "domain-name-checker,url-parsing")
  expect_identical(a$is_archived, 0L); expect_identical(a$is_fork, 0L)
  expect_identical(a$created_at, "2026-08-01T15:08:14Z")
  expect_identical(a$pushed_at, "2026-10-12T20:33:30Z")
  expect_identical(a$last_release_at, "2026-10-02T15:39:10Z")
  expect_identical(a$last_release_tag, "v3.1.0")
  # The committer's offset is turned into UTC, as GitHub's dates are.
  expect_identical(a$head_committed_at, "2026-10-12T20:40:43Z")
  b <- df[df$full_path == "cnrgh/databases/r-sqlq", ]
  expect_identical(b$issues_open, 1L); expect_identical(b$issues_closed, 35L)
  expect_identical(b$prs_closed, 1L); expect_identical(b$prs_merged, 31L)
  expect_identical(b$topics, "")
  expect_identical(b$releases_total, 0L)
  expect_identical(b$last_release_at, NA_character_); expect_identical(b$last_release_tag, NA_character_)
  c3 <- df[df$full_path == "fascinatingfingers/checkdigit", ]
  expect_identical(c3$is_archived, 1L)
  expect_identical(c3$head_committed_at, "2022-04-24T17:39:33Z")
  u <- gl_parse_projects(gl_answer(gl_fixture("code_usgs_gov.json")), "code.usgs.gov")
  expect_identical(u$node_id, "gitlab:code.usgs.gov:4520")
  expect_identical(u$forks, 2L); expect_identical(u$releases_total, 10L)
  expect_identical(u$head_committed_at, "2026-07-06T22:14:50Z")
})

test_that("a project with no releases, empty topics and an empty repository", {
  df <- gl_parse_projects(gl_answer(gl_fixture("bare.json")), "gitlab.in2p3.fr")
  expect_equal(nrow(df), 1L)
  expect_identical(df$node_id, "gitlab:gitlab.in2p3.fr:31477")
  expect_identical(df$releases_total, 0L)
  expect_identical(df$last_release_at, NA_character_); expect_identical(df$last_release_tag, NA_character_)
  expect_identical(df$topics, "")
  expect_identical(df$head_committed_at, NA_character_)
  # issueStatusCounts null: closed issues not readable, so unknown.
  expect_identical(df$issues_closed, NA_integer_); expect_identical(df$issues_open, 0L)
  expect_identical(df$created_at, "2021-02-15T09:21:37Z")
  expect_identical(df$stars, 1L)
})

test_that("disabled issues and merge requests are unknown, not zero", {
  df <- gl_parse_projects(gl_answer(gl_fixture("disabled.json")), "gitlab.com")
  for (col in c("issues_open", "issues_closed", "prs_open", "prs_closed", "prs_merged"))
    expect_identical(df[[col]], NA_integer_, info = col)
  expect_identical(df$stars, 2L)
  expect_identical(df$is_fork, 1L)
  expect_identical(df$name_with_owner, "Example-User/Boruta")
})

test_that("a null field is NA, never 0 or empty, and a node without a project id is dropped", {
  nodes <- list(
    list(id = "gid://gitlab/Project/5", fullPath = "a/b", starCount = NULL, forksCount = 4L, archived = NULL,
         isForked = NULL, topics = NULL, createdAt = NULL, issuesEnabled = NULL, openIssuesCount = 3L,
         issueStatusCounts = NULL, releases = NULL, repository = NULL),
    NULL,
    list(id = NULL, fullPath = "a/c"),
    list(id = "gid://gitlab/Group/7", fullPath = "a"))
  df <- gl_parse_projects(list(data = list(projects = list(nodes = nodes))), "gitlab.com")
  expect_equal(nrow(df), 1L)
  expect_identical(df$stars, NA_integer_); expect_identical(df$forks, 4L)
  expect_identical(df$is_archived, NA_integer_); expect_identical(df$is_fork, NA_integer_)
  expect_identical(df$topics, NA_character_)
  expect_identical(df$created_at, NA_character_)
  expect_identical(df$issues_open, 3L)
  expect_identical(df$releases_total, NA_integer_)
  expect_identical(df$head_committed_at, NA_character_)
})

test_that("gl_resolve matches two projects returned out of order by path", {
  seen <- new.env()
  io <- fake_gl_io(list(gitlab.com = gl_pool_of("gitlab_com.json")), seen)
  f <- fake_gl_pacer()
  repos <- gl_repos("gitlab.com/cnrgh/databases/r-sqlq", "gitlab.com/bart-turczynski/rurl")
  got <- gl_resolve(io, repos, f$p)
  expect_identical(names(got), c("repo_id", "node_id", "owner", "name", "name_with_owner", "status"))
  expect_length(seen$calls, 1L)
  expect_match(seen$calls[[1]]$query, "fullPaths: $keys", fixed = TRUE)
  expect_identical(seen$calls[[1]]$variables, list(keys = list("cnrgh/databases/r-sqlq", "bart-turczynski/rurl")))
  q <- got[got$repo_id == "gitlab.com/cnrgh/databases/r-sqlq", ]
  expect_identical(q$node_id, "gitlab:gitlab.com:60407353")
  expect_identical(q$owner, "cnrgh/databases"); expect_identical(q$name, "r-sqlq")
  expect_identical(q$name_with_owner, "cnrgh/databases/r-sqlq"); expect_identical(q$status, "active")
  r <- got[got$repo_id == "gitlab.com/bart-turczynski/rurl", ]
  expect_identical(r$node_id, "gitlab:gitlab.com:85027322")
  expect_identical(r$owner, "bart-turczynski"); expect_identical(r$name, "rurl")
})

test_that("gl_resolve leaves out a project the answer does not carry, and never marks it gone", {
  io <- fake_gl_io(list(gitlab.com = gl_pool_of("gitlab_com.json")))
  got <- gl_resolve(io, gl_repos("gitlab.com/hrbrmstr/wand", "gitlab.com/fascinatingfingers/checkdigit"), fake_gl_pacer()$p)
  expect_identical(got$repo_id, "gitlab.com/fascinatingfingers/checkdigit")
  expect_false(any(got$status == "gone"))
  expect_equal(nrow(gl_resolve(io, gl_repos("gitlab.com/hrbrmstr/wand"), fake_gl_pacer()$p)), 0L)
})

test_that("gl_resolve matches a lower-cased path to the answer's case and keeps the answer's spelling", {
  io <- fake_gl_io(list(gitlab.com = gl_pool_of("disabled.json")))
  got <- gl_resolve(io, gl_repos("gitlab.com/example-user/boruta"), fake_gl_pacer()$p)
  expect_identical(got$node_id, "gitlab:gitlab.com:9133744")
  expect_identical(got$name_with_owner, "Example-User/Boruta")
  expect_identical(got$owner, "Example-User"); expect_identical(got$name, "Boruta")
})

test_that("a failed resolve batch contributes no rows and the other host still resolves", {
  seen <- new.env(); ps <- new.env()
  html <- list(data = NULL, errors = NULL, status = 200L, error = "response is not JSON")
  io <- fake_gl_io(list(gitlab.com = gl_pool_of("gitlab_com.json"), code.usgs.gov = gl_pool_of("code_usgs_gov.json")),
                   seen, answer = function(domain, variables) if (domain == "code.usgs.gov") html)
  p <- recording_pacer(fake_gl_pacer()$p, ps)
  got <- gl_resolve(io, gl_repos("code.usgs.gov/inl/inlpubs", "gitlab.com/bart-turczynski/rurl"), p)
  expect_identical(got$repo_id, "gitlab.com/bart-turczynski/rurl")
  expect_identical(ps$after, c("code.usgs.gov FALSE", "gitlab.com TRUE"))
})

test_that("gl_resolve asks 50 paths per request, grouped by domain", {
  seen <- new.env()
  pool <- gl_synthetic(120)
  io <- fake_gl_io(list(gitlab.com = pool, git.ufz.de = gl_synthetic(1, 500L)), seen)
  repos <- gl_repos(c(sprintf("gitlab.com/g/p%d", 1:120), "git.ufz.de/g/p500"))
  got <- gl_resolve(io, repos, fake_gl_pacer()$p)
  expect_equal(nrow(got), 121L)
  expect_identical(vapply(seen$calls, function(x) x$domain, ""), c(rep("gitlab.com", 3), "git.ufz.de"))
  expect_identical(vapply(seen$calls, function(x) length(x$variables$keys), 0L), c(50L, 50L, 20L, 1L))
  expect_identical(got$node_id[got$repo_id == "git.ufz.de/g/p500"], "gitlab:git.ufz.de:500")
})

test_that("gl_collect_gauges matches projects by id and reports a missing one as unresolvable", {
  seen <- new.env()
  io <- fake_gl_io(list(gitlab.com = gl_pool_of("gitlab_com.json")), seen)
  ids <- c("gitlab:gitlab.com:60407353", "gitlab:gitlab.com:85027322", "gitlab:gitlab.com:999")
  got <- gl_collect_gauges(io, gl_map(ids), fake_gl_pacer()$p)
  expect_identical(names(got), c("snapshot", "deferred", "unresolvable", "unreached"))
  expect_length(seen$calls, 1L)
  expect_match(seen$calls[[1]]$query, "ids: $keys", fixed = TRUE)
  expect_identical(seen$calls[[1]]$variables$keys,
                   list("gid://gitlab/Project/60407353", "gid://gitlab/Project/85027322", "gid://gitlab/Project/999"))
  expect_identical(names(got$snapshot), gl_frame_names())
  expect_setequal(got$snapshot$node_id, ids[1:2])
  expect_identical(got$snapshot$issues_closed[got$snapshot$node_id == ids[1]], 35L)
  expect_identical(got$snapshot$prs_merged[got$snapshot$node_id == ids[2]], 238L)
  expect_identical(got$unresolvable, "gitlab:gitlab.com:999")
  expect_identical(got$deferred, character(0)); expect_identical(got$unreached, character(0))
})

test_that("one key still goes out as a JSON array", {
  seen <- new.env()
  io <- fake_gl_io(list(code.usgs.gov = gl_pool_of("code_usgs_gov.json")), seen)
  got <- gl_collect_gauges(io, gl_map("gitlab:code.usgs.gov:4520"), fake_gl_pacer()$p)
  expect_identical(as.character(jsonlite::toJSON(seen$calls[[1]]$variables, auto_unbox = TRUE)),
                   '{"keys":["gid://gitlab/Project/4520"]}')
  expect_identical(got$snapshot$node_id, "gitlab:code.usgs.gov:4520")
})

test_that("an HTML or transport error defers the batch and counts as a failure", {
  ps <- new.env(); logged <- new.env(); logged$lines <- character()
  html <- list(data = NULL, errors = NULL, status = 200L, error = "response is not JSON")
  io <- fake_gl_io(list(), answer = function(domain, variables) html)
  p <- recording_pacer(fake_gl_pacer()$p, ps)
  ids <- c("gitlab:code.usgs.gov:4520", "gitlab:code.usgs.gov:4521")
  got <- gl_collect_gauges(io, gl_map(ids), p, log = function(m) logged$lines <- c(logged$lines, m))
  expect_null(got$snapshot)
  expect_identical(got$deferred, ids)
  expect_identical(got$unresolvable, character(0))
  expect_identical(ps$after, "code.usgs.gov FALSE")
  expect_match(logged$lines, "code.usgs.gov.*response is not JSON")
  thrown <- fake_gl_io(list(), answer = function(domain, variables) stop("boom"))
  expect_identical(gl_collect_gauges(thrown, gl_map(ids), fake_gl_pacer()$p)$deferred, ids)
})

test_that("a GraphQL errors array without data defers the batch and counts as a failure", {
  ps <- new.env()
  io <- fake_gl_io(list(), answer = function(domain, variables) gl_answer(gl_fixture("errors.json")))
  p <- recording_pacer(fake_gl_pacer()$p, ps)
  ids <- "gitlab:gitlab.com:85027322"
  got <- gl_collect_gauges(io, gl_map(ids), p)
  expect_null(got$snapshot)
  expect_identical(got$deferred, ids)
  expect_identical(ps$after, "gitlab.com FALSE")
  expect_equal(nrow(gl_resolve(io, gl_repos("gitlab.com/bart-turczynski/rurl"), p)), 0L)
})

test_that("an answer with errors keeps the projects it read and defers, not drops, the rest", {
  partial <- gl_answer(gl_fixture("gitlab_com.json"))
  partial$data$projects$nodes <- partial$data$projects$nodes[1]
  partial$errors <- list(list(message = "Internal server error", path = list("projects", "nodes", 1L)))
  io <- fake_gl_io(list(), answer = function(domain, variables) partial)
  ids <- c("gitlab:gitlab.com:85027322", "gitlab:gitlab.com:60407353")
  got <- gl_collect_gauges(io, gl_map(ids), fake_gl_pacer()$p)
  expect_identical(got$snapshot$node_id, ids[1])
  expect_identical(got$deferred, ids[2])
  expect_identical(got$unresolvable, character(0))
})

test_that("requests to one host are 2 s apart while another host does not wait", {
  f <- fake_gl_pacer()
  seen <- new.env()
  io <- fake_gl_io(list(gitlab.com = gl_synthetic(120), code.usgs.gov = gl_pool_of("code_usgs_gov.json")),
                   seen, clock = f$clock)
  ids <- c("gitlab:code.usgs.gov:4520", sprintf("gitlab:gitlab.com:%d", 1:120))
  got <- gl_collect_gauges(io, gl_map(ids), f$p, now = function() f$clock$t)
  expect_equal(nrow(got$snapshot), 121L)
  at <- vapply(seen$calls, function(x) x$at, 0) - 3600 * 100
  dom <- vapply(seen$calls, function(x) x$domain, "")
  expect_identical(dom, c("code.usgs.gov", rep("gitlab.com", 3)))
  expect_equal(at, c(0, 0, 2, 4))
  expect_equal(f$clock$slept, c(2, 2))
  expect_identical(GITLAB_MIN_INTERVAL_S, 2)
})

test_that("a host gets at most 30 requests in a run, even across the clock hour", {
  f <- fake_gl_pacer()
  seen <- new.env()
  io <- fake_gl_io(list(gitlab.com = gl_synthetic(1551)), seen, clock = f$clock, cost = 130)
  ids <- sprintf("gitlab:gitlab.com:%d", 1:1551)
  got <- gl_collect_gauges(io, gl_map(ids), f$p, now = function() f$clock$t)
  expect_length(seen$calls, 30L)
  expect_identical(GITLAB_MAX_REQUESTS, 30L)
  expect_gt(floor(f$clock$t / 3600), 100)
  expect_equal(nrow(got$snapshot), 1500L)
  expect_identical(got$deferred, ids[1501:1551])
  expect_identical(got$unresolvable, character(0))
  # The same pacer holds the run's budget for the next stage too.
  more <- gl_resolve(io, gl_repos("gitlab.com/g/p1"), f$p)
  expect_equal(nrow(more), 0L)
  expect_length(seen$calls, 30L)
})

test_that("a host that fails 3 times is deferred while the other host is collected", {
  f <- fake_gl_pacer()
  seen <- new.env()
  slow <- list(data = NULL, errors = NULL, status = NA_integer_, error = "curl timed out (exit 28)")
  io <- fake_gl_io(list(gitlab.com = gl_pool_of("gitlab_com.json")), seen,
                   answer = function(domain, variables) if (domain == "git.nomics.world") slow)
  bad <- sprintf("gitlab:git.nomics.world:%d", 1:160)
  good <- c("gitlab:gitlab.com:85027322", "gitlab:gitlab.com:11251956")
  got <- gl_collect_gauges(io, gl_map(c(bad, good)), f$p, now = function() f$clock$t)
  dom <- vapply(seen$calls, function(x) x$domain, "")
  expect_equal(sum(dom == "git.nomics.world"), 3L)
  expect_identical(GITLAB_BREAKER_AFTER, 3L)
  expect_identical(f$p$deferred(), "git.nomics.world")
  expect_identical(got$deferred, bad)
  expect_setequal(got$snapshot$node_id, good)
  expect_identical(got$unresolvable, character(0)); expect_identical(got$unreached, character(0))
})

test_that("what the deadline cuts off is unreached, across hosts", {
  f <- fake_gl_pacer()
  io <- fake_gl_io(list(gitlab.com = gl_synthetic(60), code.usgs.gov = gl_pool_of("code_usgs_gov.json")),
                   clock = f$clock, cost = 10)
  ids <- c(sprintf("gitlab:gitlab.com:%d", 1:60), "gitlab:code.usgs.gov:4520")
  got <- gl_collect_gauges(io, gl_map(ids), f$p, deadline = f$clock$t + 5, now = function() f$clock$t)
  expect_equal(nrow(got$snapshot), 50L)
  expect_identical(got$unreached, ids[51:61])
  expect_identical(got$deferred, character(0))
})

test_that("nothing to collect or resolve asks nothing", {
  seen <- new.env()
  io <- fake_gl_io(list(), seen)
  got <- gl_collect_gauges(io, gl_map(character(0)), fake_gl_pacer()$p)
  expect_identical(got, list(snapshot = NULL, deferred = character(0), unresolvable = character(0),
                             unreached = character(0)))
  expect_equal(nrow(gl_resolve(io, gl_repos(character(0)), fake_gl_pacer()$p)), 0L)
  expect_length(seen$calls, 0L)
})

test_that("forgemia.inra.fr and forge.inrae.fr are one server, so they share one interval and one budget", {
  f <- fake_gl_pacer()
  seen <- new.env()
  io <- fake_gl_io(list(forgemia.inra.fr = gl_synthetic(1), forge.inrae.fr = gl_synthetic(1, 2L)), seen, clock = f$clock)
  got <- gl_collect_gauges(io, gl_map(c("gitlab:forgemia.inra.fr:1", "gitlab:forge.inrae.fr:2")), f$p,
                           now = function() f$clock$t)
  expect_equal(nrow(got$snapshot), 2L)
  # The io is still asked by the URL's domain; only the pacing is shared.
  expect_identical(vapply(seen$calls, function(x) x$domain, ""), c("forgemia.inra.fr", "forge.inrae.fr"))
  expect_equal(vapply(seen$calls, function(x) x$at, 0) - 3600 * 100, c(0, 2))
  expect_equal(f$p$spent("forge.inrae.fr"), 2L)
  expect_equal(f$p$spent("forgemia.inra.fr"), 0L)
})

test_that("failures on forgemia.inra.fr defer forge.inrae.fr too", {
  f <- fake_gl_pacer()
  seen <- new.env(); logged <- new.env(); logged$lines <- character()
  slow <- list(data = NULL, errors = NULL, status = NA_integer_, error = "curl timed out (exit 28)")
  io <- fake_gl_io(list(forge.inrae.fr = gl_synthetic(1, 7L)), seen,
                   answer = function(domain, variables) if (domain == "forgemia.inra.fr") slow)
  bad <- sprintf("gitlab:forgemia.inra.fr:%d", 101:250)
  got <- gl_collect_gauges(io, gl_map(c(bad, "gitlab:forge.inrae.fr:7")), f$p, now = function() f$clock$t,
                           log = function(m) logged$lines <- c(logged$lines, m))
  expect_length(seen$calls, 3L)
  expect_identical(f$p$deferred(), "forge.inrae.fr")
  expect_identical(got$deferred, c(bad, "gitlab:forge.inrae.fr:7"))
  expect_null(got$snapshot)
  expect_true(any(grepl("forge.inrae.fr: deferred after repeated failures, 1 left", logged$lines, fixed = TRUE)))
  expect_equal(nrow(gl_resolve(io, gl_repos("forge.inrae.fr/g/p7"), f$p)), 0L)
  expect_length(seen$calls, 3L)
})

test_that("gl_collect_gauges asks each project's own host, read from its node id", {
  seen <- new.env()
  io <- fake_gl_io(list(code.usgs.gov = gl_pool_of("code_usgs_gov.json")), seen)
  m <- gl_map("gitlab:code.usgs.gov:4520")
  m$domain <- "gitlab.com"
  got <- gl_collect_gauges(io, m, fake_gl_pacer()$p)
  expect_identical(seen$calls[[1]]$domain, "code.usgs.gov")
  expect_identical(got$snapshot$node_id, "gitlab:code.usgs.gov:4520")
  expect_identical(got$unresolvable, character(0))
  m$domain <- NULL
  expect_identical(gl_collect_gauges(io, m, fake_gl_pacer()$p)$snapshot$node_id, "gitlab:code.usgs.gov:4520")
  # An id that is not a GitLab node id is never asked and keeps its values.
  n <- length(seen$calls)
  odd <- gl_collect_gauges(io, data.frame(node_id = "R_abc", repo_id = "r1", stringsAsFactors = FALSE), fake_gl_pacer()$p)
  expect_length(seen$calls, n)
  expect_identical(odd$deferred, "R_abc")
  expect_identical(odd$unresolvable, character(0))
})

# rurl with upcoming releases ahead of the ones that are out.
gl_with_releases <- function(nodes, count = length(nodes)) {
  n <- gl_pool_of("gitlab_com.json")[[1]]
  n$releases <- list(count = count, nodes = nodes)
  list(data = list(projects = list(nodes = list(n))))
}
gl_rel <- function(tag, at, upcoming = NULL) list(tagName = tag, releasedAt = at, upcomingRelease = upcoming)

test_that("the last release is the newest one that is out, never an upcoming one", {
  now <- as.numeric(as.POSIXct("2026-10-15 00:00:00", tz = "UTC"))
  resp <- gl_with_releases(list(gl_rel("v4.0.0", "2027-03-01T00:00:00Z", TRUE),
                                gl_rel("v3.2.0", "2026-10-20T00:00:00Z"),
                                gl_rel("v3.1.0", "2026-10-02T15:39:10Z", FALSE),
                                gl_rel("v3.0.1", "2026-09-09T16:00:01Z", FALSE)))
  df <- gl_parse_projects(resp, "gitlab.com", now = now)
  expect_identical(df$last_release_tag, "v3.1.0")
  expect_identical(df$last_release_at, "2026-10-02T15:39:10Z")
  # The two upcoming releases are not counted.
  expect_identical(df$releases_total, 2L)
  # Flagged upcoming is skipped whatever the clock says.
  flagged <- gl_with_releases(list(gl_rel("v3.2.0", "2026-10-01T00:00:00Z", TRUE), gl_rel("v3.1.0", "2026-09-02T00:00:00Z", FALSE)))
  expect_identical(gl_parse_projects(flagged, "gitlab.com", now = now)$last_release_tag, "v3.1.0")
  expect_identical(gl_parse_projects(flagged, "gitlab.com", now = now)$releases_total, 1L)
  none <- gl_parse_projects(gl_with_releases(list(gl_rel("v9", "2027-01-01T00:00:00Z", TRUE),
                                                  gl_rel("v8", "2026-12-01T00:00:00Z", TRUE))),
                            "gitlab.com", now = now)
  expect_identical(none$last_release_tag, NA_character_)
  expect_identical(none$last_release_at, NA_character_)
  # Every release was fetched and every one is upcoming, so none is out.
  expect_identical(none$releases_total, 0L)
})

test_that("releases are unknown when every fetched release is upcoming and more exist", {
  now <- as.numeric(as.POSIXct("2026-10-15 00:00:00", tz = "UTC"))
  five <- lapply(1:5, function(i) gl_rel(sprintf("v9.%d", i), "2027-01-01T00:00:00Z", TRUE))
  df <- gl_parse_projects(gl_with_releases(five, count = 9L), "gitlab.com", now = now)
  expect_identical(df$releases_total, NA_integer_)
  expect_identical(df$last_release_tag, NA_character_)
  # Four upcoming among five fetched of nine: five are out.
  some <- c(five[1:4], list(gl_rel("v1.0", "2025-01-01T00:00:00Z", FALSE)))
  expect_identical(gl_parse_projects(gl_with_releases(some, count = 9L), "gitlab.com", now = now)$releases_total, 5L)
  expect_identical(gl_parse_projects(gl_with_releases(list(), count = 0L), "gitlab.com", now = now)$releases_total, 0L)
})

test_that("gl_collect_gauges judges upcoming releases by its own clock", {
  resp <- gl_with_releases(list(gl_rel("v3.2.0", "2026-10-20T00:00:00Z"), gl_rel("v3.1.0", "2026-10-02T15:39:10Z")))
  io <- fake_gl_io(list(), answer = function(domain, variables) c(resp, list(status = 200L, error = NA_character_)))
  at <- function(day) { t <- as.numeric(as.POSIXct(day, tz = "UTC")); function() t }
  ids <- "gitlab:gitlab.com:85027322"
  before <- gl_collect_gauges(io, gl_map(ids), fake_gl_pacer()$p, now = at("2026-10-15"))
  expect_identical(before$snapshot$last_release_tag, "v3.1.0")
  expect_identical(before$snapshot$releases_total, 1L)
  after <- gl_collect_gauges(io, gl_map(ids), fake_gl_pacer()$p, now = at("2026-10-21"))
  expect_identical(after$snapshot$last_release_tag, "v3.2.0")
  expect_identical(after$snapshot$last_release_at, "2026-10-20T00:00:00Z")
  expect_identical(after$snapshot$releases_total, 2L)
})

test_that("gl_resolve asks nothing once its deadline has passed, and those paths stay for the next run", {
  f <- fake_gl_pacer()
  seen <- new.env()
  io <- fake_gl_io(list(gitlab.com = gl_synthetic(60), code.usgs.gov = gl_pool_of("code_usgs_gov.json")),
                   seen, clock = f$clock, cost = 10)
  repos <- gl_repos(c(sprintf("gitlab.com/g/p%d", 1:60), "code.usgs.gov/inl/inlpubs"))
  got <- gl_resolve(io, repos, f$p, deadline = f$clock$t + 5, now = function() f$clock$t)
  expect_length(seen$calls, 1L)
  expect_equal(nrow(got), 50L)
  expect_false("code.usgs.gov/inl/inlpubs" %in% got$repo_id)
  expect_false(any(got$status == "gone"))
})
