# GitLab weekly collectors: commit totals over GraphQL, contributors and licence over REST.
# Contributor rows are trimmed from the recon samples with the identities replaced.

gl_contrib_row <- function(commits) list(list(name = "Person", email = "person@example.invalid",
                                              commits = commits, additions = 0L, deletions = 0L))

# A REST answer in http_request's shape.
gl_rest_answer <- function(status = 200L, body = list(), headers = character()) {
  list(status = status, headers = headers, error = if (status >= 400L) sprintf("HTTP %d", status) else NA_character_,
       body = as.character(jsonlite::toJSON(body, auto_unbox = TRUE)))
}

# A fake io whose gl_rest runs `answer(domain, path)` and keeps every path asked.
fake_rest_io <- function(answer, seen = new.env()) {
  seen$paths <- character()
  list(gl_rest = function(domain, path) {
    seen$paths <- c(seen$paths, paste0(domain, path))
    answer(domain, path)
  })
}

fake_rest_pacer <- function(start = 3600 * 100) {
  clock <- new.env()
  clock$t <- start
  p <- gl_rest_pacer(now = function() clock$t, sleep = function(s) clock$t <- clock$t + s)
  list(p = p, clock = clock)
}

contrib_answer <- function(total = "5", top = 444L, extra = character()) function(domain, path) {
  if (grepl("order_by=commits", path, fixed = TRUE)) gl_rest_answer(body = gl_contrib_row(top), headers = extra)
  else gl_rest_answer(body = gl_contrib_row(4L), headers = c(`x-total` = total))
}

test_that("contributors: x-total gives the count and the descending page gives the top commits", {
  seen <- new.env()
  io <- fake_rest_io(contrib_answer("5", 444L), seen)
  r <- gl_contributors(io, "gitlab.com", 32292794, fake_rest_pacer()$p)
  expect_identical(r, list(total = 5L, top_commits = 444L, stopped = FALSE, read = TRUE))
  expect_identical(seen$paths, c(
    "gitlab.com/projects/32292794/repository/contributors?per_page=1",
    "gitlab.com/projects/32292794/repository/contributors?order_by=commits&sort=desc&per_page=1"))
  expect_false(any(grepl("@", unlist(r))))
})

test_that("contributors: a missing x-total is NA and the top commits are still read", {
  io <- fake_rest_io(function(domain, path) {
    if (grepl("order_by", path)) gl_rest_answer(body = gl_contrib_row(505L)) else gl_rest_answer(body = gl_contrib_row(1L))
  })
  r <- gl_contributors(io, "code.usgs.gov", 4520, fake_rest_pacer()$p)
  expect_identical(r, list(total = NA_integer_, top_commits = 505L, stopped = FALSE, read = TRUE))
})

test_that("contributors: an empty list has a zero count and no top commits", {
  io <- fake_rest_io(function(domain, path) gl_rest_answer(body = list(), headers = c(`x-total` = "0")))
  r <- gl_contributors(io, "gitlab.com", 1, fake_rest_pacer()$p)
  expect_identical(r, list(total = 0L, top_commits = NA_integer_, stopped = FALSE, read = TRUE))
})

test_that("contributors: a 429 stops the host with no values, and later calls are refused without a request", {
  seen <- new.env()
  io <- fake_rest_io(function(domain, path) gl_rest_answer(429L, headers = c(`retry-after` = "30")), seen)
  f <- fake_rest_pacer()
  r <- gl_contributors(io, "gitlab.com", 1, f$p)
  expect_identical(r, list(total = NA_integer_, top_commits = NA_integer_, stopped = TRUE, read = FALSE))
  expect_length(seen$paths, 1L)
  r2 <- gl_contributors(io, "gitlab.com", 2, f$p)
  expect_true(r2$stopped)
  expect_length(seen$paths, 1L)
  expect_identical(f$p$deferred(), character())
  # Another host is untouched.
  expect_false(gl_contributors(fake_rest_io(contrib_answer()), "code.usgs.gov", 3, f$p)$stopped)
})

test_that("contributors: ratelimit-remaining 0 keeps the values read and stops the host", {
  seen <- new.env()
  io <- fake_rest_io(function(domain, path) gl_rest_answer(
    body = gl_contrib_row(4L), headers = c(`x-total` = "5", `ratelimit-remaining` = "0")), seen)
  f <- fake_rest_pacer()
  r <- gl_contributors(io, "gitlab.com", 1, f$p)
  expect_identical(r, list(total = 5L, top_commits = NA_integer_, stopped = TRUE, read = FALSE))
  expect_length(seen$paths, 1L)
  expect_true(gl_contributors(io, "gitlab.com", 2, f$p)$stopped)
  expect_length(seen$paths, 1L)
  # On the second request the top row is kept too.
  io2 <- fake_rest_io(contrib_answer("7", 90L, extra = c(`ratelimit-remaining` = "0")))
  r2 <- gl_contributors(io2, "git.example.org", 1, fake_rest_pacer()$p)
  expect_identical(r2, list(total = 7L, top_commits = 90L, stopped = TRUE, read = TRUE))
})

test_that("contributors: a spent hourly budget is a stopped call, and a failure is not read and counts toward the breaker", {
  io <- fake_rest_io(contrib_answer())
  f <- fake_rest_pacer()
  f$p$set_hourly_budget("gitlab.com", 1L)
  expect_true(gl_contributors(io, "gitlab.com", 1, f$p)$stopped)
  bad <- fake_rest_io(function(domain, path) gl_rest_answer(500L))
  g <- fake_rest_pacer()
  r <- gl_contributors(bad, "gitlab.com", 1, g$p)
  expect_identical(r, list(total = NA_integer_, top_commits = NA_integer_, stopped = FALSE, read = FALSE))
  expect_identical(gl_license(bad, "gitlab.com", 1, fake_rest_pacer()$p), list(key = NA_character_, stopped = FALSE, read = FALSE))
  for (i in 1:5) gl_contributors(bad, "gitlab.com", i, g$p)
  expect_identical(g$p$deferred(), "gitlab.com")
})

test_that("a 403, 404 or 410 is an answer about the project: NA values, read, not stopped, never a host failure", {
  for (code in c(403L, 404L, 410L)) {
    seen <- new.env()
    io <- fake_rest_io(function(domain, path) gl_rest_answer(code), seen)
    f <- fake_rest_pacer()
    for (i in 1:6) {
      expect_identical(gl_contributors(io, "gitlab.com", i, f$p), list(total = NA_integer_, top_commits = NA_integer_, stopped = FALSE, read = TRUE))
      expect_identical(gl_license(io, "gitlab.com", i, f$p), list(key = NA_character_, stopped = FALSE, read = TRUE))
    }
    expect_identical(f$p$deferred(), character())
    expect_length(seen$paths, 12L)
  }
  # Later projects on the same host are still read.
  answer <- function(domain, path) if (grepl("/projects/1/", path, fixed = TRUE)) gl_rest_answer(404L) else contrib_answer("3", 8L)(domain, path)
  f <- fake_rest_pacer()
  for (i in c(1, 1, 1)) gl_contributors(fake_rest_io(answer), "gitlab.com", i, f$p)
  expect_identical(gl_contributors(fake_rest_io(answer), "gitlab.com", 2, f$p), list(total = 3L, top_commits = 8L, stopped = FALSE, read = TRUE))
})

test_that("the REST pacer budgets 45 for gitlab.com, 30 elsewhere and 500 with a token", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = NA, GITLAB_TOKEN_CODE_USGS_GOV = "x")
  allowed <- function(p, host) { n <- 0L; while (isTRUE(p$before(host)) && n < 600L) n <- n + 1L; n }
  expect_identical(GITLAB_REST_HOURLY, c(gitlab.com = 45L, default = 30L))
  expect_identical(GITLAB_REST_HOURLY_TOKEN, 500L)
  expect_equal(allowed(fake_rest_pacer()$p, "gitlab.com"), 45L)
  expect_equal(allowed(fake_rest_pacer()$p, "git.example.org"), 30L)
  expect_equal(allowed(fake_rest_pacer()$p, "code.usgs.gov"), 500L)
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = "t")
  expect_equal(allowed(fake_rest_pacer()$p, "gitlab.com"), 500L)
})

test_that("licence: the key comes from license.key, null is NA, a refusal is stopped", {
  seen <- new.env()
  io <- fake_rest_io(function(domain, path) gl_rest_answer(body = list(id = 24125720L, license = list(key = "cc-by-nc-4.0"))), seen)
  expect_identical(gl_license(io, "gitlab.com", 24125720, fake_rest_pacer()$p), list(key = "cc-by-nc-4.0", stopped = FALSE, read = TRUE))
  expect_identical(seen$paths, "gitlab.com/projects/24125720?license=true")
  none <- fake_rest_io(function(domain, path) gl_rest_answer(body = list(id = 1L, license = NULL)))
  expect_identical(gl_license(none, "gitlab.com", 1, fake_rest_pacer()$p), list(key = NA_character_, stopped = FALSE, read = TRUE))
  limited <- fake_rest_io(function(domain, path) gl_rest_answer(429L))
  expect_identical(gl_license(limited, "gitlab.com", 1, fake_rest_pacer()$p), list(key = NA_character_, stopped = TRUE, read = FALSE))
  ok0 <- fake_rest_io(function(domain, path) gl_rest_answer(body = list(license = list(key = "mit")),
                                                            headers = c(`ratelimit-remaining` = "0")))
  expect_identical(gl_license(ok0, "gitlab.com", 1, fake_rest_pacer()$p), list(key = "mit", stopped = TRUE, read = TRUE))
})

test_that("licence keys map to GitHub's SPDX spelling and anything else is NA", {
  keys <- c("mit", "apache-2.0", "gpl-2.0", "gpl-3.0", "lgpl-2.1", "lgpl-3.0", "agpl-3.0", "bsd-2-clause",
            "bsd-3-clause", "mpl-2.0", "cc0-1.0", "unlicense", "epl-2.0", "isc", "cc-by-4.0", "cc-by-sa-4.0")
  expect_identical(gl_license_spdx(keys), c("MIT", "Apache-2.0", "GPL-2.0", "GPL-3.0", "LGPL-2.1", "LGPL-3.0",
    "AGPL-3.0", "BSD-2-Clause", "BSD-3-Clause", "MPL-2.0", "CC0-1.0", "Unlicense", "EPL-2.0", "ISC",
    "CC-BY-4.0", "CC-BY-SA-4.0"))
  expect_identical(gl_license_spdx(c("other", "cc-by-nc-4.0", NA, "")), rep(NA_character_, 4L))
  expect_identical(gl_license_spdx("GPL-3.0"), NA_character_)
  expect_identical(gl_license_spdx("mit"), "MIT")
})

# ---- commit counts -------------------------------------------------------------
cc_node <- function(id, n) list(id = sprintf("gid://gitlab/Project/%d", id),
                                repository = if (is.na(n)) NULL else list(commitCount = n))

# A GraphQL io answering commitCount from `counts` (named by project id); `bad` hosts reject the field.
fake_cc_io <- function(counts, bad = character(), seen = new.env()) {
  seen$calls <- list()
  list(gl_graphql = function(domain, query, variables) {
    seen$calls[[length(seen$calls) + 1L]] <- list(domain = domain, query = query, variables = variables)
    if (domain %in% bad)
      return(list(data = NULL, status = 200L, error = NA_character_, errors = list(list(
        message = "Field 'commitCount' doesn't exist on type 'Repository'"))))
    ids <- sub("^.*/", "", unlist(variables$ids))
    nodes <- lapply(ids[ids %in% names(counts)], function(i) cc_node(as.integer(i), counts[[i]]))
    list(data = list(projects = list(nodes = nodes)), errors = NULL, status = 200L, error = NA_character_)
  })
}

fake_cc_pacer <- function(start = 3600 * 100) {
  clock <- new.env()
  clock$t <- start
  list(p = gl_pacer(now = function() clock$t, sleep = function(s) clock$t <- clock$t + s), clock = clock)
}

cc_repos <- function(domain, ids, prefix = domain) data.frame(
  node_id = sprintf("gitlab:%s:%d", domain, ids), repo_id = sprintf("%s/g/p%d", prefix, ids),
  domain = domain, stringsAsFactors = FALSE)

test_that("commit counts are asked by id in batches of 50 per host and come back by repo_id", {
  seen <- new.env()
  ids <- 1:120
  io <- fake_cc_io(setNames(as.list(ids * 10L), ids), seen = seen)
  out <- gl_commit_counts(io, rbind(cc_repos("gitlab.com", ids), cc_repos("code.usgs.gov", 7L)), fake_cc_pacer()$p)
  expect_identical(names(out), c("repo_id", "commits_total"))
  expect_equal(nrow(out), 121L)
  expect_identical(out$commits_total[out$repo_id == "gitlab.com/g/p120"], 1200L)
  expect_identical(out$commits_total[out$repo_id == "code.usgs.gov/g/p7"], 70L)
  expect_identical(vapply(seen$calls, function(c) length(c$variables$ids), 1L), c(50L, 50L, 20L, 1L))
  expect_match(seen$calls[[1]]$query, "commitCount", fixed = TRUE)
  expect_match(seen$calls[[1]]$query, "ids", fixed = TRUE)
  expect_identical(seen$calls[[1]]$variables$ids[[1]], "gid://gitlab/Project/1")
})

test_that("an empty repository or a missing project is NA, never 0", {
  io <- fake_cc_io(list(`1` = 5L, `2` = NA_integer_))
  out <- gl_commit_counts(io, cc_repos("gitlab.com", 1:3), fake_cc_pacer()$p)
  expect_identical(out$commits_total, c(5L, NA, NA))
})

test_that("a commitCount schema error gives that host NA and no further batch, other hosts unaffected", {
  seen <- new.env()
  io <- fake_cc_io(setNames(as.list(1:120 * 2L), 1:120), bad = "old.example.org", seen = seen)
  repos <- rbind(cc_repos("old.example.org", 1:120), cc_repos("gitlab.com", 1:3))
  out <- gl_commit_counts(io, repos, fake_cc_pacer()$p)
  expect_true(all(is.na(out$commits_total[grepl("^old", out$repo_id)])))
  expect_identical(out$commits_total[grepl("^gitlab", out$repo_id)], c(2L, 4L, 6L))
  doms <- vapply(seen$calls, function(c) c$domain, "")
  expect_equal(sum(doms == "old.example.org"), 1L)
  expect_equal(sum(doms == "gitlab.com"), 1L)
})

test_that("commit totals go through the GraphQL pacer, and a refused or failed batch is NA", {
  io <- fake_cc_io(setNames(as.list(1:60), 1:60))
  f <- fake_cc_pacer()
  for (i in seq_len(GITLAB_MAX_REQUESTS)) f$p$before("gitlab.com")
  out <- gl_commit_counts(io, cc_repos("gitlab.com", 1:3), f$p)
  expect_true(all(is.na(out$commits_total)))
  failing <- list(gl_graphql = function(domain, query, variables) list(data = NULL, errors = NULL, status = 500L, error = "HTTP 500"))
  out2 <- gl_commit_counts(failing, cc_repos("gitlab.com", 1:3), fake_cc_pacer()$p)
  expect_true(all(is.na(out2$commits_total)))
  expect_equal(nrow(gl_commit_counts(io, cc_repos("gitlab.com", 1:3)[0, ], fake_cc_pacer()$p)), 0L)
})

# ---- the real io members and the per-run token state ---------------------------
# A fake curl: answers 401 to a request carrying the token header, else 200 with `body`.
tok_curl <- function(body, seen, refuse = TRUE) function(cmd, args, stdout = TRUE, ...) {
  plain <- gsub("^'|'$", "", args)
  files <- sub("^@", "", grep("^@", plain[which(plain == "-H") + 1L], value = TRUE))
  with_token <- length(files) > 0L
  seen$sent <- c(seen$sent, if (with_token) "token" else "anon")
  seen$urls <- c(seen$urls, utils::tail(plain, 1L))
  status <- if (with_token && refuse) 401L else 200L
  writeLines(c(sprintf("HTTP/2 %d", status), ""), plain[match("-D", plain) + 1L])
  writeLines(body, plain[match("-o", plain) + 1L], sep = "")
  as.character(status)
}

test_that("gl_rest GETs under /api/v4 with the PRIVATE-TOKEN header from a file", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = "s3cret")
  seen <- new.env()
  state <- gl_run_state()
  r <- gl_rest("gitlab.com", "/projects/1?license=true", state = state, run = tok_curl("{}", seen, refuse = FALSE))
  expect_identical(seen$urls, "https://gitlab.com/api/v4/projects/1?license=true")
  expect_identical(r$status, 200L)
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = NA)
  seen2 <- new.env()
  gl_rest("gitlab.com", "/x", state = gl_run_state(), run = tok_curl("{}", seen2))
  expect_identical(seen2$sent, "anon")
})

test_that("a 401 with a token retries once without it, drops it for REST and GraphQL, and lowers the budget", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = "s3cret", GITLAB_TOKEN_CODE_USGS_GOV = NA)
  logs <- new.env(); logs$lines <- character()
  f <- fake_rest_pacer()
  state <- gl_run_state(pacer = f$p, log = function(m) logs$lines <- c(logs$lines, m))
  seen <- new.env()
  run <- tok_curl("{\"data\": {\"projects\": {\"nodes\": []}}}", seen)
  r <- gl_rest("gitlab.com", "/projects/1", state = state, run = run)
  expect_identical(r$status, 200L)
  expect_identical(seen$sent, c("token", "anon"))
  gl_rest("gitlab.com", "/projects/2", state = state, run = run)
  g <- gl_graphql("gitlab.com", "q", list(ids = list("a")), run = run, state = state)
  expect_identical(seen$sent, c("token", "anon", "anon", "anon"))
  expect_true(is.na(g$error))
  expect_identical(logs$lines, "gitlab gitlab.com: token refused (401), continuing without it")
  expect_false(any(grepl("s3cret", logs$lines)))
  # The budget is the anonymous 45 now, not 500.
  n <- 0L; while (isTRUE(f$p$before("gitlab.com")) && n < 600L) n <- n + 1L
  expect_equal(n, 45L)
  # A fresh run starts with the token again.
  seen2 <- new.env()
  gl_rest("gitlab.com", "/projects/1", state = gl_run_state(), run = tok_curl("{}", seen2))
  expect_identical(seen2$sent, c("token", "anon"))
})

test_that("a token that works is logged once with the REST budget", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = "s3cret")
  logs <- new.env(); logs$lines <- character()
  state <- gl_run_state(log = function(m) logs$lines <- c(logs$lines, m))
  run <- tok_curl("{}", new.env(), refuse = FALSE)
  for (i in 1:3) gl_rest("gitlab.com", "/projects/1", state = state, run = run)
  expect_identical(logs$lines, "gitlab gitlab.com: token accepted, REST budget 500 per hour")
})

test_that("without a token a 401 is just an answer and nothing is retried or logged", {
  withr::local_envvar(GITLAB_TOKEN_GITLAB_COM = NA)
  logs <- new.env(); logs$lines <- character()
  seen <- new.env()
  state <- gl_run_state(log = function(m) logs$lines <- c(logs$lines, m))
  r <- gl_rest("gitlab.com", "/x", state = state, run = function(cmd, args, stdout = TRUE, ...) {
    seen$n <- sum(seen$n, 1L)
    plain <- gsub("^'|'$", "", args)
    writeLines(c("HTTP/2 401", ""), plain[match("-D", plain) + 1L])
    writeLines("{}", plain[match("-o", plain) + 1L], sep = "")
    "401"
  })
  expect_identical(r$status, 401L)
  expect_identical(seen$n, 1L)
  expect_length(logs$lines, 0L)
})
