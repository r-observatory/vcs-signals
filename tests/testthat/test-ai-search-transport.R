# gh api -i output: status line, headers, a blank line, then the body.
.gh_out <- function(status, headers = character(0), body = '{"total_count":0,"incomplete_results":false,"items":[]}') {
  out <- c(sprintf("HTTP/2.0 %d X", status), sprintf("%s: %s", names(headers), headers), "", body)
  if (status >= 400) attr(out, "status") <- 1L
  out
}
.hit_body <- '{"total_count":2,"incomplete_results":false,"items":[{"commit":{"committer":{"date":"2025-01-02T00:00:00Z"},"message":"x\\n\\nCo-authored-by: Claude <noreply@anthropic.com>","author":{"name":"p"}}}]}'
.run_seq <- function(outs) { i <- 0L; function(cmd, args, stdout) { i <<- i + 1L; outs[[i]] } }
.search <- function(outs, slept) search_earliest_commit_hit("t", "o", "n", "\"x\"", delay = 0,
  run = .run_seq(outs), sleep = function(s) slept$s <- c(slept$s, s), rand = function(...) 0,
  now = function() 1000)

test_that("a refusal naming its wait is waited out, then the answer is kept", {
  slept <- new.env(); slept$s <- numeric(0)
  got <- .search(list(.gh_out(403, c(`Retry-After` = "70")), .gh_out(200, body = .hit_body)), slept)
  expect_equal(slept$s, 70)
  expect_false(got$unavailable); expect_equal(got$total_count, 2L)
})

test_that("a spent budget waits until it resets", {
  slept <- new.env(); slept$s <- numeric(0)
  got <- .search(list(.gh_out(429, c(`X-RateLimit-Remaining` = "0", `X-RateLimit-Reset` = "1045")),
                      .gh_out(200, body = .hit_body)), slept)
  expect_equal(slept$s, 45)
  expect_equal(got$total_count, 2L)
})

test_that("a wait is capped, jittered, and three refusals in a row are a refusal", {
  expect_equal(search_wait_s(403L, c(`retry-after` = "600"), 1L, now = 0, rand = function(...) 3), 120)
  expect_equal(search_wait_s(403L, character(0), 1L, now = 0, rand = function(...) 2), 62)
  expect_equal(search_wait_s(502L, character(0), 2L, now = 0, rand = function(...) 0), 30)
  expect_true(is.na(search_wait_s(422L, character(0), 1L, now = 0, rand = function(...) 0)))
  expect_true(is.na(search_wait_s(403L, character(0), AI_SEARCH_RETRIES + 1L, now = 0, rand = function(...) 0)))
  slept <- new.env(); slept$s <- numeric(0)
  got <- .search(rep(list(.gh_out(403)), AI_SEARCH_RETRIES + 1L), slept)
  expect_true(got$unavailable); expect_true(is.na(got$total_count))
  expect_equal(length(slept$s), AI_SEARCH_RETRIES)
})

test_that("gh's header lines keep no carriage return, so a spent budget still waits for its reset", {
  # gh 2.101.0 ends each header line with CRLF and system2 keeps the CR: "0\r" is not "0".
  out <- c("HTTP/2.0 429 Too Many Requests", "Content-Type: application/json; charset=utf-8\r",
           "X-Ratelimit-Remaining: 0\r", "X-Ratelimit-Reset: 1045\r", "\r", '{"message":"x"}')
  res <- parse_gh_include(out)
  expect_equal(res$status, 429L)
  expect_equal(unname(res$headers[c("x-ratelimit-remaining", "x-ratelimit-reset")]), c("0", "1045"))
  expect_equal(res$body, '{"message":"x"}')
  expect_equal(search_wait_s(res$status, res$headers, 1L, now = 1000, rand = function(...) 0), 45)
})

test_that("an incomplete answer of zero is a refusal and an incomplete count is a floor", {
  zero <- parse_search_commit_hit('{"total_count":0,"incomplete_results":true,"items":[]}')
  expect_true(zero$unavailable)
  five <- parse_search_commit_hit(sub('"incomplete_results":false', '"incomplete_results":true',
                                      sub('"total_count":2', '"total_count":5', .hit_body, fixed = TRUE), fixed = TRUE))
  expect_false(five$unavailable); expect_equal(five$total_count, 5L); expect_equal(five$incomplete, 1L)
  expect_equal(parse_search_commit_hit(.hit_body)$incomplete, 0L)
})

test_that("a count with nothing to check is a refusal, not a zero", {
  # Logged as none it would read as a measured zero and could set assisted_commits to 0.
  for (partial in c("true", "false")) {
    got <- parse_search_commit_hit(sprintf('{"total_count":3,"incomplete_results":%s,"items":[]}', partial))
    expect_true(got$unavailable, info = partial); expect_true(is.na(got$total_count), info = partial)
  }
  expect_false(parse_search_commit_hit('{"total_count":0,"incomplete_results":false,"items":[]}')$unavailable)
})

test_that("searches are paced for the secondary limit commit search enforces", {
  expect_equal(SEARCH_DELAY_S, 12)
})
