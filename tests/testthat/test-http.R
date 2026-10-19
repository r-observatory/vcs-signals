# A fake curl: writes the header and body files named in the arguments, returns the status on stdout.
.arg_after <- function(args, flag) gsub("^'|'$", "", args[match(flag, args) + 1L])
.fake_curl <- function(status, headers = character(), body = "", exit = 0L, files = TRUE, seen = NULL) {
  function(cmd, args, stdout = TRUE, ...) {
    if (!is.null(seen)) { seen$cmd <- cmd; seen$args <- args
      d <- .arg_after(args, "--data-binary")
      if (!is.na(d) && length(d) && nzchar(d)) seen$sent <- paste(readLines(sub("^@", "", d), warn = FALSE), collapse = "\n") }
    if (files) {
      writeLines(c(sprintf("HTTP/2 %d", status), sprintf("%s: %s", names(headers), headers), ""), .arg_after(args, "-D"))
      writeLines(body, .arg_after(args, "-o"), sep = "")
    }
    out <- as.character(status)
    if (exit != 0L) attr(out, "status") <- exit
    out
  }
}

test_that("http_total reads x-total and header names come back lower-case", {
  run <- .fake_curl(200L, c(`X-Total` = "42", `Content-Type` = "application/json"), "[]")
  expect_identical(http_total("https://gitlab.com/api/v4/projects?per_page=1", run = run), 42L)
  r <- http_request("GET", "https://x", run = run)
  expect_equal(r$status, 200L); expect_true(is.na(r$error))
  expect_equal(r$headers[["x-total"]], "42"); expect_equal(r$body, "[]")
})

test_that("http_total is NA without the header or on failure", {
  expect_identical(http_total("https://x", run = .fake_curl(200L)), NA_integer_)
  expect_identical(http_total("https://x", run = .fake_curl(500L, c(`X-Total` = "5"))), NA_integer_)
})

test_that("a 429 carries the status, an error and retry-after", {
  r <- http_request("GET", "https://x", run = .fake_curl(429L, c(`Retry-After` = "30", `RateLimit-Remaining` = "0")))
  expect_equal(r$status, 429L); expect_false(is.na(r$error))
  expect_equal(r$headers[["retry-after"]], "30"); expect_equal(r$headers[["ratelimit-remaining"]], "0")
})

test_that("a curl timeout is an error, not a throw", {
  r <- http_request("GET", "https://x", run = .fake_curl(0L, exit = 28L, files = FALSE))
  expect_true(is.na(r$status))
  expect_match(r$error, "timed out|28")
  expect_equal(r$body, "")
})

test_that("a missing status line is an error", {
  run <- function(cmd, args, stdout = TRUE, ...) { writeLines("", .arg_after(args, "-D")); "200" }
  r <- http_request("GET", "https://x", run = run)
  expect_false(is.na(r$error))
})

test_that("a thrown runner is an error, not a throw", {
  r <- http_request("GET", "https://x", run = function(...) stop("no curl"))
  expect_true(is.na(r$status)); expect_match(r$error, "no curl")
})

test_that("the curl command carries the flags, quoted headers and the timeout", {
  seen <- new.env()
  http_request("POST", "https://x", headers = c("A: b c"), body = "{}", timeout = 7, run = .fake_curl(200L, seen = seen))
  expect_equal(seen$cmd, "curl")
  expect_true(all(c("-sS", "-X", "--max-time", "--data-binary") %in% seen$args))
  expect_equal(seen$args[match("--max-time", seen$args) + 1L], "7")
  expect_true(shQuote("A: b c") %in% seen$args)
  expect_equal(seen$sent, "{}")
})

test_that("http_graphql posts the query and variables and returns errors as is", {
  seen <- new.env()
  q <- "query($a: Int) { x }"; v <- list(a = 1L)
  run <- .fake_curl(200L, c(`Content-Type` = "application/json"), '{"data": null, "errors": [{"message": "x"}]}', seen = seen)
  g <- http_graphql("https://gitlab.com/api/graphql", q, v, run = run)
  expect_equal(jsonlite::fromJSON(seen$sent, simplifyVector = FALSE), list(query = q, variables = v))
  expect_null(g$data); expect_equal(g$errors[[1]]$message, "x"); expect_equal(g$status, 200L)
  expect_true(is.na(g$error))
})

test_that("http_graphql returns data and surfaces transport failure", {
  g <- http_graphql("https://x", "q", run = .fake_curl(200L, body = '{"data": {"a": 1}}'))
  expect_equal(g$data$a, 1L); expect_null(g$errors)
  g2 <- http_graphql("https://x", "q", run = .fake_curl(0L, exit = 28L, files = FALSE))
  expect_null(g2$data); expect_false(is.na(g2$error))
})

test_that("tokens go in the right header, and only when given", {
  s1 <- new.env(); s2 <- new.env(); s3 <- new.env(); s4 <- new.env()
  http_graphql("https://x", "q", token = "t", run = .fake_curl(200L, body = "{}", seen = s1))
  expect_true(shQuote("Authorization: Bearer t") %in% s1$args)
  expect_true(shQuote("Content-Type: application/json") %in% s1$args)
  http_graphql("https://x", "q", run = .fake_curl(200L, body = "{}", seen = s2))
  expect_false(any(grepl("Authorization", s2$args)))
  http_total("https://x", token = "t", run = .fake_curl(200L, seen = s3))
  expect_true(shQuote("PRIVATE-TOKEN: t") %in% s3$args)
  http_total("https://x", run = .fake_curl(200L, seen = s4))
  expect_false(any(grepl("PRIVATE-TOKEN", s4$args)))
})
