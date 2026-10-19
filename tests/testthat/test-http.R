# A fake curl: writes the header and body files named in the arguments, returns the status on stdout.
# A raw body is written as bytes. `seen` keeps the arguments, the request body and the header files.
.arg_after <- function(args, flag) gsub("^'|'$", "", args[match(flag, args) + 1L])
.args_after <- function(args, flag) gsub("^'|'$", "", args[which(args == flag) + 1L])
.fake_curl <- function(status, headers = character(), body = "", exit = 0L, files = TRUE, seen = NULL) {
  function(cmd, args, stdout = TRUE, ...) {
    if (!is.null(seen)) { seen$cmd <- cmd; seen$args <- args
      d <- .arg_after(args, "--data-binary")
      if (!is.na(d) && length(d) && nzchar(d)) seen$sent <- paste(readLines(sub("^@", "", d), warn = FALSE), collapse = "\n")
      at <- grep("^@", .args_after(args, "-H"), value = TRUE)
      seen$header_files <- sub("^@", "", at)
      seen$header_lines <- unlist(lapply(seen$header_files, readLines, warn = FALSE))
      seen$header_modes <- vapply(seen$header_files, function(f) format(file.info(f)$mode), "")
    }
    if (files) {
      writeLines(c(sprintf("HTTP/2 %d", status), sprintf("%s: %s", names(headers), headers), ""), .arg_after(args, "-D"))
      if (is.raw(body)) writeBin(body, .arg_after(args, "-o")) else writeLines(body, .arg_after(args, "-o"), sep = "")
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

test_that("a transfer cut short after the headers keeps their status", {
  r <- http_request("GET", "https://x", run = .fake_curl(429L, c(`Retry-After` = "30"), body = "{\"par", exit = 28L))
  expect_identical(r$status, 429L); expect_match(r$error, "timed out")
  expect_equal(r$headers[["retry-after"]], "30"); expect_equal(r$body, "")
  r18 <- http_request("GET", "https://x", run = .fake_curl(200L, body = "[1,", exit = 18L))
  expect_identical(r18$status, 200L); expect_match(r18$error, "18"); expect_equal(r18$body, "")
  # An informational block is not a final status.
  run100 <- function(cmd, args, stdout = TRUE, ...) {
    writeLines(c("HTTP/1.1 100 Continue", ""), .arg_after(args, "-D"))
    structure("000", status = 28L)
  }
  expect_true(is.na(http_request("POST", "https://x", body = "{}", run = run100)$status))
  g <- http_graphql("https://x", "q", run = .fake_curl(502L, exit = 28L))
  expect_identical(g$status, 502L); expect_null(g$data); expect_match(g$error, "timed out")
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

test_that("a body with an embedded NUL is an error, not a throw", {
  body <- c(charToRaw("{\"data\": \"a"), as.raw(0L), charToRaw("b\"}"))
  r <- http_request("GET", "https://x", run = .fake_curl(200L, body = body))
  expect_identical(r$status, 200L); expect_match(r$error, "could not be read"); expect_equal(r$body, "")
  g <- http_graphql("https://x", "q", run = .fake_curl(200L, body = body))
  expect_null(g$data); expect_match(g$error, "could not be read")
})

test_that("the curl command carries the flags, quoted headers and the timeout", {
  seen <- new.env()
  http_request("POST", "https://x", headers = c("A: b c"), body = "{}", timeout = 7, run = .fake_curl(200L, seen = seen))
  expect_equal(seen$cmd, "curl")
  expect_true(all(c("-sS", "-X", "--max-time", "--data-binary") %in% seen$args))
  expect_equal(seen$args[match("--max-time", seen$args) + 1L], "7")
  expect_true(shQuote("A: b c") %in% seen$args)
  expect_equal(seen$sent, "{}")
  expect_length(seen$header_files, 0L)
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

test_that("empty variables go out as an object", {
  seen <- new.env()
  http_graphql("https://x", "q", variables = list(), run = .fake_curl(200L, body = "{}", seen = seen))
  expect_identical(seen$sent, '{"query":"q","variables":{}}')
  http_graphql("https://x", "q", run = .fake_curl(200L, body = "{}", seen = seen))
  expect_identical(seen$sent, '{"query":"q"}')
})

test_that("http_graphql returns data and surfaces transport failure", {
  g <- http_graphql("https://x", "q", run = .fake_curl(200L, body = '{"data": {"a": 1}}'))
  expect_equal(g$data$a, 1L); expect_null(g$errors)
  g2 <- http_graphql("https://x", "q", run = .fake_curl(0L, exit = 28L, files = FALSE))
  expect_null(g2$data); expect_false(is.na(g2$error))
})

test_that("a JSON body that is not an object is an error, not a throw", {
  for (b in c("1", '"text"', "[1, 2]", "true", "[]")) {
    g <- http_graphql("https://x", "q", run = .fake_curl(200L, body = b))
    expect_null(g$data, info = b); expect_null(g$errors, info = b)
    expect_identical(g$error, "response is not a JSON object", info = b)
  }
  g <- http_graphql("https://x", "q", run = .fake_curl(200L, body = "{}"))
  expect_true(is.na(g$error)); expect_null(g$data)
})

test_that("tokens go in a private header file, never on the command line, and only when given", {
  s1 <- new.env(); s3 <- new.env()
  http_graphql("https://x", "q", token = "t0ken", run = .fake_curl(200L, body = "{}", seen = s1))
  expect_false(any(grepl("t0ken", s1$args)))
  expect_identical(s1$header_lines, "Authorization: Bearer t0ken")
  expect_true(shQuote("Content-Type: application/json") %in% s1$args)
  if (.Platform$OS.type == "unix") expect_identical(unname(s1$header_modes), "600")
  expect_false(file.exists(s1$header_files))
  http_total("https://x", token = "t0ken", run = .fake_curl(200L, seen = s3))
  expect_false(any(grepl("t0ken", s3$args)))
  expect_identical(s3$header_lines, "PRIVATE-TOKEN: t0ken")
  expect_false(file.exists(s3$header_files))
})

test_that("an empty or missing token sends no token header", {
  for (tok in list(NA, NA_character_, "", NULL)) {
    s1 <- new.env(); s2 <- new.env()
    http_graphql("https://x", "q", token = tok, run = .fake_curl(200L, body = "{}", seen = s1))
    http_total("https://x", token = tok, run = .fake_curl(200L, seen = s2))
    for (s in list(s1, s2)) {
      expect_length(s$header_files, 0L)
      expect_false(any(grepl("Authorization|PRIVATE-TOKEN|Bearer", s$args)))
    }
  }
})

test_that("http_graphql passes its timeout to curl, HTTP_TIMEOUT_S by default", {
  seen <- new.env()
  http_graphql("https://x", "q", run = .fake_curl(200L, body = "{}", seen = seen))
  expect_equal(seen$args[match("--max-time", seen$args) + 1L], as.character(HTTP_TIMEOUT_S))
  http_graphql("https://x", "q", timeout = 30, run = .fake_curl(200L, body = "{}", seen = seen))
  expect_equal(seen$args[match("--max-time", seen$args) + 1L], "30")
})
