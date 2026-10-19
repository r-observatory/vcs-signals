# scripts/http.R - a curl transport for REST and GraphQL. Depends on HTTP_TIMEOUT_S (config.R).

# Header lines (status line first) to a named character vector, names lower-case.
# curl -D writes one block per response (redirects, 100-continue); the last one wins.
.parse_curl_headers <- function(lines) {
  lines <- sub("\r$", "", lines)
  starts <- grep("^HTTP/", lines)
  if (!length(starts)) return(NULL)
  lines <- lines[starts[length(starts)]:length(lines)][-1]
  lines <- lines[grepl(":", lines, fixed = TRUE)]
  if (!length(lines)) return(character())
  out <- trimws(sub("^[^:]*:", "", lines))
  names(out) <- tolower(trimws(sub(":.*$", "", lines)))
  out
}

# One request. Never throws; a failure carries `error` (and the status when there is one).
http_request <- function(method, url, headers = character(), body = NULL,
                         timeout = HTTP_TIMEOUT_S, run = system2) {
  hdr <- tempfile("curl-h-"); out <- tempfile("curl-b-"); inp <- tempfile("curl-i-")
  on.exit(unlink(c(hdr, out, inp)), add = TRUE)
  res <- list(status = NA_integer_, headers = character(), body = "", error = NA_character_)
  args <- c("-sS", "-X", method, "--max-time", as.character(timeout),
            "-D", shQuote(hdr), "-o", shQuote(out), "-w", shQuote("%{http_code}"))
  for (h in headers) args <- c(args, "-H", shQuote(h))
  if (!is.null(body)) {
    writeBin(charToRaw(enc2utf8(body)), inp)
    args <- c(args, "--data-binary", shQuote(paste0("@", inp)))
  }
  args <- c(args, shQuote(url))
  stdout <- tryCatch(suppressWarnings(run("curl", args, stdout = TRUE)),
                     error = function(e) e)
  if (inherits(stdout, "error")) { res$error <- paste("curl failed:", conditionMessage(stdout)); return(res) }
  code <- attr(stdout, "status")
  parsed_h <- if (file.exists(hdr)) .parse_curl_headers(readLines(hdr, warn = FALSE)) else NULL
  if (!is.null(parsed_h)) res$headers <- parsed_h
  if (file.exists(out)) {
    res$body <- rawToChar(readBin(out, "raw", file.size(out)))
    Encoding(res$body) <- "UTF-8"
  }
  status <- suppressWarnings(as.integer(trimws(paste(utils::tail(as.character(stdout), 1L), collapse = ""))))
  if (!is.null(code) && code != 0L) {
    res$error <- if (code == 28L) "curl timed out (exit 28)" else sprintf("curl exit code %d", code)
    res$body <- ""
    return(res)
  }
  if (length(status) != 1L || is.na(status) || status == 0L || is.null(parsed_h)) {
    res$error <- "curl returned no HTTP status"
    return(res)
  }
  res$status <- status
  if (status >= 400L) res$error <- sprintf("HTTP %d", status)
  res
}

# POST a GraphQL query. GraphQL-level errors come back as `errors`, not thrown.
http_graphql <- function(endpoint, query, variables = NULL, token = NA, run = system2) {
  payload <- list(query = query)
  if (!is.null(variables)) payload$variables <- variables
  body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  h <- "Content-Type: application/json"
  if (!is.na(token)) h <- c(h, paste("Authorization: Bearer", token))
  r <- http_request("POST", endpoint, headers = h, body = as.character(body), run = run)
  res <- list(data = NULL, errors = NULL, status = r$status, error = r$error)
  if (!is.na(r$error) && is.na(r$status)) return(res)
  parsed <- tryCatch(jsonlite::fromJSON(r$body, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed)) {
    if (is.na(res$error)) res$error <- "response is not JSON"
    return(res)
  }
  res$data <- parsed$data
  res$errors <- parsed$errors
  res
}

# The x-total header of a list endpoint (use per_page=1), or NA_integer_.
http_total <- function(url, token = NA, run = system2) {
  h <- if (is.na(token)) character() else paste("PRIVATE-TOKEN:", token)
  r <- http_request("GET", url, headers = h, run = run)
  if (!is.na(r$error)) return(NA_integer_)
  if (!"x-total" %in% names(r$headers)) return(NA_integer_)
  suppressWarnings(as.integer(r$headers[["x-total"]]))
}
