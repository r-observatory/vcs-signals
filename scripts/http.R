# scripts/http.R - a curl transport for REST and GraphQL. Depends on HTTP_TIMEOUT_S (config.R).

# The last response block curl -D wrote (redirects and 100-continue add earlier ones), or NULL.
.curl_last_block <- function(lines) {
  lines <- sub("\r$", "", lines)
  starts <- grep("^HTTP/", lines)
  if (!length(starts)) return(NULL)
  lines[starts[length(starts)]:length(lines)]
}

# Header lines (status line first) to a named character vector, names lower-case.
.parse_curl_headers <- function(lines) {
  lines <- .curl_last_block(lines)
  if (is.null(lines)) return(NULL)
  lines <- lines[-1]
  lines <- lines[grepl(":", lines, fixed = TRUE)]
  if (!length(lines)) return(character())
  out <- trimws(sub("^[^:]*:", "", lines))
  names(out) <- tolower(trimws(sub(":.*$", "", lines)))
  out
}

# The final status code on the last block's status line, or NA (a 1xx is not final).
.curl_status_line <- function(lines) {
  block <- .curl_last_block(lines)
  if (is.null(block)) return(NA_integer_)
  s <- suppressWarnings(as.integer(strsplit(trimws(block[1]), "[[:space:]]+")[[1]][2]))
  if (is.na(s) || s < 200L) NA_integer_ else s
}

# TRUE for a usable token: not NULL, NA or "".
.has_token <- function(token) length(token) == 1L && !is.na(token) && nzchar(token)

# Reads the body as UTF-8 text. An embedded NUL or an unreadable file is an error, not a throw.
.read_body <- function(path) tryCatch({
  b <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(b) <- "UTF-8"
  list(body = b, error = NA_character_)
}, error = function(e) list(body = "", error = paste("response body could not be read:", conditionMessage(e))))

# One request. Never throws; a failure carries `error` (and the status when there is one).
# secret_headers (a token) are passed to curl in a file, never on the command line.
http_request <- function(method, url, headers = character(), body = NULL,
                         timeout = HTTP_TIMEOUT_S, run = system2, secret_headers = character()) {
  hdr <- tempfile("curl-h-"); out <- tempfile("curl-b-"); inp <- tempfile("curl-i-"); sec <- tempfile("curl-s-")
  on.exit(unlink(c(hdr, out, inp, sec)), add = TRUE)
  res <- list(status = NA_integer_, headers = character(), body = "", error = NA_character_)
  args <- c("-sS", "-X", method, "--max-time", as.character(timeout),
            "-D", shQuote(hdr), "-o", shQuote(out), "-w", shQuote("%{http_code}"))
  for (h in headers) args <- c(args, "-H", shQuote(h))
  if (length(secret_headers)) {
    file.create(sec)
    Sys.chmod(sec, "0600")
    writeLines(gsub("[\r\n]", "", secret_headers), sec)
    args <- c(args, "-H", shQuote(paste0("@", sec)))
  }
  if (!is.null(body)) {
    writeBin(charToRaw(enc2utf8(body)), inp)
    args <- c(args, "--data-binary", shQuote(paste0("@", inp)))
  }
  args <- c(args, shQuote(url))
  stdout <- tryCatch(suppressWarnings(run("curl", args, stdout = TRUE)),
                     error = function(e) e)
  if (inherits(stdout, "error")) { res$error <- paste("curl failed:", conditionMessage(stdout)); return(res) }
  code <- attr(stdout, "status")
  hlines <- if (file.exists(hdr)) readLines(hdr, warn = FALSE) else character()
  parsed_h <- .parse_curl_headers(hlines)
  if (!is.null(parsed_h)) res$headers <- parsed_h
  if (!is.null(code) && code != 0L) {
    res$error <- if (code == 28L) "curl timed out (exit 28)" else sprintf("curl exit code %d", code)
    # Cut short after the headers arrived (a timeout, exit 28, or a short body, exit 18): keep their status.
    res$status <- .curl_status_line(hlines)
    return(res)
  }
  if (file.exists(out)) {
    b <- .read_body(out)
    res$body <- b$body
    if (!is.na(b$error)) res$error <- b$error
  }
  status <- suppressWarnings(as.integer(trimws(paste(utils::tail(as.character(stdout), 1L), collapse = ""))))
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
  # An empty list would go out as [], and GraphQL wants an object.
  if (!is.null(variables)) payload$variables <- if (length(variables)) variables else stats::setNames(list(), character())
  body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  secret <- if (.has_token(token)) paste("Authorization: Bearer", token) else character()
  r <- http_request("POST", endpoint, headers = "Content-Type: application/json", body = as.character(body),
                    run = run, secret_headers = secret)
  res <- list(data = NULL, errors = NULL, status = r$status, error = r$error)
  if (!is.na(r$error) && is.na(r$status)) return(res)
  parsed <- tryCatch(jsonlite::fromJSON(r$body, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed)) {
    if (is.na(res$error)) res$error <- "response is not JSON"
    return(res)
  }
  if (!is.list(parsed) || is.null(names(parsed))) {
    if (is.na(res$error)) res$error <- "response is not a JSON object"
    return(res)
  }
  res$data <- parsed$data
  res$errors <- parsed$errors
  res
}

# The x-total header of a list endpoint (use per_page=1), or NA_integer_.
http_total <- function(url, token = NA, run = system2) {
  secret <- if (.has_token(token)) paste("PRIVATE-TOKEN:", token) else character()
  r <- http_request("GET", url, run = run, secret_headers = secret)
  if (!is.na(r$error)) return(NA_integer_)
  if (!"x-total" %in% names(r$headers)) return(NA_integer_)
  suppressWarnings(as.integer(r$headers[["x-total"]]))
}
