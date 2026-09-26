.cc_wd <- setwd(.repo_root)
source(file.path(.repo_root, "scripts", "ai_backfill.R"))
setwd(.cc_wd)

# A summary release with one active repository, so enumerate has something to write.
canary_release <- function() {
  rel <- tempfile("rel_"); dir.create(rel)
  scon <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel, "vcs-signals-summary.db"))
  ensure_repo_schema(scon)
  DBI::dbExecute(scon, "INSERT INTO repos (repo_id,node_id,host,host_domain,owner,name,name_with_owner,supported,n_packages,first_seen,last_seen,status) VALUES
    ('github.com/a/keep',NULL,'github','github.com','a','keep','a/keep',1,1,'2024-01-01','2026-07-01','active')")
  DBI::dbDisconnect(scon)
  rel
}

enumerate_with <- function(canary) {
  rel <- canary_release()
  io <- list(
    download = function(pattern, dir) {
      f <- list.files(rel, pattern = utils::glob2rx(pattern), full.names = TRUE)
      if (!length(f)) return(FALSE)
      file.copy(f, file.path(dir, basename(f)), overwrite = TRUE); TRUE },
    graphql = with_contents_canary(function(query) list(data = list()), canary),
    sleep = function(s) invisible(NULL))
  out <- tempfile("out_"); dir.create(out)
  list(out = out, run = function() run_enumerate_ai(io, out))
}

edit_canary <- function(f) function() { r <- contents_canary_ok(); f(r$data) }

test_that("the canary sends one document for all seven candidates", {
  seen <- character(0)
  io <- list(graphql = function(query) { seen <<- c(seen, query); contents_canary_ok() })
  expect_message(tree_query_canary(io), "passed")
  expect_length(seen, 1L)
  for (j in seq_along(unlist(TREE_QUERY_CANARY))) {
    s <- unlist(TREE_QUERY_CANARY)[j]
    expect_true(grepl(sprintf('r%d: repository(owner: "%s", name: "%s")', j - 1L,
                              sub("/.*$", "", s), sub("^[^/]*/", "", s)), seen, fixed = TRUE), info = s)
  }
})

test_that("a 502 on the canary query is read again after the wait, and a second one stops the run", {
  calls <- 0L; slept <- numeric(0)
  io <- list(graphql = function(query) {
      calls <<- calls + 1L
      if (calls == 1L) stop("gh api graphql returned no output")
      contents_canary_ok()
    },
    sleep = function(s) slept <<- c(slept, s))
  expect_message(tree_query_canary(io), "passed")
  expect_equal(calls, 2L)
  expect_equal(slept, AI_BATCH_RETRY_WAIT_S)
  calls <- 0L
  io$graphql <- function(query) { calls <<- calls + 1L; stop("gh api graphql returned no output") }
  expect_error(tree_query_canary(io), "the query did not return. gh api graphql returned no output", fixed = TRUE)
  expect_equal(calls, 2L)
})

test_that("a fault or a failed floor GitHub answered stops the canary on the first reply", {
  # The field-order fault arrives as a reply, not a thrown error, so the retry never sees it.
  fault <- list(data = NULL, errors = list(list(message = paste0(
    "Something went wrong while executing your query on 2026-09-25T14:53:23Z. ",
    "Please include `C80C:1D44DB:97B4C9:1F5EA85:6AB68AE2` when reporting this issue."))))
  no_floor <- contents_canary_ok()
  for (a in sprintf("r%d", 0:3))
    no_floor$data[[a]]$codeOfConduct <- list(url = "https://github.com/other/.github/blob/main/CODE_OF_CONDUCT.md")
  for (case in list(list(reply = fault, says = "GitHub returned no data"),
                    list(reply = no_floor, says = "no own_community candidate"))) {
    calls <- 0L; slept <- numeric(0)
    io <- list(graphql = function(query) {
        calls <<- calls + 1L
        if (calls == 1L) case$reply else contents_canary_ok()
      },
      sleep = function(s) slept <<- c(slept, s))
    expect_error(tree_query_canary(io), case$says, fixed = TRUE)
    expect_equal(calls, 1L, info = case$says)
    expect_length(slept, 0L)
  }
})

test_that("a request GitHub refused whole stops the canary with GitHub's reason", {
  # gh exits 1 on a 401 but still prints the body, which gh_graphql parses.
  refused <- jsonlite::fromJSON(
    '{"message":"Bad credentials","documentation_url":"https://docs.github.com/graphql","status":"401"}',
    simplifyVector = FALSE)
  calls <- 0L
  io <- list(graphql = function(query) { calls <<- calls + 1L; refused }, sleep = function(s) invisible(NULL))
  expect_error(tree_query_canary(io), "GitHub returned no data. Bad credentials (HTTP 401).", fixed = TRUE)
  expect_equal(calls, 1L)
})

test_that("the community-field fault stops enumerate before the roster is written", {
  e <- enumerate_with(function() list(data = NULL,
    errors = list(list(message = "Something went wrong while executing your query."))))
  expect_error(e$run(), "GitHub returned no data")
  expect_false(file.exists(file.path(e$out, "vcs-ai-roster.db")))
})

test_that("an error that is not a missing repository stops the run even with data", {
  e <- enumerate_with(function() { r <- contents_canary_ok()
    r$errors <- list(list(message = "Something went wrong while executing your query.")); r })
  expect_error(e$run(), "GitHub rejected the query")
})

test_that("every candidate coming back empty stops the run", {
  e <- enumerate_with(edit_canary(function(d) list(data = lapply(d, function(x) NULL))))
  expect_error(e$run(), "every candidate came back empty")
})

test_that("one own_community candidate without its files passes while another meets the floor", {
  e <- enumerate_with(edit_canary(function(d) { d$r0$codeOfConduct <- NULL
    d$r0$contributingGuidelines <- NULL; list(data = d) }))
  expect_no_error(e$run())
  expect_true(file.exists(file.path(e$out, "vcs-ai-roster.db")))
})

test_that("one inheritor gone missing passes while another meets the floor", {
  e <- enumerate_with(edit_canary(function(d) { d["r4"] <- list(NULL)
    list(data = d, errors = list(list(type = "NOT_FOUND", path = list("r4"), message = "gone"))) }))
  expect_no_error(e$run())
})

test_that("no own_community candidate with its own files stops the run and names each value", {
  e <- enumerate_with(edit_canary(function(d) {
    for (a in sprintf("r%d", 0:3)) d[[a]]$codeOfConduct <- list(url = "https://github.com/other/.github/blob/main/CODE_OF_CONDUCT.md")
    list(data = d) }))
  err <- tryCatch(e$run(), error = function(err) conditionMessage(err))
  expect_match(err, "no own_community candidate", fixed = TRUE)
  for (s in TREE_QUERY_CANARY$own_community) expect_match(err, s, fixed = TRUE)
  expect_match(err, "TREE_QUERY_CANARY", fixed = TRUE)
})

test_that("no inheritor reporting its owner's template stops the run and names each value", {
  e <- enumerate_with(edit_canary(function(d) {
    for (a in sprintf("r%d", 4:6)) d[[a]]$pullRequestTemplates <- list()
    list(data = d) }))
  err <- tryCatch(e$run(), error = function(err) conditionMessage(err))
  expect_match(err, "no inherited_pr_template candidate", fixed = TRUE)
  for (s in TREE_QUERY_CANARY$inherited_pr_template) expect_match(err, s, fixed = TRUE)
})

test_that("the canary runs alone from the command line", {
  hit <- FALSE
  io <- list(graphql = function(query) { hit <<- TRUE; contents_canary_ok() })
  expect_message(main("canary", tempfile(), io = io), "passed")
  expect_true(hit)
})

test_that("the enumerate tests still answer the canary after a candidate is replaced", {
  old <- TREE_QUERY_CANARY
  swapped <- old; swapped$own_community[1] <- "tidyverse/tibble"
  assign("TREE_QUERY_CANARY", swapped, envir = globalenv())
  withr::defer(assign("TREE_QUERY_CANARY", old, envir = globalenv()))
  hit <- FALSE
  e <- enumerate_with(function() { hit <<- TRUE; contents_canary_ok() })
  expect_no_error(e$run())
  expect_true(hit)
  expect_true(file.exists(file.path(e$out, "vcs-ai-roster.db")))
})
