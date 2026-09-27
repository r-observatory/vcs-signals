test_that("account counts are read per tool, and an empty default branch is a counted zero", {
  repos <- data.frame(repo_id = c("github.com/johnpaulgosling/addivortes", "github.com/o/empty", "github.com/o/gone"),
                      owner = c("johnpaulgosling", "o", "o"), name = c("addivortes", "empty", "gone"),
                      stringsAsFactors = FALSE)
  resp <- list(data = list(
    r0 = list(defaultBranchRef = list(target = list(
      a_cursor = list(totalCount = 25L, nodes = list(list(committedDate = "2026-08-13T09:47:23Z"))),
      a_claude = list(totalCount = 0L, nodes = list())))),
    r1 = list(defaultBranchRef = NULL),
    r2 = NULL))
  got <- parse_account_counts(resp, repos)
  a <- got[["github.com/johnpaulgosling/addivortes"]]
  expect_equal(a$tool, "cursor")
  expect_equal(a$commits, 25L)
  expect_equal(a$newest_commit_date, "2026-08-13T09:47:23Z")
  expect_equal(nrow(got[["github.com/o/empty"]]), 0L)
  expect_null(got[["github.com/o/gone"]])
})

test_that("a commit's author address or bot name maps to the tool whose account it is", {
  out <- match_bot_identity(c("NoReply@Anthropic.com", "someone@example.com"),
                            c("dependabot[bot]", "devin-ai-integration[bot]"))
  expect_setequal(out$tool, c("claude", "devin"))
  expect_equal(nrow(match_bot_identity("noreply@anthropic.com.evil.net", character(0))), 0L)
  expect_equal(match_bot_identity("cursoragent@cursor.com", character(0))$tool, "cursor")
  expect_equal(match_bot_identity("208079219+amazon-q-developer[bot]@users.noreply.github.com",
                                  character(0))$tool, "amazonq")
  idx <- .ai_account_index()
  expect_true(all(idx$kind %in% c("graphql", "rest_only", "linked", "names")))
  expect_equal(idx$tool[idx$value == "41898282+claude[bot]@users.noreply.github.com"], "claude")
  expect_equal(idx$kind[idx$value == "41898282+claude[bot]@users.noreply.github.com"], "rest_only")
})

test_that("commits by a tool's accounts are a search for every tool that has accounts", {
  inv <- ai_rule_inventory()
  expect_setequal(inv$tool[inv$tier == "A"], unique(vapply(AI_ACCOUNTS, `[[`, "", "tool")))
})

.sig <- function(repo, tool, tiers = "A", markers = "A", authored_commits = NA_integer_)
  .ai_align_signals(data.frame(repo_id = repo, tool = tool, first_seen_date = "2026-01-01",
    first_seen_censored = 1L, evidence_tiers = tiers, markers = markers, authored = 1L,
    authored_commits = authored_commits, last_confirmed_date = "2026-10-04", stringsAsFactors = FALSE))
.cnt <- function(repo, tool, set, n, on)
  data.frame(repo_id = repo, tool = tool, identity_set = set, commits = as.integer(n),
             newest_commit_date = "2026-09-01T00:00:00Z", measured_on = on, stringsAsFactors = FALSE)
.rd <- function(repo, counted_on) .ai_bind_like(.ai_empty_reads(), list(
  data.frame(repo_id = repo, accounts_counted_on = counted_on, stringsAsFactors = FALSE)))

test_that("a tool's accounts are counted once, the Cursor app's id-less commits by their own count", {
  addi <- "github.com/johnpaulgosling/addivortes"
  counts <- rbind(.cnt(addi, "cursor", "graphql", 19, "2026-10-04"),
                  .cnt(addi, "cursor", "cursor[bot]@users.noreply.github.com", 6, "2026-10-06"))
  got <- derive_authored_counts(.sig(addi, "cursor"), counts, .rd(addi, "2026-10-04"))
  expect_equal(got$authored_commits, 25L)
  expect_equal(got$authored_measured_on, "2026-10-06")
})

test_that("the github-actions address adds its own REST count to the account count", {
  snap <- "github.com/d-morrison/snapr"
  counts <- rbind(.cnt(snap, "claude", "graphql", 7, "2026-10-04"),
                  .cnt(snap, "claude", "41898282+claude[bot]@users.noreply.github.com", 2, "2026-10-06"))
  got <- derive_authored_counts(.sig(snap, "claude"), counts, .rd(snap, "2026-10-04"))
  expect_equal(got$authored_commits, 9L)
  expect_equal(got$authored_measured_on, "2026-10-06")
})

test_that("a Sunday count and a Tuesday count both survive the fold, in either order", {
  r <- "github.com/o/r"
  sun <- .cnt(r, "claude", "graphql", 7, "2026-10-04")
  tue <- .cnt(r, "claude", "41898282+claude[bot]@users.noreply.github.com", 2, "2026-10-06")
  a <- fold_account_counts(.ai_empty_counts(), rbind(sun, tue), counted_repos = r)
  b <- fold_account_counts(.ai_empty_counts(), rbind(tue, sun), counted_repos = r)
  expect_equal(nrow(a), 2L); expect_equal(nrow(b), 2L)
  expect_setequal(a$identity_set, b$identity_set)
})

test_that("a repository counted again with no commits by a tool loses that count but keeps REST rows", {
  r <- "github.com/o/r"
  prior <- rbind(.cnt(r, "claude", "graphql", 7, "2026-09-27"),
                 .cnt(r, "claude", "41898282+claude[bot]@users.noreply.github.com", 2, "2026-09-29"))
  got <- fold_account_counts(prior, .ai_empty_counts(), counted_repos = r)
  expect_equal(got$identity_set, "41898282+claude[bot]@users.noreply.github.com")
})

test_that("a repository whose count failed keeps its rows and its counted date", {
  r <- "github.com/o/r"
  prior <- .cnt(r, "claude", "graphql", 7, "2026-09-27")
  expect_equal(nrow(fold_account_counts(prior, .ai_empty_counts(), counted_repos = character(0))), 1L)
  reads <- fold_repo_reads(.rd(r, "2026-09-27"),
    .ai_bind_like(.ai_empty_reads(), list(data.frame(repo_id = r, last_failed_on = "2026-10-04",
      last_failure = "accounts: Something went wrong", stringsAsFactors = FALSE))))
  expect_equal(reads$accounts_counted_on, "2026-09-27")
  expect_equal(reads$last_failure, "accounts: Something went wrong")
})

test_that("an account count with no row yet creates a censored row the search pass will date", {
  f <- found_from_accounts(data.frame(tool = "cursor", commits = 19L,
                                      newest_commit_date = "2026-08-13T09:47:23Z", stringsAsFactors = FALSE))
  f$repo_id <- "github.com/o/r"
  row <- build_cheap_rows(f, "2026-10-04")
  expect_equal(row$evidence_tiers, "A"); expect_equal(row$first_seen_censored, 1L)
  expect_equal(row$first_seen_date, "2026-08-13T09:47:23Z"); expect_equal(row$authored, 1L)
})
