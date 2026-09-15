# Every job that seeds its working database from release "current" and later
# uploads whole tables back over it has to hold one shared concurrency group.
# The workflow-level groups only stopped a workflow racing itself, and
# ai-merge-rerun racing ai-weekly. weekly and ai-weekly share a cron, and their
# merges overlapped on 2026-08-30, 09-06 and 09-13: twice the AI merge seeded
# first and published last, silently putting the weekly commit and contributor
# values back to the week before, and once the order was reversed and the
# regression gate refused the stale weekly build.
#
# The lock lives in workflow YAML, where nothing else checks it, so a publisher
# added later without the block would quietly reopen the race. These tests read
# the workflow text rather than a YAML parser: CI installs no yaml package, and
# the layout every workflow here uses (top-level jobs:, one indent step per
# level) is regular enough to read line by line.

.PUBLISH_GROUP <- "vcs-signals-publish"

# What makes a job a publisher. Comment lines are dropped before matching, so a
# job that only mentions a merge in a comment is not one. `gh release create`
# is deliberately absent: the enumerate jobs create "current" only when it does
# not exist, which replaces nothing, and holding the lock there would make every
# scan's start wait behind a ninety-minute update for no protection.
.PUBLISH_SIGNATURES <- c(
  "scripts/update\\.R",                                  # run_update seeds and publishes
  "\\.R\\s+merge\\b",                                     # every merge mode ends in publish()
  "\\bgh\\s+release\\s+(upload|edit|delete|delete-asset)\\b",
  "mirror-year-tags\\.sh")

.is_code <- function(lines) !grepl("^\\s*(#.*)?$", lines)
.indent  <- function(lines) nchar(sub("^( *).*$", "\\1", lines))

# Splits a workflow into its jobs: a named list of each job's body lines, keyed
# by job id. The job indentation is read from the file rather than assumed.
.workflow_jobs <- function(lines) {
  at <- grep("^jobs:\\s*(#.*)?$", lines)
  if (length(at) != 1L) stop("expected exactly one top-level jobs: key")
  body <- lines[-seq_len(at)]
  top <- grep("^[^[:space:]#]", body)
  if (length(top)) body <- body[seq_len(top[1] - 1L)]
  code <- .is_code(body)
  if (!any(code)) return(list())
  ind <- .indent(body)
  keys <- which(code & ind == min(ind[code]))
  ends <- c(keys[-1] - 1L, length(body))
  jobs <- Map(function(k, e) if (e > k) body[(k + 1L):e] else character(0), keys, ends)
  stats::setNames(jobs, sub("^\\s*([^:]+):.*$", "\\1", body[keys]))
}

# The concurrency settings among a block's direct properties (a job body, or the
# lines above jobs: for the workflow level) as a named list, or NULL when there
# is no concurrency key. The string shorthand `concurrency: <group>` comes back
# as a group with no queue, which is what GitHub makes of it.
.concurrency_of <- function(lines) {
  code <- lines[.is_code(lines)]
  if (!length(code)) return(NULL)
  ind <- .indent(code)
  at <- which(ind == min(ind) & grepl("^\\s*concurrency:", code))
  if (!length(at)) return(NULL)
  at <- at[1]
  clean <- function(v) gsub("^['\"]|['\"]$", "", trimws(sub("\\s+#.*$", "", v)))
  inline <- clean(sub("^\\s*concurrency:", "", code[at]))
  if (nzchar(inline)) return(list(group = inline))
  after <- seq_along(code) > at
  stop_at <- which(after & ind <= ind[at])
  span <- which(after & (if (length(stop_at)) seq_along(code) < stop_at[1] else TRUE))
  kv <- code[span]
  stats::setNames(as.list(clean(sub("^[^:]+:", "", kv))), trimws(sub(":.*$", "", kv)))
}

.in_publish_group <- function(conc) {
  !is.null(conc$group) && identical(tolower(conc$group), .PUBLISH_GROUP)
}

# Returns the publishers found (file:job) and every way the file breaks the lock.
.publish_lock_audit <- function(lines, file) {
  problems <- character(0)
  publishers <- character(0)

  # The same group at workflow level would serialize whole runs, six-hour scans
  # included, and GitHub cancels a job waiting on a group its own run already
  # holds ("a deadlock ... was detected between 'top level workflow' and ...").
  top <- .concurrency_of(lines[seq_len(grep("^jobs:", lines)[1] - 1L)])
  if (.in_publish_group(top))
    problems <- c(problems, sprintf(
      "%s: %s is a workflow-level group; it belongs on the publishing job only", file, .PUBLISH_GROUP))

  jobs <- .workflow_jobs(lines)
  for (id in names(jobs)) {
    job <- jobs[[id]]
    where <- sprintf("%s job %s", file, id)
    conc <- .concurrency_of(job)
    member <- .in_publish_group(conc)
    code <- job[.is_code(job)]
    if (any(vapply(.PUBLISH_SIGNATURES, function(p) any(grepl(p, code, perl = TRUE)), logical(1)))) {
      publishers <- c(publishers, paste0(file, ":", id))
      if (!member)
        problems <- c(problems, sprintf(
          "%s writes release current but is not in concurrency group %s", where, .PUBLISH_GROUP))
    }
    if (member && !identical(conc$queue, "max"))
      problems <- c(problems, sprintf(
        "%s is in %s without queue: max; the default queue cancels a pending publisher when another arrives",
        where, .PUBLISH_GROUP))
    if (member && !is.null(conc[["cancel-in-progress"]]))
      problems <- c(problems, sprintf(
        "%s sets cancel-in-progress in %s; leave it unset, since true is invalid with queue: max and kills a publish mid-upload",
        where, .PUBLISH_GROUP))
  }
  list(publishers = publishers, problems = problems)
}

test_that("every job that writes release current holds the shared publish lock, queued", {
  wf_dir <- file.path(.repo_root, ".github", "workflows")
  skip_if_not(dir.exists(wf_dir), "workflows not in this checkout")

  publishers <- character(0)
  problems <- character(0)
  for (wf in list.files(wf_dir, pattern = "\\.ya?ml$")) {
    audit <- .publish_lock_audit(readLines(file.path(wf_dir, wf), warn = FALSE), wf)
    publishers <- c(publishers, audit$publishers)
    problems <- c(problems, audit$problems)
  }
  expect_true(length(problems) == 0L, info = paste(c("", problems), collapse = "\n  "))

  # The six publishers that exist today must all be recognised as publishers.
  # Without this, a signature that stopped matching (a renamed script, a
  # reworded step) would find no publishers and pass by checking nothing.
  known <- c("update.yml:update", "weekly.yml:merge", "ai-weekly.yml:merge",
             "ai-merge-rerun.yml:merge", "backfill.yml:merge", "ai-backfill.yml:merge")
  expect_true(all(known %in% publishers),
              info = paste("not recognised:", paste(setdiff(known, publishers), collapse = ", ")))
})

test_that("the lock audit catches a publisher that forgets the block and ignores comments", {
  wf <- c(
    "name: fixture",
    "on: workflow_dispatch",
    "concurrency:",
    "  group: fixture-own",
    "  cancel-in-progress: false",
    "jobs:",
    "  scan:",
    "    runs-on: ubuntu-latest",
    "    # only mentions Rscript scripts/weekly.R merge in a comment",
    "    steps:",
    "      - run: Rscript scripts/weekly.R fetch",
    "      - run: gh release download current --pattern x.db",
    "  forgot:",
    "    needs: scan",
    "    steps:",
    "      - run: Rscript scripts/weekly.R merge",
    "  shorthand:",
    "    concurrency: vcs-signals-publish",
    "    steps:",
    "      - run: Rscript scripts/update.R out/",
    "  single:",
    "    concurrency:",
    "      group: vcs-signals-publish",
    "    steps:",
    "      - run: bash scripts/mirror-year-tags.sh out",
    "  cancels:",
    "    concurrency:",
    "      group: VCS-Signals-Publish",
    "      queue: max",
    "      cancel-in-progress: false",
    "    steps:",
    "      - run: |",
    "          gh release edit current --notes-file out/release_notes.md",
    "  good:",
    "    # the lock",
    "    concurrency:",
    "      group: vcs-signals-publish   # shared",
    "      queue: \"max\"",
    "    timeout-minutes: 120",
    "    steps:",
    "      - run: Rscript scripts/ai_backfill.R merge")

  audit <- .publish_lock_audit(wf, "fixture.yml")
  expect_setequal(audit$publishers,
                  paste0("fixture.yml:", c("forgot", "shorthand", "single", "cancels", "good")))
  expect_length(audit$problems, 4L)
  expect_match(audit$problems, "job forgot writes release current but is not in", fixed = TRUE, all = FALSE)
  expect_match(audit$problems, "job shorthand is in vcs-signals-publish without queue: max", fixed = TRUE, all = FALSE)
  expect_match(audit$problems, "job single is in vcs-signals-publish without queue: max", fixed = TRUE, all = FALSE)
  expect_match(audit$problems, "job cancels sets cancel-in-progress", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("job (scan|good)\\b", audit$problems)))

  # A workflow-level group of the same name is refused even when the job also holds it.
  top_level <- c("name: f", "on: workflow_dispatch", "concurrency:", "  group: vcs-signals-publish",
                  "  queue: max", "jobs:", "  merge:", "    concurrency:",
                  "      group: vcs-signals-publish", "      queue: max", "    steps:",
                  "      - run: Rscript scripts/weekly.R merge")
  audit <- .publish_lock_audit(top_level, "f.yml")
  expect_identical(audit$publishers, "f.yml:merge")
  expect_length(audit$problems, 1L)
  expect_match(audit$problems, "is a workflow-level group", fixed = TRUE)
})
