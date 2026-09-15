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

# Recognising publishers fails closed. Matching the merge steps as they are
# written today let a new one through as soon as it was spelled differently: a
# quoted mode, a mode read from env, a folded run: with the mode on the next
# line, or Rscript -e calling run_merge() each left an unlocked publisher and a
# green suite. So a job that runs anything under scripts/ or calls Rscript is a
# publisher unless the call is one of these read-only modes, written as a plain
# or quoted word straight after the script. Only run_update and the three
# run_merge functions reach publish(), and publish() is the only caller of
# io$upload. A new mode or a new script counts as publishing until it is listed
# here, which is the moment to check that it never uploads.
.READ_ONLY_MODES <- list(
  "weekly.R"      = c("enumerate", "fetch"),
  "backfill.R"    = c("enumerate", "fetch"),
  "ai_backfill.R" = c("enumerate", "cheap", "gate", "gate-incremental", "deep"))

# Publishes whatever else the line says. Comment lines are dropped before
# matching, so a job that only mentions a merge in a comment is not one. `gh
# release create` is deliberately absent: the enumerate jobs create "current"
# only when it does not exist, which replaces nothing, and holding the lock
# there would make every scan's start wait behind a ninety-minute update for no
# protection.
.PUBLISH_SIGNATURES <- c(
  "\\b(run_update|run_merge|publish|gh_release_upload)\\(",
  "\\bgh\\s+release\\s+(upload|edit|delete|delete-asset)\\b",
  "\\b(gh\\s+api|curl)\\b.*\\breleases\\b|uploads\\.github\\.com")

# A step that uses a local action, whose steps this file cannot see, or an
# action built to write releases. Checked below the job's own keys only, since a
# job-level uses: is a called workflow and is handled on its own.
.PUBLISH_STEP_USES <- "^\\s*(-\\s+)?uses:\\s*['\"]?(\\./|[^\\s#'\"]*release)"

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
# top-level lines outside jobs: for the workflow level) as a named list, or NULL
# when there is no concurrency key. The string shorthand `concurrency: <group>`
# comes back as a group with no queue, which is what GitHub makes of it.
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

# Every top-level line outside jobs:. YAML keys have no order, so a workflow
# concurrency block written after the jobs is still the workflow's.
.top_level <- function(lines) {
  keys <- grep("^[^[:space:]#]", lines)
  ends <- c(keys[-1] - 1L, length(lines))
  drop <- unlist(Map(function(k, e) if (grepl("^jobs:", lines[k])) k:e, keys, ends))
  if (length(drop)) lines[-drop] else lines
}

# The lines of a job's code that make it a publisher, empty when it is not one.
# `workflows` names the files in this directory, whose jobs are audited in their
# own right; `scripts` names the files under scripts/, so a call that reaches
# one by its bare name (after a cd) is still seen.
.publish_evidence <- function(job, workflows, scripts) {
  code <- job[.is_code(job)]
  if (!length(code)) return(character(0))
  own <- .indent(code) == min(.indent(code))
  evidence <- character(0)

  # A job-level uses: calls a whole workflow. One from this directory is read
  # on its own, and its publishing job takes the lock there; one from anywhere
  # else cannot be read, so the call itself counts as publishing.
  called <- sub("^\\s*uses:\\s*", "", code[own & grepl("^\\s*uses:", code)])
  called <- gsub("^['\"]|['\"]$", "", trimws(sub("\\s+#.*$", "", called)))
  local <- startsWith(called, "./.github/workflows/") &
    sub("^\\./\\.github/workflows/", "", called) %in% workflows
  evidence <- c(evidence, called[!local])

  for (p in .PUBLISH_SIGNATURES) evidence <- c(evidence, code[grepl(p, code, perl = TRUE)])
  evidence <- c(evidence, code[!own & grepl(.PUBLISH_STEP_USES, code, perl = TRUE)])

  names_re <- paste(gsub("([.\\\\+*?^$(){}|\\[\\]])", "\\\\\\1", scripts), collapse = "|")
  call_re <- sprintf("(?<![A-Za-z0-9_.-])(scripts/[A-Za-z0-9_.-]+\\.(R|sh)%s)(?![A-Za-z0-9_.-])",
                     if (nzchar(names_re)) paste0("|", names_re) else "")
  for (line in code) {
    hits <- gregexpr(call_re, line, perl = TRUE)[[1]]
    if (hits[1] > 0) {
      for (h in seq_along(hits)) {
        script <- basename(substr(line, hits[h], hits[h] + attr(hits, "match.length")[h] - 1L))
        rest <- substring(line, hits[h] + attr(hits, "match.length")[h])
        mode <- regmatches(rest, regexec("^\\s+(['\"]?)([A-Za-z][A-Za-z-]*)\\1(\\s|[;&|)]|$)", rest, perl = TRUE))[[1]]
        if (!(length(mode) && mode[3] %in% .READ_ONLY_MODES[[script]]))
          evidence <- c(evidence, line)
      }
    }
    # An Rscript call that is neither the test suite nor a script the loop above
    # has judged: -e, a flag in front of the script, or an R file elsewhere.
    args <- regmatches(line, gregexpr("\\bRscript\\s+[^\\s;&|]+", line, perl = TRUE))[[1]]
    args <- gsub("^Rscript\\s+|^['\"]|['\"]$", "", args, perl = TRUE)
    if (any(args != "tests/testthat.R" & !grepl(call_re, args, perl = TRUE)))
      evidence <- c(evidence, line)
  }
  unique(trimws(evidence))
}

# Returns the publishers found (file:job) and every way the file breaks the lock.
.publish_lock_audit <- function(lines, file, workflows = character(0),
                                scripts = list.files(file.path(.repo_root, "scripts"),
                                                     pattern = "\\.(R|sh)$")) {
  problems <- character(0)
  publishers <- character(0)

  # The same group at workflow level would serialize whole runs, six-hour scans
  # included, and GitHub cancels a job waiting on a group its own run already
  # holds ("a deadlock ... was detected between 'top level workflow' and ...").
  # Every top-level concurrency key is read, not the first, so a second one
  # beside the workflow's own group cannot hide.
  top <- .top_level(lines)
  for (at in grep("^concurrency:", top))
    if (.in_publish_group(.concurrency_of(top[at:length(top)])))
      problems <- c(problems, sprintf(
        "%s: %s is a workflow-level group; it belongs on the publishing job only", file, .PUBLISH_GROUP))

  jobs <- .workflow_jobs(lines)
  for (id in names(jobs)) {
    job <- jobs[[id]]
    where <- sprintf("%s job %s", file, id)
    conc <- .concurrency_of(job)
    member <- .in_publish_group(conc)
    evidence <- .publish_evidence(job, workflows, scripts)
    if (length(evidence)) {
      publishers <- c(publishers, paste0(file, ":", id))
      if (!member)
        problems <- c(problems, sprintf(
          "%s writes release current but is not in concurrency group %s (a script mode missing from .READ_ONLY_MODES counts as writing): %s",
          where, .PUBLISH_GROUP, paste(evidence, collapse = " | ")))
    }
    if (member && any(grepl("^\\s*uses:\\s*['\"]?\\./\\.github/workflows/", job)) && !length(evidence))
      problems <- c(problems, sprintf(
        "%s calls a workflow from this directory while holding %s; the called workflow's publishing job takes the lock, and would wait on its own caller",
        where, .PUBLISH_GROUP))
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
  workflows <- list.files(wf_dir, pattern = "\\.ya?ml$")
  for (wf in workflows) {
    audit <- .publish_lock_audit(readLines(file.path(wf_dir, wf), warn = FALSE), wf, workflows)
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

  # Top-level keys have no order, so the same group written after jobs: is the
  # same mistake, and so is a second concurrency key beside the workflow's own.
  after_jobs <- c("name: f", "on: workflow_dispatch", "concurrency:", "  group: f-own",
                  "jobs:", "  merge:", "    concurrency:",
                  "      group: vcs-signals-publish", "      queue: max", "    steps:",
                  "      - run: Rscript scripts/weekly.R merge",
                  "concurrency:", "  group: vcs-signals-publish", "  queue: max")
  audit <- .publish_lock_audit(after_jobs, "f.yml")
  expect_identical(audit$publishers, "f.yml:merge")
  expect_length(audit$problems, 1L)
  expect_match(audit$problems, "is a workflow-level group", fixed = TRUE)
})

test_that("the lock audit counts any script call it cannot read as read-only as a publisher", {
  # Each job below is unlocked. Only the jobs that run a script in a mode listed
  # as read-only for that script may pass; every other way of reaching a script
  # has to count as publishing, or a new merge step written a little differently
  # from today's reopens the race with the suite still green.
  wf <- c(
    "name: fixture",
    "on: workflow_dispatch",
    "jobs:",
    "  quoted:",
    "    steps:",
    "      - run: Rscript scripts/weekly.R \"merge\"",
    "  from_env:",
    "    env:",
    "      MODE: merge",
    "    steps:",
    "      - run: Rscript scripts/weekly.R \"$MODE\"",
    "  folded:",
    "    steps:",
    "      - run: >-",
    "          Rscript scripts/weekly.R",
    "          merge",
    "  continued:",
    "    steps:",
    "      - run: |",
    "          Rscript scripts/backfill.R \\",
    "            merge",
    "  inline_r:",
    "    steps:",
    "      - run: Rscript -e 'source(\"scripts/weekly.R\"); run_merge(io, \"out\", \"parts\")'",
    "  no_path:",
    "    steps:",
    "      - run: Rscript -e 'run_update(default_io(), \"out\")'",
    "  new_script:",
    "    steps:",
    "      - run: Rscript scripts/republish.R fetch",
    "  wrong_script:",
    "    steps:",
    "      - run: Rscript scripts/weekly.R gate",
    "  cd_first:",
    "    steps:",
    "      - run: cd scripts && Rscript ai_backfill.R merge",
    "  second_call:",
    "    steps:",
    "      - run: Rscript scripts/weekly.R fetch && Rscript scripts/weekly.R merge",
    "  any_shell:",
    "    steps:",
    "      - run: bash scripts/some-new-mirror.sh out",
    "  api_upload:",
    "    steps:",
    "      - run: gh api -X POST repos/o/r/releases/1/assets -F name=x.db",
    "  release_action:",
    "    steps:",
    "      - uses: softprops/action-gh-release@v2",
    "        with:",
    "          tag_name: current",
    "  local_action:",
    "    steps:",
    "      - uses: ./.github/actions/publish",
    "  read_only:",
    "    env:",
    "      FULL_GATE: 'false'",
    "    steps:",
    "      - uses: actions/checkout@v7",
    "      - run: Rscript tests/testthat.R",
    "      - run: Rscript scripts/weekly.R enumerate",
    "      - run: Rscript scripts/backfill.R 'fetch'",
    "      - run: |",
    "          if [ \"$FULL_GATE\" = \"true\" ]; then",
    "            Rscript scripts/ai_backfill.R gate",
    "          else",
    "            Rscript scripts/ai_backfill.R gate-incremental",
    "          fi",
    "      - run: Rscript scripts/ai_backfill.R deep",
    "      - run: gh release download current --pattern vcs-signals-summary.db --dir out")

  audit <- .publish_lock_audit(wf, "fixture.yml")
  unlocked <- c("quoted", "from_env", "folded", "continued", "inline_r", "no_path", "new_script",
                "wrong_script", "cd_first", "second_call", "any_shell", "api_upload",
                "release_action", "local_action")
  expect_setequal(audit$publishers, paste0("fixture.yml:", unlocked))
  expect_length(audit$problems, length(unlocked))
  expect_false(any(grepl("job read_only\\b", audit$problems)))
})

test_that("a called workflow is audited in its own file, not at the call", {
  # A job that calls a workflow from this directory is not a publisher itself:
  # the called file is read like every other, and its publishing job carries the
  # lock. Taking the lock on the calling job as well would leave that job waiting
  # on a group its own caller holds. A workflow from anywhere else cannot be read
  # here, so the call has to hold the lock.
  wf <- c(
    "name: caller",
    "on: workflow_dispatch",
    "jobs:",
    "  local:",
    "    uses: ./.github/workflows/weekly.yml",
    "  local_locked:",
    "    concurrency:",
    "      group: vcs-signals-publish",
    "      queue: max",
    "    uses: ./.github/workflows/weekly.yml",
    "  missing:",
    "    uses: ./.github/workflows/gone.yml",
    "  remote:",
    "    uses: r-observatory/.github/.github/workflows/publish.yml@main",
    "  remote_locked:",
    "    concurrency:",
    "      group: vcs-signals-publish",
    "      queue: max",
    "    uses: r-observatory/.github/.github/workflows/publish.yml@main")

  audit <- .publish_lock_audit(wf, "caller.yml", workflows = c("caller.yml", "weekly.yml"))
  expect_setequal(audit$publishers, paste0("caller.yml:", c("missing", "remote", "remote_locked")))
  expect_length(audit$problems, 3L)
  expect_match(audit$problems, "job missing writes release current but is not in", fixed = TRUE, all = FALSE)
  expect_match(audit$problems, "job remote writes release current but is not in", fixed = TRUE, all = FALSE)
  expect_match(audit$problems, "job local_locked calls a workflow from this directory", fixed = TRUE, all = FALSE)
})
