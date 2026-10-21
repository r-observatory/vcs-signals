#!/usr/bin/env Rscript
# Writes the golden record: every table and schema each publishing path's jobs leave in their
# out dirs, every request and release call, and the manifest and release notes, under
# tests/testthat/fixtures/golden/<path>/. Run from the repo root, only when a published value
# is meant to change:
#   Rscript tests/golden/record.R
if (!file.exists(file.path("scripts", "update.R")))
  stop("run this from the repository root", call. = FALSE)
porcelain <- suppressWarnings(system2("git", c("status", "--porcelain", "scripts/"), stdout = TRUE))
if (!is.null(attr(porcelain, "status")) || length(porcelain))
  stop("scripts/ differs from the last commit (or git failed), and the record must come from committed code:\n",
       paste(porcelain, collapse = "\n"), call. = FALSE)

suppressPackageStartupMessages({ library(testthat); library(DBI); library(RSQLite) })
# The same scripts, in the same order, as tests/testthat.R.
for (f in c("config.R", "hosts.R", "http.R", "pacing.R", "helpers.R", "github.R", "gitlab.R", "gitlab_weekly.R", "gitlab_contents.R", "codeberg.R", "codeberg_clone.R", "bitbucket.R", "ai_signals.R", "dev_tooling.R"))
  source(file.path("scripts", f))
# The helpers and the fixture paths are relative to tests/testthat, as when the suite runs.
setwd(file.path("tests", "testthat"))
for (h in sort(list.files(".", pattern = "^helper.*\\.[rR]$"))) source(h)

for (p in golden_paths()) {
  dir <- file.path("fixtures", "golden", p)
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  txt <- golden_snapshot_text(golden_run(p))
  for (f in names(txt)) writeLines(txt[[f]], file.path(dir, f), useBytes = TRUE)
  cat(sprintf("%s: %d files, %d requests and release calls\n", p, length(txt),
              if (nzchar(txt[["requests.txt"]])) length(strsplit(txt[["requests.txt"]], "\n", fixed = TRUE)[[1]]) else 0L))
}
