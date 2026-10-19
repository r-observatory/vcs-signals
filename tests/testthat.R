#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(testthat); library(DBI); library(RSQLite)
})
source("scripts/config.R")
source("scripts/hosts.R")
source("scripts/http.R")
source("scripts/pacing.R")
source("scripts/helpers.R")
source("scripts/github.R")
source("scripts/ai_signals.R")
source("scripts/dev_tooling.R")
test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
