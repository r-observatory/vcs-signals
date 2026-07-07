#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(testthat); library(DBI); library(RSQLite)
})
source("scripts/config.R")
source("scripts/helpers.R")
source("scripts/github.R")
test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
