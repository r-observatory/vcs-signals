#!/usr/bin/env Rscript
# scripts/update.R -  orchestration: acquire CRAN + Bioc URLs, resolve repos, persist.
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[1] else "out"

self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
script_dir <- if (length(self)) dirname(self) else "scripts"
source(file.path(script_dir, "config.R"))
source(file.path(script_dir, "helpers.R"))
suppressPackageStartupMessages({ library(DBI); library(RSQLite) })

acquire_cran <- function() {
  pdb <- tools::CRAN_package_db()
  pdb <- pdb[!duplicated(pdb$Package), ]
  data.frame(package = pdb$Package, origin = "cran",
             url_raw = pdb$URL, bugreports_raw = pdb$BugReports, stringsAsFactors = FALSE)
}

fetch_views <- function(u) {
  txt <- tryCatch(paste(readLines(url(u), warn = FALSE), collapse = "\n"),
                  error = function(e) NA_character_)
  if (is.na(txt) || !grepl("(^|\n)Package:", txt))
    stop(sprintf("VIEWS fetch failed or empty: %s", u))
  txt
}

acquire_bioc <- function() {
  parts <- lapply(VIEWS_URLS, function(u) {
    m <- read.dcf(textConnection(fetch_views(u)))
    g <- function(f) if (f %in% colnames(m)) as.character(m[, f]) else rep(NA_character_, nrow(m))
    data.frame(package = g("Package"), origin = "bioc",
               url_raw = g("URL"), bugreports_raw = g("BugReports"), stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, parts)
  df[!duplicated(df$package), ]
}

main <- function(out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  db_path <- file.path(out_dir, "vcs-repos.db")
  input <- rbind(acquire_cran(), acquire_bioc())
  resolved <- resolve_all(input)
  idx <- build_repo_index(resolved)

  prev_pkgs <- 0L; prev_repos <- 0L
  if (file.exists(db_path)) {
    c0 <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    if (DBI::dbExistsTable(c0, "repos"))
      prev_repos <- DBI::dbGetQuery(c0, "SELECT COUNT(*) n FROM repos WHERE status IN ('active','moved')")$n
    # The package || origin concat below assumes the current two-origin vocabulary (cran/bioc).
    # A future third origin could collide and would need an explicit delimiter here.
    if (DBI::dbExistsTable(c0, "repo_packages"))
      prev_pkgs <- DBI::dbGetQuery(c0, "SELECT COUNT(DISTINCT package || origin) n FROM repo_packages")$n
    DBI::dbDisconnect(c0)
  }
  curr_pkgs <- length(unique(paste(idx$repo_packages$package, idx$repo_packages$origin)))
  universe_guard(prev_pkgs, prev_repos, curr_pkgs, nrow(idx$repos))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  today <- format(Sys.Date())
  write_repo_tables(con, idx$repos, idx$repo_packages, today)
  print_coverage(input, resolved, idx)
}

main(out_dir)
