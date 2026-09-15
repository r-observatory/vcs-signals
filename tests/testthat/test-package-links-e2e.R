# A package leaving CRAN, coming back, and moving its repository, run through
# the daily update against a release that lives in a directory. The link from
# package to repository has to outlast every one of those, and nothing else the
# pipeline publishes may change meaning because of it.

# Answers the three queries a daily run makes, for any repository asked about.
.links_graphql <- function(query) {
  if (grepl("rateLimit", query)) return(list(data = list(nodes = list())))
  if (grepl("followRenames", query)) {
    hits <- regmatches(query, gregexpr(
      'r[0-9]+: repository\\(owner: "[^"]+", name: "[^"]+"', query))[[1]]
    data <- list()
    for (h in hits) {
      p <- regmatches(h, regexec('(r[0-9]+): repository\\(owner: "([^"]+)", name: "([^"]+)"', h))[[1]]
      data[[p[2]]] <- list(id = paste0("R_", p[3], "_", p[4]),
                           nameWithOwner = paste(p[3], p[4], sep = "/"),
                           isArchived = FALSE, isFork = FALSE, isMirror = FALSE,
                           createdAt = "2020-01-01T00:00:00Z")
    }
    return(list(data = data))
  }
  ids <- gsub('"', "", regmatches(query, gregexpr('"R_[^"]+"', query))[[1]], fixed = TRUE)
  list(data = list(nodes = lapply(ids, function(id) list(
    id = id, nameWithOwner = sub("_", "/", sub("^R_", "", id), fixed = TRUE),
    stargazerCount = 10L, forkCount = 2L))))
}

# Fifty packages that never change, so one package leaving is inside the 2%
# the gate allows repo_packages and the summary, as it is in production.
.fillers <- sprintf("fill%02d", 1:50)

.cran <- function(pkgs) data.frame(
  package = names(pkgs), origin = "cran",
  url_raw = paste0("https://github.com/o/", unname(pkgs)),
  bugreports_raw = NA_character_, stringsAsFactors = FALSE)

.universe <- function(...) {
  base <- stats::setNames(.fillers, .fillers)
  .cran(c(base, c(...)))
}

# The release io is local_release_io from helper-setup.R, so every run reads the
# generation it seeds from and publishes against it as the real ones do.
.release <- function() {
  remote <- tempfile("links_rel_"); dir.create(remote)
  state <- new.env()
  state$remote <- remote
  state$uploads <- 0L
  state$acquire <- NULL
  state$io <- local_release_io(remote,
    acquire = function() state$acquire,
    graphql = .links_graphql,
    on_upload = function(name) state$uploads <- state$uploads + 1L)
  state
}

# run_update and publish read the day off Sys.Date(). A binding in the global
# environment is what the sourced scripts find first.
.run_on <- function(rel, day, universe, opts = list()) {
  assign("Sys.Date", function() as.Date(day), envir = globalenv())
  on.exit(rm("Sys.Date", envir = globalenv()), add = TRUE)
  rel$acquire <- universe
  out <- tempfile(paste0("day_", day, "_")); dir.create(out)
  run_update(rel$io, out, opts)
}

.published <- function(rel, sql, asset = "vcs-signals-summary.db") {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel$remote, asset))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql)
}

.link <- function(rel, repo, pkg, asset = "vcs-signals-summary.db") .published(rel, sprintf(
  "SELECT first_seen, last_seen FROM repo_package_links
    WHERE repo_id = 'github.com/o/%s' AND package = '%s' AND origin = 'cran'", repo, pkg), asset)

# A hundred links from before the table existed, for packages long gone.
.history_backfill <- function() {
  path <- tempfile(fileext = ".csv.gz")
  df <- data.frame(repo_id = sprintf("github.com/old/o%03d", 1:100),
                   package = sprintf("old%03d", 1:100), origin = "cran",
                   first_seen = "2026-08-01", last_seen = "2026-08-20",
                   stringsAsFactors = FALSE)
  con <- gzfile(path, "w")
  utils::write.csv(df, con, row.names = FALSE, quote = FALSE)
  close(con)
  path
}

.fast_batches <- function(env = parent.frame()) {
  old <- BATCH_DELAY_S
  assign("BATCH_DELAY_S", 0, envir = globalenv())
  withr::defer(assign("BATCH_DELAY_S", old, envir = globalenv()), envir = env)
}

.ai_rows <- data.frame(
  repo_id = c("github.com/o/repo1", "github.com/o/repo2"), tool = c("claude", "copilot"),
  first_seen_date = c("2025-03-01", "2025-06-01"), first_seen_censored = 0L,
  evidence_tiers = c("A,D", "D"), markers = c("CLAUDE.md", ".github/copilot-instructions.md"),
  authored = 0L, authored_commits = c(4L, NA), assisted_commits = c(9L, NA),
  last_confirmed_date = "2026-09-01", stringsAsFactors = FALSE)

test_that("a delisted, relisted and moved package keeps every link it ever had", {
  .fast_batches()
  rel <- .release()
  bf <- .history_backfill()
  opts <- list(links_backfill = bf)

  # Day 1: pkgA lives in repo1 and pkgB in repo2.
  .run_on(rel, "2026-09-01", .universe(pkgA = "repo1", pkgB = "repo2"), opts)
  expect_equal(.link(rel, "repo2", "pkgB")$last_seen, "2026-09-01")

  # The weekly AI merge then finds tooling in both repositories.
  for (asset in c("vcs-signals-recent.db", "vcs-signals-summary.db")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(rel$remote, asset))
    DBI::dbWriteTable(con, "vcs_ai_signals", .ai_rows, append = TRUE)
    DBI::dbDisconnect(con)
  }
  ai_cols <- paste(names(.ai_rows), collapse = ", ")
  ai_sql <- sprintf("SELECT %s FROM vcs_ai_signals ORDER BY repo_id", ai_cols)

  # Day 2: CRAN archives pkgB.
  .run_on(rel, "2026-09-02", .universe(pkgA = "repo1"), opts)

  # Everything that means "listed today" still means that.
  expect_false("pkgB" %in% .published(rel, "SELECT package FROM repo_packages")$package)
  expect_false("pkgB" %in% .published(rel, "SELECT package FROM vcs_signals_summary")$package)
  expect_equal(.published(rel,
    "SELECT status FROM repos WHERE repo_id = 'github.com/o/repo2'")$status, "retired")
  m <- jsonlite::fromJSON(file.path(rel$remote, "manifest.json"))
  expect_equal(m$summary$packages, 51L)
  expect_equal(m$summary$packages,
               .published(rel, "SELECT COUNT(*) AS n FROM repo_packages")$n)

  # The link does not.
  b <- .link(rel, "repo2", "pkgB")
  expect_equal(c(b$first_seen, b$last_seen), c("2026-09-01", "2026-09-01"))
  expect_equal(.link(rel, "repo1", "pkgA")$last_seen, "2026-09-02")
  expect_equal(.published(rel, ai_sql), .ai_rows)
  # The recent shard is what tomorrow seeds from, so it has to agree.
  expect_equal(.link(rel, "repo2", "pkgB", "vcs-signals-recent.db")$last_seen, "2026-09-01")
  # The historical links rode along and are not counted as packages anywhere.
  expect_equal(.published(rel,
    "SELECT COUNT(*) AS n FROM repo_package_links WHERE repo_id LIKE 'github.com/old/%'")$n, 100L)

  # Day 3: pkgB is back.
  .run_on(rel, "2026-09-03", .universe(pkgA = "repo1", pkgB = "repo2"), opts)
  b <- .link(rel, "repo2", "pkgB")
  expect_equal(c(b$first_seen, b$last_seen), c("2026-09-01", "2026-09-03"))
  expect_true("pkgB" %in% .published(rel, "SELECT package FROM repo_packages")$package)

  # Day 4: pkgA's URL moves to repo3.
  .run_on(rel, "2026-09-04", .universe(pkgA = "repo3", pkgB = "repo2"), opts)
  a1 <- .link(rel, "repo1", "pkgA"); a3 <- .link(rel, "repo3", "pkgA")
  expect_equal(c(a1$first_seen, a1$last_seen), c("2026-09-01", "2026-09-03"))
  expect_equal(c(a3$first_seen, a3$last_seen), c("2026-09-04", "2026-09-04"))
  expect_equal(.published(rel,
    "SELECT repo_id FROM repo_packages WHERE package = 'pkgA'")$repo_id, "github.com/o/repo3")
  expect_equal(.published(rel, ai_sql), .ai_rows)

  # Day 5: a forced full rebuild changes none of it.
  before <- .published(rel, "SELECT * FROM repo_package_links ORDER BY repo_id, package")
  .run_on(rel, "2026-09-05", .universe(pkgA = "repo3", pkgB = "repo2"),
          c(opts, list(force_full = TRUE)))
  after <- .published(rel, "SELECT * FROM repo_package_links ORDER BY repo_id, package")
  expect_equal(after[c("repo_id", "package", "origin", "first_seen")],
               before[c("repo_id", "package", "origin", "first_seen")])
  still <- after$repo_id %in% c(sprintf("github.com/o/%s", .fillers),
                                "github.com/o/repo2", "github.com/o/repo3")
  expect_true(all(after$last_seen[still] == "2026-09-05"))
  expect_equal(after$last_seen[!still], before$last_seen[!still])
  expect_equal(.published(rel, ai_sql), .ai_rows)
})

test_that("a malformed backfill stops the daily run before it publishes anything", {
  .fast_batches()
  rel <- .release()
  .run_on(rel, "2026-09-01", .universe(pkgA = "repo1"),
          list(links_backfill = .history_backfill()))
  uploads <- rel$uploads
  live <- file_sha256(file.path(rel$remote, "vcs-signals-summary.db"))

  bad <- tempfile(fileext = ".csv.gz")
  con <- gzfile(bad, "w")
  writeLines(c("repo_id,package,origin,first_seen,last_seen",
               "github.com/old/o001,old001,cran,2026-08-01,2026-08-20",
               "github.com/old/o002,old002,cran,2026-08-01,not-a-date"), con)
  close(con)

  expect_error(.run_on(rel, "2026-09-02", .universe(pkgA = "repo1"),
                       list(links_backfill = bad)), "line 3")
  expect_equal(rel$uploads, uploads)
  expect_equal(file_sha256(file.path(rel$remote, "vcs-signals-summary.db")), live)
})

test_that("the daily run applies the committed backfill when no other is named", {
  .fast_batches()
  rel <- .release()
  .run_on(rel, "2026-09-16", .universe(pkgA = "repo1"))
  n <- .published(rel, "SELECT COUNT(*) AS n FROM repo_package_links")$n
  expect_gte(n, 16474L)
})

test_that("the weekly, backfill and AI merges publish the link table they were seeded with", {
  # None of them resolves packages, so none of them may write this table. Each
  # rebuilds the summary from repo_packages; if the links went the same way, a
  # delisted package's link would be gone after the first merge.
  rel <- .release()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  ensure_repo_schema(con); ensure_series_schema(con)
  write_repo_tables(con,
    data.frame(repo_id = "github.com/o/repo1", host = "github", host_domain = "github.com",
               owner = "o", name = "repo1", name_with_owner = "o/repo1", supported = 1L,
               n_packages = 1L, stringsAsFactors = FALSE),
    data.frame(repo_id = "github.com/o/repo1", package = "pkgA", origin = "cran",
               resolved_from = "url", stringsAsFactors = FALSE),
    "2026-09-02",
    links_backfill = data.frame(repo_id = "github.com/o/repo2", package = "pkgB",
                                origin = "cran", first_seen = "2026-08-01",
                                last_seen = "2026-09-01", stringsAsFactors = FALSE))
  seeded <- DBI::dbGetQuery(con, "SELECT * FROM repo_package_links ORDER BY repo_id")
  publish(rel$io, con, tempfile("pub_"), "current", "live", force_full = TRUE,
          base_generation = "")
  DBI::dbDisconnect(con)

  merge_from <- function(script) {
    e <- new.env(parent = globalenv())
    wd <- setwd(.repo_root); on.exit(setwd(wd))
    sys.source(file.path("scripts", script), envir = e)
    e$run_merge
  }
  for (script in c("weekly.R", "backfill.R", "ai_backfill.R")) {
    out <- tempfile("merge_"); dir.create(out)
    parts <- tempfile("parts_"); dir.create(parts)
    suppressMessages(merge_from(script)(rel$io, out, parts))
    for (asset in c("vcs-signals-summary.db", "vcs-signals-recent.db")) {
      got <- .published(rel, "SELECT * FROM repo_package_links ORDER BY repo_id", asset)
      expect_equal(got, seeded, info = paste(script, asset))
    }
  }
})

test_that("a merge a daily run publishes inside seeds again and keeps the links that run advanced", {
  # The weekly merge seeds on Sunday morning and publishes hours later, and the
  # daily update can publish in between. Built from the older seed, the merge
  # would send every link the update moved back to the day before, which the link
  # rule refuses as lost ground. publish() sees the release moved first, and the
  # merge seeds again from what the update left.
  .fast_batches()
  rel <- .release()
  opts <- list(links_backfill = .history_backfill())
  .run_on(rel, "2026-09-01", .universe(pkgA = "repo1"), opts)

  weekly <- new.env(parent = globalenv())
  withr::with_dir(.repo_root, sys.source(file.path("scripts", "weekly.R"), envir = weekly))
  fired <- FALSE
  io <- local_release_io(rel$remote, on_download = function(pattern) {
    if (!fired && identical(pattern, "vcs-signals-recent.db")) {
      fired <<- TRUE
      .run_on(rel, "2026-09-02", .universe(pkgA = "repo1"), opts)
    }
  })
  parts <- tempfile("parts_"); dir.create(parts)
  withr::local_envvar(VCS_PARTS = parts)

  suppressMessages(expect_message(weekly$main("merge", tempfile("weekly_out_"), io = io),
                                  "another publisher replaced assets"))
  expect_true(fired)
  a <- .link(rel, "repo1", "pkgA")
  expect_equal(c(a$first_seen, a$last_seen), c("2026-09-01", "2026-09-02"))
  expect_equal(.link(rel, "repo1", "pkgA", "vcs-signals-recent.db")$last_seen, "2026-09-02")
})
