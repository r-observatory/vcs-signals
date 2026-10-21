# A git repository for the Codeberg clone readers, built once per session from fixed commits, and a
# clone io member that clones it from its local path. No git config file is read or written.

# Runs git in `dir` with no global or system config; stops on a non-zero exit.
.cb_fixture_git <- function(dir, args, env = character()) {
  out <- suppressWarnings(system2("git", c("-c", "init.defaultBranch=main", "-c", "commit.gpgsign=false",
                                           "-C", shQuote(dir), args), stdout = TRUE, stderr = TRUE,
                                  env = c("GIT_CONFIG_GLOBAL=/dev/null", "GIT_CONFIG_NOSYSTEM=1", env)))
  if (!is.null(attr(out, "status"))) stop("fixture git ", args[1], " failed: ", paste(out, collapse = "\n"))
  out
}

# Author and committer for one commit, at a UTC time given as 2025-01-10T10:00:00Z.
.cb_fixture_who <- function(name, email, at) {
  t <- shQuote(sprintf("@%d +0000", as.integer(as.POSIXct(at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))))
  c(paste0("GIT_AUTHOR_NAME=", name), paste0("GIT_AUTHOR_EMAIL=", email), paste0("GIT_AUTHOR_DATE=", t),
    paste0("GIT_COMMITTER_NAME=", name), paste0("GIT_COMMITTER_EMAIL=", email), paste0("GIT_COMMITTER_DATE=", t))
}

.cb_fixture_commit <- function(work, who, msg, files) {
  for (f in names(files)) {
    dir.create(dirname(file.path(work, f)), showWarnings = FALSE, recursive = TRUE)
    writeLines(files[[f]], file.path(work, f))
  }
  # --force, so a personal ~/.config/git/ignore never drops a fixture file.
  .cb_fixture_git(work, c("add", "--force", "--", shQuote(names(files))))
  .cb_fixture_git(work, c("commit", "--quiet", "-m", shQuote(msg)), who)
}

#' The fixture's bare repository. `main` holds 6 commits, one a merge, by 4 name and address pairs,
#' the top one with 3.
cb_fixture_repo <- function() {
  bare <- file.path(tempdir(), "cb-fixture-repo.git")
  if (dir.exists(bare)) return(bare)
  build <- tempfile("cb-fixture-build-")
  on.exit(unlink(build, recursive = TRUE, force = TRUE), add = TRUE)
  work <- file.path(build, "work")
  dir.create(work, recursive = TRUE)
  .cb_fixture_git(work, c("init", "--quiet"))
  ann <- function(at) .cb_fixture_who("Ann", "ann@example.invalid", at)
  .cb_fixture_commit(work, ann("2025-01-10T10:00:00Z"), "Add DESCRIPTION",
                     list(DESCRIPTION = c("Package: cbpkg", "Version: 0.3.0")))
  .cb_fixture_commit(work, ann("2025-02-10T10:00:00Z"), "Ignore RStudio files and add CI",
                     list(.gitignore = ".Rproj.user",
                          .woodpecker.yml = c("steps:", "  - name: check", "    image: rocker/r-ver",
                                              "    commands:", "      - R CMD check .")))
  .cb_fixture_git(work, c("checkout", "--quiet", "-b", "topic"))
  .cb_fixture_commit(work, .cb_fixture_who("Bo", "bo@example.invalid", "2025-03-10T10:00:00Z"), "Add a function",
                     list(`R/a.R` = "a <- function() 1"))
  .cb_fixture_git(work, c("checkout", "--quiet", "main"))
  .cb_fixture_git(work, c("merge", "--quiet", "--no-ff", "-m", shQuote("Merge branch topic"), "topic"),
                  ann("2025-03-12T10:00:00Z"))
  .cb_fixture_commit(work, .cb_fixture_who("Ann", "ann.b@example.invalid", "2025-06-01T12:00:00Z"), "Add CLAUDE.md",
                     list(CLAUDE.md = c("# cbpkg", "", "Run the tests before each commit.")))
  .cb_fixture_commit(work, .cb_fixture_who("Cy", "cy@example.invalid", "2025-09-01T08:30:00Z"), "Ignore .claude",
                     list(.gitignore = c(".Rproj.user", ".claude")))
  .cb_fixture_git(build, c("clone", "--bare", "--quiet", "work", "repo.git"))
  if (!file.rename(file.path(build, "repo.git"), bare)) stop("the fixture repository could not be moved into place")
  bare
}

#' A cb_clone io member that clones `src` under the clone environment, treeless as from Codeberg: over
#' file://, since a plain local clone ignores the filter, with an upload-pack that allows it.
cb_local_clone <- function(src) function(url, dir)
  .cb_git(system2, c("clone", "--bare", paste0("--filter=", CODEBERG_CLONE_FILTER), "--no-tags", "--single-branch",
                     "--quiet", paste0("--upload-pack=", shQuote("git -c uploadpack.allowFilter=true upload-pack")),
                     shQuote(paste0("file://", src)), shQuote(dir)),
          stderr = TRUE, timeout = CODEBERG_CLONE_TIMEOUT_S)
