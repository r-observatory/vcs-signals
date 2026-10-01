# scripts/config.R - constants for the vcs-signals resolver. No logic.

# Bioconductor release VIEWS DCF files (software, annotation, experiment, workflows).
VIEWS_URLS <- c(
  software   = "https://bioconductor.org/packages/release/bioc/VIEWS",
  annotation = "https://bioconductor.org/packages/release/data/annotation/VIEWS",
  experiment = "https://bioconductor.org/packages/release/data/experiment/VIEWS",
  workflows  = "https://bioconductor.org/packages/release/workflows/VIEWS"
)

# Backoff between attempts at a VIEWS fetch, in seconds; one more attempt is made
# than there are waits. bioconductor.org served 504 for at least eight minutes on
# 2026-07-26 and took the whole daily run down with it, so the schedule is sized
# to outlast an outage of that length rather than a momentary blip. Holding the
# runner idle for a quarter hour is far cheaper than forfeiting the day's run.
# The first wait is deliberately small so a one-off blip costs seconds; the tail
# is what covers a sustained outage.
VIEWS_RETRY_WAITS_S <- c(5, 15, 30, 60, 120, 300, 600)

# Known forge domain -> host key. Checked before the denylist so r-forge survives.
KNOWN_FORGES <- c(
  "github.com" = "github", "gitlab.com" = "gitlab", "codeberg.org" = "codeberg",
  "bitbucket.org" = "bitbucket", "git.sr.ht" = "sourcehut", "sr.ht" = "sourcehut",
  "r-forge.r-project.org" = "rforge",
  # INRAE's GitLab; no forge token in its name, so it needs naming here.
  "forgemia.inra.fr" = "gitlab"
)

# Non-repo domains: return NULL even with an owner/name-shaped path (DOIs, preprints, docs, publishers).
NON_REPO_DENYLIST <- c(
  "doi.org", "dx.doi.org", "arxiv.org", "biorxiv.org", "medrxiv.org", "rpubs.com",
  "jstatsoft.org", "osf.io", "zenodo.org", "figshare.com", "ssrn.com", "researchgate.net",
  "sciencedirect.com", "springer.com", "link.springer.com", "onlinelibrary.wiley.com",
  "tandfonline.com", "journals.sagepub.com", "nature.com", "cran.r-project.org"
)
# Denied domain suffixes (covers subdomains of these).
NON_REPO_SUFFIXES <- c(".r-project.org", ".google.com")

# A non-known, non-denied domain becomes host='other' only if it looks like a self-hosted forge.
FORGE_LABEL_TOKENS <- c("git", "code", "gitlab", "gitea", "forgejo", "forge")
FORGE_SUBSTRINGS   <- c("gitlab", "gitea", "forgejo")

# github.io docs sites are handled by parse_pages_url, not parse_vcs_url.
PAGES_SUFFIX <- ".github.io"

# Read-only mirrors excluded from social-signal collection.
MIRROR_GITHUB_OWNERS <- c("cran", "bioc")   # exact github.com owner match
MIRROR_DOMAINS       <- c("git.bioconductor.org")

# Hosts we have an adapter for in v1.
SUPPORTED_HOSTS <- c("github")

# ---- GitHub forward-gauge collection + publishing ----
GRAPHQL_ENDPOINT <- "https://api.github.com/graphql"
RELEASE_REPO     <- "r-observatory/vcs-signals"
# Waits, in seconds, before publish() reads the release listing again when the
# assets it has just uploaded do not yet show the digests of the files it sent.
# A listing served a moment behind the upload is not another publisher, and a run
# that has already put its data out should not go red over it. A real mixture of
# two builds is still there after the last wait, and is reported then.
PUBLISH_CONFIRM_WAITS_S <- c(5, 15)
# Waits, in seconds, before reading the release's asset digests again after gh
# fails to. A publish reads them at least four times, and the release API has
# served 502s here (the update of 2026-08-20), so one bad answer would otherwise
# throw away a 90-minute update or fail a publish whose data is already out.
RELEASE_READ_RETRY_WAITS_S <- c(5, 20)
# Waits, in seconds, before downloading a release asset again after gh fails to.
# The update of 2026-09-30 did 118 minutes of work and then stopped on one failed
# download of a shard the release still listed, during a burst of 502s and 504s.
RELEASE_DOWNLOAD_RETRY_WAITS_S <- c(15, 45, 90)
# After a publish conflict, a merge waits for the release to read the same
# PUBLISH_SETTLE_QUIET_READS times in a row, PUBLISH_SETTLE_POLL_S seconds apart,
# before it seeds again, giving up the wait after PUBLISH_SETTLE_MAX_READS polls.
# A minute without a change is meant to outlast one asset's upload; ten minutes
# is well past a whole publish's uploads, and is small beside a rebuild.
PUBLISH_SETTLE_POLL_S      <- 20
PUBLISH_SETTLE_QUIET_READS <- 3L
PUBLISH_SETTLE_MAX_READS   <- 30L
FORWARD_METRICS  <- c("stars", "forks", "watchers", "issues_open", "issues_closed",
                      "prs_open", "prs_closed", "prs_merged",
                      "releases_total", "size_kb")
CHEAP_BATCH    <- 25L    # repos per cheap-gauge GraphQL query (small enough to stay under GitHub's execution-time limit)
COMMIT_BATCH   <- 8L     # repos per commit-count query (history.totalCount is expensive server-side and times out in larger batches)
RECENT_WINDOW  <- 400L   # days of series kept in the recent shard
REVISION_WINDOW<- 10L    # trailing days re-materialized each run (must be < RECENT_WINDOW)
POINT_RESERVE  <- 1500L  # GraphQL points left unspent as headroom
BATCH_DELAY_S  <- 0.35   # pause between GraphQL batches, to stay well under secondary rate limits
# Minutes into update.R after which the gauge pass stops and the run publishes
# what it has. The job is capped at 150; setup and tests take ~4 and the stages
# after the gauges took up to 28 (2026-09-23..27), so 100 leaves ~18 spare.
GAUGE_DEADLINE_MIN <- 100
# More unresolvable ids than this (or than 1% of the gauged repos) in one run
# reads as a GitHub fault, not deleted repos, so none are acted on.
UNRESOLVABLE_CAP <- 100L
UNRESOLVABLE_CAP_FRAC <- 0.01
# A run that leaves more than this share of repos unreached publishes, then fails,
# so the catch-up cron runs the day again.
UNREACHED_FAIL_FRAC <- 0.25
OWNER_STALE_DAYS <- 14L  # days an owner row outlives the last gauge query that returned it
# The summary's per-repository values. A repository a run does not collect keeps its
# prior values, and every summary builder reads and writes this one list.
REPO_ATTR_COLS <- c("license", "topics", "is_archived", "last_commit_date", "last_release_date",
                    "last_release_tag", "repo_created_at", "median_days_between_releases")
# pipeline_state key written with the first summary whose dates come from the gauge itself.
REPO_DATES_KEY <- "repo_dates_source"
REPO_DATES_SOURCE <- "head-commit+latest-release"

# ---- historical cumulative-series backfill (stars, forks, releases) ----
STARGAZER_PAGE   <- 100L  # items per GraphQL connection page (all metrics share one page size)
BACKFILL_DELAY_S <- 0.8   # pause between connection pages: each page costs 1 GraphQL point, so this keeps a single token under the 5000-points/hour primary budget (~4500/hr)

# Per-metric GraphQL connection shape: conn = connection field name, order =
# orderBy field, sel = "edges" or "nodes" (the selection shape GitHub uses for
# that connection), ts = the timestamp field name inside each edge/node,
# ts_close = the closedAt-equivalent field (open metrics only), kind =
# "cumulative" (reconstruct_cumulative_series) or "open" (reconstruct_open_series).
METRIC_CONNECTIONS <- list(
  stars          = list(conn = "stargazers",   order = "STARRED_AT", sel = "edges", ts = "starredAt",  kind = "cumulative"),
  forks          = list(conn = "forks",        order = "CREATED_AT", sel = "nodes", ts = "createdAt",  kind = "cumulative"),
  releases_total = list(conn = "releases",     order = "CREATED_AT", sel = "nodes", ts = "createdAt",  kind = "cumulative"),
  issues_open    = list(conn = "issues",       order = "CREATED_AT", sel = "nodes", ts = "createdAt", ts_close = "closedAt", kind = "open"),
  prs_open       = list(conn = "pullRequests", order = "CREATED_AT", sel = "nodes", ts = "createdAt", ts_close = "closedAt", kind = "open")
)
BACKFILL_METRICS <- c("stars", "forks", "releases_total")  # default metric set for a backfill run; open metrics (issues_open/prs_open) are run explicitly via VCS_METRICS
BATCH_REPOS <- 20L   # repos per batched first-page query (a multi-repo aliased query is ~1 GraphQL point)

# ---- weekly commit-count + contributor-count collection ----
# top_contributor_commits and top_contributor_bot come from the body of the
# contributors call; the bot flag is 1 for a bot account, 0 for a user account.
WEEKLY_METRICS <- c("commits_total", "contributors_total",
                    "median_days_to_close_issue", "median_days_to_close_pr",
                    "median_open_issue_age_days",
                    "top_contributor_commits", "top_contributor_bot")
COMMIT_HISTORY_BATCH <- 12L  # repos per commits.history.totalCount aliased query: execution-time expensive server-side, so kept well under the ~15-repo point where it starts to time out (not the 20-40 a cheap connection page can batch)
MEDIAN_BATCH <- 10L  # repos per responsiveness query: 3 connections x 50 nodes/repo is execution-time heavy server-side, so kept well below the cheap-connection batch size to avoid 502s
CONTRIBUTOR_DELAY_S   <- 0.5 # pause between per-repo REST contributor-count lookups (one request per repo, no batching available)

# ---- AI-tooling detection ----
# Config markers. location = which tree the entry appears in ("root" or the
# ".github" subtree). agnostic = tool-agnostic (recorded but excluded from the
# tool count / first-tool rollups and never names a package alone). .replit and
# .deepsource.toml are deliberately absent: a bare platform-config file is
# non-evidence, so Replit is detected only via its commit trailer below.
# requires = a pattern one of the same location's entries must match. ignore_line = FALSE when
# no ignore-file line counts, or the ignore files whose line counts (default TRUE, both).
AI_MARKERS <- list(
  list(path = "CLAUDE.md",       tool = "claude",    kind = "file", location = "root",   agnostic = FALSE),
  list(path = "CLAUDE.local.md", tool = "claude",    kind = "file", location = "root",   agnostic = FALSE),
  # RStudio 2026.04 and later write ^\.claude$ into .Rbuildignore on their own, so only
  # a .gitignore line counts.
  list(path = ".claude",        tool = "claude",    kind = "dir",  location = "root",   agnostic = FALSE,
       ignore_line = "gitignore"),
  list(path = ".mcp.json",       tool = "claude",    kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".codex",          tool = "codex",     kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".cursor",         tool = "cursor",    kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".cursorrules",    tool = "cursor",    kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".cursorignore",   tool = "cursor",    kind = "file", location = "root",   agnostic = FALSE),
  list(path = "copilot-instructions.md", tool = "copilot", kind = "file", location = "github", agnostic = FALSE),
  list(path = ".aider.conf.yml", tool = "aider",     kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".aiderignore",    tool = "aider",     kind = "file", location = "root",   agnostic = FALSE),
  list(path = "GEMINI.md",       tool = "gemini",    kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".gemini",         tool = "gemini",    kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".aiexclude",      tool = "gemini",    kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".windsurf",       tool = "windsurf",  kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".windsurfrules",  tool = "windsurf",  kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".clinerules",     tool = "cline",     kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".continue",       tool = "continue",  kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".junie",          tool = "junie",     kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".amazonq",        tool = "amazonq",   kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".roo",            tool = "roo",       kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".roomodes",       tool = "roo",       kind = "file", location = "root",   agnostic = FALSE),
  list(path = "AGENTS.md",       tool = "agents-md", kind = "file", location = "root",   agnostic = TRUE),
  # Agent-neutral instruction files and directories. .agents/ is where every agent
  # except Claude Code installs project skills (Codex, Cursor, Gemini CLI, OpenCode,
  # Amp, Cline, Zed, Warp and ~60 more all target it), so attributing it to any one
  # product would misreport the rest. AGENT.md is the singular spelling; it is used
  # broadly as a neutral alias rather than by one tool, so it is agnostic like AGENTS.md.
  list(path = ".agents",         tool = "agents-dir", kind = "dir",  location = "root",  agnostic = TRUE),
  list(path = ".agents/skills",  tool = "agents-dir", kind = "dir",  location = "root",  agnostic = TRUE),
  list(path = "AGENT.md",        tool = "agents-md",  kind = "file", location = "root",  agnostic = TRUE),
  # Claude Code is the one agent with its own project skills directory; the rest use
  # .agents/skills above. .claude-plugin marks a plugin marketplace published from the repo.
  list(path = ".claude/skills",  tool = "claude",    kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".claude/agents",  tool = "claude",    kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".claude-plugin",  tool = "claude",    kind = "dir",  location = "root",   agnostic = FALSE),
  # xAI. These did not exist when the ruleset was written and the methods note still
  # says Grok leaves nothing durable; it does now.
  list(path = "GROK.md",         tool = "grok",      kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".grok",           tool = "grok",      kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".xai",            tool = "grok",      kind = "dir",  location = "root",   agnostic = FALSE),
  # Google Antigravity, their agentic editor. Jules is not here: it works through pull
  # requests and is already covered by AI_PR_AGENT_LOGINS.
  list(path = ".antigravity",    tool = "antigravity", kind = "dir", location = "root",  agnostic = FALSE),
  # Kiro, the successor to Amazon Q Developer's command-line tool.
  list(path = ".kiro",          tool = "kiro",      kind = "dir",  location = "root",   agnostic = FALSE),
  # Devin and Devin Desktop (Windsurf renamed) keep rules and DeepWiki settings here.
  list(path = ".devin",         tool = "devin",     kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".devinignore",   tool = "devin",     kind = "file", location = "root",   agnostic = FALSE),
  # Copilot's .github folders share plain names with root folders people ignore (prompts/, skills/),
  # so only the committed folder counts.
  # Copilot custom agents count only when the folder holds a Markdown agent file.
  list(path = "agents",         tool = "copilot",   kind = "dir",  location = "github", agnostic = FALSE,
       requires = "^agents/[^/]+\\.md$", ignore_line = FALSE),
  list(path = "instructions",   tool = "copilot",   kind = "dir",  location = "github", agnostic = FALSE,
       ignore_line = FALSE),
  list(path = "prompts",        tool = "copilot",   kind = "dir",  location = "github", agnostic = FALSE,
       ignore_line = FALSE),
  list(path = "chatmodes",      tool = "copilot",   kind = "dir",  location = "github", agnostic = FALSE,
       ignore_line = FALSE),
  list(path = "skills",         tool = "copilot",   kind = "dir",  location = "github", agnostic = FALSE,
       ignore_line = FALSE),
  list(path = "workflows/copilot-setup-steps.yml", tool = "copilot", kind = "file", location = "github",
       agnostic = FALSE),
  # RStudio 2026.06 and later write these into the ignore files once the folder exists, so
  # only the committed path counts.
  list(path = ".posit/assistant",       tool = "posit-assistant", kind = "dir",  location = "root",
       agnostic = FALSE, ignore_line = FALSE),
  list(path = ".positai/settings.json", tool = "posit-assistant", kind = "file", location = "root",
       agnostic = FALSE, ignore_line = FALSE),
  list(path = ".positai/plans",         tool = "posit-assistant", kind = "dir",  location = "root",
       agnostic = FALSE, ignore_line = FALSE),
  list(path = ".positai/agents",        tool = "posit-assistant", kind = "dir",  location = "root",
       agnostic = FALSE, ignore_line = FALSE),
  list(path = "opencode.json",  tool = "opencode",  kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".opencode",      tool = "opencode",  kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = "QWEN.md",        tool = "qwen",      kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".qwen",          tool = "qwen",      kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".kilocode",      tool = "kilo",      kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".kilo",          tool = "kilo",      kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = "WARP.md",        tool = "warp",      kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".jules",         tool = "jules",     kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".openhands",     tool = "openhands", kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".openhands_instructions", tool = "openhands", kind = "file", location = "root", agnostic = FALSE),
  # Watched folders and files, found in no scanned repository on 2026-09-25.
  list(path = ".trae",          tool = "trae",      kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".augment",       tool = "augment",   kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = "CRUSH.md",       tool = "crush",     kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".goosehints",    tool = "goose",     kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".factory",       tool = "factory",   kind = "dir",  location = "root",   agnostic = FALSE),
  list(path = ".vibe",          tool = "vibe",      kind = "dir",  location = "root",   agnostic = FALSE),
  # Ambient IDE marker: the editor writes .positai regardless of AI use, so it is EXCLUDED
  # from the AI signal (ai_deliberate_markers). A marker with no class field defaults to
  # "deliberate". Recording ambient markers as a dev-tooling datum is deferred to the
  # separate dev-tooling signal.
  list(path = ".positai",        tool = "positron",  kind = "file", location = "root",   agnostic = FALSE, class = "ambient"),
  list(path = ".idx",            tool = "idx",       kind = "dir",  location = "root",   agnostic = FALSE, class = "ambient")
)

# Review tools' configuration. Kept out of AI_MARKERS so a review bot never counts as a
# tool that wrote the package. `only`: the entries a folder may hold to count as review.
AI_REVIEW_FILES <- list(
  list(path = ".coderabbit.yaml", tool = "coderabbit",         location = "root"),
  list(path = ".coderabbit.yml",  tool = "coderabbit",         location = "root"),
  list(path = ".gemini",          tool = "gemini-code-assist", location = "root",
       only = c("config.yaml", "styleguide.md"))
)

# ---- Development-tooling detection (data-only) ----
# The single source of truth for the vcs_dev_tooling flag column set. Each entry names a
# table COLUMN (col) and the tree-entry names that satisfy it (paths: a flag is 1 if ANY is
# present). location = which fetched tree to check ("root" = HEAD:, "github" = HEAD:.github,
# "both" = either). match = "exact" set-membership by default, or "suffix" (endsWith) for the
# *.Rproj case. Detection is existence-of-entry-name only; nothing reads file contents. The
# classifier (classify_dev_tooling), the DDL (dev_tooling_create_sql), and the empty helper
# (.devtool_empty) are all derived from these col names, so the column set cannot drift.
# readme_source (TEXT enum) and has_ci (the OR of the ci_* systems) are declared in
# DEV_TOOLING_DERIVED. repo_id, last_scanned and ruleset_version are stamped by the cheap pass.
DEV_TOOLING_MARKERS <- list(
  # CI / CD: one distinct system per column; has_ci is the producer-computed OR of these.
  list(col = "ci_github_actions", paths = c("workflows"),          location = "github"),
  list(col = "ci_gitlab",         paths = c(".gitlab-ci.yml"),     location = "root"),
  list(col = "ci_travis",         paths = c(".travis.yml"),        location = "root"),
  list(col = "ci_appveyor",       paths = c("appveyor.yml"),       location = "root"),
  list(col = "ci_circleci",       paths = c(".circleci"),          location = "root"),
  list(col = "ci_tic",            paths = c("tic.R"),              location = "root"),
  list(col = "ci_jenkins",        paths = c("Jenkinsfile"),        location = "root"),
  list(col = "ci_azure",          paths = c("azure-pipelines.yml"),location = "root"),
  list(col = "ci_drone",          paths = c(".drone.yml"),         location = "root"),
  # Maintenance automation.
  list(col = "has_dependabot",    paths = c("dependabot.yml"),         location = "github"),
  list(col = "has_renovate",      paths = c("renovate.json"),          location = "both"),
  list(col = "has_precommit",     paths = c(".pre-commit-config.yaml"),location = "root"),
  # Lint / format / editor.
  list(col = "has_lintr",         paths = c(".lintr"),          location = "root"),
  list(col = "has_air",           paths = c("air.toml", ".air.toml"), location = "root"),
  list(col = "has_jarl",          paths = c("jarl.toml"),       location = "root"),
  list(col = "has_editorconfig",  paths = c(".editorconfig"),   location = "root"),
  list(col = "has_vscode",        paths = c(".vscode"),         location = "root"),
  list(col = "has_rproj",         paths = c(".Rproj"),          location = "root", match = "suffix"),
  list(col = "has_idea",          paths = c(".idea"),           location = "root"),
  # Ambient IDE marker. Positron writes .positai regardless of AI use, so it is EXCLUDED from
  # the AI signal; here is where recording it as a dev-tooling datum finally lands.
  list(col = "has_positron",      paths = c(".positai"),        location = "root"),
  # Reproducibility / dev-env.
  list(col = "has_renv",          paths = c("renv.lock", "renv"),               location = "root"),
  list(col = "has_data_raw",      paths = c("data-raw"),                        location = "root"),
  list(col = "has_tests_dir",     paths = c("tests"),                           location = "root"),
  list(col = "has_makefile",      paths = c("Makefile"),                        location = "root"),
  list(col = "has_dockerfile",    paths = c("Dockerfile"),                      location = "root"),
  list(col = "has_devcontainer",  paths = c(".devcontainer"),                   location = "root"),
  list(col = "has_nix",           paths = c("flake.nix", "shell.nix", "default.nix"), location = "root"),
  list(col = "has_binder",        paths = c(".binder", "runtime.txt", "apt.txt"),     location = "root"),
  list(col = "has_gitpod",        paths = c(".gitpod.yml"),                     location = "root"),
  # Coverage service.
  list(col = "has_codecov",       paths = c("codecov.yml", ".codecov.yml"),     location = "root"),
  list(col = "has_covrignore",    paths = c(".covrignore"),                     location = "root"),
  # CRAN process. CRAN-SUBMISSION is the current name; CRAN-RELEASE (pre-2021) is not tracked.
  list(col = "has_cran_comments", paths = c("cran-comments.md"),  location = "root"),
  list(col = "has_revdep",        paths = c("revdep"),           location = "root"),
  list(col = "has_cran_submission", paths = c("CRAN-SUBMISSION"),location = "root"),
  # Docs source (repo-only). readme_source is computed; has_quarto is a flag.
  list(col = "has_quarto",        paths = c("_quarto.yml"),      location = "root"),
  # pkgdown is the most common documentation site in the ecosystem and was the
  # conspicuous absence here: _quarto.yml was detectable and this was not. Its
  # config sits at the root or in inst/ under either extension, or in a pkgdown/
  # directory, which also holds templates or extra pages.
  list(col = "has_pkgdown",       paths = c("_pkgdown.yml", "_pkgdown.yaml", "pkgdown",
                                            "inst/_pkgdown.yml", "inst/_pkgdown.yaml"),
                                                                 location = "root"),
  # altdoc keeps its config in altdoc/ at the root, whatever backend it drives
  # (altdoc/mkdocs.yml, altdoc/quarto_website.yml, altdoc/docsify.html). Note
  # altdoc/pkgdown.yml exists too: that is altdoc driving pkgdown, and counting
  # it as a pkgdown site would overstate pkgdown, so the two stay separate.
  list(col = "has_altdoc",        paths = c("altdoc"),           location = "root"),
  # litedown's config is NOT a root file. Every observed instance sits under
  # site/ or docs/, so a root-only rule would report litedown as unused
  # everywhere. The site subtree is fetched for exactly this.
  # docs/ is not listed (it holds built output), so a docs/ path could never match.
  list(col = "has_litedown",      paths = c("_litedown.yml", "site/_litedown.yml"), location = "root"),

  # Documentation written for language models to read. This is the package describing
  # itself TO a model, not evidence a model worked on it, so it is a practice and never
  # an AI-tooling marker.
  list(col = "has_llms_txt",      paths = c("llms.txt", "llms-full.txt"), location = "root"),
  # Agent skills the package SHIPS. inst/ is installed, so these are a deliverable for
  # the package's users, the same kind of thing as a vignette. Distinct from the skills
  # under .claude/ or .agents/, which are what the maintainer used to build it.
  list(col = "has_agent_skills",  paths = c("inst/skills"),      location = "root"),
  # Research / citation / archival.
  list(col = "has_citation_cff",     paths = c("CITATION.cff"),      location = "root"),
  list(col = "has_codemeta",         paths = c("codemeta.json"),     location = "root"),
  list(col = "has_joss",             paths = c("paper.md"),          location = "root"),
  list(col = "has_zenodo",           paths = c(".zenodo.json"),      location = "root"),
  list(col = "has_all_contributors", paths = c(".all-contributorsrc"),location = "root"),
  # Governance / community.
  list(col = "has_issue_template", paths = c("ISSUE_TEMPLATE", "ISSUE_TEMPLATE.md", "issue_template.md"),
                                                                                 location = "both"),
  list(col = "has_funding",        paths = c("FUNDING.yml"),                        location = "github"),
  list(col = "has_security",       paths = c("SECURITY.md"),                        location = "both"),
  list(col = "has_codeowners",     paths = c("CODEOWNERS"),                         location = "both"),
  list(col = "has_support",        paths = c("SUPPORT.md"),                         location = "both"),
  list(col = "has_governance",     paths = c("GOVERNANCE.md"),                      location = "root"),
  # Git-structural.
  list(col = "has_gitattributes",  paths = c(".gitattributes"),         location = "root"),
  list(col = "has_gitmodules",     paths = c(".gitmodules"),            location = "root"),
  list(col = "has_blame_ignore",   paths = c(".git-blame-ignore-revs"), location = "root")
)

# Community files, checked at the root and in .github. The same lists as rpkg-analyzer's git input.
COC_TREE_PATHS <- c("CODE_OF_CONDUCT.md", "CODE_OF_CONDUCT", "CODE_OF_CONDUCT.Rmd", "CODE_OF_CONDUCT.rst",
                    "code_of_conduct.md", "Code_of_conduct.md", "CODE-OF-CONDUCT.md", "CONDUCT.md")
CONTRIBUTING_TREE_PATHS <- c("CONTRIBUTING.md", "CONTRIBUTING", "CONTRIBUTING.Rmd", "CONTRIBUTING.rst",
                             "contributing.md", "Contributing.md", "CONTRIBUTING.MD")
PR_TEMPLATE_TREE_PATHS <- c("pull_request_template.md", "PULL_REQUEST_TEMPLATE.md", "PULL_REQUEST_TEMPLATE")
# pkgdown 2.2.0's config paths the scan can list; the pkgdown/ directory stands for the other two.
PKGDOWN_CONFIG_TREE_PATHS <- c("_pkgdown.yml", "_pkgdown.yaml", "inst/_pkgdown.yml", "inst/_pkgdown.yaml")

# Release items rbuildignore_excluded reports, in output order. The analyzer's build_ignored
# uses the same names, so the viewer reads both with one lookup.
RBUILDIGNORE_ITEMS <- c("README.md", "README.Rmd", "README.qmd", "NEWS.md", "NEWS", "tests", "vignettes",
                        "vignettes/articles", "_pkgdown.yml", "pkgdown", "docs", "CODE_OF_CONDUCT.md",
                        "CONTRIBUTING.md", "data-raw", ".github")
# A vignette source one level under vignettes/, as the scan sees names only.
VIGNETTE_SOURCE_PATTERN <- "\\.(Rmd|Rnw|qmd|Rtex|Rhtml|asis)$"
# rbuildignore_text keeps a .Rbuildignore up to this many bytes (the largest in the sweep is 10,739).
RBUILDIGNORE_TEXT_MAX_BYTES <- 65536L

# GitHub Actions workflow rules, matched on the text of each .yml or .yaml file in
# .github/workflows. Case-sensitive, as measured, unless the pattern says (?i).
WORKFLOW_TEXT_RULES <- list(
  rcmdcheck      = "check-r-package|rcmdcheck|R CMD check|devtools::check|BiocCheck|rworkflows",
  rcmdcheck_name = "(?i)r-?cmd-?check|^check|cran-check|bioc|rworkflows",
  platforms      = "(?i)\\b(ubuntu|macos|windows)-(latest|[0-9])",
  r_devel        = "r(-version)?:\\s*['\"]?devel",
  coverage       = "covr::|codecov|test-coverage|coveralls",
  site_deploy    = paste0("build_site_github_pages|pkgdown::deploy|github-pages-deploy-action|",
                          "actions/deploy-pages|peaceiris/actions-gh-pages|altdoc::render|",
                          "quarto publish|quarto-actions/publish"),
  lint           = "lintr::|lint_package|jarl|air format|styler::",
  lint_skip      = "issue_comment")

# Every vcs_dev_tooling column not in DEV_TOOLING_MARKERS: its SQLite type, where the value
# comes from (tree, graphql, workflow_text or derived) and the rule vcs_dev_tooling_rules publishes.
DEV_TOOLING_DERIVED <- list(
  list(col = "readme_source", type = "TEXT", source = "tree",
       rule = "README.qmd, else README.Rmd, else README.md at root, else none"),
  list(col = "has_ci", type = "INTEGER", source = "derived",
       rule = "any of ci_github_actions to ci_drone is 1"),
  list(col = "package_at_root", type = "INTEGER", source = "derived", rule = "DESCRIPTION at root"),
  list(col = "ci_travis_only", type = "INTEGER", source = "derived",
       rule = "ci_travis is 1 and every other CI configuration column is 0"),
  list(col = "has_code_of_conduct", type = "INTEGER", source = "derived",
       rule = "1 when coc_source is repo or account_default, 0 when none"),
  list(col = "coc_source", type = "TEXT", source = "derived", paths = COC_TREE_PATHS,
       rule = "from GitHub's code of conduct url: repo inside the repository, account_default in the owner's .github repository, none when GitHub returns none"),
  list(col = "has_contributing", type = "INTEGER", source = "derived",
       rule = "1 when contributing_source is repo or account_default, 0 when none"),
  list(col = "contributing_source", type = "TEXT", source = "derived", paths = CONTRIBUTING_TREE_PATHS,
       rule = "from GitHub's contributing guide url: repo inside the repository, account_default in the owner's .github repository, none when GitHub returns none"),
  list(col = "has_pr_template", type = "INTEGER", source = "derived",
       rule = "1 when pr_template_source is repo or account_default, 0 when none"),
  list(col = "pr_template_source", type = "TEXT", source = "derived", paths = PR_TEMPLATE_TREE_PATHS,
       rule = "from GitHub's pull request templates: repo when one belongs to the repository, account_default when one belongs to the owner's .github repository, none when there are none"),
  list(col = "rbuildignore_excluded", type = "TEXT", source = "derived",
       rule = "items present in the repository that .Rbuildignore leaves out of the release, read as R CMD build reads it"),
  list(col = "rbuildignore_text", type = "TEXT", source = "graphql",
       rule = ".Rbuildignore text up to 65536 bytes"),
  list(col = "ci_workflow_files", type = "TEXT", source = "workflow_text",
       rule = ".yml and .yaml file names in .github/workflows, in listing order"),
  list(col = "ci_rcmdcheck", type = "INTEGER", source = "workflow_text",
       rule = paste("a workflow text matches", WORKFLOW_TEXT_RULES$rcmdcheck,
                    "or a workflow GitHub returns no text for is named", WORKFLOW_TEXT_RULES$rcmdcheck_name)),
  list(col = "ci_platforms", type = "TEXT", source = "workflow_text",
       rule = paste("linux, macos and windows as named by", WORKFLOW_TEXT_RULES$platforms,
                    "in the R CMD check workflows")),
  list(col = "ci_r_devel", type = "INTEGER", source = "workflow_text",
       rule = paste("an R CMD check workflow matches", WORKFLOW_TEXT_RULES$r_devel)),
  list(col = "ci_coverage", type = "INTEGER", source = "workflow_text",
       rule = paste("a workflow text matches", WORKFLOW_TEXT_RULES$coverage)),
  list(col = "ci_site_deploy", type = "INTEGER", source = "workflow_text",
       rule = paste("a workflow text matches", WORKFLOW_TEXT_RULES$site_deploy)),
  list(col = "ci_lint", type = "INTEGER", source = "workflow_text",
       rule = paste("a workflow text without", WORKFLOW_TEXT_RULES$lint_skip, "matches", WORKFLOW_TEXT_RULES$lint)),
  list(col = "has_pages", type = "INTEGER", source = "graphql",
       rule = "a github-pages environment or deployment"),
  list(col = "pages_last_deploy", type = "TEXT", source = "graphql",
       rule = "createdAt of the newest github-pages deployment"),
  list(col = "pages_url", type = "TEXT", source = "derived",
       rule = "the deployment's http(s) url when its host does not end in .github.io, else built from the repository's current name"),
  list(col = "site_generator", type = "TEXT", source = "derived",
       rule = "pkgdown when a built pkgdown.yml is found, else altdoc, litedown, pkgdown or quarto from their config, else unknown"),
  list(col = "site_pkgdown_source", type = "TEXT", source = "graphql",
       rule = "gh-pages when gh-pages:pkgdown.yml exists, else docs when HEAD:docs/pkgdown.yml exists"),
  list(col = "site_pkgdown_version", type = "TEXT", source = "graphql",
       rule = "the pkgdown: line of the built pkgdown.yml"),
  list(col = "site_pkgdown_last_built", type = "TEXT", source = "graphql",
       rule = "the last_built: line of the built pkgdown.yml, as written"),
  list(col = "site_url", type = "TEXT", source = "graphql",
       rule = "the reference url in the urls block of the built pkgdown.yml, else the article url, without its last path segment"),
  list(col = "repo_desc_package", type = "TEXT", source = "graphql", rule = "Package in HEAD:DESCRIPTION"),
  list(col = "repo_desc_version", type = "TEXT", source = "graphql", rule = "Version in HEAD:DESCRIPTION"),
  list(col = "cran_version_at_scan", type = "TEXT", source = "derived",
       rule = "the CRAN version of repo_desc_package when that name is one of the repository's CRAN packages"),
  list(col = "repo_version_vs_cran", type = "TEXT", source = "derived",
       rule = "ahead, equal or behind: compareVersion(repo_desc_version, cran_version_at_scan)"),
  list(col = "funding_links", type = "TEXT", source = "graphql", rule = "fundingLinks as platform and url pairs"),
  list(col = "owner_sponsorable", type = "INTEGER", source = "graphql", rule = "the owner has a GitHub Sponsors listing"),
  list(col = "is_fork", type = "INTEGER", source = "graphql", rule = "isFork"),
  list(col = "parent_name_with_owner", type = "TEXT", source = "graphql", rule = "parent nameWithOwner"),
  list(col = "has_issues_enabled", type = "INTEGER", source = "graphql", rule = "hasIssuesEnabled"),
  list(col = "homepage_url", type = "TEXT", source = "graphql", rule = "homepageUrl, trimmed, when not empty"),
  list(col = "has_discussions", type = "INTEGER", source = "graphql", rule = "hasDiscussionsEnabled"),
  list(col = "discussions_total", type = "INTEGER", source = "graphql", rule = "discussions totalCount"))
# v1 first scan 2026-07-18 (d115e2d), v2 00312fe, b903376, f861918 (2026-07-29 to 08-02), v3 this change.
DEV_TOOLING_RULESET_VERSION <- "v3 (2026-09-26)"

# The ruleset in which every repository's commits began to be read each week. Rules
# added or revised then carry it as since_ruleset. The day this reached main.
AI_RULESET_UNGATED <- "2026-09-27"

# Commits by a tool's accounts. graphql: counted by one GraphQL history filter per tool.
# rest_only: counted by REST author-email. The filter resolves 41898282+ to github-actions[bot],
# and its <id>+ form misses commits written with a bot's id-less address.
# linked: addresses the graphql count already includes, matched in the commit read and
# never counted on their own. names: bot author names.
AI_ACCOUNTS <- list(
  list(tool = "claude",
       graphql = c("noreply@anthropic.com", "209825114+claude[bot]@users.noreply.github.com",
                   "242468646+Claude@users.noreply.github.com"),
       rest_only = c("41898282+claude[bot]@users.noreply.github.com", "claude[bot]@users.noreply.github.com"),
       linked = character(0), names = "claude[bot]"),
  list(tool = "copilot", graphql = "198982749+Copilot@users.noreply.github.com",
       rest_only = character(0), linked = character(0), names = "copilot-swe-agent[bot]"),
  list(tool = "cursor",
       graphql = c("cursoragent@cursor.com", "composer@anysphere.co",
                   "206951365+cursor[bot]@users.noreply.github.com"),
       rest_only = "cursor[bot]@users.noreply.github.com", linked = character(0), names = "cursor[bot]"),
  list(tool = "devin", graphql = "158243242+devin-ai-integration[bot]@users.noreply.github.com",
       rest_only = character(0), linked = character(0), names = "devin-ai-integration[bot]"),
  list(tool = "jules", graphql = "161369871+google-labs-jules[bot]@users.noreply.github.com",
       rest_only = character(0), linked = character(0), names = "google-labs-jules[bot]"),
  list(tool = "openhands", graphql = "openhands@all-hands.dev",
       rest_only = character(0), linked = character(0), names = character(0)),
  list(tool = "amazonq", graphql = "208079219+amazon-q-developer[bot]@users.noreply.github.com",
       rest_only = character(0), linked = character(0), names = "amazon-q-developer[bot]"),
  list(tool = "codex",
       graphql = c("242516109+Codex@users.noreply.github.com", "267193182+codex@users.noreply.github.com"),
       rest_only = character(0), linked = character(0), names = character(0)),
  list(tool = "kiro",
       graphql = c("244629292+kiro-agent@users.noreply.github.com",
                   "245459735+kiro-agent[bot]@users.noreply.github.com"),
       rest_only = character(0), linked = character(0), names = "kiro-agent[bot]"),
  list(tool = "junie", graphql = "junie@jetbrains.com",
       rest_only = character(0), linked = character(0), names = character(0)),
  list(tool = "replit", graphql = "agent@replit.com",
       rest_only = character(0), linked = character(0), names = character(0)),
  # Roo Code's cloud agent, counted under Roo Code.
  list(tool = "roo",
       graphql = c("301996811+roomote-roomote[bot]@users.noreply.github.com",
                   "263205322+roomote[bot]@users.noreply.github.com", "roomote@roomote.dev"),
       rest_only = character(0), linked = character(0), names = character(0)),
  list(tool = "amp", graphql = "amp@ampcode.com",
       rest_only = character(0), linked = character(0), names = character(0))
)
# Account ids behind every <id>+ address above, each checked on the REST users API.
AI_VERIFIED_ACCOUNT_IDS <- c(209825114, 242468646, 198982749, 206951365, 158243242, 161369871,
                             208079219, 242516109, 267193182, 244629292, 245459735, 301996811,
                             263205322)
# Repositories per account-count query.
AI_ACCOUNT_BATCH <- 10L
# Non-AI bots that must never be flagged (backstops the allowlist).
AI_BOT_DENYLIST <- c(
  "dependabot[bot]", "renovate[bot]", "github-actions[bot]", "pre-commit-ci[bot]",
  "codecov[bot]", "allcontributors[bot]", "web-flow", "lintr-bot", "styler-bot"
)
# Accounts that open pull requests, spelled as GraphQL returns author { login } (no
# "[bot]"). Bare "copilot", "devin", "jules", "amp", "kiro" and "junie" are people.
AI_PR_AGENT_LOGINS <- c(
  "copilot-swe-agent"          = "copilot",
  "devin-ai-integration"       = "devin",
  "google-labs-jules"          = "jules",
  "cursor"                     = "cursor",
  "openhands-agent"            = "openhands",
  "anthropic-code-agent"       = "claude",
  "claude"                     = "claude",     # Anthropic's own account, user 81847
  "amazon-q-developer"         = "amazonq",
  "openai-code-agent"          = "codex",
  "kiro-agent"                 = "kiro"
)
# Pull requests a tool wrote that a person opened, matched on the raw branch or description,
# case-sensitive unless (?i). names = FALSE names no tool, only admits the repository that week.
AI_PR_RULES <- list(
  list(key = "pr.cursor.agent-body", rev = 1L, since_ruleset = AI_RULESET_UNGATED, tool = "cursor",
       field = "body", names = TRUE, min_created = "2025-05-01",
       pattern = "^\\s*<!-- CURSOR_AGENT_PR_BODY_BEGIN -->|cursor\\.com/(agents/bc-|agents\\?id=bc-|background-agent\\?bcId=)"),
  # The hex suffix and date floor keep out older branches about a text cursor.
  list(key = "pr.cursor.agent-branch", rev = 1L, since_ruleset = AI_RULESET_UNGATED, tool = "cursor",
       field = "head", names = TRUE, min_created = "2025-05-01",
       pattern = "^cursor/[A-Za-z0-9._-]+-[0-9a-f]{4}$"),
  list(key = "pr.cursor.made-with", rev = 1L, since_ruleset = AI_RULESET_UNGATED, tool = "cursor",
       field = "body", names = TRUE, pattern = "(?i)made with \\[cursor\\]\\(https://cursor\\.com\\)"),
  list(key = "pr.claude.branch", rev = 1L, since_ruleset = AI_RULESET_UNGATED, tool = "claude",
       field = "head", names = TRUE, pattern = "^claude/"),
  list(key = "pr.claude.session", rev = 1L, since_ruleset = AI_RULESET_UNGATED, tool = "claude",
       field = "body", names = TRUE, pattern = "claude\\.ai/code/session_"),
  list(key = "pr.claude.footer", rev = 1L, since_ruleset = AI_RULESET_UNGATED, tool = "claude",
       field = "body", names = TRUE, pattern = "Generated with \\[Claude Code\\]"),
  list(key = "pr.codex.task", rev = 1L, since_ruleset = AI_RULESET_UNGATED, tool = "codex",
       field = "body", names = TRUE, pattern = "chatgpt\\.com/codex/tasks/task_"),
  list(key = "pr.codex.footer", rev = 1L, since_ruleset = AI_RULESET_UNGATED, tool = "codex",
       field = "body", names = TRUE, pattern = "Generated with \\[Codex\\]\\(https://openai\\.com/codex/?\\)"),
  list(key = "pr.codex.branch", rev = 1L, since_ruleset = AI_RULESET_UNGATED, tool = "codex",
       field = "head", names = FALSE, pattern = "^codex/"),
  list(key = "pr.devin.session", rev = 1L, since_ruleset = AI_RULESET_UNGATED, tool = "devin",
       field = "body", names = TRUE, pattern = "app\\.devin\\.ai/sessions/"),
  list(key = "pr.devin.branch", rev = 1L, since_ruleset = AI_RULESET_UNGATED, tool = "devin",
       field = "head", names = TRUE, pattern = "^devin/[0-9]{10}-"),
  list(key = "pr.openhands.branch", rev = 1L, since_ruleset = AI_RULESET_UNGATED, tool = "openhands",
       field = "head", names = TRUE, pattern = "^openhands-fix-(issue|pr)-[0-9]+"),
  list(key = "pr.amazonq.branch", rev = 1L, since_ruleset = AI_RULESET_UNGATED, tool = "amazonq",
       field = "head", names = TRUE, pattern = "^Q-DEV-issue-[0-9]+-[0-9]+")
)
# Tier B commit-message trailers. Anchored to the canonical bot identity so a
# human named Claude is rejected. Matched case-insensitively.
# `pattern` is the regex a fetched message is VERIFIED against; `query` is the literal
# phrase handed to the commit-search API, which does no regex. A search hit whose message
# fails the pattern is a fuzzy candidate and is recorded as a censored floor, never as an
# exact onset, so a person named Claude cannot mint an immutable date.
AI_TRAILER_PATTERNS <- list(
  # The name carries a model between "Claude" and the address in practice:
  # sampling four roster repositories returned 79 trailers, of which 79 were
  # "Claude Sonnet 4.5", "Claude Opus 4.8 (1M context)" and the like, and none
  # were the bare form this pattern used to demand. Every tier-B hit was failing
  # verification and taking a floor date instead of an exact one. [^<\n] keeps
  # the match on one line and still requires the name to begin with Claude, so
  # "Claudia" at the same address does not qualify.
  list(key = "msg.claude.coauthor", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       pattern = "co-authored-by:\\s*claude\\b[^<\\n]*<noreply@anthropic\\.com>", tool = "claude",
       query = "\"Co-Authored-By: Claude\""),
  list(key = "msg.claude.generated", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       pattern = "generated with \\[?claude code",                          tool = "claude",
       query = "\"Generated with Claude Code\""),
  # There is deliberately no "generated by Replit" rule. Every one of the six
  # occurrences found in real commits is a maintainer describing Replit, not
  # Replit signing: "Removed extra directories generated by Replit", "Ignore
  # Replit config files (auto-generated by Replit environment)". The phrase is a
  # pure false-positive generator, and anchoring it to a line start only hid
  # that. Replit-Commit-Author below is the actual signature.
  list(key = "msg.replit.author", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       pattern = "replit-commit-author:",                                   tool = "replit",
       query = "\"Replit-Commit-Author:\""),
  # Codex signs with a plain name and an OpenAI address; the name alone is far
  # too common to key on. Sampling 100 real commits carrying a Codex trailer
  # returned 1,134 trailer lines across eight shapes, of which only 23 said
  # "codex-cli" and the rest were the name plus an address this rule never saw.
  # The address is the anchor, as it is for Claude. codex@agent appears too and
  # is kept separate rather than widened into "any address", which would match a
  # person who happens to be called Codex.
  list(key = "msg.codex.coauthor", rev = 2L, since_ruleset = AI_RULESET_UNGATED, search = "always",
       pattern = "co-authored-by:\\s*codex[^<\\n]*<[^>\\n]*@(openai\\.com|agent|local)>", tool = "codex",
       query = "\"Co-Authored-By: Codex\""),
  list(key = "msg.codex.cli", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       pattern = "codex-cli",                                               tool = "codex",
       query = "\"codex-cli\""),

  # Every rule below keys on an address, a bot account or a line the tool writes, never on a name
  # alone: Devin's search also returns Devin Logan and devin.logan, and Jules is a person's name.
  list(key = "msg.cursor.agent-coauthor", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       pattern = "co-authored-by:[^<\\n]*<cursoragent@cursor\\.com>",           tool = "cursor",
       query = "\"cursoragent@cursor.com\""),
  list(key = "msg.cursor.at-agent", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       pattern = "co-authored-by:\\s*cursor[^<\\n]*<cursor@agent>",              tool = "cursor",
       query = "\"Co-Authored-By: Cursor\""),
  list(key = "msg.devin.bot-coauthor", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       pattern = "co-authored-by:[^<\\n]*<[^>\\n]*devin-ai-integration\\[bot\\]@",  tool = "devin",
       query = "\"devin-ai-integration\""),
  list(key = "msg.openhands.coauthor", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       pattern = "co-authored-by:[^<\\n]*<[^>\\n]*@all-hands\\.dev>",           tool = "openhands",
       query = "\"all-hands.dev\""),
  list(key = "msg.jules.coauthor", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       pattern = "co-authored-by:[^<\\n]*<[^>\\n]*google-labs-jules\\[bot\\]@",     tool = "jules",
       query = "\"google-labs-jules\""),
  list(key = "msg.windsurf.coauthor", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       pattern = "co-authored-by:[^<\\n]*<[^>\\n]*@windsurf\\.(ai|com)>",       tool = "windsurf",
       query = "\"Co-Authored-By: Windsurf\""),
  list(key = "msg.windsurf.bot", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       pattern = "co-authored-by:[^<\\n]*<[^>\\n]*windsurf-bot\\[bot\\]@",          tool = "windsurf",
       query = "\"windsurf-bot\""),
  # Antigravity signs with Google addresses, which the Gemini rules match only beside Gemini's
  # name or gemini-cli. The lookahead leaves out an "Antigravity Bot" credit of unknown origin.
  list(key = "msg.antigravity.coauthor", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "always",
       pattern = "co-authored-by:\\s*antigravity[^<\\n]*<(?!bot@antigravity\\.ai>)[^>\\n]*@([a-z0-9.-]+\\.)?(google\\.com|antigravity\\.(ai|dev))>",
       tool = "antigravity", query = "\"Co-Authored-By: Antigravity\""),
  # A Gemini credit at a google.com address must start with Gemini's name, since the address
  # is generic. An address naming gemini-cli is distinctive on its own.
  list(key = "msg.gemini.coauthor", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       pattern = "co-authored-by:\\s*gemini[^<\\n]*<[^>\\n]*@google\\.com>",  tool = "gemini",
       query = "\"Co-Authored-By: Gemini\""),
  # Gemini Code Assist accepting a review suggestion is a review credit (AI_REVIEW_RULES).
  list(key = "msg.gemini.bot", rev = 2L, since_ruleset = AI_RULESET_UNGATED, search = "always",
       pattern = "co-authored-by:[^<\\n]*<[^>\\n]*gemini-cli\\[?[^>\\n]*@", tool = "gemini",
       query = "\"gemini-cli\""),
  # Aider names the provider and model in the parentheses. The address is the
  # anchor; the parenthetical is what Addendum 2 of the design would read.
  list(key = "msg.aider.coauthor", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       pattern = "co-authored-by:[^<\\n]*<aider@aider\\.chat>",                 tool = "aider",
       query = "\"aider@aider.chat\""),
  list(key = "msg.claude.address", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "always",
       pattern = "co-authored-by:[^<\\n]*<noreply@anthropic\\.com>", tool = "claude",
       query = "\"noreply@anthropic.com\""),
  list(key = "msg.claude.session", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "always",
       pattern = "(^|\\n)(claude-session:\\s*)?https://claude\\.ai/code/session_", tool = "claude",
       query = "\"claude.ai/code/session_\""),
  list(key = "msg.cursor.made-with", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "always",
       pattern = "(^|\\n)made-with:\\s*cursor\\s*(\\n|$)", tool = "cursor",
       query = "\"Made-with: Cursor\""),
  # Copilot CLI, SDK and Desktop.
  list(key = "msg.copilot.cli", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "always",
       pattern = "co-authored-by:[^<\\n]*<223556219\\+copilot@users\\.noreply\\.github\\.com>", tool = "copilot",
       query = "\"223556219+Copilot\""),
  # VS Code 1.117 wrote this line without Copilot use from 2026-04-22 to 2026-05-06.
  list(key = "msg.copilot.vscode", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "always",
       pattern = "co-authored-by:\\s*copilot[^<\\n]*<copilot@github\\.com>", tool = "copilot",
       query = "\"copilot@github.com\""),
  list(key = "msg.copilot.cloud-coauthor", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "always",
       pattern = "co-authored-by:[^<\\n]*<198982749\\+copilot@", tool = "copilot",
       query = "\"198982749+Copilot\""),
  list(key = "msg.devin.generated", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "always",
       pattern = "(^|\\n)generated with \\[?devin", tool = "devin",
       query = "\"Generated with Devin\""),
  list(key = "msg.devin.cognition", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "always",
       pattern = "co-authored-by:\\s*devin[^<\\n]*<devin@cognition\\.ai>", tool = "devin",
       query = "\"devin@cognition.ai\""),
  list(key = "msg.windsurf.cascade", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "always",
       pattern = "co-authored-by:\\s*(windsurf|cascade)[^<\\n]*<[^>\\n]*@(windsurf\\.(ai|com)|codeium\\.com)>",
       tool = "windsurf", query = "\"codeium.com\""),
  # One line naming any tool: AI_ASSISTED_BY_TOOLS maps it, and its search spans every tool.
  list(key = "msg.any.assisted-by", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "always",
       pattern = "(^|\\n)assisted-by:\\s*(.+)", tool = "any", query = "\"Assisted-by:\""),
  # Watched credits, asked only where the weekly read matched them.
  list(key = "msg.corteza.address", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "on_window_hit",
       pattern = "<noreply@cornball\\.ai>", tool = "corteza", query = "\"noreply@cornball.ai\""),
  list(key = "msg.eca.address", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "on_window_hit",
       pattern = "<git@eca\\.dev>", tool = "eca", query = "\"git@eca.dev\""),
  list(key = "msg.warp.address", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "on_window_hit",
       pattern = "<[^>\\n]*@warp\\.dev>", tool = "warp", query = "\"warp.dev\""),
  list(key = "msg.qwen.address", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "on_window_hit",
       pattern = "<qwen-coder@alibabacloud\\.com>", tool = "qwen", query = "\"qwen-coder@alibabacloud.com\""),
  list(key = "msg.crush.address", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "on_window_hit",
       pattern = "<crush@charm\\.land>", tool = "crush", query = "\"crush@charm.land\""),
  list(key = "msg.amp.address", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "on_window_hit",
       pattern = "<amp@ampcode\\.com>", tool = "amp", query = "\"amp@ampcode.com\""),
  list(key = "msg.vibe.address", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "on_window_hit",
       pattern = "<vibe@mistral\\.ai>", tool = "vibe", query = "\"vibe@mistral.ai\""),
  list(key = "msg.factory.bot", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "on_window_hit",
       pattern = "factory-droid\\[bot\\]", tool = "factory", query = "\"factory-droid\""),
  list(key = "msg.grok.address", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "on_window_hit",
       pattern = "<(grok@|noreply@)x\\.ai>", tool = "grok", query = "\"x.ai\""),
  list(key = "msg.kimi.address", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "on_window_hit",
       pattern = "<noreply@moonshot\\.ai>", tool = "kimi", query = "\"noreply@moonshot.ai\""),
  list(key = "msg.continue.address", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "on_window_hit",
       pattern = "<noreply@continue\\.dev>", tool = "continue", query = "\"noreply@continue.dev\""),
  list(key = "msg.augment.address", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "on_window_hit",
       pattern = "<noreply@augmentcode\\.com>", tool = "augment", query = "\"noreply@augmentcode.com\""),
  list(key = "msg.opencode.address", rev = 1L, since_ruleset = AI_RULESET_UNGATED, search = "on_window_hit",
       pattern = "<noreply@opencode\\.ai>", tool = "opencode", query = "\"noreply@opencode.ai\"")
)
# Author-name suffixes. `query` searches the author field rather than the message.
AI_AUTHOR_SUFFIXES <- list(
  list(key = "name.aider.suffix", rev = 1L, since_ruleset = "2026-09-19", search = "always",
       suffix = "(aider)", tool = "aider", query = "author-name:\"(aider)\"")
)
# What an Assisted-by line names, matched case-insensitively, first match wins.
AI_ASSISTED_BY_TOOLS <- c(
  "\\bclaude\\b|noreply@anthropic\\.com" = "claude",
  "\\bcodex\\b|@openai\\.com"            = "codex",
  "\\bgemini\\b"                         = "gemini",
  "\\bcopilot\\b"                        = "copilot",
  "\\bcursor\\b"                         = "cursor",
  "\\bcrush\\b|crush@charm\\.land"       = "crush",
  "\\baider\\b"                          = "aider",
  "\\bopencode\\b"                       = "opencode")
# Review bots' credits on commits that accepted a suggestion. The lookahead sits right
# after the colon so a Copilot Autofix credit, a security fix, never matches.
AI_REVIEW_RULES <- list(
  list(key = "review.gemini-code-assist.coauthor", rev = 1L, since_ruleset = AI_RULESET_UNGATED,
       search = "on_window_hit", tool = "gemini-code-assist",
       pattern = "co-authored-by:[^<\\n]*<[^>\\n]*gemini-code-assist\\[bot\\]@", query = "\"gemini-code-assist\""),
  list(key = "review.copilot.suggestion", rev = 1L, since_ruleset = AI_RULESET_UNGATED,
       search = "on_window_hit", tool = "copilot-review",
       pattern = "co-authored-by:(?![ \\t]*copilot autofix)[^<\\n]*<175728472\\+copilot@users\\.noreply\\.github\\.com>",
       query = "\"175728472+Copilot\"")
)
# Dates on which a VS Code Copilot credit says nothing (microsoft/vscode#314311).
AI_COPILOT_VSCODE_FALSE_WINDOW <- c("2026-04-22", "2026-05-06")
# Renamed-marker predecessors (new path -> old path) probed so a rename does
# not reset onset.
AI_MARKER_PREDECESSORS <- c(".cursor" = ".cursorrules")
# Detection ruleset version, surfaced by the viewer methods note.
AI_RULESET_VERSION <- AI_RULESET_UNGATED
# The page note a ruleset's first publish carries, by ruleset version. A version not
# named here carries none.
AI_RULESET_CHANGE_KEYS <- stats::setNames("ungated-weekly-read", AI_RULESET_UNGATED)
# Tie-break order between ways a tool was found (lower sorts first).
TIER_PRIORITY <- c(A = 1L, B = 2L, C = 3L, PR = 4L, PB = 5L, D = 6L)

# Pacing for the REST commit-search API (search/commits, ~1,800/hr = 30/min),
# a budget separate from the GraphQL 5000/hr and from core REST. Each onset
# search sleeps this long after its request so a gated deep scan stays under it.
# Commit search enforces a SECONDARY rate limit far tighter than the documented
# ~30/min, and hand-testing tripped it after five queries. At 2s the first backfill
# issued about 12,000 searches, was refused by almost all of them, and recorded the
# refusals as "no trailer found". Pace for the limit that actually exists.
SEARCH_DELAY_S <- 12
# A refused search is asked again this many times, never waiting longer than this per try.
AI_SEARCH_RETRIES <- 3L
AI_SEARCH_MAX_WAIT_S <- 120
# Subtrees the contents query lists one level of, alias to path. build_tree_query and
# parse_tree_markers both iterate this; a path under .github/ lands in github_entries.
TREE_SUBTREES <- c(workflowsTree = ".github/workflows", claudeTree = ".claude", agentsTree = ".agents",
                   instTree = "inst", vignettesTree = "vignettes", siteTree = "site",
                   githubAgentsTree = ".github/agents", positTree = ".posit",
                   positaiTree = ".positai", geminiTree = ".gemini")
# The contents query canary: one floor per set, met when any one candidate meets it.
TREE_QUERY_CANARY <- list(
  own_community = c("tidyverse/forcats", "tidyverse/dplyr", "r-lib/usethis", "easystats/insight"),
  inherited_pr_template = c("epiverse-trace/linelist", "epiverse-trace/epiparameter", "ecmwf/eccodes"))
# Repositories per contents and activity document in the cheap pass, kept small because both are
# heavy server-side (a tree fetch, or 50 pull requests and 100 commits per repository).
TIER_D_BATCH <- 10L
# Agent-era boundary. AI coding agents did not open PRs before this date, so an
# allowlisted agent login on an earlier PR is a login collision, not adoption: it
# contributes no PR evidence and no PR onset. Full ISO date, compared lexicographically
# against createdAt (ISO instants sort correctly as strings).
AI_PR_CUTOFF <- "2023-01-01"
# The weekly commit read starts this many days before the last one, for merge commits
# that bring older committer dates in.
AI_COMMIT_OVERLAP_DAYS <- 14L
# Extra 100-commit pages one repository may read in a week before the gap is recorded,
# asked for this many repositories per query.
AI_COMMIT_PAGE_CAP <- 5L
AI_COMMIT_PAGE_BATCH <- 5L
# The one-off walk through older pull requests: repositories per query, points per shard.
AI_PR_WALK_BATCH <- 5L
AI_PR_WALK_POINTS <- 300L
# GraphQL points left unspent as headroom for the cheap and deep passes, mirroring
# POINT_RESERVE (the daily pass's reserve). The cheap pass's PR query
# (pullRequests(first: 50) per alias) is not the ~1-point-per-batch the tree query is, so
# run_cheap and run_deep both check graphql_rate_remaining(io) against this reserve
# before spending down the shared token, pausing rather than faulting when it is low.
AI_POINT_RESERVE <- 1500L
# Repositories whose answers the weekly documents must reproduce before a run reads
# anything. Floors only, since counts only grow.
AI_QUERY_CANARY <- list(
  accounts = c("ss3sim/ss3sim", "johnpaulgosling/addivortes"),
  activity = c("ericrayanderson/shinyglass", "ss3sim/ss3sim"),
  prs = list(c("ericrayanderson/shinyglass", "49"), c("apache/arrow-nanoarrow", "927")),
  commits = list(c("xrobin/pROC", "fe5c63c197db1fe8ce70eb740bb5ffe4af0f4e99"),
                 c("alyssafrazee/ballgown", "ab1da7b7b32be605c5f291a7ace100a0e65e08f6")),
  addivortes_cursor_floor = 19L)
# A repository that still fails alone after halving is read once more after this wait.
AI_BATCH_RETRY_WAIT_S <- 30
# Consecutive identical single-repository failures that end one document's reads in a shard.
AI_BREAKER_LIMIT <- 20L
# A cheap shard stops when its contents failures reach both the count and the share of repositories it attempted.
TREE_DROP_MIN <- 5L
TREE_DROP_MAX_SHARE <- 0.05
# The merge publishes, then fails the run, when distinct failed repositories exceed this share of the roster.
AI_SCAN_FAILURE_MAX_SHARE <- 0.02

# Channels known to be silent, each with the evidence someone gathered and the
# date they gathered it. The canary reports every (tier, tool) that has a rule
# and no detection anywhere on the roster; an entry here is a dated claim about
# one of those zeros, not a way to make the report quiet.
#
# status = "genuine": we looked and the zero is real.
# status = "open":    we looked, the zero is not yet explained, and the reason
#                     says what would settle it. These are re-reported every run
#                     so they cannot rot into silence, which is the whole thing
#                     this table exists to prevent.
#
# An entry is retired the moment the channel detects. Six were carried here long
# after the data had answered them, because the canary counted the rows in this
# file instead of the channels actually at zero: B/cursor, B/gemini, B/jules,
# B/openhands and A/jules had been detecting for weeks, and B/devin's tier-B
# trailer matched on 2026-08-16, on a repository the PR channel had already
# flagged, exactly as the entry predicted it eventually would. All six are gone
# from this list. If any of them falls back to zero it will arrive as an
# unexplained channel and fail the merge, which is the right way to hear about a
# detection that broke.
# Below this many repos in the merged roster, a channel at zero is not evidence
# of anything and the canary stands down. Production merges ~15,000; fixtures
# merge a handful.
# One page of commit-search results. The scan already issues this request and
# already pays for it; asking for a page rather than a single hit turns the same
# response into the repository's model history. Measured at 486ms for one item
# and 508ms for twelve, against the same throttled budget.
AI_SEARCH_PAGE <- 100L

AI_CANARY_MIN_ROSTER <- 200L

# Vendor facts in these reasons, each checked on 2026-09-26 at the page named.
# Kiro CLI replaced the Amazon Q Developer CLI in November 2025: https://kiro.dev/docs/upgrade-guides/migrating-from-q/
# Windsurf became Devin Desktop on 2026-06-02, rules in .devin/rules: https://docs.devin.ai/desktop/devin-desktop-faq
# Grok Build reads AGENTS.md, CLAUDE.md and .grok/rules, not GROK.md: https://github.com/xai-org/grok-build/blob/HEAD/crates/codegen/xai-grok-tools/src/types/compat.rs
# Junie reads AGENTS.md first, .junie/guidelines.md is its legacy format: https://junie.jetbrains.com/docs/guidelines-and-memory.html
# The Roo Code extension shut down on 2026-05-15, the day its repository was archived: https://github.com/RooCodeInc/Roo-Code (README)
AI_SILENT_CHANNELS_KNOWN <- read.csv(text = trimws(r"(
tier,tool,status,reason,recorded_on
A,devin,genuine,"No repository has a commit by Devin's account (devin-ai-integration[bot]) on its default branch. Commits by it are counted in every repository each week, and a check of all 15,875 on 2026-09-25 found none. Devin shows up through pull requests instead: its account opened 6 in xsdm-devel, and maintainers opened pull requests from a Devin session in sobol and maxentcpp.",2026-09-25
B,replit,genuine,"No commit crediting Replit has been found. Searched on 2026-07-31 in the 1,964 repositories found by then, and every repository's new commits are now read each week. No repository has a .replit, replit.nix or replit.md file, and the newest 100 commits of 600 sampled repositories had no Replit-Commit-Author line on 2026-09-25.",2026-09-25
B,windsurf,genuine,"Every public commit crediting Windsurf was listed on 2026-09-25: 963 naming Windsurf, 552 naming its bot account (windsurf-bot) and 76 at codeium.com, and none is in a repository we scan. Every repository's new commits are now read each week. Windsurf became Devin Desktop on 2026-06-02 and keeps its rules in .devin/rules, which this page lists under Devin.",2026-09-25
D,amazonq,genuine,"No repository has an .amazonq folder, checked across 15,875. Amazon Q is found another way: its account opened a pull request in ss3sim, merged in May 2025, and made 2 commits there. Its command-line tool has been Kiro since November 2025, which this page lists as its own tool.",2026-09-25
D,grok,genuine,"No GROK.md, .grok or .xai in any repository, checked across 15,875. xAI's Grok Build reads AGENTS.md, CLAUDE.md and .grok/rules rather than GROK.md, so a repository using it may show only as AGENTS.md or CLAUDE.md.",2026-09-25
D,junie,genuine,"No .junie folder in any repository, no commit by Junie's account (junie@jetbrains.com) on any default branch, and no pull request by it among each repository's newest 50, checked across 15,875. Junie now reads AGENTS.md first, so a repository using it may show only as AGENTS.md.",2026-09-25
D,roo,genuine,"No .roo, .roomodes, .roorules or .rooignore in any repository, checked across 15,875, and none of the 218 public commits by Roo's cloud agent account (roomote[bot]) is in a repository we scan. The Roo Code extension was shut down on 2026-05-15, so this will not change.",2026-09-25
PR,cursor,genuine,"No pull request among each repository's newest 50 was opened by Cursor's app account, checked across 161,444 pull requests. Cursor's cloud agent opens pull requests under the maintainer's own account, and this page finds them by the note Cursor writes at the top of the description or the branch name it gives them.",2026-09-25
PR,openhands,genuine,"No pull request among each repository's newest 50 was opened by OpenHands's account (openhands-agent), checked across 161,444 pull requests.",2026-09-25
D,trae,genuine,"No .trae folder in any repository, checked across 15,875 on 2026-09-25. The files at the top of every repository are read each week.",2026-09-25
D,augment,genuine,"No .augment folder in any repository, checked across 15,875 on 2026-09-25. The files at the top of every repository are read each week.",2026-09-25
D,crush,genuine,"No CRUSH.md file in any repository, checked across 15,875 on 2026-09-25. The files at the top of every repository are read each week.",2026-09-25
D,goose,genuine,"No .goosehints file in any repository, checked across 15,875 on 2026-09-25. The files at the top of every repository are read each week.",2026-09-25
D,factory,genuine,"No .factory folder in any repository, checked across 15,875 on 2026-09-25. The files at the top of every repository are read each week.",2026-09-25
D,vibe,genuine,"No .vibe folder in any repository, checked across 15,875 on 2026-09-25. The files at the top of every repository are read each week.",2026-09-25
A,codex,genuine,"No commit by Codex's accounts (openai-code-agent[bot] and codex) has been found. Commits by them are counted in every repository each week.",2026-09-27
A,kiro,genuine,"No commit by Kiro's accounts (kiro-agent and kiro-agent[bot]) on any default branch, checked across 15,875 on 2026-09-25. Commits by them are counted in every repository each week.",2026-09-25
A,junie,genuine,"No commit by Junie's account (junie@jetbrains.com) on any default branch, checked across 15,875 on 2026-09-25. Commits by it are counted in every repository each week.",2026-09-25
A,replit,genuine,"No commit by Replit Agent's account (agent@replit.com) on any default branch, checked across 15,875 on 2026-09-25. Commits by it are counted in every repository each week.",2026-09-25
A,roo,genuine,"No commit by Roo Code's cloud agent accounts (roomote-roomote[bot], roomote[bot] and roomote@roomote.dev) has been found. The older roomote[bot] had none on any default branch across 15,875 on 2026-09-25, and all three are counted in every repository each week.",2026-09-25
A,amp,genuine,"No commit by Amp's account (amp@ampcode.com) has been found. Commits by it are counted in every repository each week.",2026-09-27
B,amp,genuine,"No commit crediting Amp (a co-author line at amp@ampcode.com) among 373,252 recent commits from 9,193 repositories, checked 2026-09-25. Every repository's new commits are read each week.",2026-09-25
B,vibe,genuine,"No commit crediting Mistral Vibe (a co-author line at vibe@mistral.ai) among 373,252 recent commits from 9,193 repositories, checked 2026-09-25. Every repository's new commits are read each week.",2026-09-25
B,factory,genuine,"No commit crediting Factory (a co-author line naming factory-droid[bot]) among 373,252 recent commits from 9,193 repositories, checked 2026-09-25. Every repository's new commits are read each week.",2026-09-25
B,grok,genuine,"No commit crediting Grok (a co-author line at an x.ai address) among 373,252 recent commits from 9,193 repositories, checked 2026-09-25. Every repository's new commits are read each week.",2026-09-25
B,kimi,genuine,"No commit crediting Kimi (a co-author line at noreply@moonshot.ai) among 373,252 recent commits from 9,193 repositories, checked 2026-09-25. Every repository's new commits are read each week.",2026-09-25
B,continue,genuine,"No commit crediting Continue (a co-author line at noreply@continue.dev) among 373,252 recent commits from 9,193 repositories, checked 2026-09-25. Every repository's new commits are read each week.",2026-09-25
B,augment,genuine,"No commit crediting Augment Code (a co-author line at noreply@augmentcode.com) among 373,252 recent commits from 9,193 repositories, checked 2026-09-25. Every repository's new commits are read each week.",2026-09-25
B,opencode,genuine,"No commit crediting OpenCode (a co-author line at noreply@opencode.ai) among 373,252 recent commits from 9,193 repositories, checked 2026-09-25. Every repository's new commits are read each week.",2026-09-25
PR,codex,genuine,"No pull request among each repository's newest 50 was opened by Codex's account (openai-code-agent), checked across 161,444 pull requests on 2026-09-25. Codex's cloud tasks reach a repository as a pull request a maintainer opens, which this page finds by the task link or the Generated with Codex line in its description.",2026-09-25
PR,kiro,genuine,"No pull request among each repository's newest 50 was opened by Kiro's account (kiro-agent), checked across 161,444 pull requests on 2026-09-25.",2026-09-25
PB,amazonq,genuine,"No pull request opened by a maintainer from a branch Amazon Q named (Q-DEV-issue-) has been found among the pull requests read in every repository each week. The one such branch found, in ss3sim, was opened by Amazon Q's own account.",2026-09-27
)"), stringsAsFactors = FALSE)

# Tables the summary shard carries beyond the five it takes as named arguments.
# Declared in one place because the export step used to name every table
# explicitly, so three tables added to the pipeline were created empty in the
# published database and never filled: a consumer reading them saw a table with
# no rows, which is indistinguishable from a table nothing has written yet.
#
# repo_package_links is here for the same round trip and has more riding on it
# than the others. Only the daily update writes it, every merge publishes it as
# seeded, and once a package has left CRAN or moved its URL it is the only record
# anywhere of which repository the package was. A path that dropped it would
# lose those links for good, because nothing resolves a delisted package again.
# vcs_repo_owner stays the last entry; tables added later go before it.
SUMMARY_EXTRA_TABLES <- c("vcs_ai_models", "vcs_ai_rule_inventory",
                          "vcs_ai_silent_channels", "repo_package_links",
                          "vcs_dev_tooling_rules",
                          "vcs_ai_repo_reads", "vcs_ai_account_counts", "vcs_ai_search_log",
                          "vcs_ai_search_coverage", "vcs_ai_review_signals",
                          "vcs_ai_outside_prs", "vcs_ai_ruleset_history",
                          "vcs_repo_owner")

# Package-to-repository links this pipeline published before it kept them. Built
# from every surviving copy of what it published: the vcs_signals_summary in a
# merged observatory.db of 2026-08-01, the summaries the weekly AI runs carried
# in their artifacts from 2026-08-04 to 2026-09-13, and the release's previous
# summary of 2026-09-14 and summary of 2026-09-15. Each row spans the
# first and last of those copies the link appears in. first_seen is 2026-08-01
# for most rows because that is the oldest copy left, not the day the link
# began, and a link that came and went during July is not here at all. The
# artifacts expire, so this file is the only place the history lives.
#
# The copies are up to a week apart, so every date here is a bound and not a
# sighting: first_seen is on or before the day the link began, last_seen on or
# after the day it ended. 63 of the 80 links the published table had already
# lost sit on a retired repository whose repos.last_seen is one to six days past
# the link's last_seen here. They were not tightened from it, because
# repos.last_seen dates the repository and not the link: the 3 on repositories
# still active are 9 days past it, since another package kept them listed.
#
# Applied by every daily run, so a link table that was reset gets these rows back
# on the next one. Only these: a link first recorded by a daily run is not in
# this file, and nothing restores it.
#
# Resolved to an absolute path now, while the working directory is the
# repository root: every script sources this file by a root-relative path, and
# the suite then runs its tests from tests/testthat, where a relative path would
# find nothing.
LINKS_BACKFILL_PATH <- file.path(getwd(), "data", "repo-package-links-backfill.csv.gz")

# Wall-clock budget for one deep shard, comfortably inside the 240 minute job
# timeout in ai-weekly.yml. A cancelled job skips its upload step, so a shard
# that overruns discards every repo it scanned; stopping short leaves a partial
# shard that is uploaded and folded, and the tail rides the next dispatch.
AI_DEEP_BUDGET_S <- 3.25 * 3600

# The order the search pass works through each week, most urgent first.
AI_WORK_PRIORITY <- c(onset = 1L, `account-count` = 2L, `window-hit` = 3L, `count-refresh` = 4L,
                      `re-ask` = 5L, campaign = 6L, `rule-new` = 7L, `never-asked` = 8L)
