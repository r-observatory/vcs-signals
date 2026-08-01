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
  "r-forge.r-project.org" = "rforge"
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
FORWARD_METRICS  <- c("stars", "forks", "watchers", "issues_open", "issues_closed",
                      "prs_open", "prs_closed", "prs_merged",
                      "releases_total", "size_kb")
CHEAP_BATCH    <- 25L    # repos per cheap-gauge GraphQL query (small enough to stay under GitHub's execution-time limit)
COMMIT_BATCH   <- 8L     # repos per commit-count query (history.totalCount is expensive server-side and times out in larger batches)
RECENT_WINDOW  <- 400L   # days of series kept in the recent shard
REVISION_WINDOW<- 10L    # trailing days re-materialized each run (must be < RECENT_WINDOW)
POINT_RESERVE  <- 1500L  # GraphQL points left unspent as headroom
BATCH_DELAY_S  <- 0.35   # pause between GraphQL batches, to stay well under secondary rate limits

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
WEEKLY_METRICS <- c("commits_total", "contributors_total",
                    "median_days_to_close_issue", "median_days_to_close_pr",
                    "median_open_issue_age_days")
COMMIT_HISTORY_BATCH <- 12L  # repos per commits.history.totalCount aliased query: execution-time expensive server-side, so kept well under the ~15-repo point where it starts to time out (not the 20-40 a cheap connection page can batch)
MEDIAN_BATCH <- 10L  # repos per responsiveness query: 3 connections x 50 nodes/repo is execution-time heavy server-side, so kept well below the cheap-connection batch size to avoid 502s
CONTRIBUTOR_DELAY_S   <- 0.5 # pause between per-repo REST contributor-count lookups (one request per repo, no batching available)

# ---- AI-tooling detection ----
# Config markers. location = which tree the entry appears in ("root" or the
# ".github" subtree). agnostic = tool-agnostic (recorded but excluded from the
# tool count / first-tool rollups and never names a package alone). .replit and
# .deepsource.toml are deliberately absent: a bare platform-config file is
# non-evidence, so Replit is detected only via its commit trailer below.
AI_MARKERS <- list(
  list(path = "CLAUDE.md",       tool = "claude",    kind = "file", location = "root",   agnostic = FALSE),
  list(path = "CLAUDE.local.md", tool = "claude",    kind = "file", location = "root",   agnostic = FALSE),
  list(path = ".claude",         tool = "claude",    kind = "dir",  location = "root",   agnostic = FALSE),
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
  # Review agents. A configured reviewer is tooling adoption, and the tool is named on
  # the surface, so a reader can tell review from authoring.
  list(path = ".coderabbit.yaml", tool = "coderabbit", kind = "file", location = "root", agnostic = FALSE),
  list(path = ".coderabbit.yml",  tool = "coderabbit", kind = "file", location = "root", agnostic = FALSE),
  # Ambient IDE marker: the editor writes .positai regardless of AI use, so it is EXCLUDED
  # from the AI signal (ai_deliberate_markers). A marker with no class field defaults to
  # "deliberate". Recording ambient markers as a dev-tooling datum is deferred to the
  # separate dev-tooling signal.
  list(path = ".positai",        tool = "positron",  kind = "file", location = "root",   agnostic = FALSE, class = "ambient"),
  list(path = ".idx",            tool = "idx",       kind = "dir",  location = "root",   agnostic = FALSE, class = "ambient")
)

# ---- Development-tooling detection (data-only) ----
# The single source of truth for the vcs_dev_tooling flag column set. Each entry names a
# table COLUMN (col) and the tree-entry names that satisfy it (paths: a flag is 1 if ANY is
# present). location = which fetched tree to check ("root" = HEAD:, "github" = HEAD:.github,
# "both" = either). match = "exact" set-membership by default, or "suffix" (endsWith) for the
# *.Rproj case. Detection is existence-of-entry-name only; nothing reads file contents. The
# classifier (classify_dev_tooling), the DDL (dev_tooling_create_sql), and the empty helper
# (.devtool_empty) are all derived from these col names, so the column set cannot drift.
# readme_source (TEXT enum) and has_ci (the OR of the ci_* systems) are COMPUTED additions,
# not entries here. repo_id / last_scanned are stamped by the cheap pass, not by the classifier.
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
  list(col = "has_issue_template", paths = c("ISSUE_TEMPLATE", "ISSUE_TEMPLATE.md"), location = "github"),
  list(col = "has_pr_template",    paths = c("PULL_REQUEST_TEMPLATE.md"),           location = "github"),
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

# Tier A bot identities: exact, case-normalized email/login match only.
AI_BOT_ALLOWLIST <- c(
  "noreply@anthropic.com"      = "claude",
  "devin-ai-integration[bot]"  = "devin",
  "openhands-agent"            = "openhands",
  "google-labs-jules[bot]"     = "jules",
  "cursor[bot]"                = "cursor",
  "copilot-swe-agent[bot]"     = "copilot"
)
# Non-AI bots that must never be flagged (backstops the allowlist).
AI_BOT_DENYLIST <- c(
  "dependabot[bot]", "renovate[bot]", "github-actions[bot]", "pre-commit-ci[bot]",
  "codecov[bot]", "allcontributors[bot]", "web-flow", "lintr-bot", "styler-bot"
)
# PR-authorship channel: agent logins that open PRs (exact, lowercase).
# PR channel (GraphQL). Spelled WITHOUT the "[bot]" suffix, because
# author { login } returns a bot's login stripped. Four of the six entries here
# used to carry the suffix, so they matched nothing and the channel published a
# confident zero across the whole roster while copilot-swe-agent was opening
# pull requests in the roster's busiest repositories.
#
# These are NOT the same strings as AI_BOT_ALLOWLIST above, and the difference
# is not an oversight. The two lists feed different APIs, which want opposite
# shapes. Measured against dotnet/runtime:
#
#   REST  search/commits  author:copilot-swe-agent[bot]  -> 982 hits
#   REST  search/commits  author:copilot-swe-agent       ->   0 hits
#   GraphQL author { login }                             -> "copilot-swe-agent"
#
# So AI_BOT_ALLOWLIST keeps its suffixes and this list drops them. Making them
# agree would break whichever one is changed.
#
# Bare "copilot" is deliberately absent: it is a person's account, not the
# agent, and including it would trade a false zero for a false positive.
AI_PR_AGENT_LOGINS <- c(
  "copilot-swe-agent"          = "copilot",
  "devin-ai-integration"       = "devin",
  "google-labs-jules"          = "jules",
  "cursor"                     = "cursor",
  "openhands-agent"            = "openhands"
)
# Tier B commit-message trailers. Anchored to the canonical bot identity so a
# human named Claude is rejected. Matched case-insensitively.
# `pattern` is the regex a fetched message is VERIFIED against; `query` is the literal
# phrase handed to the commit-search API, which does no regex. A search hit whose message
# fails the pattern is a fuzzy candidate and is recorded as a censored floor, never as an
# exact onset, so a person named Claude cannot mint an immutable date.
AI_TRAILER_PATTERNS <- list(
  list(pattern = "co-authored-by:\\s*claude\\s*<noreply@anthropic\\.com>", tool = "claude",
       query = "\"Co-Authored-By: Claude\""),
  list(pattern = "generated with \\[?claude code",                          tool = "claude",
       query = "\"Generated with Claude Code\""),
  list(pattern = "generated by replit",                                     tool = "replit",
       query = "\"Generated by Replit\""),
  list(pattern = "replit-commit-author:",                                   tool = "replit",
       query = "\"Replit-Commit-Author:\""),
  list(pattern = "codex-cli",                                               tool = "codex",
       query = "\"codex-cli\"")
)
# Tier C author-name suffixes. `query` searches the author field rather than the message.
AI_AUTHOR_SUFFIXES <- list(
  list(suffix = "(aider)", tool = "aider", query = "author-name:\"(aider)\"")
)
# Renamed-marker predecessors (new path -> old path) probed so a rename does
# not reset onset.
AI_MARKER_PREDECESSORS <- c(".cursor" = ".cursorrules")
# Detection ruleset version, surfaced by the viewer methods note.
AI_RULESET_VERSION <- "2026-07-29"
# Evidence-tier strength for deterministic ordering (lower = stronger/earlier on ties).
TIER_PRIORITY <- c(A = 1L, B = 2L, C = 3L, PR = 4L, D = 5L)

# Pacing for the REST commit-search API (search/commits, ~1,800/hr = 30/min),
# a budget separate from the GraphQL 5000/hr and from core REST. Each onset
# search sleeps this long after its request so a gated deep scan stays under it.
# Commit search enforces a SECONDARY rate limit far tighter than the documented
# ~30/min, and hand-testing tripped it after five queries. At 2s the first backfill
# issued about 12,000 searches, was refused by almost all of them, and recorded the
# refusals as "no trailer found". Pace for the limit that actually exists.
SEARCH_DELAY_S <- 6
# Repos per aliased tree-marker / PR-login query in the cheap pass. Both queries are
# execution-heavy server-side (a tree fetch plus 50 PR nodes per alias), so this is
# kept small like COMMIT_HISTORY_BATCH rather than the 20-40 a cheap connection page
# can batch. A whole-batch fault halves and retries (fetch_tree_markers / fetch_pr_agents).
TIER_D_BATCH <- 10L
# Agent-era boundary. AI coding agents did not open PRs before this date, so an
# allowlisted agent login on an earlier PR is a login collision, not adoption: it
# contributes no PR evidence and no PR onset. Full ISO date, compared lexicographically
# against createdAt (ISO instants sort correctly as strings).
AI_PR_CUTOFF <- "2023-01-01"
# GraphQL points left unspent as headroom for the cheap and deep passes, mirroring
# POINT_RESERVE (the daily pass's reserve). The cheap pass's PR query
# (pullRequests(first: 50) per alias) is not the ~1-point-per-batch the tree query is, so
# run_cheap and run_deep both check graphql_rate_remaining(io) against this reserve
# before spending down the shared token, pausing rather than faulting when it is low.
AI_POINT_RESERVE <- 1500L
