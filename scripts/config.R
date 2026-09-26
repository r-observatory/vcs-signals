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
  # pkgdown is the most common documentation site in the ecosystem and was the
  # conspicuous absence here: _quarto.yml was detectable and this was not. Three
  # shapes, because maintainers use all three: the config at the root under
  # either extension, and a pkgdown/ directory holding templates or extra pages.
  list(col = "has_pkgdown",       paths = c("_pkgdown.yml", "_pkgdown.yaml", "pkgdown"),
                                                                 location = "root"),
  # altdoc keeps its config in altdoc/ at the root, whatever backend it drives
  # (altdoc/mkdocs.yml, altdoc/quarto_website.yml, altdoc/docsify.html). Note
  # altdoc/pkgdown.yml exists too: that is altdoc driving pkgdown, and counting
  # it as a pkgdown site would overstate pkgdown, so the two stay separate.
  list(col = "has_altdoc",        paths = c("altdoc"),           location = "root"),
  # litedown's config is NOT a root file. Every observed instance sits under
  # site/ or docs/, so a root-only rule would report litedown as unused
  # everywhere. The site subtree is fetched for exactly this.
  list(col = "has_litedown",      paths = c("_litedown.yml", "site/_litedown.yml",
                                            "docs/_litedown.yml"), location = "root"),

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
  # The name carries a model between "Claude" and the address in practice:
  # sampling four roster repositories returned 79 trailers, of which 79 were
  # "Claude Sonnet 4.5", "Claude Opus 4.8 (1M context)" and the like, and none
  # were the bare form this pattern used to demand. Every tier-B hit was failing
  # verification and taking a floor date instead of an exact one. [^<\n] keeps
  # the match on one line and still requires the name to begin with Claude, so
  # "Claudia" at the same address does not qualify.
  list(pattern = "co-authored-by:\\s*claude\\b[^<\\n]*<noreply@anthropic\\.com>", tool = "claude",
       query = "\"Co-Authored-By: Claude\""),
  list(pattern = "generated with \\[?claude code",                          tool = "claude",
       query = "\"Generated with Claude Code\""),
  # There is deliberately no "generated by Replit" rule. Every one of the six
  # occurrences found in real commits is a maintainer describing Replit, not
  # Replit signing: "Removed extra directories generated by Replit", "Ignore
  # Replit config files (auto-generated by Replit environment)". The phrase is a
  # pure false-positive generator, and anchoring it to a line start only hid
  # that. Replit-Commit-Author below is the actual signature.
  list(pattern = "replit-commit-author:",                                   tool = "replit",
       query = "\"Replit-Commit-Author:\""),
  # Codex signs with a plain name and an OpenAI address; the name alone is far
  # too common to key on. Sampling 100 real commits carrying a Codex trailer
  # returned 1,134 trailer lines across eight shapes, of which only 23 said
  # "codex-cli" and the rest were the name plus an address this rule never saw.
  # The address is the anchor, as it is for Claude. codex@agent appears too and
  # is kept separate rather than widened into "any address", which would match a
  # person who happens to be called Codex.
  list(pattern = "co-authored-by:\\s*codex[^<\\n]*<[^>\\n]*@(openai\\.com|agent)>", tool = "codex",
       query = "\"Co-Authored-By: Codex\""),
  list(pattern = "codex-cli",                                               tool = "codex",
       query = "\"codex-cli\""),

  # Every rule below keys on the agent's ADDRESS, never on its name. Devin's own
  # search returns Devin Logan and devin.logan alongside the agent, and Jules is
  # a person's name too; a name match would flag them. Each shape here was
  # counted in real commit messages, not inferred from documentation.
  #
  # Devin and OpenHands leave no config marker anywhere in AI_MARKERS, so until
  # these rules the only way either could be seen was a pull request it happened
  # to open. These give both a channel that works on commits.
  list(pattern = "co-authored-by:[^<\\n]*<cursoragent@cursor\\.com>",           tool = "cursor",
       query = "\"cursoragent@cursor.com\""),
  list(pattern = "co-authored-by:\\s*cursor[^<\\n]*<cursor@agent>",              tool = "cursor",
       query = "\"Co-Authored-By: Cursor\""),
  list(pattern = "co-authored-by:[^<\\n]*<[^>\\n]*devin-ai-integration\\[bot\\]@",  tool = "devin",
       query = "\"devin-ai-integration\""),
  list(pattern = "co-authored-by:[^<\\n]*<[^>\\n]*@all-hands\\.dev>",           tool = "openhands",
       query = "\"all-hands.dev\""),
  list(pattern = "co-authored-by:[^<\\n]*<[^>\\n]*google-labs-jules\\[bot\\]@",     tool = "jules",
       query = "\"google-labs-jules\""),
  list(pattern = "co-authored-by:[^<\\n]*<[^>\\n]*@windsurf\\.(ai|com)>",       tool = "windsurf",
       query = "\"Co-Authored-By: Windsurf\""),
  list(pattern = "co-authored-by:[^<\\n]*<[^>\\n]*windsurf-bot\\[bot\\]@",          tool = "windsurf",
       query = "\"windsurf-bot\""),
  # Gemini signs four ways. noreply@google.com is generic, so that rule also
  # requires the name to start with Gemini; the bot addresses are distinctive
  # enough on their own.
  list(pattern = "co-authored-by:\\s*gemini[^<\\n]*<[^>\\n]*@google\\.com>",  tool = "gemini",
       query = "\"Co-Authored-By: Gemini\""),
  list(pattern = "co-authored-by:[^<\\n]*<[^>\\n]*gemini-(code-assist|cli)\\[?[^>\\n]*@", tool = "gemini",
       query = "\"gemini-code-assist\""),
  # Aider names the provider and model in the parentheses. The address is the
  # anchor; the parenthetical is what Addendum 2 of the design would read.
  list(pattern = "co-authored-by:[^<\\n]*<aider@aider\\.chat>",                 tool = "aider",
       query = "\"aider@aider.chat\"")
)
# Tier C author-name suffixes. `query` searches the author field rather than the message.
AI_AUTHOR_SUFFIXES <- list(
  list(suffix = "(aider)", tool = "aider", query = "author-name:\"(aider)\"")
)
# Renamed-marker predecessors (new path -> old path) probed so a rename does
# not reset onset.
AI_MARKER_PREDECESSORS <- c(".cursor" = ".cursorrules")
# Detection ruleset version, surfaced by the viewer methods note.
AI_RULESET_VERSION <- "2026-09-19"
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
A,cursor,open,"We search for commits by cursor[bot], the account the Cursor app uses to review and merge pull requests. Cursor's coding agent commits under another account, cursoragent (cursoragent@cursor.com), which had made 120 commits in 11 repositories, 6 of them with no Cursor finding here. The search also runs only where a file, a line in an ignore file or a pull request already named Cursor. This is settled once commits by cursoragent are counted in every repository.",2026-09-25
A,devin,genuine,"No repository has a commit by Devin's account (devin-ai-integration[bot]) on its default branch, checked across 15,875. The search itself runs only in xsdm-devel, where Devin's account opened pull requests, and that repository has no commits by the account and 29 commits crediting Devin. Devin also appears in pull requests that maintainers opened from a Devin session in sobol and maxentcpp, which this page does not count yet.",2026-09-25
A,openhands,open,"The account we look for is right (openhands-agent, openhands@all-hands.dev), but the search runs only where a pull request opened by OpenHands's account was found, and none has been. One commit by that account exists, in kuzuR, which this page counts only through a commit crediting OpenHands. This is settled once commits by the account are counted in every repository.",2026-09-25
B,replit,genuine,"Searched on 2026-07-31 in the 1,964 repositories found by then, with no match. Other checks agree: no repository has a .replit, replit.nix or replit.md file, no default branch has a commit by Replit Agent's account (agent@replit.com), and the newest 100 commits of 600 sampled repositories have no Replit-Commit-Author line.",2026-09-25
B,windsurf,genuine,"Searches for commits crediting Windsurf began on 2026-08-02 and no full search of every repository has finished since, so every public commit crediting Windsurf was listed instead: 963 naming Windsurf, 552 naming its bot account (windsurf-bot) and 76 at codeium.com. None is in a repository we scan. Windsurf became Devin Desktop on 2026-06-02 and now keeps its rules in .devin/rules.",2026-09-25
D,amazonq,genuine,"No repository has an .amazonq folder, checked across 15,875. Amazon Q itself is present in one repository: its account opened a pull request in ss3sim, merged in May 2025, and made 2 commits there, which this page does not count yet. Its command-line tool has been Kiro since November 2025. One repository has a .kiro folder and 5 more name .kiro in an ignore file.",2026-09-25
D,grok,genuine,"No GROK.md, .grok or .xai in any repository, checked across 15,875. xAI's Grok Build reads AGENTS.md, CLAUDE.md and .grok/rules rather than GROK.md, so a repository using it may show only as AGENTS.md or CLAUDE.md.",2026-09-25
D,junie,genuine,"No .junie folder in any repository, no commit by Junie's account (junie@jetbrains.com) on any default branch, and no pull request by it among each repository's newest 50, checked across 15,875. Junie now reads AGENTS.md first, so a repository using it may show only as AGENTS.md.",2026-09-25
D,roo,genuine,"No .roo, .roomodes, .roorules or .rooignore in any repository, checked across 15,875, and none of the 218 public commits by Roo's cloud agent account (roomote[bot]) is in a repository we scan. The Roo Code extension was shut down on 2026-05-15, so this will not change.",2026-09-25
PR,cursor,open,"No pull request among each repository's newest 50 was opened by Cursor's app account, checked across 161,444 pull requests. Cursor's cloud agent opens them under the maintainer's own account instead, with a note at the top of the description and a branch named cursor/ plus the task and four hexadecimal characters. 18 repositories have one among their newest 50 pull requests, 2 of them from an outside contributor. Separately, 44 repositories have a commit crediting Cursor among their newest 50 commits since May 2025, but commit messages are searched only where a file, a line in an ignore file or a pull request by a tool's account was already found, so 27 of them show no Cursor use here. This is settled once pull request descriptions and every repository's recent commits are read.",2026-09-25
PR,openhands,genuine,"No pull request among each repository's newest 50 was opened by OpenHands's account (openhands-agent), checked across 161,444 pull requests.",2026-09-25
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
SUMMARY_EXTRA_TABLES <- c("vcs_ai_models", "vcs_ai_rule_inventory",
                          "vcs_ai_silent_channels", "repo_package_links")

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
