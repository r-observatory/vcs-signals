# scripts/config.R - constants for the vcs-signals resolver. No logic.

# Bioconductor release VIEWS DCF files (software, annotation, experiment, workflows).
VIEWS_URLS <- c(
  software   = "https://bioconductor.org/packages/release/bioc/VIEWS",
  annotation = "https://bioconductor.org/packages/release/data/annotation/VIEWS",
  experiment = "https://bioconductor.org/packages/release/data/experiment/VIEWS",
  workflows  = "https://bioconductor.org/packages/release/workflows/VIEWS"
)

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
