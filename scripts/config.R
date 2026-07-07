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
