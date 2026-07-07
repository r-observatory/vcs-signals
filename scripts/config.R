# scripts/config.R - constants for the vcs-signals resolver (SP1). No logic.

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
