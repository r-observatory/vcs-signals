# vcs-signals

Resolves the source repository behind every CRAN and Bioconductor package (from its
DESCRIPTION `URL`/`BugReports`) and, in later stages, collects VCS social signals
(stars, forks, issues, pull requests, commits, contributors, releases) over time.

This is the r-observatory `vcs-signals` pipeline. SP1 (this stage) produces the
repository dimension only.

## SP1 output

`out/vcs-repos.db` with two tables:

- `repos(repo_id, node_id, host, host_domain, owner, name, name_with_owner, supported, n_packages, first_seen, last_seen, status)`
- `repo_packages(repo_id, package, origin, resolved_from)`

`repo_id` is a frozen surrogate slug `host_domain/owner/name`. Read-only mirrors
(github.com/cran, github.com/bioc, git.bioconductor.org) are excluded. Each package
resolves to a single primary repo; a repo backs many packages.

## Run

```bash
Rscript scripts/update.R out/     # writes out/vcs-repos.db and prints a coverage report
Rscript tests/testthat.R          # runs the unit suite
```
