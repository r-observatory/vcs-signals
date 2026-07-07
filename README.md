# vcs-signals

Resolves the source repository behind every CRAN and Bioconductor package (from its DESCRIPTION `URL`/`BugReports`) and, in later stages, collects VCS social signals (stars, forks, issues, pull requests, commits, contributors, releases) over time.

This is the r-observatory `vcs-signals` pipeline. SP1 (this stage) produces the repository dimension only.

## SP1 output

`out/vcs-repos.db` with two tables:

- `repos(repo_id, node_id, host, host_domain, owner, name, name_with_owner, supported, n_packages, first_seen, last_seen, status)`
- `repo_packages(repo_id, package, origin, resolved_from)`

`repo_id` is a frozen surrogate slug `host_domain/owner/name`. Read-only mirrors (github.com/cran, github.com/bioc, git.bioconductor.org) are excluded. Each package resolves to a single primary repo; a repo backs many packages.

## Run

```bash
Rscript scripts/update.R out/     # writes out/vcs-repos.db and prints a coverage report
Rscript tests/testthat.R          # runs the unit suite
```

## Signals (SP2)

SP2 extends the SP1 repo dimension with a daily forward gauge snapshot of every supported (currently GitHub-only) repo: `stars, forks, watchers, issues_open, issues_closed, prs_open, prs_closed, prs_merged, commits_total, releases_total, size_kb`. Each run resolves any repo missing a GitHub node id, collects the current values for these gauges over GraphQL, and writes only what changed since the prior run.

The collected values feed two outputs. `signals_series` is a sparse, change-only time series keyed `(repo_id, date, metric)` -> a row is written only the first day a metric's value differs from what it was the day before, so an unchanged repo accumulates no new rows. `series_latest` tracks the current value of every metric per repo and is fully rebuilt each run. `vcs_signals_summary` fans each repo back out to every package it backs (a repo can back many CRAN/Bioconductor packages) and carries the current gauge values plus `last_commit_date`, `license`, `topics`, `is_archived`, a 30-day `trend_30d` change in stars, and `first_seen`/`last_seen`.

Published output is a rolling `current` GitHub Release on this repo, refreshed daily, with these assets:

- `vcs-signals-<YYYY>.db` - one shard per year of `signals_series`, one file per calendar year touched by the series.
- `vcs-signals-recent.db` - a rolling window (400 days) of `signals_series`, plus the full `repos`, `repo_packages`, `series_latest`, and `pipeline_state` tables, so a fresh checkout can reconstruct current state without downloading every year shard.
- `vcs-signals-summary.db` - `vcs_signals_summary` plus `repos` and `repo_packages`, for point-in-time per-package lookups.
- `manifest.json` - lists which shards changed this run, the release tag, and a `summary` block (`source_kind`, `last_checked`, `data_through`); a run with no changed gauges still refreshes this file as a heartbeat and uploads nothing else.

Only shards whose content actually changed are re-uploaded each run (a content hash gate), and a prior manifest plus recent and year shards are pulled down and checked before any upload, so a run that cannot reach the release's existing history aborts rather than silently shrinking it.

Two caveats worth stating plainly. Stars are forward-only: GitHub restricted the list-stargazers endpoint on 2026-06-30, so there is no way to reconstruct a repo's star history before this pipeline started collecting it, and `signals_series` for `stars` simply begins at go-live (the current `stargazerCount` itself is exact and unaffected). `contributors_total` is not collected in SP2 at all; it requires walking commit history rather than a cheap gauge query and is deferred to a later stage.

The daily update runs via `.github/workflows/update.yml` on a `30 6 * * *` UTC cron (and on demand via `workflow_dispatch`, with a `force_full_rebuild` input that re-exports and re-uploads every shard regardless of the change gate). It runs the full test suite before every update and needs a `VCS_SIGNALS_TOKEN` secret (a fine-grained PAT with public-repo read) for the GraphQL calls, alongside the built-in `GITHUB_TOKEN` for release operations.
