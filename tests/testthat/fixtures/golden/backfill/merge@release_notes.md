# vcs-signals (current)

VCS social signals for CRAN and Bioconductor package repositories, refreshed daily.

## Coverage

- Packages: 10
- Repositories: 10
- Data through: <DATE>
- Years present: 2016 to 2026
- Metrics: stars, forks, open issues, open pull requests, releases; plus topics, license, and archived state

## Assets

- `vcs-signals-summary.db` - one row per package with current values (table `vcs_signals_summary`)
- `vcs-signals-recent.db` - rolling 400-day series window plus the summary
- `vcs-signals-<YYYY>.db` - per-year change-only time series (table `signals_series`: repo_id, date, metric, value)
- `manifest.json` - freshness fields and the per-run change list

## Dates

- `last_commit_date` is the newest commit on the default branch and `last_release_date` the publish time of the newest GitHub release, both ISO-8601 instants. Earlier summaries held the last push to any branch and the day the release count was last seen to change.

## Most recent run

- Changed shards: `vcs-signals-2023.db`, `vcs-signals-2025.db`, `vcs-signals-2016.db`, `vcs-signals-2018.db`, `vcs-signals-2020.db`, `vcs-signals-2024.db`, `vcs-signals-2026.db`, `vcs-signals-2022.db`, `vcs-signals-2017.db`, `vcs-signals-recent.db`, `vcs-signals-summary.db`

## Access

```
gh release download current --repo r-observatory/vcs-signals --pattern "vcs-signals-summary.db"
```

