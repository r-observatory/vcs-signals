# vcs-signals

Resolves the source repository behind every CRAN and Bioconductor package (from its DESCRIPTION `URL`/`BugReports`) and collects VCS signals (stars, forks, watchers, issues, pull requests, commits, releases) for GitHub-hosted repos. Data is published daily as SQLite shards attached to a single rolling GitHub release tag (`current`).

## Data Access

```bash
gh release download current \
  --repo r-observatory/vcs-signals \
  --pattern "vcs-signals-summary.db"
```

```r
library(RSQLite)
con <- dbConnect(SQLite(), "vcs-signals-summary.db")

# Top packages by stars
dbGetQuery(con, "
  SELECT package, repo_id, stars, forks, issues_open, trend_30d
  FROM vcs_signals_summary
  ORDER BY stars DESC LIMIT 20
")

dbDisconnect(con)
```

For the full time series, download `vcs-signals-recent.db` (rolling 400-day window) or a per-year `vcs-signals-<YYYY>.db` archive, and query `signals_series(repo_id, date, metric, value)`.

## Schema

`vcs_signals_summary` carries one row per package: `package, origin, repo_id, stars, forks, issues_open, prs_open, commits_total, releases_total, last_commit_date, license, topics, trend_30d`. `signals_series` is a sparse, change-only time series keyed `(repo_id, date, metric)`.

## Caveats

Star history is forward-only: GitHub restricted the list-stargazers endpoint in 2026, so `signals_series` for `stars` only begins at the point this pipeline started collecting. Figures are best-effort, not absolute.
