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

`vcs_signals_summary` and `repo_packages` hold only the packages listed on CRAN or Bioconductor today. `repo_package_links(repo_id, package, origin, first_seen, last_seen)` holds every package-to-repository link the pipeline has seen, including packages that have since been archived and packages whose URL now points somewhere else, so a retired repository's rows can still be traced to the package that led to it. Rows are never deleted, and `last_seen` stops moving once the package no longer resolves to that repository.

The daily run records the days it sees a link. The links from before the table existed were rebuilt from the published copies that survive, dated 2026-08-01, 08-04, 08-09, 08-16, 08-23, 08-30, 09-06, 09-13, 09-14 and 09-15, so a date that came from them is the copy a link appears in rather than the day it began or ended. Such a `first_seen` means "on or before", and 2026-08-01 stands for anything older. Such a `last_seen` means "on or after": the package may have gone on resolving to that repository for up to six more days, until the day before the next copy. A link that began after 2026-09-15 but before the first daily run that kept this table has a `first_seen` of the last run before that one, which is a bound in the same way.

`vcs_repo_owner(repo_id, node_id, owner_login_current, owner_type, owner_node_id, name_with_owner_current, observed_on)` names the current owner of every active GitHub repository, as the daily gauge query returned it on `observed_on`. `owner_type` is `Organization` or `User`, and `owner_login_current` and `name_with_owner_current` are in GitHub's own case. A repository that moved to another account keeps its `repo_id`, which comes from the package's DESCRIPTION URL, and carries its new owner and name, so two `repo_id`s with the same `node_id` are one repository. A row leaves the table when its repository is no longer an active GitHub repository here, or when the query has not returned it for more than 14 days, so a repository with no row has no known owner. The table is a snapshot and keeps no ownership history.

## Caveats

Star history is reconstructed from the GraphQL `stargazers` connection timestamps back to each repository's creation, then maintained forward daily; only the REST list-stargazers endpoint is restricted, not the GraphQL connection. Figures are best-effort, not absolute.

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.
