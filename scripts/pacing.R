# scripts/pacing.R - per-host request pacing: a minimum interval, hourly and run budgets and a failure breaker.

# One pacer per run. State is per host, so one host never blocks another.
# Settings are named vectors; a host without its own entry uses `default`.
# run_budget caps a host's requests over the pacer's life, so over the run.
host_pacer <- function(min_interval = c(default = 1), hourly_budget = c(default = Inf),
                       breaker_after = 5L, now = function() as.numeric(Sys.time()),
                       sleep = Sys.sleep, run_budget = c(default = Inf)) {
  state <- new.env(parent = emptyenv())
  # Budgets set mid-run override the setting.
  budgets <- new.env(parent = emptyenv())
  setting <- function(x, host) if (host %in% names(x)) x[[host]] else x[["default"]]
  get_host <- function(host) {
    if (is.null(state[[host]]))
      state[[host]] <- list(last = -Inf, hour = NA_real_, count = 0L, total = 0L, fails = 0L, deferred = FALSE)
    state[[host]]
  }
  hourly_limit <- function(host) if (is.null(budgets[[host]])) setting(hourly_budget, host) else budgets[[host]]
  set_hourly_budget <- function(host, n) { budgets[[host]] <- n; invisible(NULL) }
  # The budget counts requests in the current UTC clock hour.
  hour_now <- function() floor(now() / 3600)
  spent_in_hour <- function(h) if (identical(h$hour, hour_now())) h$count else 0L

  before <- function(host) {
    h <- get_host(host)
    if (h$deferred) return(FALSE)
    if (spent_in_hour(h) >= hourly_limit(host)) return(FALSE)
    if (h$total >= setting(run_budget, host)) return(FALSE)
    wait <- h$last + setting(min_interval, host) - now()
    if (wait > 0) sleep(wait)
    h$count <- spent_in_hour(h) + 1L
    h$total <- h$total + 1L
    h$hour <- hour_now()
    h$last <- now()
    state[[host]] <- h
    TRUE
  }
  after <- function(host, ok) {
    h <- get_host(host)
    h$fails <- if (isTRUE(ok)) 0L else h$fails + 1L
    if (h$fails >= breaker_after) h$deferred <- TRUE
    state[[host]] <- h
    invisible(NULL)
  }
  deferred <- function() {
    hosts <- ls(state)
    sort(hosts[vapply(hosts, function(x) state[[x]]$deferred, logical(1))])
  }
  spent <- function(host) spent_in_hour(get_host(host))

  list(before = before, after = after, deferred = deferred, spent = spent,
       hourly_budget = hourly_limit, set_hourly_budget = set_hourly_budget)
}
