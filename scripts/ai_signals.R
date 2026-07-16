# scripts/ai_signals.R - pure AI-tooling detection: classifiers, naming
# threshold, tool ordering, onset reducer, summary rollups. No I/O.

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
