#' Convert a netify_comparison to a tibble
#'
#' S3 method for `tibble::as_tibble()` that returns the
#' `$comparisons` data frame directly. The raw `netify_comparison`
#' object is a list with mixed scalar / nested fields, which
#' `tibble::as_tibble()` cannot coerce cleanly. The per-pair
#' comparison table is almost always what tidyverse users want
#' downstream (filter / arrange / pivot / join with metadata).
#'
#' Registered against the `tibble::as_tibble` generic via `.onLoad`,
#' so `tibble` is not a hard dependency.
#'
#' @param x A `netify_comparison` object from [compare_networks()].
#' @param ... Currently unused.
#' @return A tibble of pairwise comparisons (one row per
#' (`net_i`, `net_j`, `metric`) triple). If the comparison object
#' has no `$comparisons` slot (e.g., a single-network input), an
#' empty tibble is returned with a one-shot inform.
#'
#' @seealso [compare_networks()], [as_tibble.netify()].
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
as_tibble.netify_comparison <- function(x, ...) {
	cmp <- x$comparisons
	if (is.null(cmp) || !is.data.frame(cmp) || nrow(cmp) == 0L) {
		cli::cli_inform(c(
			"i" = "{.cls netify_comparison} has no pairwise {.field comparisons} frame; returning empty tibble."
		),
		.frequency = "once",
		.frequency_id = "as_tibble_netify_comparison_empty")
		cmp <- data.frame(
			net_i = character(0),
			net_j = character(0),
			metric = character(0),
			value = numeric(0),
			p_value = numeric(0),
			stringsAsFactors = FALSE
		)
	}
	if (requireNamespace("tibble", quietly = TRUE)) {
		tibble::as_tibble(cmp)
	} else {
		cmp
	}
}
