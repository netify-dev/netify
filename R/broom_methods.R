#' Tidy a netify object into a long edge data frame
#'
#' `tidy.netify` is an S3 method for the `tidy()` generic from the
#' \pkg{broom} package. It returns one row per edge with all attached
#' nodal and dyadic attributes — equivalent to
#' `unnetify(x, remove_zeros = TRUE)` but exposed under the broom
#' convention so the netify object plays nicely with broom-style
#' workflows. (broom is not a hard dependency; this method is registered
#' as an S3 method on `tidy` and only triggers when the generic is
#' available, e.g., when the user has `library(broom)` loaded.)
#'
#' @param x A netify object.
#' @param remove_zeros Logical. Drop zero-weight edges? Default `TRUE`
#' (matches the typical broom expectation that the returned frame is
#' actually-observed observations).
#' @param ... Additional arguments passed to [unnetify()].
#'
#' @return A tibble (or data.frame if `tibble` isn't installed) with one
#' row per (non-zero) edge. Columns include `from`, `to`, optional `time`
#' (longitudinal), the edge weight column, dyadic covariates, and nodal
#' covariates merged in with `_from` / `_to` suffixes. Zero-edge inputs
#' return a 0-row tibble with the schema preserved.
#'
#' @seealso [unnetify()] for the underlying converter, and `glance.netify`
#' for one-row-per-network summary statistics.
#'
#' @examples
#' data(icews)
#' icews_10 <- icews[icews$year == 2010, ]
#' net <- netify(icews_10, actor1 = "i", actor2 = "j",
#' symmetric = FALSE, weight = "verbCoop")
#' td <- tidy.netify(net)
#' head(td)
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
tidy.netify <- function(x, remove_zeros = TRUE, ...) {
	netify_check(x)
	out <- unnetify(x, remove_zeros = remove_zeros, ...)
	out <- strip_tidy_internal_cols(out, x)
	# wrap in tibble when available
	if (requireNamespace("tibble", quietly = TRUE)) {
		tibble::as_tibble(out)
	} else {
		out
	}
}

#' One-row-per-network summary of a netify object (broom style)
#'
#' `glance.netify` is an S3 method for the `glance()` generic from the
#' \pkg{broom} package. It returns the graph-level statistics produced
#' by [summary.netify()] — one row per network / time period / layer —
#' so the netify object plays nicely with broom-style workflows. (broom
#' is not a hard dependency; this method is registered as an S3 method
#' on `glance` and only triggers when the generic is available.)
#'
#' @param x A netify object.
#' @param ... Additional arguments passed to [summary.netify()] (e.g.,
#' `other_stats = list(my_stat = my_fn)`).
#'
#' @return A tibble (or data.frame if `tibble` isn't installed): one row
#' per (network, time, layer) combination with density, reciprocity,
#' mutual, transitivity, edge counts, etc.
#'
#' @seealso [summary.netify()] for the underlying summary, and `tidy.netify`
#' for one-row-per-edge output.
#'
#' @examples
#' data(icews)
#' icews_10 <- icews[icews$year == 2010, ]
#' net <- netify(icews_10, actor1 = "i", actor2 = "j",
#' symmetric = FALSE, weight = "verbCoop")
#' glance.netify(net)
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
glance.netify <- function(x, ...) {
	netify_check(x)
	out <- summary(x, ...)
	if (requireNamespace("tibble", quietly = TRUE) && is.data.frame(out)) {
		tibble::as_tibble(out)
	} else {
		out
	}
}
