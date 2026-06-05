#' Tidy a netify object into a long edge data frame
#'
#' `tidy.netify` is an s3 method for the `tidy()` generic from the
#' broom package. it returns one row per edge with all attached
#' nodal and dyadic attributes -- equivalent to
#' `unnetify(x, remove_zeros = TRUE)` but exposed under the broom
#' convention so the netify object plays nicely with broom-style
#' workflows. (broom is not a hard dependency; this method is registered
#' as an s3 method on `tidy` and only triggers when the generic is
#' available, e.g., when the user has `library(broom)` loaded.)
#'
#' @param x a netify object.
#' @param remove_zeros logical. drop zero-weight edges? default `TRUE`
#' (matches the typical broom expectation that the returned frame is
#' actually-observed observations).
#' @param ... additional arguments passed to [unnetify()].
#'
#' @return a tibble (or data.frame if `tibble` isn't installed) with one
#' row per (non-zero) edge. columns include `from`, `to`, optional `time`
#' (longitudinal), the edge weight column, dyadic covariates, and nodal
#' covariates merged in with `_from` / `_to` suffixes. zero-edge inputs
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
#' @author cassy dorff, shahryar minhas
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

#' one-row-per-network summary of a netify object (broom style)
#'
#' `glance.netify` is an s3 method for the `glance()` generic from the
#' broom package. it returns the graph-level statistics produced
#' by [summary.netify()] -- one row per network / time period / layer --
#' so the netify object plays nicely with broom-style workflows. (broom
#' is not a hard dependency; this method is registered as an s3 method
#' on `glance` and only triggers when the generic is available.)
#'
#' @param x a netify object.
#' @param ... additional arguments passed to [summary.netify()] (e.g.,
#' `other_stats = list(my_stat = my_fn)`).
#'
#' @return a tibble (or data.frame if `tibble` isn't installed): one row
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
#' @author cassy dorff, shahryar minhas
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
