#' Type predicates and convenience accessors for netify objects
#'
#' These mirror the natural shape questions a user would ask: is
#' this object bipartite? longitudinal? multilayer? how many actors
#' does it have? They share a roxygen page with the small
#' attribute-accessor helpers `is_binary()` and `nodal_data()`.
#'
#' @name netify_predicates
#' @rdname netify_predicates
#' @param x A netify object.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
NULL

#' @rdname netify_predicates
#' @return `is_binary()` returns a single logical: `TRUE` when every
#' off-diagonal cell of the underlying adjacency is `0`, `1`, or
#' `NA`. Reads the cached `"is_binary"` attribute when available
#' and falls back to probing the raw matrix / array / list.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export is_binary
is_binary <- function(x) {
	if (!is_netify(x)) cli::cli_abort("{.arg x} is not a netify object.")
	ib <- attr(x, "is_binary")
	if (!is.null(ib) && length(ib) >= 1L) {
		return(all(as.logical(ib)))
	}
	# defensive fallback: probe raw values
	raw <- get_raw(x)
	probe <- function(m) {
		v <- as.numeric(m)
		v <- v[!is.na(v)]
		length(v) == 0L || all(v %in% c(0, 1))
	}
	if (is.list(raw) && !is.matrix(raw)) {
		all(vapply(raw, probe, logical(1)))
	} else {
		probe(raw)
	}
}

#' @rdname netify_predicates
#' @return `nodal_data()` returns the nodal-attribute data.frame
#' stored on the netify object (the `"nodal_data"` attribute), or
#' `NULL` if no nodal attributes have been attached. Convenience
#' wrapper so users do not have to remember the `attr()` call.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export nodal_data
nodal_data <- function(x) {
	if (!is_netify(x)) cli::cli_abort("{.arg x} is not a netify object.")
	attr(x, "nodal_data")
}

#' @rdname netify_predicates
#' @return `is_bipartite()` returns a single logical. If `igraph` is
#' loaded after `netify`, the bare `is_bipartite()` may be masked by
#' `igraph::is_bipartite()` (which doesn't accept a netify). Use
#' `is_bipartite_netify()` (alias) or call as `netify::is_bipartite()`
#' to avoid the collision.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export is_bipartite
is_bipartite <- function(x) {
	if (!is_netify(x)) cli::cli_abort("{.arg x} is not a netify object.")
	identical(attr(x, "mode"), "bipartite")
}

#' @rdname netify_predicates
#' @return `is_bipartite_netify()` is an alias for `is_bipartite()` that
#' won't collide with `igraph::is_bipartite()` when both packages are
#' attached.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export is_bipartite_netify
is_bipartite_netify <- function(x) is_bipartite(x)

#' @rdname netify_predicates
#' @return `is_directed_netify()` returns a single logical. Convenience
#' alias for `!isTRUE(attr(x, "symmetric"))` that won't collide with
#' `igraph::is_directed()`.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export is_directed_netify
is_directed_netify <- function(x) {
	if (!is_netify(x)) cli::cli_abort("{.arg x} is not a netify object.")
	s <- attr(x, "symmetric")
	if (length(s) > 1L) !s else !isTRUE(s)
}

#' @rdname netify_predicates
#' @return `is_longitudinal()` returns a single logical.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export is_longitudinal
is_longitudinal <- function(x) {
	if (!is_netify(x)) cli::cli_abort("{.arg x} is not a netify object.")
	!identical(attr(x, "netify_type"), "cross_sec")
}

#' @rdname netify_predicates
#' @return `is_multilayer()` returns a single logical.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export is_multilayer
is_multilayer <- function(x) {
	if (!is_netify(x)) cli::cli_abort("{.arg x} is not a netify object.")
	length(attr(x, "layers")) > 1L
}

#' @rdname netify_predicates
#' @return `is_symmetric_netify()` returns a single logical (or, for
#' mixed-directedness multilayer, the per-layer vector).
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export is_symmetric_netify
is_symmetric_netify <- function(x) {
	if (!is_netify(x)) cli::cli_abort("{.arg x} is not a netify object.")
	s <- attr(x, "symmetric")
	if (length(s) > 1L) s else isTRUE(s)
}

#' @rdname netify_predicates
#' @return `n_actors()` returns a single integer (number of unique
#' actors across all periods / both modes; for bipartite a length-2
#' integer vector `c(row, col)`).
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export n_actors
n_actors <- function(x) {
	if (!is_netify(x)) cli::cli_abort("{.arg x} is not a netify object.")
	mode_attr <- attr(x, "mode")
	netlet_type <- attr(x, "netify_type")
	bip <- identical(mode_attr, "bipartite")
	# pull actor counts per netify_type
	if (netlet_type == "cross_sec") {
		raw <- get_raw(x)
		if (is.array(raw) && length(dim(raw)) == 3L) {
			row_n <- nrow(raw); col_n <- ncol(raw)
		} else {
			row_n <- nrow(raw); col_n <- ncol(raw)
		}
	} else if (netlet_type == "longit_array") {
		raw <- get_raw(x)
		row_n <- dim(raw)[1]; col_n <- dim(raw)[2]
	} else {
		raw <- get_raw(x)
		row_n <- max(vapply(raw, function(m) nrow(m), integer(1)))
		col_n <- max(vapply(raw, function(m) ncol(m), integer(1)))
	}
	if (bip) {
		c(row = as.integer(row_n), col = as.integer(col_n))
	} else {
		as.integer(row_n)
	}
}

#' @rdname netify_predicates
#' @return `n_periods()` returns a single integer (1 for
#' cross-sectional).
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export n_periods
n_periods <- function(x) {
	if (!is_netify(x)) cli::cli_abort("{.arg x} is not a netify object.")
	switch(attr(x, "netify_type"),
		"cross_sec"     = 1L,
		"longit_array"  = as.integer(dim(get_raw(x))[3]),
		"longit_list"   = length(x)
	)
}

#' @rdname netify_predicates
#' @return `n_layers()` returns a single integer (1 for single-layer).
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export n_layers
n_layers <- function(x) {
	if (!is_netify(x)) cli::cli_abort("{.arg x} is not a netify object.")
	length(attr(x, "layers"))
}
