# Matrix utility functions for netify
#
# Internal file: shared matrix manipulation utilities used across the
# netify package. The only user-facing export is `as.matrix.netify`,
# documented below; the remaining helpers are internal (`@noRd`).

#' Coerce a netify object to a plain matrix
#'
#' Strips the netify class and netify-specific attributes so the result
#' is a clean numeric matrix carrying only `dim` and `dimnames`. For
#' longitudinal netify objects, `time` selects which slice to return;
#' it defaults to the first time period and emits a hint. Round-trips
#' (`net |> as.matrix() |> netify()`) recover a fresh cross-sectional
#' netify object, but structural attributes (symmetric / diag_to_NA /
#' weight) are re-detected from the matrix on the way back in rather
#' than copied across, so a directed matrix or one with a non-NA
#' diagonal will be flagged accordingly.
#'
#' @param x A netify object.
#' @param time For longitudinal netify objects, either the integer index
#'   or character label of the time slice to extract. Defaults to the
#'   first slice and emits a hint when used implicitly.
#' @param ... Additional args (ignored).
#'
#' @return A plain numeric matrix with `dim` and `dimnames` only (no
#'   `netify` class, no netify metadata attributes).
#'
#' @seealso \code{\link{get_adjacency}} for the data.frame-input
#'   counterpart that also accepts a netify object; \code{\link{netify}}
#'   for rebuilding a netify object from a plain matrix.
#'
#' @examples
#' data(icews)
#' icews_2010 <- icews[icews$year == 2010, ]
#' net <- netify(icews_2010, actor1 = "i", actor2 = "j",
#'     symmetric = FALSE, weight = "verbCoop")
#' m <- as.matrix(net)
#' dim(m)
#' class(m)
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
as.matrix.netify <- function(x, time = NULL, ...) {
	nt <- attr(x, "netify_type")
	if (is.null(nt) || nt == "cross_sec") {
		mat <- unclass(x)
		attributes(mat) <- list(
			dim = dim(x),
			dimnames = dimnames(x)
		)
		return(mat)
	}

	# longitudinal: select a slice
	if (nt == "longit_array") {
		dn3 <- dimnames(x)[[3]]
		if (is.null(time)) {
			time <- 1L
			cli::cli_alert_info(
				"{.fn as.matrix.netify} returning slice {.val {dn3[time] %||% time}} (first period). Pass {.arg time} to choose another."
			)
		}
		idx <- if (is.character(time)) match(time, dn3) else as.integer(time)
		if (is.na(idx) || idx < 1L || idx > dim(x)[3]) {
			cli::cli_abort("time = {.val {time}} is out of range for this netify object.")
		}
		mat <- unclass(x)[, , idx, drop = TRUE]
		attributes(mat) <- list(dim = dim(mat), dimnames = dimnames(mat))
		return(mat)
	}

	if (nt == "longit_list") {
		nms <- names(unclass(x))
		if (is.null(time)) {
			time <- 1L
			cli::cli_alert_info(
				"{.fn as.matrix.netify} returning slice {.val {nms[time] %||% time}} (first period). Pass {.arg time} to choose another."
			)
		}
		idx <- if (is.character(time)) match(time, nms) else as.integer(time)
		if (is.na(idx) || idx < 1L || idx > length(unclass(x))) {
			cli::cli_abort("time = {.val {time}} is out of range for this netify object.")
		}
		mat <- unclass(x)[[idx]]
		attributes(mat) <- list(dim = dim(mat), dimnames = dimnames(mat))
		return(mat)
	}

	cli::cli_abort("Unknown netify_type {.val {nt}}.")
}

#' Extract matrix from netify object
#'
#' Unified function to extract adjacency matrix from different types of netify objects
#'
#' @param net A netify object (cross-sectional, longitudinal array, or longitudinal list)
#' @param time_index For longitudinal data, which time period to extract (default: 1)
#' @return Numeric matrix
#'
#' @keywords internal
#' @noRd
extract_matrix <- function(net, time_index = 1) {
	attrs <- attributes(net)
	netify_type <- attrs$netify_type

	if (netify_type == "cross_sec") {
		return(as.matrix(net))
	} else if (netify_type == "longit_array") {
		return(net[, , time_index])
	} else if (netify_type == "longit_list") {
		return(net[[time_index]])
	} else {
		stop("Unknown netify type: ", netify_type)
	}
}

#' Get all unique actors from networks
#'
#' Extracts all unique actors from a list of networks or a single network
#'
#' @param nets Either a single netify object or a list of netify objects
#' @return Character vector of sorted unique actors
#'
#' @keywords internal
#' @noRd
get_all_actors <- function(nets) {
	if (!is.list(nets) || inherits(nets, "netify")) {
		# single net
		mat <- extract_matrix(nets)
		return(sort(unique(c(rownames(mat), colnames(mat)))))
	} else {
		# list of nets - use rcpp for efficiency
		mats <- lapply(nets, extract_matrix)
		return(get_all_actors_cpp(mats))
	}
}

#' Align matrices to common actors
#'
#' Ensures two or more matrices have the same dimensions and actor ordering
#'
#' @param ... Either two matrices or a list of matrices
#' @param all_actors Optional character vector of actors to align to
#' @param include_diagonal Whether to preserve diagonal values
#' @return For two matrices: list with mat1 and mat2. For multiple: list of aligned matrices
#'
#' @keywords internal
#' @noRd
align_matrices <- function(..., all_actors = NULL, include_diagonal = FALSE) {
	dots <- list(...)

	# handle different input types
	if (length(dots) == 1 && is.list(dots[[1]])) {
		# list of mats passed
		mats <- dots[[1]]
	} else if (length(dots) == 2) {
		# two mats passed directly
		mats <- dots
	} else {
		stop("align_matrices requires either 2 matrices or a list of matrices")
	}

	# extract matrices if they're netify objects
	if (inherits(mats[[1]], "netify")) {
		mats <- lapply(mats, extract_matrix)
	}

	if (length(mats) == 2) {
		# use rcpp for two matrices
		return(align_matrices_cpp(mats[[1]], mats[[2]], all_actors))
	} else {
		# use rcpp for multiple matrices
		return(batch_align_matrices_cpp(mats, all_actors, include_diagonal))
	}
}

#' Melt matrix to long format
#'
#' Converts matrix to data frame with row, col, value columns.
#' Replaces multiple melt functions with single efficient implementation.
#'
#' @param mat Matrix to melt
#' @param remove_diagonal Remove diagonal elements (default: TRUE)
#' @param remove_zeros Remove zero values (default: TRUE)
#' @param na.rm Remove NA values (default: TRUE)
#' @param value.name Name for value column (default: "value")
#' @return Data frame with row, col, and value columns
#'
#' @keywords internal
#' @noRd
melt_matrix <- function(mat,
						remove_diagonal = TRUE,
						remove_zeros = TRUE,
						na.rm = TRUE,
						value.name = "value") {
	# use rcpp implementation
	result <- melt_matrix_cpp(mat, remove_diagonal, remove_zeros, na.rm)

	# rename columns to match expected format
	names(result)[names(result) == "row"] <- "Var1"
	names(result)[names(result) == "col"] <- "Var2"

	# rename value column if requested
	if (value.name != "value") {
		names(result)[names(result) == "value"] <- value.name
	}

	return(result)
}

#' Calculate correlation with proper handling of edge cases
#'
#' Wrapper around correlation_cpp that handles constant vectors and missing data
#'
#' @param x First numeric vector
#' @param y Second numeric vector
#' @param na.rm Remove NA values before calculation
#' @return Correlation coefficient or NA if cannot be calculated
#'
#' @importFrom stats var
#' @keywords internal
#' @noRd
safe_correlation <- function(x, y, na.rm = TRUE) {
	if (na.rm) {
		complete <- !is.na(x) & !is.na(y)
		x <- x[complete]
		y <- y[complete]
	}

	if (length(x) < 3) {
		return(NA_real_)
	}

	# check for zero variance
	if (var(x) == 0 || var(y) == 0) {
		if (var(x) == 0 && var(y) == 0 && all(x == y)) {
			# perfect correlation if both constant and equal
			return(1) 
		}
		# no correlation if one is constant
		return(0) 
	}

	return(correlation_cpp(x, y))
}

#' Ensure matrices have same dimensions
#'
#' Pads smaller matrix with zeros to match larger matrix dimensions
#'
#' @param mat1 First matrix
#' @param mat2 Second matrix
#' @return List with two matrices of same dimensions
#'
#' @keywords internal
#' @noRd
ensure_same_dimensions <- function(mat1, mat2) {
	n1 <- nrow(mat1)
	n2 <- nrow(mat2)

	if (n1 == n2 && ncol(mat1) == ncol(mat2)) {
		return(list(mat1 = mat1, mat2 = mat2))
	}

	# determine target dimensions
	n <- max(n1, n2, ncol(mat1), ncol(mat2))

	# pad matrices if needed
	if (n1 < n || ncol(mat1) < n) {
		new_mat1 <- matrix(0, n, n)
		new_mat1[1:n1, 1:ncol(mat1)] <- mat1
		mat1 <- new_mat1
	}

	if (n2 < n || ncol(mat2) < n) {
		new_mat2 <- matrix(0, n, n)
		new_mat2[1:n2, 1:ncol(mat2)] <- mat2
		mat2 <- new_mat2
	}

	return(list(mat1 = mat1, mat2 = mat2))
}
