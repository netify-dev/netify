####
#' Is this object a netify object?
#'
#' @param x An R object
#' @return Logical constant, \code{TRUE} if argument \code{x} is a netify
#' object
#' @author Colin Henry
#' @keywords netify
#' @export is_netify

is_netify <- function(x) {
	"netify" %in% class(x)
}
####

####
#' netify_check
#'
#' Checks to make sure that object is of class netify
#' and stops process if not
#' @param netlet user inputted object to check
#' @return NULL object but stops the process if there
#' is an error detected
#' @author Ha Eun Choi, Colin Henry, Shahryar Minhas
#' @keywords internal
#' @noRd

netify_check <- function(netlet) {
	if (!is_netify(netlet)) {
		cli::cli_abort("check data type. Inputted object is not a `netify` object.")
	}

	return(invisible(NULL))
}
####

####
#' Low-level constructor for netify objects
#'
#' `new_netify` (also available as `new_netlet`) is a low-level constructor that
#' creates netify objects from raw matrix, array, or list data structures. This
#' function automatically detects network properties and sets appropriate attributes,
#' making it useful for converting existing network data into the netify format.
#'
#' @param data A network data structure to convert:
#'   \itemize{
#'     \item \strong{Matrix}: Creates a cross-sectional netify object
#'     \item \strong{3D array}: Creates a longitudinal array netify object
#'       (dimensions: actors × actors × time)
#'     \item \strong{List of matrices}: Creates a longitudinal list netify object
#'       (useful for time-varying actor composition)
#'   }
#' @param ... Additional parameters to set as attributes on the netify object.
#'   Common parameters include:
#'   \itemize{
#'     \item \code{symmetric}: Logical indicating if network is undirected
#'     \item \code{mode}: "unipartite" or "bipartite"
#'     \item \code{weight}: Name of the edge weight variable
#'     \item \code{diag_to_NA}: Whether to set diagonal to NA
#'     \item \code{missing_to_zero}: Whether to treat missing edges as zeros
#'     \item \code{nodal_data}: Data frame of node attributes
#'     \item \code{dyad_data}: Dyadic attributes (see netify documentation)
#'   }
#'
#'   If not provided, these properties are automatically detected from the data.
#'
#' @return A netify object with class "netify" and appropriate structure:
#'   \itemize{
#'     \item For matrices: A single netify matrix with netify_type = "cross_sec"
#'     \item For arrays: A netify array with netify_type = "longit_array"
#'     \item For lists: A netify list with netify_type = "longit_list", where
#'       each element is itself a netify object
#'   }
#'
#'   All netify objects include automatically detected or user-specified attributes
#'   for network properties, making them ready for use with netify functions.
#'
#' @details
#' \strong{Automatic property detection:}
#'
#' When properties are not explicitly provided, `new_netify` intelligently detects:
#' \itemize{
#'   \item \strong{Symmetry}: Checks if matrix equals its transpose
#'   \item \strong{Mode}: Infers unipartite/bipartite from dimensions and actor names
#'   \item \strong{Edge weights}: Detects binary (0/1) vs. weighted networks
#'   \item \strong{Diagonal treatment}: Checks if diagonal contains all NAs
#'   \item \strong{Missing values}: Determines if NAs exist off-diagonal
#'   \item \strong{Actor composition}: For longitudinal data, detects if actors
#'     remain constant or vary over time
#' }
#'
#' \strong{Naming conventions:}
#'
#' If row/column names are not provided:
#' \itemize{
#'   \item Unipartite networks: Actors named "a1", "a2", ...
#'   \item Bipartite networks: Row actors "r1", "r2", ...; column actors "c1", "c2", ...
#'   \item Time periods: Named as "1", "2", ... if not specified
#' }
#'
#' \strong{Longitudinal data handling:}
#'
#' For longitudinal networks:
#' \itemize{
#'   \item Arrays assume constant actor composition across time
#'   \item Lists allow for time-varying actor composition
#'   \item Each time slice in a list becomes a separate cross-sectional netify object
#'   \item Properties are detected across all time periods (e.g., symmetric if ALL
#'     time slices are symmetric)
#' }
#'
#' @note
#' This is a low-level constructor primarily intended for package developers or
#' advanced users. Most users should use the higher-level `netify()` function,
#' which provides more comprehensive data validation and preprocessing.
#'
#' The function does not support multilayer networks directly. To create multilayer
#' networks, create separate netify objects and combine them with `layer_netify()`.
#'
#' While the function attempts to detect network properties automatically, explicitly
#' providing these parameters is recommended for clarity.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export new_netify

new_netify <- function(data, ...) {
	default_params <- list(
		netify_type = NULL,
		actor_time_uniform = NULL,
		actor_pds = NULL,
		weight = NULL,
		detail_weight = NULL,
		is_binary = NULL,
		symmetric = NULL,
		mode = NULL,
		layers = NULL,
		diag_to_NA = NULL,
		missing_to_zero = NULL,
		sum_dyads = FALSE,
		nodal_data = NULL,
		dyad_data = NULL,
		graph_data = NULL
	)

	user_params <- list(...)
	final_params <- utils::modifyList(default_params, user_params)

	data_class <- class(data)[1]
	if (!data_class %in% c("matrix", "array", "list")) {
		cli::cli_abort("`data` must be a matrix, array, or list.")
	}

	# reject multilayer inputs here
	if ((data_class == "array" && length(dim(data)) > 3) ||
		(data_class == "list" && any(
			sapply(data, function(x) !is.matrix(x) || length(dim(x)) > 2)
		))) {
		cli::cli_abort(
			"`new_netify` doesn't support multilayer networks currently. Please create separate netlets with `new_netify` and then use the `layer_netify` function to combine into a multilayer netify object."
		)
	}

	netify_type <- switch(data_class,
		"matrix" = "cross_sec",
		"array"  = "longit_array",
		"list"   = "longit_list"
	)

	# attributes the user did not set must be detected
	detect_symmetric <- is.null(final_params$symmetric)
	detect_mode <- is.null(final_params$mode)
	detect_diag_to_NA <- is.null(final_params$diag_to_NA)
	detect_missing_to_zero <- is.null(final_params$missing_to_zero)
	detect_weight <- is.null(final_params$weight)
	detect_is_binary <- is.null(final_params$is_binary)
	detect_actor_time_uniform <- is.null(final_params$actor_time_uniform)
	detect_actor_pds <- is.null(final_params$actor_pds)

	if (netify_type == "cross_sec") {
		mat <- data
		if (detect_symmetric) {
			final_params$symmetric <- check_symmetric(mat)
		}
		if (detect_mode) {
			final_params$mode <- guess_mode(mat)
		}
		if (detect_diag_to_NA) {
			final_params$diag_to_NA <- guess_diag_to_NA(mat)
		}
		if (detect_missing_to_zero) {
			final_params$missing_to_zero <- guess_missing_to_zero(
				mat, final_params$diag_to_NA
			)
		}
		if (detect_weight) {
			if (check_binary(mat)) {
				final_params$weight <- NULL
				final_params$detail_weight <- "Binary ties"
				final_params$is_binary <- TRUE
			} else {
				final_params$weight <- "edge_value"
				final_params$detail_weight <- paste0("Edges weighted by ", final_params$weight)
				final_params$is_binary <- FALSE
			}
		} else if (detect_is_binary) {
			final_params$is_binary <- check_binary(mat)
		}

		mat <- assign_dimnames(mat, final_params$mode)
		data <- mat

		if (detect_actor_time_uniform) {
			final_params$actor_time_uniform <- TRUE
		}

		if (detect_actor_pds) {
			final_params$actor_pds <- data.frame(
				actor = unique(c(rownames(mat), colnames(mat))),
				min_time = 1, max_time = 1,
				stringsAsFactors = FALSE
			)
		}

	} else if (netify_type == "longit_array") {
		dims <- dim(data)

		if (detect_mode) {
			final_params$mode <- guess_mode(data[, , 1, drop = TRUE])
		}

		data <- assign_dimnames(data, final_params$mode)

		if (is.null(dimnames(data)[[3]])) {
			cdim <- dimnames(data)
			cdim[[3]] <- seq_len(dims[3])
			dimnames(data) <- cdim
		}

		# scan slices to detect symmetry and binary status
		any_not_sym <- FALSE
		any_nonbinary <- FALSE
		for (ii in seq_len(dims[3])) {
			mat <- data[, , ii, drop = TRUE]
			if (detect_symmetric && !any_not_sym) {
				if (!check_symmetric(mat)) any_not_sym <- TRUE
			}
			if (detect_weight && !any_nonbinary) {
				if (!check_binary(mat)) any_nonbinary <- TRUE
			}
		}
		if (detect_mode) {
			final_params$mode <- guess_mode(data[, , 1, drop = TRUE])
		}
		if (detect_symmetric) {
			final_params$symmetric <- !any_not_sym
		}
		if (detect_diag_to_NA) {
			final_params$diag_to_NA <- all(
				sapply(
					seq_len(dims[3]), function(ii) guess_diag_to_NA(data[, , ii, drop = TRUE])
				)
			)
		}
		if (detect_missing_to_zero) {
			final_params$missing_to_zero <- all(
				sapply(
					seq_len(dims[3]), function(ii) {
						guess_missing_to_zero(data[, , ii, drop = TRUE], final_params$diag_to_NA)
					}
				)
			)
		}
		if (detect_weight) {
			if (!any_nonbinary) {
				final_params$weight <- NULL
				final_params$detail_weight <- "Binary ties"
				final_params$is_binary <- TRUE
			} else {
				final_params$weight <- "edge_value"
				final_params$detail_weight <- paste0("Edges weighted by ", final_params$weight)
				final_params$is_binary <- FALSE
			}
		} else if (detect_is_binary) {
			any_nonbin <- FALSE
			for (ii in seq_len(dims[3])) {
				if (!check_binary(data[, , ii, drop = TRUE])) {
					any_nonbin <- TRUE; break
				}
			}
			final_params$is_binary <- !any_nonbin
		}

		if (detect_actor_time_uniform) {
			final_params$actor_time_uniform <- TRUE
		}

		if (detect_actor_pds) {
			pds <- dimnames(data)[[3]]
			final_params$actor_pds <- data.frame(
				actor = unique(c(rownames(mat), colnames(mat))),
				min_time = pds[1], max_time = pds[length(pds)],
				stringsAsFactors = FALSE
			)
		}

	} else if (netify_type == "longit_list") {
		if (is.null(names(data))) {
			names(data) <- seq_along(data)
		}

		# infer mode from first slice if needed
		guess_md <- final_params$mode
		if (
			detect_mode && length(data) > 0 && is.matrix(data[[1]])
		) {
			guess_md <- guess_mode(data[[1]])
		}

		any_not_sym <- FALSE
		any_nonbinary <- FALSE
		for (ii in seq_along(data)) {
			mat <- data[[ii]]
			if (!is.matrix(mat)) next
			if (is.null(guess_md)) {
				guess_md <- guess_mode(mat)
			}
			mat <- assign_dimnames(mat, guess_md)
			data[[ii]] <- mat

			if (detect_symmetric && !any_not_sym) {
				if (!check_symmetric(mat)) any_not_sym <- TRUE
			}
			if (detect_weight && !any_nonbinary) {
				if (!check_binary(mat)) any_nonbinary <- TRUE
			}
		}

		if (detect_mode && !is.null(guess_md)) {
			final_params$mode <- guess_md
		}
		if (detect_symmetric) {
			final_params$symmetric <- !any_not_sym
		}
		if (detect_diag_to_NA) {
			final_params$diag_to_NA <- all(sapply(data, guess_diag_to_NA))
		}
		if (detect_missing_to_zero) {
			final_params$missing_to_zero <- all(
				unlist(lapply(data, function(mat) {
					guess_missing_to_zero(mat, final_params$diag_to_NA)
				}))
			)
		}
		if (detect_weight) {
			if (!any_nonbinary) {
				final_params$weight <- NULL
				final_params$detail_weight <- "Binary ties"
				final_params$is_binary <- TRUE
			} else {
				final_params$weight <- "edge_value"
				final_params$detail_weight <- paste0("Edges weighted by ", final_params$weight)
				final_params$is_binary <- FALSE
			}
		} else if (detect_is_binary) {
			any_nonbin <- FALSE
			for (ii in seq_along(data)) {
				m <- data[[ii]]
				if (!is.matrix(m)) next
				if (!check_binary(m)) { any_nonbin <- TRUE; break }
			}
			final_params$is_binary <- !any_nonbin
		}

		if (is.null(final_params$mode)) {
			final_params$mode <- "unipartite"
		}

		# detect whether actor set is stable across time
		if (detect_actor_time_uniform) {
			same_actors <- TRUE
			if (length(data) > 0 && is.matrix(data[[1]])) {
				row0 <- rownames(data[[1]])
				col0 <- colnames(data[[1]])
				for (i in seq_along(data)) {
					if (!identical(row0, rownames(data[[i]])) ||
						!identical(col0, colnames(data[[i]]))) {
						same_actors <- FALSE
						break
					}
				}
			}
			final_params$actor_time_uniform <- same_actors
		}

		if (detect_actor_pds) {
			if (final_params$actor_time_uniform) {
				pds <- names(data)
				actors <- unique_vector(
					unlist(lapply(data, function(mat) c(rownames(mat), colnames(mat))))
				)
				actor_pds <- data.frame(
					actor = actors, min_time = pds[1], max_time = pds[length(pds)],
					stringsAsFactors = FALSE
				)
			}

			if (!final_params$actor_time_uniform) {
				actor_pds <- get_actor_time_info(
					melt_list_sparse(data), "Var1", "Var2", "L1"
				)
			}
		}
	}

	# enforce resolved diag_to_NA / missing_to_zero on the data so flags
	# and contents stay in sync. explicit FALSE leaves data untouched.
	apply_diag_NA <- isTRUE(final_params$diag_to_NA)
	apply_miss_0  <- isTRUE(final_params$missing_to_zero)
	apply_flags_to_slice <- function(mat) {
		if (!is.matrix(mat)) return(mat)
		# diag_to_NA only meaningful on square (unipartite) matrices
		if (apply_diag_NA && nrow(mat) == ncol(mat)) {
			if (any(!is.na(diag(mat)))) diag(mat) <- NA
		}
		if (apply_miss_0 && anyNA(mat)) {
			# preserve diagonal NAs when filling off-diagonal NAs
			if (apply_diag_NA && nrow(mat) == ncol(mat)) {
				d <- diag(mat)
				mat[is.na(mat)] <- 0
				diag(mat) <- d
			} else {
				mat[is.na(mat)] <- 0
			}
		}
		mat
	}
	if (netify_type == "cross_sec") {
		data <- apply_flags_to_slice(data)
	} else if (netify_type == "longit_array") {
		dims_x <- dim(data)
		for (ii in seq_len(dims_x[3])) {
			data[, , ii] <- apply_flags_to_slice(data[, , ii, drop = TRUE])
		}
	} else if (netify_type == "longit_list") {
		for (ii in seq_along(data)) {
			data[[ii]] <- apply_flags_to_slice(data[[ii]])
		}
	}

	# default layer label
	if (is.null(final_params$layers)) {
		if (is.null(final_params$weight)) {
			final_params$layers <- "weight1"
		} else {
			final_params$layers <- as.character(final_params$weight)
		}
	}

	if (is.null(final_params$detail_weight)) {
		if (!is.null(final_params$weight)) {
			final_params$detail_weight <- paste0("Edges weighted by ", final_params$weight)
		} else {
			final_params$detail_weight <- "Binary ties"
		}
	}

	# tag each slice in a list as its own cross_sec netify
	if (netify_type == "longit_list") {
		for (ii in seq_along(data)) {
			mat_slice <- data[[ii]]
			if (!is.matrix(mat_slice)) {
				cli::cli_abort(
					"all elements of the 'list' must be matrices."
				)
			}
			attributes(mat_slice) <- c(attributes(mat_slice), final_params)
			attr(mat_slice, "netify_type") <- "cross_sec"
			attr(mat_slice, "actor_time_uniform") <- NULL
			attr(mat_slice, "actor_pds") <- NULL
			attr(mat_slice, "nodal_data") <- NULL
			attr(mat_slice, "dyad_data") <- NULL
			class(mat_slice) <- "netify"
			data[[ii]] <- mat_slice
		}
	}

	out <- structure(
		data,
		class = "netify",
		netify_type = netify_type
	)

	for (nm in names(final_params)) {
		if (nm == "netify_type") next
		attr(out, nm) <- final_params[[nm]]
	}

	return(out)
}
####

####
#' Assign Default Row/Column Names
#'
#' Internal helper function that assigns default row and column names to a matrix,
#' depending on whether the network is unipartite or bipartite. If the matrix
#' already has row/col names, they are preserved.
#'
#' @param mat A matrix (potentially without row or column names).
#' @param mode Character string: "unipartite" or "bipartite".
#'
#' @return The same \code{mat} with updated row and column names as needed.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
assign_dimnames <- function(mat, mode) {
	nr <- nrow(mat)
	nc <- ncol(mat)

	# paste0("a", seq_len(0)) recycles to "a"; guard the zero-dim case
	make_names <- function(n, prefix) {
		if (n == 0L) character(0) else paste0(prefix, seq_len(n))
	}

	if (is.null(rownames(mat))) {
		prefix <- if (mode == "unipartite") "a" else "r"
		rownames(mat) <- make_names(nr, prefix)
	}

	if (is.null(colnames(mat))) {
		prefix <- if (mode == "unipartite") "a" else "c"
		colnames(mat) <- make_names(nc, prefix)
	}

	return(mat)
}
####

####
#' Check if a Matrix is Symmetric (Ignoring NA Diagonals)
#'
#' Internal helper that checks whether \code{mat} is equal to its transpose,
#' after temporarily setting any \code{NA} diagonal entries to \code{0}.
#'
#' @param mat A matrix to be checked.
#'
#' @return Logical. \code{TRUE} if \code{mat} is symmetric, otherwise \code{FALSE}.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
check_symmetric <- function(mat) {
	diag(mat) <- 0
	return(identical(mat, t(mat)))
}
####

####
#' Check if Matrix is Strictly Binary
#'
#' Internal helper that tests if all non-\code{NA} entries of \code{mat}
#' are in \code{\{0, 1\}}.
#'
#' @param mat A matrix to be tested.
#'
#' @return Logical. \code{TRUE} if all non-NA entries are 0/1, otherwise \code{FALSE}.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
check_binary <- function(mat) {
	non_na_vals <- mat[!is.na(mat)]
	return(!any(non_na_vals != 0 & non_na_vals != 1))
}
####

####
#' Check if Diagonal is Entirely NA
#'
#' Internal helper that checks whether the diagonal of \code{mat} is fully
#' \code{NA}. Used to guess whether \code{diag_to_NA} should be \code{TRUE}.
#'
#' @param mat A matrix to examine.
#'
#' @return Logical. \code{TRUE} if \code{diag(mat)} is all \code{NA}, otherwise \code{FALSE}.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
guess_diag_to_NA <- function(mat) {
	return(all(is.na(diag(mat))))
}
####

####
#' Check if Missing Values Appear Off-Diagonal
#'
#' Internal helper that determines if any off-diagonal entries of \code{mat}
#' are \code{NA}. Sets diagonal to zero temporarily, then checks for \code{NA}.
#'
#' @param mat A matrix to examine.
#' @param diag_NA Logical. If \code{TRUE}, treat diagonal entries as \code{0} for the
#'  purpose of this check. If \code{FALSE}, diagonal entries are not considered.
#'
#' @return Logical. \code{TRUE} if no off-diagonal \code{NA}s are found,
#'   otherwise \code{FALSE}.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
guess_missing_to_zero <- function(mat, diag_NA = TRUE) {
	if (diag_NA) {
		diag(mat) <- 0
	}

	return(!any(is.na(mat)))
}
####

####
#' Guess Whether Matrix is Unipartite or Bipartite
#'
#' Internal helper that infers if \code{mat} should be treated as unipartite or
#' bipartite based on its dimensions and row/column name overlap.
#'
#' @param mat A matrix whose dimensions/names we examine.
#'
#' @return Character. Either \code{"unipartite"} or \code{"bipartite"}.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
guess_mode <- function(mat) {
	nr <- nrow(mat)
	nc <- ncol(mat)

	if (nr != nc) {
		return("bipartite")
	}

	rown <- rownames(mat)
	coln <- colnames(mat)

	if (!is.null(rown) && !is.null(coln)) {
		if (length(intersect(rown, coln)) == length(rown)) {
			return("unipartite")
		} else {
			return("bipartite")
		}
	}

	return("unipartite")
}
####
