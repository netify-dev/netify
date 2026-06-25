# miscellaneous functions for use in package

#' check if dependency is installed
#'
#' @keywords internal
#' @noRd
check_dependency <- function(library_name) {
	flag <- requireNamespace(library_name, quietly = TRUE)
	if (!isTRUE(flag)) {
		msg <- sprintf("Please install the `%s` package.", library_name)
		return(msg)
	} else {
		return(TRUE)
	}
}
assert_dependency <- checkmate::makeAssertionFunction(check_dependency)

#' convert a sparsematrix input to a dense base matrix with a size guard
#'
#' netify stores adjacencies densely. accepting a large, very sparse
#' sparsematrix and silently densifying can balloon ram (e.g. n = 15k
#' -> ~1.7 gb for the dense allocation, regardless of how sparse the
#' source was). bail out unless the caller opts in via
#' `force_dense = TRUE`.
#'
#' @param x a matrix-package sparse matrix (e.g. dgcmatrix).
#' @param force_dense logical; if TRUE skip the size/density guard.
#'
#' @return a base r matrix.
#'
#' @keywords internal
#' @noRd
densify_sparse_input <- function(x, force_dense = FALSE) {
	# matrix package must be present to interrogate sparse matrices
	if (!requireNamespace("Matrix", quietly = TRUE)) {
		cli::cli_abort(
			"Sparse matrix input requires the {.pkg Matrix} package."
		)
	}

	# basic shape
	dims <- dim(x)
	n_row <- dims[1]; n_col <- dims[2]
	n_cells <- as.numeric(n_row) * as.numeric(n_col)

	# density = nonzero / total cells. pattern matrices (ngcmatrix) have
	# no @x slot, so guard the slot access and fall back to matrix::nnzero
	nnz <- if (methods::.hasSlot(x, "x")) length(x@x) else Matrix::nnzero(x)
	density <- nnz / max(n_cells, 1)

	# guard against accidental gigabyte allocations: large n with very
	# sparse fill is almost certainly a mistake on the user's end
	too_big <- max(n_row, n_col) > 5000L
	too_sparse <- density < 0.01
	if (!isTRUE(force_dense) && too_big && too_sparse) {
		gb <- round((n_cells * 8) / (1024^3), 2)
		cli::cli_abort(
			c(
				"x" = "Refusing to densify a {n_row}x{n_col} sparseMatrix at density {round(density * 100, 3)}%.",
				"i" = "Dense storage would allocate ~{gb} GB (8 bytes per cell).",
				"!" = "Pass {.code force_dense = TRUE} to override, or build from an edgelist {.code data.frame} to skip the matrix intermediate. See {.code ?netify_workflows}."
			)
		)
	}

	# densify via matrix dispatch (handles dgcmatrix, dgtmatrix, lgcmatrix, ...)
	out <- as.matrix(x)
	# preserve dimnames in case matrix dropped them
	if (is.null(dimnames(out)) && !is.null(dimnames(x))) {
		dimnames(out) <- dimnames(x)
	}
	out
}

#' NULL-coalescing operator
#'
#'
#' @param x the primary value to check
#' @param y the fallback value to use if x is NULL
#'
#' @return returns x if x is not NULL, otherwise returns y
#'
#' @examples
#' # not run (internal function):
#' # x %||% y  # returns x if x is not NULL, otherwise y
#' # NULL %||% "default"  # returns "default"
#' # "value" %||% "default"  # returns "value"
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
	if (is.null(x)) {
		y
	} else {
		x
	}
}

#' char
#'
#' converts values into characters
#' @param x vector
#' @return character vector
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
char <- function(x) {
	as.character(x)
}


#' num
#'
#' converts values into character then numeric
#' @param x vector
#' @return numeric vector
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
num <- function(x) {
	as.numeric(char(x))
}

#' convert various time formats to numeric
#'
#' internal function to convert date, posixct, character, etc. to numeric
#' values that can be used for creating networks
#'
#' @param time_data vector of time values
#' @param time_col name of the time column (for error messages)
#'
#' @return a list with:
#'   - numeric_time: numeric version of the time variable
#'   - time_labels: character labels for the time periods
#'   - original_class: the original class of the time data
#'
#' @keywords internal
#' @noRd

convert_time_to_numeric <- function(time_data, time_col = "time") {
	original_class <- class(time_data)[1]

	if (is.numeric(time_data) || is.integer(time_data)) {
		# already numeric
		unique_times <- sort(unique(time_data))
		return(list(
			numeric_time = time_data,
			time_labels = char(unique_times),
			original_class = original_class
		))
	}

	if (inherits(time_data, "Date")) {
		# convert dates to numeric (days since origin)
		# but we'll use unique indices for the network
		unique_dates <- sort(unique(time_data))
		time_mapping <- setNames(seq_along(unique_dates), char(unique_dates))

		return(list(
			numeric_time = time_mapping[char(time_data)],
			time_labels = char(unique_dates),
			original_class = original_class
		))
	}

	if (inherits(time_data, c("POSIXct", "POSIXlt"))) {
		# preserve distinct timestamps rather than collapsing to calendar dates
		time_posix <- as.POSIXct(time_data)
		unique_times <- sort(unique(time_posix))
		time_keys <- char(time_posix)
		unique_keys <- char(unique_times)
		time_mapping <- setNames(seq_along(unique_times), unique_keys)

		return(list(
			numeric_time = time_mapping[time_keys],
			time_labels = format(unique_times, "%Y-%m-%d %H:%M:%S %Z"),
			original_class = original_class
		))
	}

	if (is.character(time_data)) {
		# for character, create ordered mapping
		unique_times <- sort(unique(time_data))
		time_mapping <- setNames(seq_along(unique_times), unique_times)

		# check if they look like dates and warn if so
		if (any(grepl("^\\d{4}-\\d{2}-\\d{2}$", utils::head(unique_times, 5)))) {
			cli::cli_alert_warning(
				"Character time variable looks like dates. Consider converting to Date class for better handling."
			)
		}

		return(list(
			numeric_time = time_mapping[time_data],
			time_labels = unique_times,
			original_class = original_class
		))
	}

	# this shouldn't happen if time_check is working correctly
	cli::cli_abort("Unsupported time variable type: {original_class}")
}

#' unique_vector
#'
#' get unique vector from
#' multiple vector inputs
#' @param ... vector inputs
#' @return numeric vector
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
unique_vector <- function(...) {
	u_vec <- unique(c(...))
	u_vec <- sort(u_vec)
	return(u_vec)
}

#' normalize_bipartite_nodelist
#'
#' parse partition-aware bipartite nodelists.
#'
#' @keywords internal
#' @noRd
normalize_bipartite_nodelist <- function(nodelist, row_actors, col_actors) {
	row_actors <- as.character(row_actors)
	col_actors <- as.character(col_actors)
	parse_mode <- function(x) {
		x <- tolower(as.character(x))
		ifelse(x %in% c("row", "rows", "row_actor", "row_actors", "actor1",
						"source", "sender", "from", "false", "0"),
			"row",
			ifelse(x %in% c("col", "cols", "column", "columns", "col_actor",
							"col_actors", "actor2", "target", "receiver", "to",
							"true", "1"),
				"col", NA_character_))
	}

	if (is.data.frame(nodelist)) {
		df <- as.data.frame(nodelist, stringsAsFactors = FALSE)
		actor_col <- intersect(c("actor", "name", "node", "vertex"), names(df))[1]
		mode_col <- intersect(c("mode", "type", "part", "partition", "bipartite_mode"), names(df))[1]
		if (is.na(actor_col) || is.na(mode_col)) {
			cli::cli_abort(c(
				"x" = "Bipartite {.arg nodelist} data frames need actor and mode columns.",
				"i" = "Use columns like {.code actor} and {.code mode}, with mode values {.val row} or {.val col}."
			))
		}
		mode <- parse_mode(df[[mode_col]])
		if (anyNA(mode)) {
			cli::cli_abort("{.arg nodelist} has bipartite mode values that are not {.val row} or {.val col}.")
		}
		rows <- as.character(df[[actor_col]][mode == "row"])
		cols <- as.character(df[[actor_col]][mode == "col"])
		return(list(
			rows = unique_vector(row_actors, rows),
			cols = unique_vector(col_actors, cols)
		))
	}

	if (is.list(nodelist)) {
		row_nm <- intersect(c("row", "rows", "row_actors", "actor1", "sources", "senders"), names(nodelist))[1]
		col_nm <- intersect(c("col", "cols", "columns", "col_actors", "actor2", "targets", "receivers"), names(nodelist))[1]
		if (is.na(row_nm) || is.na(col_nm)) {
			cli::cli_abort(c(
				"x" = "Bipartite {.arg nodelist} lists need row and column entries.",
				"i" = "Use {.code nodelist = list(row = row_actors, col = col_actors)}."
			))
		}
		return(list(
			rows = unique_vector(row_actors, as.character(unlist(nodelist[[row_nm]], use.names = FALSE))),
			cols = unique_vector(col_actors, as.character(unlist(nodelist[[col_nm]], use.names = FALSE)))
		))
	}

	flat <- as.character(nodelist)
	unknown <- setdiff(flat, c(row_actors, col_actors))
	if (length(unknown) > 0L) {
		cli::cli_abort(c(
			"x" = "A flat {.arg nodelist} cannot place bipartite isolate{?s} into row or column mode: {.val {unknown}}.",
			"i" = "Use {.code nodelist = list(row = ..., col = ...)} or a data frame with actor/mode columns."
		))
	}
	list(
		rows = unique_vector(row_actors, intersect(flat, row_actors)),
		cols = unique_vector(col_actors, intersect(flat, col_actors))
	)
}

#' identical_recursive
#'
#' recursively check if two or more objects are identical
#' @param ... objects to check
#' @return logical indicating whether objects are identical
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
identical_recursive <- function(...) {
	# keep comparison objects in a list
	args <- list(...)

	# if called with a single list argument, use that list
	if (length(args) == 1 && is.list(args[[1]])) {
		to_compare <- args[[1]]
	} else {
		to_compare <- args
	}

	# if just two, then just compare
	if (length(to_compare) == 2) {
		ident_logic <- identical(
			to_compare[[1]], to_compare[[2]]
		)
	}

	# if more than two, then recursively call fn
	if (length(to_compare) > 2) {
		ident_logic <- c(
			identical(to_compare[[1]], to_compare[[2]]),
			identical_recursive(to_compare[-1])
		)
	}

	# return true if all are identical
	return(all(ident_logic))
}

#' break string into list of strings by some fixed character
#' and then extract the desired values around that fixed
#' character
#'
#' @param string_to_split character: string to be split
#' @param break_by character: character to break string by
#' @param to_extract integer: index of the string to be extracted
#' @param fixed if `TRUE` match exactly, otherwise use regular expressions
#' @return a character vector of the extracted strings
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
split_string <- function(string_to_split, break_by, to_extract, fixed = TRUE) {
	str_to_list <- strsplit(string_to_split, break_by, fixed = fixed)
	extract_relev <- lapply(str_to_list, function(x) {
		x[to_extract]
	})
	return(unlist(extract_relev))
}

#' array_to_list
#'
#' this function converts a three dimensional array
#' into a list of matrices
#' @param arr three dimensional array to list
#' @param preserveattr logical indicating whether to preserve attributes
#' @return list object
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
array_to_list <- function(arr, preserveAttr = TRUE) {
	# convert to list elements along third mode
	l <- lapply(1:dim(arr)[3], function(ii) {
		arr[, , ii]
	})
	names(l) <- dimnames(arr)[[3]]

	# if we want to preserve attrs and keep netify object
	if (preserveAttr) {
		# add back in netify attribs to top level list
		class(l) <- "netify"
		attr(l, "netify_type") <- "longit_list"
		arr_attr <- attributes(arr)
		for (ii in 5:length(arr_attr)) {
			attr(l, names(arr_attr)[ii]) <- arr_attr[[ii]]
		}

		# adjust array attributes for cross_sec
		arr_attr_cross <- arr_attr[1:15]
		arr_attr_cross["dim"]$dim <- arr_attr_cross["dim"]$dim[1:2]
		arr_attr_cross["dimnames"]$dimnames <- arr_attr_cross["dimnames"]$dimnames[1:2]
		arr_attr_cross["netify_type"] <- "cross_sec"
		arr_attr_cross["actor_pds"] <- NULL

		# add attributes to every element in list
		for (ii in seq_along(l)) {
			attributes(l[[ii]]) <- arr_attr_cross
		}
	}

	#
	return(l)
}

#' list_to_array
#'
#' this function converts a list of matrices
#' into a three dimensional array
#' @param list_of_mats list object
#' @return three dimensional array
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
list_to_array <- function(list_of_mats) {
	# get dim info to create array
	row_actors <- unique(unlist(lapply(list_of_mats, rownames)))
	col_actors <- unique(unlist(lapply(list_of_mats, colnames)))
	time_points <- names(list_of_mats)

	# create array
	arr <- array(NA,
		dim = c(length(row_actors), length(col_actors), length(time_points)),
		dimnames = list(row_actors, col_actors, time_points)
	)

	# fill array
	for (ii in 1:length(list_of_mats)) {
		list_of_mats_slice <- list_of_mats[[ii]]
		rows_sl <- rownames(list_of_mats_slice)
		cols_sl <- colnames(list_of_mats_slice)
		arr[rows_sl, cols_sl, ii] <- list_of_mats_slice
	}

	#
	return(arr)
}


#' evaluate code with a temporary seed
#'
#' wraps `withr::with_seed()` so functions can provide reproducible seeded
#' work without directly assigning into the user's global environment.
#'
#' @param seed optional integer seed.
#' @param code expression to evaluate.
#' @return the result of `code`.
#' @keywords internal
#' @noRd
with_local_seed <- function(seed, code) {
	if (is.null(seed)) {
		force(code)
	} else {
		withr::with_seed(seed, code)
	}
}
