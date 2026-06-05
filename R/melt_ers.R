#' Melt methods for netify objects
#'
#' convert netify matrices/arrays to long format data frames.
#' these methods provide a consistent interface for melting different
#' types of netify objects while leveraging c++ for performance.
#'
#' @param data an object to be melted (e.g., a netify object)
#' @param ... additional arguments passed to methods
#'
#' @return see method-specific documentation (e.g., \code{\link{melt.netify}})
#'
#' @name melt
#' @rdname melt
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
melt <- function(data, ...) {
	UseMethod("melt")
}

#' @rdname melt
#' @method melt netify
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
#' @param data a netify object
#' @param ... additional arguments (see details)
#' @param remove_diagonal logical. remove diagonal elements (default: TRUE)
#' @param remove_zeros logical. remove zero values (default: TRUE)
#' @param na.rm logical. remove na values (default: TRUE)
#' @param value.name character. name for value column (default: "value")
#' @return data frame with columns: `var1`, `var2`, `value` (and optionally
#'   `time` / `layer`). the `var1` / `var2` names are inherited from base r's
#'   `as.data.frame.table` / `reshape2::melt` heritage and are used by internal
#'   helpers (`decompose_helpers`, `plot_homophily`, etc.); rename them yourself
#'   downstream if you need snake_case (e.g.
#'   `rlang::set_names(out, c("from", "to", "value", ...))`). for a fully
#'   snake_case, dyad-attribute-merged edge frame, use [unnetify()] or
#'   [as_tibble.netify()] instead.
#' @details
#' the melt method converts netify objects from their matrix representation to
#' a long format data frame suitable for analysis and visualization. the output
#' format depends on the type of netify object:
#' \itemize{
#'   \item cross-sectional: returns columns `var1`, `var2`, `value`
#'   \item longitudinal: returns columns `var1`, `var2`, `time`, `value`
#'   \item multilayer: returns columns `var1`, `var2`, `layer`, `value` (and `time` if longitudinal)
#' }
melt.netify <- function(data, ...,
						remove_diagonal = TRUE,
						remove_zeros = TRUE,
						na.rm = TRUE,
						value.name = "value") {
	attrs <- attributes(data)
	netify_type <- attrs$netify_type

	if (netify_type == "cross_sec") {
		# simple matrix case
		return(melt_matrix(as.matrix(data),
			remove_diagonal = remove_diagonal,
			remove_zeros = remove_zeros,
			na.rm = na.rm,
			value.name = value.name
		))
	} else if (netify_type == "longit_array") {
		# 3d array case
		return(melt_array(data,
			remove_diagonal = remove_diagonal,
			remove_zeros = remove_zeros,
			na.rm = na.rm,
			value.name = value.name
		))
		} else if (netify_type == "longit_list") {
			# list of matrices case
			results <- lapply(seq_along(data), function(i) {
				df <- melt_matrix(data[[i]],
				remove_diagonal = remove_diagonal,
				remove_zeros = remove_zeros,
				na.rm = na.rm,
				value.name = value.name
			)
			if (nrow(df) > 0) {
				df$time <- names(data)[i]
			}
			df
			})

			# combine results
			nonempty <- results[sapply(results, nrow) > 0]
			if (length(nonempty) == 0L) {
				result <- results[[1]][0, , drop = FALSE]
				result$time <- character(0)
				return(result[, c("Var1", "Var2", "time", value.name), drop = FALSE])
			}
			result <- do.call(rbind, nonempty)
			rownames(result) <- NULL
			return(result)
		} else if (netify_type == "multilayer") {
		# multilayer case
		layer_names <- names(data)
		results <- lapply(seq_along(data), function(i) {
			df <- melt.netify(data[[i]],
				remove_diagonal = remove_diagonal,
				remove_zeros = remove_zeros,
				na.rm = na.rm,
				value.name = value.name
			)
			if (nrow(df) > 0) {
				df$layer <- layer_names[i]
			}
			df
		})

			# combine results
			nonempty <- results[sapply(results, nrow) > 0]
			if (length(nonempty) == 0L) {
				result <- results[[1]][0, , drop = FALSE]
				result$layer <- character(0)
				return(result)
			}
			result <- do.call(rbind, nonempty)
			rownames(result) <- NULL
			return(result)
	}

	stop("Unknown netify type: ", netify_type)
}

#' melt array to long format
#'
#' internal function to melt 3d arrays
#'
#' @param arr 3d array
#' @param remove_diagonal remove diagonal elements
#' @param remove_zeros remove zero values
#' @param na.rm remove na values
#' @param value.name name for value column
#' @return data frame with row, col, time, and value columns
#'
#' @keywords internal
#' @noRd
melt_array <- function(arr, remove_diagonal = TRUE, remove_zeros = TRUE,
					   na.rm = TRUE, value.name = "value") {
	dims <- dim(arr)
	dn <- dimnames(arr)

	# process each time slice
	results <- lapply(seq_len(dims[3]), function(k) {
		df <- melt_matrix(arr[, , k],
			remove_diagonal = remove_diagonal,
			remove_zeros = remove_zeros,
			na.rm = na.rm,
			value.name = value.name
		)
		if (nrow(df) > 0) {
			df$L1 <- if (!is.null(dn[[3]])) dn[[3]][k] else as.character(k)
		}
		df
	})

	# combine
	nonempty <- results[sapply(results, nrow) > 0]
	if (length(nonempty) == 0L) {
		result <- results[[1]][0, , drop = FALSE]
		result$L1 <- character(0)
		return(result[, c("Var1", "Var2", "L1", value.name), drop = FALSE])
	}
	result <- do.call(rbind, nonempty)
	rownames(result) <- NULL

	# reorder columns
	L1_col <- which(names(result) == "L1")
	other_cols <- setdiff(seq_along(result), L1_col)
	result[c(other_cols[1:2], L1_col, other_cols[3:length(other_cols)])]
}

#' legacy function names for backwards compatibility
#' @keywords internal
#' @noRd
melt_matrix_base <- function(mat) {
	melt_matrix(mat, remove_diagonal = FALSE, remove_zeros = FALSE, na.rm = FALSE)
}

#' @keywords internal
#' @noRd
melt_matrix_sparse <- function(mat, remove_zeros = TRUE, remove_diagonal = TRUE) {
	result <- melt_matrix(mat, remove_diagonal = remove_diagonal, remove_zeros = remove_zeros, na.rm = TRUE)
	# keep column names consistent
	names(result)[names(result) == "row"] <- "Var1"
	names(result)[names(result) == "col"] <- "Var2"
	return(result)
}

#' @keywords internal
#' @noRd
melt_array_sparse <- function(arr, remove_zeros = TRUE, remove_diagonal = TRUE) {
	melt_array(arr, remove_diagonal = remove_diagonal, remove_zeros = remove_zeros, na.rm = TRUE)
}

#' @keywords internal
#' @noRd
melt_list_sparse <- function(lst, remove_zeros = TRUE, remove_diagonal = TRUE) {
	results <- lapply(seq_along(lst), function(i) {
		df <- melt_matrix(lst[[i]],
			remove_diagonal = remove_diagonal,
			remove_zeros = remove_zeros,
			na.rm = TRUE
		)
		if (nrow(df) > 0) {
			df$L1 <- names(lst)[i]
		}
		df
	})

	# combine results
	nonempty <- results[sapply(results, nrow) > 0]
	if (length(nonempty) == 0L) {
		return(data.frame(
			Var1 = character(),
			Var2 = character(),
			L1 = character(),
			value = numeric(),
			stringsAsFactors = FALSE
		))
	}
	result <- do.call(rbind, nonempty)
	rownames(result) <- NULL

	# reorder columns
	time_col <- which(names(result) == "L1")
	other_cols <- setdiff(seq_along(result), time_col)
	result[c(other_cols[1:2], time_col, other_cols[3:length(other_cols)])]
}

#' melt variable time list
#' @keywords internal
#' @noRd
melt_var_time_list <- function(var_time_list) {
	if (length(var_time_list) == 0) {
		return(data.frame(
			Var1 = character(),
			Var2 = character(),
			Var3 = character(),
			L1 = character(),
			value = numeric(),
			stringsAsFactors = FALSE
		))
	}

	# process each time period in the list
	results <- lapply(names(var_time_list), function(time_name) {
		time_data <- var_time_list[[time_name]]

		if (is.list(time_data)) {
			# this is a list of variables for this time period
			var_results <- lapply(names(time_data), function(var_name) {
				mat <- time_data[[var_name]]
				if (!is.null(mat) && length(mat) > 0) {
					# handle character matrices differently
					if (is.character(mat)) {
						df <- melt_matrix_character(mat, remove_diagonal = TRUE)
					} else {
						df <- melt_matrix(mat, remove_diagonal = TRUE, remove_zeros = FALSE, na.rm = FALSE)
					}
					if (nrow(df) > 0) {
						df$Var3 <- var_name
						df$L1 <- time_name
						df
					}
				}
			})
			do.call(rbind, Filter(Negate(is.null), var_results))
		} else if (is.matrix(time_data)) {
			# fallback single-matrix branch (character handled separately)
			if (is.character(time_data)) {
				df <- melt_matrix_character(time_data, remove_diagonal = TRUE)
			} else {
				df <- melt_matrix(time_data, remove_diagonal = TRUE, remove_zeros = FALSE, na.rm = FALSE)
			}
			if (nrow(df) > 0) {
				df$Var3 <- time_name # use time_name as variable name in this case
				df$L1 <- "1"
				df
			}
		}
	})

	# combine all results
	result <- do.call(rbind, Filter(Negate(is.null), results))
	if (!is.null(result) && nrow(result) > 0) {
		rownames(result) <- NULL
		# reorder columns
		result[c("Var1", "Var2", "Var3", "L1", "value")]
	} else {
		data.frame(
			Var1 = character(),
			Var2 = character(),
			Var3 = character(),
			L1 = character(),
			value = numeric(),
			stringsAsFactors = FALSE
		)
	}
}

#' melt character matrix
#' @keywords internal
#' @noRd
melt_matrix_character <- function(mat, remove_diagonal = TRUE) {
	if (!is.matrix(mat)) {
		mat <- as.matrix(mat)
	}

	# get dimensions
	n_rows <- nrow(mat)
	n_cols <- ncol(mat)

	# get row and column names
	row_names <- rownames(mat)
	col_names <- colnames(mat)

	if (is.null(row_names)) row_names <- as.character(1:n_rows)
	if (is.null(col_names)) col_names <- as.character(1:n_cols)

	# create indices
	rows <- rep(row_names, times = n_cols)
	cols <- rep(col_names, each = n_rows)
	values <- as.vector(mat)

	# create data frame
	result <- data.frame(
		Var1 = rows,
		Var2 = cols,
		value = values,
		stringsAsFactors = FALSE
	)

	# remove diagonal if requested
	if (remove_diagonal) {
		diag_idx <- result$Var1 != result$Var2
		result <- result[diag_idx, ]
	}

	# remove rows with na values
	result <- result[!is.na(result$value), ]

	rownames(result) <- NULL
	return(result)
}

#' melt data frame for plotting
#' @keywords internal
#' @noRd
melt_df <- function(data, id) {
	# get value columns (all columns not in id)
	value_cols <- setdiff(names(data), id)

	# create a list to store melted data
	melted_list <- list()

	# process each value column
	for (var in value_cols) {
		# create a subset with id columns and current value column
		subset_data <- data[c(id, var)]
		subset_data$variable <- var
		names(subset_data)[names(subset_data) == var] <- "value"
		melted_list[[var]] <- subset_data
	}

	# combine all melted data
	result <- do.call(rbind, melted_list)
	rownames(result) <- NULL

	# reorder columns to put variable before value
	col_order <- c(id, "variable", "value")
	result <- result[, col_order]

	return(result)
}
