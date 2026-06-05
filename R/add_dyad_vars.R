#' Add dyadic variables to a netify object
#'
#' `add_dyad_vars` (also available as `add_edge_attributes`)
#' merges additional dyadic (edge-level) variables from a data.frame
#' into an existing netify object. this function allows you to incrementally
#' build up the dyadic attributes of your network after initial creation, which
#' is useful when variables come from different sources or need different
#' preprocessing.
#'
#' @param netlet a netify object (class "netify") to which dyadic variables will be added.
#' @param dyad_data a data.frame object containing the dyadic variables to add. must
#'   include columns matching the actor1, actor2, and time specifications used
#'   in the original netify object. will be coerced to data.frame if a tibble or
#'   data.table is provided.
#' @param actor1 character string specifying the column name in dyad_data for the first actor
#'   in each dyad. should match the actor1 specification used when creating the
#'   netify object.
#' @param actor2 character string specifying the column name in dyad_data for the second actor
#'   in each dyad. should match the actor2 specification used when creating the
#'   netify object.
#' @param time character string specifying the column name in dyad_data for time periods.
#'   required for longitudinal netify objects. should match the time specification
#'   used when creating the netify object. set to NULL for cross-sectional networks.
#' @param dyad_vars character vector of column names from dyad_data to add as
#'   dyadic variables. if NULL (default), all columns except actor1, actor2,
#'   and time will be added.
#' @param dyad_vars_symmetric logical vector indicating whether each dyadic
#'   variable represents symmetric relationships. must have the same length as
#'   dyad_vars. if NULL, defaults to the symmetry setting of the netify object,
#'   but a warning will be issued recommending explicit specification.
#' @param replace_existing logical scalar. if TRUE, existing dyadic variables with the
#'   same names will be replaced. if FALSE (default), attempting to add variables
#'   that already exist will result in an error.
#'
#' @return a netify object (class "netify") with the additional dyadic variables stored in the
#'   'dyad_data' attribute. the structure is a nested list where:
#'   \itemize{
#'     \item first level: named list with time periods as names (or "1" for cross-sectional data)
#'     \item second level: named list with variable names as names
#'     \item values: matrix objects with actors as rows/columns and numeric, integer,
#'       logical, or character values
#'   }
#'
#' @details
#' dyadic variables are stored as matrix objects where rows represent the first actor
#' (sender in directed networks) and columns represent the second actor (receiver
#' in directed networks). for symmetric variables in undirected networks, the
#'   function ensures that \code{matrix[i,j]} equals \code{matrix[j,i]}.
#'
#' the function optimizes storage by automatically detecting the data type of
#' each variable and using the appropriate matrix storage mode:
#' \itemize{
#'   \item logical vectors -> logical matrices
#'   \item integer vectors -> integer matrices
#'   \item numeric vectors with only integer values -> integer matrices
#'   \item numeric vectors with decimals -> double matrices
#'   \item character vectors -> character matrices
#' }
#'
#' for longitudinal networks, the function handles time-varying actor sets
#' appropriately, creating matrices that include only actors present at each
#' time point.
#'
#' missing dyadic observations (na values) in the input data.frame will be set to missing
#' in the resulting matrices as well.
#'
#' @note
#' the input `dyad_data` must be a `data.frame` or an object that can be coerced into a `data.frame`
#' (e.g., a `tibble` or `data.table`). inputs such as matrices or arrays are not supported.
#'
#' when adding dyadic variables to bipartite networks, all variables are
#' automatically treated as asymmetric regardless of the dyad_vars_symmetric
#' specification.
#'
#' for large networks, consider the memory implications of adding many dyadic
#' variables, as each variable requires a full adjacency matrix for storage.
#'
#' @examples
#' # load example data
#' data(icews)
#'
#' # cross-sectional example
#' icews_10 <- icews[icews$year == 2010, ]
#' actors <- sort(unique(c(icews_10$i, icews_10$j)))[1:35]
#' icews_10 <- icews_10[icews_10$i %in% actors & icews_10$j %in% actors, ]
#'
#' # create initial netify object with just the main weight
#' verbCoop_net <- netify(
#'     icews_10, # data.frame input
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # check class
#' class(verbCoop_net) # "netify"
#'
#' # add additional dyadic variables
#' verbCoop_net <- add_dyad_vars(
#'     netlet = verbCoop_net, # netify object
#'     dyad_data = icews_10, # data.frame with variables to add
#'     actor1 = "i", actor2 = "j",
#'     dyad_vars = c("matlCoop", "verbConf", "matlConf"),
#'     dyad_vars_symmetric = rep(FALSE, 3)
#' )
#'
#' # access the dyadic data structure (returns list)
#' dyad_data_structure <- attr(verbCoop_net, "dyad_data")
#' class(dyad_data_structure) # "list"
#' names(dyad_data_structure) # time periods
#' names(dyad_data_structure[["1"]]) # variables at time 1
#'
#' # access specific variable matrix
#' matlCoop_matrix <- dyad_data_structure[["1"]][["matlCoop"]]
#' class(matlCoop_matrix) # "matrix" "array"
#' dim(matlCoop_matrix)
#' matlCoop_matrix[1:5, 1:5] # view subset
#'
#' # longitudinal example
#' icews_panel <- icews[
#'     icews$year %in% 2002:2004 &
#'         icews$i %in% actors &
#'         icews$j %in% actors,
#' ]
#'
#' verbCoop_longit_net <- netify(
#'     icews_panel, # data.frame input
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # add dyadic variables across all time periods
#' verbCoop_longit_net <- add_dyad_vars(
#'     netlet = verbCoop_longit_net, # netify object
#'     dyad_data = icews_panel, # data.frame with longitudinal data
#'     actor1 = "i", actor2 = "j", time = "year",
#'     dyad_vars = c("matlCoop", "verbConf", "matlConf"),
#'     dyad_vars_symmetric = rep(FALSE, 3)
#' )
#'
#' # access data for specific year (returns list)
#' year_2002_data <- attr(verbCoop_longit_net, "dyad_data")[["2002"]]
#' class(year_2002_data) # "list"
#' names(year_2002_data) # available variables
#'
#' # each variable is stored as a matrix
#' matlCoop_2002 <- year_2002_data[["matlCoop"]]
#' class(matlCoop_2002) # "matrix" "array"
#'
#' # example: add variables from a different source
#' \donttest{
#' # create a new data.frame with trade information
#' trade_data <- data.frame(
#'     i = icews_10$i,
#'     j = icews_10$j,
#'     trade_volume = runif(nrow(icews_10), 0, 1000),
#'     trade_balance = rnorm(nrow(icews_10))
#' )
#' class(trade_data) # "data.frame"
#'
#' verbCoop_net <- add_dyad_vars(
#'     netlet = verbCoop_net,
#'     dyad_data = trade_data,
#'     actor1 = "i", actor2 = "j",
#'     dyad_vars = c("trade_volume", "trade_balance"),
#'     dyad_vars_symmetric = c(FALSE, FALSE)
#' )
#' }
#'
#' @author cassy dorff, colin henry, shahryar minhas
#'
#' @export add_dyad_vars
#' @aliases add_edge_attributes

add_dyad_vars <- function(
	netlet, dyad_data,
	actor1 = NULL, actor2 = NULL, time = NULL,
	dyad_vars = NULL, dyad_vars_symmetric = NULL,
	replace_existing = FALSE) {
	# user input checks
	netify_check(netlet)
	dyad_data <- df_check(dyad_data)
	actor_check(actor1, actor2, dyad_data)
	add_var_time_check(netlet, time)

	# pull attribute info
	netlet_type <- attr(netlet, "netify_type")
	netlet_mode <- attr(netlet, "mode")
	actor_unif <- attr(netlet, "actor_time_uniform")

	# default to all variables if none specified
	if (is.null(dyad_vars)) {
		dyad_vars <- setdiff(names(dyad_data), c(actor1, actor2, time))
	}
	missing_dyad_vars <- setdiff(dyad_vars, names(dyad_data))
	if (length(missing_dyad_vars) > 0L) {
		cli::cli_abort(
			"{.arg dyad_vars} column{?s} not found in {.arg dyad_data}: {.val {missing_dyad_vars}}."
		)
	}
	if (length(dyad_vars) == 0L) {
		return(netlet)
	}

	# default symmetry from netlet and warn user to be explicit
	if (is.null(dyad_vars_symmetric) & netlet_mode != "bipartite") {
		dyad_vars_symmetric <- rep(all(attr(netlet, "symmetric")), length(dyad_vars))
		cli::cli_alert_warning(
			"Warning: When adding dyadic variables it is best to specify whether the variable being added is symmetric or not"
		)
	}
	if (netlet_mode == "bipartite") {
		dyad_vars_symmetric <- rep(FALSE, length(dyad_vars))
	}
	if (!is.logical(dyad_vars_symmetric) ||
		length(dyad_vars_symmetric) != length(dyad_vars) ||
		anyNA(dyad_vars_symmetric)) {
		cli::cli_abort(
			"{.arg dyad_vars_symmetric} must be a logical vector with one value per {.arg dyad_vars} entry."
		)
	}

	key_cols <- c(actor1, actor2, time)
	dyad_data <- collapse_duplicate_dyad_keys(dyad_data, key_cols, dyad_vars, time)

	nd_vars <- length(dyad_vars)

	# check for existing dyad_data attribute
	dyad_data_0 <- attributes(netlet)$dyad_data
	dyad_data_attrib_exists <- !is.null(dyad_data_0)

	# drop variables that will be replaced
	if (dyad_data_attrib_exists & replace_existing) {
		for (time_period in names(dyad_data_0)) {
			for (var_name in dyad_vars) {
				dyad_data_0[[time_period]][[var_name]] <- NULL
			}
		}
	}

	msrmnts <- netify_measurements(netlet)

	# placeholder time for cross-sectional networks
	if (is.null(msrmnts$time)) {
		msrmnts$time <- 1
	}

	# pre-split dyad_data by time
	if (!is.null(time) & attributes(netlet)$netify_type != "cross_sec") {
		time_indices <- split(seq_len(nrow(dyad_data)), dyad_data[, time])
		dyad_actor1 <- dyad_data[, actor1]
		dyad_actor2 <- dyad_data[, actor2]
	}

	# build list of time periods -> list of variable matrices
	dyad_structure <- lapply(msrmnts$time, function(timePd) {
		# actors in the matrices at this time point
		if (netlet_type == "longit_list") {
			actors_rows <- msrmnts$row_actors[[timePd]]
			actors_cols <- msrmnts$col_actors[[timePd]]
			n_actors_rows <- msrmnts$n_row_actors[[timePd]]
			n_actors_cols <- msrmnts$n_col_actors[[timePd]]
		}
		if (netlet_type %in% c("longit_array", "cross_sec")) {
			actors_rows <- msrmnts$row_actors
			actors_cols <- msrmnts$col_actors
			n_actors_rows <- msrmnts$n_row_actors
			n_actors_cols <- msrmnts$n_col_actors
		}

		# slice dyad_data for this time period
		if (!is.null(time) & attributes(netlet)$netify_type != "cross_sec") {
			slice_indices <- time_indices[[as.character(timePd)]]
			if (is.null(slice_indices)) slice_indices <- integer(0)

			if (length(slice_indices) > 0) {
				slice_actor1 <- dyad_actor1[slice_indices]
				slice_actor2 <- dyad_actor2[slice_indices]
				slice_data <- dyad_data[slice_indices, dyad_vars, drop = FALSE]
			} else {
				slice_actor1 <- character(0)
				slice_actor2 <- character(0)
				slice_data <- dyad_data[integer(0), dyad_vars, drop = FALSE]
			}
		} else {
			slice_actor1 <- dyad_data[, actor1]
			slice_actor2 <- dyad_data[, actor2]
			slice_data <- dyad_data[, dyad_vars, drop = FALSE]
		}

		# keep only rows present in the netlet
		valid_rows <- slice_actor1 %in% actors_rows & slice_actor2 %in% actors_cols
		slice_actor1 <- slice_actor1[valid_rows]
		slice_actor2 <- slice_actor2[valid_rows]
		slice_data <- slice_data[valid_rows, , drop = FALSE]

		# pre-compute matrix indices
		if (length(slice_actor1) > 0) {
			mat_row_indices <- match(slice_actor1, actors_rows)
			mat_col_indices <- match(slice_actor2, actors_cols)
		} else {
			mat_row_indices <- integer(0)
			mat_col_indices <- integer(0)
		}

		# build matrix for each dyadic variable
		var_matrices <- vector("list", nd_vars)
		names(var_matrices) <- dyad_vars

		for (ii in 1:nd_vars) {
			var_name <- dyad_vars[ii]

			# pick storage mode based on value types
			var_values <- if (length(slice_actor1) > 0) slice_data[, var_name] else numeric(0)
			storage_mode <- determine_storage_mode(var_values)

			# dispatch to the matching c++ function
			if (storage_mode == "double") {
				var_matrices[[var_name]] <- get_matrix(
					n_rows = n_actors_rows,
					n_cols = n_actors_cols,
					actors_rows = actors_rows,
					actors_cols = actors_cols,
					matRowIndices = mat_row_indices,
					matColIndices = mat_col_indices,
					value = var_values,
					symmetric = dyad_vars_symmetric[ii],
					missing_to_zero = TRUE,
					diag_to_NA = FALSE
				)
			} else if (storage_mode == "integer") {
				var_matrices[[var_name]] <- get_matrix_integer(
					n_rows = n_actors_rows,
					n_cols = n_actors_cols,
					actors_rows = actors_rows,
					actors_cols = actors_cols,
					matRowIndices = mat_row_indices,
					matColIndices = mat_col_indices,
					value = as.integer(var_values),
					symmetric = dyad_vars_symmetric[ii],
					missing_to_zero = TRUE,
					diag_to_NA = FALSE
				)
			} else if (storage_mode == "logical") {
				var_matrices[[var_name]] <- get_matrix_logical(
					n_rows = n_actors_rows,
					n_cols = n_actors_cols,
					actors_rows = actors_rows,
					actors_cols = actors_cols,
					matRowIndices = mat_row_indices,
					matColIndices = mat_col_indices,
					value = as.logical(var_values),
					symmetric = dyad_vars_symmetric[ii],
					missing_to_zero = TRUE,
					diag_to_NA = FALSE
				)
			} else if (storage_mode == "character") {
				var_matrices[[var_name]] <- get_matrix_character(
					n_rows = n_actors_rows,
					n_cols = n_actors_cols,
					actors_rows = actors_rows,
					actors_cols = actors_cols,
					matRowIndices = mat_row_indices,
					matColIndices = mat_col_indices,
					value = as.character(var_values),
					symmetric = dyad_vars_symmetric[ii],
					missing_to_zero = TRUE,
					diag_to_NA = FALSE
				)
			} else {
				# numeric fallback for unknown types
				var_matrices[[var_name]] <- get_matrix(
					n_rows = n_actors_rows,
					n_cols = n_actors_cols,
					actors_rows = actors_rows,
					actors_cols = actors_cols,
					matRowIndices = mat_row_indices,
					matColIndices = mat_col_indices,
					value = as.numeric(var_values),
					symmetric = dyad_vars_symmetric[ii],
					missing_to_zero = TRUE,
					diag_to_NA = FALSE
				)
			}
		}

		# merge with any existing dyadic variables
		if (dyad_data_attrib_exists) {
			existing_vars <- dyad_data_0[[as.character(timePd)]]
			if (!is.null(existing_vars)) {
				if (replace_existing) {
					existing_vars <- existing_vars[!names(existing_vars) %in% names(var_matrices)]
				} else {
					overlap <- intersect(names(existing_vars), names(var_matrices))
					if (length(overlap) > 0) {
						var_matrices <- var_matrices[!names(var_matrices) %in% overlap]
					}
				}
				var_matrices <- c(existing_vars, var_matrices)
			}
		}

		return(var_matrices)
	})

	names(dyad_structure) <- as.character(msrmnts$time)

	attr(netlet, "dyad_data") <- dyad_structure

	return(netlet)
}

#' @rdname add_dyad_vars
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
add_edge_attributes <- add_dyad_vars

collapse_duplicate_dyad_keys <- function(dyad_data, key_cols, dyad_vars, time = NULL) {
	dup_key <- duplicated(dyad_data[, key_cols, drop = FALSE]) |
		duplicated(dyad_data[, key_cols, drop = FALSE], fromLast = TRUE)
	if (!any(dup_key)) {
		return(dyad_data)
	}

	key_id <- do.call(
		paste,
		c(dyad_data[, key_cols, drop = FALSE], sep = "\r")
	)
	groups <- split(seq_len(nrow(dyad_data)), key_id)

	collapse_one <- function(vals, var_name, key_rows) {
		if (is.factor(vals)) {
			vals <- as.character(vals)
		}
		if (length(vals) == 1L) {
			return(vals)
		}
		if (all(is.na(vals))) {
			return(vals[1])
		}
		if (is.numeric(vals) || is.integer(vals)) {
			return(mean(vals, na.rm = TRUE))
		}
		unique_vals <- unique(vals[!is.na(vals)])
		if (length(unique_vals) <= 1L) {
			return(if (length(unique_vals) == 0L) vals[1] else unique_vals[1])
		}
		dup_vals <- utils::head(unique(do.call(
			paste,
			c(dyad_data[key_rows, key_cols, drop = FALSE], sep = " / ")
		)), 5)
		cli::cli_abort(c(
			"x" = "{.arg dyad_data} has conflicting non-numeric values within a dyad{if (!is.null(time)) '-time' else ''} key.",
			"i" = "Variable: {.val {var_name}}.",
			"i" = "Conflicting key{?s}: {.val {dup_vals}}.",
			"i" = "Pre-aggregate or clean {.arg dyad_data} before calling {.fn add_dyad_vars}."
		))
	}

	first_idx <- vapply(groups, `[`, integer(1), 1L)
	out <- dyad_data[first_idx, key_cols, drop = FALSE]
	for (var in dyad_vars) {
		out[[var]] <- unlist(lapply(groups, function(idx) {
			collapse_one(dyad_data[[var]][idx], var, idx)
		}), use.names = FALSE)
	}

	cli::cli_inform(
		c(
			"i" = "{.fn add_dyad_vars} collapsed repeated dyad{if (!is.null(time)) '-time' else ''} rows before attaching dyadic variables.",
			"*" = "Numeric dyadic variables were averaged within repeated keys; non-numeric variables must agree."
		),
		.frequency = "once",
		.frequency_id = "add_dyad_vars_collapsed_duplicate_keys"
	)

	out
}

#' determine optimal storage mode for matrix values
#'
#' this internal function examines a vector of values and determines the most
#' efficient storage mode for creating matrices. it helps optimize memory usage
#' by selecting the appropriate data type-specific matrix creation function.
#'
#' @param values a vector of values that will be stored in a matrix. can be
#'   logical, character, integer, or numeric.
#'
#' @return a character string indicating the optimal storage mode:
#'   \itemize{
#'     \item \code{"logical"} for logical vectors
#'     \item \code{"character"} for character vectors
#'     \item \code{"integer"} for integer vectors or numeric vectors containing only integer values
#'     \item \code{"double"} for numeric vectors with decimal values or as default fallback
#'   }
#'
#' @details
#' the function performs type checking in a specific order to determine the most
#' appropriate storage mode. for numeric values, it additionally checks whether
#' all values are integers (even if stored as doubles) to potentially use more
#' memory-efficient integer storage. empty vectors default to "double" storage.
#'
#' this optimization is particularly important for large networks where using
#' the correct storage type can significantly reduce memory usage.
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd

determine_storage_mode <- function(values) {
	if (length(values) == 0) {
		return("double")
	}

	if (is.logical(values)) {
		return("logical")
	}
	if (is.factor(values)) {
		return("character")
	}
	if (is.character(values)) {
		return("character")
	}
	if (is.integer(values)) {
		return("integer")
	}
	if (is.numeric(values)) {
		# finite whole-number doubles in integer range can use integer storage
		non_missing <- values[!is.na(values)]
		integerish <- length(non_missing) > 0L &&
			all(is.finite(non_missing)) &&
			all(non_missing >= -.Machine$integer.max - 1) &&
			all(non_missing <= .Machine$integer.max) &&
			all(non_missing == floor(non_missing))
		if (integerish) {
			return("integer")
		} else {
			return("double")
		}
	}

	return("double")
}
