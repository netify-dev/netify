# user input checks for package functions

#' df_check
#'
#' checks to make sure a data.frame is inputted and
#' if a `tibble` or `data.table` data.frame is inputted then it is
#' converted to a base r data.frame object
#' @param df user inputted object to check
#' @param msg msg to user if df check fails
#' @return data.frame object
#' @author ha eun choi, shahryar minhas
#'
#' @keywords internal
#' @noRd

df_check <- function(
	df,
	msg = "Error: check data type. `dyad_data` is not a dataframe.") {
	if (!inherits(df, "data.frame")) {
		cli::cli_abort(msg)
	}

	# coerce tibble/data.table to base data.frame for base subsetting
	df <- as.data.frame(df, stringsAsFactors = FALSE)

	if (nrow(df) == 0) {
		cli::cli_abort(
			c(
				"x" = "Cannot create network from empty dataset.",
				"i" = "dyad_data has {nrow(df)} rows.",
				"!" = "Please provide a dataset with at least one dyadic observation."
			)
		)
	}

	return(df)
}

#' logical_check
#'
#' checks to make sure user has correctly inputted logicals
#' for select inputs
#' @param sum_dyads user supplied input
#' @param symmetric user supplied input
#' @param diag_to_NA user supplied input
#' @param missing_to_zero user supplied input
#' @param actor_time_uniform optional user supplied input
#' @return returns a NULL object but stops the process
#' if there is an error detected
#' @author ha eun choi, shahryar minhas
#'
#' @keywords internal
#' @noRd

logical_check <- function(
	sum_dyads, symmetric,
	diag_to_NA, missing_to_zero,
	actor_time_uniform = NULL) {
	assert_flag_arg <- function(x, arg) {
		if (!is.logical(x) || length(x) != 1L || is.na(x)) {
			cli::cli_abort("{.arg {arg}} must be TRUE or FALSE.")
		}
	}

	assert_flag_arg(sum_dyads, "sum_dyads")
	assert_flag_arg(symmetric, "symmetric")
	assert_flag_arg(diag_to_NA, "diag_to_NA")
	assert_flag_arg(missing_to_zero, "missing_to_zero")

	if (!is.null(actor_time_uniform)) {
		assert_flag_arg(actor_time_uniform, "actor_time_uniform")
	}

	return(invisible(NULL))
}

#' actor_check
#'
#' checks to make sure that the actor fields
#' are populated and that they do not contain nas
#' or non-character values
#' @param actor1 user inputted object denoting
#' actor1 variable in data.frame
#' @param actor2 user inputted object denoting
#' actor2 variable in data.frame
#' @param dyad_data data.frame in which actor1 and actor2
#' values are located
#' @return returns a NULL object but stops the process
#' if there is an error detected
#' @author ha eun choi, shahryar minhas
#'
#' @keywords internal
#' @noRd

actor_check <- function(actor1, actor2, dyad_data) {
	cols_available <- colnames(dyad_data)

	if (is.null(actor1) | is.null(actor2)) {
		# suggest first two columns as a working example
		example_hint <- if (length(cols_available) >= 2) {
			sprintf(
				"Try: netify(dyad_data, actor1 = \"%s\", actor2 = \"%s\")",
				cols_available[1], cols_available[2]
			)
		} else {
			NA_character_
		}
		msg <- c(
			"x" = "{.arg actor1} or {.arg actor2} values are required.",
			"i" = "Available columns in {.arg dyad_data}: {.val {cols_available}}"
		)
		if (!is.na(example_hint)) {
			msg <- c(msg, "i" = example_hint)
		}
		cli::cli_abort(msg)
	}

	if (!actor1 %in% cols_available | !actor2 %in% cols_available) {
		missing_cols <- c(
			if (!actor1 %in% cols_available) actor1 else NULL,
			if (!actor2 %in% cols_available) actor2 else NULL
		)
		# suggest closest matches via edit distance / substring
		hint_lines <- vapply(missing_cols, function(mc) {
			contains_idx <- grep(mc, cols_available, fixed = TRUE)
			if (length(contains_idx) >= 1L) {
				return(sprintf(
					"Did you mean '%s' instead of '%s'?",
					cols_available[contains_idx[1]], mc
				))
			}
			d <- utils::adist(mc, cols_available)[1, ]
			best <- which.min(d)
			thresh <- max(1L, min(nchar(mc) %/% 3L, nchar(cols_available[best]) %/% 3L))
			if (length(best) && d[best] <= thresh) {
				sprintf("Did you mean '%s' instead of '%s'?", cols_available[best], mc)
			} else {
				sprintf("Column '%s' not found in {.arg dyad_data}.", mc)
			}
		}, character(1))
		msg <- c(
			"x" = "{.arg actor1} and/or {.arg actor2} variables do not exist in the {.arg dyad_data} object.",
			"i" = "Available columns: {.val {cols_available}}"
		)
		msg <- c(msg, stats::setNames(hint_lines, rep("i", length(hint_lines))))
		cli::cli_abort(msg)
	}

	if (any(is.na(dyad_data[, actor1])) | any(is.na(dyad_data[, actor2]))) {
		cli::cli_abort("{.arg actor1} and/or {.arg actor2} contains missing value(s).")
	}

	return(invisible(NULL))
}

#' weight_check
#'
#' checks to make sure that the weight field
#' is populated correctly
#' @param weight user inputted object for weight
#' and NULL by default
#' @param dyad_data data.frame in which weight
#' values are located
#' @return returns a NULL object but stops the process
#' if there is an error detected
#' @author ha eun choi, shahryar minhas
#'
#' @keywords internal
#' @noRd

weight_check <- function(weight, dyad_data) {
	if (!is.null(weight) & !is.character(weight)) {
		cli::cli_abort("check data type. {.arg weight} should be left NULL or a character value referring to a variable from the dyad_data object should be provided.")
	}

	if (!is.null(weight)) {
		if (!weight %in% colnames(dyad_data)) {
			cli::cli_abort("{.arg weight} variable does not exist in the {.arg dyad_data} object.")
		}
	}

	# reject classes that would silently coerce to nonsensical numerics
	if (!is.null(weight)) {
		w_col <- dyad_data[, weight]
		bad_class <- NULL
		if (is.factor(w_col)) bad_class <- "factor"
		else if (inherits(w_col, c("Date", "POSIXct", "POSIXlt", "difftime"))) bad_class <- class(w_col)[1]
		else if (is.character(w_col)) bad_class <- "character"
		else if (!(is.numeric(w_col) || is.logical(w_col))) bad_class <- class(w_col)[1]
		if (!is.null(bad_class)) {
			cli::cli_abort(c(
				"{.arg weight} column {.val {weight}} is of class {.cls {bad_class}}; netify expects a numeric or logical weight.",
				"i" = "Convert it explicitly before calling netify(), e.g. {.code dyad_data${weight} <- as.numeric(dyad_data${weight})}, so the resulting weights are intentional rather than coerced silently."
			))
		}

		# warn once about infinite weights
		if (is.numeric(w_col) && any(is.infinite(w_col))) {
			n_inf <- sum(is.infinite(w_col))
			cli::cli_inform(c(
				"!" = "{.arg weight} column {.val {weight}} contains {n_inf} infinite value{?s}; summary statistics that touch these cells will return Inf / NaN.",
				"i" = "Replace with a large finite cap or NA before calling {.fn netify} if you want finite summaries."
			),
				.frequency = "once",
				.frequency_id = paste0("netify_inf_weight_", weight)
			)
		}

		# warn once about nan weights
		if (is.numeric(w_col) && any(is.nan(w_col))) {
			n_nan <- sum(is.nan(w_col))
			cli::cli_inform(c(
				"!" = "{.arg weight} column {.val {weight}} contains {n_nan} NaN value{?s}; these will be treated as missing (NA).",
				"i" = "Replace NaNs with explicit NA or a finite value before calling {.fn netify} if a different treatment is desired."
			),
				.frequency = "once",
				.frequency_id = paste0("netify_nan_weight_", weight)
			)
		}
	}

	return(invisible(NULL))
}


#' weight_string_label
#'
#' create attribute label of for weight
#' based on user inputs to netify
#'
#' @param weight user input for weight
#' @param sum_dyads logical user input for sum_dyads
#' @return character string
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd

weight_string_label <- function(weight, sum_dyads) {
	# binary ties
	if (is.null(weight) & !sum_dyads) {
		weight_string <- "Binary Weights"
	}

	# summed binary ties
	if (is.null(weight) & sum_dyads) {
		weight_string <- "Sum of Binary Weights"
	}

	# observed weights
	if (!is.null(weight) & !sum_dyads) {
		weight_string <- paste0(
			"Weights from `", weight, "`"
		)
	}

	# summed observed weights
	if (!is.null(weight) & sum_dyads) {
		weight_string <- paste0(
			"Sum of Weights from `", weight, "`"
		)
	}

	#
	return(weight_string)
}

#' time_check
#'
#' checks to make sure that the time field
#' is populated correctly
#' @param time user inputted object for weight
#' and NULL by default
#' @param dyad_data data.frame in which time
#' values are located
#' @return returns a NULL object but stops the process
#' if there is an error detected
#' @author ha eun choi, shahryar minhas
#'
#' @keywords internal
#' @noRd

time_check <- function(time, dyad_data) {
	# check data type
	if (!is.character(time)) {
		cli::cli_abort("check data type. {.arg time} should be left NULL or a character value referring to a variable from the dyad_data object should be provided.")
	}

	# check to make sure that time variable exists in data
	if (!time %in% colnames(dyad_data)) {
		cli::cli_abort("{.arg time} variable does not exist in the {.arg dyad_data} object.")
	}

	#
	time_data <- dyad_data[[time]]

	# supported time classes
	if (!inherits(time_data, c("numeric", "integer", "Date", "POSIXct", "POSIXlt", "character"))) {
		cli::cli_abort(
			"Time variable must be numeric, Date, POSIXct, POSIXlt, or character. Found class: {class(time_data)[1]}"
		)
	}
	if (anyNA(time_data)) {
		cli::cli_abort("{.arg time} variable {.val {time}} contains missing values. Remove or impute missing time values before creating a longitudinal netify object.")
	}
	if (is.numeric(time_data) && any(!is.finite(time_data))) {
		cli::cli_abort("{.arg time} variable {.val {time}} must contain finite numeric values.")
	}

	# if charac, check if it can be reasonably sorted
	if (is.character(time_data)) {
		#
		sample_vals <- utils::head(unique(time_data), 10)
		cli::cli_alert_info(
			"Time variable is character. Will sort alphabetically. Sample values: {paste(sample_vals, collapse=', ')}"
		)
	}

	#
	return(invisible(NULL))
}

# repeat_dyads_check
#
#' check whether dyadic observations are repeating in the data.frame object
#'
#' this function checks for repeating dyadic observations in a data.frame,
#' possibly considering a time dimension. it uses c++ code for speed.
#'
#' @param dyad_data a data.frame containing the dyadic data.
#' @param actor1 character string specifying the column name for actor1.
#' @param actor2 character string specifying the column name for actor2.
#' @param time optional character string specifying the column name for time.
#'             if not provided, dyads are considered without regard to time.
#' @return an integer count of the number of repeating dyads in the data.frame.
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd

repeat_dyads_check <- function(dyad_data, actor1, actor2, time = NULL) {
	# clean up params
	if (!is.null(time)) {
		time_vec <- dyad_data[[time]]
	} else {
		time_vec <- rep(1, nrow(dyad_data)) # default time vector if none provided
	}

	#
	actor1Vec <- dyad_data[[actor1]]
	actor2Vec <- dyad_data[[actor2]]

	#
	out <- count_duplicate_dyads(actor1Vec, actor2Vec, time_vec)

	#
	return(out)
}


#' edge_value_check
#'
#' warns user about how edge values in adjacency matrices will be determined
#' @param weight user inputted weight value
#' @param sum_dyads user inputted sum_dyads logical
#' @param time logical indicating whether inputted data is longitudinal
#' @return returns a NULL object but provides warnings to users
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

edge_value_check <- function(weight, sum_dyads, time = FALSE) {
	# gen warning starter
	if (!time) {
		rep_wrn <- "Warning: there are repeating dyads in the dataset. "
	}
	if (time) {
		rep_wrn <- "Warning: there are repeating dyads within time periods in the dataset. "
	}

	# repeating dyads with no weight
	# and sum_dyads set to false
	# example use case for us: event data where we want to produce binary edges indicating whether a pair of actors ever interacted
	if (is.null(weight) & !sum_dyads) {
		cli::cli_alert_warning(
			paste0(rep_wrn, "When `weight` is not supplied and `sum_dyads` is set to FALSE, edges in the outputted adjacency matrix will represent binary interactions between actors.")
		)
	}

	# repeating dyads with weight variable not supplied and sum dyads set to true
	# example use case for us: event data where we want to produce edges indicating the total number of interactions between actors
	if (is.null(weight) & sum_dyads) {
		cli::cli_alert_warning(
			paste0(rep_wrn, "When `sum_dyads = TRUE` and `weight` is not supplied, edges in the outputted adjacency matrix represent a count of interactions between actors.")
		)
	}

	# handle repeating dyads with a weight column: auto-promote sum_dyads
	# so the matrix is a deterministic sum of the repeated weights
	auto_promote <- FALSE
	if (!is.null(weight) & !sum_dyads) {
		cli::cli_alert_danger(
			paste0(rep_wrn, "When `sum_dyads = FALSE` and `weight` is supplied, edges cannot be uniquely identified.")
		)
		cli::cli_inform(c(
			"i" = "Auto-promoting to {.code sum_dyads = TRUE} so repeated weights are summed deterministically.",
			"i" = "Set {.code sum_dyads = TRUE} explicitly, or pre-aggregate with {.fn aggregate_dyad}, to silence this message."
		))
		auto_promote <- TRUE
	}

	#
	return(invisible(list(sum_dyads = auto_promote || sum_dyads,
		auto_promote = auto_promote)))
}

#' add_var_time_check
#'
#' stops the process if the user tries to add time to a
#' non-longitudinal dataset and vice versa
#'
#' @param netlet user inputted netlet object
#' @param time user inputted time variable
#' @return returns a NULL object but stops the process
#' if there is an error detected
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

add_var_time_check <- function(netlet, time) {
	# stop process if time variable supplied and netify type is cross-sec
	if (attributes(netlet)$netify_type == "cross_sec" & !is.null(time)) {
		cli::cli_abort("{.arg time} variable should be left to NULL, if netlet is of object type {.val cross_sec}.")
	}

	# stop process if time variable not supplied and netify type is longit
	if (attributes(netlet)$netify_type != "cross_sec" & is.null(time)) {
		cli::cli_abort("{.arg time} variable should be supplied, if netlet is of object type {.val longit_array} or {.val longit_list}.")
	}
}


#' validate actor data for network creation
#'
#' this function validates that actor columns contain valid data and
#' performs mode-specific checks (e.g., bipartite actor distinctness)
#'
#' @param dyad_data a data frame containing dyadic data
#' @param actor1 character: name of the actor 1 variable in the data
#' @param actor2 character: name of the actor 2 variable in the data
#' @param mode character: whether the network is unipartite or bipartite
#'
#' @return invisible(TRUE) if validation passes, otherwise throws error
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

actor_mode_check <- function(dyad_data, actor1, actor2, mode) {
	# extract unique actors from each column
	actors_1 <- unique(dyad_data[, actor1])
	actors_2 <- unique(dyad_data[, actor2])

	# check for empty actor sets
	if (length(actors_1) == 0) {
		cli::cli_abort(
			c(
				"x" = "No valid actors found in actor1 column '{actor1}'.",
				"!" = "Please check your data and column specification."
			)
		)
	}

	if (length(actors_2) == 0) {
		cli::cli_abort(
			c(
				"x" = "No valid actors found in actor2 column '{actor2}'.",
				"!" = "Please check your data and column specification."
			)
		)
	}

	# mode-specific validation
	if (mode == "bipartite") {
		bipartite_actor_check(actors_1, actors_2)
	} else if (mode == "unipartite") {
		unipartite_actor_check(actors_1, actors_2)
	} else {
		cli::cli_abort(
			c(
				"x" = "Unknown mode '{mode}'.",
				"!" = "Mode must be either 'unipartite' or 'bipartite'."
			)
		)
	}

	invisible(TRUE)
}

#' validate bipartite network actor requirements
#'
#' @param actors_1 character vector of unique actor1 values
#' @param actors_2 character vector of unique actor2 values
#'
#' @return invisible(TRUE) if validation passes, otherwise throws error or warning
#'
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

bipartite_actor_check <- function(actors_1, actors_2) {
	# check for overlapping actors (warning, not error)
	overlap <- intersect(actors_1, actors_2)
	if (length(overlap) > 0) {
		cli::cli_alert_warning(
			c(
				"!" = "Mode specified as 'bipartite' but {length(overlap)} actor{?s} appear{?s/} in both actor1 and actor2:",
				"i" = "Overlapping actors: {.val {head(overlap, 5)}}{if(length(overlap) > 5) ' ...'}"
			)
		)
	}

	# check for sufficient actors in each mode (error)
	if (length(actors_1) < 1 || length(actors_2) < 1) {
		cli::cli_abort(
			c(
				"x" = "Bipartite networks require actors in both modes.",
				"i" = "Found {length(actors_1)} unique actor1 value{?s} and {length(actors_2)} unique actor2 value{?s}."
			)
		)
	}

	invisible(TRUE)
}

#' validate unipartite network actor requirements
#'
#' @param actors_1 character vector of unique actor1 values
#' @param actors_2 character vector of unique actor2 values
#'
#' @return invisible(TRUE) if validation passes, otherwise throws error or warning
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

unipartite_actor_check <- function(actors_1, actors_2) {
	# for unipartite networks, we might want to check other things
	# require at least one actor
	all_actors <- unique(c(actors_1, actors_2))

	if (length(all_actors) < 4) {
		cli::cli_alert_warning(
			c(
				"!" = "Unipartite network has only {length(all_actors)} unique actor{?s}.",
				"i" = " Networks with fewer than 4 actors may not be meaningful."
			)
		)
	}

	invisible(TRUE)
}
