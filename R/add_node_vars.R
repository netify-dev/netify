#' Add nodal variables to a netify object
#'
#' `add_node_vars` (also available as `add_vertex_attributes`)
#' merges nodal (vertex-level) variables from a data.frame into an
#' existing netify object. this function allows you to incrementally build up
#' the nodal attributes of your network after initial creation, which is useful
#' when actor-level variables come from different sources or need different
#' preprocessing.
#'
#' @param netlet a netify object (class "netify") to which nodal variables will be added.
#' @param node_data a data.frame object containing the nodal variables to add. for
#'   cross-sectional networks, must have one row per unique actor. for longitudinal
#'   networks, must have one row per actor-time combination. will be coerced to
#'   data.frame if a tibble or data.table is provided.
#' @param actor character string specifying the column name in node_data that uniquely
#'   identifies each actor. this should contain the same actor identifiers used in
#'   the original netify object.
#' @param time character string specifying the column name in node_data for time periods.
#'   required for longitudinal netify objects. should match the time specification
#'   used when creating the netify object. set to NULL for cross-sectional networks.
#' @param node_vars character vector of column names from node_data to add as nodal
#'   variables. if NULL (default), all columns except actor and time will be added.
#' @param replace_existing logical scalar. if TRUE, existing nodal variables with the
#'   same names will be replaced. if FALSE (default), attempting to add variables
#'   that already exist will result in an error.
#'
#' @return a netify object (class "netify") with the additional nodal variables stored in the
#'   'nodal_data' attribute as a data.frame. the structure includes:
#'   \itemize{
#'     \item \strong{actor}: column with actor identifiers
#'     \item \strong{time}: column with time periods (longitudinal only)
#'     \item \strong{nodal variables}: columns for each variable specified in node_vars
#'   }
#'
#' @details
#' nodal variables are stored as a data.frame where each row represents an actor
#' (cross-sectional) or an actor-time combination (longitudinal). this format
#' allows for efficient storage and easy manipulation of actor-level attributes.
#'
#' the function automatically handles merging based on actor identifiers, ensuring
#' that nodal attributes are properly aligned with the network structure. for
#' longitudinal networks, the function matches both actor and time dimensions.
#'
#' missing actors in the node_data will result in na values for those actors'
#' attributes in the netify object. similarly, if node_data contains actors not
#' present in the network, those rows will be ignored.
#'
#'
#' @note
#' the input `node_data` must be a `data.frame` or an object that can be coerced
#' into a `data.frame` (e.g., a `tibble` or `data.table`). inputs such as matrices
#' or arrays are not supported.
#'
#' for longitudinal networks, ensure that node_data contains entries for all
#' actor-time combinations you wish to have attributes for. missing combinations
#' will result in na values for those actors at those time points.
#'
#' when working with large networks, the nodal data storage is more memory-efficient
#' than dyadic data, as it scales linearly with the number of actors rather than
#' quadratically.
#'
#' @examples
#' # load example data
#' data(icews)
#'
#' # cross-sectional example
#' icews_10 <- icews[icews$year == 2010, ]
#'
#' # create initial netify object
#' verbCoop_net <- netify(
#'     icews_10, # data.frame input
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # prepare nodal data - one row per unique actor
#' nvars <- c("i_polity2", "i_gdp", "i_log_gdp", "i_pop", "i_log_pop")
#' nodedata <- unique(icews_10[, c("i", nvars)])
#' class(nodedata) # "data.frame"
#' nrow(nodedata) # number of unique actors
#' head(nodedata)
#'
#' # add nodal variables
#' verbCoop_net <- add_node_vars(
#'     netlet = verbCoop_net, # netify object
#'     node_data = nodedata, # data.frame with actor attributes
#'     actor = "i", # column identifying actors
#'     node_vars = nvars # variables to add
#' )
#'
#' # access nodal data (returns data.frame)
#' node_data_stored <- attr(verbCoop_net, "nodal_data")
#' class(node_data_stored) # "data.frame"
#' head(node_data_stored)
#' names(node_data_stored) # "actor" plus variable names
#'
#' \donttest{
#' # longitudinal example
#' verbCoop_longit_net <- netify(
#'     icews, # data.frame input
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # prepare longitudinal nodal data - one row per actor-time combination
#' nodedata_longit <- unique(icews[, c("i", "year", nvars)])
#'
#' # add nodal variables with time dimension
#' verbCoop_longit_net <- add_node_vars(
#'     netlet = verbCoop_longit_net, # netify object
#'     node_data = nodedata_longit, # data.frame with longitudinal data
#'     actor = "i", # column identifying actors
#'     time = "year", # column identifying time
#'     node_vars = nvars # variables to add
#' )
#' }
#'
#' \donttest{
#' # add variables from external source
#' # suppose you have additional actor data
#' external_data <- data.frame(
#'     i = unique(icews_10$i),
#'     democracy_score = runif(length(unique(icews_10$i)), 0, 10),
#'     trade_openness = runif(length(unique(icews_10$i)), 0, 100)
#' )
#'
#' verbCoop_net <- add_node_vars(
#'     netlet = verbCoop_net,
#'     node_data = external_data,
#'     actor = "i",
#'     node_vars = c("democracy_score", "trade_openness")
#' )
#' }
#'
#' @author colin henry, shahryar minhas
#'
#' @export add_node_vars
#' @aliases add_vertex_attributes

add_node_vars <- function(
	netlet, node_data, actor = NULL,
	time = NULL, node_vars = NULL,
	replace_existing = FALSE) {
	# user input checks
	netify_check(netlet)
	node_data <- df_check(node_data)
	actor_check(actor, actor, node_data)
	add_var_time_check(netlet, time)

	# default to all variables if none specified
	if (is.null(node_vars)) {
		node_vars <- setdiff(names(node_data), c(actor, time))
	}
	missing_node_vars <- setdiff(node_vars, names(node_data))
	if (length(missing_node_vars) > 0L) {
		cli::cli_abort(
			"{.arg node_vars} column{?s} not found in {.arg node_data}: {.val {missing_node_vars}}."
		)
	}

	key_cols <- c(actor, time)
	key_cols <- key_cols[!is.null(key_cols)]
	node_data <- compact_duplicate_node_keys(node_data, key_cols, node_vars, time)

	# build frame from actor_pds when no nodal data is attached yet
	if (is.null(attr(netlet, "nodal_data"))) {
		netlet_type <- attributes(netlet)$netify_type
		if (netlet_type != "cross_sec") {
			time_labels <- netify_measurements(netlet)$time
		} else {
			time_labels <- NULL
		}
		frame <- actor_pds_to_frame(attributes(netlet)$actor_pds, time_labels)
		if (is.null(time)) {
			frame <- frame[, -2, drop = FALSE]
		}
	}

	# pull existing nodal data and honour replace_existing
	if (!is.null(attr(netlet, "nodal_data"))) {
		frame <- attr(netlet, "nodal_data")
		existing_vars <- intersect(node_vars, names(frame))
		if (length(existing_vars) > 0) {
			if (replace_existing) {
				frame <- frame[, !names(frame) %in% existing_vars, drop = FALSE]
			} else {
				cli::cli_warn(
					"Variable(s) {.val {existing_vars}} already exist in nodal_data. Use {.arg replace_existing = TRUE} to overwrite."
				)
				node_vars <- setdiff(node_vars, existing_vars)
				if (length(node_vars) == 0) return(netlet)
			}
		}
	}

	# build matching ids for the merge
	if (is.null(time)) {
		node_dataID <- paste(node_data[, actor], "1", sep = "_")
		frame_dataID <- paste(frame[, "actor"], "1", sep = "_")
	}
	if (!is.null(time)) {
		node_dataID <- paste(
			node_data[, actor], node_data[, time],
			sep = "_"
		)
		frame_dataID <- paste(
			frame[, "actor"], frame[, "time"],
			sep = "_"
		)
	}

	# join selected nodal variables onto the frame
	nfcol <- ncol(frame)
	frame <- cbind(
		frame,
		node_data[
			match(frame_dataID, node_dataID),
			node_vars,
			drop = FALSE
		]
	)
	names(frame)[(nfcol + 1):ncol(frame)] <- node_vars

	rownames(frame) <- NULL
	attr(netlet, "nodal_data") <- frame

	return(netlet)
}

#' @rdname add_node_vars
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
add_vertex_attributes <- add_node_vars

compact_duplicate_node_keys <- function(node_data, key_cols, node_vars, time = NULL) {
	dup_key <- duplicated(node_data[, key_cols, drop = FALSE]) |
		duplicated(node_data[, key_cols, drop = FALSE], fromLast = TRUE)
	if (!any(dup_key)) {
		return(node_data)
	}

	key_id <- do.call(
		paste,
		c(node_data[, key_cols, drop = FALSE], sep = "\r")
	)
	groups <- split(seq_len(nrow(node_data)), key_id)
	for (idx in groups[lengths(groups) > 1L]) {
		for (var in node_vars) {
			vals <- node_data[[var]][idx]
			if (length(unique(vals)) > 1L) {
				dup_vals <- utils::head(unique(do.call(
					paste,
					c(node_data[idx, key_cols, drop = FALSE], sep = " / ")
				)), 5)
				cli::cli_abort(c(
					"x" = "{.arg node_data} has conflicting values within an actor{if (!is.null(time)) '-time' else ''} key.",
					"i" = "Conflicting key{?s}: {.val {dup_vals}}.",
					"i" = "Pre-aggregate or clean {.arg node_data} before calling {.fn add_node_vars}."
				))
			}
		}
	}

	node_data[!duplicated(node_data[, key_cols, drop = FALSE]), , drop = FALSE]
}

aggregate_node_vars_from_dyads <- function(node_data, key_cols, node_vars, time = NULL) {
	dup_key <- duplicated(node_data[, key_cols, drop = FALSE]) |
		duplicated(node_data[, key_cols, drop = FALSE], fromLast = TRUE)
	if (!any(dup_key)) {
		return(node_data)
	}

	key_id <- do.call(
		paste,
		c(node_data[, key_cols, drop = FALSE], sep = "\r")
	)
	groups <- split(seq_len(nrow(node_data)), key_id)
	first_idx <- vapply(groups, `[`, integer(1), 1L)
	out <- node_data[first_idx, key_cols, drop = FALSE]

	collapse_one <- function(vals) {
		if (is.factor(vals)) {
			vals <- as.character(vals)
		}
		if (all(is.na(vals))) {
			return(vals[1])
		}
		if (is.numeric(vals) || is.integer(vals)) {
			return(mean(vals, na.rm = TRUE))
		}
		non_missing <- vals[!is.na(vals)]
		tab <- sort(table(non_missing), decreasing = TRUE)
		names(tab)[1]
	}

	for (var in node_vars) {
		out[[var]] <- unlist(lapply(groups, function(idx) {
			collapse_one(node_data[[var]][idx])
		}), use.names = FALSE)
	}

	cli::cli_inform(
		c(
			"i" = "{.fn netify} collapsed repeated actor{if (!is.null(time)) '-time' else ''} rows before attaching nodal variables.",
			"*" = "Numeric nodal variables were averaged; non-numeric variables use the most common non-missing value within each key."
		),
		.frequency = "once",
		.frequency_id = "netify_collapsed_duplicate_node_keys"
	)

	rownames(out) <- NULL
	out
}
