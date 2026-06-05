#' Preview subsets of network data from netify objects
#'
#' `peek` provides a
#' convenient way to inspect portions of network data stored in netify objects.
#' rather than displaying entire networks (which can be overwhelming for large
#' datasets), this function allows you to examine specific actors, time periods,
#' or layers for data exploration, verification, and debugging.
#'
#' @param netlet a netify object to preview. can be cross-sectional, longitudinal
#'   (array or list format), and/or multilayer.
#'
#' @param actors character vector of actor names or numeric indices to subset.
#'   selects these actors as both senders and receivers (extracts subgraph among
#'   these actors). overridden by \code{from} and \code{to} if specified.
#'
#' @param from specifies which actors to display as senders of ties (row actors
#'   in the adjacency matrix). can be:
#'   \itemize{
#'     \item \strong{single number}: shows the first n actors (e.g., \code{from = 5}
#'       displays actors 1-5)
#'     \item \strong{numeric vector}: shows specific positions (e.g., \code{from = c(1,3,5)}
#'       displays the 1st, 3rd, and 5th actors)
#'     \item \strong{character vector}: shows named actors (e.g., \code{from = c("usa", "china")}
#'       displays these specific countries)
#'     \item \strong{NULL}: shows all row actors (default is 3)
#'   }
#'   in bipartite networks, from represents actors in the first mode.
#'
#' @param to specifies which actors to display as receivers of ties (column actors
#'   in the adjacency matrix). accepts the same input types as \code{from}. in
#'   bipartite networks, columns represent actors in the second mode. default is 3.
#'
#' @param time for longitudinal networks, specifies which time periods to display. can be:
#'   \itemize{
#'     \item \strong{single number}: shows the nth time period (e.g., \code{time = 1}
#'       shows the first time period)
#'     \item \strong{numeric vector}: shows specific time indices (e.g., \code{time = c(1,5,10)})
#'     \item \strong{character vector}: shows named time periods (e.g., \code{time = c("2002", "2006")}
#'       displays these specific years)
#'     \item \strong{NULL}: shows all time periods
#'   }
#'   default is 1 (first time period only). ignored for cross-sectional networks.
#'
#' @param layers for multilayer networks, specifies which layer(s) to display. must be
#'   a character vector matching layer names in the netify object (e.g.,
#'   \code{layers = c("trade", "alliance")}). for single-layer networks, this
#'   parameter is ignored. default is NULL (shows all layers).
#' @param drop_dimensions logical. whether to drop singleton array dimensions in
#'   the returned preview. default TRUE.
#'
#' @return returns a subset of the raw network data (without netify attributes):
#'
#'   \describe{
#'     \item{\strong{cross-sectional networks}}{
#'       \itemize{
#'         \item single layer: returns a matrix with selected rows and columns
#'         \item multilayer: returns a 3d array (rows x columns x layers)
#'       }
#'     }
#'     \item{\strong{longitudinal networks}}{
#'       \itemize{
#'         \item array format: returns an array with dimensions depending on selection
#'         \item list format: returns a list of matrices, one per selected time period
#'       }
#'     }
#'   }
#'
#'   all returned objects preserve dimension names (actor names, time labels, layer
#'   names) for easy interpretation. single dimensions are automatically dropped.
#'
#' @details
#' \strong{purpose and design}
#'
#' \code{peek} is designed as a lightweight data exploration tool. unlike
#' \code{\link{subset_netify}}, which creates new netify objects with all attributes
#' preserved, \code{peek} returns only the raw network data for quick inspection.
#' this makes it ideal for:
#' \itemize{
#'   \item verifying data structure and content
#'   \item checking specific relationships
#'   \item debugging data issues
#'   \item quick visual inspection of network patterns
#' }
#'
#' \strong{understanding network directions}
#'
#' in directed networks:
#' \itemize{
#'   \item \strong{from} represents actors sending ties (out-ties)
#'   \item \strong{to} represents actors receiving ties (in-ties)
#'   \item cell \code{[i,j]} contains the tie from actor i to actor j
#' }
#'
#' for example, if cell \code{["usa", "china"]} = 5, this means usa sends a tie of
#' strength 5 to china.
#'
#' \strong{smart selection behavior}
#'
#' the function includes several convenience features:
#' \itemize{
#'   \item single numbers are expanded to ranges (e.g., \code{from = 5} becomes first 5 actors)
#'   \item out-of-bounds indices are silently ignored (no errors during exploration)
#'   \item character names are matched to actor labels
#'   \item dimension reduction: if only one time period or layer is selected, that
#'     dimension is dropped from the output
#' }
#'
#'
#' @note
#' \strong{important distinctions:}
#' \itemize{
#'   \item use \code{peek} for quick data inspection (returns raw matrices)
#'   \item use \code{\link{subset_netify}} to create new netify objects with attributes
#'   \item use \code{\link{get_raw}} to extract all raw data from a netify object
#' }
#'
#' when multiple layers are present and no layer selection is specified, all layers
#' are returned with a warning message to remind you about the multilayer structure.
#'
#' @examples
#' # load example data
#' data(icews)
#'
#' # example 1: basic usage with cross-sectional network
#' icews_10 <- icews[icews$year == 2010, ]
#' net <- netify(
#'     icews_10,
#'     actor1 = "i", actor2 = "j",
#'     weight = "verbCoop"
#' )
#'
#' # default: see first 3 actors (both sending and receiving)
#' peek(net)
#'
#' # see first 5 senders and first 5 receivers
#' peek(net, from = 5, to = 5)
#'
#' # example 2: specific actors by name
#' # see ties from us and china to russia, india, and brazil
#' peek(net,
#'     from = c("united states", "china"),
#'     to = c("russia", "india", "brazil")
#' )
#'
#' # use actors parameter to see subgraph
#' peek(net,
#'     actors = c("united states", "china", "russia")
#' )
#'
#' # example 3: longitudinal network
#' net_longit <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     weight = "matlConf"
#' )
#'
#' # see first 5 actors in specific years
#' peek(net_longit,
#'     from = 5, to = 5,
#'     time = c("2002", "2006", "2010")
#' )
#'
#' # see all actors in year 2010
#' peek(net_longit,
#'     from = NULL, to = NULL,
#'     time = "2010"
#' )
#'
#' # example 4: using numeric indices
#' # see specific positions in the network
#' peek(net,
#'     from = c(1, 3, 5, 7), # 1st, 3rd, 5th, 7th senders
#'     to = 1:10
#' ) # first 10 receivers
#'
#' # example 5: quick inspection patterns
#' # see who usa interacts with
#' peek(net, from = "united states", to = 10) # usa's ties to first 10 countries
#' peek(net, from = 10, to = "united states") # first 10 countries' ties to usa
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export peek

peek <- function(netlet,
				 actors = NULL,
				 from = 3,
				 to = 3,
				 time = 1,
				 layers = NULL,
				 drop_dimensions = TRUE) {
	# user input checks
	netify_check(netlet)

	# handle actors parameter - if specified, it sets both from and to
	if (!is.null(actors)) {
		from <- actors
		to <- actors
	}

	# cache attributes for efficiency
	netify_type <- attr(netlet, "netify_type")
	n_layers <- length(attr(netlet, "layers"))
	is_multilayer <- n_layers > 1
	is_longit <- netify_type != "cross_sec"

	# handle layer selection
	if (is_multilayer && is.null(layers)) {
		cli::cli_alert_warning("Multiple layers detected. Showing all layers. Specify 'layers' parameter to select specific layers.")
	} else if (!is_multilayer && !is.null(layers)) {
		cli::cli_alert_warning("Single layer network - ignoring 'layers' parameter.")
		layers <- NULL
	}

	# validate layer selection if provided
	if (!is.null(layers)) {
		available_layers <- attr(netlet, "layers")
		invalid_layers <- setdiff(layers, available_layers)
		if (length(invalid_layers) > 0) {
			cli::cli_abort(
				"Invalid layer(s): {paste(invalid_layers, collapse=', ')}. Available layers: {paste(available_layers, collapse=', ')}"
			)
		}
	}

	# get raw data for processing
	raw_data <- get_raw(netlet)

	# process time selection for longitudinal data
	if (is_longit) {
		time_labels <- if (netify_type == "longit_list") {
			names(raw_data)
		} else {
			dimnames(raw_data)[[if (is_multilayer) 4 else 3]]
		}

		# convert time parameter to indices
		if (is.null(time)) {
			time_idx <- seq_along(time_labels)
		} else if (is.character(time)) {
			time_idx <- match(time, time_labels)
			time_idx <- time_idx[!is.na(time_idx)]
		} else {
			time_idx <- time
		}

		# validate time indices
		max_time <- length(time_labels)
		time_idx <- time_idx[time_idx >= 1 & time_idx <= max_time]

		if (length(time_idx) == 0) {
			cli::cli_abort("No valid time periods selected")
		}
	}

	# process row/column selection based on netify type
	if (netify_type == "cross_sec") {
		return(peek_cross_sectional(raw_data, from, to, layers, is_multilayer, drop_dimensions))
	} else if (netify_type == "longit_array") {
		return(peek_longit_array(raw_data, from, to, time_idx, layers, is_multilayer, drop_dimensions))
	} else if (netify_type == "longit_list") {
		return(peek_longit_list(raw_data, from, to, time_idx, layers, is_multilayer, drop_dimensions))
	}
}

#' extract subset from cross-sectional network data
#'
#' internal helper function that processes cross-sectional network data
#' for the peek function. handles both single-layer and multilayer networks.
#'
#' @param data raw network data (matrix or 3d array)
#' @param rows row selection (actors sending ties)
#' @param cols column selection (actors receiving ties)
#' @param layers layer names to select (NULL for all layers)
#' @param is_multilayer logical indicating if network has multiple layers
#'
#' @return subsetted matrix or array based on selection criteria
#'
#' @keywords internal
#' @noRd
peek_cross_sectional <- function(data, rows, cols, layers, is_multilayer, drop_dimensions = TRUE) {
	# get dimensions
	dims <- dim(data)
	actor_rows <- dimnames(data)[[1]]
	actor_cols <- dimnames(data)[[2]]

	# process row selection
	row_idx <- process_actor_selection(rows, actor_rows, dims[1])
	col_idx <- process_actor_selection(cols, actor_cols, dims[2])

	# subset data
	if (!is_multilayer) {
		return(data[row_idx, col_idx, drop = FALSE])
	} else {
		if (is.null(layers)) {
			out <- data[row_idx, col_idx, , drop = FALSE]
		} else {
			out <- data[row_idx, col_idx, layers, drop = FALSE]
		}
		return(if (isTRUE(drop_dimensions)) drop(out) else out)
	}
}

#' extract subset from longitudinal array network data
#'
#' internal helper function that processes longitudinal array-format network data
#' for the peek function. handles time dimension along with actor and layer selection.
#'
#' @param data raw network data in array format (3d or 4d array)
#' @param rows row selection (actors sending ties)
#' @param cols column selection (actors receiving ties)
#' @param time_idx numeric indices of time periods to extract
#' @param layers layer names to select (NULL for all layers)
#' @param is_multilayer logical indicating if network has multiple layers
#'
#' @return subsetted array with selected dimensions
#'
#' @keywords internal
#' @noRd
peek_longit_array <- function(data, rows, cols, time_idx, layers, is_multilayer, drop_dimensions = TRUE) {
	# get dimensions
	dims <- dim(data)
	actor_rows <- dimnames(data)[[1]]
	actor_cols <- dimnames(data)[[2]]

	# process actor selection
	row_idx <- process_actor_selection(rows, actor_rows, dims[1])
	col_idx <- process_actor_selection(cols, actor_cols, dims[2])

	# subset data
	if (!is_multilayer) {
		out <- data[row_idx, col_idx, time_idx, drop = FALSE]
	} else {
		if (is.null(layers)) {
			out <- data[row_idx, col_idx, , time_idx, drop = FALSE]
		} else {
			out <- data[row_idx, col_idx, layers, time_idx, drop = FALSE]
		}
	}
	if (isTRUE(drop_dimensions)) {
		return(drop(out))
	}
	out
}

#' extract subset from longitudinal list network data
#'
#' internal helper function that processes longitudinal list-format network data
#' for the peek function. handles networks where actor composition may vary across
#' time periods.
#'
#' @param data list of network matrices, one per time period
#' @param rows row selection (actors sending ties)
#' @param cols column selection (actors receiving ties)
#' @param time_idx numeric indices of time periods to extract
#' @param layers layer names to select (NULL for all layers)
#' @param is_multilayer logical indicating if network has multiple layers
#'
#' @return list of subsetted matrices for selected time periods
#'
#' @keywords internal
#' @noRd
peek_longit_list <- function(data, rows, cols, time_idx, layers, is_multilayer, drop_dimensions = TRUE) {
	# subset to requested time periods
	data_subset <- data[time_idx]

	# process each time period
	result <- lapply(data_subset, function(slice) {
		dims <- dim(slice)
		actor_rows <- dimnames(slice)[[1]]
		actor_cols <- dimnames(slice)[[2]]

		# process actor selection for this time period
		row_idx <- process_actor_selection(rows, actor_rows, dims[1])
		col_idx <- process_actor_selection(cols, actor_cols, dims[2])

		# subset
		if (!is_multilayer) {
			return(slice[row_idx, col_idx, drop = FALSE])
		} else {
			if (is.null(layers)) {
				out <- slice[row_idx, col_idx, , drop = FALSE]
			} else {
				out <- slice[row_idx, col_idx, layers, drop = FALSE]
			}
			return(if (isTRUE(drop_dimensions)) drop(out) else out)
		}
	})

	return(result)
}

#' process actor selection for peek operations
#'
#' internal helper function that converts various actor selection formats into
#' numeric indices. handles single numbers (first n), numeric vectors (specific
#' positions), character vectors (actor names), and NULL (all actors).
#'
#' @param selection user input for actor selection (numeric, character, or NULL)
#' @param actor_names character vector of available actor names
#' @param max_dim maximum dimension size (total number of actors)
#'
#' @return numeric vector of valid actor indices
#'
#' @keywords internal
#' @noRd
process_actor_selection <- function(selection, actor_names, max_dim) {
	if (is.null(selection)) {
		# null means all actors
		return(seq_len(max_dim))
	} else if (is.numeric(selection) && length(selection) == 1) {
		# single number means first n actors
		return(seq_len(min(selection, max_dim)))
	} else if (is.numeric(selection)) {
		# numeric vector of indices
		return(selection[selection >= 1 & selection <= max_dim])
	} else if (is.character(selection) || is.factor(selection)) {
		# character vector of actor names
		if (is.factor(selection)) selection <- as.character(selection)
		idx <- match(selection, actor_names)
		return(idx[!is.na(idx)])
	} else {
		cli::cli_abort("Invalid selection type. Use numeric indices or character names.")
	}
}
