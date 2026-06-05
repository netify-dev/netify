#' Create a netify array from longitudinal dyadic data
#'
#' `get_adjacency_array` converts longitudinal dyadic data into a three-dimensional
#' netify array where the first two dimensions represent actors and the third
#' dimension represents time periods. this function creates an array of class
#' "netify" and should only be used when actor composition remains constant
#' across all time periods.
#'
#' @param dyad_data a data.frame containing longitudinal dyadic observations. will
#'   be coerced to data.frame if a tibble or data.table is provided.
#' @param actor1 character string specifying the column name for the first actor
#'   in each dyad.
#' @param actor2 character string specifying the column name for the second actor
#'   in each dyad.
#' @param time character string specifying the column name for time periods.
#' @param symmetric logical. if TRUE (default), treats the network as undirected
#'   (i.e., edges have no direction). if FALSE, treats the network as directed.
#' @param mode character string specifying network structure. options are:
#'   \itemize{
#'     \item \code{"unipartite"}: one set of actors (default)
#'     \item \code{"bipartite"}: two distinct sets of actors
#'   }
#' @param weight character string specifying the column name containing edge weights.
#'   if NULL (default), edges are treated as unweighted (binary).
#' @param sum_dyads logical. if TRUE, sums weight values when multiple edges exist
#'   between the same actor pair in the same time period. if FALSE (default), uses
#'   the last observed value.
#' @param diag_to_NA logical. if TRUE (default), sets diagonal values (self-loops)
#'   to na. automatically set to FALSE for bipartite networks.
#' @param missing_to_zero logical. if TRUE (default), treats missing edges as zeros.
#'   if FALSE, missing edges remain as na.
#' @param nodelist character vector of actor names to include in the network.
#'   if provided, ensures all listed actors appear in the network even if they
#'   have no edges (isolates). useful when working with edgelists that only
#'   contain active dyads.
#'
#' @return a three-dimensional array of class "netify" (a netify array) with:
#'   \itemize{
#'     \item \strong{dimensions}: \code{[n_actors x n_actors x n_time]} for unipartite
#'       networks or \code{[n_actors1 x n_actors2 x n_time]} for bipartite networks
#'     \item \strong{class}: "netify" - this is a full netify object compatible
#'       with all netify functions
#'     \item \strong{attributes}: extensive metadata including network properties,
#'       actor information, and processing parameters
#'   }
#'
#'   the returned object is a netify array that can be used with all netify
#'   functions such as `summary()`, `plot()`, `to_igraph()`, etc.
#'
#' @details
#' \strong{note on usage:}
#'
#' while this function is exported and available for direct use, the primary and
#' recommended way to create netify arrays from longitudinal dyadic data is through
#' the `netify()` function. the `netify()` function:
#' \itemize{
#'   \item automatically determines whether to create an array or list structure
#'   \item handles time-varying actor composition
#'   \item validates inputs before constructing arrays
#'   \item offers a unified interface for all types of network data
#' }
#'
#' use `get_adjacency_array()` directly only when you specifically need low-level
#' control over array creation and are certain your actors remain constant across
#' time.
#'
#' @examples
#' # load example data
#' data(icews)
#'
#' # create a netify array (longitudinal directed network)
#' # with material conflict as edge weights
#' icews_array <- get_adjacency_array(
#'     dyad_data = icews,
#'     actor1 = "i",
#'     actor2 = "j",
#'     time = "year",
#'     symmetric = FALSE,
#'     weight = "matlConf"
#' )
#'
#' # verify it's a netify object
#' class(icews_array) # "netify"
#'
#' # check dimensions
#' dim(icews_array) # [n_actors, n_actors, n_years]
#'
#' # access specific time period
#' icews_2010 <- icews_array[, , "2010"]
#'
#' @author cassy dorff, ha eun choi, shahryar minhas
#'
#' @export get_adjacency_array

get_adjacency_array <- function(
	dyad_data,
	actor1 = NULL, actor2 = NULL, time = NULL,
	symmetric = TRUE, mode = "unipartite",
	weight = NULL, sum_dyads = FALSE,
	diag_to_NA = TRUE, missing_to_zero = TRUE,
	nodelist = NULL) {
	# create weight string for storage as attribute in netify object
	weight_label <- weight_string_label(weight, sum_dyads)

	# bipartite forces diag_to_NA=false and asymmetric
	if (mode == "bipartite") {
		diag_to_NA <- FALSE
		symmetric <- FALSE
	}

	# convert time to numeric and get labels
	time_info <- convert_time_to_numeric(dyad_data[[time]], time)
	dyad_data[[time]] <- time_info$numeric_time

	# time check
	if (!is.numeric(dyad_data[, time])) {
		cli::cli_abort("Failed to convert time variable to numeric format.")
	}

	# add weight if not supplied
	w_orig <- weight
	if (is.null(weight)) {
		dyad_data$weight_var <- 1
		weight <- "weight_var"
	}

	# subset to relevant vars once
	dyad_data <- dyad_data[, c(actor1, actor2, time, weight)]

	# get vector of time periods from conversion
	time_pds <- time_info$time_labels
	time_pds_num <- sort(unique(time_info$numeric_time))

	# get vector of actors
	actors_rows <- unique_vector(dyad_data[, actor1])
	actors_cols <- unique_vector(dyad_data[, actor2])
	actors <- unique_vector(actors_rows, actors_cols)
	
	# incorporate nodelist if provided
	if (!is.null(nodelist)) {
		# add any missing actors from nodelist
		if (mode == "unipartite") {
			nodelist <- as.character(nodelist)
			actors <- unique_vector(actors, nodelist)
			actors_rows <- actors_cols <- actors
		} else {
			bp_nodes <- normalize_bipartite_nodelist(nodelist, actors_rows, actors_cols)
			actors_rows <- bp_nodes$rows
			actors_cols <- bp_nodes$cols
			actors <- unique_vector(actors_rows, actors_cols)
		}
	} else if (mode == "unipartite") {
		actors_rows <- actors_cols <- actors
	}

	# add info on actor time sample
	actor_pds <- data.frame(
		actor = actors,
		stringsAsFactors = FALSE
	)
	actor_pds$min_time <- time_pds[1]
	actor_pds$max_time <- time_pds[length(time_pds)]

	# auto-promote sum_dyads if repeats are present and weight is supplied
	num_repeat_dyads <- repeat_dyads_check(dyad_data, actor1, actor2, time)
	if (num_repeat_dyads > 0) {
		evc <- edge_value_check(w_orig, sum_dyads, TRUE)
		sum_dyads <- evc$sum_dyads
	}

	# aggregate data if sum dyads selected
	if (sum_dyads) {
		dyad_data <- aggregate_dyad(dyad_data, actor1, actor2, time, weight, symmetric, missing_to_zero)
	}

	# drop zero-weight rows but keep nan/na so they propagate as missing
	if (missing_to_zero) {
		w_vec <- dyad_data[, weight]
		nan_rows <- is.nan(w_vec)
		if (any(nan_rows)) {
			w_vec[nan_rows] <- NA_real_
			dyad_data[, weight] <- w_vec
		}
		dyad_data <- dyad_data[which(is.na(w_vec) | w_vec != 0), , drop = FALSE]
	}

	# pre-split data by time periods for faster subsetting
	time_indices <- split(seq_len(nrow(dyad_data)), dyad_data[, time])

	# cache frequently accessed columns
	dyad_actor1 <- dyad_data[, actor1]
	dyad_actor2 <- dyad_data[, actor2]
	dyad_weight <- dyad_data[, weight]

	# organize array dimensions
	n_rows <- length(actors_rows)
	n_cols <- length(actors_cols)
	t <- length(time_pds)
	adj_out <- array(NA,
		dim = c(n_rows, n_cols, t),
		dimnames = list(actors_rows, actors_cols, time_pds)
	)

	# binary weight check vector
	bin_check <- logical(t)

	# fill in array by time slice
	for (t_idx in seq_along(time_pds)) {
		time_pd <- time_pds[t_idx]
		time_pd_num <- time_pds_num[t_idx]

		# get indices for this time period
		slice_indices <- time_indices[[as.character(time_pd_num)]]
		if (is.null(slice_indices)) slice_indices <- integer(0)

		# get values and indices for this time slice
		if (length(slice_indices) > 0) {
			slice_actor1 <- dyad_actor1[slice_indices]
			slice_actor2 <- dyad_actor2[slice_indices]
			value <- dyad_weight[slice_indices]

			# pre-compute matrix indices
			mat_row_indices <- match(slice_actor1, actors_rows)
			mat_col_indices <- match(slice_actor2, actors_cols)
		} else {
			value <- numeric(0)
			mat_row_indices <- integer(0)
			mat_col_indices <- integer(0)
		}

		# create logical value that is true if observed weights are just 0/1
		value_observed <- value[!is.na(value)]
		is_binary <- length(value_observed) == 0 || all(value_observed %in% c(0, 1))
		bin_check[t_idx] <- is_binary

		# build adjacency matrix
		adj_mat <- get_matrix(
			n_rows = length(actors_rows),
			n_cols = length(actors_cols),
			actors_rows = actors_rows,
			actors_cols = actors_cols,
				matRowIndices = mat_row_indices,
				matColIndices = mat_col_indices,
				value = value,
				symmetric = symmetric,
				missing_to_zero = missing_to_zero,
				diag_to_NA = diag_to_NA && mode == "unipartite"
			)

		# insert into array
		adj_out[, , as.character(time_pd)] <- adj_mat
	}

	# record weight as null when no weight supplied and sum_dyads is false
	if (!sum_dyads && is.null(w_orig)) {
		weight <- NULL
	}

	# layer label
	if (is.null(weight)) {
		layer_label <- "weight1"
	} else {
		layer_label <- weight
	}

	# add attributes to array
	class(adj_out) <- "netify"
	attributes(adj_out) <- c(attributes(adj_out), list(
		netify_type = "longit_array",
		actor_time_uniform = TRUE,
		actor_pds = actor_pds,
		weight = weight,
			detail_weight = weight_label,
			is_binary = all(bin_check),
			symmetric = symmetric,
			mode = mode,
		layers = layer_label,
		diag_to_NA = diag_to_NA,
		missing_to_zero = missing_to_zero,
		sum_dyads = sum_dyads,
		nodal_data = NULL,
		dyad_data = NULL
	))

	return(adj_out)
}
