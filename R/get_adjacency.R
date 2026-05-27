#' Create a netify matrix from cross-sectional dyadic data
#'
#' `get_adjacency` converts cross-sectional dyadic data into an adjacency matrix
#' of class "netify". This function creates a single network matrix representing
#' relationships at one point in time.
#'
#' @param dyad_data A data.frame containing dyadic observations. Will be coerced
#'   to data.frame if a tibble or data.table is provided. As a convenience,
#'   an existing netify object is also accepted: in that case `get_adjacency()`
#'   returns the underlying adjacency as a plain matrix (no class / attributes),
#'   and for longitudinal inputs returns the first time slice with a cli hint
#'   pointing to `as.matrix(net, time = ...)` for selecting a specific slice.
#' @param actor1 Character string specifying the column name for the first actor
#'   in each dyad.
#' @param actor2 Character string specifying the column name for the second actor
#'   in each dyad.
#' @param symmetric Logical. If TRUE (default), treats the network as undirected
#'   (i.e., edges have no direction). If FALSE, treats the network as directed.
#' @param mode Character string specifying network structure. Options are:
#'   \itemize{
#'     \item \code{"unipartite"}: One set of actors (default)
#'     \item \code{"bipartite"}: Two distinct sets of actors
#'   }
#' @param weight Character string specifying the column name containing edge weights.
#'   If NULL (default), edges are treated as unweighted (binary).
#' @param sum_dyads Logical. If TRUE, sums weight values when multiple edges exist
#'   between the same actor pair. If FALSE (default), uses the last observed value.
#' @param diag_to_NA Logical. If TRUE (default), sets diagonal values (self-loops)
#'   to NA. Automatically set to FALSE for bipartite networks.
#' @param missing_to_zero Logical. If TRUE (default), treats missing edges as zeros.
#'   If FALSE, missing edges remain as NA.
#' @param nodelist Character vector of actor names to include in the network.
#'   If provided, ensures all listed actors appear in the network even if they
#'   have no edges (isolates). Useful when working with edgelists that only
#'   contain active dyads.
#'
#' @return A matrix of class "netify" (a netify matrix) with:
#'   \itemize{
#'   \item \strong{Dimensions}: \code{[n_actors x n_actors]} for unipartite networks
#'       or \code{[n_actors1 x n_actors2]} for bipartite networks
#'     \item \strong{Class}: "netify" - this is a full netify object compatible
#'       with all netify functions
#'     \item \strong{Attributes}: Metadata including network properties and
#'       processing parameters
#'   }
#'
#'   The returned object is a netify matrix that can be used with all netify
#'   functions such as `summary()`, `plot()`, `to_igraph()`, etc.
#'
#' @details
#' \strong{Note on usage:}
#'
#' While this function is exported and available for direct use, the primary and
#' recommended way to create netify objects from dyadic data is through the
#' `netify()` function. The `netify()` function:
#' \itemize{
#'   \item Provides a consistent interface for both cross-sectional and longitudinal data
#'   \item Includes additional data validation and preprocessing options
#'   \item Can incorporate nodal and dyadic attributes during creation
#'   \item Offers more comprehensive parameter checking
#' }
#'
#' Use `get_adjacency()` directly only when you need a simple adjacency matrix
#' creation without additional features.
#'
#' @examples
#' # Load example data
#' data(icews)
#'
#' # Subset to one year for cross-sectional analysis
#' icews_2010 <- icews[icews$year == 2010, ]
#'
#' # Create a directed network with verbal cooperation weights
#' verbCoop_net <- get_adjacency(
#'     dyad_data = icews_2010,
#'     actor1 = "i",
#'     actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # Create a directed network with material conflict weights
#' matlConf_net <- get_adjacency(
#'     dyad_data = icews_2010,
#'     actor1 = "i",
#'     actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "matlConf"
#' )
#'
#' # Verify class
#' class(verbCoop_net) # "netify"
#'
#' # Check dimensions
#' dim(verbCoop_net)
#'
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @export get_adjacency
#'

get_adjacency <- function(
	dyad_data,
	actor1 = NULL, actor2 = NULL,
	symmetric = TRUE, mode = "unipartite",
	weight = NULL, sum_dyads = FALSE,
	diag_to_NA = TRUE, missing_to_zero = TRUE,
	nodelist = NULL) {
	# if input is a netify object, return the underlying adjacency as a plain matrix
	if (inherits(dyad_data, "netify")) {
		nt <- attr(dyad_data, "netify_type")
		if (is.null(nt) || nt == "cross_sec") {
			mat <- unclass(dyad_data)
			attributes(mat) <- list(
				dim = dim(dyad_data),
				dimnames = dimnames(dyad_data)
			)
			return(mat)
		}
		if (nt == "longit_array") {
			cli::cli_alert_info(
				"{.fn get_adjacency} returned the first time slice of a longitudinal array. Use {.code as.matrix(net, time = ...)} to pick a specific slice."
			)
			mat <- unclass(dyad_data)[, , 1, drop = TRUE]
			attributes(mat) <- list(
				dim = dim(mat), dimnames = dimnames(mat)
			)
			return(mat)
		}
		if (nt == "longit_list") {
			cli::cli_alert_info(
				"{.fn get_adjacency} returned the first time slice of a longitudinal list. Use {.code as.matrix(net, time = ...)} to pick a specific slice."
			)
			first <- unclass(dyad_data)[[1]]
			attributes(first) <- list(
				dim = dim(first), dimnames = dimnames(first)
			)
			return(first)
		}
	}

	# bipartite forces diag_to_NA=FALSE and asymmetric; preserve user choice
	user_symmetric <- symmetric
	if (mode == "bipartite") {
		diag_to_NA <- FALSE
		symmetric <- FALSE
	}

	# create weight string for storage as attribute in netify object
	weight_label <- weight_string_label(weight, sum_dyads)

	# add weight if not supplied
	w_orig <- weight
	if (is.null(weight)) {
		dyad_data$weight_var <- 1
		weight <- "weight_var"
	}

	# subset to relevant vars once
	dyad_data <- dyad_data[, c(actor1, actor2, weight)]

	# get vector of actors
	actors_rows <- unique_vector(dyad_data[, actor1])
	actors_cols <- unique_vector(dyad_data[, actor2])
	actors <- unique_vector(actors_rows, actors_cols)
	
	# incorporate nodelist if provided
	if (!is.null(nodelist)) {
		# convert to character to ensure consistency
		nodelist <- as.character(nodelist)
		
		# add any missing actors from nodelist
		if (mode == "unipartite") {
			actors <- unique_vector(actors, nodelist)
			actors_rows <- actors_cols <- actors
		} else {
			# for bipartite, assume nodelist contains all actors
			# user should specify which are row/col actors
			cli::cli_alert_info("For bipartite networks, nodelist should contain all actors. Assigning to both row and column actors.")
			actors_rows <- unique_vector(actors_rows, nodelist)
			actors_cols <- unique_vector(actors_cols, nodelist)
			actors <- unique_vector(actors_rows, actors_cols)
		}
	} else if (mode == "unipartite") {
		actors_rows <- actors_cols <- actors
	}

	# actor year info
	actor_pds <- data.frame(
		actor = actors,
		stringsAsFactors = FALSE
	)
	actor_pds$min_time <- 1
	actor_pds$max_time <- 1

	# auto-promote sum_dyads if repeats are present and weight is supplied
	num_repeat_dyads <- repeat_dyads_check(dyad_data, actor1, actor2)
	if (num_repeat_dyads > 0) {
		evc <- edge_value_check(w_orig, sum_dyads, TRUE)
		sum_dyads <- evc$sum_dyads
	}

	# aggregate data if sum dyads selected
	if (sum_dyads) {
		dyad_data <- aggregate_dyad(dyad_data, actor1, actor2, NULL, weight, symmetric, missing_to_zero)
	}

	# drop zero-weight rows but keep NaN/NA so they propagate as missing
	if (missing_to_zero) {
		w_vec <- dyad_data[, weight]
		nan_rows <- is.nan(w_vec)
		if (any(nan_rows)) {
			w_vec[nan_rows] <- NA_real_
			dyad_data[, weight] <- w_vec
		}
		dyad_data <- dyad_data[which(is.na(w_vec) | w_vec != 0), , drop = FALSE]
	}

	# cache frequently accessed columns
	dyad_actor1 <- dyad_data[, actor1]
	dyad_actor2 <- dyad_data[, actor2]
	dyad_weight <- dyad_data[, weight]

	# assign cross-section value for adjmat depending on user inputs
	value <- dyad_weight

	# create logical value that is TRUE if weight is just 0/1
	is_binary <- length(value) == 0 || all(value %in% c(0, 1))

	# pre-compute matrix indices
	mat_row_indices <- match(dyad_actor1, actors_rows)
	mat_col_indices <- match(dyad_actor2, actors_cols)

	# convert to adjacency matrix
	adj_out <- get_matrix(
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

	# record weight as NULL when no weight supplied and sum_dyads is FALSE
	if (!sum_dyads && is.null(w_orig)) {
		weight <- NULL
	}

	# layer label
	if (is.null(weight)) {
		layer_label <- "weight1"
	} else {
		layer_label <- weight
	}

	# add class info and attributes
	class(adj_out) <- "netify"
	attributes(adj_out) <- c(attributes(adj_out), list(
		netify_type = "cross_sec",
		actor_time_uniform = TRUE,
		actor_pds = actor_pds,
		weight = weight,
		detail_weight = weight_label,
		is_binary = is_binary,
		symmetric = user_symmetric,
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
