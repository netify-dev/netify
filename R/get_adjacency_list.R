#' Create a netify list from longitudinal dyadic data
#'
#' `get_adjacency_list` converts longitudinal dyadic data into a list of adjacency
#' matrices of class "netify". this function creates a list structure where each
#' element is a network matrix for a specific time period, allowing for
#' time-varying actor composition.
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
#' @param actor_time_uniform logical indicating how to handle actor composition:
#'   \itemize{
#'     \item \code{TRUE}: assumes all actors exist across the entire time range
#'     \item \code{FALSE}: determines actor existence from the data - actors exist
#'       from their first observed interaction to their last
#'   }
#' @param actor_pds optional data.frame specifying when actors enter and exit the
#'   network. must contain columns 'actor', 'min_time', and 'max_time'. can be
#'   created using `get_actor_time_info()`. if provided, overrides actor_time_uniform.
#' @param diag_to_NA logical. if TRUE (default), sets diagonal values (self-loops)
#'   to na. automatically set to FALSE for bipartite networks.
#' @param missing_to_zero logical. if TRUE (default), treats missing edges as zeros.
#'   if FALSE, missing edges remain as na.
#' @param nodelist character vector of actor names to include in the network.
#'   if provided, ensures all listed actors appear in the network even if they
#'   have no edges (isolates). useful when working with edgelists that only
#'   contain active dyads.
#'
#' @return a list of class "netify" (a netify list) with:
#'   \itemize{
#'     \item \strong{elements}: named list where each element is a netify matrix
#'       for one time period
#'     \item \strong{names}: character representation of time periods
#'     \item \strong{class}: "netify" - this is a full netify object compatible
#'       with all netify functions
#'     \item \strong{attributes}: extensive metadata including network properties,
#'       actor composition information, and processing parameters
#'   }
#'
#'   each matrix in the list may have different dimensions if actor composition
#'   varies over time. the returned object can be used with all netify functions
#'   such as `summary()`, `plot()`, `to_igraph()`, etc.
#'
#' @details
#' \strong{note on usage:}
#'
#' while this function is exported and available for direct use, the primary and
#' recommended way to create netify objects from longitudinal dyadic data is through
#' the `netify()` function. the `netify()` function:
#' \itemize{
#'   \item automatically chooses between array and list representations based on
#'     your data
#'   \item validates inputs before constructing matrices
#'   \item can incorporate nodal and dyadic attributes during creation
#'   \item offers a unified interface for all types of network data
#' }
#'
#' use `get_adjacency_list()` directly only when you specifically need a list
#' structure or require low-level control over the creation process.
#'
#' \strong{actor composition handling:}
#'
#' this function is particularly useful when actors enter and exit the network
#' over time. unlike `get_adjacency_array()`, which requires constant actor
#' composition, this function can handle:
#' \itemize{
#'   \item new actors appearing in later time periods
#'   \item actors exiting and no longer appearing in the data
#'   \item different sets of actors active in each time period
#' }
#'
#' @examples
#' # load example data
#' data(icews)
#'
#' # create a netify list with constant actor composition
#' icews_list <- get_adjacency_list(
#'     dyad_data = icews,
#'     actor1 = "i",
#'     actor2 = "j",
#'     time = "year",
#'     actor_time_uniform = TRUE,
#'     symmetric = FALSE,
#'     weight = "verbConf"
#' )
#'
#' # verify it's a netify object
#' class(icews_list) # "netify"
#'
#' # check structure
#' length(icews_list) # number of time periods
#' names(icews_list) # time period labels
#'
#' # access specific time period
#' icews_2010 <- icews_list[["2010"]]
#' dim(icews_2010)
#'
#' @author cassy dorff, ha eun choi, shahryar minhas
#'
#' @export get_adjacency_list
#'

get_adjacency_list <- function(
	dyad_data,
	actor1 = NULL, actor2 = NULL, time = NULL,
	symmetric = TRUE, mode = "unipartite",
	weight = NULL, sum_dyads = FALSE,
	actor_time_uniform = FALSE,
	actor_pds = NULL,
	diag_to_NA = TRUE, missing_to_zero = TRUE,
	nodelist = NULL) {
	# create weight string for storage as attribute in netify object
	weight_label <- weight_string_label(weight, sum_dyads)

	# bipartite forces diag_to_NA=false and asymmetric in stored metadata
	if (mode == "bipartite") {
		diag_to_NA <- FALSE
		symmetric <- FALSE
	}

	# check if user supplied actor_pds
	user_actor_pds <- !is.null(actor_pds)
	actor_pds_attr <- NULL

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

	# pre-compute all actors from data
	a1_all <- unique_vector(dyad_data[, actor1])
	a2_all <- unique_vector(dyad_data[, actor2])
	
	# incorporate nodelist if provided
	if (!is.null(nodelist)) {
		# add nodelist actors to the actor sets
		if (mode == "unipartite") {
			nodelist <- as.character(nodelist)
			actors_from_data <- unique_vector(a1_all, a2_all)
			all_actors <- unique_vector(actors_from_data, nodelist)
			a1_all <- a2_all <- all_actors
		} else {
			bp_nodes <- normalize_bipartite_nodelist(nodelist, a1_all, a2_all)
			a1_all <- bp_nodes$rows
			a2_all <- bp_nodes$cols
		}
	}

	# if no actor_pds provided then calculate based on actor_time_uniform
	if (is.null(actor_pds)) {
		if (actor_time_uniform) {
			actors <- unique_vector(a1_all, a2_all)
			min_time <- min(dyad_data[, time], na.rm = TRUE)
			max_time <- max(dyad_data[, time], na.rm = TRUE)
			actor_pds <- data.frame(
				actor = actors,
				min_time = min_time,
				max_time = max_time,
				stringsAsFactors = FALSE
			)
		} else {
			# include nodelist actors that may be missing from edges
			actor_pds <- get_actor_time_info(dyad_data, actor1, actor2, time)
			
			# add nodelist actors absent from actor_pds
			if (!is.null(nodelist)) {
				existing_actors <- actor_pds$actor
				nodelist_actors <- if (mode == "bipartite") {
					unique_vector(a1_all, a2_all)
				} else {
					as.character(nodelist)
				}
				missing_actors <- setdiff(nodelist_actors, existing_actors)
				
				if (length(missing_actors) > 0) {
					# add missing actors with full time range
					min_time <- min(dyad_data[, time], na.rm = TRUE)
					max_time <- max(dyad_data[, time], na.rm = TRUE)
					
					missing_actor_pds <- data.frame(
						actor = missing_actors,
						min_time = min_time,
						max_time = max_time,
						stringsAsFactors = FALSE
					)
					
					actor_pds <- rbind(actor_pds, missing_actor_pds)
				}
			}
		}
		actor_pds_attr <- actor_pds
	} else {
		# make sure actor_pds is a data.frame
		actor_pds <- df_check(actor_pds)

		# make sure every actor has only been entered once
		if (length(unique(actor_pds$actor)) != nrow(actor_pds)) {
			cli::cli_abort(
				"Actors are repeating in `actor_pds`. Every actor must show up only once with a unique `min_time` and `max_time`."
			)
		}

		# rename columns consistently
		names(actor_pds) <- c("actor", "min_time", "max_time")
		actor_pds_attr <- actor_pds

		normalise_actor_pds_bound <- function(x, bound_name) {
			if (time_info$original_class %in% c("numeric", "integer")) {
				out <- suppressWarnings(as.numeric(x))
				if (anyNA(out) || any(!is.finite(out))) {
					cli::cli_abort("{.arg actor_pds${bound_name}} must use numeric time values for numeric {.arg time}.")
				}
				return(out)
			}

			labels <- as.character(time_info$time_labels)
			out <- match(as.character(x), labels)
			missing <- is.na(out)
			if (any(missing)) {
				idx <- suppressWarnings(as.integer(as.character(x[missing])))
				valid_idx <- !is.na(idx) & idx >= 1L & idx <= length(labels)
				out[which(missing)[valid_idx]] <- idx[valid_idx]
			}
			if (anyNA(out)) {
				bad <- unique(as.character(x[is.na(out)]))
				cli::cli_abort(c(
					"x" = "{.arg actor_pds${bound_name}} contains time value{?s} not present in {.arg time}: {.val {bad}}.",
					"i" = "Use labels from the data's time column or integer positions in the netlet time order."
				))
			}
			as.numeric(out)
		}

		# work internally on the numeric scale used by dyad_data, while keeping
		# the user-facing actor_pds labels for the stored netlet attribute.
		actor_pds$min_time <- normalise_actor_pds_bound(actor_pds$min_time, "min_time")
		actor_pds$max_time <- normalise_actor_pds_bound(actor_pds$max_time, "max_time")
		if (any(actor_pds$min_time > actor_pds$max_time)) {
			cli::cli_abort("{.arg actor_pds$min_time} must not occur after {.arg actor_pds$max_time}.")
		}

		# expand to every integer period between min and max
		ap_min <- min(actor_pds$min_time, na.rm = TRUE)
		ap_max <- max(actor_pds$max_time, na.rm = TRUE)
		ap_span <- seq(ap_min, ap_max, by = 1)
		time_pds_num <- sort(unique_vector(
			ap_span,
			actor_pds$min_time,
			actor_pds$max_time
		))
		time_pds <- if (time_info$original_class %in% c("numeric", "integer")) {
			char(time_pds_num)
		} else {
			time_info$time_labels[match(time_pds_num, seq_along(time_info$time_labels))]
		}

		# get actors present in data
		actors_in_data <- unique_vector(a1_all, a2_all)
		if (mode == "unipartite") {
			a1_all <- a2_all <- actors_in_data
		}

		# create logical vectors for filtering
		actor1_valid <- dyad_data[, actor1] %in% actors_in_data
		actor2_valid <- dyad_data[, actor2] %in% actors_in_data
		time_valid <- dyad_data[, time] %in% time_pds_num

		# apply all filters at once
		valid_rows <- actor1_valid & actor2_valid & time_valid
		dyad_data <- dyad_data[valid_rows, ]

		# stop process if no dyads remain
		if (nrow(dyad_data) == 0) {
			cli::cli_abort(
				"No dyads remain after subsetting to the actors and years defined in `actor_pds`."
			)
		}

		# create lookup table for valid actor-time combinations
		actor_time_lookup <- do.call(rbind, lapply(1:nrow(actor_pds), function(i) {
			data.frame(
				actor = actor_pds$actor[i],
				time = actor_pds$min_time[i]:actor_pds$max_time[i],
				stringsAsFactors = FALSE
			)
		}))

		# create keys for fast lookup
		lookup_key <- paste(actor_time_lookup$actor, actor_time_lookup$time, sep = "_")
		data_key1 <- paste(dyad_data[, actor1], dyad_data[, time], sep = "_")
		data_key2 <- paste(dyad_data[, actor2], dyad_data[, time], sep = "_")

		# filter using vectorized %in% operations
		valid_pairs <- data_key1 %in% lookup_key & data_key2 %in% lookup_key
		dyad_data <- dyad_data[valid_pairs, ]

		# update actor lists based on filtered data
		a1_all <- unique_vector(dyad_data[, actor1])
		a2_all <- unique_vector(dyad_data[, actor2])
	}

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

	# pre-compute actor presence matrix for all time periods
	actor_presence_matrix <- matrix(FALSE, nrow = nrow(actor_pds), ncol = length(time_pds_num))
	for (i in 1:nrow(actor_pds)) {
		actor_presence_matrix[i, ] <- (time_pds_num >= actor_pds$min_time[i]) &
			(time_pds_num <= actor_pds$max_time[i])
	}

	# pre-split data by time periods for faster subsetting
	time_indices <- split(seq_len(nrow(dyad_data)), dyad_data[, time])

	# cache frequently accessed columns
	dyad_actor1 <- dyad_data[, actor1]
	dyad_actor2 <- dyad_data[, actor2]
	dyad_weight <- dyad_data[, weight]

	# iterate through time periods
	adj_out <- vector("list", length(time_pds))
	names(adj_out) <- time_pds

	for (t_idx in seq_along(time_pds)) {
		time_pd <- time_pds[t_idx]
		time_pd_num <- time_pds_num[t_idx]

		# get indices for this time period
		slice_indices <- time_indices[[as.character(time_pd_num)]]
		if (is.null(slice_indices)) slice_indices <- integer(0)

		# determine actors present in this time period
		actor_present <- actor_presence_matrix[, t_idx]
		actors <- sort(actor_pds$actor[actor_present])
		actors_rows <- actors_cols <- actors

		# break up into rows and cols for bipartite
		if (mode == "bipartite") {
			actors_rows <- sort(actor_pds$actor[actor_present & actor_pds$actor %in% a1_all])
			actors_cols <- sort(actor_pds$actor[actor_present & actor_pds$actor %in% a2_all])
		}

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

		# if user left weight null and set sum_dyads to false
		weight_attr <- if (!sum_dyads && is.null(w_orig)) NULL else weight
		layer_label <- if (is.null(weight_attr)) "weight1" else weight_attr

		# add class info and attributes
		class(adj_mat) <- "netify"
		attributes(adj_mat) <- c(attributes(adj_mat), list(
			netify_type = "cross_sec",
			actor_time_uniform = NULL,
			actor_pds = NULL,
			weight = weight_attr,
			detail_weight = weight_label,
			is_binary = is_binary,
			symmetric = symmetric,
			mode = mode,
			layers = layer_label,
			diag_to_NA = diag_to_NA,
			missing_to_zero = missing_to_zero,
			sum_dyads = sum_dyads,
			nodal_data = NULL,
			dyad_data = NULL
		))

		adj_out[[t_idx]] <- adj_mat
	}

	# final weight attribute handling
	weight_final <- if (!sum_dyads && is.null(w_orig)) NULL else weight
	layer_label_final <- if (is.null(weight_final)) "weight1" else weight_final

	# if user supplied actor_pds then set actor_time_uniform to false
	if (user_actor_pds) actor_time_uniform <- FALSE

	# get info on binary weights using vectorized operation
	bin_check <- vapply(adj_out, function(x) attr(x, "is_binary"), logical(1))

	# add attributes to list
	class(adj_out) <- "netify"
	attributes(adj_out) <- c(attributes(adj_out), list(
			netify_type = "longit_list",
			actor_time_uniform = actor_time_uniform,
			actor_pds = actor_pds_attr %||% actor_pds,
		weight = weight_final,
		detail_weight = weight_label,
		is_binary = all(bin_check),
			symmetric = symmetric,
		mode = mode,
		layers = layer_label_final,
		diag_to_NA = diag_to_NA,
		missing_to_zero = missing_to_zero,
		sum_dyads = sum_dyads,
		nodal_data = NULL,
		dyad_data = NULL
	))

	return(adj_out)
}
