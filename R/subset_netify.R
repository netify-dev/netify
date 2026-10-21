#' Subset netify objects
#'
#' extracts a subset of a netify object based on specified actors, time periods,
#' and/or layers while preserving all netify attributes and structure.
#'
#' @param x a netify object to subset
#' @param actors character vector of actor names or numeric indices to subset.
#'   extracts the subgraph among these actors (includes ties both from and to
#'   these actors). default is NULL, which includes all actors.
#' @param time time periods to subset. can be:
#'   \itemize{
#'     \item numeric vector: used as indices to the time dimension
#'     \item character vector: matched against time dimension labels
#'     \item NULL: includes all time periods (default)
#'   }
#' @param layers character vector of layer names to subset from multilayer networks.
#'   for single-layer networks, this is ignored. for multilayer networks, at least
#'   one layer must be specified.
#' @param from character vector of actor names or numeric indices for actors sending
#'   ties (row actors). overrides \code{actors}. set to NULL to include all
#'   sending actors. in bipartite networks, this refers to actors in the first mode.
#'   in unipartite networks, the result stays square over the union of \code{from}
#'   and \code{to} actors, and ties outside the \code{from} x \code{to} block are
#'   set to NA.
#' @param to character vector of actor names or numeric indices for actors receiving
#'   ties (column actors). overrides \code{actors}. set to NULL to include all
#'   receiving actors. in bipartite networks, this refers to actors in the second mode.
#'   in unipartite networks, see \code{from} for how the two selections combine.
#' @param ... additional arguments (currently unused)
#'
#' @return a netify object containing the requested subset with:
#'   \itemize{
#'     \item subsetted adjacency matrix/matrices
#'     \item corresponding nodal attributes (filtered to included actors/times)
#'     \item corresponding dyadic attributes (filtered to included actor pairs/times)
#'     \item updated netify attributes reflecting the new dimensions
#'   }
#'
#'   the returned object's structure depends on the subset:
#'   \itemize{
#'     \item if one time period is selected from longitudinal data, returns cross-sectional
#'     \item if one layer is selected from multilayer data, returns single-layer
#'     \item otherwise, maintains the original structure type
#'   }
#'
#' @details
#' this function is a netify-aware wrapper around the \code{\link{peek}} function,
#' which handles the raw data extraction. while \code{peek} returns raw matrices/arrays,
#' \code{subset} additionally:
#' \itemize{
#'   \item preserves and updates all netify attributes
#'   \item filters nodal and dyadic attribute data to match the subset
#'   \item adjusts the netify type when dimensions change (e.g., longitudinal to cross-sectional)
#'   \item maintains consistency between network data and attributes
#' }
#'
#' the \code{from} and \code{to} parameters allow precise control over which ties
#' to include:
#' \itemize{
#'   \item use \code{actors} to get all ties among a set of actors (subgraph extraction)
#'   \item use \code{from} to get all ties sent by specific actors
#'   \item use \code{to} to get all ties received by specific actors
#'   \item use both \code{from} and \code{to} to get ties between specific sets of actors
#' }
#'
#' for unipartite networks, a \code{from}/\code{to} subset keeps a square matrix
#' over the union of the selected actors (in their original order), so the result
#' remains a valid unipartite network. cells for ties that are not sent by a
#' \code{from} actor to a \code{to} actor are set to NA. for symmetric networks,
#' ties in either direction between the two sets are kept.
#'
#' for bipartite networks, \code{from} refers to actors in the first mode (e.g.,
#' people) and \code{to} refers to actors in the second mode (e.g., organizations).
#' selections must include actors from both modes.
#'
#' @note
#' when subsetting longitudinal data to a single time period, the function automatically
#' converts the result to a cross-sectional netify object. similarly, subsetting
#' multilayer data to a single layer produces a single-layer object.
#'
#' @examples
#'
#' \donttest{
#' # load example directed event data from icews
#' data(icews)
#'
#' # generate a longitudinal netify object
#' # with both dyadic and nodal attributes
#' icews_matlConf <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE, weight = "matlConf",
#'     nodal_vars = c("i_polity2", "i_log_gdp", "i_log_pop"),
#'     dyad_vars = c("matlCoop", "verbCoop", "verbConf"),
#'     dyad_vars_symmetric = c(FALSE, FALSE, FALSE)
#' )
#'
#' # subset to a few countries using s3 method
#' icews_subset <- subset(
#'     icews_matlConf,
#'     actors = c(
#'         "United States", "United Kingdom",
#'         "Russian Federation", "China"
#'     )
#' )
#'
#' # subset to a few countries and a few years
#' icews_subset_2 <- subset(
#'     icews_matlConf,
#'     actors = c(
#'         "United States", "United Kingdom",
#'         "Russian Federation", "China"
#'     ),
#'     time = c("2010", "2011")
#' )
#'
#' # can also use subset_netify directly
#' icews_subset_3 <- subset_netify(
#'     netlet = icews_matlConf,
#'     actors = c(
#'         "United States", "United Kingdom",
#'         "Russian Federation", "China"
#'     ),
#'     time = c("2010", "2011")
#' )
#' }
#'
#' @author cassy dorff, shahryar minhas
#'
#' @method subset netify
#' @export

subset.netify <- function(
	x,
	actors = NULL,
	time = NULL,
	layers = NULL,
	from = NULL,
	to = NULL,
	...) {
	# call subset_netify to handle nitty gritty
	subset_netify(
		netlet = x,
		actors = actors,
		time = time,
		layers = layers,
		from = from,
		to = to
	)
}

#' internal subset function for netify objects
#'
#' this is the internal workhorse function called by the s3 method \code{subset.netify}.
#' users should typically use \code{subset()} on netify objects rather than calling
#' this function directly.
#'
#' @param netlet a netify object to subset
#' @param actors character vector of actor names or numeric indices to subset
#' @param time time periods to subset
#' @param layers character vector of layer names to subset from multilayer networks
#' @param from character vector of actor names or numeric indices for actors sending ties
#' @param to character vector of actor names or numeric indices for actors receiving ties
#'
#' @return a netify object containing the requested subset
#'
#' @keywords internal
#' @export subset_netify

subset_netify <- function(
	netlet,
	actors = NULL,
	time = NULL,
	layers = NULL,
	from = NULL,
	to = NULL) {
	# check if netify object
	netify_check(netlet)

	# pull out attrs and msrmnts of original - cache for efficiency
	obj_attrs <- attributes(netlet)
	msrmnts <- netify_measurements(netlet)
	nlayers <- length(obj_attrs$layers)
	netify_type <- obj_attrs$netify_type

	# if longit_list with only one time period, flatten it
	if (netify_type == "longit_list" && length(netlet) == 1) {
		cli::cli_alert_warning(
			"Input is a longitudinal list with only one time period. Converting to cross-sectional."
		)
		# extract the single element
		netlet <- netlet[[1]]
		# update attributes
		obj_attrs <- attributes(netlet)
		netify_type <- obj_attrs$netify_type
		msrmnts <- netify_measurements(netlet)
	}

	# set from/to defaults based on actors parameter
	if (is.null(from) && !is.null(actors)) from <- actors
	if (is.null(to) && !is.null(actors)) to <- actors

	# cache logical checks
	is_multilayer_orig <- nlayers > 1
	is_longit <- netify_type != "cross_sec"

	# friendly error if user passes time on a cross-sectional network
	if (!is_longit && !is.null(time)) {
		cli::cli_abort(c(
			"!" = "{.arg time} was supplied but this is a cross-sectional network (no time dimension).",
			"i" = "Drop the {.arg time} argument, or build a longitudinal netify with {.code time = <your_time_column>}."
		))
	}

		# choose multilayer output
	nlayers_subset <- if (!is.null(layers)) length(layers) else nlayers
	is_multilayer_out <- nlayers_subset > 1

		# choose time points
	time_values <- NULL
	if (is_longit) {
			# keep all time points when none are requested
		if (is.null(time)) {
			time_values <- msrmnts$time
		} else if (is.numeric(time)) {
			# keep requested periods in netlet order
			time_idx <- seq_along(msrmnts$time) %in% time
			if (!any(time_idx)) {
				cli::cli_abort("No valid time periods selected.")
			}
			time_values <- msrmnts$time[time_idx]
		} else {
			time_idx <- as.character(msrmnts$time) %in% as.character(time)
			if (!any(time_idx)) {
				cli::cli_abort("No valid time periods selected.")
			}
			time_values <- msrmnts$time[time_idx]
		}
		n_time_out <- length(time_values)
		is_longit_out <- n_time_out > 1
	} else {
		is_longit_out <- FALSE
	}

	# unipartite from/to selections keep a square actor set
	is_bipartite_in <- identical(obj_attrs$mode, "bipartite")
	square_from_to <- !is_bipartite_in && !identical(from, to)

	# use peek to subset data with new parameter names
	sub_net <- peek(
		netlet,
		from = if (square_from_to) NULL else from,
		to = if (square_from_to) NULL else to,
		time = time_values %||% time,
		layers = layers,
		drop_dimensions = !is_multilayer_out
	)

	# restrict to the union of from and to actors and mask other cells
	if (square_from_to) {
		symmetric_mask <- isTRUE(all(obj_attrs$symmetric))
		if (is.list(sub_net)) {
			sub_net <- lapply(
				sub_net,
				mask_from_to,
				from = from,
				to = to,
				symmetric = symmetric_mask
			)
		} else {
			sub_net <- mask_from_to(
				sub_net,
				from = from,
				to = to,
				symmetric = symmetric_mask
			)
		}
	}

	# stop when a bipartite selection leaves one mode empty
	if (is_bipartite_in) {
		sub_list <- if (is.list(sub_net)) sub_net else list(sub_net)
		no_rows <- all(vapply(sub_list, NROW, integer(1)) == 0L)
		no_cols <- all(vapply(sub_list, NCOL, integer(1)) == 0L)
		if (no_rows || no_cols) {
			empty_mode <- if (no_rows) "row (first-mode)" else "column (second-mode)"
			fix_hint <- if (!is.null(actors)) {
				"Include actors from both modes in {.arg actors}, or use {.arg from} for row actors and {.arg to} for column actors."
			} else {
				"Check that {.arg from} lists row actors and {.arg to} lists column actors."
			}
			cli::cli_abort(c(
				"x" = "The selection leaves no {empty_mode} actors in this bipartite network.",
				"i" = fix_hint
			))
		}
	}

	# drop the time dimension of a single-period multilayer array
	if (netify_type == "longit_array" && is_multilayer_out && !is_longit_out) {
		sub_dims <- dim(sub_net)
		sub_net <- array(
			sub_net,
			dim = sub_dims[1:3],
			dimnames = dimnames(sub_net)[1:3]
		)
	}

	# rebuild the netify structure on top of peek's raw matrices
	if (netify_type == "longit_list" && is.list(sub_net)) {
		# get a reference element to copy attributes from
		ref_elem <- netlet[[1]]
		ref_attrs <- attributes(ref_elem)
		sub_net_names <- names(sub_net)

		# rebuild each element with proper netify attributes
		sub_net <- lapply(names(sub_net), function(time_name) {
			mat <- sub_net[[time_name]]

			# start with the matrix attributes (dim, dimnames)
			mat_attrs <- attributes(mat)

			# add all the netify attributes from reference, except dim/dimnames
			netify_attrs <- ref_attrs[!names(ref_attrs) %in% c("dim", "dimnames")]

			# combine attributes
			all_attrs <- c(mat_attrs, netify_attrs)

			# apply attributes
			attributes(mat) <- all_attrs

			# keep netify class
			class(mat) <- "netify"

			return(mat)
		})
		names(sub_net) <- sub_net_names
	}

	# handle structure changes when going from longit to cross-sectional
	extracted_single_time <- FALSE
	if (is_longit && !is_longit_out) {
		if (is.list(sub_net) && length(sub_net) == 1) {
			# extract from list
			sub_net <- sub_net[[1]]
			extracted_single_time <- TRUE
		} else if (netify_type == "longit_array") {
			# update type for arrays
			obj_attrs$netify_type <- "cross_sec"
		}
	}

	# create new attributes object
	if (extracted_single_time) {
		# use the extracted element's attributes as base
		new_attrs <- attributes(sub_net)

		# update actor_pds, nodal_data, dyad_data from parent if needed
		if (!is.null(obj_attrs$actor_pds)) {
			new_attrs$actor_pds <- obj_attrs$actor_pds
		}
		if (!is.null(obj_attrs$nodal_data)) {
			new_attrs$nodal_data <- obj_attrs$nodal_data
		}
		if (!is.null(obj_attrs$dyad_data)) {
			new_attrs$dyad_data <- obj_attrs$dyad_data
		}
	} else {
		# normal case - copy original attributes
		new_attrs <- obj_attrs
	}

	# update layer-related attributes if layers were subsetted
	if (!is.null(layers)) {
		new_attrs$layers <- layers

		# find indices of selected layers efficiently
		layer_idx <- match(layers, obj_attrs$layers)

		# update all layer-specific attributes at once
		if (!is.null(obj_attrs[["weight"]])) {
			weight_vec <- strsplit(as.character(obj_attrs[["weight"]]), ", ")[[1]]
			if (length(weight_vec) == length(obj_attrs$layers)) {
				new_attrs[["weight"]] <- paste(weight_vec[layer_idx], collapse = ", ")
			}
		}

		if (!is.null(obj_attrs$detail_weight)) {
			detail_vec <- strsplit(as.character(obj_attrs$detail_weight), " \\| ", perl = TRUE)[[1]]
			if (length(detail_vec) == length(obj_attrs$layers)) {
				new_attrs$detail_weight <- paste(detail_vec[layer_idx], collapse = " | ")
			}
		}

		# update logical vectors
		for (attr_name in c("is_binary", "diag_to_NA", "missing_to_zero", "symmetric")) {
			if (!is.null(obj_attrs[[attr_name]]) && length(obj_attrs[[attr_name]]) > 1) {
				subset_val <- obj_attrs[[attr_name]][layer_idx]
				# collapse to scalar if only one layer or all values identical
				if (length(subset_val) == 1 || length(unique(subset_val)) == 1) {
					subset_val <- unname(subset_val[1])
				}
				new_attrs[[attr_name]] <- subset_val
			}
		}
	}

	# bipartite uses both row and column actors as the universe
	is_bipartite <- identical(new_attrs$mode, "bipartite") ||
		identical(obj_attrs$mode, "bipartite")

	# get actors in subsetted netlet efficiently
	if (is.list(sub_net) && is_longit_out) {
		# for lists, get unique actors across all time periods at once
		if (is_bipartite) {
			all_actors <- unique(c(
				unlist(lapply(sub_net, rownames), use.names = FALSE),
				unlist(lapply(sub_net, colnames), use.names = FALSE)
			))
		} else {
			all_actors <- unique(unlist(lapply(sub_net, rownames), use.names = FALSE))
		}

		# update list-specific attributes
		new_attrs$names <- names(sub_net)

		# update attributes for each list element if needed
		if (is_multilayer_orig || !is.null(layers)) {
			sub_net <- lapply(sub_net, function(x) {
				elem_attrs <- attributes(x)
				# update layer-related attributes in each element
				elem_attrs$layers <- new_attrs$layers
				elem_attrs[["weight"]] <- new_attrs[["weight"]]
				elem_attrs$detail_weight <- new_attrs$detail_weight
				elem_attrs$is_binary <- new_attrs$is_binary
				elem_attrs$diag_to_NA <- new_attrs$diag_to_NA
				elem_attrs$missing_to_zero <- new_attrs$missing_to_zero
				elem_attrs$symmetric <- new_attrs$symmetric
				# reapply attributes
				attributes(x) <- elem_attrs
				return(x)
			})
		}
	} else {
		if (is_bipartite) {
			all_actors <- unique(c(rownames(sub_net), colnames(sub_net)))
		} else {
			all_actors <- rownames(sub_net)
		}

		# update dimensions if not already done
		if (!extracted_single_time) {
			new_attrs[1:2] <- attributes(sub_net)[1:2]
		}

		# update netify_type based on dimensions
		n_dims <- length(dim(sub_net))
		if (n_dims == 2 && !is_multilayer_out) {
			new_attrs$netify_type <- "cross_sec"
		} else if (n_dims == 3 && is_multilayer_out && !is_longit_out) {
			new_attrs$netify_type <- "cross_sec"
		}
	}

	# update actor_pds efficiently using vectorized operations
	if (!is.null(new_attrs$actor_pds)) {
		actor_pds <- new_attrs$actor_pds
		actor_pds_subset <- actor_pds[actor_pds$actor %in% all_actors, , drop = FALSE]

		if (is_longit && !is_longit_out) {
			# set bounds to the single retained period
			actor_pds_subset$min_time <- time_values[1]
			actor_pds_subset$max_time <- time_values[length(time_values)]
		} else if (is_longit && nrow(actor_pds_subset) > 0L) {
			# clip actor windows to the retained periods
			actor_pds_subset <- clip_actor_pds_to_time(
				actor_pds_subset,
				orig_labels = msrmnts$time,
				kept_labels = time_values
			)
		}
		new_attrs$actor_pds <- actor_pds_subset
	}

	# update nodal_data efficiently
	if (!is.null(new_attrs$nodal_data)) {
		nodal_data <- new_attrs$nodal_data

		# filter by time for longitudinal networks
		if (is_longit) {
			keep_rows <- nodal_data$actor %in% all_actors &
				as.character(nodal_data$time) %in% as.character(time_values)
		} else {
			keep_rows <- nodal_data$actor %in% all_actors
		}

		new_attrs$nodal_data <- nodal_data[keep_rows, , drop = FALSE]
		if (is_longit && !is_longit_out && "time" %in% names(new_attrs$nodal_data)) {
			new_attrs$nodal_data$time <- NULL
		}
	}

	# update dyad_data efficiently
	if (!is.null(new_attrs$dyad_data)) {
		dyad_data <- new_attrs$dyad_data

		# subset time periods first if needed
		if (is_longit && !extracted_single_time) {
			dyad_data <- dyad_data[as.character(time_values)]
		}

		# for single time extraction, get the right time period
		if (extracted_single_time && length(dyad_data) > 1) {
			dyad_data <- dyad_data[as.character(time_values)]
		}

		# cross-sectional dyad data is keyed by "1"
		if (is_longit && !is_longit_out) {
			names(dyad_data) <- "1"
		}

		# process each time period
		new_attrs$dyad_data <- lapply(dyad_data, function(time_data) {
			# process each variable matrix
			lapply(time_data, function(var_matrix) {
				# get actors that exist in this matrix
				toKeep_rows <- intersect(rownames(var_matrix), all_actors)
				toKeep_cols <- intersect(colnames(var_matrix), all_actors)
				# use direct indexing with actor names
				var_matrix[toKeep_rows, toKeep_cols, drop = FALSE]
			})
		})
	}

	# apply attributes based on object type
	if (is.list(sub_net) && is_longit_out) {
		# for longitudinal lists, apply list-level attributes
		attributes(sub_net) <- new_attrs
		# keep netify class on the list
		class(sub_net) <- "netify"
	} else {
		# for single matrices/arrays, apply all attributes at once
		attributes(sub_net) <- new_attrs
		# keep netify class
		if (!inherits(sub_net, "netify")) {
			class(sub_net) <- c("netify", class(sub_net))
		}
	}

	return(sub_net)
}

#' restrict a unipartite slice to the union of from and to actors
#'
#' keeps rows and columns for every actor in \code{from} or \code{to}
#' (in original order) and sets cells outside \code{from} x \code{to}
#' to NA. for symmetric networks the transposed block is kept as well.
#' extra dimensions (layers, time) are carried through unchanged.
#'
#' @param arr matrix or array with actors on the first two dimensions
#' @param from row actor selection passed to \code{process_actor_selection}
#' @param to column actor selection passed to \code{process_actor_selection}
#' @param symmetric logical, keep the transposed block as well
#'
#' @return matrix or array over the union of selected actors
#'
#' @keywords internal
#' @noRd
mask_from_to <- function(arr, from, to, symmetric = FALSE) {
	actor_names <- dimnames(arr)[[1]]
	n_actors <- length(actor_names)
	from_idx <- process_actor_selection(from, actor_names, n_actors)
	to_idx <- process_actor_selection(to, actor_names, n_actors)
	keep <- sort(unique(c(from_idx, to_idx)))

	# mark cells between from and to actors
	in_block <- matrix(FALSE, n_actors, n_actors)
	in_block[from_idx, to_idx] <- TRUE
	if (symmetric) {
		in_block <- in_block | t(in_block)
	}
	in_block <- in_block[keep, keep, drop = FALSE]

	# subset the actor dimensions and keep the rest
	extra_dims <- rep(list(TRUE), length(dim(arr)) - 2L)
	out <- do.call(
		`[`,
		c(list(arr, keep, keep), extra_dims, list(drop = FALSE))
	)
	out[rep(!as.vector(in_block), length.out = length(out))] <- NA
	out
}
