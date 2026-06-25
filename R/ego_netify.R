#' Create ego network from a netify object
#'
#' `ego_netify` extracts an ego network from a
#' netify object. an ego network consists of a focal node (ego) and its immediate
#' neighbors (alters). for weighted networks, users can define neighborhoods using
#' edge weight thresholds. the function returns a netify object representing the
#' ego network.
#'
#' @param netlet a netify object (class "netify") from which to extract the ego network.
#' @param ego character string specifying the name of the ego for whom to
#'   create the ego network. must match an actor name in the netify object.
#' @param threshold numeric value or vector specifying the threshold for including
#'   alters in the ego network based on edge weights. for longitudinal networks,
#'   can be a vector with length equal to the number of time periods to apply
#'   different thresholds over time. if NULL (default), uses 0 for unweighted
#'   networks and the mean edge weight for weighted networks.
#' @param ngbd_direction character string specifying which neighbors to include
#'   for directed networks. options are:
#'   \itemize{
#'     \item \code{"out"}: include alters that ego has outgoing ties to
#'     \item \code{"in"}: include alters that ego has incoming ties from
#'     \item \code{"any"}: include alters with any tie to/from ego (default)
#'   }
#' @param include_ego logical. if TRUE (default), the ego node is included in
#'   the ego network. if FALSE, only alters are included.
#'
#' @return a netify object representing the ego network. for longitudinal networks,
#'   returns a list of netify objects with one ego network per time period.
#'
#'   each returned netify object includes additional attributes:
#'   \itemize{
#'     \item \code{ego_netify}: TRUE (indicator that this is an ego network)
#'     \item \code{ego_id}: identifier of the ego
#'     \item \code{threshold}: threshold value(s) used
#'     \item \code{ngbd_direction}: direction specification used
#'     \item \code{include_ego}: whether ego was included
#'   }
#'
#' @details
#' the function extracts an ego network by identifying all nodes connected to the
#' specified ego based on the given criteria:
#'
#' \strong{neighborhood definition:}
#' \itemize{
#'   \item for unweighted networks: all nodes with edges to/from ego (threshold = 0)
#'   \item for weighted networks: all nodes with edge weights exceeding the threshold
#'   \item direction matters only for directed networks (controlled by ngbd_direction)
#' }
#'
#' \strong{threshold behavior:}
#' \itemize{
#'   \item if not specified, defaults to 0 for unweighted networks
#'   \item if not specified for weighted networks, uses the mean edge weight
#'   \item for longitudinal networks, can vary by time period if a vector is provided
#'   \item edges with weights > threshold are included (not >=)
#' }
#'
#' \strong{output structure:}
#'
#' the function preserves all attributes from the original netify object, including
#' nodal and dyadic variables, but subsets them to include only ego and its neighbors.
#' for longitudinal networks, ego networks may vary in composition across time periods
#' as relationships change.
#'
#' \strong{limitations:}
#' \itemize{
#'   \item currently does not support multilayer networks
#'   \item currently does not support bipartite networks
#' }
#'
#' @note
#' to create ego networks for multiple egos, use \code{lapply} or a loop to call
#' this function for each ego separately.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @examples
#' # cross-sectional ego network from the bundled classroom data
#' data(classroom_edges)
#' data(classroom_nodes)
#' net <- netify(
#'     classroom_edges,
#'     actor1 = "from", actor2 = "to",
#'     symmetric = TRUE,
#'     nodal_data = classroom_nodes
#' )
#' s07_ego <- ego_netify(net, ego = "s07")
#' print(s07_ego)
#'
#' \donttest{
#' # longitudinal ego network with a weighted, directed netlet
#' data(icews)
#' netlet <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     weight = "verbCoop"
#' )
#' pakistan_ego <- ego_netify(netlet, ego = "Pakistan")
#' summary(pakistan_ego)
#' }
#'
#' @export ego_netify

ego_netify <- function(
	netlet, ego, threshold = NULL,
	ngbd_direction = "any", include_ego = TRUE) {
	####
	# check if netify object
	netify_check(netlet)

	# convert longit array to longit list since ego sets vary over time
	if (attr(netlet, "netify_type") == "longit_array") {
		netlet <- array_to_list(netlet, preserveAttr = TRUE)
	}

	# pull out attrs and msrmnts of original
	obj_attrs <- attributes(netlet)
	msrmnts <- netify_measurements(netlet)

	# determine if multilayer and/or longitudinal
	weighted <- !obj_attrs$is_binary
	multilayer <- ifelse(length(obj_attrs$layers) > 1, TRUE, FALSE)
	longitudinal <- ifelse(obj_attrs$netify_type != "cross_sec", TRUE, FALSE)
	bipartite <- ifelse(obj_attrs$mode == "bipartite", TRUE, FALSE)

	# get netlet type
	netlet_type <- obj_attrs$netify_type
	####

	####
	# stop if multilayer and say we dont support
	if (multilayer) {
		cli::cli_abort(
			"This object has multiple layers.
			`ego_netify` does not currently support multilayer `netify` inputs.
			Please use the `subset_netify` function to create a `netify` object with a single layer."
		)
	}

	# stop if bipartite and say we dont support
	if (bipartite) {
		cli::cli_abort(
			"This object is bipartite.
			`ego_netify` does not currently support bipartite `netify` inputs.
			Please use the `subset_netify` function to create a `netify` object with a single layer."
		)
	}
	####

	####
	# check if the ego is a single character value
	if (!is.character(ego) || length(ego) != 1) {
		cli::cli_abort("`ego` must be a single character value.")
	}

	# check if the ego is in the network
	poss_actors <- unique(obj_attrs$actor_pds$actor)
	if (!ego %in% poss_actors) {
		cli::cli_abort(
			paste0(
				'The ego "', ego, '" is not in the data.'
			)
		)
	}

	# add check for ngbd_direction
	if (!ngbd_direction %in% c("out", "in", "any")) {
		cli::cli_abort(
			'`ngbd_direction` must be one of "out", "in", or "any".'
		)
	}
	####

	####
	# if supplied check that threshold is numeric
	if (!is.null(threshold) && !is.numeric(threshold)) {
		cli::cli_abort("`threshold` argument must be a numeric input.")
	}
	####

	####
	# get raw network
	raw_net <- get_raw(netlet)

	# if cross sec convert to a list object so that
	# we can use lapply
	if (netlet_type == "cross_sec") {
		raw_net <- list(raw_net)
	}

	# drop periods where the ego does not exist in the slice and inform the user
	ego_dropped_periods <- character(0)
	if (longitudinal) {
		ego_in_slice <- vapply(raw_net, function(net) {
			ego %in% rownames(net) || ego %in% colnames(net)
		}, logical(1))
		if (any(!ego_in_slice)) {
			ego_dropped_periods <- names(raw_net)[!ego_in_slice]
			raw_net <- raw_net[ego_in_slice]
		}
		if (length(raw_net) == 0) {
			cli::cli_abort(c(
				"!" = paste0("Ego '", ego, "' is not present in any time period of the netlet."),
				"i" = "Check `get_actor_time_info(netlet)` for this actor's entry / exit window."
			))
		}
		if (length(ego_dropped_periods) > 0) {
			cli::cli_alert_info(paste0(
				"Ego '", ego, "' not present in ", length(ego_dropped_periods),
				" time period(s); dropping: ",
				paste(ego_dropped_periods, collapse = ", "), "."
			))
		}
	}
	####

	####
	# if unweighted then define neighborhood using threshold of zero
	if (!weighted) {
		threshold <- rep(0, length(raw_net))
	}

	# if weighted then define neighborhood using mean of edge weights
	# if threshold is not supplied
	if (weighted) {
		# recycle scalar threshold across time periods if needed
		if (!is.null(threshold) && longitudinal) {
			if (length(threshold) != length(raw_net)) {
				threshold <- rep(threshold, length(raw_net))
			}
		}

		# default threshold to per-slice mean edge weight
		if (is.null(threshold)) {
			threshold <- unlist(
				lapply(raw_net, function(net) {
					mean(c(net), na.rm = TRUE)
				})
			)
		}
	}
	####

	####
	# define neighborhood for ego
	# based on thresh get vector of actors to keep by list element
	ego_nets <- get_ngbd_net_for_ego(
		raw_net, ego, threshold, include_ego, ngbd_direction
	)

	# check if ego has no neighbors in any time period and provide informative message
	if (longitudinal) {
		# for longitudinal networks, check each time period
		isolated_periods <- sapply(ego_nets, function(net) {
			n_actors <- ifelse(is.null(dim(net)), 1, nrow(net))
			return(n_actors == 1 && include_ego)
		})

		if (any(isolated_periods)) {
			time_labels <- names(raw_net)[isolated_periods]
			if (length(time_labels) == length(raw_net)) {
				cli::cli_alert_info(
					paste0("Ego '", ego, "' has no neighbors meeting the threshold in any time period.")
				)
			} else {
				cli::cli_alert_info(
					paste0("Ego '", ego, "' has no neighbors meeting the threshold in ",
						   length(time_labels), " time period(s): ",
						   paste(time_labels, collapse = ", "))
				)
			}
		}
	} else {
		# for cross-sectional networks
		n_actors <- ifelse(is.null(dim(ego_nets[[1]])), 1, nrow(ego_nets[[1]]))
		if (n_actors == 1 && include_ego) {
			cli::cli_alert_info(
				paste0("Ego '", ego, "' has no neighbors meeting the threshold (",
					   round(threshold[1], 3), "). ",
					   "The ego network contains only the ego node.")
			)
		} else if (n_actors == 0) {
			cli::cli_alert_warning(
				paste0("Ego '", ego, "' has no neighbors and ego was not included. ",
					   "The resulting network is empty.")
			)
		}
	}
	####

	####
	# add back in netify attributes

	# if cross sec then just work with the single network
	if (netlet_type == "cross_sec") {
		ego_net <- ego_nets[[1]]

		# create new attributes based on original
		new_attrs <- obj_attrs
		new_attrs$ego_netify <- TRUE
		new_attrs$threshold <- threshold
		new_attrs$ngbd_direction <- ngbd_direction
		new_attrs$include_ego <- include_ego
		new_attrs$ego_id <- ego
		new_attrs$ego_entry <- ego

		# update dim and dimnames
		new_attrs[c("dim", "dimnames")] <- attributes(ego_net)[c("dim", "dimnames")]

		# apply attributes
		attributes(ego_net) <- new_attrs

		return(ego_net)
	}

	# if longitudinal, process list of networks
	if (netlet_type == "longit_list") {
		# process each time period
		ego_nets_processed <- lapply(1:length(ego_nets), function(ii) {
			net <- ego_nets[[ii]]
			time_name <- names(ego_nets)[ii]

			# create attributes for this time period
			subobj_attrs <- attributes(netlet[[1]])
			subobj_attrs$ego_netify <- TRUE
			subobj_attrs$threshold <- threshold[ii]
			subobj_attrs$ngbd_direction <- ngbd_direction
			subobj_attrs$include_ego <- include_ego
			subobj_attrs$ego_id <- ego # just the ego name, not ego: time
			subobj_attrs$ego_entry <- ego

			# update dim and dimnames
			subobj_attrs[c("dim", "dimnames")] <- attributes(net)[c("dim", "dimnames")]

			# apply attributes
			attributes(net) <- subobj_attrs
			return(net)
		})

		# add names - just time periods like regular netify objects
		names(ego_nets_processed) <- names(ego_nets)

		# add top-level attributes
		list_attrs <- obj_attrs
		list_attrs$ego_netify <- TRUE
		list_attrs$threshold <- threshold
		list_attrs$ngbd_direction <- ngbd_direction
		list_attrs$include_ego <- include_ego
		list_attrs$ego_longit <- TRUE
		list_attrs$ego_entry <- ego
		list_attrs$ego_id <- ego # just the ego name
		list_attrs$names <- names(ego_nets)

		# apply attributes to list
		attributes(ego_nets_processed) <- list_attrs

		return(ego_nets_processed)
	}
	####
}
