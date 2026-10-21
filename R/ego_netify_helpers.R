#' Get Neighborhood Network for Ego
#'
#' `get_ngbd_net_for_ego` extracts the neighborhood network for a specified ego from a given list of raw networks. it allows the user to define the neighborhood based on outgoing, incoming, or any relationship, and includes a threshold for weighted networks.
#'
#' @param raw_net a list of raw network matrices.
#' @param ego a character vector specifying the name of the ego for whom to create the neighborhood network.
#' @param threshold a numeric vector specifying the threshold for including alters in the neighborhood network. default for unweighted networks is 0 and default for weighted networks is the average edge weight.
#' @param include_ego logical; if TRUE, the ego node will be included in the neighborhood network. default is TRUE.
#' @param ngbd_direction a character string specifying the type of relationship to consider for directed networks. options are "out" for outgoing ties, "in" for incoming ties, and "any" for any relationship. default is "any".
#'
#' @return a list of neighborhood networks, each represented as a matrix.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

get_ngbd_net_for_ego <- function(
	raw_net, ego, threshold,
	include_ego = TRUE, ngbd_direction = "any") {
	####
	# iterate through nets to construct ngbds
	ngbd_actors <- lapply(1:length(raw_net), function(ii) {
		# get iith net and thresh
		net <- raw_net[[ii]]
		thresh <- threshold[ii]

		# for the ego, figure out what actors have connections
		ngbd_row <- rownames(net)[which(net[ego, ] > thresh)]
		ngbd_col <- colnames(net)[which(net[, ego] > thresh)]
		ngbd_any <- unique(c(ngbd_row, ngbd_col))

		# add back ego if include_ego is true
		if (include_ego) {
			ngbd_row <- c(ego, ngbd_row)
			ngbd_col <- c(ego, ngbd_col)
			ngbd_any <- c(ego, ngbd_any)
		}

		# construct ngbd net based on ngbd_direction
		#use drop=false to maintain matrix structure even for 1x1 matrices
		if (ngbd_direction == "out") {
			ngbd_net <- net[ngbd_row, ngbd_row, drop = FALSE]
		}
		if (ngbd_direction == "in") {
			ngbd_net <- net[ngbd_col, ngbd_col, drop = FALSE]
		}
		if (ngbd_direction == "any") {
			ngbd_net <- net[ngbd_any, ngbd_any, drop = FALSE]
		}

		return(ngbd_net)
	})
	####

	####
	# add names, keeping period labels for longitudinal input
	if (!is.null(names(raw_net))) {
		names(ngbd_actors) <- names(raw_net)
	} else {
		names(ngbd_actors) <- ego
	}
	####

	####
	#
	return(ngbd_actors)
	####
}

#' subset netlet attributes to match ego networks
#'
#' filters actor_pds, nodal_data, and dyad_data so they only describe the
#' actors and periods retained in the ego networks.
#'
#' @param attrs list of netlet attributes to update.
#' @param ego_nets list of ego network matrices, named by period for
#'   longitudinal input.
#' @param longitudinal logical, whether the ego networks are longitudinal.
#' @param time_labels time labels of the original netlet.
#'
#' @return the updated attribute list.
#'
#' @keywords internal
#' @noRd
subset_ego_attrs <- function(attrs, ego_nets, longitudinal, time_labels = NULL) {
	actor_sets <- lapply(ego_nets, rownames)
	ego_actors <- unique(unlist(actor_sets, use.names = FALSE))
	pd_names <- names(ego_nets)

	# keep actors in the ego networks, clipped to the periods they appear in
	if (!is.null(attrs$actor_pds)) {
		actor_pds <- attrs$actor_pds
		actor_pds <- actor_pds[actor_pds$actor %in% ego_actors, , drop = FALSE]
		if (longitudinal && nrow(actor_pds) > 0L) {
			actor_pds <- do.call(rbind, lapply(seq_len(nrow(actor_pds)), function(ii) {
				in_pd <- vapply(actor_sets, function(pd_actors) {
					actor_pds$actor[ii] %in% pd_actors
				}, logical(1))
				clip_actor_pds_to_time(
					actor_pds[ii, , drop = FALSE],
					orig_labels = time_labels,
					kept_labels = pd_names[in_pd]
				)
			}))
		}
		rownames(actor_pds) <- NULL
		attrs$actor_pds <- actor_pds
	}

	# keep nodal rows for actors present in each ego network
	if (!is.null(attrs$nodal_data)) {
		nodal_data <- attrs$nodal_data
		if (longitudinal && "time" %in% names(nodal_data)) {
			ego_keys <- unlist(lapply(pd_names, function(pd) {
				paste(actor_sets[[pd]], pd, sep = "\r")
			}), use.names = FALSE)
			node_keys <- paste(nodal_data$actor, as.character(nodal_data$time), sep = "\r")
			keep_rows <- node_keys %in% ego_keys
		} else {
			keep_rows <- nodal_data$actor %in% ego_actors
		}
		attrs$nodal_data <- nodal_data[keep_rows, , drop = FALSE]
	}

	# keep dyad matrices for retained periods and ego network actors
	if (!is.null(attrs$dyad_data)) {
		dyad_data <- attrs$dyad_data
		if (longitudinal) {
			dyad_pds <- intersect(pd_names, names(dyad_data))
			net_pds <- dyad_pds
		} else {
			dyad_pds <- names(dyad_data)[1]
			net_pds <- 1L
		}
		dyad_data <- lapply(seq_along(dyad_pds), function(ii) {
			pd_actors <- rownames(ego_nets[[net_pds[ii]]])
			lapply(dyad_data[[dyad_pds[ii]]], function(var_matrix) {
				keep_rows <- pd_actors[pd_actors %in% rownames(var_matrix)]
				keep_cols <- pd_actors[pd_actors %in% colnames(var_matrix)]
				var_matrix[keep_rows, keep_cols, drop = FALSE]
			})
		})
		names(dyad_data) <- dyad_pds
		attrs$dyad_data <- dyad_data
	}

	# record whether ego networks share one actor set over time
	if (longitudinal) {
		attrs$actor_time_uniform <- actor_sets_uniform(ego_nets)
	}

	attrs
}

#' speedy extraction of time from ego-time concatenated strings
#' @keywords internal
#' @noRd
extract_ego_time <- function(ego_time_vec) {
	sub("^[^_]+__", "", ego_time_vec)
}

#' fast extraction of ego from ego-time concatenated strings
#' @keywords internal
#' @noRd
extract_ego_name <- function(ego_time_vec) {
	# extract everything before "__"
	sub("__.*$", "", ego_time_vec)
}

#' split ego-time string into components
#' @keywords internal
#' @noRd
split_ego_time <- function(ego_time_vec) {
	out <- data.frame(
		ego = sub("__.*$", "", ego_time_vec),
		time = sub("^[^_]+__", "", ego_time_vec),
		stringsAsFactors = FALSE
	)

	#
	return(out)
}
