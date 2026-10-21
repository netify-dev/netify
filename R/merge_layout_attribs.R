#' Merge netlet attributes with layout information
#'
#' this function organizes `netlet` data by integrating x and y positions from `nodes_list` and
#' connection data from `edges_list` into the node and edge data structures returned by
#' `decompose_netify`. it supports both cross-sectional and longitudinal data formats.
#'
#' @param netlet a `netify` object containing the network data.
#' @param nodes_list a list of data frames (or a single data frame for cross-sectional data) containing
#'        node positions with columns `actor`, `x`, and `y`. for longitudinal data, each list element
#'        corresponds to a different time slice.
#' @param edges_list a list of data frames (or a single data frame for cross-sectional data) detailing
#'        the connections between nodes. each data frame should include at least the columns `from`, `to`,
#'        and for longitudinal data, `time`.
#'
#' @return a list containing two data frames: `nodal_data` and `edge_data`. each data frame incorporates
#'         the corresponding node or edge data along with their spatial coordinates (and time indices for
#'         longitudinal data).
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

merge_layout_attribs <- function(netlet, nodes_list, edges_list) {
	# check if the input is a valid netify object
	netify_check(netlet)

	# extract attributes from the netify object
	obj_attrs <- attributes(netlet)
	is_cross_sec <- obj_attrs$netify_type == "cross_sec"
	is_symmetric <- isTRUE(all(obj_attrs$symmetric))

	# decompose the netify object into its components
	net_dfs <- decompose_netify(netlet)

	# wrap single layout data frames in a list
	if (is.data.frame(nodes_list)) {
		nodes_list <- list(nodes_list)
	}
	if (is.data.frame(edges_list)) {
		edges_list <- list(edges_list)
	}

	# determine time slice names
	if (is_cross_sec) {
		list_names <- rep("", length(nodes_list))
	} else if (!is.null(obj_attrs$ego_netlet)) {
		list_names <- sub("^[^_]+__", "", names(netlet))
	} else {
		list_names <- names(nodes_list)
	}

	# keep one row per unordered pair for undirected networks
	if (is_symmetric && nrow(net_dfs$edge_data) > 0) {
		pair_key <- paste(
			pmin(net_dfs$edge_data$from, net_dfs$edge_data$to),
			pmax(net_dfs$edge_data$from, net_dfs$edge_data$to),
			net_dfs$edge_data$time,
			sep = "__|__"
		)
		net_dfs$edge_data <- net_dfs$edge_data[!duplicated(pair_key), , drop = FALSE]
		rownames(net_dfs$edge_data) <- NULL
	}

	# split bipartite actors that appear in both modes into row and col nodes
	bip_modes <- bipartite_overlap_modes(netlet, list_names)
	if (!is.null(bip_modes)) {
		net_dfs$nodal_data <- split_bipartite_nodal_data(
			net_dfs$nodal_data,
			bip_modes,
			is_cross_sec
		)
	}

	# layout ids for nodes and edge endpoints
	node_time <- if (is_cross_sec) "" else net_dfs$nodal_data$time
	edge_time <- if (is_cross_sec) "" else net_dfs$edge_data$time
	node_layout_name <- net_dfs$nodal_data$name
	edge_from_name <- net_dfs$edge_data$from
	edge_to_name <- net_dfs$edge_data$to
	if (!is.null(bip_modes)) {
		prefixed_times <- bip_modes$time[bip_modes$overlap]
		node_prefixed <- node_time %in% prefixed_times & !is.na(net_dfs$nodal_data$actor_mode)
		node_layout_name[node_prefixed] <- paste0(
			net_dfs$nodal_data$actor_mode[node_prefixed],
			":",
			node_layout_name[node_prefixed]
		)
		edge_prefixed <- edge_time %in% prefixed_times
		edge_from_name[edge_prefixed] <- paste0("row:", edge_from_name[edge_prefixed])
		edge_to_name[edge_prefixed] <- paste0("col:", edge_to_name[edge_prefixed])
	}

	# combine layout nodes into a single data frame with time information
	nodes_combined <- do.call(rbind, lapply(seq_along(nodes_list), function(i) {
		nodes_i <- nodes_list[[i]]
		if (!is.null(nodes_i) && nrow(nodes_i) > 0) {
			data.frame(
				actor = as.character(nodes_i$actor),
				x = nodes_i$x,
				y = nodes_i$y,
				time = list_names[i],
				stringsAsFactors = FALSE
			)
		}
	}))
	if (is.null(nodes_combined)) {
		nodes_combined <- data.frame(
			actor = character(0),
			x = numeric(0),
			y = numeric(0),
			time = character(0),
			stringsAsFactors = FALSE
		)
	}

	# assign node coordinates
	node_key_layout <- paste(nodes_combined$actor, nodes_combined$time, sep = "__|__")
	node_x_lookup <- setNames(nodes_combined$x, node_key_layout)
	node_y_lookup <- setNames(nodes_combined$y, node_key_layout)
	node_key_df <- paste(node_layout_name, node_time, sep = "__|__")
	net_dfs$nodal_data$x <- unname(node_x_lookup[node_key_df])
	net_dfs$nodal_data$y <- unname(node_y_lookup[node_key_df])

	# fall back to plain actor names for user layouts without mode prefixes
	if (!is.null(bip_modes)) {
		missing_xy <- is.na(net_dfs$nodal_data$x)
		plain_key <- paste(net_dfs$nodal_data$name, node_time, sep = "__|__")[missing_xy]
		net_dfs$nodal_data$x[missing_xy] <- unname(node_x_lookup[plain_key])
		net_dfs$nodal_data$y[missing_xy] <- unname(node_y_lookup[plain_key])
	}

	# combine layout edges into a single data frame with time information
	edges_combined <- do.call(rbind, lapply(seq_along(edges_list), function(i) {
		edges_i <- edges_list[[i]]
		if (!is.null(edges_i) && nrow(edges_i) > 0) {
			edges_i$time <- list_names[i]
			edges_i
		}
	}))

	# assign edge coordinates if both edges and edge data exist
	if (!is.null(edges_combined) && nrow(edges_combined) > 0 && nrow(net_dfs$edge_data) > 0) {
		edge_key_layout <- paste(
			edges_combined$from,
			edges_combined$to,
			edges_combined$time,
			sep = "__|__"
		)
		x1 <- edges_combined$x1
		y1 <- edges_combined$y1
		x2 <- edges_combined$x2
		y2 <- edges_combined$y2

		# index both directions for undirected nets
		if (is_symmetric) {
			edge_key_layout <- c(
				edge_key_layout,
				paste(edges_combined$to, edges_combined$from, edges_combined$time, sep = "__|__")
			)
			x_from <- c(x1, x2)
			y_from <- c(y1, y2)
			x2 <- c(x2, x1)
			y2 <- c(y2, y1)
			x1 <- x_from
			y1 <- y_from
		}

		edge_key_df <- paste(edge_from_name, edge_to_name, edge_time, sep = "__|__")
		key_idx <- match(edge_key_df, edge_key_layout)
		net_dfs$edge_data$x1 <- x1[key_idx]
		net_dfs$edge_data$y1 <- y1[key_idx]
		net_dfs$edge_data$x2 <- x2[key_idx]
		net_dfs$edge_data$y2 <- y2[key_idx]
	} else {
		no_coords <- rep(NA_real_, nrow(net_dfs$edge_data))
		net_dfs$edge_data$x1 <- no_coords
		net_dfs$edge_data$y1 <- no_coords
		net_dfs$edge_data$x2 <- no_coords
		net_dfs$edge_data$y2 <- no_coords
	}

	# return network data frames
	return(net_dfs)
}

#' find bipartite actors that appear in both modes
#'
#' @param netlet a netify object.
#' @param list_names time labels aligned with the layout slices.
#'
#' @return NULL when the network is not bipartite or no actor appears as both
#'   a row and a column actor; otherwise a list with the time labels, a logical
#'   `overlap` flag per period, and the row and column actors per period.
#'
#' @keywords internal
#' @noRd

bipartite_overlap_modes <- function(netlet, list_names) {
	obj_attrs <- attributes(netlet)
	if (!identical(obj_attrs$mode, "bipartite")) {
		return(NULL)
	}

	# row and col actors for each period
	raw <- get_raw(netlet)
	mode_actors <- switch(obj_attrs$netify_type,
		cross_sec = list(dimnames(raw)[1:2]),
		longit_array = rep(list(dimnames(raw)[1:2]), dim(raw)[3]),
		longit_list = lapply(raw, function(m) dimnames(m)[1:2])
	)
	if (length(mode_actors) != length(list_names)) {
		return(NULL)
	}

	row_actors <- lapply(mode_actors, function(dn) as.character(dn[[1]]))
	col_actors <- lapply(mode_actors, function(dn) as.character(dn[[2]]))
	overlap <- vapply(
		seq_along(mode_actors),
		function(i) length(intersect(row_actors[[i]], col_actors[[i]])) > 0,
		logical(1)
	)
	if (!any(overlap)) {
		return(NULL)
	}

	list(
		time = list_names,
		overlap = overlap,
		row = row_actors,
		col = col_actors
	)
}

#' give bipartite actors one nodal row per mode
#'
#' @param nodal_data nodal data from `decompose_netify()`.
#' @param bip_modes output of `bipartite_overlap_modes()`.
#' @param is_cross_sec logical, whether the network is cross-sectional.
#'
#' @return nodal data with an `actor_mode` column ("row" or "col") where actors
#'   appearing in both modes have one row for each mode.
#'
#' @keywords internal
#' @noRd

split_bipartite_nodal_data <- function(nodal_data, bip_modes, is_cross_sec) {
	tag_rows <- function(df, mode_label) {
		df$actor_mode <- rep(mode_label, nrow(df))
		df
	}

	pieces <- lapply(seq_along(bip_modes$time), function(i) {
		if (is_cross_sec) {
			nd_t <- nodal_data
		} else {
			nd_t <- nodal_data[nodal_data$time == bip_modes$time[i], , drop = FALSE]
		}
		in_row <- nd_t$name %in% bip_modes$row[[i]]
		in_col <- nd_t$name %in% bip_modes$col[[i]]
		rbind(
			tag_rows(nd_t[in_row, , drop = FALSE], "row"),
			tag_rows(nd_t[in_col, , drop = FALSE], "col"),
			tag_rows(nd_t[!in_row & !in_col, , drop = FALSE], NA_character_)
		)
	})

	# keep rows from periods without a layout slice
	if (!is_cross_sec) {
		other_times <- !(nodal_data$time %in% bip_modes$time)
		pieces <- c(pieces, list(tag_rows(nodal_data[other_times, , drop = FALSE], NA_character_)))
	}

	out <- do.call(rbind, pieces)
	rownames(out) <- NULL
	out
}
