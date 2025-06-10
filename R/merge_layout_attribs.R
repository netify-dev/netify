#' Merge netlet attributes with layout information
#'
#' This function organizes `netlet` data by integrating x and y positions from `nodes_list` and
#' connection data from `edges_list` into the node and edge data structures returned by
#' `decompose_netify`. It supports both cross-sectional and longitudinal data formats.
#'
#' @param netlet A `netify` object containing the network data.
#' @param nodes_list A list of data frames (or a single data frame for cross-sectional data) containing
#'        node positions with columns `actor`, `x`, and `y`. For longitudinal data, each list element
#'        corresponds to a different time slice.
#' @param edges_list A list of data frames (or a single data frame for cross-sectional data) detailing
#'        the connections between nodes. Each data frame should include at least the columns `from`, `to`,
#'        and for longitudinal data, `time`.
#'
#' @return A list containing two data frames: `nodal_data` and `edge_data`. Each data frame incorporates
#'         the corresponding node or edge data along with their spatial coordinates (and time indices for
#'         longitudinal data).
#' 
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

merge_layout_attribs <- function(netlet, nodes_list, edges_list) {

	# check if netify object
	netify_check(netlet)

	# pull out attrs
	obj_attrs <- attributes(netlet)

	# organize dv, dyadic, and nodal attributes in netlet
	net_dfs <- decompose_netify(netlet)

	# cross-sectional case
	if (obj_attrs$netify_type == 'cross_sec') {
		# pull out nodes and edges from list format
		nodes <- nodes_list[[1]]
		edges <- edges_list[[1]]
		
		# use match for nodal data
		node_match <- match(net_dfs$nodal_data$name, nodes$actor)
		net_dfs$nodal_data$x <- nodes$x[node_match]
		net_dfs$nodal_data$y <- nodes$y[node_match]
		
		# use match for edge data - create composite key
		edge_key_df <- paste(net_dfs$edge_data$from, net_dfs$edge_data$to, sep = "__|__")
		edge_key_layout <- paste(edges$from, edges$to, sep = "__|__")
		edge_match <- match(edge_key_df, edge_key_layout)
		
		# assign coordinates
		net_dfs$edge_data$x1 <- edges$x1[edge_match]
		net_dfs$edge_data$y1 <- edges$y1[edge_match]
		net_dfs$edge_data$x2 <- edges$x2[edge_match]
		net_dfs$edge_data$y2 <- edges$y2[edge_match]
		
	} else {
		# longitudinal case
		ego_netlet <- obj_attrs$ego_netlet
		
		# get time names
		if (!is.null(ego_netlet)) {
			list_names <- names(netlet)
			list_names <- sub("^[^_]+__", "", list_names)
		} else {
			list_names <- names(nodes_list)
		}
		
		# combine nodes more efficiently using lapply and do.call
		nodes_with_time <- lapply(seq_along(nodes_list), function(i) {
			nodes_i <- nodes_list[[i]]
			if (nrow(nodes_i) > 0) {
				nodes_i$time <- list_names[i]
				nodes_i
			}
		})
		# remove null entries and combine
		nodes <- do.call(rbind, nodes_with_time[!sapply(nodes_with_time, is.null)])
		
		# combine edges similarly
		edges_with_time <- lapply(seq_along(edges_list), function(i) {
			edges_i <- edges_list[[i]]
			if (!is.null(edges_i) && nrow(edges_i) > 0) {
				edges_i$time <- list_names[i]
				edges_i
			}
		})
		# remove null entries and combine
		edges_with_time <- edges_with_time[!sapply(edges_with_time, is.null)]
		
		if (length(edges_with_time) > 0) {
			edges <- do.call(rbind, edges_with_time)
		} else {
			edges <- data.frame(
				from = character(0), to = character(0), time = character(0),
				x1 = numeric(0), y1 = numeric(0), x2 = numeric(0), y2 = numeric(0),
				stringsAsFactors = FALSE
			)
		}
		
		# create node lookup tables
		node_key_layout <- paste(nodes$actor, nodes$time, sep = "__|__")
		x_lookup <- setNames(nodes$x, node_key_layout)
		y_lookup <- setNames(nodes$y, node_key_layout)
		
		# match nodal data
		node_key_df <- paste(net_dfs$nodal_data$name, net_dfs$nodal_data$time, sep = "__|__")
		net_dfs$nodal_data$x <- x_lookup[node_key_df]
		net_dfs$nodal_data$y <- y_lookup[node_key_df]
		
		# create edge lookup tables
		if (nrow(edges) > 0) {
			edge_key_layout <- paste(edges$from, edges$to, edges$time, sep = "__|__")
			x1_lookup <- setNames(edges$x1, edge_key_layout)
			y1_lookup <- setNames(edges$y1, edge_key_layout)
			x2_lookup <- setNames(edges$x2, edge_key_layout)
			y2_lookup <- setNames(edges$y2, edge_key_layout)
			
			# match edge data
			edge_key_df <- paste(net_dfs$edge_data$from, net_dfs$edge_data$to, 
										net_dfs$edge_data$time, sep = "__|__")
			net_dfs$edge_data$x1 <- x1_lookup[edge_key_df]
			net_dfs$edge_data$y1 <- y1_lookup[edge_key_df]
			net_dfs$edge_data$x2 <- x2_lookup[edge_key_df]
			net_dfs$edge_data$y2 <- y2_lookup[edge_key_df]
		}
	}

	#
	return(net_dfs)
}
