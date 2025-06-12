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

	# check if the input is a valid netify object
	netify_check(netlet)

	# extract attributes from the netify object
	obj_attrs <- attributes(netlet)

	# decompose the netify object into its components
	net_dfs <- decompose_netify(netlet)

	# handle cross-sectional data
	if (obj_attrs$netify_type == 'cross_sec') {
		# extract nodes and edges from the input lists
		nodes <- nodes_list[[1]]
		edges <- edges_list[[1]]
		
		# create lookup tables for node coordinates
		node_x_lookup <- setNames(nodes$x, nodes$actor)
		node_y_lookup <- setNames(nodes$y, nodes$actor)
		
		# assign x and y coordinates to nodes using vectorized lookup
		net_dfs$nodal_data$x <- node_x_lookup[net_dfs$nodal_data$name]
		net_dfs$nodal_data$y <- node_y_lookup[net_dfs$nodal_data$name]
		
		# create lookup tables for edge coordinates using composite keys
		edge_key_layout <- paste(edges$from, edges$to, sep = "__|__")
		edge_x1_lookup <- setNames(edges$x1, edge_key_layout)
		edge_y1_lookup <- setNames(edges$y1, edge_key_layout)
		edge_x2_lookup <- setNames(edges$x2, edge_key_layout)
		edge_y2_lookup <- setNames(edges$y2, edge_key_layout)
		
		# generate composite keys for edges in the data frame
		edge_key_df <- paste(net_dfs$edge_data$from, net_dfs$edge_data$to, sep = "__|__")
		
		# assign edge coordinates using vectorized lookup
		net_dfs$edge_data$x1 <- edge_x1_lookup[edge_key_df]
		net_dfs$edge_data$y1 <- edge_y1_lookup[edge_key_df]
		net_dfs$edge_data$x2 <- edge_x2_lookup[edge_key_df]
		net_dfs$edge_data$y2 <- edge_y2_lookup[edge_key_df]
		
	} else {
		# handle longitudinal data
		ego_netlet <- obj_attrs$ego_netlet
		
		# determine time slice names
		if (!is.null(ego_netlet)) {
			list_names <- names(netlet)
			list_names <- sub("^[^_]+__", "", list_names)
		} else {
			list_names <- names(nodes_list)
		}
		
		# combine all nodes into a single data frame with time information
		nodes_combined <- do.call(rbind, lapply(seq_along(nodes_list), function(i) {
			nodes_i <- nodes_list[[i]]
			if (nrow(nodes_i) > 0) {
				nodes_i$time <- list_names[i]
				nodes_i
			}
		}))
		
		# create lookup tables for node coordinates with composite keys
		node_key <- paste(nodes_combined$actor, nodes_combined$time, sep = "__|__")
		node_x_lookup <- setNames(nodes_combined$x, node_key)
		node_y_lookup <- setNames(nodes_combined$y, node_key)
		
		# combine all edges into a single data frame with time information
		edges_combined <- do.call(rbind, lapply(seq_along(edges_list), function(i) {
			edges_i <- edges_list[[i]]
			if (!is.null(edges_i) && nrow(edges_i) > 0) {
				edges_i$time <- list_names[i]
				edges_i
			}
		}))
		
		# handle the case where there are no edges
		if (is.null(edges_combined) || nrow(edges_combined) == 0) {
			edges_combined <- data.frame(
				from = character(0), to = character(0), time = character(0),
				x1 = numeric(0), y1 = numeric(0), x2 = numeric(0), y2 = numeric(0),
				stringsAsFactors = FALSE
			)
		}
		
		# assign x and y coordinates to nodes using vectorized lookup
		node_key_df <- paste(net_dfs$nodal_data$name, net_dfs$nodal_data$time, sep = "__|__")
		net_dfs$nodal_data$x <- node_x_lookup[node_key_df]
		net_dfs$nodal_data$y <- node_y_lookup[node_key_df]
		
		# assign edge coordinates if both edges and edge data exist
		if (nrow(edges_combined) > 0 && nrow(net_dfs$edge_data) > 0) {
			# create lookup tables for edge coordinates with composite keys
			edge_key_layout <- paste(
				edges_combined$from, edges_combined$to, 
				edges_combined$time, sep = "__|__")
			edge_x1_lookup <- setNames(edges_combined$x1, edge_key_layout)
			edge_y1_lookup <- setNames(edges_combined$y1, edge_key_layout)
			edge_x2_lookup <- setNames(edges_combined$x2, edge_key_layout)
			edge_y2_lookup <- setNames(edges_combined$y2, edge_key_layout)
			
			# generate composite keys for edges in the data frame
			edge_key_df <- paste(
				net_dfs$edge_data$from, net_dfs$edge_data$to, 
				net_dfs$edge_data$time, sep = "__|__")
			
			# assign edge coordinates using vectorized lookup
			net_dfs$edge_data$x1 <- edge_x1_lookup[edge_key_df]
			net_dfs$edge_data$y1 <- edge_y1_lookup[edge_key_df]
			net_dfs$edge_data$x2 <- edge_x2_lookup[edge_key_df]
			net_dfs$edge_data$y2 <- edge_y2_lookup[edge_key_df]
		}
	}

	# return the updated network data frames
	return(net_dfs)
}
