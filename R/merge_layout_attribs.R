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

merge_layout_attribs <- function(
    netlet, nodes_list, edges_list
    ){

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
		# longitudinal case - optimized
		ego_netlet <- obj_attrs$ego_netlet
		
		# get time names
		if (!is.null(ego_netlet)) {
			list_names <- names(netlet)
			list_names <- sub("^[^_]+__", "", list_names)
		} else {
			list_names <- names(nodes_list)
		}
		
		# pre-calculate total rows for pre-allocation
		n_nodes_total <- sum(sapply(nodes_list, nrow))
		n_edges_total <- sum(sapply(edges_list, function(x) if(nrow(x) > 0) nrow(x) else 0))
		
		# pre-allocate for nodes
		nodes_combined <- vector("list", length = 4)
		names(nodes_combined) <- c("actor", "time", "x", "y")
		
		# fill nodes data
		current_row <- 1
		for (i in seq_along(nodes_list)) {
			nodes_i <- nodes_list[[i]]
			n_rows <- nrow(nodes_i)
			if (n_rows > 0) {
				rows <- current_row:(current_row + n_rows - 1)
				if (current_row == 1) {
					# initialize on first iteration
					nodes_combined$actor <- character(n_nodes_total)
					nodes_combined$time <- character(n_nodes_total)
					nodes_combined$x <- numeric(n_nodes_total)
					nodes_combined$y <- numeric(n_nodes_total)
				}
				nodes_combined$actor[rows] <- nodes_i$actor
				nodes_combined$time[rows] <- list_names[i]
				nodes_combined$x[rows] <- nodes_i$x
				nodes_combined$y[rows] <- nodes_i$y
				current_row <- current_row + n_rows
			}
		}
		nodes <- as.data.frame(nodes_combined, stringsAsFactors = FALSE)
		
		# similar optimization for edges
		if (n_edges_total > 0) {
			edges_combined <- vector("list", length = 7)
			names(edges_combined) <- c("from", "to", "time", "x1", "y1", "x2", "y2")
			
			current_row <- 1
			for (i in seq_along(edges_list)) {
				edges_i <- edges_list[[i]]
				n_rows <- nrow(edges_i)
				if (n_rows > 0) {
					rows <- current_row:(current_row + n_rows - 1)
					if (current_row == 1) {
						# initialize
						for (nm in names(edges_combined)) {
							edges_combined[[nm]] <- if(nm %in% c("x1", "y1", "x2", "y2")) {
								numeric(n_edges_total)
							} else {
								character(n_edges_total)
							}
						}
					}
					edges_combined$from[rows] <- edges_i$from
					edges_combined$to[rows] <- edges_i$to
					edges_combined$time[rows] <- list_names[i]
					edges_combined$x1[rows] <- edges_i$x1
					edges_combined$y1[rows] <- edges_i$y1
					edges_combined$x2[rows] <- edges_i$x2
					edges_combined$y2[rows] <- edges_i$y2
					current_row <- current_row + n_rows
				}
			}
			edges <- as.data.frame(edges_combined, stringsAsFactors = FALSE)
		} else {
			edges <- data.frame(
				from = character(0), to = character(0), time = character(0),
				x1 = numeric(0), y1 = numeric(0), x2 = numeric(0), y2 = numeric(0),
				stringsAsFactors = FALSE
			)
		}
		
		# use match for merging
		# for nodal data
		node_key_df <- paste(net_dfs$nodal_data$name, net_dfs$nodal_data$time, sep = "__|__")
		node_key_layout <- paste(nodes$actor, nodes$time, sep = "__|__")
		node_match <- match(node_key_df, node_key_layout)
		
		net_dfs$nodal_data$x <- nodes$x[node_match]
		net_dfs$nodal_data$y <- nodes$y[node_match]
		
		# for edge data
		edge_key_df <- paste(net_dfs$edge_data$from, net_dfs$edge_data$to, 
												net_dfs$edge_data$time, sep = "__|__")
		edge_key_layout <- paste(edges$from, edges$to, edges$time, sep = "__|__")
		edge_match <- match(edge_key_df, edge_key_layout)
		
		net_dfs$edge_data$x1 <- edges$x1[edge_match]
		net_dfs$edge_data$y1 <- edges$y1[edge_match]
		net_dfs$edge_data$x2 <- edges$x2[edge_match]
		net_dfs$edge_data$y2 <- edges$y2[edge_match]
	}

	#
	return(net_dfs)
}
