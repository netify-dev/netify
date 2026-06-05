#' Generate edge layout coordinates for netify visualization
#'
#' `get_edge_layout` prepares edge data for network visualization by calculating
#' start and end coordinates for line segments representing edges. this function
#' maps edges from a netify object to their corresponding node positions as
#' determined by a layout algorithm.
#'
#' @param netlet a netify object (class "netify") containing the network structure
#'   from which edges will be extracted.
#' @param nodes_layout a data.frame or matrix containing node positions, or a list
#'   of such objects for longitudinal networks. each element must include columns:
#'   \itemize{
#'     \item \strong{actor}: character string identifying each node
#'     \item \strong{x}: numeric x-coordinate of the node position
#'     \item \strong{y}: numeric y-coordinate of the node position
#'   }
#'
#'   for longitudinal networks, provide a named list where:
#'   \itemize{
#'     \item names correspond to time periods in the netify object
#'     \item each element follows the structure described above
#'     \item time period names must match those in the netify object
#'   }
#' @param ig_netlet an optional pre-converted igraph object. if provided, this
#'   function will use it directly instead of converting the netify object again.
#'
#' @return depending on the input netify object:
#'   \itemize{
#'     \item \strong{cross-sectional}: a list containing one data.frame with columns:
#'       \itemize{
#'         \item \code{from}: source node name
#'         \item \code{to}: target node name
#'         \item \code{x1}, \code{y1}: coordinates of the source node
#'         \item \code{x2}, \code{y2}: coordinates of the target node
#'       }
#'     \item \strong{longitudinal}: a named list of data.frames (one per time period)
#'       with the same structure as above
#'   }
#'
#'   the output maintains the same temporal structure as the input netify object.
#'
#' @details
#' this function performs the following operations:
#'
#' \strong{edge extraction:}
#' \itemize{
#'   \item converts the netify object to igraph format internally
#'   \item extracts the edge list preserving edge directions
#'   \item handles both cross-sectional and longitudinal networks
#' }
#'
#' \strong{coordinate mapping:}
#' \itemize{
#'   \item matches each edge endpoint to its corresponding node position
#'   \item creates a complete set of coordinates for drawing edges
#'   \item preserves the temporal structure for longitudinal networks
#' }
#'
#' \strong{use in visualization:}
#'
#' this function is typically used as part of a visualization pipeline:
#' \enumerate{
#'   \item create node layout using `get_node_layout()` or a custom layout algorithm
#'   \item generate edge coordinates using this function
#'   \item pass both to visualization functions for plotting
#' }
#'
#' @note
#' the nodes_layout structure must exactly match the actors and time periods in
#' the netify object. missing actors in the layout will result in na coordinates
#' for their associated edges.
#'
#' for longitudinal networks, ensure that the names of the nodes_layout list
#' match the time period labels in the netify object (e.g., "2008", "2009").
#'
#' this function always returns a list structure for consistency, even for
#' cross-sectional networks where the list contains only one element.
#'
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export get_edge_layout

get_edge_layout <- function(
	netlet,
	nodes_layout,
	ig_netlet = NULL) {
	#
	netify_check(netlet)

	# convert to igraph without attributes for speed
	if (is.null(ig_netlet)) {
		g <- netify_to_igraph(netlet,
			add_nodal_attribs = FALSE,
			add_dyad_attribs = FALSE,
			.quiet_na = TRUE
		)
	} else {
		# use provided igraph object if avail
		g <- ig_netlet
	}

	# make sure igraph object is in the right format
	if (igraph::is_igraph(g)) {
		g <- list(g)
	}

	# make sure nodes_layout is in the right format
	if (!is.list(nodes_layout)) {
		nodes_layout <- list(nodes_layout)
	}

	# vectorized path for single time period
	if (length(g) == 1) {
		g_slice <- g[[1]]
		nodes <- nodes_layout[[1]]

		# get all edges at once
		edges <- igraph::as_edgelist(g_slice, names = TRUE)

		# create lookup vectors for o(1) access
		node_x <- setNames(nodes$x, nodes$actor)
		node_y <- setNames(nodes$y, nodes$actor)

		# vectorized coordinate assignment
		edges_df <- data.frame(
			from = edges[, 1],
			to = edges[, 2],
			x1 = node_x[edges[, 1]],
			y1 = node_y[edges[, 1]],
			x2 = node_x[edges[, 2]],
			y2 = node_y[edges[, 2]],
			stringsAsFactors = FALSE
		)

		return(list(edges_df))
	}

	edges_list <- vector("list", length(g))
	names(edges_list) <- names(g)

	# pre-process all node lookups
	node_lookups <- lapply(nodes_layout, function(nodes) {
		list(
			x = setNames(nodes$x, nodes$actor),
			y = setNames(nodes$y, nodes$actor)
		)
	})

	# process each time period
	for (ii in seq_along(g)) {
		edges <- igraph::as_edgelist(g[[ii]], names = TRUE)
		lookup <- node_lookups[[ii]]

		edges_list[[ii]] <- data.frame(
			from = edges[, 1],
			to = edges[, 2],
			x1 = lookup$x[edges[, 1]],
			y1 = lookup$y[edges[, 1]],
			x2 = lookup$x[edges[, 2]],
			y2 = lookup$y[edges[, 2]],
			stringsAsFactors = FALSE
		)
	}

	return(edges_list)
}
