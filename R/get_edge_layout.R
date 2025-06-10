#' Generate edge layout coordinates for netify visualization
#'
#' `get_edge_layout` prepares edge data for network visualization by calculating 
#' start and end coordinates for line segments representing edges. This function 
#' maps edges from a netify object to their corresponding node positions as 
#' determined by a layout algorithm.
#'
#' @param netlet A netify object (class "netify") containing the network structure 
#'   from which edges will be extracted.
#' @param nodes_layout A data.frame or matrix containing node positions, or a list 
#'   of such objects for longitudinal networks. Each element must include columns:
#'   \itemize{
#'     \item \strong{actor}: Character string identifying each node
#'     \item \strong{x}: Numeric x-coordinate of the node position
#'     \item \strong{y}: Numeric y-coordinate of the node position
#'   }
#'   
#'   For longitudinal networks, provide a named list where:
#'   \itemize{
#'     \item Names correspond to time periods in the netify object
#'     \item Each element follows the structure described above
#'     \item Time period names must match those in the netify object
#'   }
#'
#' @return Depending on the input netify object:
#'   \itemize{
#'     \item \strong{Cross-sectional}: A list containing one data.frame with columns:
#'       \itemize{
#'         \item \code{from}: Source node name
#'         \item \code{to}: Target node name
#'         \item \code{x1}, \code{y1}: Coordinates of the source node
#'         \item \code{x2}, \code{y2}: Coordinates of the target node
#'       }
#'     \item \strong{Longitudinal}: A named list of data.frames (one per time period) 
#'       with the same structure as above
#'   }
#'   
#'   The output maintains the same temporal structure as the input netify object.
#'
#' @details
#' This function performs the following operations:
#' 
#' \strong{Edge extraction:}
#' \itemize{
#'   \item Converts the netify object to igraph format internally
#'   \item Extracts the edge list preserving edge directions
#'   \item Handles both cross-sectional and longitudinal networks
#' }
#' 
#' \strong{Coordinate mapping:}
#' \itemize{
#'   \item Matches each edge endpoint to its corresponding node position
#'   \item Creates a complete set of coordinates for drawing edges
#'   \item Preserves the temporal structure for longitudinal networks
#' }
#' 
#' \strong{Use in visualization:}
#' 
#' This function is typically used as part of a visualization pipeline:
#' \enumerate{
#'   \item Create node layout using `get_node_layout()` or a custom layout algorithm
#'   \item Generate edge coordinates using this function
#'   \item Pass both to visualization functions for plotting
#' }
#'
#' @note 
#' The nodes_layout structure must exactly match the actors and time periods in 
#' the netify object. Missing actors in the layout will result in NA coordinates 
#' for their associated edges.
#' 
#' For longitudinal networks, ensure that the names of the nodes_layout list 
#' match the time period labels in the netify object (e.g., "2008", "2009").
#' 
#' This function always returns a list structure for consistency, even for 
#' cross-sectional networks where the list contains only one element.
#'
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export get_edge_layout

get_edge_layout <- function(netlet, nodes_layout) {

    # 
    netify_check(netlet)
    
    # get the igraph object from netlet
    g <- netify_to_igraph(
        netlet, 
        add_nodal_attribs = FALSE, 
        add_dyad_attribs = FALSE
    )
    
    # make sure igraph object is in the right format
    if (igraph::is_igraph(g)) {
        g <- list(g)
    }
    
    # make sure nodes_layout is in the right format
    if (!is.list(nodes_layout)) {
        nodes_layout <- list(nodes_layout)
    }
    
    # vectorized approach for single time period
    if (length(g) == 1) {
        g_slice <- g[[1]]
        nodes <- nodes_layout[[1]]
        
        # get all edges at once
        edges <- igraph::as_edgelist(g_slice, names = TRUE)
        
        # create lookup vectors for O(1) access
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
    
    # for multiple time periods, process more efficiently
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
