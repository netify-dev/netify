#' Generate edge layout for netify object
#'
#' This function prepares the edge data for visualization by calculating the coordinates
#' for line segments representing edges based on the node layouts provided by `get_node_layout`.
#' It is specifically tailored for use with `netify` objects and their corresponding layout data.
#'
#' @param netlet A `netify` object used to derive the graph and edges.
#' @param nodes_layout A matrix or a list of matrices representing node layouts for visualization.
#'              Each matrix should include the following columns:
#'              - `actor`: A character string identifying each node.
#'              - `x`: Numeric, the x-coordinate of the node in the layout.
#'              - `y`: Numeric, the y-coordinate of the node in the layout.
#'              In longitudinal studies, `nodes` should be a list where each element is a matrix
#'              corresponding to a specific time point, named with the respective year or time label
#'              (e.g., '2008', '2009'). Each matrix must maintain consistent structure and naming
#'              conventions for time points.
#'
#'              Example structure for `nodes`:
#'              ```
#'              $`2008`
#'              actor          x             y
#'              Afghanistan  0.5852844  0.4633507
#'              Albania      0.0976207  0.8473642
#'              ...
#'
#'              $`2009`
#'              actor          x             y
#'              Afghanistan -0.7392849  0.5709252
#'              Albania     -1.1160445  1.0141463
#'              ...
#'              ```
#'              Each matrix uses the `actor` names for row identification.
#'
#' @return A matrix with edge layout information or list of the same
#'         with their start and end coordinates ('x1', 'y1' for the 'from' node and 'x2', 'y2' for the 'to' node).
#' @author Cassy Dorff, Shahryar Minhas 
#'
#' @importFrom igraph as_edgelist
#'
#' @export get_edge_layout

get_edge_layout <- function(netlet, nodes_layout) {

    # Ensure the netify object is checked
    netify_check(netlet)
    
    # Get the igraph object from netlet
    g <- prep_for_igraph(netlet, 
		add_nodal_attribs = FALSE, 
		add_dyad_attribs = FALSE )

	# make sure igraph object is in the right format
	if(igraph::is_igraph(g)){ g = list(g) }

    # make sure nodes_layout is in the right format
    if(!is.list(nodes_layout)){
        nodes_layout = list(nodes_layout) }

    # Generate edges list
    edges_list <- lapply(1:length(g), function(ii) {
        g_slice <- g[[ii]]
        edges <- igraph::as_edgelist(g_slice, names = TRUE)
        edges <- data.frame(edges, stringsAsFactors = FALSE)
        names(edges) <- c("from", "to")
        
        # Retrieve node coordinates
        nodes <- nodes_layout[[ii]]
        
        # Bind coordinates to edges
        edges <- merge(
            edges, nodes[,c('actor','x','y')], 
            by.x = "from", by.y = "actor", 
            all.x = TRUE, all.y=FALSE)
        edges <- merge(
            edges, nodes[,c('actor','x','y')], 
            by.x = "to", by.y = "actor", 
            all.x = TRUE, all.y=FALSE)
        names(edges)[c(3:6)] <- c("x1", "y1", "x2", "y2")
        
        return(edges) })
    names(edges_list) = names(g)

    #    
    return(edges_list)
}
