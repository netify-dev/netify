#' Calculate node layout positions for a netify object
#'
#' This function converts a netify object into an igraph object to compute layout positions 
#' using various igraph layout algorithms. It supports both cross-sectional and longitudinal 
#' data, with an option for static positioning of actors.
#'
#' @param netlet A netify object to be used for layout computation.
#' @param layout A character string specifying the layout algorithm from igraph 
#'        to be used. If NULL and the object mode is 'bipartite', 'bipartite' layout is used;
#'        otherwise, 'nicely' is used as default.
#' @param static_actor_positions Logical indicating whether to use static positions for actors.
#'        Useful in longitudinal studies where node positions should remain consistent over time.
#' @param which_static Integer indicating which time point's layout should be used as the static layout.
#'
#' @return A list containing two elements: `nodes` and `edges`. Each of these is a list of data frames
#'         representing the nodes and edges for each time point in the netify object.
#'         Each node data frame contains columns for node indices, actor names, and their x, y coordinates.
#'         Each edge data frame includes from and to node names, along with start and end coordinates for drawing edges.
#'#' @author Cassy Dorff, Shahryar Minhas
#' 
#'
#' @importFrom igraph layout_nicely layout_with_fr layout_with_kk layout_randomly
#'             layout_in_circle layout_as_star layout_on_grid layout_with_graphopt
#'             layout_with_sugiyama layout_with_drl layout_with_lgl layout_as_bipartite
#' @export get_node_layout
#'

get_node_layout <- function(
	netlet,
	layout=NULL,
	static_actor_positions=FALSE,
	which_static=1, 
	seed=6886
	){

	# check if netify object
	netify_check(netlet)		

    # clean up some inputs that need to 
    # be adjusted if they are set to NULL
    if(is.null(static_actor_positions)){ static_actor_positions = FALSE }
    if(is.null(which_static)){ which_static = 1 }
	if(is.null(seed)){ seed = 6886 }

	# pull out attrs
	obj_attrs <- attributes(netlet)

	# pull out msrmnts
	msrmnts <- netify_measurements(netlet)

	# convert to igraph to get
	# layout positions
	g = prep_for_igraph(netlet)

	# check to see if the user has a desired layout
	# algo from igraph, if not default to nicely
    # or bipartite depending on the mode
	if(is.null(layout)){

		# if bipartite then we need to use 
		# bipartite layout otherwise use nicely
		if(obj_attrs$mode == 'bipartite'){
			layout_fun = 'bipartite'
		} else { layout_fun <- 'nicely' }

    # if the user has specified a layout
    # then match it using the code below
    } else {
    layout_fun <- match.arg(
        layout, 
        choices = c(
            "nicely", "fruchterman.reingold", 
            "kamada.kawai", "random", "circle", 
            "star", "grid", "graphopt", 
            "sugiyama", "drl", "lgl", 'bipartite',
            'tree', 'randomly', 'dh', 'fr',
            'kk', 'gem', 'mds'
            )
        )
	}

	# based on layout choice or default get the relevant
	# layout function from igraph and assign it to layout_fun
	layout_fun <- switch(
		layout_fun,
		nicely = layout_nicely,
		bipartite = layout_as_bipartite,
		fruchterman.reingold = layout_with_fr,
		fr = layout_with_fr,
		kamada.kawai = layout_with_kk,
		kk = layout_with_kk,
		random = layout_randomly,
		circle = layout_in_circle,
		star = layout_as_star,
		grid = layout_on_grid,
		graphopt = layout_with_graphopt,
		sugiyama = layout_with_sugiyama,
		drl = layout_with_drl,
		lgl = layout_with_lgl,
		tree = layout_as_tree,
		randomly = layout_randomly,
		dh = layout_with_dh, 
		gem = layout_with_gem,
		mds = layout_with_mds
		)

	# Calculate node positions using the specified layout
	# if this is a longit then create a list of matrices
	if(obj_attrs$netify_type != 'cross_sec'){
		layout_matrix <- lapply(g, function(g_slice){
			l_matrix_slice = layout_fun(g_slice)
			rownames(l_matrix_slice) <- igraph::V(g_slice)$name
			return(l_matrix_slice) })
		names(layout_matrix) = names(g)
	} else {
		layout_matrix <- layout_fun(g)
		rownames(layout_matrix) <- igraph::V(g)$name }

	# if static layout then choose first
	if(static_actor_positions){
		layout_matrix = layout_matrix[[which_static]] }

	# if layout_matrix and g are not lists then
	# wrap them in one so we can have one
	# set of code for getting positions for 
	# longit and cross_sec
	if(!is.list(layout_matrix)){
		layout_matrix = list(layout_matrix)  }
	if(class(g)=='igraph'){
		g = list(g) }

	# set up nodes
	nodes_list = lapply(layout_matrix, function(layout_slice){

		# set up nodes df
		nodes = data.frame(
			index = 1:nrow(layout_slice),
			actor = rownames(layout_slice) )
		nodes = cbind(nodes, layout_slice)
		names(nodes)[3:4] = c("x", "y")

		#
		return(nodes) })

	#
	return( nodes_list )
}


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
#'
#' @export get_edge_layout

get_edge_layout <- function(netlet, nodes_layout) {

    # Ensure the netify object is checked
    netify_check(netlet)
    
    # Get the igraph object from netlet
    g <- prep_for_igraph(netlet)

	# make sure igraph object is in the right format
	if(class(g)=='igraph'){ g = list(g) }

    # make sure nodes_layout is in the right format
    if(!is.list(nodes_layout)){
        nodes_layout = list(nodes_layout) }

    # Generate edges list
    edges_list <- lapply(1:length(g), function(ii) {
        g_slice <- g[[ii]]
        edges <- as_edgelist(g_slice, names = TRUE)
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


