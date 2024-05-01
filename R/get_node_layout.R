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
#'        If TRUE, the layout by default is calculated based on a collapsed adjacency matrix across 
#'        all time points. Users can also specify a specific time point to use as the static layout by 
#'        setting `which_static` to the desired time point.
#' @param which_static Integer indicating which time point's layout should be used as the static layout.
#' @param seed Integer specifying the seed for random number generation.
#'
#' @return A list containing two elements: `nodes` and `edges`. Each of these is a list of data frames
#'         representing the nodes and edges for each time point in the netify object.
#'         Each node data frame contains columns for node indices, actor names, and their x, y coordinates.
#'         Each edge data frame includes from and to node names, along with start and end coordinates for drawing edges.
#' @author Cassy Dorff, Shahryar Minhas
#' 
#'
#' @importFrom igraph layout_nicely layout_with_fr layout_with_kk layout_randomly
#'             layout_in_circle layout_as_star layout_on_grid layout_with_graphopt
#'             layout_with_sugiyama layout_with_drl layout_with_lgl layout_as_bipartite
#'             V
#' @export get_node_layout
#'

get_node_layout <- function(
	netlet,
	layout=NULL,
	static_actor_positions=FALSE,
	which_static=NULL, 
	seed=6886
	){

	# check if netify object
	netify_check(netlet)		

    # clean up some inputs that need to 
    # be adjusted if they are set to NULL
    if(is.null(static_actor_positions)){ static_actor_positions = FALSE }
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
		nicely = igraph::layout_nicely,
		bipartite = igraph::layout_as_bipartite,
		fruchterman.reingold = igraph::layout_with_fr,
		fr = igraph::layout_with_fr,
		kamada.kawai = igraph::layout_with_kk,
		kk = igraph::layout_with_kk,
		random = igraph::layout_randomly,
		circle = igraph::layout_in_circle,
		star = igraph::layout_as_star,
		grid = igraph::layout_on_grid,
		graphopt = igraph::layout_with_graphopt,
		sugiyama = igraph::layout_with_sugiyama,
		drl = igraph::layout_with_drl,
		lgl = igraph::layout_with_lgl,
		tree = igraph::layout_as_tree,
		randomly = igraph::layout_randomly,
		dh = igraph::layout_with_dh, 
		gem = igraph::layout_with_gem,
		mds = igraph::layout_with_mds
		)

	# Calculate node positions using the specified layout
	# if this is a longit then create a list of matrices
	if(obj_attrs$netify_type != 'cross_sec'){

		# if static actor positions chosen then 
		# just choose gth object and use that for
		# setting layout
		if(static_actor_positions){

			# if no time point chosen to base layout off of then
			# calculate based on summed/averaged across all time points
			if(is.null(which_static)){
				# create aggregated adjacency matrix
				raw_netlet = get_raw(netlet)

				# if list then turn into an array
				if(is.list(raw_netlet)){ raw_netlet = list_to_array(raw_netlet) }

				# get static layout
				if(obj_attrs$weight_binary){
					raw_netlet_static = apply(raw_netlet, 1:2, sum, na.rm=TRUE)
				} else { 
					raw_netlet_static = apply(raw_netlet, 1:2, mean, na.rm=TRUE) }

				# melt and turn into netify object
				raw_netlet_df = reshape2::melt(raw_netlet_static)
				raw_netlet_df = raw_netlet_df[raw_netlet_df$Var1 != raw_netlet_df$Var2, ]
				netlet_static = suppressMessages(suppressWarnings(netify(
					dyad_data = raw_netlet_df,
					actor1 = 'Var1', actor2 = 'Var2', 
					symmetric = obj_attrs$symmetric, mode = obj_attrs$mode,
					diag_to_NA = obj_attrs$diag_to_NA, missing_to_zero = TRUE )))
				g_static = prep_for_igraph(netlet_static) }

			# if specific time point chosen to base layout off of then
			# set g_static based on what was chosen
			if(!is.null(which_static)){ g_static = g[[which_static]] }

			# set seed
			set.seed(seed)

			# get layout
			layout_matrix_static = layout_fun(g_static)
			rownames(layout_matrix_static) = igraph::V(g_static)$name
			layout_matrix = rep(list(layout_matrix_static), length(g))
		} # end if block for static_actor_positions in longit case

		# if static positions not specified then iterate through graph object
		# and create a separate layout for every graph using the fun
		# layout chosen
		if(!static_actor_positions){

			layout_matrix <- lapply(g, function(g_slice){

				# set seed
				set.seed(seed)

				# apply chosen layout
				l_matrix_slice = layout_fun(g_slice)

				# set rownames
				rownames(l_matrix_slice) <- igraph::V(g_slice)$name

				#
				return(l_matrix_slice) })
		} # end if block for non static_actor_positions in longit case

		# name list elements
		names(layout_matrix) = names(g) } # end if block for longit condition

	# cross-sectional case 
	if(obj_attrs$netify_type == 'cross_sec'){

		# set seed
		set.seed(seed)

		# if cross_sec then just one matrix
		layout_matrix <- layout_fun(g)

		# set rownames
		rownames(layout_matrix) <- igraph::V(g)$name

		# wrap into list for easier processing later
		layout_matrix = list(layout_matrix) } # end of if block for cross_sec case

	# convert g to list if not already for easier processing
	if(igraph::is_igraph(g)){ g = list(g) }

	# set up dfs of node layouts into a list with a numeric
	# index column, actor name column, and relabel the layout
	# cols generated by igraph to x and y
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
