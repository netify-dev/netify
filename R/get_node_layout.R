#' Calculate node layout positions for netify visualization
#'
#' `get_node_layout` computes node positions for network visualization using various 
#' layout algorithms from igraph. This function converts a netify object to igraph 
#' format, applies the specified layout algorithm, and returns node coordinates 
#' suitable for plotting.
#'
#' @param netlet A netify object (class "netify") for which to compute layout positions.
#' @param layout Character string specifying the layout algorithm to use. Options include:
#'   \itemize{
#'     \item \code{"nicely"}: Automatic selection of appropriate layout (default for unipartite)
#'     \item \code{"bipartite"}: Two-column layout for bipartite networks (default for bipartite)
#'     \item \code{"fruchtermanreingold"} or \code{"fr"}: Force-directed layout
#'     \item \code{"kamadakawai"} or \code{"kk"}: Another force-directed layout
#'     \item \code{"circle"}: Nodes arranged in a circle
#'     \item \code{"star"}: Star-shaped layout
#'     \item \code{"grid"}: Nodes on a grid
#'     \item \code{"tree"}: Hierarchical tree layout
#'     \item \code{"random"} or \code{"randomly"}: Random positions
#'     \item Additional options: \code{"graphopt"}, \code{"sugiyama"}, \code{"drl"}, 
#'       \code{"lgl"}, \code{"dh"}, \code{"gem"}, \code{"mds"}
#'   }
#'   If NULL, defaults to "nicely" for unipartite or "bipartite" for bipartite networks.
#' @param static_actor_positions Logical. If TRUE, maintains consistent node positions 
#'   across all time periods in longitudinal networks. If FALSE (default), each time 
#'   period gets its own optimized layout.
#' @param which_static Integer specifying which time period's layout to use as the 
#'   static template when static_actor_positions is TRUE. If NULL (default), creates 
#'   a static layout based on the union of all edges across time periods, giving
#'   more weight to persistent edges.
#' @param seed Integer for random number generation to ensure reproducible layouts. 
#'   Default is 6886.
#' @param ig_netlet An optional pre-converted igraph object. If provided, this
#'   function will use it directly instead of converting the netify object again.
#'
#' @return A list of data frames (one per time period) where each data frame contains:
#'   \itemize{
#'     \item \strong{index}: Integer node index
#'     \item \strong{actor}: Character string with actor name
#'     \item \strong{x}: Numeric x-coordinate for node position
#'     \item \strong{y}: Numeric y-coordinate for node position
#'   }
#'   
#'   For cross-sectional networks, returns a list with one element. For longitudinal 
#'   networks, returns a named list with time periods as names.
#'
#' @details
#' This function handles layout generation for both cross-sectional and longitudinal 
#' networks with several key features:
#' 
#' \strong{Layout algorithms:}
#' 
#' The function provides access to all major igraph layout algorithms. The default 
#' "nicely" option automatically selects an appropriate algorithm based on the 
#' network structure. 
#' 
#' \strong{Longitudinal layouts:}
#' 
#' For longitudinal networks, two approaches are available:
#' \itemize{
#'   \item \strong{Dynamic layouts}: Each time period gets its own optimized layout, 
#'     which may better reveal structural changes but makes visual comparison harder
#'   \item \strong{Static layouts}: All time periods use the same node positions, 
#'     facilitating visual comparison of network evolution
#' }
#' 
#' When using static layouts with which_static = NULL, the function creates a 
#' composite layout based on the union of all edges across time periods. Edges
#' that appear more frequently are given higher weight, producing layouts that
#' emphasize the stable core structure of the network.
#' 
#' \strong{Bipartite networks:}
#' 
#' For bipartite networks, the default layout arranges the two node sets in separate 
#' columns.
#'
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @export get_node_layout
#' 

get_node_layout <- function(
	netlet,
	layout=NULL,
	static_actor_positions=FALSE,
	which_static=NULL, 
	seed=6886,
	ig_netlet=NULL	
	){

	# check if netify object
	netify_check(netlet)		

	# clean up null inputs
	if(is.null(static_actor_positions)){ static_actor_positions = FALSE }
	if(is.null(seed)){ seed = 6886 }

	# cache attributes once
	obj_attrs <- attributes(netlet)
	netify_type <- obj_attrs$netify_type
	is_bipartite <- obj_attrs$mode == 'bipartite'
	
	# convert to igraph without attributes 
	# cuz we got a need for speed
	if(is.null(ig_netlet)){
		g = netify_to_igraph(netlet, 
			add_nodal_attribs = FALSE, 
			add_dyad_attribs = FALSE )
	} else {
		# use provided igraph object if avail
		g = ig_netlet }

	# determine layout function
	if(is.null(layout)){
		layout_fun <- if(is_bipartite) 'bipartite' else 'nicely'
	} else {
		# make lowercase and match
		layout_fun <- match.arg(
			tolower(layout), 
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

	# get the actual layout function from igraph
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

	# handle cross-sectional case first (simpler)
	if(netify_type == 'cross_sec'){
		set.seed(seed)
		layout_matrix <- layout_fun(g)
		rownames(layout_matrix) <- igraph::V(g)$name
		
		# format as data frame
		nodes_df <- data.frame(
			index = seq_len(nrow(layout_matrix)),
			actor = rownames(layout_matrix),
			x = layout_matrix[,1],
			y = layout_matrix[,2],
			stringsAsFactors = FALSE
		)
		
		return(list(nodes_df))
	}

	# longitudinal case
	# ensure g is a list
	if(igraph::is_igraph(g)){ g = list(g) }
	
	if(static_actor_positions){
		# determine which graph to use for static layout
		if(is.null(which_static)){
			# create union graph with edge weights based on frequency
			g_static <- create_union_graph(g, obj_attrs)
		} else {
			# use specific time period
			g_static <- g[[which_static]]
		}
		
		# compute layout once
		set.seed(seed)
		layout_matrix_static <- layout_fun(g_static)
		rownames(layout_matrix_static) <- igraph::V(g_static)$name
		
		# create mapping for all actors across time
		all_actors <- unique(unlist(lapply(g, function(x) igraph::V(x)$name)))
		actor_positions <- data.frame(
			actor = all_actors,
			x = NA_real_,
			y = NA_real_,
			stringsAsFactors = FALSE
		)
		
		# fill in positions for actors in static layout
		match_idx <- match(rownames(layout_matrix_static), actor_positions$actor)
		actor_positions$x[match_idx] <- layout_matrix_static[,1]
		actor_positions$y[match_idx] <- layout_matrix_static[,2]
		
		# create layout for each time period
		nodes_list <- lapply(seq_along(g), function(i){
			actors_t <- igraph::V(g[[i]])$name
			match_idx <- match(actors_t, actor_positions$actor)
			
			data.frame(
				index = seq_along(actors_t),
				actor = actors_t,
				x = actor_positions$x[match_idx],
				y = actor_positions$y[match_idx],
				stringsAsFactors = FALSE
			)
		})
		
	} else {
		# dynamic layouts for each time period
		nodes_list <- lapply(seq_along(g), function(i){
			set.seed(seed + i - 1)  # ensure different but reproducible layouts
			l_matrix <- layout_fun(g[[i]])
			
			data.frame(
				index = seq_len(nrow(l_matrix)),
				actor = igraph::V(g[[i]])$name,
				x = l_matrix[,1],
				y = l_matrix[,2],
				stringsAsFactors = FALSE
			)
		})
	}
	
	# add names
	names(nodes_list) <- names(g)
	
	return(nodes_list)
}

#' Create union graph for static layout
#' 
#' creates a weighted union graph from a list of graphs where edge weights
#' represent the frequency or average weight across time periods
#' 
#' @param g_list list of igraph objects
#' @param obj_attrs attributes from the netify object
#' @return single igraph object representing the union
#' @keywords internal
#' @noRd

create_union_graph <- function(g_list, obj_attrs){
	# get all unique actors across time
	all_actors <- unique(unlist(lapply(g_list, function(x) igraph::V(x)$name)))
	n_actors <- length(all_actors)
	
	# create empty adjacency matrix
	union_adj <- matrix(0, n_actors, n_actors, 
		dimnames = list(all_actors, all_actors))
	
	# aggregate edges across time periods
	for(g_t in g_list){
		# get adjacency matrix for this time period
		adj_t <- igraph::as_adjacency_matrix(g_t, attr = "weight", sparse = FALSE)
		
		# map to union matrix positions
		actors_t <- rownames(adj_t)
		idx <- match(actors_t, all_actors)
		
		# add to union (increment for binary, sum for weighted)
		if(obj_attrs$weight_binary){
			union_adj[idx, idx] <- union_adj[idx, idx] + (adj_t > 0)
		} else {
			union_adj[idx, idx] <- union_adj[idx, idx] + adj_t
		}
	}
	
	# normalize by number of time periods for weighted networks
	if(!obj_attrs$weight_binary){
		union_adj <- union_adj / length(g_list)
	}
	
	# create igraph object from union
	igraph::graph_from_adjacency_matrix(
		union_adj,
		mode = ifelse(obj_attrs$symmetric, "undirected", "directed"),
		weighted = TRUE,
		diag = FALSE
	)
}