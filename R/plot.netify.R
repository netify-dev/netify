#' Plot method for netify objects
#'
#' `plot.netify` takes in a netify object
#' and produces a network visualization
#' using the `tidygraph` and `ggraph` packages.
#'
#' @param x object of class netify, produced by netify()
#' @param ... Additional arguments passed to geom functions within 'ggraph'.
#'           This can include aesthetics mappings such as alpha, size, and color,
#'           or any other graphical parameters supported by 'geom_edge_link' and 'geom_node_point'.
#' @return ggraph object
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @import igraph
#' @import ggplot2
#' 
#' @export plot.netify

###notes to sm and cd int he documentation for this or vignette: 
### tel users that these are the igraph options
		# choices = c(
		# 	"nicely", "fruchterman.reingold", 
		# 	"kamada.kawai", "random", "circle", 
		# 	"star", "grid", "graphopt", 
		# 	"sugiyama", "drl", "lgl", 'bipartite',
		# 	'tree', 'randomly'
		# )

# library(netify)
# library(igraph)
# library(ggplot2)

# example(decompose_netlet)
# x = netlet

# x = subset_netlet(
# 	x, 
# 	when_to_subset=c('2008','2009') 
# 	# when_to_subset=c('2009')
# )

# plot_args = list(
# 	layout='kamada.kawai'
# )

# layout: user has selected a layout algo from igraph 
# based on the choices in the documentatoin
# layout_matrix : user has made a matrix already inwhich 
# they describe the nodal positions of actors
# static_actors: default is TRUE

plot.netify <- function(x, ...){

	# check if netify object
	netify_check(x)	

	# get plot args
	plot_args = list(...)	

	######################
	#
	netlet <- x ; rm(x)

	# pull out attrs
	obj_attrs <- attributes(netlet)

	# pull out msrmnts
	msrmnts <- netify_measurements(netlet)
	######################

	######################
	# 
	return(paste0("patience ", Sys.info()['user']))


# 	######################
# 	# org the netlet into a  and dyadic df with all the
# 	# relev attributes so that we can plot
# 	net_dfs = decompose_netlet( netlet ) 

# 	# cross_sec
# 	if(obj_attrs$netify_type == 'cross_sec'){

# 		# pull out nodes and edges from list format
# 		nodes = nodes_list[[1]]
# 		edges = edges_list[[1]]

# 		# in the nodal part of net_dfs add in the
# 		# xy pos of actors
# 		net_dfs$nodal_data = merge(
# 			net_dfs$nodal_data, nodes, 
# 			by.x='name', by.y='actor' )
		
# 		# now do the same for the edge data
# 		net_dfs$edge_data = merge(
# 			net_dfs$edge_data, edges, 
# 			by.x=c('from', 'to'), 
# 			by.y=c('from', 'to') ) }

# 	# longit
# 	if(obj_attrs$netify_type != 'cross_sec'){

# 		# pull out nodes and edges from list format
# 		nodes = lapply(1:length(nodes_list), function(ii){
# 			nodes = nodes_list[[ii]]
# 			nodes$time = names(nodes_list)[ii]
# 			return(nodes) })
# 		nodes = do.call('rbind', nodes)
# 		edges = lapply(1:length(edges_list), function(ii){
# 			edges = edges_list[[ii]]
# 			edges$time = names(edges_list)[ii]
# 			return(edges) })
# 		edges = do.call('rbind', edges) 

# 		# in the nodal part of net_dfs add in the
# 		# xy pos of actors
# 		net_dfs$nodal_data = merge(
# 			net_dfs$nodal_data, nodes, 
# 			by.x=c('name','time'), by.y=c('actor','time') )
		
# 		# now do the same for the edge data
# 		net_dfs$edge_data = merge(
# 			net_dfs$edge_data, edges, 
# 			by.x=c('from', 'to', 'time'), 
# 			by.y=c('from', 'to', 'time') ) }
# 	######################

# 	######################
# 	# plot

# # user parameters for plot

# # nodes:
# 	# color
# 	# size
# 	# alpha

# # edges:
# 	# alpha
# 	# color
# 	# linewidth

# 	ggplot() +
# 		geom_point(
# 			data=net_dfs$nodal_data,
# 			aes(
# 				x = x, 
# 				y = y,
# 				color = i_polity2,
# 				size = i_log_pop
# 			)
# 		) + 
# 		geom_text(
# 			data=net_dfs$nodal_data,
# 			aes(
# 				x = x, 
# 				y = y,
# 				label = name
# 			),
# 			check_overlap = TRUE
# 		) +
# 		geom_segment(
# 			data=net_dfs$edge_data,
# 			aes(
# 				x = x1, 
# 				y = y1,
# 				xend = x2,
# 				yend = y2,
# 				alpha = verbCoop
# 				# color = factor(matlConfBin)
# 			),
# 			size=1.5,
# 			arrow = arrow(length = unit(0.3, "cm"))
# 		) +
# 		facet_wrap(~time) +
# 		theme_netify()
# 	######################

}

# }




#' theme_netify function
#'
#' This function returns a customized theme for netify plots.
#' It is based on the `theme_minimal` function from the `ggplot2` package.
#' It removes axis text and titles from the plot.
#'
#' @return A customized theme object for netify plots.
#' @author Cassy Dorff, Shahryar Minhas
#' @import ggplot2
#' @export theme_netify
#'
#'
theme_netify = function(){
	theme_minimal() + 
	theme(
		axis.text = element_blank(),
		axis.title = element_blank(),
		legend.position='top'
	)
}




