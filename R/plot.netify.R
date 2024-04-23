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
# )

# x1= subset_netlet(
# 	x, 
# 	when_to_subset=c('2009')
# )

# plot_args = list(
# 	layout='fr'
# )

# layout: user has selected a layout algo from igraph 
	# based on the choices in the documentatoin
# node_layout : user has made a matrix already in which 
	# they describe the nodal positions of actors
# static_actor_positions: default is TRUE
# which_static: default is 1
# weighted: logical
# symmetric: 
# node_color_var:
# node_size_var:
# node_alpha_var:
# edge_color_var:
# edge_alpha_var:
# edge_linewidth_var:
# edge_arrow: logical
# edge_arrow_size:



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
	# get layouts of nodes after checking
	# whether user has supplied their own
	if(is.null(plot_args$node_layout)){
		nodes_list = get_node_layout(
			netlet, 
			layout=plot_args$layout, 
			static_actor_positions=plot_args$static_actor_positions,
			which_static=plot_args$which_static )
	} else { 
		nodes_list = plot_args$node_layout
		if(!is.list(nodes_list)){
			nodes_list = list(nodes_list)
		}
	}

	# get info for drawing edge segments
	edges_list = get_edge_layout(
		netlet, 
		nodes_layout=nodes_list )
	######################	

	######################
	# org the netlet into a  and dyadic df with all the
	# relev attributes so that we can plot
	net_dfs = decompose_netlet( netlet ) 

	# cross_sec
	if(obj_attrs$netify_type == 'cross_sec'){

		# pull out nodes and edges from list format
		nodes = nodes_list[[1]]
		edges = edges_list[[1]]

		# in the nodal part of net_dfs add in the
		# xy pos of actors
		net_dfs$nodal_data = merge(
			net_dfs$nodal_data, nodes, 
			by.x='name', by.y='actor' )
		
		# now do the same for the edge data
		net_dfs$edge_data = merge(
			net_dfs$edge_data, edges, 
			by.x=c('from', 'to'), 
			by.y=c('from', 'to') ) }

	# longit
	if(obj_attrs$netify_type != 'cross_sec'){

		# pull out nodes and edges from list format
		nodes = lapply(1:length(nodes_list), function(ii){
			nodes = nodes_list[[ii]]
			nodes$time = names(nodes_list)[ii]
			return(nodes) })
		nodes = do.call('rbind', nodes)
		edges = lapply(1:length(edges_list), function(ii){
			edges = edges_list[[ii]]
			edges$time = names(edges_list)[ii]
			return(edges) })
		edges = do.call('rbind', edges) 

		# in the nodal part of net_dfs add in the
		# xy pos of actors
		net_dfs$nodal_data = merge(
			net_dfs$nodal_data, nodes, 
			by.x=c('name','time'), by.y=c('actor','time') )
		
		# now do the same for the edge data
		net_dfs$edge_data = merge(
			net_dfs$edge_data, edges, 
			by.x=c('from', 'to', 'time'), 
			by.y=c('from', 'to', 'time') ) }
	######################	

	######################
	# check some user inputs for the plot

	# if users did not supply an argument
	# for checking overlap in text/label
	# then set default to TRUE
	if(is.null(plot_args$check_overlap)){
		plot_args$check_overlap = TRUE }

	# if users chose to label specific countries
	# then replace name column
	if(!is.null(plot_args$select_text)){

		# pull out groups to highlight
		to_label = unique(plot_args$select_text)

		# save old
		net_dfs$nodal_data$name_old = net_dfs$nodal_data$name

		# replace name label in net_dfs$nodal_data
		# with NA if not in to_label
		net_dfs$nodal_data$name = ifelse(
			net_dfs$nodal_data$name %in% to_label,
			net_dfs$nodal_data$name, NA )
	}

	# if users did not supply an argument 
	# for line segment curve or not
	# then set default to no curve
	if(is.null(plot_args$curve_edges)){
		plot_args$curve_edges = FALSE }

	# if users did not supply an argument
	# for curvature of line segments
	# then set default to 0.5
	if(is.null(plot_args$edge_curvature)){
		plot_args$edge_curvature = 0.5 }
	
	# if users did not supply an argument
	# for angle of line segments
	# then set default to 0
	if(is.null(plot_args$edge_angle)){
		plot_args$edge_angle = 90 }
	
	# if users did not supply an argument
	# for ncp of line segments
	# then set default to 5
	if(is.null(plot_args$edge_ncp)){
		plot_args$edge_ncp = 5 }

	# if users did not supply an argument
	# for arrows and the network is directed
	# then set up default instructions
	# for arrows
	if(!obj_attrs$symmetric){
		if(is.null(plot_args$edge_arrow)){
			plot_args$edge_arrow = arrow(length = unit(0.3, "cm"))
		} else {
			plot_args$edge_arrow = NULL }
	}

	# if network is weighted and users did not
	# 
	######################

	######################	
	# plot
	viz = ggplot()
	######################	

	# nodal data #####################
	# add points
	if(plot_args$add_points){
		viz = viz + geom_point(
			data=net_dfs$nodal_data,
			aes_string(
				x = 'x', 
				y = 'y',
				alpha = plot_args$node_alpha_var,			
				color = plot_args$node_color_var,
				fill = plot_args$node_color_var,
				shape = plot_args$node_shape_var,			
				size = plot_args$node_size_var, 
				stroke = plot_args$node_stroke_var
			),
			alpha = plot_args$node_alpha,
			color = plot_args$node_color,
			fill = plot_args$node_color,
			size = plot_args$node_size,
			stroke = plot_args$node_stroke
		)
	}

	# add text
	if(plot_args$add_text){
		viz = viz + geom_text(
			data=net_dfs$nodal_data,
			aes_string(
				x = 'x', 
				y = 'y',
				label = 'name',
				alpha = plot_args$node_alpha_var,
				color = plot_args$node_color_var,
				size = plot_args$node_size_var
			),
			check_overlap = plot_args$check_overlap,
			alpha = plot_args$text_alpha,
			color = plot_args$text_color,
			size = plot_args$text_size,			
			family = plot_args$text_family,
			fontface = plot_args$text_fontface,
			angle = plot_args$text_angle
		)
	}

	# add label
	if(plot_args$add_label){
		viz = viz + geom_label(
			data=net_dfs$nodal_data,
			aes_string(
				x = 'x', 
				y = 'y',
				label = 'name',
				alpha = plot_args$node_alpha_var,
				color = plot_args$node_color_var,
				size = plot_args$node_size_var
			),
			check_overlap = plot_args$check_overlap,
			alpha = plot_args$label_alpha,
			color = plot_args$label_color,
			size = plot_args$label_size,			
			family = plot_args$label_family,
			fontface = plot_args$label_fontface,
			angle = plot_args$label_angle
		)
	}	
	######################

	# edge data #####################

# need to add stuff about weighted or not


	if(plot_args$curve_edges){
		viz = viz + geom_curve(
			data=net_dfs$edge_data,
			aes(
				x = 'x1', 
				y = 'y1',
				xend = 'x2',
				yend = 'y2',
				alpha = plot_args$edge_alpha_var,
				color = plot_args$edge_color_var,
				linetype = plot_args$edge_linetype_var,
				linewidth = plot_args$edge_linewidth_var
			),
			curvature = plot_args$edge_curvature,
			angle = plot_args$edge_angle,
			ncp = plot_args$edge_ncp,
			arrow = plot_args$edge_arrow			
		)
	} else {
		viz = viz + geom_segment(
			data=net_dfs$edge_data,
			aes(
				x = 'x1', 
				y = 'y1',
				xend = 'x2',
				yend = 'y2',
				alpha = plot_args$edge_alpha_var,
				color = plot_args$edge_color_var,
				linetype = plot_args$edge_linetype_var,
				linewidth = plot_args$edge_linewidth_var
			),
			arrow = plot_args$edge_arrow
		)
	}
	######################

# 	# facet instructions #####################
# viz + facet_wrap(~time, scales='free') + theme_netify()

# 	######################			

# 	# theme changes #####################
# viz + facet_wrap(~time, scales='free') + theme_netify()

	######################			

	######################
	# 
	return(paste0("patience ", Sys.info()['user']))


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

# ggplot() +
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
# 				yend = y2
# 				# alpha = verbCoop
# 				# color = factor(matlConfBin)
# 			)
# 			# size=1.5,
# 			# arrow = arrow(length = unit(0.1, "cm"))
# 		) +
# 		facet_wrap(~time, scales='free') +
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




