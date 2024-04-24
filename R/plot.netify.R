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
# # library(igraph)
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

# plot_args = list()

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
			which_static=plot_args$which_static,
			seed=plot_args$seed
			)
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
	net_dfs <- merge_layout_attribs(
		netlet, nodes_list, edges_list)
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

	# if network is undirected then only include 
	# one side of edge observations this is a bit 
	# redundant since the merging step takes care 
	# of this in merge_layout_attribs.R
	if(obj_attrs$symmetric){

		# Create a new column 'edge' with sorted 'from' and 'to' values
		net_dfs$edge_data$edge <- apply(
			net_dfs$edge_data[, c("from", "to")], 1, function(x){
				paste(sort(x), collapse = "-")} )

		# add time var if cross_sec
		if(obj_attrs$netify_type == 'cross_sec'){
			net_dfs$edge_data$time = 1 }

		# Remove duplicates based on 'edge', 'time' and keep the first occurrence
		net_dfs$edge_data <- net_dfs$edge_data[
			!duplicated(
				net_dfs$edge_data[c("edge", "time")]), ]
	}

	# if network is weighted and users did not
	# specify alpha level choose for them
	
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

		# geom_text(
		# 	data=net_dfs$nodal_data,
		# 	aes(
		# 		x = x, 
		# 		y = y,
		# 		label = name
		# 	),
		# 	check_overlap = TRUE
		# ) +

# ggplot() +
# 		geom_curve(
# 			data=net_dfs$edge_data,
# 			aes(
# 				x = x1, 
# 				y = y1,
# 				xend = x2,
# 				yend = y2
# 				# alpha = verbCoop
# 				# color = factor(matlConfBin)
# 			),
# 			linewidth=.1
# 			# color='grey',
# 			# alpha=.1
# 			# arrow = arrow(length = unit(0.1, "cm"))
# 		) +
# 		geom_point(
# 			data=net_dfs$nodal_data,
# 			aes(
# 				x = x, 
# 				y = y,
# 				color = i_polity2,
# 				size = i_log_pop
# 			)
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




