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
# # library(rlang)

# example(decompose_netlet)
# # x = netlet

# x2 = subset_netlet(
# 	netlet, 
# 	when_to_subset=c('2008','2009') 
# )

# x1= subset_netlet(
# 	netlet, 
# 	when_to_subset=c('2009')
# )

# plot.netify(netlet, node_color_var='i_polity2', node_size_var='i_log_pop', node_alpha=.1)
# plot.netify(x2, node_color_var='i_polity2', node_size_var='i_log_pop', node_alpha=.1)

# plot_args = list()

# plot_args = list(
# 	layout='fr'
# )

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
		netlet,  nodes_layout=nodes_list )
	######################	

	######################
	# org the netlet into a  and dyadic df with all the
	# relev attributes so that we can plot
	net_dfs <- merge_layout_attribs(
		netlet, nodes_list, edges_list)
	######################	

	######################	
	# adjust plot args
	out <- adjust_plot_args(plot_args, net_dfs, obj_attrs)
	plot_args <- out$plot_args
	net_dfs <- out$net_dfs
	rm(out)
	######################	

	######################	
	# get aesthetic parameters
	ggnet_params = gg_params(
		plot_args
	)
	######################	

	######################	
	# plot
	viz = ggplot()
	######################	

	# build edges #####################
	if(plot_args$add_edges){

		# edge static param list
		edge_static_params = ggnet_params$edge$static

		# curve static param list
		curve_static_params = ggnet_params$curve$static

		# Prepare a list to conditionally build the aes() for edges
		edge_aes_list <- ggnet_params$edge$var

		# curved edges
		if(plot_args$curve_edges){
			viz <- viz + layer(
				data = net_dfs$edge_data,
				mapping = aes(!!!edge_aes_list),
				geom = GeomCurve,
				stat = "identity",
				position = "identity",
				params = curve_static_params,
				inherit.aes = TRUE,
				show.legend = NA
			)
		} else {
			viz <- viz + layer(
				data = net_dfs$edge_data,
				mapping = aes(!!!edge_aes_list),
				geom = GeomSegment,
				stat = "identity",
				position = "identity",
				params = edge_static_params,
				inherit.aes = TRUE,
				show.legend = NA
			)
		}

	}
	######################

	# build nodes #####################	

	# geom_point
	if(plot_args$add_points){

		# node static param list
		node_static_params = ggnet_params$node$static

		# node var param list
		node_aes_list <- ggnet_params$node$var

		# create geom_point
		viz <- viz + layer(
			data = net_dfs$nodal_data, 
			mapping = aes(!!!node_aes_list),  
			geom = GeomPoint,  
			stat = "identity", 
			position = "identity",  
			params = node_static_params,
			inherit.aes = TRUE,  
			show.legend = NA  
		)
	}

	# geom_text
	if(plot_args$add_text){

		# text static param list
		text_static_params = ggnet_params$text$static

		# Prepare a list to conditionally build the aes()
		text_aes_list <- ggnet_params$text$var

		# create geom_point
		viz <- viz + layer(
			data = net_dfs$nodal_data,
			mapping = aes(!!!text_aes_list),
			geom = GeomText,
			stat = "identity",
			position = "identity",
			params = text_static_params,
			inherit.aes = TRUE,
			show.legend = NA
		)

	}

	# geom_label
	if(plot_args$add_label){

		# label static param list
		label_static_params = ggnet_params$label$static

		# Prepare a list to conditionally build the aes()
		label_aes_list <- ggnet_params$label$var

		# create geom_label
		viz <- viz + layer(
			data = net_dfs$nodal_data,
			mapping = aes(!!!label_aes_list),
			geom = GeomLabel,
			stat = "identity",
			position = "identity",
			params = label_static_params,
			inherit.aes = TRUE,
			show.legend = NA
		)
	}
	######################

	# facet instructions #####################
	if(obj_attrs$netify_type!='cross_sec'){
		viz = viz + facet_wrap(~time, scales='free')
	}
	######################			

	# theme changes #####################
	if(plot_args$use_theme_netify){
		viz = viz + theme_netify()
	}
	######################			

	#
	return(viz)
}

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






