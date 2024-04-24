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
	# plot
	viz = ggplot()
	######################	

	# build nodes #####################	

	# geom_point
	if(plot_args$add_points){

		# node static param list
		node_static_params = list(
			alpha = plot_args$node_alpha,
			color = plot_args$node_color,
			fill = plot_args$node_fill,
			shape = plot_args$node_shape,
			size = plot_args$node_size,
			stroke = plot_args$node_stroke )

		# Prepare a list to conditionally build the aes()
		node_aes_list <- list(x = ~x, y = ~y)

		# Add conditional aesthetics based on non-NULL entries
		if(!is.null(plot_args$node_alpha_var)){
			node_aes_list$alpha = formula(
				paste0('~', plot_args$node_alpha_var))
			node_static_params = node_static_params[
				-which(names(node_static_params)=='node_alpha')]
		}
		if(!is.null(plot_args$node_color_var)){
			node_aes_list$color = formula(
				paste0('~', plot_args$node_color_var))
			node_static_params = node_static_params[
				-which(names(node_static_params)=='node_color')]
		}
		if(!is.null(plot_args$node_fill_var)){
			node_aes_list$fill = formula(
				paste0('~', plot_args$node_fill_var))
			node_static_params = node_static_params[
				-which(names(node_static_params)=='node_fill')]
		}
		if(!is.null(plot_args$node_shape_var)){
			node_aes_list$shape = formula(
				paste0('~', plot_args$node_shape_var))
			node_static_params = node_static_params[
				-which(names(node_static_params)=='node_shape')]
		}
		if(!is.null(plot_args$node_size_var)){
			node_aes_list$size = formula(
				paste0('~', plot_args$node_size_var))
			node_static_params = node_static_params[
				-which(names(node_static_params)=='node_size')]
		}
		if(!is.null(plot_args$node_stroke_var)){
			node_aes_list$stroke = formula(
				paste0('~', plot_args$node_stroke_var))
			node_static_params = node_static_params[
				-which(names(node_static_params)=='node_stroke')]
		}

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
		text_static_params = list(
			check_overlap = plot_args$check_overlap,
			alpha = plot_args$text_alpha,
			color = plot_args$text_color,
			size = plot_args$text_size,
			family = plot_args$text_family,
			fontface = plot_args$text_fontface,
			angle = plot_args$text_angle )

		# Prepare a list to conditionally build the aes()
		text_aes_list <- list(
			x = ~x, y = ~y,
			label = ~name )

		# Add conditional aesthetics based on non-NULL entries
		if(!is.null(plot_args$text_alpha_var)){
			text_aes_list$alpha <- formula(
				paste0('~', plot_args$text_alpha_var))
			text_static_params = text_static_params[
				-which(names(text_static_params)=='text_alpha')]
		}
		if(!is.null(plot_args$text_color_var)){
			text_aes_list$color <- formula(
				paste0('~', plot_args$text_color_var))
			text_static_params = text_static_params[
				-which(names(text_static_params)=='text_color')]
		}
		if(!is.null(plot_args$text_size_var)){
			text_aes_list$size <- formula(
				paste0('~', plot_args$text_size_var))
			text_static_params = text_static_params[
				-which(names(text_static_params)=='text_size')]
		}

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
		label_static_params = list(
			check_overlap = plot_args$check_overlap,
			alpha = plot_args$label_alpha,
			color = plot_args$label_color,
			fill = plot_args$label_fill,
			size = plot_args$label_size,
			family = plot_args$label_family,
			fontface = plot_args$label_fontface,
			angle = plot_args$label_angle,
			hjust = plot_args$label_hjust,
			vjust = plot_args$label_vjust,
			lineheight = plot_args$label_lineheight )

		# Prepare a list to conditionally build the aes()
		label_aes_list <- list(
			x = ~x, y = ~y,
			label = ~name )

		# Add conditional aesthetics based on non-NULL entries
		if(!is.null(plot_args$label_alpha_var)){
			label_aes_list$alpha <- formula(
				paste0('~', plot_args$label_alpha_var) )
			label_static_params = label_static_params[
				-which(names(label_static_params)=='label_alpha')]
		}
		if(!is.null(plot_args$label_color_var)){
			label_aes_list$color <- formula(
				paste0('~', plot_args$label_color_var) )
			label_static_params = label_static_params[
				-which(names(label_static_params)=='label_color')]
		}
		if(!is.null(plot_args$label_fill_var)){
			label_aes_list$fill <- formula(
				paste0('~', plot_args$label_fill_var) )
			label_static_params = label_static_params[
				-which(names(label_static_params)=='label_fill')]
		}
		if(!is.null(plot_args$label_size_var)){
			label_aes_list$size <- formula(
				paste0('~', plot_args$label_size_var) )
			label_static_params = label_static_params[
				-which(names(label_static_params)=='label_size')]
		}

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

	# build edges #####################
	if(plot_args$add_edges){

		# edge static param list
		edge_static_params = list(
			color = plot_args$edge_color,
			linewidth = plot_args$edge_linewidth,
			linetype = plot_args$edge_linetype,
			alpha = plot_args$edge_alpha,
			arrow = plot_args$edge_arrow,
			arrow.fill = plot_args$edge_arrow_fill,
			lineend=plot_args$edge_lineend,
			linejoin=plot_args$edge_linejoin
		)

		# curve static param list
		curve_static_params = edge_static_params
		curve_static_params$curvature = plot_args$edge_curvature
		curve_static_params$angle = plot_args$edge_angle
		curve_static_params$ncp = plot_args$edge_ncp

		# Prepare a list to conditionally build the aes() for edges
		edge_aes_list <- list(
			x = ~x1, y = ~y1,
			xend = ~x2, yend = ~y2
			)

		# Add conditional aesthetics based on non-NULL entries
		if(!is.null(plot_args$edge_alpha_var)){
			edge_aes_list$alpha <- formula(
				paste0('~', plot_args$edge_alpha_var) )
			edge_static_params = edge_static_params[
				-which(names(edge_static_params)=='edge_alpha')]
		}
		if(!is.null(plot_args$edge_color_var)){
			edge_aes_list$color <- formula(
				paste0('~', plot_args$edge_color_var) )
			edge_static_params = edge_static_params[
				-which(names(edge_static_params)=='edge_color')]
		}
		if(!is.null(plot_args$edge_linetype_var)){
			edge_aes_list$linetype <- formula(
				paste0('~', plot_args$edge_linetype_var) )
			edge_static_params = edge_static_params[
				-which(names(edge_static_params)=='edge_linetype')]
		}
		if(!is.null(plot_args$edge_linewidth_var)){
			edge_aes_list$linewidth <- formula(
				paste0('~', plot_args$edge_linewidth_var) )
			edge_static_params = edge_static_params[
				-which(names(edge_static_params)=='edge_linewidth')]
		}

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

	# ######################
	# # 
	# return(paste0("patience ", Sys.info()['user']))


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

# }

# }

# for the future
# library(ggplot2)
# library(rlang)  # for sym() and !! (bang-bang operator)

# # add aesthetics only if they are not NULL
# add_conditional_aes <- function(aes_list, plot_args, arg_name) {
#   if (!is.null(plot_args[[arg_name]])) {
#     aes_list[[arg_name]] <- !!sym(plot_args[[arg_name]])
#   }
#   aes_list
# }

# # start with mandatory aesthetics
# aes_list <- aes(x = x, y = y)

# # dynamically add optional aesthetics
# aes_list <- add_conditional_aes(aes_list, plot_args, 'node_alpha_var')
# aes_list <- add_conditional_aes(aes_list, plot_args, 'node_color_var')
# aes_list <- add_conditional_aes(aes_list, plot_args, 'node_shape_var')
# aes_list <- add_conditional_aes(aes_list, plot_args, 'node_size_var')
# aes_list <- add_conditional_aes(aes_list, plot_args, 'node_stroke_var')

# viz <- viz + geom_point(
#     data = net_dfs$nodal_data,
#     aes = aes_list,
#     alpha = plot_args$node_alpha,
#     color = plot_args$node_color,
#     fill = plot_args$node_color,
#     size = plot_args$node_size,
#     stroke = plot_args$node_stroke
# )



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






