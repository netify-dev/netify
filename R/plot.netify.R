#' Plotting method for 'netify' objects
#'
#' This function provides a comprehensive tool to visualize 'netify' objects through various graphical representations including points, edges, texts, and labels. It leverages the capabilities of the 'igraph' and 'ggplot' packages to create customizable network visualizations.
#'
#' @param x A 'netify' object, which contains the network data structured for analysis and visualization.
#' @param ... Additional arguments, which can include but are not limited to:
#'        - `point_layout`: Optional, user-provided node layout; if not provided, layout will be generated based on `layout` parameter.
#'        - `layout`: Specifies the layout algorithm from 'igraph' to position the nodes if `point_layout` is not provided. Available options include "nicely", "fruchterman.reingold", "kamada.kawai", "random", "circle", "star", "grid", "graphopt", "sugiyama", "drl", "lgl", "bipartite", "tree", "randomly", "dh", "fr", "kk", "gem", and "mds".
#'        - `remove_isolates`: Logical; if TRUE, isolates will be removed from the plot. Default is TRUE.
#'        - `static_actor_positions` Logical indicating whether to use static positions for actors.
#'        Useful in longitudinal studies where node positions should remain consistent over time.
#'        If TRUE, the layout by default is calculated based on a collapsed adjacency matrix across 
#'        all time points. Users can also specify a specific time point to use as the static layout by 
#'        setting `which_static` to the desired time point. 
#'        - `add_edges`: Logical; if TRUE, edges will be added to the plot. Default is TRUE.
#'        - `curve_edges`: Logical; if TRUE, edges will be curved. Default is FALSE.
#'        - `add_points`: Logical; if TRUE, points (nodes) will be plotted. Default is TRUE.
#'        - `add_text`: Logical; if TRUE, text annotations will be added. Default is FALSE.
#'        - `add_label`: Logical; if TRUE, labels will be added. Default is FALSE.
#'        - `select_text`: A vector of node names to specifically add text to; others will not have text.
#'        - `select_label`: A vector of node names to specifically add labels to; others will not have labels.
#'        - `point_size`: A fixed size for all points, equivalent to `geom_point(size = point_size)` in `ggplot2`.
#'        - `point_size_var`: A variable from the node data to dynamically size points, equivalent to `geom_point(aes(size = point_size_var))` in `ggplot2`.
#'        These arguments control various aspects of the plot's appearance and functionality.
#'
#' @return A 'ggplot' plot object that can be further modified or printed.
#'
#' @details
#' `plot.netify` creates a network plot with flexible options for customization. It allows the user to specify:
#' - Whether or not to include edges and how they are rendered (straight or curved).
#' - Whether nodes are displayed as points.
#' - Whether to annotate nodes with text or labels.
#' - Specific nodes to annotate, allowing selective emphasis within the network.
#' - Custom layouts if the automatic placement does not suffice.
#' Additional customization options like `point_size` and `point_size_var` allow users to apply typical `ggplot2` methods directly to network visualizations, facilitating easy integration of familiar graphical adjustments.
#' 
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @examples
#' # load icews data
#' data(icews)
#' 
#' # choose attributes
#' nvars = c( 'i_polity2', 'i_log_gdp', 'i_log_pop' )
#' dvars = c( 'matlCoop', 'verbConf', 'matlConf' )
#'
#' # create a netify object
#' netlet = netify(
#'     dyad_data=icews, actor1='i', actor2='j',
#'     time = 'year',
#'     symmetric=FALSE, weight='verbCoop',
#'     mode='unipartite', sum_dyads=FALSE,
#'     actor_time_uniform=TRUE, actor_pds=NULL,
#'     diag_to_NA=TRUE, missing_to_zero=TRUE,
#'     nodal_vars = nvars, 
#'     dyad_vars = dvars
#' )
#'
#' # add basic summary network stats
#' netlet = add_node_vars(
#'     netlet, 
#'     summary_actor(netlet),
#'     actor='actor', time='time')
#' 
#' # plot the network
#' plot(netlet, 
#'      edge_color='lightgrey', 
#'      point_size_var = 'degree_total',
#'      point_color_var = 'i_polity2'
#'      )
#' 
#' @import ggplot2
#' @importFrom ggnewscale new_scale_color new_scale_fill new_scale 
#' 
#' @export plot.netify
#' @export

plot.netify <- function(x, ...){

	######################
	# check if netify object
	netify_check(x)

	# pull out attrs
	obj_attrs <- attributes(x)
	######################	

	######################
	# get plot data and parameters for gg
	net_plot_info = net_plot_data(x, list(...))

	# pull out plot args
	plot_args = net_plot_info$plot_args
	ggnet_params = net_plot_info$ggnet_params
	net_dfs = net_plot_info$net_dfs
	######################	

	######################	
	# start on plot
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

		# point static param list
		point_static_params = ggnet_params$point$static

		# point var param list
		point_aes_list <- ggnet_params$point$var

		# make room for new scales
		viz <- viz + 
			ggnewscale::new_scale_color() + 
			ggnewscale::new_scale_fill() +
			ggnewscale::new_scale('alpha')

		# create geom_point
		viz <- viz + layer(
			data = net_dfs$nodal_data, 
			mapping = aes(!!!point_aes_list),  
			geom = GeomPoint,  
			stat = "identity", 
			position = "identity",  
			params = point_static_params,
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

		# make room for new scales
		viz <- viz + 
			ggnewscale::new_scale_color() + 
			ggnewscale::new_scale('alpha') +
			ggnewscale::new_scale('size')

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

		# make room for new scales
		viz <- viz + 
			ggnewscale::new_scale_color() + 
			ggnewscale::new_scale('alpha') +
			ggnewscale::new_scale_fill() +
			ggnewscale::new_scale('size')

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
