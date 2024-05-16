#' Plotting method for 'netify' objects
#'
#' This function provides a comprehensive tool to visualize 'netify' objects through various graphical representations including points, edges, texts, and labels. It leverages the capabilities of the 'igraph' and 'ggplot' packages to create customizable network visualizations.
#'
#' @param x A 'netify' object, which contains the network data structured for analysis and visualization.
#' @param ... Additional arguments, which can include but are not limited to:
#'        - `point_layout`: Optional, user-provided node layout; if not provided, layout will be generated based on `layout` parameter.
#'        - `layout`: Specifies the layout algorithm from 'igraph' to position the nodes if `point_layout` is not provided.
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
#' netlet = add_nodal(
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

	# if more than one layer tell user they must specify a single layer
	if(length(attributes(x)$layers) > 1){
		cli::cli_alert_danger(
			'Error: This object has multiple layers. 
			`plot` does not currently support multilayer `netify` inputs.
			Please use the `filter_layers` function to create a `netify` object with a single layer.' )
		stop() }

	# get plot args
	plot_args = list(...)	
	######################

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
	if(is.null(plot_args$point_layout)){
		nodes_list = get_node_layout(
			netlet, 
			layout=plot_args$layout, 
			static_actor_positions=plot_args$static_actor_positions,
			which_static=plot_args$which_static,
			seed=plot_args$seed
			)
	} else { 
		nodes_list = plot_args$point_layout
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
	ggnet_params = gg_params( plot_args )
	######################	

	######################	
	# remove isolates
	if(plot_args$remove_isolates){

		# get actor summary
		actor_summ <- summary_actor(netlet)

		# figure out what to keep and need to 
		# do separate for symmetric and asymmetric
		if(obj_attrs$symmetric){
			to_keep <- actor_summ[actor_summ$degree > 0,]
		} else {
			to_keep <- actor_summ[actor_summ$degree_total > 0 ,]}

		# add time var to to_keep if cross_sec
		if(obj_attrs$netify_type == 'cross_sec'){
			to_keep$time = net_dfs$nodal_data$time[1] }

		# create id vars
		to_keep$id = with(to_keep, paste(actor, time, sep='_'))
		net_dfs$nodal_data$id = with(net_dfs$nodal_data, paste(name, time, sep='_'))
		# net_dfs$edge_data$from_id = with(net_dfs$edge_data, paste(from, time, sep='_'))
		# net_dfs$edge_data$to_id = with(net_dfs$edge_data, paste(to, time, sep='_'))

		# subset
		net_dfs$nodal_data <- net_dfs$nodal_data[net_dfs$nodal_data$id %in% to_keep$id,]
	}
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
