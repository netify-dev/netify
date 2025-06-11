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
#'     icews, actor1='i', actor2='j',
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
	# Build components separately
	components <- list()
	
	# Base plot
	components$base <- ggplot()
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
			components$edges <- list(
				geom = GeomCurve,
				data = net_dfs$edge_data,
				mapping = aes(!!!edge_aes_list),
				params = curve_static_params,
				stat = "identity",
				position = "identity",
				inherit.aes = TRUE,
				show.legend = NA
			)
		} else {
			components$edges <- list(
				geom = GeomSegment,
				data = net_dfs$edge_data,
				mapping = aes(!!!edge_aes_list),
				params = edge_static_params,
				stat = "identity",
				position = "identity",
				inherit.aes = TRUE,
				show.legend = NA
			)
		}
		
		# Store edge scale names for documentation
		components$edge_scales <- list()
		if(!is.null(plot_args$edge_color_var)) {
			components$edge_scales$color <- plot_args$edge_color_var
		}
		if(!is.null(plot_args$edge_alpha_var)) {
			components$edge_scales$alpha <- plot_args$edge_alpha_var
		}
		if(!is.null(plot_args$edge_linewidth_var)) {
			components$edge_scales$linewidth <- plot_args$edge_linewidth_var
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

		# create geom_point component
		components$points <- list(
			geom = GeomPoint,
			data = net_dfs$nodal_data,
			mapping = aes(!!!point_aes_list),
			params = point_static_params,
			stat = "identity",
			position = "identity",
			inherit.aes = TRUE,
			show.legend = NA
		)
		
		# Store point scale names
		components$point_scales <- list()
		if(!is.null(plot_args$point_color_var)) {
			components$point_scales$color <- plot_args$point_color_var
		}
		if(!is.null(plot_args$point_fill_var)) {
			components$point_scales$fill <- plot_args$point_fill_var
		}
		if(!is.null(plot_args$point_size_var)) {
			components$point_scales$size <- plot_args$point_size_var
		}
	}

	# geom_text
	if(plot_args$add_text){

		# text static param list
		text_static_params = ggnet_params$text$static

		# Prepare a list to conditionally build the aes()
		text_aes_list <- ggnet_params$text$var

		# create geom_text component
		components$text <- list(
			geom = GeomText,
			data = net_dfs$nodal_data,
			mapping = aes(!!!text_aes_list),
			params = text_static_params,
			stat = "identity",
			position = "identity",
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

		# create geom_label component
		components$label <- list(
			geom = GeomLabel,
			data = net_dfs$nodal_data,
			mapping = aes(!!!label_aes_list),
			params = label_static_params,
			stat = "identity",
			position = "identity",
			inherit.aes = TRUE,
			show.legend = NA
		)
	}
	######################

	# facet instructions #####################
	if(obj_attrs$netify_type!='cross_sec'){
		components$facets <- facet_wrap(~time, scales='free')
	}
	######################			

	# theme changes #####################
	if(plot_args$use_theme_netify){
		components$theme <- theme_netify()
	}
	######################			
	
	# Store additional info that might be useful
	components$net_dfs <- net_dfs
	components$plot_args <- plot_args
	components$obj_attrs <- obj_attrs

	######################
	# If return_components is TRUE, return the list
	if(isTRUE(plot_args$return_components)) {
		class(components) <- c("netify_plot_components", "list")
		return(components)
	}
	######################
	
	######################
	# Otherwise, assemble the plot as before
	viz <- components$base
	
	# Add edges
	if(!is.null(components$edges)) {
		viz <- viz + layer(
			geom = components$edges$geom,
			data = components$edges$data,
			mapping = components$edges$mapping,
			stat = components$edges$stat,
			position = components$edges$position,
			params = components$edges$params,
			inherit.aes = components$edges$inherit.aes,
			show.legend = components$edges$show.legend
		)
	}
	
	# Reset scales if we have both edges and nodes with color/fill mappings
	if(!is.null(components$edges) && 
	   (!is.null(components$points) || !is.null(components$text) || !is.null(components$label))) {
		if(!is.null(components$edge_scales$color) || 
		   !is.null(components$point_scales$color) ||
		   !is.null(components$point_scales$fill)) {
			viz <- viz + 
				ggnewscale::new_scale_color() + 
				ggnewscale::new_scale_fill() +
				ggnewscale::new_scale('alpha')
		}
	}
	
	# Add nodes
	if(!is.null(components$points)) {
		viz <- viz + layer(
			geom = components$points$geom,
			data = components$points$data,
			mapping = components$points$mapping,
			stat = components$points$stat,
			position = components$points$position,
			params = components$points$params,
			inherit.aes = components$points$inherit.aes,
			show.legend = components$points$show.legend
		)
	}
	
	# Add text
	if(!is.null(components$text)) {
		# Reset scales again if needed
		if(!is.null(components$points)) {
			viz <- viz + 
				ggnewscale::new_scale_color() + 
				ggnewscale::new_scale('alpha') +
				ggnewscale::new_scale('size')
		}
		
		viz <- viz + layer(
			geom = components$text$geom,
			data = components$text$data,
			mapping = components$text$mapping,
			stat = components$text$stat,
			position = components$text$position,
			params = components$text$params,
			inherit.aes = components$text$inherit.aes,
			show.legend = components$text$show.legend
		)
	}
	
	# Add label
	if(!is.null(components$label)) {
		# Reset scales again if needed
		if(!is.null(components$text) || !is.null(components$points)) {
			viz <- viz + 
				ggnewscale::new_scale_color() + 
				ggnewscale::new_scale('alpha') +
				ggnewscale::new_scale_fill() +
				ggnewscale::new_scale('size')
		}
		
		viz <- viz + layer(
			geom = components$label$geom,
			data = components$label$data,
			mapping = components$label$mapping,
			stat = components$label$stat,
			position = components$label$position,
			params = components$label$params,
			inherit.aes = components$label$inherit.aes,
			show.legend = components$label$show.legend
		)
	}
	
	# Add facets
	if(!is.null(components$facets)) {
		viz <- viz + components$facets
	}
	
	# Add theme
	if(!is.null(components$theme)) {
		viz <- viz + components$theme
	}
	
	return(viz)
	######################
}

# Add a print method for the components
print.netify_plot_components <- function(x, ...) {
	cat("Netify plot components:\n")
	cat("  Base plot: ggplot object\n")
	if(!is.null(x$edges)) cat("  Edges: geom_segment/geom_curve layer\n")
	if(!is.null(x$points)) cat("  Points: geom_point layer\n")
	if(!is.null(x$text)) cat("  Text: geom_text layer\n")
	if(!is.null(x$label)) cat("  Labels: geom_label layer\n")
	if(!is.null(x$facets)) cat("  Facets: facet_wrap layer\n")
	if(!is.null(x$theme)) cat("  Theme: theme_netify\n")
	cat("\nUse build_netify_plot() to assemble or build manually with ggplot2.\n")
}

# Helper function to build the plot from components
build_netify_plot <- function(components, 
                             edge_color_scale = NULL,
                             edge_alpha_scale = NULL,
                             edge_size_scale = NULL,
                             node_color_scale = NULL,
                             node_fill_scale = NULL,
                             node_size_scale = NULL,
                             ...) {
	
	if(!inherits(components, "netify_plot_components")) {
		stop("Input must be netify_plot_components from plot(..., return_components = TRUE)")
	}
	
	# Start with base
	p <- components$base
	
	# Add edges with custom scales
	if(!is.null(components$edges)) {
		p <- p + layer(
			geom = components$edges$geom,
			data = components$edges$data,
			mapping = components$edges$mapping,
			stat = components$edges$stat,
			position = components$edges$position,
			params = components$edges$params,
			inherit.aes = components$edges$inherit.aes,
			show.legend = components$edges$show.legend
		)
		
		# Add edge scales
		if(!is.null(edge_color_scale)) p <- p + edge_color_scale
		if(!is.null(edge_alpha_scale)) p <- p + edge_alpha_scale
		if(!is.null(edge_size_scale)) p <- p + edge_size_scale
	}
	
	# Reset scales before nodes
	if(!is.null(components$edges) && !is.null(components$points)) {
		p <- p + 
			ggnewscale::new_scale_color() + 
			ggnewscale::new_scale_fill() +
			ggnewscale::new_scale('alpha')
	}
	
	# Add nodes with custom scales
	if(!is.null(components$points)) {
		p <- p + layer(
			geom = components$points$geom,
			data = components$points$data,
			mapping = components$points$mapping,
			stat = components$points$stat,
			position = components$points$position,
			params = components$points$params,
			inherit.aes = components$points$inherit.aes,
			show.legend = components$points$show.legend
		)
		
		# Add node scales
		if(!is.null(node_color_scale)) p <- p + node_color_scale
		if(!is.null(node_fill_scale)) p <- p + node_fill_scale
		if(!is.null(node_size_scale)) p <- p + node_size_scale
	}
	
	# Add other components
	if(!is.null(components$facets)) p <- p + components$facets
	if(!is.null(components$theme)) p <- p + components$theme
	
	# Add any additional ggplot2 elements passed in ...
	p <- p + list(...)
	
	return(p)
}