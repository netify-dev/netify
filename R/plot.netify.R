#' Plotting method for 'netify' objects
#'
#' This function provides a comprehensive tool to visualize 'netify' objects through various graphical representations including points, edges, texts, and labels. It leverages the capabilities of the 'igraph' and 'ggplot' packages to create customizable network visualizations.
#'
#' @param x A 'netify' object, which contains the network data structured for analysis and visualization.
#' @param ... Additional arguments, which can include but are not limited to:
#' \itemize{
#'   \item \code{point_layout}: Optional, user-provided node layout; if not provided, layout will be generated based on \code{layout} parameter.
#'   \item \code{layout}: Specifies the layout algorithm from 'igraph' to position the nodes if \code{point_layout} is not provided. Available options include:
#'     \itemize{
#'       \item "nicely"
#'       \item "fruchterman.reingold"
#'       \item "kamada.kawai"
#'       \item "random"
#'       \item "circle"
#'       \item "star"
#'       \item "grid"
#'       \item "graphopt"
#'       \item "sugiyama"
#'       \item "drl"
#'       \item "lgl"
#'       \item "bipartite"
#'       \item "tree"
#'       \item "randomly"
#'       \item "dh"
#'       \item "fr"
#'       \item "kk"
#'       \item "gem"
#'       \item "mds"
#'     }
#'   \item \code{remove_isolates}: Logical; if \code{TRUE}, isolates will be removed from the plot. Default is \code{TRUE}.
#'   \item \code{static_actor_positions}: Logical indicating whether to use static positions for actors. Useful in longitudinal studies where node positions should remain consistent over time. If \code{TRUE}, the layout by default is calculated based on a collapsed adjacency matrix across all time points. Users can also specify a specific time point to use as the static layout by setting \code{which_static} to the desired time point.
#'   \item \code{add_edges}: Logical; if \code{TRUE}, edges will be added to the plot. Default is \code{TRUE}.
#'   \item \code{curve_edges}: Logical; if \code{TRUE}, edges will be curved. Default is \code{FALSE}.
#'   \item \code{add_points}: Logical; if \code{TRUE}, points (nodes) will be plotted. Default is \code{TRUE}.
#'   \item \code{add_text}: Logical; if \code{TRUE}, text annotations will be added. Default is \code{FALSE}.
#'   \item \code{add_label}: Logical; if \code{TRUE}, labels will be added. Default is \code{FALSE}.
#'   \item \code{select_text}: A vector of node names to specifically add text to; others will not have text.
#'   \item \code{select_label}: A vector of node names to specifically add labels to; others will not have labels.
#'   \item \code{point_size}: A fixed size for all points, equivalent to \code{geom_point(size = point_size)} in \code{ggplot2}.
#'   \item \code{point_size_var}: A variable from the node data to dynamically size points, equivalent to \code{geom_point(aes(size = point_size_var))} in \code{ggplot2}.
#' }
#'
#' These arguments control various aspects of the plot's appearance and functionality.
#'
#' @return A 'ggplot' plot object that can be further modified or printed.
#'
#' @details
#' \code{plot.netify} creates a network plot with flexible options for customization. It allows the user to specify:
#' \itemize{
#'   \item Whether or not to include edges and how they are rendered (straight or curved).
#'   \item Whether nodes are displayed as points.
#'   \item Whether to annotate nodes with text or labels.
#'   \item Specific nodes to annotate, allowing selective emphasis within the network.
#'   \item Custom layouts if the automatic placement does not suffice.
#' }
#' Additional customization options like \code{point_size} and \code{point_size_var} allow users to apply typical \code{ggplot2} methods directly to network visualizations, facilitating easy integration of familiar graphical adjustments.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @examples
#' # load icews data
#' data(icews)
#'
#' # choose attributes
#' nvars = c('i_polity2', 'i_log_gdp', 'i_log_pop')
#' dvars = c('matlCoop', 'verbConf', 'matlConf')
#'
#' # create a netify object
#' netlet = netify(
#'     icews, actor1 = 'i', actor2 = 'j',
#'     time = 'year',
#'     symmetric = FALSE, weight = 'verbCoop',
#'     mode = 'unipartite', sum_dyads = FALSE,
#'     actor_time_uniform = TRUE, actor_pds = NULL,
#'     diag_to_NA = TRUE, missing_to_zero = TRUE,
#'     nodal_vars = nvars,
#'     dyad_vars = dvars
#' )
#'
#' # add basic summary network stats
#' netlet = add_node_vars(
#'     netlet,
#'     summary_actor(netlet),
#'     actor = 'actor', time = 'time'
#' )
#'
#' # plot the network
#' plot(netlet,
#'      edge_color = 'lightgrey',
#'      point_size_var = 'degree_total',
#'      point_color_var = 'i_polity2'
#' )
#'
#' @import ggplot2
#' @importFrom ggnewscale new_scale_color new_scale_fill new_scale
#'
#' @export plot.netify
#' @export

plot.netify <- function(x, ...) {

	# check if the input is a netify object
	netify_check(x)

	# extract attributes from the netify object
	obj_attrs <- attributes(x)

	# get plot data and parameters for ggplot
	net_plot_info = net_plot_data(x, list(...))

	# extract plot arguments and ggplot parameters
	plot_args = net_plot_info$plot_args
	ggnet_params = net_plot_info$ggnet_params
	net_dfs = net_plot_info$net_dfs

    # Handle weight transformation
    if (!is.null(plot_args$weight_transform)) {
        # Apply transformation to edge data
        weight_col <- attr(x, 'weight')
        if (!is.null(weight_col) && weight_col %in% names(net_dfs$edge_data)) {
            transform_fn <- match.fun(plot_args$weight_transform)
            net_dfs$edge_data[[weight_col]] <- transform_fn(net_dfs$edge_data[[weight_col]])
        }
    }

	# process labels
	scale_labels <- list(
		# Edge labels
		edge_alpha = plot_args$edge_alpha_label,
		edge_color = plot_args$edge_color_label,
		edge_linewidth = plot_args$edge_linewidth_label,
		edge_linetype = plot_args$edge_linetype_label,
		
		# Node labels  
		node_size = plot_args$node_size_label,
		node_color = plot_args$node_color_label,
		node_fill = plot_args$node_fill_label,
		node_shape = plot_args$node_shape_label,
		node_alpha = plot_args$node_alpha_label,
		node_stroke = plot_args$node_stroke_label,
		
		# Text labels
		text_size = plot_args$text_size_label,
		text_color = plot_args$text_color_label,
		text_alpha = plot_args$text_alpha_label,
		
		# Label (box) labels
		label_size = plot_args$label_size_label,
		label_color = plot_args$label_color_label,
		label_fill = plot_args$label_fill_label,
		label_alpha = plot_args$label_alpha_label )

	# initialize a list to store plot components
	components <- list()

	# create the base ggplot object
	components$base <- ggplot()
	
	# pour labels into components
	components$scale_labels <- scale_labels	

	# add edges to the plot if specified
	if (plot_args$add_edges) {

		# static parameters for edges
		edge_static_params = ggnet_params$edge$static

		# static parameters for curved edges
		curve_static_params = ggnet_params$curve$static

		# dynamic aesthetic mappings for edges
		edge_aes_list <- ggnet_params$edge$var

		# add curved edges if specified
		if (plot_args$curve_edges) {
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
			# add straight edges otherwise
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

		# store edge scale names for documentation
		components$edge_scales <- list()
		if (!is.null(plot_args$edge_color_var)) {
			components$edge_scales$color <- plot_args$edge_color_var
		}
		if (!is.null(plot_args$edge_alpha_var)) {
			components$edge_scales$alpha <- plot_args$edge_alpha_var
		}
		if (!is.null(plot_args$edge_linewidth_var)) {
			components$edge_scales$linewidth <- plot_args$edge_linewidth_var
		}
		if (!is.null(plot_args$edge_linetype_var)) {
			components$edge_scales$linetype <- plot_args$edge_linetype_var
		}
	}

	# add nodes to the plot if specified
	if (plot_args$add_points) {

		# static parameters for points
		point_static_params = ggnet_params$point$static

		# dynamic aesthetic mappings for points
		point_aes_list <- ggnet_params$point$var

		# create the geom_point component
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

		# store point scale names
		components$point_scales <- list()
		if (!is.null(plot_args$point_color_var)) {
			components$point_scales$color <- plot_args$point_color_var
		}
		if (!is.null(plot_args$point_fill_var)) {
			components$point_scales$fill <- plot_args$point_fill_var
		}
		if (!is.null(plot_args$point_size_var)) {
			components$point_scales$size <- plot_args$point_size_var
		}
		if (!is.null(plot_args$point_shape_var)) {
			components$point_scales$shape <- plot_args$point_shape_var
		}
		if (!is.null(plot_args$point_alpha_var)) {
			components$point_scales$alpha <- plot_args$point_alpha_var
		}
		if (!is.null(plot_args$point_stroke_var)) {
			components$point_scales$stroke <- plot_args$point_stroke_var
		}
	}

	# add text annotations to the plot if specified
	if (plot_args$add_text) {

		# static parameters for text
		text_static_params = ggnet_params$text$static

		# dynamic aesthetic mappings for text
		text_aes_list <- ggnet_params$text$var

		# create the geom_text component
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
		
		# store text scale names
		components$text_scales <- list()
		if (!is.null(plot_args$text_color_var)) {
			components$text_scales$color <- plot_args$text_color_var
		}
		if (!is.null(plot_args$text_alpha_var)) {
			components$text_scales$alpha <- plot_args$text_alpha_var
		}
		if (!is.null(plot_args$text_size_var)) {
			components$text_scales$size <- plot_args$text_size_var
		}
	}

	# add labels to the plot if specified
	if (plot_args$add_label) {

		# static parameters for labels
		label_static_params = ggnet_params$label$static

		# dynamic aesthetic mappings for labels
		label_aes_list <- ggnet_params$label$var

		# create the geom_label component
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
		
		# store label scale names
		components$label_scales <- list()
		if (!is.null(plot_args$label_color_var)) {
			components$label_scales$color <- plot_args$label_color_var
		}
		if (!is.null(plot_args$label_fill_var)) {
			components$label_scales$fill <- plot_args$label_fill_var
		}
		if (!is.null(plot_args$label_alpha_var)) {
			components$label_scales$alpha <- plot_args$label_alpha_var
		}
		if (!is.null(plot_args$label_size_var)) {
			components$label_scales$size <- plot_args$label_size_var
		}
	}

	# add facets for longitudinal data if applicable
	if (obj_attrs$netify_type != 'cross_sec') {
		components$facets <- facet_wrap(~time, scales = 'free')
	}

	# apply the netify theme if specified
	if (plot_args$use_theme_netify) {
		components$theme <- theme_netify()
	}

	# store additional information for debugging or further customization
	components$net_dfs <- net_dfs
	components$plot_args <- plot_args
	components$obj_attrs <- obj_attrs

	# return components if requested
	if (isTRUE(plot_args$return_components)) {
		class(components) <- c("netify_plot_components", "list")
		return(components)
	}

	# assemble the plot from components
	viz <- components$base

	# add edges to the plot if they exist
	if (!is.null(components$edges)) {
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
		
		# apply edge scale labels if they are defined
		if (!is.null(scale_labels$edge_alpha) && !is.null(components$edge_scales$alpha)) {
			viz <- viz + labs(alpha = scale_labels$edge_alpha)
		}
		if (!is.null(scale_labels$edge_color) && !is.null(components$edge_scales$color)) {
			viz <- viz + labs(color = scale_labels$edge_color)
		}
		if (!is.null(scale_labels$edge_linewidth) && !is.null(components$edge_scales$linewidth)) {
			viz <- viz + labs(linewidth = scale_labels$edge_linewidth)
		}
		if (!is.null(scale_labels$edge_linetype) && !is.null(components$edge_scales$linetype)) {
			viz <- viz + labs(linetype = scale_labels$edge_linetype)
		}
	}

	# reset scales if both edges and nodes have color or fill mappings
	if (!is.null(components$edges) &&
		(!is.null(components$points) || !is.null(components$text) || !is.null(components$label))) {
		if (!is.null(components$edge_scales$color) ||
			!is.null(components$point_scales$color) ||
			!is.null(components$point_scales$fill)) {
			viz <- viz +
				ggnewscale::new_scale_color() +
				ggnewscale::new_scale_fill() +
				ggnewscale::new_scale('alpha')
		}
	}

	# add nodes to the plot if they exist
	if (!is.null(components$points)) {
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
		
		# apply node scale labels if they are defined
		if (!is.null(scale_labels$node_size) && !is.null(components$point_scales$size)) {
			viz <- viz + labs(size = scale_labels$node_size)
		}
		if (!is.null(scale_labels$node_color) && !is.null(components$point_scales$color)) {
			viz <- viz + labs(color = scale_labels$node_color)
		}
		if (!is.null(scale_labels$node_fill) && !is.null(components$point_scales$fill)) {
			viz <- viz + labs(fill = scale_labels$node_fill)
		}
		if (!is.null(scale_labels$node_shape) && !is.null(components$point_scales$shape)) {
			viz <- viz + labs(shape = scale_labels$node_shape)
		}
		if (!is.null(scale_labels$node_alpha) && !is.null(components$point_scales$alpha)) {
			viz <- viz + labs(alpha = scale_labels$node_alpha)
		}
		if (!is.null(scale_labels$node_stroke) && !is.null(components$point_scales$stroke)) {
			viz <- viz + labs(stroke = scale_labels$node_stroke)
		}
	}

	# add text annotations to the plot if they exist
	if (!is.null(components$text)) {
		# reset scales again if needed
		if (!is.null(components$points)) {
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
		
		# apply text scale labels if they are defined
		if (!is.null(scale_labels$text_size) && !is.null(components$text_scales$size)) {
			viz <- viz + labs(size = scale_labels$text_size)
		}
		if (!is.null(scale_labels$text_color) && !is.null(components$text_scales$color)) {
			viz <- viz + labs(color = scale_labels$text_color)
		}
		if (!is.null(scale_labels$text_alpha) && !is.null(components$text_scales$alpha)) {
			viz <- viz + labs(alpha = scale_labels$text_alpha)
		}
	}

	# add labels to the plot if they exist
	if (!is.null(components$label)) {
		# reset scales again if needed
		if (!is.null(components$text) || !is.null(components$points)) {
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
		
		# apply label scale labels if they are defined
		if (!is.null(scale_labels$label_size) && !is.null(components$label_scales$size)) {
			viz <- viz + labs(size = scale_labels$label_size)
		}
		if (!is.null(scale_labels$label_color) && !is.null(components$label_scales$color)) {
			viz <- viz + labs(color = scale_labels$label_color)
		}
		if (!is.null(scale_labels$label_fill) && !is.null(components$label_scales$fill)) {
			viz <- viz + labs(fill = scale_labels$label_fill)
		}
		if (!is.null(scale_labels$label_alpha) && !is.null(components$label_scales$alpha)) {
			viz <- viz + labs(alpha = scale_labels$label_alpha)
		}
	}

	# add facets to the plot if they are defined (useful for longitudinal data)
	if (!is.null(components$facets)) {
		viz <- viz + components$facets
	}

	# add the theme to the plot if it is defined
	if (!is.null(components$theme)) {
		viz <- viz + components$theme
	}

	# 
	if (!isTRUE(plot_args$return_components)) {
		# store components in the plot environment for netify_scale_labels
		viz$plot_env$last_netify_components <- components
	}

	# return the final assembled plot
	return(viz)
}