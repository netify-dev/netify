#' Plotting method for netify objects
#'
#' Creates customizable network visualizations from netify objects using ggplot2. 
#' Supports cross-sectional and longitudinal networks with extensive options for 
#' mapping network attributes to visual properties.
#'
#' @param x A 'netify' object containing network data to visualize.
#' @param ... Additional arguments controlling plot appearance:
#' 
#' @section Layout Parameters:
#' \describe{
#'   \item{\code{layout}}{Character string specifying the igraph layout algorithm. 
#'     Options include: \code{"nicely"} (default), \code{"fr"} (Fruchterman-Reingold), 
#'     \code{"kk"} (Kamada-Kawai), \code{"circle"}, \code{"star"}, \code{"grid"}, 
#'     \code{"tree"}, \code{"bipartite"} (for bipartite networks), \code{"randomly"}, 
#'     and others. See \code{\link{get_node_layout}} for full list.}
#'   \item{\code{point_layout}}{Optional data.frame or list of data.frames containing 
#'     pre-computed node positions with columns 'actor', 'x', and 'y'. Overrides 
#'     \code{layout} if provided.}
#'   \item{\code{static_actor_positions}}{Logical. For longitudinal networks, should 
#'     node positions remain constant across time? Default is \code{FALSE}.}
#'   \item{\code{which_static}}{Integer. When \code{static_actor_positions = TRUE}, 
#'     which time period's layout to use as template? If \code{NULL} (default), 
#'     creates composite layout from all time periods.}
#'   \item{\code{seed}}{Integer for reproducible layouts. Default is 6886.}
#' }
#' 
#' @section Display Control:
#' \describe{
#'   \item{\code{add_edges}}{Logical. Display edges? Default is \code{TRUE}.}
#'   \item{\code{add_points}}{Logical. Display nodes as points? Default is \code{TRUE}.}
#'   \item{\code{add_text}}{Logical. Add text labels to nodes? Default is \code{FALSE}.}
#'   \item{\code{add_label}}{Logical. Add boxed labels to nodes? Default is \code{FALSE}.}
#'   \item{\code{remove_isolates}}{Logical. Remove unconnected nodes? Default is \code{TRUE}.}
#'   \item{\code{curve_edges}}{Logical. Use curved edges? Default is \code{FALSE}.}
#'   \item{\code{use_theme_netify}}{Logical. Apply netify theme? Default is \code{TRUE}.}
#' }
#'
#' @section Node Aesthetics:
#' 
#' Fixed aesthetics (same for all nodes):
#' \describe{
#'   \item{\code{node_size} or \code{point_size}}{Numeric. Size of all nodes.}
#'   \item{\code{node_color} or \code{point_color}}{Color of node borders.}
#'   \item{\code{node_fill} or \code{point_fill}}{Fill color of nodes (note that fill will only work with certain shapes).}
#'   \item{\code{node_shape} or \code{point_shape}}{Shape of nodes (see \code{?pch}).}
#'   \item{\code{node_alpha} or \code{point_alpha}}{Transparency (0-1).}
#'   \item{\code{node_stroke} or \code{point_stroke}}{Width of node borders.}
#' }
#' 
#' Variable aesthetics (mapped to data):
#' \describe{
#'   \item{\code{node_size_by} or \code{point_size_var}}{Column name for size mapping.}
#'   \item{\code{node_color_by} or \code{point_color_var}}{Column name for border color.}
#'   \item{\code{node_fill_by} or \code{point_fill_var}}{Column name for fill color (note that fill will only work with certain shapes).}
#'   \item{\code{node_shape_by} or \code{point_shape_var}}{Column name for shape.}
#'   \item{\code{node_alpha_by} or \code{point_alpha_var}}{Column name for transparency.}
#' }
#' 
#' @section Edge Aesthetics:
#' 
#' Fixed aesthetics:
#' \describe{
#'   \item{\code{edge_color}}{Color for all edges. Default is "black".}
#'   \item{\code{edge_linewidth}}{Width for all edges. Default is 0.5.}
#'   \item{\code{edge_linetype}}{Line type (1=solid, 2=dashed, etc.).}
#'   \item{\code{edge_alpha}}{Transparency (0-1).}
#'   \item{\code{edge_curvature}}{Curvature amount when \code{curve_edges = TRUE}.}
#'   \item{\code{edge_arrow}}{Arrow specification for directed networks. Example: 
#'     \code{arrow(length = unit(0.2, "cm"))}.}
#'   \item{\code{adjust_arrow_endpoints}}{Logical. Should arrow endpoints be adjusted 
#'     to stop at node boundaries? Default is \code{FALSE}. Only affects directed networks.}
#'   \item{\code{edge_arrow_gap}}{Numeric. Additional gap between arrow tip and node boundary 
#'     as a proportion of node radius (0-1). Default is 0.2. Only used when 
#'     \code{adjust_arrow_endpoints = TRUE}.}
#'   \item{\code{edge_arrow_size_scale}}{Numeric. Scale factor for converting node sizes to 
#'     coordinate units. If \code{NULL} (default), automatically calculated based on plot range.}
#' }
#' 
#' Variable aesthetics:
#' \describe{
#'   \item{\code{edge_color_by} or \code{edge_color_var}}{Column name for color mapping.}
#'   \item{\code{edge_linewidth_by} or \code{edge_linewidth_var}}{Column name for width.}
#'   \item{\code{edge_linetype_by} or \code{edge_linetype_var}}{Column name for line type.}
#'   \item{\code{edge_alpha_by} or \code{edge_alpha_var}}{Column name for transparency.
#'     For weighted networks, defaults to the weight variable if not specified.}
#' }
#' 
#' @section Text and Label Options:
#' 
#' Selective labeling:
#' \describe{
#'   \item{\code{select_text}}{Character vector of node names to show as text.}
#'   \item{\code{select_text_display}}{Alternative text to display (same length as 
#'     \code{select_text}).}
#'   \item{\code{select_label}}{Character vector of node names to show with boxes.}
#'   \item{\code{select_label_display}}{Alternative labels (same length as 
#'     \code{select_label}).}
#' }
#' 
#' Text aesthetics:
#' \describe{
#'   \item{\code{text_size}}{Fixed size for all text. Default is 3.88.}
#'   \item{\code{text_color}}{Fixed color for all text. Default is "black".}
#'   \item{\code{text_alpha}}{Fixed transparency for text.}
#'   \item{\code{text_size_by}}{Variable to map to text size.}
#'   \item{\code{text_color_by}}{Variable to map to text color.}
#' }
#' 
#' Label (boxed text) aesthetics have similar parameters with \code{label_} prefix.
#' 
#' @section Scale Labels:
#' 
#' Customize legend titles:
#' \describe{
#'   \item{\code{node_size_label} or \code{point_size_label}}{Legend title for size.}
#'   \item{\code{node_color_label} or \code{point_color_label}}{Legend title for color.}
#'   \item{\code{edge_alpha_label}}{Legend title for edge transparency.}
#'   \item{\code{edge_color_label}}{Legend title for edge color.}
#' }
#' 
#' @section Special Parameters:
#' \describe{
#'   \item{\code{weight_transform}}{Function to transform edge weights before plotting.
#'     Example: \code{log1p} for log(x+1) transformation. Applied before mapping to
#'     aesthetics.}
#'   \item{\code{check_overlap}}{Logical. Avoid text overlap? Default is \code{TRUE}.}
#'   \item{\code{return_components}}{Logical. Return plot components instead of 
#'     assembled plot? Useful for manual customization. Default is \code{FALSE}.}
#'   \item{\code{palette}}{Character string for color palette, see \code{list_palettes()}.}
#'   \item{\code{style}}{A style function (e.g., \code{style_budapest}) or its name as a 
#'     string. Applies a complete visual style including colors, shapes, and layout preferences.}
#' }
#'
#' @return 
#' A ggplot2 object that can be further customized with additional layers, scales, 
#' themes, etc. For longitudinal networks, includes facets for each time period.
#' 
#' If \code{return_components = TRUE}, returns a list of plot components that can 
#' be manually assembled or modified.
#'
#' @details
#' 
#' \strong{Naming Conventions:}
#' 
#' The function supports two naming styles for parameters:
#' \itemize{
#'   \item \strong{Recommended}: Use \code{node_*} for node attributes and 
#'     \code{*_by} for variable mappings (e.g., \code{node_size_by = "degree"})
#'   \item \strong{Legacy}: Use \code{point_*} for nodes and \code{*_var} for 
#'     variables (e.g., \code{point_size_var = "degree"})
#' }
#' 
#' \strong{Default Behaviors:}
#' \itemize{
#'   \item For weighted networks, edge transparency maps to weight by default
#'   \item For directed networks, arrows are added automatically
#'   \item For longitudinal networks, time periods are shown as facets
#'   \item Isolates are removed by default (set \code{remove_isolates = FALSE} to keep)
#' }
#' 
#' \strong{Customization Tips:}
#' \itemize{
#'   \item Use \code{weight_transform} to handle skewed weight distributions
#'   \item Combine fixed and variable aesthetics (e.g., fixed color with variable size)
#'   \item Add ggplot2 layers after the plot call for further customization
#'   \item Use \code{select_text} for selective labeling in dense networks
#' }
#'
#' @examples
#' # Load example data
#' data(icews)
#'
#' # Basic cross-sectional network
#' icews_10 <- icews[icews$year == 2010,]
#' net_10 <- netify(
#'   icews_10,
#'   actor1 = 'i', actor2 = 'j',
#'   symmetric = FALSE,
#'   weight = 'verbCoop'
#' )
#' 
#' # Simple plot
#' plot(net_10)
#' 
#' # add nodal stats to netlet
#' net_10 <- add_node_vars(
#'   net_10, 
#'   summary_actor(net_10), 
#'   'actor'
#' )
#' 
#' # Customized plot with new naming convention
#' plot(net_10,
#'   edge_color = 'lightgrey',
#'   node_size_by = 'degree_total',          # Instead of point_size_var
#'   node_color = 'steelblue',
#'   edge_alpha_by = 'verbCoop',       # Instead of edge_alpha_var
#'   node_size_label = 'Degree',
#'   edge_alpha_label = 'Verbal Cooperation'
#' )
#' 
#' # Longitudinal network example
#' net_longit <- netify(
#'   icews,
#'   actor1 = 'i', actor2 = 'j',
#'   time = 'year',
#'   symmetric = FALSE,
#'   weight = 'verbCoop',
#'   nodal_vars = c('i_polity2', 'i_log_gdp')
#' )
#' 
#' # Add network statistics
#' net_longit <- add_node_vars(
#'   net_longit,
#'   summary_actor(net_longit),
#'   actor = 'actor', 
#'   time = 'time'
#' )
#' 
#' # Plot with multiple aesthetics
#' plot(net_longit,
#'   # Edges
#'   edge_color = 'grey70',
#'   weight_transform = log1p,         # Transform weights
#'   # Nodes  
#'   node_size_by = 'degree_total',
#'   node_color_by = 'i_polity2',
#'   # Labels
#'   node_size_label = 'Total Degree',
#'   node_color_label = 'Polity Score',
#'   edge_alpha_label = 'Log(Verbal Coop.)',
#'   # Layout
#'   static_actor_positions = TRUE     # Keep positions constant
#' )
#' 
#' # Selective labeling example
#' plot(net_10,
#'   node_size_by = 'degree_total',
#'   select_text = c('United States', 'China', 'Russian Federation'),
#'   text_size = 3,
#'   text_color = 'darkred'
#' )
#' 
#' # choose alternative labels for selected text
#' plot(net_10,
#'   node_size_by = 'degree_total',
#'   select_text = c('United States', 'China', 'Russian Federation'),
#'   select_text_display = c('USA', 'CHN', 'RUS'),
#'   text_size = 3,
#'   text_color = 'darkred'
#' )
#' 
#' # use return_components=TRUE
#' # to get back ggplot2 pieces of plot
#' g10 <- plot(
#'   net_10, 
#'   node_alpha=.8,
#'   arrow = ggplot2::arrow(length = ggplot2::unit(0.01, "inches")),  
#'   node_size_by = 'log(degree_total)',
#'   node_size_label = 'Log(Degree)',
#'   edge_alpha_label='Log(Verbal Coop.)',
#'   remove_isolates = TRUE,
#'   weight_transform = log1p,
#'   return_components = TRUE )
#' 
#' # Manually assemble with custom modifications
#' # to scale aesthetics such as edges
#' g10$base +
#'   netify_edge(g10) +
#'   ggplot2::scale_alpha_continuous(range = c(0.01, 0.2)) +
#'   netify_node(g10) +
#'   theme_netify()
#'
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

	# anything passed in goes to the plot arg dumpster
	plot_args <- list(...)

	# style over substance
	if (!is.null(plot_args$style)) {

		#
		style_fun <- plot_args$style
		
		# if a string get the fn
		if (is.character(style_fun)) {
			style_fun <- match.fun(style_fun) }
		
		# get style params
		style_params <- style_fun()
		
		# temove style from plot_args
		plot_args$style <- NULL
		
		# merge style params with plot_args
		plot_args <- c(style_params, plot_args) }

	# get plot data and parameters for ggplot
	net_plot_info = net_plot_data(x, plot_args)

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

    # apply color palettes if specified
    if (!is.null(plot_args$node_color_palette) && !is.null(components$point_scales$color)) {
        var_data <- net_dfs$nodal_data[[components$point_scales$color]]
        if (is.numeric(var_data) && length(unique(var_data)) > 10) {
            viz <- viz + scale_color_distiller(
                palette = plot_args$node_color_palette,
                direction = plot_args$node_color_direction %||% 1
            )
        } else {
            viz <- viz + scale_color_brewer(
                palette = plot_args$node_color_palette,
                type = "qual"
            )
        }
    }
    
    # ditto for fill
    if (!is.null(plot_args$node_fill_palette) && !is.null(components$point_scales$fill)) {
        var_data <- net_dfs$nodal_data[[components$point_scales$fill]]
        if (is.numeric(var_data) && length(unique(var_data)) > 10) {
            viz <- viz + scale_fill_distiller(
                palette = plot_args$node_fill_palette,
                direction = plot_args$node_fill_direction %||% 1
            )
        } else {
            viz <- viz + scale_fill_brewer(
                palette = plot_args$node_fill_palette,
                type = "qual"
            )
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