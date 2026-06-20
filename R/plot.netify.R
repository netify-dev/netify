#' Plotting method for netify objects
#'
#' creates customizable network visualizations from netify objects using ggplot2.
#' supports cross-sectional and longitudinal networks with extensive options for
#' mapping network attributes to visual properties.
#'
#' @param x a 'netify' object containing network data to visualize.
#' @param auto_format logical. if TRUE (default), automatically adjusts plot
#'   parameters based on network characteristics such as size, density, and
#'   structure. this includes "intelligent" defaults for:
#'   \itemize{
#'     \item node size (smaller for larger networks)
#'     \item edge transparency (lower for denser networks)
#'     \item text labels (enabled for small networks)
#'     \item curved edges (for small dense networks)
#'     \item isolate removal (for large networks)
#'   }
#'   set to FALSE to disable all automatic formatting. individual parameters
#'   can still be overridden even when auto_format is TRUE.
#' @param ... additional arguments controlling plot appearance:
#'
#' @section layout parameters:
#' \describe{
#'   \item{\code{layout}}{character string specifying the igraph layout algorithm.
#'     options include: \code{"nicely"} (default), \code{"fr"} (fruchterman-reingold),
#'     \code{"kk"} (kamada-kawai), \code{"circle"}, \code{"star"}, \code{"grid"},
#'     \code{"tree"}, \code{"bipartite"} (for bipartite networks), \code{"randomly"},
#'     and others. for ego networks, additional options are available: \code{"radial"}
#'     (ego-centric with optional grouping) and \code{"concentric"} (ego at center with
#'     alters in rings). see \code{\link{get_node_layout}} and \code{\link{get_ego_layout}}
#'     for full details.}
#'   \item{\code{point_layout}}{optional data.frame or list of data.frames containing
#'     pre-computed node positions with columns 'actor', 'x', and 'y'. overrides
#'     \code{layout} if provided.}
#'   \item{\code{static_actor_positions}}{logical. for longitudinal networks, should
#'     node positions remain constant across time? default is \code{FALSE}.}
#'   \item{\code{which_static}}{integer. when \code{static_actor_positions = TRUE},
#'     which time period's layout to use as template? if \code{NULL} (default),
#'     creates composite layout from all time periods.}
#'   \item{\code{seed}}{integer for reproducible layouts. default is 6886.}
#' }
#'
#' @section display control:
#' \describe{
#'   \item{\code{add_edges}}{logical. display edges? default is \code{TRUE}.}
#'   \item{\code{add_points}}{logical. display nodes as points? default is \code{TRUE}.}
#'   \item{\code{add_text}}{logical. add text labels to nodes? default is \code{FALSE}.}
#'   \item{\code{add_text_repel}}{logical. add text labels with automatic repositioning to avoid overlaps?
#'     default is \code{FALSE}. when \code{TRUE}, overrides \code{add_text}. uses ggrepel for positioning.}
#'   \item{\code{add_label}}{logical. add boxed labels to nodes? default is \code{FALSE}.}
#'   \item{\code{add_label_repel}}{logical. add boxed labels with automatic repositioning to avoid overlaps?
#'     default is \code{FALSE}. when \code{TRUE}, overrides \code{add_label}. uses ggrepel for positioning.}
#'   \item{\code{remove_isolates}}{logical. remove unconnected nodes? default is \code{TRUE}.}
#'   \item{\code{curve_edges}}{logical. use curved edges? default is \code{FALSE}.}
#'   \item{\code{use_theme_netify}}{logical. apply netify theme? default is \code{TRUE}.}
#'   \item{\code{facet_type}}{character. for multilayer longitudinal networks, controls
#'     faceting style: \code{"grid"} (default) creates a 2d grid with time x layer,
#'     \code{"wrap"} creates wrapped facets with combined time-layer labels.}
#'   \item{\code{facet_ncol}}{integer. number of columns for facet_wrap layouts. only
#'     used when \code{facet_type = "wrap"} or for single-dimension faceting.}
#'   \item{\code{rescale_edge_weights}}{logical. for multilayer networks, should edge weights
#'     be rescaled to a common 0-1 range across all layers? this is useful when layers have
#'     very different weight scales. default is \code{FALSE}.}
#' }
#'
#' @section subsetting parameters:
#' \describe{
#'   \item{\code{node_filter}}{a formula, quoted expression, or function to filter nodes.
#'     the expression can reference any nodal attribute. for example:
#'     \code{node_filter = ~ degree_total > 5} shows only nodes with total degree
#'     greater than 5.}
#'   \item{\code{edge_filter}}{a formula, quoted expression, or function to filter edges.
#'     the expression can reference any edge attribute, including 'weight' for weighted
#'     networks. for example: \code{edge_filter = ~ weight > 0.5} shows only edges
#'     with weight greater than 0.5.}
#'   \item{\code{time_filter}}{for longitudinal networks, a vector of time periods to include in the plot.
#'     can be numeric indices or character labels matching the time dimension. if NULL
#'     (default), all time periods are plotted. for cross-sectional networks, this
#'     parameter is ignored.}
#' }
#'
#' @section node aesthetics:
#'
#' fixed aesthetics (same for all nodes):
#' \describe{
#'   \item{\code{node_size} or \code{point_size}}{numeric. size of all nodes.}
#'   \item{\code{node_color} or \code{point_color}}{color of node borders.}
#'   \item{\code{node_fill} or \code{point_fill}}{fill color of nodes (note that fill will only work with certain shapes).}
#'   \item{\code{node_shape} or \code{point_shape}}{shape of nodes (see \code{?pch}).}
#'   \item{\code{node_alpha} or \code{point_alpha}}{transparency (0-1).}
#'   \item{\code{node_stroke} or \code{point_stroke}}{width of node borders.}
#' }
#'
#' variable aesthetics (mapped to data):
#' \describe{
#'   \item{\code{node_size_by} or \code{point_size_var}}{column name for size mapping.}
#'   \item{\code{node_color_by} or \code{point_color_var}}{column name for border color.}
#'   \item{\code{node_fill_by} or \code{point_fill_var}}{column name for fill color (note that fill will only work with certain shapes).}
#'   \item{\code{node_shape_by} or \code{point_shape_var}}{column name for shape.}
#'   \item{\code{node_alpha_by} or \code{point_alpha_var}}{column name for transparency.}
#' }
#'
#' @section edge aesthetics:
#'
#' fixed aesthetics:
#' \describe{
#'   \item{\code{edge_color}}{color for all edges. default is "black".}
#'   \item{\code{edge_linewidth}}{width for all edges. default is 0.5.}
#'   \item{\code{edge_linetype}}{line type (1=solid, 2=dashed, etc.).}
#'   \item{\code{edge_alpha}}{transparency (0-1).}
#'   \item{\code{edge_curvature}}{curvature amount when \code{curve_edges = TRUE}.}
#'   \item{\code{edge_arrow}}{arrow specification for directed networks. example:
#'     \code{arrow(length = unit(0.2, "cm"))}.}
#' }
#'
#' variable aesthetics:
#' \describe{
#'   \item{\code{edge_color_by} or \code{edge_color_var}}{column name for color mapping.}
#'   \item{\code{edge_linewidth_by} or \code{edge_linewidth_var}}{column name for width.}
#'   \item{\code{edge_linetype_by} or \code{edge_linetype_var}}{column name for line type.}
#'   \item{\code{edge_alpha_by} or \code{edge_alpha_var}}{column name for transparency.
#'     for weighted networks, defaults to the weight variable if not specified.}
#' }
#'
#' @section text and label options:
#'
#' selective labeling:
#' \describe{
#'   \item{\code{select_text}}{character vector of node names to show as text. when used,
#'     text labels will automatically use \code{geom_text_repel} to avoid overlaps.}
#'   \item{\code{select_text_display}}{alternative text to display (same length as
#'     \code{select_text}).}
#'   \item{\code{select_label}}{character vector of node names to show with boxes. when used,
#'     labels will automatically use \code{geom_label_repel} to avoid overlaps.}
#'   \item{\code{select_label_display}}{alternative labels (same length as
#'     \code{select_label}).}
#' }
#'
#' text aesthetics:
#' \describe{
#'   \item{\code{text_size}}{fixed size for all text. default is 3.88.}
#'   \item{\code{text_color}}{fixed color for all text. default is "black".}
#'   \item{\code{text_alpha}}{fixed transparency for text.}
#'   \item{\code{text_size_by}}{variable to map to text size.}
#'   \item{\code{text_color_by}}{variable to map to text color.}
#' }
#'
#' label (boxed text) aesthetics have similar parameters with \code{label_} prefix.
#'
#' text repel parameters (when \code{add_text_repel = TRUE}):
#' \describe{
#'   \item{\code{text_repel_force}}{force of repulsion between overlapping text. default is 1.}
#'   \item{\code{text_repel_max_overlaps}}{maximum number of overlaps to tolerate. default is 10.}
#'   \item{\code{text_repel_box_padding}}{padding around text. default is 0.25.}
#'   \item{\code{text_repel_point_padding}}{padding around points. default is 0.}
#'   \item{\code{text_repel_segment_color}}{color of connecting segments. default is "grey50".}
#' }
#'
#' label repel parameters (when \code{add_label_repel = TRUE}):
#' \describe{
#'   \item{similar to text_repel but with \code{label_repel_} prefix}{}
#'   \item{\code{label_repel_label_padding}}{padding around label boxes. default is 0.25.}
#'   \item{\code{label_repel_label_r}}{radius of label box corners. default is 0.15.}
#' }
#'
#' @section scale labels:
#'
#' customize legend titles:
#' \describe{
#'   \item{\code{node_size_label} or \code{point_size_label}}{legend title for size.}
#'   \item{\code{node_color_label} or \code{point_color_label}}{legend title for color.}
#'   \item{\code{edge_alpha_label}}{legend title for edge transparency.}
#'   \item{\code{edge_color_label}}{legend title for edge color.}
#' }
#'
#' @section highlighting parameters:
#' \describe{
#'   \item{\code{highlight}}{character vector of node names to highlight with different colors.
#'     non-highlighted nodes will be colored grey. highlighted nodes can also be automatically
#'     enlarged if \code{highlight_size_increase} is greater than 1.}
#'   \item{\code{highlight_color}}{named vector of colors for highlighted nodes. if NULL,
#'     uses default distinct colors (red, blue, green for up to 3 nodes, or a color palette
#'     for more). names should match the values in the \code{highlight} parameter.
#'     example: \code{c('usa' = 'blue', 'china' = 'red', 'russia' = 'green')}.}
#'   \item{\code{highlight_label}}{title for the highlight legend. default is "highlighted".}
#'   \item{\code{highlight_size_increase}}{numeric factor(s) to increase size of highlighted nodes.
#'     can be a single value (applied to all highlighted nodes) or a vector of length
#'     \code{length(highlight) + 1} where each value corresponds to a highlighted node
#'     and the last value applies to "other" nodes. default is 1 (no size increase).
#'     example: \code{c(3, 1, 1, 0.5)} for 3 highlighted
#'     nodes where the first is 3x larger, the next two are normal size, and all
#'     others are half size.}
#'   \item{\code{show_other_in_legend}}{logical. include "other" category in legend? default is FALSE.
#'     when FALSE, only highlighted nodes appear in the legend.}
#' }
#'
#' @section ego layout parameters:
#'
#' for ego networks (created with \code{\link{ego_netify}}), additional layout options
#' control the ego-centric visualization:
#' \describe{
#'   \item{\code{ego_group_by}}{character string specifying a nodal attribute to use for
#'     grouping alters in ego layouts. for "radial" layout, creates sectors. for
#'     "concentric" layout, determines ring assignment.}
#'   \item{\code{ego_order_by}}{character string specifying a nodal attribute to use for
#'     ordering alters within groups or rings. common options include "degree_total".}
#'   \item{\code{ego_weight_to_distance}}{logical. for weighted networks with "radial"
#'     layout, should edge weights determine distance from ego? higher weights place
#'     alters closer to ego. default is \code{FALSE}.}
#'   \item{\code{ego_ring_gap}}{numeric (0-1). gap between concentric rings as proportion
#'     of radius. only for "concentric" layout. default is 0.3.}
#'   \item{\code{ego_size}}{numeric. relative size of central area reserved for ego.
#'     larger values create more space between ego and alters. default is 0.1.}
#' }
#'
#' @section special parameters:
#' \describe{
#'   \item{\code{mutate_weight}}{function to transform edge weights before plotting.
#'     example: \code{log1p} for log(x+1) transformation. applied before mapping to
#'     aesthetics.}
#'   \item{\code{return_components}}{logical. return plot components instead of
#'     assembled plot? useful for manual customization. default is \code{FALSE}.}
#'   \item{\code{style}}{either a style function (e.g., \code{style_rose})
#'     that applies a complete visual style including colors, shapes, and layout
#'     preferences, or the string \code{"heatmap"} to render the adjacency
#'     matrix as a tile plot instead of a node-link diagram. when
#'     \code{style = "heatmap"} and edge weights cross zero (signed network),
#'     the fill scale automatically uses a diverging palette centred at zero;
#'     otherwise a sequential viridis ramp is used. optional \code{low},
#'     \code{mid}, and \code{high} arguments override the diverging palette
#'     endpoints.}
#' }
#'
#' @return
#' a ggplot2 object that can be further customized with additional layers, scales,
#' themes, etc. for longitudinal networks, includes facets for each time period.
#'
#' if \code{return_components = TRUE}, returns a list of plot components that can
#' be manually assembled or modified.
#'
#' @details
#'
#' \strong{naming conventions:}
#'
#' the function supports two naming styles for parameters:
#' \itemize{
#'   \item \strong{recommended}: use \code{node_*} for node attributes and
#'     \code{*_by} for variable mappings (e.g., \code{node_size_by = "degree"})
#'   \item \strong{legacy}: use \code{point_*} for nodes and \code{*_var} for
#'     variables (e.g., \code{point_size_var = "degree"})
#' }
#'
#' \strong{default behaviors:}
#' \itemize{
#'   \item for weighted networks, edge transparency maps to weight by default
#'   \item for directed networks, arrows are added automatically
#'   \item for longitudinal networks, time periods are shown as facets
#'   \item isolates are removed by default (set \code{remove_isolates = FALSE} to keep)
#' }
#'
#' \strong{customization tips:}
#' \itemize{
#'   \item use \code{mutate_weight} to handle skewed weight distributions
#'   \item combine fixed and variable aesthetics (e.g., fixed color with variable size)
#'   \item add ggplot2 layers after the plot call for further customization
#'   \item use \code{select_text} for selective labeling in dense networks
#' }
#'
#' @examples
#' data(classroom_edges)
#' data(classroom_nodes)
#'
#' net <- netify(
#'     classroom_edges,
#'     actor1 = "from", actor2 = "to",
#'     symmetric = TRUE,
#'     nodal_data = classroom_nodes,
#'     missing_to_zero = TRUE
#' )
#'
#' plot(net)
#'
#' plot(net,
#'     node_size_by = "gpa",
#'     node_color_by = "gender",
#'     node_size_label = "gpa",
#'     node_color_label = "gender"
#' )
#'
#' pieces <- plot(net,
#'     node_alpha = 0.8,
#'     node_size_by = "gpa",
#'     return_components = TRUE
#' )
#'
#' pieces$base +
#'     netify_edge(pieces) +
#'     netify_node(pieces) +
#'     theme_netify()
#'
#' @import ggplot2
#' @importFrom ggnewscale new_scale_color new_scale_fill new_scale
#' @importFrom ggrepel geom_text_repel geom_label_repel
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export plot.netify
#' @export

plot.netify <- function(x, auto_format = TRUE, ...) {
	# check if the input is a netify object
	netify_check(x)

	# extract attributes from the netify object
	obj_attrs <- attributes(x)

	# size guard for very large networks
	raw_x <- get_raw(x)
	if (obj_attrs$netify_type %in% c("cross_sec", "longit_array")) {
		n_actors <- dim(raw_x)[1]
	} else {
		n_actors <- max(vapply(raw_x, nrow, integer(1)))
	}
	if (n_actors > 1000) {
		cli::cli_warn(c(
			"Plotting a network with {n_actors} actors may be very slow or unresponsive.",
			"i" = "Consider subsetting to fewer actors with {.fn subset_netify} before plotting."
		))
	}

	# anything passed in goes to the plot arg dumpster
	plot_args <- list(...)

	# handle time_filter subsetting if provided
	if (!is.null(plot_args$time_filter)) {
		# extract time_filter parameter before passing to net_plot_data
		time_subset <- plot_args$time_filter

		# check if it's a longitudinal network
		if (obj_attrs$netify_type != "cross_sec") {
			# subset the netify object for the specified time periods
			x <- subset(x, time = time_subset)

			# update attributes after subsetting
			obj_attrs <- attributes(x)

			# remove time_filter from plot_args since we've already handled it
			plot_args$time_filter <- NULL
		} else {
			# warn if time_filter is specified for cross-sectional network
			cli::cli_alert_warning("'time_filter' parameter ignored for cross-sectional networks")
			plot_args$time_filter <- NULL
		}
	}

	# heatmap short-circuit: render the adjacency matrix as a tile plot.
	# signed weights (range crossing zero) auto-fire a diverging palette
	# centred at zero so positive/negative ties read at a glance.
	if (identical(plot_args$style, "heatmap")) {
		plot_args$style <- NULL
		return(plot_netify_heatmap(x, obj_attrs, plot_args))
	}

	# validate parameters and warn about common mistakes
	validate_plot_params(plot_args, ...)

	# style over substance
	if (!is.null(plot_args$style)) {
		#
		style_fun <- plot_args$style

		# if a string get the fn
		if (is.character(style_fun)) {
			style_fun <- match.fun(style_fun)
		}

		# get style params
		style_params <- style_fun()

		# remove style from plot_args
		plot_args$style <- NULL

		# merge style params with plot_args; explicit user args override style defaults
		plot_args <- utils::modifyList(style_params, plot_args)
	} # end styling

	# store filter arguments before removing them
	node_filter <- plot_args$node_filter
	edge_filter <- plot_args$edge_filter

	# remove filter args before passing to net_plot_data
	plot_args$node_filter <- NULL
	plot_args$edge_filter <- NULL

	# pass auto_format setting to net_plot_data
	plot_args$auto_format <- auto_format

	# get plot data and parameters for ggplot
	net_plot_info <- net_plot_data(x, plot_args)

	# extract plot arguments and ggplot parameters
	plot_args <- net_plot_info$plot_args
	ggnet_params <- net_plot_info$ggnet_params
	net_dfs <- net_plot_info$net_dfs

	# handle node filtering if provided
	if (!is.null(node_filter)) {
		# apply the filter to nodal data
		if (nrow(net_dfs$nodal_data) > 0) {
			# apply the filter using helper function
			filter_result <- apply_node_filter(net_dfs$nodal_data, node_filter)

			# apply the filter
			net_dfs$nodal_data <- net_dfs$nodal_data[filter_result, , drop = FALSE]

			# update edge data to remove edges connected to filtered nodes
			if (nrow(net_dfs$edge_data) > 0) {
				remaining_nodes <- net_dfs$nodal_data$name

				# keep only edges where both endpoints are in remaining nodes
				edges_to_keep <- net_dfs$edge_data$from %in% remaining_nodes &
					net_dfs$edge_data$to %in% remaining_nodes

				net_dfs$edge_data <- net_dfs$edge_data[edges_to_keep, , drop = FALSE]
			}
		}
	}

	# handle edge filtering if provided
	if (!is.null(edge_filter)) {
		# apply the filter to edge data
		if (nrow(net_dfs$edge_data) > 0) {
			# get weight column name if available
			weight_col <- attr(x, "weight", exact = TRUE)

			# apply the filter using the helper function
			filter_result <- apply_edge_filter(net_dfs$edge_data, edge_filter, weight_col)

			# apply the filter
			net_dfs$edge_data <- net_dfs$edge_data[filter_result, , drop = FALSE]

			# remove orphaned node rows
			if (plot_args$remove_isolates && nrow(net_dfs$edge_data) > 0) {
				# get nodes that still have edges after filtering
				remaining_nodes <- unique(c(net_dfs$edge_data$from, net_dfs$edge_data$to))

				# add time dimension if longitudinal
				if (obj_attrs$netify_type != "cross_sec") {
					remaining_node_time <- unique(net_dfs$edge_data[, c("from", "to", "time")])
					remaining_combos <- unique(rbind(
						data.frame(name = remaining_node_time$from, time = remaining_node_time$time),
						data.frame(name = remaining_node_time$to, time = remaining_node_time$time)
					))

					# create matching id
					remaining_combos$id <- paste(remaining_combos$name, remaining_combos$time, sep = "_")
					net_dfs$nodal_data$id <- paste(net_dfs$nodal_data$name, net_dfs$nodal_data$time, sep = "_")

					# keep only nodes that still have connections
					net_dfs$nodal_data <- net_dfs$nodal_data[net_dfs$nodal_data$id %in% remaining_combos$id, ]

					# remove temporary id column
					net_dfs$nodal_data$id <- NULL
				} else {
					# cross-sectional case
					net_dfs$nodal_data <- net_dfs$nodal_data[net_dfs$nodal_data$name %in% remaining_nodes, ]
				}
			}
		}
	}

	# handle weight transformation
	if (!is.null(plot_args$mutate_weight)) {
		# apply transformation to edge data
		weight_col <- attr(x, "weight", exact = TRUE)
		if (!is.null(weight_col) && weight_col %in% names(net_dfs$edge_data)) {
			transform_fn <- match.fun(plot_args$mutate_weight)
			net_dfs$edge_data[[weight_col]] <- transform_fn(net_dfs$edge_data[[weight_col]])
		}
	}

	# process labels
	scale_labels <- list(
		# edge labels
		edge_alpha = plot_args$edge_alpha_label,
		edge_color = plot_args$edge_color_label,
		edge_linewidth = plot_args$edge_linewidth_label,
		edge_linetype = plot_args$edge_linetype_label,

		# node labels
		node_size = plot_args$node_size_label,
		node_color = plot_args$node_color_label,
		node_fill = plot_args$node_fill_label,
		node_shape = plot_args$node_shape_label,
		node_alpha = plot_args$node_alpha_label,
		node_stroke = plot_args$node_stroke_label,

		# text labels
		text_size = plot_args$text_size_label,
		text_color = plot_args$text_color_label,
		text_alpha = plot_args$text_alpha_label,

		# label (box) labels
		label_size = plot_args$label_size_label,
		label_color = plot_args$label_color_label,
		label_fill = plot_args$label_fill_label,
		label_alpha = plot_args$label_alpha_label
	)

	# initialize a list to store plot components
	components <- list()

	# create the base ggplot object
	components$base <- ggplot()

	# pour labels into components
	components$scale_labels <- scale_labels

	# pre-process data for faceting if needed
	# this must happen before creating geoms that reference net_dfs
	is_longitudinal <- obj_attrs$netify_type != "cross_sec"
	is_multilayer <- isTRUE(plot_args$is_multilayer)

	if (is_multilayer && is_longitudinal) {
		facet_type <- plot_args$facet_type %||% "grid"
		if (facet_type == "wrap") {
			# create combined time_layer column before geoms are created
			if ("layer" %in% names(net_dfs$nodal_data) && "time" %in% names(net_dfs$nodal_data)) {
				net_dfs$nodal_data$time_layer <- paste(net_dfs$nodal_data$time,
					net_dfs$nodal_data$layer,
					sep = " - "
				)
				if (!is.null(net_dfs$edge_data) && nrow(net_dfs$edge_data) > 0) {
					net_dfs$edge_data$time_layer <- paste(net_dfs$edge_data$time,
						net_dfs$edge_data$layer,
						sep = " - "
					)
				}
			}
		}
	}

	# add edges to the plot if specified
	if (plot_args$add_edges) {
		# static parameters for edges
		edge_static_params <- ggnet_params$edge$static

		# static parameters for curved edges
		curve_static_params <- ggnet_params$curve$static

		# dynamic aesthetic mappings for edges
		edge_aes_list <- ggnet_params$edge$var

		# add curved edges if specified
		if (plot_args$curve_edges) {
			components$edges <- list(
				geom = GeomCurve,
				data = net_dfs$edge_data,
				mapping = do.call(aes, edge_aes_list),
				params = curve_static_params,
				stat = "identity",
				position = "identity",
				inherit.aes = TRUE,
				show.legend = if (length(edge_aes_list) > 0) TRUE else NA
			)
		} else {
			# add straight edges otherwise
			components$edges <- list(
				geom = GeomSegment,
				data = net_dfs$edge_data,
				mapping = do.call(aes, edge_aes_list),
				params = edge_static_params,
				stat = "identity",
				position = "identity",
				inherit.aes = TRUE,
				show.legend = if (length(edge_aes_list) > 0) TRUE else NA
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
		point_static_params <- ggnet_params$point$static

		# dynamic aesthetic mappings for points
		point_aes_list <- ggnet_params$point$var

		# create the geom_point component
		components$points <- list(
			geom = GeomPoint,
			data = net_dfs$nodal_data,
			mapping = do.call(aes, point_aes_list),
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
		text_static_params <- ggnet_params$text$static

		# dynamic aesthetic mappings for text
		text_aes_list <- ggnet_params$text$var

		# create the geom_text component
		components$text <- list(
			geom = GeomText,
			data = net_dfs$nodal_data,
			mapping = do.call(aes, text_aes_list),
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

	# add text_repel annotations to the plot if specified
	if (plot_args$add_text_repel) {
		# static parameters for text_repel
		text_repel_static_params <- ggnet_params$text_repel$static

		# dynamic aesthetic mappings for text_repel
		text_repel_aes_list <- ggnet_params$text_repel$var

		# create the geom_text_repel component
		components$text_repel <- list(
			geom = ggrepel::GeomTextRepel,
			data = net_dfs$nodal_data,
			mapping = do.call(aes, text_repel_aes_list),
			params = text_repel_static_params,
			stat = "identity",
			position = "identity",
			inherit.aes = TRUE,
			show.legend = NA
		)

		# store text_repel scale names (same as text)
		components$text_repel_scales <- list()
		if (!is.null(plot_args$text_color_var)) {
			components$text_repel_scales$color <- plot_args$text_color_var
		}
		if (!is.null(plot_args$text_alpha_var)) {
			components$text_repel_scales$alpha <- plot_args$text_alpha_var
		}
		if (!is.null(plot_args$text_size_var)) {
			components$text_repel_scales$size <- plot_args$text_size_var
		}
	}

	# add labels to the plot if specified
	if (plot_args$add_label) {
		# static parameters for labels
		label_static_params <- ggnet_params$label$static

		# dynamic aesthetic mappings for labels
		label_aes_list <- ggnet_params$label$var

		# create the geom_label component
		components$label <- list(
			geom = GeomLabel,
			data = net_dfs$nodal_data,
			mapping = do.call(aes, label_aes_list),
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

	# add label_repel annotations to the plot if specified
	if (plot_args$add_label_repel) {
		# static parameters for label_repel
		label_repel_static_params <- ggnet_params$label_repel$static

		# dynamic aesthetic mappings for label_repel
		label_repel_aes_list <- ggnet_params$label_repel$var

		# create the geom_label_repel component
		components$label_repel <- list(
			geom = ggrepel::GeomLabelRepel,
			data = net_dfs$nodal_data,
			mapping = do.call(aes, label_repel_aes_list),
			params = label_repel_static_params,
			stat = "identity",
			position = "identity",
			inherit.aes = TRUE,
			show.legend = NA
		)

		# store label_repel scale names (same as label)
		components$label_repel_scales <- list()
		if (!is.null(plot_args$label_color_var)) {
			components$label_repel_scales$color <- plot_args$label_color_var
		}
		if (!is.null(plot_args$label_fill_var)) {
			components$label_repel_scales$fill <- plot_args$label_fill_var
		}
		if (!is.null(plot_args$label_alpha_var)) {
			components$label_repel_scales$alpha <- plot_args$label_alpha_var
		}
		if (!is.null(plot_args$label_size_var)) {
			components$label_repel_scales$size <- plot_args$label_size_var
		}
	}

	# add facets for longitudinal and/or multilayer data
	# (variables already defined earlier)

	# determine faceting based on data structure and user preferences
	if (is_multilayer && is_longitudinal) {
		# both multilayer and longitudinal
		facet_type <- plot_args$facet_type %||% "grid" # default to grid for 2d faceting

		if (facet_type == "grid") {
			components$facets <- facet_grid(time ~ layer)
		} else if (facet_type == "wrap") {
			# time_layer column was already created earlier
			components$facets <- facet_wrap(~time_layer,
				scales = "free",
				ncol = plot_args$facet_ncol %||% NULL
			)
		}
	} else if (is_multilayer) {
		# only multilayer
		components$facets <- facet_wrap(~layer,
			scales = "free",
			ncol = plot_args$facet_ncol %||% NULL
		)
	} else if (is_longitudinal) {
		# only longitudinal (original behavior)
		components$facets <- facet_wrap(~time, scales = "free")
	}

	# apply the netify theme if specified
	if (plot_args$use_theme_netify) {
		components$theme <- theme_netify()
	}

	# store additional information for debugging or further customization
	components$net_dfs <- net_dfs
	components$plot_args <- plot_args
	components$obj_attrs <- obj_attrs
	components$ggnet_params <- ggnet_params

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
			# add alpha legend for multilayer plots
			viz <- viz + scale_alpha_continuous()
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
		# only reset color and fill scales if there's a conflict
		if (!is.null(components$edge_scales$color) &&
			(!is.null(components$point_scales$color) || !is.null(components$point_scales$fill))) {
			viz <- viz +
				ggnewscale::new_scale_color() +
				ggnewscale::new_scale_fill()
		}
		# only reset alpha scale if both edges and points use alpha
		if (!is.null(components$edge_scales$alpha) &&
			!is.null(components$point_scales$alpha)) {
			viz <- viz +
				ggnewscale::new_scale("alpha")
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
		# hide size guide if requested (for highlighting)
		if (isTRUE(plot_args$point_size_guide == "none") && !is.null(components$point_scales$size)) {
			viz <- viz + guides(size = "none")
		}

		# apply node color / fill scales right after the points layer
		# so that ggnewscale-renamed aesthetics resolve to the active scale
		# (the original placement at the end of the function targeted the
		# default `colour` aesthetic, which no geom uses once ggnewscale
		# has reset the color scale -- producing a spurious "no shared
		# levels found" warning when `highlight=` is in use)
			if (!is.null(plot_args$node_color_palette) && !is.null(components$point_scales$color)) {
				var_data <- net_dfs$nodal_data[[components$point_scales$color]]
				if (is.numeric(var_data)) {
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
			if (!is.null(plot_args$node_fill_palette) && !is.null(components$point_scales$fill)) {
				var_data <- net_dfs$nodal_data[[components$point_scales$fill]]
				if (is.numeric(var_data)) {
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

		# apply highlight colors right after the points layer
		if (isTRUE(plot_args$is_highlighting)) {
			if (!is.null(components$point_scales$fill) && components$point_scales$fill == "highlight_status") {
				viz <- viz + scale_fill_manual(
					values = plot_args$highlight_color,
					name = plot_args$highlight_label %||% "Highlighted",
					breaks = names(plot_args$highlight_color)[names(plot_args$highlight_color) != "Other"],
					labels = names(plot_args$highlight_color)[names(plot_args$highlight_color) != "Other"]
				)
			} else if (!is.null(components$point_scales$color) && components$point_scales$color == "highlight_status") {
				viz <- viz + scale_color_manual(
					values = plot_args$highlight_color,
					name = plot_args$highlight_label %||% "Highlighted",
					breaks = names(plot_args$highlight_color)[names(plot_args$highlight_color) != "Other"],
					labels = names(plot_args$highlight_color)[names(plot_args$highlight_color) != "Other"]
				)
			}
		}
	}

	# add text annotations to the plot if they exist
	if (!is.null(components$text)) {
		# reset scales again if needed
		if (!is.null(components$points)) {
			viz <- viz +
				ggnewscale::new_scale_color() +
				ggnewscale::new_scale("alpha") +
				ggnewscale::new_scale("size")
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

	# add text_repel annotations to the plot if they exist
	if (!is.null(components$text_repel)) {
		# reset scales again if needed
		if (!is.null(components$points)) {
			viz <- viz +
				ggnewscale::new_scale_color() +
				ggnewscale::new_scale("alpha") +
				ggnewscale::new_scale("size")
		}

		viz <- viz + layer(
			geom = components$text_repel$geom,
			data = components$text_repel$data,
			mapping = components$text_repel$mapping,
			stat = components$text_repel$stat,
			position = components$text_repel$position,
			params = components$text_repel$params,
			inherit.aes = components$text_repel$inherit.aes,
			show.legend = components$text_repel$show.legend
		)

		# apply text scale labels if they are defined (same as regular text)
		if (!is.null(scale_labels$text_size) && !is.null(components$text_repel_scales$size)) {
			viz <- viz + labs(size = scale_labels$text_size)
		}
		if (!is.null(scale_labels$text_color) && !is.null(components$text_repel_scales$color)) {
			viz <- viz + labs(color = scale_labels$text_color)
		}
		if (!is.null(scale_labels$text_alpha) && !is.null(components$text_repel_scales$alpha)) {
			viz <- viz + labs(alpha = scale_labels$text_alpha)
		}
	}

	# add labels to the plot if they exist
	if (!is.null(components$label)) {
		# reset scales again if needed
		if (!is.null(components$text) || !is.null(components$points)) {
			viz <- viz +
				ggnewscale::new_scale_color() +
				ggnewscale::new_scale("alpha") +
				ggnewscale::new_scale_fill() +
				ggnewscale::new_scale("size")
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

	# add label_repel annotations to the plot if they exist
	if (!is.null(components$label_repel)) {
		# reset scales again if needed
		if (!is.null(components$text) || !is.null(components$text_repel) || !is.null(components$points)) {
			viz <- viz +
				ggnewscale::new_scale_color() +
				ggnewscale::new_scale("alpha") +
				ggnewscale::new_scale_fill() +
				ggnewscale::new_scale("size")
		}

		viz <- viz + layer(
			geom = components$label_repel$geom,
			data = components$label_repel$data,
			mapping = components$label_repel$mapping,
			stat = components$label_repel$stat,
			position = components$label_repel$position,
			params = components$label_repel$params,
			inherit.aes = components$label_repel$inherit.aes,
			show.legend = components$label_repel$show.legend
		)

		# apply label scale labels if they are defined (same as regular label)
		if (!is.null(scale_labels$label_size) && !is.null(components$label_repel_scales$size)) {
			viz <- viz + labs(size = scale_labels$label_size)
		}
		if (!is.null(scale_labels$label_color) && !is.null(components$label_repel_scales$color)) {
			viz <- viz + labs(color = scale_labels$label_color)
		}
		if (!is.null(scale_labels$label_fill) && !is.null(components$label_repel_scales$fill)) {
			viz <- viz + labs(fill = scale_labels$label_fill)
		}
		if (!is.null(scale_labels$label_alpha) && !is.null(components$label_repel_scales$alpha)) {
			viz <- viz + labs(alpha = scale_labels$label_alpha)
		}
	}

	# node color/fill scales and highlight scales are now applied inline
	# immediately after the points layer (see above), so that they target
	# the active (possibly ggnewscale-renamed) aesthetic.

	# add facets to the plot if they are defined (useful for longitudinal data)
	if (!is.null(components$facets)) {
		viz <- viz + components$facets
	}

	# add axis limits for facet_grid centering
	if (!is.null(plot_args$xlim) && !is.null(plot_args$ylim)) {
		viz <- viz +
			coord_cartesian(xlim = plot_args$xlim, ylim = plot_args$ylim) +
			theme(aspect.ratio = 1) # ensure square panels
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

#' apply edge filter expression to edge data
#'
#' internal function to safely apply edge filtering expressions
#'
#' @param edge_data data frame containing edge information
#' @param filter_input a formula, quoted expression, or function to evaluate
#' @param weight_col name of the weight column if applicable
#'
#' @return logical vector indicating which edges to keep
#'
#' @keywords internal
#' @noRd

apply_edge_filter <- function(edge_data, filter_input, weight_col = NULL) {
	# create a safe evaluation environment
	eval_env <- edge_data

	# add 'weight' as an alias if weight column exists
	if (!is.null(weight_col) && weight_col %in% names(edge_data)) {
		eval_env$weight <- edge_data[[weight_col]]
	}

	# handle different input types
	if (inherits(filter_input, "formula")) {
		# extract expression from formula
		filter_expr <- filter_input[[2]]
	} else if (is.function(filter_input)) {
		# apply function directly
		return(filter_input(eval_env))
	} else if (is.language(filter_input)) {
		# use quoted expression directly
		filter_expr <- filter_input
	} else {
		cli::cli_abort("Edge filter must be a formula (~ expr), quoted expression (quote(expr)), or function")
	}

	# try to evaluate the expression
	tryCatch(
		{
			# evaluate the filter expression
			result <- eval(filter_expr, envir = eval_env)

			# require a logical filter result
			if (!is.logical(result)) {
				cli::cli_alert_warning("Edge filter expression must return logical values. Attempting conversion.")
				result <- as.logical(result)
			}

			# handle missing filter values
			result[is.na(result)] <- FALSE

			# check length
			if (length(result) != nrow(edge_data)) {
				cli::cli_abort("Edge filter expression must return a value for each edge")
			}

			return(result)
		},
		error = function(e) {
			cli::cli_abort(c(
				"Error evaluating edge filter expression",
				"x" = e$message,
				"i" = "Available variables: {paste(names(eval_env), collapse = ', ')}"
			))
		}
	)
}

#' apply node filter expression to nodal data
#'
#' internal function to safely apply node filtering expressions
#'
#' @param nodal_data data frame containing node information
#' @param filter_input a formula, quoted expression, or function to evaluate
#'
#' @return logical vector indicating which nodes to keep
#'
#' @keywords internal
#' @noRd

apply_node_filter <- function(nodal_data, filter_input) {
	# create a safe evaluation environment
	eval_env <- nodal_data

	# add common aliases for convenience
	if ("name" %in% names(nodal_data)) {
		eval_env$actor <- nodal_data$name
	}

	# handle different input types
	if (inherits(filter_input, "formula")) {
		# extract expression from formula
		filter_expr <- filter_input[[2]]
	} else if (is.function(filter_input)) {
		# apply function directly
		return(filter_input(eval_env))
	} else if (is.language(filter_input)) {
		# use quoted expression directly
		filter_expr <- filter_input
	} else {
		cli::cli_abort("Node filter must be a formula (~ expr), quoted expression (quote(expr)), or function")
	}

	# try to evaluate the expression
	tryCatch(
		{
			# evaluate the filter expression
			result <- eval(filter_expr, envir = eval_env)

			# require a logical filter result
			if (!is.logical(result)) {
				cli::cli_alert_warning("Node filter expression must return logical values. Attempting conversion.")
				result <- as.logical(result)
			}

			# handle missing filter values
			result[is.na(result)] <- FALSE

			# check length
			if (length(result) != nrow(nodal_data)) {
				cli::cli_abort("Node filter expression must return a value for each node")
			}

			return(result)
		},
		error = function(e) {
			cli::cli_abort(c(
				"Error evaluating node filter expression",
				"x" = e$message,
				"i" = "Available variables: {paste(names(eval_env), collapse = ', ')}"
			))
		}
	)
}


#' adjacency-matrix heatmap renderer for netify objects
#'
#' builds a ggplot tile heatmap from a netify adjacency matrix. used by
#' `plot.netify` when the caller passes `style = "heatmap"`. for weights that
#' straddle zero (signed networks), the fill scale auto-flips to a diverging
#' palette centred at zero; otherwise a sequential viridis ramp is used.
#'
#' @param x a netify object.
#' @param obj_attrs attribute list pulled from `attributes(x)`.
#' @param plot_args named list of optional plot tweaks (currently honors
#'   `title`, `xlab`, `ylab`, `low`, `mid`, `high`).
#'
#' @return a ggplot object.
#' @keywords internal
#' @noRd
plot_netify_heatmap <- function(x, obj_attrs, plot_args = list()) {
	netify_type <- obj_attrs$netify_type

	make_entry <- function(mat, time = NA_character_, layer = NA_character_) {
		list(matrix = mat, time = time, layer = layer)
	}
	entries <- list()
	add_entry <- function(mat, time = NA_character_, layer = NA_character_) {
		entries[[length(entries) + 1L]] <<- make_entry(mat, time, layer)
	}

	raw_x <- get_raw(x)
	if (netify_type == "cross_sec") {
		if (is.array(raw_x) && length(dim(raw_x)) == 3L) {
			layer_names <- dimnames(raw_x)[[3]] %||% as.character(seq_len(dim(raw_x)[3]))
			for (ll in seq_along(layer_names)) {
				add_entry(raw_x[, , ll, drop = TRUE], layer = layer_names[ll])
			}
		} else {
			add_entry(raw_x)
		}
	}
	if (netify_type == "longit_array") {
		if (length(dim(raw_x)) == 4L) {
			layer_names <- dimnames(raw_x)[[3]] %||% as.character(seq_len(dim(raw_x)[3]))
			time_names <- dimnames(raw_x)[[4]] %||% as.character(seq_len(dim(raw_x)[4]))
			for (tt in seq_along(time_names)) {
				for (ll in seq_along(layer_names)) {
					add_entry(raw_x[, , ll, tt, drop = TRUE],
						time = time_names[tt], layer = layer_names[ll])
				}
			}
		} else {
			time_names <- dimnames(raw_x)[[3]] %||% as.character(seq_len(dim(raw_x)[3]))
			for (tt in seq_along(time_names)) {
				add_entry(raw_x[, , tt, drop = TRUE], time = time_names[tt])
			}
		}
	}
	if (netify_type == "longit_list") {
		time_names <- names(raw_x) %||% as.character(seq_along(raw_x))
		for (tt in seq_along(raw_x)) {
			mat_t <- raw_x[[tt]]
			if (is.array(mat_t) && length(dim(mat_t)) == 3L) {
				layer_names <- dimnames(mat_t)[[3]] %||% as.character(seq_len(dim(mat_t)[3]))
				for (ll in seq_along(layer_names)) {
					add_entry(mat_t[, , ll, drop = TRUE],
						time = time_names[tt], layer = layer_names[ll])
				}
			} else {
				add_entry(mat_t, time = time_names[tt])
			}
		}
	}

	# long-format edge frame across all matrices
	df_parts <- lapply(entries, function(entry) {
		m <- entry$matrix
		rn <- rownames(m) %||% as.character(seq_len(nrow(m)))
		cn <- colnames(m) %||% as.character(seq_len(ncol(m)))
		data.frame(
			from = rep(rn, times = ncol(m)),
			to = rep(cn, each = nrow(m)),
			weight = as.numeric(m),
			time = entry$time,
			layer = entry$layer,
			stringsAsFactors = FALSE
		)
	})
	df <- do.call(rbind, df_parts)

	# preserve row/col ordering exactly as the netlet stores them
	row_levels <- unique(unlist(lapply(entries, function(entry) {
		m <- entry$matrix
		rownames(m) %||% as.character(seq_len(nrow(m)))
	}), use.names = FALSE))
	col_levels <- unique(unlist(lapply(entries, function(entry) {
		m <- entry$matrix
		colnames(m) %||% as.character(seq_len(ncol(m)))
	}), use.names = FALSE))
	df$from <- factor(df$from, levels = rev(row_levels))
	df$to   <- factor(df$to, levels = col_levels)

	# pick palette: diverging if weights cross zero, sequential otherwise
	w_finite <- df$weight[is.finite(df$weight)]
	is_signed <- length(w_finite) > 0 &&
		min(w_finite, na.rm = TRUE) < 0 &&
		max(w_finite, na.rm = TRUE) > 0

	low_col  <- plot_args$low  %||% "#2166AC"
	mid_col  <- plot_args$mid  %||% "#F7F7F7"
	high_col <- plot_args$high %||% "#B2182B"

	p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$to, y = .data$from, fill = .data$weight)) +
		ggplot2::geom_tile(color = NA) +
		ggplot2::coord_equal() +
		ggplot2::labs(
			x = plot_args$xlab %||% "",
			y = plot_args$ylab %||% "",
			fill = obj_attrs$weight %||% "weight",
			title = plot_args$title
		) +
		ggplot2::theme_minimal(base_size = 11) +
		ggplot2::theme(
			axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
			panel.grid = ggplot2::element_blank()
		)

	if (is_signed) {
		lim <- max(abs(w_finite), na.rm = TRUE)
		p <- p + ggplot2::scale_fill_gradient2(
			low = low_col, mid = mid_col, high = high_col,
			midpoint = 0, limits = c(-lim, lim), na.value = "grey90"
		)
	} else {
		p <- p + ggplot2::scale_fill_viridis_c(option = "magma", na.value = "grey90")
	}

	# facet over time and/or layer when present
	has_time <- any(!is.na(df$time)) && length(unique(df$time[!is.na(df$time)])) > 1L
	has_layer <- any(!is.na(df$layer)) && length(unique(df$layer[!is.na(df$layer)])) > 1L
	if (has_time && has_layer) {
		p <- p + ggplot2::facet_grid(layer ~ time)
	} else if (has_time) {
		p <- p + ggplot2::facet_wrap(~ time)
	} else if (has_layer) {
		p <- p + ggplot2::facet_wrap(~ layer)
	}

	p
}
