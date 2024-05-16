#' Plotting method for node label in 'netify' visualizations
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
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @return A 'ggplot' plot object that can be further modified or printed.
#' 
#' @import ggplot2
#' @importFrom ggnewscale new_scale_color new_scale_fill new_scale 
#' 
#' @export net_label
#' @export

net_label <- function(x, ...) {
}

	# # geom_label
	# if(plot_args$add_label){

	# 	# label static param list
	# 	label_static_params = ggnet_params$label$static

	# 	# Prepare a list to conditionally build the aes()
	# 	label_aes_list <- ggnet_params$label$var

	# 	# make room for new scales
	# 	viz <- viz + 
	# 		ggnewscale::new_scale_color() + 
	# 		ggnewscale::new_scale('alpha') +
	# 		ggnewscale::new_scale_fill() +
	# 		ggnewscale::new_scale('size')

	# 	# create geom_label
	# 	viz <- viz + layer(
	# 		data = net_dfs$nodal_data,
	# 		mapping = aes(!!!label_aes_list),
	# 		geom = GeomLabel,
	# 		stat = "identity",
	# 		position = "identity",
	# 		params = label_static_params,
	# 		inherit.aes = TRUE,
	# 		show.legend = NA
	# 	)
	# }