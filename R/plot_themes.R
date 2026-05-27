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

theme_netify <- function() {
	theme_minimal() +
		theme(
			axis.text = element_blank(),
			axis.title = element_blank(),
			legend.position = "top",
			# stack multiple legends vertically
			legend.box = "vertical",
			panel.border = element_blank(),
			axis.ticks = element_blank(),
			strip.text.x = element_text(color = "white", hjust = 0),
			strip.text.y = element_text(color = "white", hjust = 0),
			strip.background = element_rect(fill = "black", color = "black")
		)
}

#' theme_stat_netify function
#'
#' This function returns a customized theme for netify stat plots.
#'
#' @return A customized theme object for netify stat plots.
#' @author Cassy Dorff, Shahryar Minhas
#' @import ggplot2
#' @export theme_stat_netify
#'

theme_stat_netify <- function() {
	theme_bw() +
		theme(
			panel.border = element_blank(),
			axis.ticks = element_blank(),
			axis.text.x = element_text(angle = 90, hjust = 1),
			strip.text.x = element_text(color = "white", hjust = 0),
			strip.text.y = element_text(color = "white", hjust = 0),
			strip.background = element_rect(fill = "black", color = "black"),
			legend.position = "bottom"
		)
}

#' ggplot theme for netify network plots
#'
#' A ggplot theme with a larger base font, italicized legend titles, and
#' (by default) stripped axes / panel chrome — the right default for a
#' force-directed network layout. Drop on top of any `plot.netify()` output.
#'
#' @param base_size Base font size, passed to `theme_minimal()`. Default 12.
#' @param for_network Logical. When `TRUE` (default) removes axis text /
#'   titles / ticks / panel border, which is the right behavior for a
#'   force-directed network layout. Set `FALSE` if you want to keep axes
#'   on (e.g. for a heatmap or actor-stat plot).
#' @return A ggplot2 theme object.
#' @author Cassy Dorff, Shahryar Minhas
#' @import ggplot2
#' @export theme_publication_netify
#'

theme_publication_netify <- function(base_size = 12, for_network = TRUE) {
	thm <- theme_minimal(base_size = base_size) +
		theme(
			plot.title = element_text(face = "bold", size = base_size + 2),
			plot.subtitle = element_text(color = "grey30", size = base_size),
			legend.title = element_text(face = "italic", size = base_size - 1),
			legend.text = element_text(size = base_size - 1),
			legend.position = "right",
			legend.key.size = unit(0.8, "lines"),
			strip.text.x = element_text(color = "white", hjust = 0, face = "bold"),
			strip.text.y = element_text(color = "white", hjust = 0, face = "bold"),
			strip.background = element_rect(fill = "black", color = "black"),
			panel.grid.minor = element_blank()
		)

	# strip axis chrome for network layouts
	if (isTRUE(for_network)) {
		thm <- thm + theme(
			axis.text = element_blank(),
			axis.title = element_blank(),
			axis.ticks = element_blank(),
			panel.border = element_blank(),
			panel.grid.major = element_blank()
		)
	}

	thm
}

#' ggplot theme for netify time-series / stat plots
#'
#' Companion to `theme_publication_netify()` for plots that keep their
#' axes — actor-stat time series, similarity heatmaps, mixing matrices,
#' etc. Same typographic settings (italic legend titles, bold strip text,
#' larger base font) but axes and gridlines are retained.
#'
#' @param base_size Base font size. Default 12.
#' @return A ggplot2 theme object.
#' @author Cassy Dorff, Shahryar Minhas
#' @import ggplot2
#' @export theme_publication_netify_ts
#'

theme_publication_netify_ts <- function(base_size = 12) {
	theme_minimal(base_size = base_size) +
		theme(
			plot.title = element_text(face = "bold", size = base_size + 2),
			plot.subtitle = element_text(color = "grey30", size = base_size),
			axis.title = element_text(face = "bold", size = base_size),
			axis.text = element_text(size = base_size - 1),
			legend.title = element_text(face = "italic", size = base_size - 1),
			legend.text = element_text(size = base_size - 1),
			legend.position = "bottom",
			legend.key.size = unit(0.8, "lines"),
			strip.text.x = element_text(color = "white", hjust = 0, face = "bold"),
			strip.text.y = element_text(color = "white", hjust = 0, face = "bold"),
			strip.background = element_rect(fill = "black", color = "black"),
			panel.grid.minor = element_blank()
		)
}

#' Get smart defaults based on network properties
#'
#' Automatically adjusts plotting defaults based on network characteristics
#' such as size, density, and structure.
#'
#' @param netlet A netify object
#' @param msrmnts Measurement data associated with the netlet
#' @param plot_args Existing plot arguments to merge with
#' @return Updated plot_args with smart defaults applied
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd

get_smart_defaults <- function(netlet, msrmnts = NULL, plot_args = list()) {
	# if measurements are not provided, try to extract from netlet
	if (is.null(msrmnts)) {
		msrmnts <- netify_measurements(netlet)
	}

	# calculate network properties
	if (attr(netlet, "netify_type") == "cross_sec") {
		n_nodes <- length(unique(c(msrmnts$row_actors, msrmnts$col_actors)))
		# get edge count from raw network
		net_raw <- get_raw(netlet)
		n_edges <- sum(net_raw != 0, na.rm = TRUE)
		if (all(attr(netlet, "symmetric"))) n_edges <- n_edges / 2
	} else {
		# for longitudinal, use average across time
		if (attr(netlet, "netify_type") == "longit_list") {
			all_actors <- unique(unlist(c(msrmnts$row_actors, msrmnts$col_actors)))
			n_nodes <- length(all_actors)
			# average edges across time
			n_edges <- mean(sapply(netlet, function(x) sum(get_raw(x) != 0, na.rm = TRUE)))
		} else {
			n_nodes <- length(unique(c(msrmnts$row_actors, msrmnts$col_actors)))
			net_raw <- get_raw(netlet)
			n_edges <- mean(apply(net_raw, 3, function(x) sum(x != 0, na.rm = TRUE)))
		}
	}

	# calculate density
	max_edges <- if (all(attr(netlet, "symmetric"))) {
		n_nodes * (n_nodes - 1) / 2
	} else {
		n_nodes * (n_nodes - 1)
	}
	density <- n_edges / max(max_edges, 1)

	# smart defaults based on network properties
	smart_defaults <- list()

	# check if a style has been applied
	has_style <- !is.null(plot_args$point_fill) ||
		!is.null(plot_args$edge_color) ||
		!is.null(plot_args$curve_edges)

	# point size based on number of nodes
	if (is.null(plot_args$point_size) && is.null(plot_args$node_size)) {
		if (n_nodes > 100) {
			smart_defaults$point_size <- 1.0
		} else if (n_nodes > 50) {
			smart_defaults$point_size <- 1.5
		} else if (n_nodes > 20) {
			smart_defaults$point_size <- 2.0
		} else {
			smart_defaults$point_size <- 2.5
		}
	}

	# edge transparency based on density
	if (is.null(plot_args$edge_alpha) && is.null(plot_args$edge_alpha_var) && !has_style) {
		if (density > 0.5) {
			smart_defaults$edge_alpha <- 0.1
		} else if (density > 0.3) {
			smart_defaults$edge_alpha <- 0.2
		} else if (density > 0.15) {
			smart_defaults$edge_alpha <- 0.3
		} else {
			smart_defaults$edge_alpha <- 0.5
		}
	}

	# text labels for small networks
	if (is.null(plot_args$add_text) && is.null(plot_args$add_text_repel) && is.null(plot_args$select_text)) {
		smart_defaults$add_text <- n_nodes <= 15
	}

	# curved edges for small dense networks
	if (is.null(plot_args$curve_edges)) {
		smart_defaults$curve_edges <- n_nodes < 30 && density > 0.3
	}

	# remove isolates by default for large networks
	if (is.null(plot_args$remove_isolates)) {
		smart_defaults$remove_isolates <- n_nodes > 20
	}

	# edge color default
	if (is.null(plot_args$edge_color)) {
		smart_defaults$edge_color <- "#666666" # medium gray
	}

	# merge smart defaults with existing plot_args
	for (name in names(smart_defaults)) {
		if (is.null(plot_args[[name]])) {
			plot_args[[name]] <- smart_defaults[[name]]
		}
	}

	return(plot_args)
}

#' Apply color palettes based on variable type and mapping
#'
#' Automatically selects appropriate ColorBrewer palettes based on
#' whether variables are continuous or categorical
#'
#' @param plot_args Plot arguments
#' @param net_dfs Network data frames from decompose_netify
#' @return Updated plot_args with color palette specifications
#'
#' @keywords internal
#' @noRd

apply_smart_palettes <- function(plot_args, net_dfs) {
	# node color mapping
	if (!is.null(plot_args$point_color_var) || !is.null(plot_args$node_color_var)) {
		var_name <- plot_args$point_color_var %||% plot_args$node_color_var
		if (var_name %in% names(net_dfs$nodal_data)) {
			var_data <- net_dfs$nodal_data[[var_name]]

			# determine if categorical or continuous
			if (is.numeric(var_data) && length(unique(var_data)) > 10) {
				# continuous - use sequential palette
				if (is.null(plot_args$node_color_palette)) {
					plot_args$node_color_palette <- "Blues"
					plot_args$node_color_direction <- 1
				}
			} else {
				# categorical - use qualitative palette
				n_cats <- length(unique(var_data))
				if (is.null(plot_args$node_color_palette)) {
					if (n_cats <= 8) {
						plot_args$node_color_palette <- "Set2"
					} else if (n_cats <= 12) {
						plot_args$node_color_palette <- "Set3"
					} else {
						plot_args$node_color_palette <- "Paired"
					}
				}
			}
		}
	}

	# node fill mapping
	if (!is.null(plot_args$point_fill_var) || !is.null(plot_args$node_fill_var)) {
		var_name <- plot_args$point_fill_var %||% plot_args$node_fill_var
		if (var_name %in% names(net_dfs$nodal_data)) {
			var_data <- net_dfs$nodal_data[[var_name]]

			if (is.numeric(var_data) && length(unique(var_data)) > 10) {
				# continuous
				if (is.null(plot_args$node_fill_palette)) {
					plot_args$node_fill_palette <- "Oranges"
					plot_args$node_fill_direction <- 1
				}
			} else {
				# categorical
				n_cats <- length(unique(var_data))
				if (is.null(plot_args$node_fill_palette)) {
					if (n_cats <= 8) {
						plot_args$node_fill_palette <- "Set1"
					} else {
						plot_args$node_fill_palette <- "Set3"
					}
				}
			}
		}
	}

	# edge color mapping
	if (!is.null(plot_args$edge_color_var)) {
		var_name <- plot_args$edge_color_var
		if (var_name %in% names(net_dfs$edge_data)) {
			var_data <- net_dfs$edge_data[[var_name]]

			if (is.numeric(var_data) && length(unique(var_data)) > 10) {
				# continuous - use diverging palette for edges
				if (is.null(plot_args$edge_color_palette)) {
					plot_args$edge_color_palette <- "RdBu"
					plot_args$edge_color_direction <- -1 # reverse for better visibility
				}
			} else {
				# categorical edges
				if (is.null(plot_args$edge_color_palette)) {
					plot_args$edge_color_palette <- "Dark2"
				}
			}
		}
	}

	return(plot_args)
}
