#' Adjust plotting parameters and data for netify visualization
#'
#' `adjust_plot_args` prepares plotting parameters and modifies network data
#' from a netify object for visualization. This function sets default values
#' for various plotting parameters and adjusts node and edge data frames based
#' on the network's properties and user specifications.
#'
#' @param plot_args A list of user-defined plotting arguments. Can include parameters
#'   for controlling visual elements such as text overlap checking, edge curvature,
#'   node appearance, and selective labeling. See Details for the full list of
#'   available parameters and their defaults.
#' @param net_dfs A list containing data frames as returned by `decompose_netify`.
#'   Must include:
#'   \itemize{
#'     \item \strong{nodal_data}: A data.frame containing node information with
#'       at minimum a 'name' column identifying each node
#'     \item \strong{edge_data}: A data.frame containing edge information with
#'       columns for source, target, and optionally edge weights
#'   }
#' @param obj_attrs A list of attributes from the netify object. Must include:
#'   \itemize{
#'     \item \strong{symmetric}: Logical indicating whether the network is
#'       undirected (TRUE) or directed (FALSE)
#'   }
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item \strong{plot_args}: The input plot_args list with all default values
#'       set for any unspecified parameters
#'     \item \strong{net_dfs}: The modified net_dfs list with additional columns
#'       for selective labeling (name_text and name_label)
#'   }
#'
#' @details
#' This function handles three main tasks:
#'
#' \strong{1. Setting Default Values}
#'
#' The function sets sensible defaults for all plotting parameters if not
#' explicitly provided by the user:
#'
#' \emph{General plot settings:}
#' \itemize{
#'   \item \code{use_theme_netify}: Use the netify theme (default: TRUE)
#'   \item \code{remove_isolates}: Remove isolated nodes (default: TRUE)
#'   \item \code{check_overlap}: Check for text/label overlap (default: TRUE)
#' }
#'
#' \emph{Geometry visibility:}
#' \itemize{
#'   \item \code{add_points}: Show nodes as points (default: TRUE)
#'   \item \code{add_text}: Show node names as text (default: FALSE)
#'   \item \code{add_label}: Show node names as labels (default: FALSE)
#'   \item \code{add_edges}: Show edges (default: TRUE)
#'   \item \code{curve_edges}: Use curved edges (default: FALSE)
#' }
#'
#' \emph{Point (node) appearance:}
#' \itemize{
#'   \item \code{point_alpha}: Transparency (default: NA)
#'   \item \code{point_color}: Border color (default: 'black')
#'   \item \code{point_fill}: Fill color (default: NA)
#'   \item \code{point_shape}: Shape code (default: 19)
#'   \item \code{point_size}: Size (default: 1.5)
#'   \item \code{point_stroke}: Border width (default: 0.5)
#' }
#'
#' \emph{Text appearance:}
#' \itemize{
#'   \item \code{text_alpha}: Transparency (default: NA)
#'   \item \code{text_color}: Color (default: 'black')
#'   \item \code{text_fill}: Background fill (default: 'white')
#'   \item \code{text_size}: Size (default: 3.88)
#'   \item \code{text_family}: Font family (default: '')
#'   \item \code{text_fontface}: Font face (default: 1)
#'   \item \code{text_angle}: Rotation angle (default: 0)
#'   \item \code{text_hjust}: Horizontal justification (default: 0.5)
#'   \item \code{text_vjust}: Vertical justification (default: 0.5)
#'   \item \code{text_lineheight}: Line height (default: 1.2)
#' }
#'
#' \emph{Label appearance:}
#' \itemize{
#'   \item \code{label_alpha}: Transparency (default: NA)
#'   \item \code{label_color}: Text color (default: 'black')
#'   \item \code{label_fill}: Background color (default: 'white')
#'   \item \code{label_size}: Size (default: 3.88)
#'   \item \code{label_family}: Font family (default: '')
#'   \item \code{label_fontface}: Font face (default: 1)
#'   \item \code{label_angle}: Rotation angle (default: 0)
#'   \item \code{label_hjust}: Horizontal justification (default: 0.5)
#'   \item \code{label_vjust}: Vertical justification (default: 0.5)
#'   \item \code{label_lineheight}: Line height (default: 1.2)
#' }
#'
#' \emph{Edge appearance:}
#' \itemize{
#'   \item \code{edge_color}: Color (default: 'black')
#'   \item \code{edge_linewidth}: Width (default: 0.5)
#'   \item \code{edge_linetype}: Line type (default: 1)
#'   \item \code{edge_alpha}: Transparency (default: NA)
#'   \item \code{edge_curvature}: Curvature amount (default: 0.5)
#'   \item \code{edge_angle}: Angle in degrees (default: 90)
#'   \item \code{edge_ncp}: Number of control points (default: 5)
#'   \item \code{edge_lineend}: Line end style (default: 'butt')
#'   \item \code{edge_linejoin}: Line join style (default: 'round')
#'   \item \code{edge_arrow}: Arrow specification for directed networks (default:
#'     arrow with 0.2cm length for directed networks, NULL for undirected)
#' }
#'
#' \strong{2. Selective Node Labeling}
#'
#' The function supports selective labeling of nodes through:
#' \itemize{
#'   \item \code{select_text}: Character vector of node names to show as text
#'   \item \code{select_text_display}: Optional character vector of alternative
#'     text to display (must match length and order of select_text)
#'   \item \code{select_label}: Character vector of node names to show as labels
#'   \item \code{select_label_display}: Optional character vector of alternative
#'     labels to display (must match length and order of select_label)
#' }
#'
#' When selective labeling is used, the function creates new columns in the
#' nodal_data data.frame (name_text and name_label) and automatically sets
#' the corresponding add_text or add_label parameter to TRUE.
#'
#' \strong{3. Edge Weight Handling}
#'
#' For weighted networks, the function automatically detects edge weights and
#' sets the edge_alpha_var parameter to map transparency to edge weights if no
#' alpha variable is specified by the user.
#'
#' @note
#' This function is primarily intended for internal use by netify plotting
#' functions.
#'
#' The function assumes that edge weights, if present, are stored in the fourth
#' column of the edge_data data.frame as created by `decompose_netify`.
#'
#' All color parameters accept standard R color specifications including named
#' colors, hex codes, and RGB values.
#'
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#'
#' @keywords internal
#' @noRd
NULL

#' Validate that *_by columns exist in the right data frame
#'
#' Catches user typos like `node_color_by = "deg_total"` (missing the second
#' "e") or referencing a column the user forgot to attach (e.g. asking for
#' "degree_total" without first calling `add_node_vars(net, summary_actor(net))`).
#' Without this guard ggplot fails downstream with "object 'X' not found",
#' which never names the offending kwarg or hints at available columns.
#'
#' @param plot_args internal-name plot args (already translated _by -> _var)
#' @param net_dfs list with `nodal_data` and `edge_data` data frames
#' @keywords internal
#' @noRd
validate_plot_by_cols <- function(plot_args, net_dfs) {
	# map internal _var arg names back to user-facing _by names for error messages
	node_var_args <- c(
		point_alpha_var  = "node_alpha_by",
		point_color_var  = "node_color_by",
		point_fill_var   = "node_fill_by",
		point_shape_var  = "node_shape_by",
		point_size_var   = "node_size_by",
		point_stroke_var = "node_stroke_by",
		text_alpha_var   = "text_alpha_by",
		text_color_var   = "text_color_by",
		text_size_var    = "text_size_by",
		label_alpha_var  = "label_alpha_by",
		label_color_var  = "label_color_by",
		label_fill_var   = "label_fill_by",
		label_size_var   = "label_size_by"
	)
	edge_var_args <- c(
		edge_alpha_var     = "edge_alpha_by",
		edge_color_var     = "edge_color_by",
		edge_linewidth_var = "edge_linewidth_by",
		edge_linetype_var  = "edge_linetype_by"
	)

	nodal_cols <- if (!is.null(net_dfs$nodal_data)) names(net_dfs$nodal_data) else character(0)
	edge_cols  <- if (!is.null(net_dfs$edge_data))  names(net_dfs$edge_data)  else character(0)

	check_one <- function(internal_arg, user_arg, df_cols, df_label) {
		col <- plot_args[[internal_arg]]
		if (is.null(col) || !is.character(col) || length(col) != 1) return(invisible())
		if (col %in% df_cols) return(invisible())

		# suggest closest match if available
		hint <- NULL
		if (length(df_cols) > 0) {
			dists <- utils::adist(col, df_cols)[1, ]
			best <- df_cols[which.min(dists)]
			if (length(best) == 1 && min(dists) <= max(3, ceiling(nchar(col) / 2))) {
				hint <- paste0("Did you mean '", best, "'?")
			}
		}

		# hint for network statistics that have not been attached yet
		stat_cols <- c(
			"degree", "degree_in", "degree_out", "degree_total",
			"closeness", "betweenness", "eigen_vector",
			"prop_ties", "network_share"
		)
		stat_hint <- if (col %in% stat_cols) {
			paste0(
				"'", col, "' is a network statistic. ",
				"Attach it first with: ",
				"net <- add_node_vars(net, summary_actor(net), \"actor\")"
			)
		} else NULL

		avail <- if (length(df_cols) > 0) {
			paste0("Available ", df_label, " columns: ",
				paste(shQuote(df_cols), collapse = ", "))
		} else {
			paste0("No ", df_label, " columns are attached to this netify object.")
		}

		msg <- c(
			"x" = paste0("`", user_arg, " = \"", col,
				"\"` refers to a column that does not exist."),
			if (!is.null(stat_hint)) c("i" = stat_hint),
			if (!is.null(hint))      c("i" = hint),
			"i" = avail
		)
		cli::cli_abort(msg, call = NULL)
	}

	for (internal_arg in names(node_var_args)) {
		check_one(internal_arg, node_var_args[[internal_arg]], nodal_cols, "nodal")
	}
	for (internal_arg in names(edge_var_args)) {
		check_one(internal_arg, edge_var_args[[internal_arg]], edge_cols, "edge")
	}

	invisible(NULL)
}

adjust_plot_args <- function(plot_args, net_dfs, obj_attrs) {
	# default to theme_netify
	if (is.null(plot_args$use_theme_netify)) {
		plot_args$use_theme_netify <- TRUE
	}

	# default to removing isolates
	if (is.null(plot_args$remove_isolates)) {
		plot_args$remove_isolates <- TRUE
	}

	# default to checking text/label overlap
	if (is.null(plot_args$check_overlap)) {
		plot_args$check_overlap <- TRUE
	}

	# ego network defaults
	if (isTRUE(obj_attrs$ego_netify)) {
		# keep ego alters even if they are isolates
		if (is.null(plot_args$remove_isolates) || plot_args$remove_isolates == TRUE) {
			plot_args$remove_isolates <- FALSE
		}

		# hierarchical layout default for ego networks
		if (is.null(plot_args$layout)) {
			plot_args$layout <- "hierarchical"
		}

		# radius defaults for hierarchical layout
		if (plot_args$layout == "hierarchical") {
			if (is.null(plot_args$min_radius)) {
				plot_args$min_radius <- 1.5
			}
			if (is.null(plot_args$max_radius)) {
				plot_args$max_radius <- 4.5
			}
		}

		# highlight the ego node by default
		ego_name <- obj_attrs$ego_id
		if (!is.null(ego_name) && is.null(plot_args$highlight)) {
			plot_args$highlight <- ego_name
			if (is.null(plot_args$highlight_color)) {
				plot_args$highlight_color <- c("#4daf4a", "grey40")
				names(plot_args$highlight_color) <- c(ego_name, "Other")
			}
			if (is.null(plot_args$highlight_size_increase)) {
				plot_args$highlight_size_increase <- c(1, 1)
			}
		}
	}

	# map node_* to point_* internally so users can use either
	node_to_point_mapping <- list(
		# static parameters
		"node_alpha" = "point_alpha",
		"node_color" = "point_color",
		"node_fill" = "point_fill",
		"node_shape" = "point_shape",
		"node_size" = "point_size",
		"node_stroke" = "point_stroke",
		# variable parameters - convert both _by and legacy _var
		"node_alpha_by" = "point_alpha_var",
		"node_color_by" = "point_color_var",
		"node_fill_by" = "point_fill_var",
		"node_shape_by" = "point_shape_var",
		"node_size_by" = "point_size_var",
		"node_stroke_by" = "point_stroke_var",
		# legacy _var support
		"node_alpha_var" = "point_alpha_var",
		"node_color_var" = "point_color_var",
		"node_fill_var" = "point_fill_var",
		"node_shape_var" = "point_shape_var",
		"node_size_var" = "point_size_var",
		"node_stroke_var" = "point_stroke_var"
	)

	for (old_name in names(node_to_point_mapping)) {
		new_name <- node_to_point_mapping[[old_name]]
		if (!is.null(plot_args[[old_name]]) && is.null(plot_args[[new_name]])) {
			plot_args[[new_name]] <- plot_args[[old_name]]
			plot_args[[old_name]] <- NULL
		}
	}

	# map _by to _var for edge, point, text, label parameters
	by_to_var_mapping <- list(
		# edge parameters
		"edge_alpha_by" = "edge_alpha_var",
		"edge_color_by" = "edge_color_var",
		"edge_linewidth_by" = "edge_linewidth_var",
		"edge_linetype_by" = "edge_linetype_var",
		# point parameters (in case used directly)
		"point_alpha_by" = "point_alpha_var",
		"point_color_by" = "point_color_var",
		"point_fill_by" = "point_fill_var",
		"point_shape_by" = "point_shape_var",
		"point_size_by" = "point_size_var",
		"point_stroke_by" = "point_stroke_var",
		# text parameters
		"text_alpha_by" = "text_alpha_var",
		"text_color_by" = "text_color_var",
		"text_size_by" = "text_size_var",
		# label parameters
		"label_alpha_by" = "label_alpha_var",
		"label_color_by" = "label_color_var",
		"label_fill_by" = "label_fill_var",
		"label_size_by" = "label_size_var"
	)

	for (old_name in names(by_to_var_mapping)) {
		new_name <- by_to_var_mapping[[old_name]]
		if (!is.null(plot_args[[old_name]]) && is.null(plot_args[[new_name]])) {
			plot_args[[new_name]] <- plot_args[[old_name]]
			plot_args[[old_name]] <- NULL
		}
	}

	# validate that columns referenced by *_by mappings exist on the data
	validate_plot_by_cols(plot_args, net_dfs)

	# palette settings
	if (!is.null(plot_args$palette)) {
		palette_settings <- get_palette(plot_args$palette)

		if (!is.null(palette_settings)) {
			# apply palette only when the user has not overridden
			if (is.null(plot_args$edge_color)) {
				plot_args$edge_color <- palette_settings$edge_color
			}
			if (is.null(plot_args$edge_alpha) && is.null(plot_args$edge_alpha_var)) {
				plot_args$edge_alpha <- palette_settings$edge_alpha
			}
			if (is.null(plot_args$point_fill) && is.null(plot_args$node_fill)) {
				plot_args$point_fill <- palette_settings$node_fill
			}
			if (is.null(plot_args$point_color) && is.null(plot_args$node_color)) {
				plot_args$point_color <- palette_settings$node_color
			}
			if (is.null(plot_args$curve_edges)) {
				plot_args$curve_edges <- palette_settings$curve_edges
			}
			# shape 21 supports the fill color
			if (is.null(plot_args$point_shape) && is.null(plot_args$node_shape)) {
				plot_args$point_shape <- 21
			}
		} else {
			cli::cli_alert_warning(
				"Unknown palette '{plot_args$palette}'. Use list_palettes() to see available options."
			)
		}
	}

	# geom choices
	if (is.null(plot_args$add_points)) {
		plot_args$add_points <- TRUE
	}
	# small networks get readable repel labels by default
	n_actors_plot <- if (!is.null(net_dfs$nodal_data)) {
		length(unique(net_dfs$nodal_data$name))
	} else {
		NA_integer_
	}
	small_net <- isTRUE(!is.na(n_actors_plot) && n_actors_plot <= 30)
	if (is.null(plot_args$add_text)) {
		plot_args$add_text <- FALSE
	}
	if (is.null(plot_args$add_label)) {
		plot_args$add_label <- FALSE
	}
	if (is.null(plot_args$add_text_repel)) {
		auto_text <- small_net &&
			!isTRUE(plot_args$add_text) &&
			!isTRUE(plot_args$add_label) &&
			!isTRUE(plot_args$add_label_repel) &&
			is.null(plot_args$select_text) &&
			is.null(plot_args$select_label)
		plot_args$add_text_repel <- auto_text
	}
	if (is.null(plot_args$add_label_repel)) {
		plot_args$add_label_repel <- FALSE
	}
	if (is.null(plot_args$add_edges)) {
		plot_args$add_edges <- TRUE
	}
	if (is.null(plot_args$curve_edges)) {
		plot_args$curve_edges <- FALSE
	}

	# selective text/label for individual nodes
	net_dfs$nodal_data$name_text <- net_dfs$nodal_data$name
	net_dfs$nodal_data$name_label <- net_dfs$nodal_data$name

	# apply user choice for text labels on specific nodes
	if (!is.null(plot_args$select_text) && length(plot_args$select_text) > 0) {
		if (!is.null(plot_args$select_text_display)) {
			# map select_text -> custom display text
			text_mapping <- setNames(plot_args$select_text_display, plot_args$select_text)

			net_dfs$nodal_data$name_text <- ifelse(
				net_dfs$nodal_data$name %in% plot_args$select_text,
				text_mapping[net_dfs$nodal_data$name],
				NA
			)
		} else {
			# use actor names as text on selected nodes
			net_dfs$nodal_data$name_text <- ifelse(
				net_dfs$nodal_data$name %in% plot_args$select_text,
				net_dfs$nodal_data$name,
				NA
			)
		}

		# prefer repel for selective labeling
		plot_args$add_text_repel <- TRUE
		plot_args$add_text <- FALSE
	}

	# apply user choice for box labels on specific nodes
	if (!is.null(plot_args$select_label) && length(plot_args$select_label) > 0) {
		if (!is.null(plot_args$select_label_display)) {
			# map select_label -> custom display label
			label_mapping <- setNames(plot_args$select_label_display, plot_args$select_label)

			net_dfs$nodal_data$name_label <- ifelse(
				net_dfs$nodal_data$name %in% plot_args$select_label,
				label_mapping[net_dfs$nodal_data$name],
				NA
			)
		} else {
			# use actor names as labels on selected nodes
			net_dfs$nodal_data$name_label <- ifelse(
				net_dfs$nodal_data$name %in% plot_args$select_label,
				net_dfs$nodal_data$name,
				NA
			)
		}

		# always use label_repel to avoid node overlap
		plot_args$add_label_repel <- TRUE
		plot_args$add_label <- FALSE
	}

	# text/label are mutually exclusive with their repel versions
	if (plot_args$add_text_repel && plot_args$add_text) {
		plot_args$add_text <- FALSE
	}
	if (plot_args$add_label_repel && plot_args$add_label) {
		plot_args$add_label <- FALSE
	}

	# static geom_point defaults
	if (is.null(plot_args$point_alpha)) {
		plot_args$point_alpha <- NA
	}
	if (is.null(plot_args$point_color)) {
		plot_args$point_color <- "black"
	}
	if (is.null(plot_args$point_fill)) {
		plot_args$point_fill <- NA
	}
	if (is.null(plot_args$point_shape)) {
		plot_args$point_shape <- 19
	}
	if (is.null(plot_args$point_size)) {
		plot_args$point_size <- 1.5
	}
	if (is.null(plot_args$point_stroke)) {
		plot_args$point_stroke <- 0.5
	}

	# static geom_text defaults
	if (is.null(plot_args$text_alpha)) {
		plot_args$text_alpha <- NA
	}
	if (is.null(plot_args$text_color)) {
		plot_args$text_color <- "black"
	}
	if (is.null(plot_args$text_fill)) {
		plot_args$text_fill <- "white"
	}
	if (is.null(plot_args$text_size)) {
		plot_args$text_size <- 3.88
	}
	if (is.null(plot_args$text_family)) {
		plot_args$text_family <- ""
	}
	if (is.null(plot_args$text_fontface)) {
		plot_args$text_fontface <- 1
	}
	if (is.null(plot_args$text_angle)) {
		plot_args$text_angle <- 0
	}
	if (is.null(plot_args$text_hjust)) {
		plot_args$text_hjust <- 0.5
	}
	if (is.null(plot_args$text_vjust)) {
		plot_args$text_vjust <- 0.5
	}
	if (is.null(plot_args$text_lineheight)) {
		plot_args$text_lineheight <- 1.2
	}

	# static geom_label defaults
	if (is.null(plot_args$label_alpha)) {
		plot_args$label_alpha <- NA
	}
	if (is.null(plot_args$label_color)) {
		plot_args$label_color <- "black"
	}
	if (is.null(plot_args$label_fill)) {
		plot_args$label_fill <- "white"
	}
	if (is.null(plot_args$label_size)) {
		plot_args$label_size <- 3.88
	}
	if (is.null(plot_args$label_family)) {
		plot_args$label_family <- ""
	}
	if (is.null(plot_args$label_fontface)) {
		plot_args$label_fontface <- 1
	}
	if (is.null(plot_args$label_angle)) {
		plot_args$label_angle <- 0
	}
	if (is.null(plot_args$label_hjust)) {
		plot_args$label_hjust <- 0.5
	}
	if (is.null(plot_args$label_vjust)) {
		plot_args$label_vjust <- 0.5
	}
	if (is.null(plot_args$label_lineheight)) {
		plot_args$label_lineheight <- 1.2
	}

	# static geom_text_repel defaults, scaled by network size
	repel_force_default <- if (isTRUE(n_actors_plot >= 20)) 3 else 1
	repel_max_overlaps_default <- if (isTRUE(n_actors_plot >= 20)) Inf else 10
	repel_box_pad_default <- if (isTRUE(n_actors_plot >= 20)) 0.6 else 0.25
	repel_point_pad_default <- if (isTRUE(n_actors_plot >= 20)) 0.3 else 0
	repel_min_seg_default <- if (isTRUE(n_actors_plot >= 20)) 0 else 0.5
	repel_max_time_default <- if (isTRUE(n_actors_plot >= 20)) 2 else 0.5
	# basic repel parameters
	if (is.null(plot_args$text_repel_force)) {
		plot_args$text_repel_force <- repel_force_default
	}
	if (is.null(plot_args$text_repel_force_pull)) {
		plot_args$text_repel_force_pull <- 1
	}
	if (is.null(plot_args$text_repel_max_overlaps)) {
		plot_args$text_repel_max_overlaps <- repel_max_overlaps_default
	}
	if (is.null(plot_args$text_repel_nudge_x)) {
		plot_args$text_repel_nudge_x <- 0
	}
	if (is.null(plot_args$text_repel_nudge_y)) {
		plot_args$text_repel_nudge_y <- 0
	}
	if (is.null(plot_args$text_repel_box_padding)) {
		plot_args$text_repel_box_padding <- repel_box_pad_default
	}
	if (is.null(plot_args$text_repel_point_padding)) {
		plot_args$text_repel_point_padding <- repel_point_pad_default
	}
	if (is.null(plot_args$text_repel_min_segment_length)) {
		plot_args$text_repel_min_segment_length <- repel_min_seg_default
	}
	if (is.null(plot_args$text_repel_arrow)) {
		plot_args$text_repel_arrow <- NULL
	}
	if (is.null(plot_args$text_repel_max_time)) {
		plot_args$text_repel_max_time <- repel_max_time_default
	}
	if (is.null(plot_args$text_repel_max_iter)) {
		plot_args$text_repel_max_iter <- 10000
	}
	if (is.null(plot_args$text_repel_seed)) {
		plot_args$text_repel_seed <- NA
	}
	if (is.null(plot_args$text_repel_xlim)) {
		plot_args$text_repel_xlim <- c(NA, NA)
	}
	if (is.null(plot_args$text_repel_ylim)) {
		plot_args$text_repel_ylim <- c(NA, NA)
	}
	if (is.null(plot_args$text_repel_direction)) {
		plot_args$text_repel_direction <- "both"
	}
	# segment aesthetics
	if (is.null(plot_args$text_repel_segment_color)) {
		plot_args$text_repel_segment_color <- "grey50"
	}
	if (is.null(plot_args$text_repel_segment_size)) {
		plot_args$text_repel_segment_size <- 0.5
	}
	if (is.null(plot_args$text_repel_segment_alpha)) {
		plot_args$text_repel_segment_alpha <- NA
	}
	if (is.null(plot_args$text_repel_segment_linetype)) {
		plot_args$text_repel_segment_linetype <- 1
	}
	if (is.null(plot_args$text_repel_segment_curvature)) {
		plot_args$text_repel_segment_curvature <- 0
	}
	if (is.null(plot_args$text_repel_segment_angle)) {
		plot_args$text_repel_segment_angle <- 90
	}
	if (is.null(plot_args$text_repel_segment_ncp)) {
		plot_args$text_repel_segment_ncp <- 1
	}
	if (is.null(plot_args$text_repel_segment_square)) {
		plot_args$text_repel_segment_square <- TRUE
	}
	if (is.null(plot_args$text_repel_segment_inflect)) {
		plot_args$text_repel_segment_inflect <- FALSE
	}

	# static geom_label_repel defaults
	if (is.null(plot_args$label_repel_force)) {
		plot_args$label_repel_force <- repel_force_default
	}
	if (is.null(plot_args$label_repel_force_pull)) {
		plot_args$label_repel_force_pull <- 1
	}
	if (is.null(plot_args$label_repel_max_overlaps)) {
		plot_args$label_repel_max_overlaps <- repel_max_overlaps_default
	}
	if (is.null(plot_args$label_repel_nudge_x)) {
		plot_args$label_repel_nudge_x <- 0
	}
	if (is.null(plot_args$label_repel_nudge_y)) {
		plot_args$label_repel_nudge_y <- 0
	}
	if (is.null(plot_args$label_repel_box_padding)) {
		plot_args$label_repel_box_padding <- repel_box_pad_default
	}
	if (is.null(plot_args$label_repel_point_padding)) {
		plot_args$label_repel_point_padding <- repel_point_pad_default
	}
	if (is.null(plot_args$label_repel_min_segment_length)) {
		plot_args$label_repel_min_segment_length <- repel_min_seg_default
	}
	if (is.null(plot_args$label_repel_arrow)) {
		plot_args$label_repel_arrow <- NULL
	}
	if (is.null(plot_args$label_repel_max_time)) {
		plot_args$label_repel_max_time <- repel_max_time_default
	}
	if (is.null(plot_args$label_repel_max_iter)) {
		plot_args$label_repel_max_iter <- 10000
	}
	if (is.null(plot_args$label_repel_seed)) {
		plot_args$label_repel_seed <- NA
	}
	if (is.null(plot_args$label_repel_xlim)) {
		plot_args$label_repel_xlim <- c(NA, NA)
	}
	if (is.null(plot_args$label_repel_ylim)) {
		plot_args$label_repel_ylim <- c(NA, NA)
	}
	if (is.null(plot_args$label_repel_direction)) {
		plot_args$label_repel_direction <- "both"
	}
	# segment aesthetics
	if (is.null(plot_args$label_repel_segment_color)) {
		plot_args$label_repel_segment_color <- "grey50"
	}
	if (is.null(plot_args$label_repel_segment_size)) {
		plot_args$label_repel_segment_size <- 0.5
	}
	if (is.null(plot_args$label_repel_segment_alpha)) {
		plot_args$label_repel_segment_alpha <- NA
	}
	if (is.null(plot_args$label_repel_segment_linetype)) {
		plot_args$label_repel_segment_linetype <- 1
	}
	if (is.null(plot_args$label_repel_segment_curvature)) {
		plot_args$label_repel_segment_curvature <- 0
	}
	if (is.null(plot_args$label_repel_segment_angle)) {
		plot_args$label_repel_segment_angle <- 90
	}
	if (is.null(plot_args$label_repel_segment_ncp)) {
		plot_args$label_repel_segment_ncp <- 1
	}
	if (is.null(plot_args$label_repel_segment_square)) {
		plot_args$label_repel_segment_square <- TRUE
	}
	if (is.null(plot_args$label_repel_segment_inflect)) {
		plot_args$label_repel_segment_inflect <- FALSE
	}
	# label box padding
	if (is.null(plot_args$label_repel_label_padding)) {
		plot_args$label_repel_label_padding <- 0.25
	}
	if (is.null(plot_args$label_repel_label_r)) {
		plot_args$label_repel_label_r <- 0.15
	}
	if (is.null(plot_args$label_repel_label_size)) {
		plot_args$label_repel_label_size <- 0.25
	}

	# highlight defaults, only when highlight is in use
	if (!is.null(plot_args$highlight) && length(plot_args$highlight) > 0) {
		if (is.null(plot_args$highlight_color)) {
			plot_args$highlight_color <- NULL
		}
		if (is.null(plot_args$highlight_label)) {
			plot_args$highlight_label <- NULL
		}
		if (is.null(plot_args$highlight_size_increase)) {
			plot_args$highlight_size_increase <- 1
		}
		if (is.null(plot_args$show_other_in_legend)) {
			plot_args$show_other_in_legend <- FALSE
		}
	}

	# geom_segment / curve / arrow defaults

	# segment
	if (is.null(plot_args$edge_color)) {
		plot_args$edge_color <- "black"
	}
	if (is.null(plot_args$edge_linewidth)) {
		plot_args$edge_linewidth <- 0.5
	}
	if (is.null(plot_args$edge_linetype)) {
		plot_args$edge_linetype <- 1
	}
	if (is.null(plot_args$edge_alpha)) {
		plot_args$edge_alpha <- NA
	}

	# curve
	if (is.null(plot_args$edge_curvature)) {
		plot_args$edge_curvature <- 0.5
	}
	if (is.null(plot_args$edge_angle)) {
		plot_args$edge_angle <- 90
	}
	if (is.null(plot_args$edge_ncp)) {
		plot_args$edge_ncp <- 5
	}

	# line end/join type
	if (is.null(plot_args$edge_lineend)) {
		plot_args$edge_lineend <- "butt"
	}
	if (is.null(plot_args$edge_linejoin)) {
		plot_args$edge_linejoin <- "round"
	}

	# arrow
	plot_args$edge_arrow_fill <- NULL
	if (!all(obj_attrs$symmetric)) {
		if (is.null(plot_args$edge_arrow)) {
			plot_args$edge_arrow <- ggplot2::arrow(length = unit(0.2, "cm"))
		}
	} else {
		# no arrows on symmetric networks
		plot_args$edge_arrow <- NULL
	}

	# scale label defaults
	# edge scale labels
	if (is.null(plot_args$edge_alpha_label)) {
		# default the label to the variable name
		if (!is.null(plot_args$edge_alpha_var)) {
			plot_args$edge_alpha_label <- plot_args$edge_alpha_var
		} else {
			plot_args$edge_alpha_label <- NULL
		}
	}
	if (is.null(plot_args$edge_color_label)) {
		plot_args$edge_color_label <- NULL
	}
	if (is.null(plot_args$edge_linewidth_label)) {
		plot_args$edge_linewidth_label <- NULL
	}
	if (is.null(plot_args$edge_linetype_label)) {
		plot_args$edge_linetype_label <- NULL
	}

	# node/point scale labels (accept either node_ or point_ prefix)
	if (is.null(plot_args$node_size_label) && is.null(plot_args$point_size_label)) {
		plot_args$node_size_label <- NULL
	} else {
		plot_args$node_size_label <- plot_args$node_size_label %||% plot_args$point_size_label
	}
	if (is.null(plot_args$node_color_label) && is.null(plot_args$point_color_label)) {
		plot_args$node_color_label <- NULL
	} else {
		plot_args$node_color_label <- plot_args$node_color_label %||% plot_args$point_color_label
	}
	if (is.null(plot_args$node_fill_label) && is.null(plot_args$point_fill_label)) {
		plot_args$node_fill_label <- NULL
	} else {
		plot_args$node_fill_label <- plot_args$node_fill_label %||% plot_args$point_fill_label
	}
	if (is.null(plot_args$node_shape_label) && is.null(plot_args$point_shape_label)) {
		plot_args$node_shape_label <- NULL
	} else {
		plot_args$node_shape_label <- plot_args$node_shape_label %||% plot_args$point_shape_label
	}
	if (is.null(plot_args$node_alpha_label) && is.null(plot_args$point_alpha_label)) {
		plot_args$node_alpha_label <- NULL
	} else {
		plot_args$node_alpha_label <- plot_args$node_alpha_label %||% plot_args$point_alpha_label
	}
	if (is.null(plot_args$node_stroke_label) && is.null(plot_args$point_stroke_label)) {
		plot_args$node_stroke_label <- NULL
	} else {
		plot_args$node_stroke_label <- plot_args$node_stroke_label %||% plot_args$point_stroke_label
	}

	# text scale labels
	if (is.null(plot_args$text_size_label)) {
		plot_args$text_size_label <- NULL
	}
	if (is.null(plot_args$text_color_label)) {
		plot_args$text_color_label <- NULL
	}
	if (is.null(plot_args$text_alpha_label)) {
		plot_args$text_alpha_label <- NULL
	}

	# label (box) scale labels
	if (is.null(plot_args$label_size_label)) {
		plot_args$label_size_label <- NULL
	}
	if (is.null(plot_args$label_color_label)) {
		plot_args$label_color_label <- NULL
	}
	if (is.null(plot_args$label_fill_label)) {
		plot_args$label_fill_label <- NULL
	}
	if (is.null(plot_args$label_alpha_label)) {
		plot_args$label_alpha_label <- NULL
	}

	# color palette parameters
	if (is.null(plot_args$node_color_palette)) {
		plot_args$node_color_palette <- NULL
	}
	if (is.null(plot_args$node_fill_palette)) {
		plot_args$node_fill_palette <- NULL
	}
	if (is.null(plot_args$edge_color_palette)) {
		plot_args$edge_color_palette <- NULL
	}
	if (is.null(plot_args$node_color_direction)) {
		plot_args$node_color_direction <- 1
	}
	if (is.null(plot_args$node_fill_direction)) {
		plot_args$node_fill_direction <- 1
	}
	if (is.null(plot_args$edge_color_direction)) {
		plot_args$edge_color_direction <- 1
	}

	plot_args <- apply_smart_palettes(plot_args, net_dfs)

	# process highlight nodes
	if (!is.null(plot_args$highlight) && length(plot_args$highlight) > 0) {
		plot_args$highlight <- as.character(plot_args$highlight)

		# highlight defaults
		if (is.null(plot_args$highlight_color)) {
			plot_args$highlight_color <- NULL
		}
		if (is.null(plot_args$highlight_label)) {
			plot_args$highlight_label <- NULL
		}
		if (is.null(plot_args$show_other_in_legend)) {
			plot_args$show_other_in_legend <- FALSE
		}

		# default to no size change for highlighted nodes
		if (is.null(plot_args$highlight_size_increase)) {
			plot_args$highlight_size_increase <- 1
		}

		# build a factor variable for highlighting
		net_dfs$nodal_data$highlight_status <- ifelse(
			net_dfs$nodal_data$name %in% plot_args$highlight,
			net_dfs$nodal_data$name,
			"Other"
		)

		# factor levels put highlighted nodes first, then "Other"
		highlight_levels <- c(plot_args$highlight, "Other")
		net_dfs$nodal_data$highlight_status <- factor(
			net_dfs$nodal_data$highlight_status,
			levels = highlight_levels[highlight_levels %in% net_dfs$nodal_data$highlight_status]
		)

		# map highlight_status to color or fill based on point shape
		if (is.null(plot_args$point_color_var) && is.null(plot_args$point_fill_var)) {
			if (!is.null(plot_args$point_shape) && plot_args$point_shape %in% c(21:25)) {
				plot_args$point_fill_var <- "highlight_status"
			} else {
				plot_args$point_color_var <- "highlight_status"
			}
		}

		# default highlight colors
		if (is.null(plot_args$highlight_color)) {
			n_highlights <- length(plot_args$highlight)
			if (n_highlights <= 3) {
				highlight_node_colors <- c("#E41A1C", "#377EB8", "#4DAF4A")[1:n_highlights]
			} else {
				assert_dependency("scales")
				highlight_node_colors <- scales::hue_pal()(n_highlights)
			}
			# grey for non-highlighted nodes
			plot_args$highlight_color <- c(highlight_node_colors, "#999999")
			names(plot_args$highlight_color) <- c(plot_args$highlight, "Other")
		}

		# size increase accepts a scalar or a vector
		if (length(plot_args$highlight_size_increase) > 1) {
			# vector must match length(highlight) + 1 for "Other"
			if (length(plot_args$highlight_size_increase) != length(plot_args$highlight) + 1) {
				cli::cli_abort("{.arg highlight_size_increase} must have length 1 or {length(highlight) + 1} (for 'Other' category)")
			}

			size_multipliers <- plot_args$highlight_size_increase
			names(size_multipliers) <- c(plot_args$highlight, "Other")

			if (is.null(plot_args$point_size_var)) {
				# build a size variable based on highlight status
				net_dfs$nodal_data$highlight_size <- plot_args$point_size
				for (i in seq_along(plot_args$highlight)) {
					net_dfs$nodal_data$highlight_size[net_dfs$nodal_data$name == plot_args$highlight[i]] <-
						plot_args$point_size * size_multipliers[plot_args$highlight[i]]
				}
				net_dfs$nodal_data$highlight_size[!net_dfs$nodal_data$name %in% plot_args$highlight] <-
					plot_args$point_size * size_multipliers["Other"]

				plot_args$point_size_var <- "highlight_size"
				plot_args$point_size_guide <- "none"
			} else {
				# combine with an existing size variable
				original_size_var <- plot_args$point_size_var

				if (original_size_var %in% names(net_dfs$nodal_data)) {
					net_dfs$nodal_data$highlight_combined_size <- net_dfs$nodal_data[[original_size_var]]

					for (i in seq_along(plot_args$highlight)) {
						net_dfs$nodal_data$highlight_combined_size[net_dfs$nodal_data$name == plot_args$highlight[i]] <-
							net_dfs$nodal_data[[original_size_var]][net_dfs$nodal_data$name == plot_args$highlight[i]] *
							size_multipliers[plot_args$highlight[i]]
					}
					net_dfs$nodal_data$highlight_combined_size[!net_dfs$nodal_data$name %in% plot_args$highlight] <-
						net_dfs$nodal_data[[original_size_var]][!net_dfs$nodal_data$name %in% plot_args$highlight] *
						size_multipliers["Other"]

					plot_args$point_size_var <- "highlight_combined_size"
				}
			}
		} else if (plot_args$highlight_size_increase != 1) {
			# scalar multiplier
			if (is.null(plot_args$point_size_var)) {
				net_dfs$nodal_data$highlight_size <- ifelse(
					net_dfs$nodal_data$name %in% plot_args$highlight,
					plot_args$point_size * plot_args$highlight_size_increase,
					plot_args$point_size
				)
				plot_args$point_size_var <- "highlight_size"

				# hide the size legend since it duplicates color
				plot_args$point_size_guide <- "none"
			} else {
				original_size_var <- plot_args$point_size_var

				# only modify if it is a simple column reference
				if (original_size_var %in% names(net_dfs$nodal_data)) {
					net_dfs$nodal_data$highlight_combined_size <- ifelse(
						net_dfs$nodal_data$name %in% plot_args$highlight,
						net_dfs$nodal_data[[original_size_var]] * plot_args$highlight_size_increase,
						net_dfs$nodal_data[[original_size_var]]
					)

					plot_args$point_size_var <- "highlight_combined_size"
				}
			}
		}

		plot_args$is_highlighting <- TRUE
	}

	# auto-assign edge_alpha_var for weighted networks
	# the weight column lives at position 4 of edge_data from decompose_netify
	if (length(unique(net_dfs$edge_data[, 4])) > 1) {
		if (is.null(plot_args$edge_alpha_var)) {
			weight_col_name <- names(net_dfs$edge_data)[4]
			# multilayer weight names contain commas
			if (!grepl(",", weight_col_name)) {
				plot_args$edge_alpha_var <- weight_col_name
			} else if (weight_col_name == "weight") {
				plot_args$edge_alpha_var <- "weight"
			}
		}
	}

	# friendlier label for the auto-assigned edge_alpha_var
	if (!is.null(plot_args$edge_alpha_var) &&
		(is.null(plot_args$edge_alpha_label) ||
			identical(plot_args$edge_alpha_label, "weight_var"))) {
		plot_args$edge_alpha_label <- if (identical(plot_args$edge_alpha_var, "weight_var")) {
			"Edge weight"
		} else {
			plot_args$edge_alpha_var
		}
	}

	return(
		list(
			plot_args = plot_args,
			net_dfs = net_dfs
		)
	)
}
