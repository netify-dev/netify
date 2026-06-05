#' Prepare netify data for network visualization
#'
#' `net_plot_data` processes a netify object and generates all necessary components
#' for network visualization. this function handles layout computation, aesthetic
#' parameter organization, and data structuring for subsequent plotting with ggplot2
#' or other visualization tools.
#'
#' @param netlet a netify object (class "netify") containing the network to be
#'   visualized. cross-sectional, longitudinal, and multilayer netify objects are
#'   supported.
#' @param plot_args a list of plotting arguments controlling visualization appearance
#'   and behavior. can include:
#'
#'   \strong{layout parameters:}
#'   \itemize{
#'     \item \code{point_layout}: pre-computed node positions as a data.frame or list
#'       of data.frames (for longitudinal networks). if provided, overrides layout
#'       algorithm selection
#'     \item \code{layout}: character string specifying the igraph layout algorithm.
#'       options: "nicely" (default), "fr" (fruchterman-reingold), "kk" (kamada-kawai),
#'       "circle", "star", "grid", "tree", "bipartite", and others. see
#'       \code{\link{get_node_layout}} for full list
#'     \item \code{static_actor_positions}: logical. if TRUE, maintains consistent
#'       node positions across time periods in longitudinal networks
#'     \item \code{which_static}: integer specifying which time period to use as
#'       the template for static positions
#'     \item \code{seed}: integer for reproducible random layouts
#'   }
#'
#'   \strong{display options:}
#'   \itemize{
#'     \item \code{remove_isolates}: logical. remove unconnected nodes (default: TRUE)
#'     \item \code{add_edges}: logical. include edges in visualization (default: TRUE)
#'     \item \code{curve_edges}: logical. use curved edges instead of straight (default: FALSE)
#'     \item \code{add_points}: logical. display nodes as points (default: TRUE)
#'     \item \code{add_text}: logical. add text labels to nodes (default: FALSE)
#'     \item \code{add_label}: logical. add boxed labels to nodes (default: FALSE)
#'   }
#'
#'   \strong{selective labeling:}
#'   \itemize{
#'     \item \code{select_text}: character vector of node names to label with text
#'     \item \code{select_label}: character vector of node names to label with boxes
#'   }
#'
#'   additional aesthetic parameters are processed by \code{adjust_plot_args}
#'   and \code{gg_params}.
#'
#' @return a list with three components for creating network visualizations:
#'   \itemize{
#'     \item \strong{plot_args}: processed plotting arguments with defaults applied
#'       and parameters validated. includes all layout and display settings
#'     \item \strong{ggnet_params}: organized aesthetic parameters for ggplot2 mapping.
#'       contains separate specifications for nodes, edges, text, and labels with both
#'       static and dynamic (data-mapped) aesthetics
#'     \item \strong{net_dfs}: data frames ready for plotting:
#'       \itemize{
#'         \item \code{nodal_data}: node information including positions (x, y),
#'           attributes, and any additional variables
#'         \item \code{edge_data}: edge information including endpoint coordinates
#'           (x1, y1, x2, y2) and edge attributes
#'       }
#'   }
#'
#' @details
#' this function serves as the data preparation layer for netify visualization,
#' performing several operations:
#'
#' \strong{data validation:}
#' \itemize{
#'   \item ensures the input is a valid netify object
#'   \item handles single-layer and multilayer networks
#'   \item validates ego networks contain only one ego
#' }
#'
#' \strong{layout computation:}
#' \itemize{
#'   \item generates node positions using specified algorithm if not provided
#'   \item calculates edge endpoint coordinates based on node positions
#'   \item handles both cross-sectional and longitudinal layouts
#' }
#'
#' \strong{data organization:}
#' \itemize{
#'   \item merges layout information with network attributes
#'   \item processes plotting arguments and applies defaults
#'   \item organizes aesthetic parameters for ggplot2 compatibility
#'   \item removes isolates if requested
#' }
#'
#' \strong{output structure:}
#'
#' the returned data is structured for direct use with ggplot2 or can be further
#' customized. the separation of layout, aesthetics, and data allows for flexible
#' visualization workflows.
#'
#' @note
#' this function is primarily designed for use with netify's plot method but can
#' be called directly for custom visualization workflows.
#'
#' for multilayer networks, the returned node and edge data include a \code{layer}
#' column.
#'
#' for ego networks with multiple egos, create separate visualizations and combine
#' them using packages like patchwork.
#'
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export net_plot_data

net_plot_data <- function(netlet, plot_args = list()) {
	netify_check(netlet)

	# handle multilayer networks by processing each layer
	layers <- attributes(netlet)$layers
	n_layers <- length(layers)

	if (n_layers > 1) {
		layer_results <- list()

		for (i in seq_along(layers)) {
			layer_name <- layers[i]

			layer_net <- subset_netify(netlet, layers = layer_name)
			layer_plot_args <- plot_args
			layer_weight <- attr(layer_net, "weight")
			if (!is.null(layer_weight) && length(layer_weight) == 1L &&
				!is.na(layer_weight) && !identical(layer_weight, "weight")) {
				edge_weight_args <- c(
					"edge_alpha_by", "edge_alpha_var",
					"edge_color_by", "edge_color_var",
					"edge_linewidth_by", "edge_linewidth_var"
				)
				for (arg in edge_weight_args) {
					if (identical(layer_plot_args[[arg]], "weight")) {
						layer_plot_args[[arg]] <- layer_weight
					}
				}
			}

			layer_result <- net_plot_data(layer_net, layer_plot_args)

			# tag rows with layer
			if (!is.null(layer_result$net_dfs$nodal_data) && nrow(layer_result$net_dfs$nodal_data) > 0) {
				layer_result$net_dfs$nodal_data$layer <- layer_name
			}
			if (!is.null(layer_result$net_dfs$edge_data) && nrow(layer_result$net_dfs$edge_data) > 0) {
				layer_result$net_dfs$edge_data$layer <- layer_name
			}

			layer_results[[i]] <- layer_result
		}

		# seed combined result from first layer's params
		combined_result <- layer_results[[1]]

		nodal_list <- lapply(layer_results, function(x) x$net_dfs$nodal_data)
		nodal_list <- nodal_list[!sapply(nodal_list, is.null)]
		if (length(nodal_list) > 0) {
			combined_result$net_dfs$nodal_data <- do.call(rbind, nodal_list)
			rownames(combined_result$net_dfs$nodal_data) <- NULL
		}

		edge_list <- lapply(layer_results, function(x) x$net_dfs$edge_data)
		edge_list <- edge_list[!sapply(edge_list, is.null)]
		if (length(edge_list) > 0) {
			# normalize the weight column name across layers
			for (i in seq_along(edge_list)) {
				if (ncol(edge_list[[i]]) >= 4) {
					names(edge_list[[i]])[4] <- "weight"
				}
			}
				combined_result$net_dfs$edge_data <- do.call(rbind, edge_list)
				rownames(combined_result$net_dfs$edge_data) <- NULL
				for (arg in c("edge_alpha_var", "edge_color_var", "edge_linewidth_var", "edge_linetype_var")) {
					user_arg <- sub("_var$", "_by", arg)
					if (identical(plot_args[[arg]], "weight") ||
						identical(plot_args[[user_arg]], "weight")) {
						combined_result$plot_args[[arg]] <- "weight"
					}
				}

				if (!isFALSE(plot_args$edge_alpha)) {
					combined_result$plot_args$edge_alpha_var <- "weight"

				if (is.null(combined_result$plot_args$edge_alpha_label)) {
					combined_result$plot_args$edge_alpha_label <- "Edge Weight"
				}
			}

			if (isTRUE(plot_args$rescale_edge_weights)) {
				all_weights <- combined_result$net_dfs$edge_data$weight

				valid_weights <- all_weights[!is.na(all_weights)]

				if (length(valid_weights) > 0) {
					min_weight <- min(valid_weights)
					max_weight <- max(valid_weights)

					if (max_weight > min_weight) {
						combined_result$net_dfs$edge_data$weight <-
							(combined_result$net_dfs$edge_data$weight - min_weight) /
								(max_weight - min_weight)

						if (is.null(plot_args$edge_alpha_label)) {
							combined_result$plot_args$edge_alpha_label <- "Edge Weight (Rescaled)"
						}
					}
				}
			}
		}

		combined_result$plot_args$is_multilayer <- TRUE
		combined_result$plot_args$n_layers <- n_layers
		combined_result$plot_args$layer_names <- layers

		# detect temporal multilayer
		is_temporal <- FALSE
		if ("time" %in% names(combined_result$net_dfs$nodal_data)) {
			n_times <- length(unique(combined_result$net_dfs$nodal_data$time))
			is_temporal <- n_times > 1
		}

		if (is_temporal) {
			facet_type <- plot_args$facet_type %||% "grid"
			if (facet_type == "grid") {
				# center each network at (0,0) for facet_grid display
				if (!is.null(combined_result$net_dfs$nodal_data) && nrow(combined_result$net_dfs$nodal_data) > 0) {
					nodal_data <- combined_result$net_dfs$nodal_data
					unique_combos <- unique(nodal_data[, c("time", "layer")])

					centering_adj <- data.frame()
					for (i in 1:nrow(unique_combos)) {
						time_val <- unique_combos$time[i]
						layer_val <- unique_combos$layer[i]

						subset_data <- nodal_data[nodal_data$time == time_val & nodal_data$layer == layer_val, ]

						centering_adj <- rbind(centering_adj, data.frame(
							time = time_val,
							layer = layer_val,
							x_center = mean(subset_data$x, na.rm = TRUE),
							y_center = mean(subset_data$y, na.rm = TRUE)
						))
					}

					nodal_data <- merge(nodal_data, centering_adj, by = c("time", "layer"))
					nodal_data$x <- nodal_data$x - nodal_data$x_center
					nodal_data$y <- nodal_data$y - nodal_data$y_center
					nodal_data$x_center <- NULL
					nodal_data$y_center <- NULL
					combined_result$net_dfs$nodal_data <- nodal_data

					if (!is.null(combined_result$net_dfs$edge_data) && nrow(combined_result$net_dfs$edge_data) > 0) {
						edge_data <- combined_result$net_dfs$edge_data
						edge_data <- merge(edge_data, centering_adj, by = c("time", "layer"))
						edge_data$x1 <- edge_data$x1 - edge_data$x_center
						edge_data$y1 <- edge_data$y1 - edge_data$y_center
						edge_data$x2 <- edge_data$x2 - edge_data$x_center
						edge_data$y2 <- edge_data$y2 - edge_data$y_center
						edge_data$x_center <- NULL
						edge_data$y_center <- NULL
						combined_result$net_dfs$edge_data <- edge_data
					}

					x_range <- range(combined_result$net_dfs$nodal_data$x, na.rm = TRUE)
					y_range <- range(combined_result$net_dfs$nodal_data$y, na.rm = TRUE)
					max_extent <- max(abs(c(x_range, y_range)))

					combined_result$plot_args$xlim <- c(-max_extent, max_extent)
					combined_result$plot_args$ylim <- c(-max_extent, max_extent)
				}
			}
		}

		combined_result$ggnet_params <- gg_params(combined_result$plot_args)

		return(combined_result)
	}

	# ego networks: only one ego allowed
	ego_netlet <- attr(netlet, "ego_netlet")
	ego_longit <- FALSE
	if (!is.null(ego_netlet)) {
		ego_longit <- attr(netlet, "ego_longit")
		ego_entry <- attr(netlet, "ego_entry")

		if (length(ego_entry) > 1) {
			cli::cli_abort(
				"This object has multiple egos.
				`plot` does not currently support multiple ego inputs.
				For plotting purposes please create a `netify` object with a single ego,
				and consider just patching (via `patchwork`, for example) the plots
				together."
			)
		}
	}

	obj_attrs <- attributes(netlet)
	msrmnts <- netify_measurements(netlet)

	# apply smart defaults unless disabled
	auto_format <- plot_args$auto_format %||% TRUE
	plot_args$auto_format <- NULL

	if (auto_format) {
		plot_args <- get_smart_defaults(netlet, msrmnts, plot_args)
	}

	g_list <- netify_to_igraph(netlet,
		add_nodal_attribs = FALSE,
		add_dyad_attribs = FALSE,
		.quiet_na = TRUE
	)

	if (is.null(plot_args$point_layout)) {
		ego_params <- plot_args[grep("^ego_", names(plot_args))]

		nodes_list <- do.call(get_node_layout, c(
			list(
				netlet = netlet,
				ig_netlet = g_list,
				layout = plot_args$layout,
				static_actor_positions = plot_args$static_actor_positions,
				which_static = plot_args$which_static,
				seed = plot_args$seed
			),
			ego_params
		))
	} else {
		nodes_list <- plot_args$point_layout
	}

	edges_list <- get_edge_layout(
		netlet = netlet,
		nodes_layout = nodes_list,
		ig_netlet = g_list
	)

	# merge layout with nodal/dyadic attributes for plotting
	net_dfs <- merge_layout_attribs(netlet, nodes_list, edges_list)

	out <- adjust_plot_args(plot_args, net_dfs, obj_attrs)
	plot_args <- out$plot_args
	net_dfs <- out$net_dfs
	rm(out)

	ggnet_params <- gg_params(plot_args)

	if (plot_args$remove_isolates) {
		actor_summ <- summary_actor(netlet)

		# pick degree column based on directedness
		if (all(obj_attrs$symmetric)) {
			to_keep <- actor_summ[actor_summ$degree > 0, ]
		} else if (length(obj_attrs$symmetric) > 1) {
			# mixed multilayer: keep if any degree column shows ties
			has_degree <- !is.na(actor_summ$degree) & actor_summ$degree > 0
			has_degree_total <- !is.na(actor_summ$degree_total) & actor_summ$degree_total > 0
			to_keep <- actor_summ[has_degree | has_degree_total, ]
		} else {
			to_keep <- actor_summ[actor_summ$degree_total > 0, ]
		}

		if (obj_attrs$netify_type == "cross_sec") {
			to_keep$time <- net_dfs$nodal_data$time[1]
		}

		to_keep$id <- with(to_keep, paste(actor, time, sep = "_"))
		net_dfs$nodal_data$id <- with(net_dfs$nodal_data, paste(name, time, sep = "_"))

		net_dfs$nodal_data <- net_dfs$nodal_data[net_dfs$nodal_data$id %in% to_keep$id, ]
	}

	return(
		list(
			plot_args = plot_args,
			ggnet_params = ggnet_params,
			net_dfs = net_dfs
		)
	)
}
