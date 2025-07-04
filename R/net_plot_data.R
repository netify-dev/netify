#' Prepare netify data for network visualization
#'
#' `net_plot_data` processes a netify object and generates all necessary components
#' for network visualization. This function handles layout computation, aesthetic
#' parameter organization, and data structuring for subsequent plotting with ggplot2
#' or other visualization tools.
#'
#' @param netlet A netify object (class "netify") containing the network to be
#'   visualized. Must be a single-layer network (multilayer networks not currently
#'   supported).
#' @param plot_args A list of plotting arguments controlling visualization appearance
#'   and behavior. Can include:
#'
#'   \strong{Layout parameters:}
#'   \itemize{
#'     \item \code{point_layout}: Pre-computed node positions as a data.frame or list
#'       of data.frames (for longitudinal networks). If provided, overrides layout
#'       algorithm selection
#'     \item \code{layout}: Character string specifying the igraph layout algorithm.
#'       Options: "nicely" (default), "fr" (Fruchterman-Reingold), "kk" (Kamada-Kawai),
#'       "circle", "star", "grid", "tree", "bipartite", and others. See
#'       \code{\link{get_node_layout}} for full list
#'     \item \code{static_actor_positions}: Logical. If TRUE, maintains consistent
#'       node positions across time periods in longitudinal networks
#'     \item \code{which_static}: Integer specifying which time period to use as
#'       the template for static positions
#'     \item \code{seed}: Integer for reproducible random layouts
#'   }
#'
#'   \strong{Display options:}
#'   \itemize{
#'     \item \code{remove_isolates}: Logical. Remove unconnected nodes (default: TRUE)
#'     \item \code{add_edges}: Logical. Include edges in visualization (default: TRUE)
#'     \item \code{curve_edges}: Logical. Use curved edges instead of straight (default: FALSE)
#'     \item \code{add_points}: Logical. Display nodes as points (default: TRUE)
#'     \item \code{add_text}: Logical. Add text labels to nodes (default: FALSE)
#'     \item \code{add_label}: Logical. Add boxed labels to nodes (default: FALSE)
#'   }
#'
#'   \strong{Selective labeling:}
#'   \itemize{
#'     \item \code{select_text}: Character vector of node names to label with text
#'     \item \code{select_label}: Character vector of node names to label with boxes
#'   }
#'
#'   Additional aesthetic parameters are processed by \code{adjust_plot_args}
#'   and \code{gg_params}.
#'
#' @return A list with three components for creating network visualizations:
#'   \itemize{
#'     \item \strong{plot_args}: Processed plotting arguments with defaults applied
#'       and parameters validated. Includes all layout and display settings
#'     \item \strong{ggnet_params}: Organized aesthetic parameters for ggplot2 mapping.
#'       Contains separate specifications for nodes, edges, text, and labels with both
#'       static and dynamic (data-mapped) aesthetics
#'     \item \strong{net_dfs}: Data frames ready for plotting:
#'       \itemize{
#'         \item \code{nodal_data}: Node information including positions (x, y),
#'           attributes, and any additional variables
#'         \item \code{edge_data}: Edge information including endpoint coordinates
#'           (x1, y1, x2, y2) and edge attributes
#'       }
#'   }
#'
#' @details
#' This function serves as the data preparation layer for netify visualization,
#' performing several operations:
#'
#' \strong{Data validation:}
#' \itemize{
#'   \item Ensures the input is a valid netify object
#'   \item Checks for single-layer networks (multilayer not supported)
#'   \item Validates ego networks contain only one ego
#' }
#'
#' \strong{Layout computation:}
#' \itemize{
#'   \item Generates node positions using specified algorithm if not provided
#'   \item Calculates edge endpoint coordinates based on node positions
#'   \item Handles both cross-sectional and longitudinal layouts
#' }
#'
#' \strong{Data organization:}
#' \itemize{
#'   \item Merges layout information with network attributes
#'   \item Processes plotting arguments and applies defaults
#'   \item Organizes aesthetic parameters for ggplot2 compatibility
#'   \item Removes isolates if requested
#' }
#'
#' \strong{Output structure:}
#'
#' The returned data is structured for direct use with ggplot2 or can be further
#' customized. The separation of layout, aesthetics, and data allows for flexible
#' visualization workflows.
#'
#' @note
#' This function is primarily designed for use with netify's plot method but can
#' be called directly for custom visualization workflows.
#'
#' For multilayer networks, use \code{\link{subset_netify}} to extract individual
#' layers before visualization.
#'
#' For ego networks with multiple egos, create separate visualizations and combine
#' them using packages like patchwork.
#'
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export net_plot_data

net_plot_data <- function(netlet, plot_args = list()) {
    ######################
    # check if netify object
    netify_check(netlet)
    ######################

    ######################
    # Handle multilayer networks by processing each layer
    layers <- attributes(netlet)$layers
    n_layers <- length(layers)

    if (n_layers > 1) {
        # Process multilayer networks
        layer_results <- list()

        for (i in seq_along(layers)) {
            layer_name <- layers[i]

            # Subset to single layer
            layer_net <- subset_netify(netlet, layers = layer_name)

            # Recursive call to process this single layer
            # Pass plot_args through
            layer_result <- net_plot_data(layer_net, plot_args)

            # Add layer information to the data frames
            if (!is.null(layer_result$net_dfs$nodal_data) && nrow(layer_result$net_dfs$nodal_data) > 0) {
                layer_result$net_dfs$nodal_data$layer <- layer_name
            }
            if (!is.null(layer_result$net_dfs$edge_data) && nrow(layer_result$net_dfs$edge_data) > 0) {
                layer_result$net_dfs$edge_data$layer <- layer_name
            }

            layer_results[[i]] <- layer_result
        }

        # Combine results from all layers
        # Use the first layer's plot_args and ggnet_params
        combined_result <- layer_results[[1]]

        # Combine nodal data
        nodal_list <- lapply(layer_results, function(x) x$net_dfs$nodal_data)
        nodal_list <- nodal_list[!sapply(nodal_list, is.null)]
        if (length(nodal_list) > 0) {
            combined_result$net_dfs$nodal_data <- do.call(rbind, nodal_list)
            rownames(combined_result$net_dfs$nodal_data) <- NULL
        }

        # Combine edge data
        edge_list <- lapply(layer_results, function(x) x$net_dfs$edge_data)
        edge_list <- edge_list[!sapply(edge_list, is.null)]
        if (length(edge_list) > 0) {
            # Before combining, ensure consistent column names
            # The 4th column is typically the weight column, but might have different names
            for (i in seq_along(edge_list)) {
                if (ncol(edge_list[[i]]) >= 4) {
                    # Rename the weight column to a generic name
                    names(edge_list[[i]])[4] <- "weight"
                }
            }
            combined_result$net_dfs$edge_data <- do.call(rbind, edge_list)
            rownames(combined_result$net_dfs$edge_data) <- NULL

            # For multilayer, set edge_alpha_var to the generic weight column
            # if the user hasn't explicitly disabled it
            if (!isFALSE(plot_args$edge_alpha)) {
                combined_result$plot_args$edge_alpha_var <- "weight"

                # Set a default label if not provided
                if (is.null(combined_result$plot_args$edge_alpha_label)) {
                    combined_result$plot_args$edge_alpha_label <- "Edge Weight"
                }
            }

            # Handle edge weight rescaling if requested
            if (isTRUE(plot_args$rescale_edge_weights)) {
                # Get all edge weights across layers
                all_weights <- combined_result$net_dfs$edge_data$weight

                # Remove NA values for scaling
                valid_weights <- all_weights[!is.na(all_weights)]

                if (length(valid_weights) > 0) {
                    # Rescale to 0-1 range
                    min_weight <- min(valid_weights)
                    max_weight <- max(valid_weights)

                    if (max_weight > min_weight) {
                        combined_result$net_dfs$edge_data$weight <-
                            (combined_result$net_dfs$edge_data$weight - min_weight) /
                                (max_weight - min_weight)

                        # Update the label to indicate rescaling
                        if (is.null(plot_args$edge_alpha_label)) {
                            combined_result$plot_args$edge_alpha_label <- "Edge Weight (Rescaled)"
                        }
                    }
                }
            }
        }

        # Add multilayer flag to plot_args
        combined_result$plot_args$is_multilayer <- TRUE
        combined_result$plot_args$n_layers <- n_layers
        combined_result$plot_args$layer_names <- layers

        # For temporal multilayer networks, center each network if using default facet_grid
        # Check if this is actually temporal (multiple time periods)
        is_temporal <- FALSE
        if ("time" %in% names(combined_result$net_dfs$nodal_data)) {
            n_times <- length(unique(combined_result$net_dfs$nodal_data$time))
            is_temporal <- n_times > 1
        }

        if (is_temporal) {
            facet_type <- plot_args$facet_type %||% "grid"
            if (facet_type == "grid") {
                # Center each network at (0,0) for consistent display in facet_grid
                if (!is.null(combined_result$net_dfs$nodal_data) && nrow(combined_result$net_dfs$nodal_data) > 0) {
                    # Calculate centering adjustments for each time-layer combination
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

                    # Apply centering to node positions
                    nodal_data <- merge(nodal_data, centering_adj, by = c("time", "layer"))
                    nodal_data$x <- nodal_data$x - nodal_data$x_center
                    nodal_data$y <- nodal_data$y - nodal_data$y_center
                    nodal_data$x_center <- NULL
                    nodal_data$y_center <- NULL
                    combined_result$net_dfs$nodal_data <- nodal_data

                    # Apply same centering to edge coordinates
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

                    # Calculate common axis limits
                    x_range <- range(combined_result$net_dfs$nodal_data$x, na.rm = TRUE)
                    y_range <- range(combined_result$net_dfs$nodal_data$y, na.rm = TRUE)
                    max_extent <- max(abs(c(x_range, y_range)))

                    # Store limits in plot_args for later use
                    combined_result$plot_args$xlim <- c(-max_extent, max_extent)
                    combined_result$plot_args$ylim <- c(-max_extent, max_extent)
                }
            }
        }

        # Regenerate ggnet_params with the updated plot_args that includes edge_alpha_var
        combined_result$ggnet_params <- gg_params(combined_result$plot_args)

        return(combined_result)
    }
    ######################

    ######################
    # if ego net modify names and stop if more than one ego
    ego_netlet <- attr(netlet, "ego_netlet")
    ego_longit <- FALSE
    if (!is.null(ego_netlet)) {
        # get info on ego network
        ego_longit <- attr(netlet, "ego_longit")
        ego_entry <- attr(netlet, "ego_entry")

        # stop if more than one ego
        if (length(ego_entry) > 1) {
            cli::cli_alert_danger(
                "Error: This object has multiple egos.
                `plot` does not currently support multiple ego inputs.
                For plotting purposes please create a `netify` object with a single ego,
                and consider just patching (via `patchwork`, for example) the plots
                together."
            )
            stop()
        }
    }
    ######################

    ######################
    # pull out attrs
    obj_attrs <- attributes(netlet)

    # pull out msrmnts
    msrmnts <- netify_measurements(netlet)

    # try to set some smart defaults
    plot_args <- get_smart_defaults(netlet, msrmnts, plot_args)
    ######################

    ######################
    #
    g_list <- netify_to_igraph(netlet,
        add_nodal_attribs = FALSE,
        add_dyad_attribs = FALSE
    )

    # Modify get_node_layout to accept pre-converted igraph
    if (is.null(plot_args$point_layout)) {
        # Extract ego-specific parameters from plot_args
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
        # Use provided custom layout
        nodes_list <- plot_args$point_layout
    }

    # Modify get_edge_layout to use the same igraph
    edges_list <- get_edge_layout(
        netlet = netlet,
        nodes_layout = nodes_list,
        ig_netlet = g_list
    )
    ######################

    ######################
    # org the netlet into a  and dyadic df with all the
    # relev attributes so that we can plot
    net_dfs <- merge_layout_attribs(netlet, nodes_list, edges_list)
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
    ggnet_params <- gg_params(plot_args)
    ######################

    # ######################
    # # check if the network is directed and arrow endpoints need adjustment
    # if (!obj_attrs$symmetric && plot_args$adjust_arrow_endpoints && plot_args$add_edges) {

    # # figure out the size of nodes being used
    # if (!is.null(plot_args$point_size_var)) {
    #     # nodes have variable sizes, use the column name for size info
    #     node_size_info <- plot_args$point_size_var
    # } else {
    #     # nodes have fixed sizes, use the numeric value for size info
    #     node_size_info <- plot_args$point_size
    # }

    # # calculate the size scale if it hasn't been provided
    # if (is.null(plot_args$edge_arrow_size_scale)) {
    #     size_scale <- calculate_size_scale(net_dfs$nodal_data)
    # } else {
    #     size_scale <- plot_args$edge_arrow_size_scale
    # }

    # # adjust the endpoints of edges to account for node sizes and other parameters
    # net_dfs$edge_data <- adjust_edge_endpoints(
    #     edge_data = net_dfs$edge_data,
    #     node_data = net_dfs$nodal_data,
    #     node_size = node_size_info,
    #     size_scale = size_scale,
    #     arrow_gap = plot_args$edge_arrow_gap,
    #     curved = plot_args$curve_edges,
    #     curvature = plot_args$edge_curvature %||% 0.5
    # )
    # }
    # ######################

    ######################
    # remove isolates
    if (plot_args$remove_isolates) {
        # get actor summary
        actor_summ <- summary_actor(netlet)

        # figure out what to keep and need to
        # do separate for symmetric and asymmetric
        if (obj_attrs$symmetric) {
            to_keep <- actor_summ[actor_summ$degree > 0, ]
        } else {
            to_keep <- actor_summ[actor_summ$degree_total > 0, ]
        }

        # add time var to to_keep if cross_sec
        if (obj_attrs$netify_type == "cross_sec") {
            to_keep$time <- net_dfs$nodal_data$time[1]
        }

        # create id vars
        to_keep$id <- with(to_keep, paste(actor, time, sep = "_"))
        net_dfs$nodal_data$id <- with(net_dfs$nodal_data, paste(name, time, sep = "_"))
        # net_dfs$edge_data$from_id = with(net_dfs$edge_data, paste(from, time, sep='_'))
        # net_dfs$edge_data$to_id = with(net_dfs$edge_data, paste(to, time, sep='_'))

        # subset
        net_dfs$nodal_data <- net_dfs$nodal_data[net_dfs$nodal_data$id %in% to_keep$id, ]
    }
    ######################

    ######################
    # Return all prepared data
    return(
        list(
            plot_args = plot_args,
            ggnet_params = ggnet_params,
            net_dfs = net_dfs
        )
    )
}
######################
