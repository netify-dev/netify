#' Visualize homophily analysis results
#'
#' Creates visualizations for homophily analysis results from \code{homophily()}.
#' The function can create different types of plots including similarity distributions,
#' comparison plots across multiple attributes, and temporal evolution plots.
#'
#' @param homophily_results Data frame output from \code{homophily()} or a list
#'   of such data frames for comparison plots.
#' @param netlet Optional. The netify object used in the analysis. Required for
#'   distribution plots to extract actual similarity data.
#' @param type Character string specifying the plot type:
#'   \describe{
#'     \item{"distribution"}{Shows similarity score distributions for connected vs
#'       unconnected pairs (requires netlet)}
#'     \item{"comparison"}{Compares homophily across multiple attributes}
#'     \item{"temporal"}{Shows homophily evolution over time (for longitudinal data)}
#'   }
#' @param attribute Character string. For distribution plots, specifies which attribute
#'   to visualize. Should match the attribute used in \code{homophily()}.
#' @param method Character string. For distribution plots, the similarity method used.
#'   Should match the method used in \code{homophily()}.
#' @param sample_size Integer. For distribution plots with large networks, the number
#'   of dyad pairs to sample for visualization. Default is NULL (use all pairs).
#' @param colors Character vector of two colors for connected/unconnected or
#'   significant/non-significant pairs. Default uses package theme colors.
#' @param ... Additional arguments passed to ggplot2 functions.
#'
#' @return A ggplot2 object that can be further customized.
#'
#' @examples
#' \dontrun{
#' # Load example data
#' data(icews)
#'
#' # Create a simple network
#' ntwk <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j",
#'     time = "year",
#'     symmetric = FALSE,
#'     weight = "matlCoop"
#' )
#'
#' # Run homophily analysis
#' homophily_result <- homophily(
#'     ntwk,
#'     attribute = "i_polity2",
#'     method = "correlation"
#' )
#'
#' # Create distribution plot
#' plot_homophily(
#'     homophily_result,
#'     netlet = ntwk,
#'     type = "distribution",
#'     attribute = "i_polity2"
#' )
#' }
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom stats complete.cases
#' @export plot_homophily

plot_homophily <- function(
    homophily_results,
    netlet = NULL,
    type = c("distribution", "comparison", "temporal"),
    attribute = NULL,
    method = "correlation",
    sample_size = NULL,
    colors = c("#2E86AB", "#F18F01"),
    ...) {
    # what we doing
    type <- match.arg(type)

    # For distribution plots, we need both netlet and attribute
    if (type == "distribution" && (is.null(netlet) || is.null(attribute))) {
        cli::cli_abort(c(
            "x" = "Distribution plots require both 'netlet' and 'attribute' parameters.",
            "i" = "Try: plot_homophily(homophily_results, netlet = your_network, attribute = 'your_attribute')"
        ))
    }

    # Check if this is a multilayer network and suggest alternative
    if (type == "distribution" && !is.null(netlet)) {
        obj_attrs <- attributes(netlet)
        layers <- obj_attrs$layers
        if (length(layers) > 1 && obj_attrs$netify_type == "cross_sec") {
            cli::cli_alert_warning("Distribution plots for multilayer networks may have issues. Consider extracting individual layers first.")
        }
    }

    # go do a thing
    switch(type,
        "distribution" = plot_homophily_distribution(
            homophily_results, netlet,
            attribute, method, sample_size,
            colors, ...
        ),
        "comparison" = plot_homophily_comparison(homophily_results, colors, ...),
        "temporal" = plot_homophily_temporal(homophily_results, colors, ...)
    )
}

#' Create distribution plot for homophily analysis
#'
#' Internal function to create distribution plots showing similarity scores
#' for connected vs unconnected pairs
#'
#' @param homophily_results Data frame from homophily
#' @param netlet netify object
#' @param attribute Character string of attribute name
#' @param method Character string of similarity method
#' @param sample_size Integer for sampling
#' @param colors Character vector of colors
#' @param ... Additional arguments
#'
#' @return A ggplot2 object
#'
#' @keywords internal
#' @importFrom rlang .data
#' @noRd

plot_homophily_distribution <- function(
    homophily_results, netlet, attribute,
    method, sample_size, colors, ...) {
    # validate input parameters
    if (is.null(netlet)) {
        cli::cli_abort(c(
            "x" = "netlet is required for distribution plots."
        ))
    }

    if (is.null(attribute)) {
        cli::cli_abort(c(
            "x" = "attribute must be specified for distribution plots."
        ))
    }

    # extract similarity and tie data from the network
    similarity_data <- extract_similarity_data(netlet, attribute, method)

    # check if similarity data was successfully extracted
    if (is.null(similarity_data) || nrow(similarity_data) == 0) {
        cli::cli_abort(c(
            "x" = "Could not extract similarity data from the network.",
            "i" = "This often happens when the network matrix and attribute data cannot be properly matched.",
            "i" = "For multilayer networks, try plotting individual layers separately."
        ))
    }

    # Debug: Check the structure of similarity_data
    cli::cli_alert_info("Similarity data dimensions: {nrow(similarity_data)} rows x {ncol(similarity_data)} cols")
    cli::cli_alert_info("Connected levels: {paste(levels(similarity_data$connected), collapse=', ')}")
    cli::cli_alert_info("Connected counts: Connected={sum(similarity_data$connected == 'Connected')}, Not Connected={sum(similarity_data$connected == 'Not Connected')}")

    # sample the data if a sample size is provided and the data is larger than the sample size
    if (!is.null(sample_size) && nrow(similarity_data) > sample_size) {
        similarity_data <- similarity_data[sample(nrow(similarity_data), sample_size), ]
    }

    # extract statistics from homophily results
    if (is.data.frame(homophily_results)) {
        # use the first row of the data frame, assuming it represents the relevant time period
        stats <- homophily_results[1, ]
    } else {
        cli::cli_abort(c(
            "x" = "homophily_results must be a data frame from homophily()."
        ))
    }

    # determine if the similarity data is categorical (binary similarity values)
    unique_sim_vals <- unique(similarity_data$similarity)
    is_categorical <- length(unique_sim_vals) <= 2 &&
        all(unique_sim_vals %in% c(0, 1))

    if (is_categorical) {
        # for categorical variables, create a bar plot

        # create a contingency table of similarity and connection status
        cont_table <- table(similarity_data$similarity, similarity_data$connected)

        # Check if table is valid
        if (nrow(cont_table) == 0 || ncol(cont_table) == 0) {
            cli::cli_abort(c(
                "x" = "Unable to create contingency table from similarity data.",
                "i" = "This might happen if all pairs have the same similarity or connection status."
            ))
        }

        # calculate proportions for each connection status
        col_sums <- colSums(cont_table)
        if (any(col_sums == 0)) {
            cli::cli_abort(c(
                "x" = "One or more connection categories have zero counts.",
                "i" = "Cannot calculate proportions when column sums are zero."
            ))
        }
        prop_table <- sweep(cont_table, 2, col_sums, "/")

        # convert the contingency table to a data frame for plotting
        prop_data <- data.frame(
            similarity = rep(as.numeric(rownames(prop_table)), ncol(prop_table)),
            connected = rep(colnames(prop_table), each = nrow(prop_table)),
            prop = as.vector(prop_table),
            stringsAsFactors = FALSE
        )

        # create the bar plot using ggplot
        p <- ggplot(prop_data, aes(x = factor(.data$similarity), y = .data$prop, fill = .data$connected)) +
            geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
            geom_text(aes(label = paste0(round(.data$prop * 100, 1), "%")),
                position = position_dodge(width = 0.7),
                vjust = -0.5, size = 3.5
            ) +
            scale_fill_manual(
                values = setNames(colors, c("Connected", "Not Connected")),
                labels = c("Connected", "Not Connected")
            ) +
            scale_y_continuous(
                labels = function(x) paste0(x * 100, "%"),
                limits = c(0, NA),
                expand = expansion(mult = c(0, 0.1))
            ) +
            scale_x_discrete(labels = c("0" = "Different", "1" = "Same")) +
            labs(
                title = paste0(attribute, " Homophily by Connection Status"),
                subtitle = paste0(
                    "Homophily correlation = ", round(stats$homophily_correlation, 3),
                    ", p-value = ", format.pval(stats$p_value, digits = 3)
                ),
                x = paste0(attribute, " Category Match"),
                y = "Proportion of Pairs",
                fill = "Connection Status"
            ) +
            theme_minimal() +
            theme(
                legend.position = "top",
                panel.grid.major.x = element_blank()
            )
    } else {
        # for continuous variables, create a density plot

        # Final check before plotting
        n_connected <- sum(similarity_data$connected == "Connected")
        n_not_connected <- sum(similarity_data$connected == "Not Connected")

        if (n_connected == 0 || n_not_connected == 0) {
            cli::cli_abort(c(
                "x" = "Cannot create distribution plot: data contains only one connection type.",
                "i" = "Connected: {n_connected}, Not Connected: {n_not_connected}"
            ))
        }

        # create the density plot using ggplot
        p <- ggplot(similarity_data, aes(x = .data$similarity, fill = .data$connected)) +
            geom_density(alpha = 0.7, adjust = 1.5) +
            geom_vline(
                xintercept = stats$mean_similarity_connected,
                color = colors[1], linetype = "dashed", linewidth = 1
            ) +
            geom_vline(
                xintercept = stats$mean_similarity_unconnected,
                color = colors[2], linetype = "dashed", linewidth = 1
            ) +
            scale_fill_manual(
                values = c("Connected" = colors[1], "Not Connected" = colors[2])
            ) +
            labs(
                title = paste0(attribute, " Similarity Distribution by Connection Status"),
                subtitle = paste0(
                    "Homophily correlation = ", round(stats$homophily_correlation, 3),
                    ", p-value = ", format.pval(stats$p_value, digits = 3)
                ),
                x = paste0(attribute, " Similarity Score"),
                y = "Density",
                fill = "Connection Status"
            ) +
            theme_minimal() +
            theme(legend.position = "top")
    }

    #
    return(p)
}

#' Create comparison plot for homophily analysis
#'
#' Internal function to create comparison plots showing homophily
#' across multiple attributes
#'
#' @param homophily_results Data frame or list of data frames
#' @param colors Character vector of colors
#' @param ... Additional arguments
#'
#' @return A ggplot2 object
#'
#' @keywords internal
#' @importFrom rlang .data
#' @noRd

plot_homophily_comparison <- function(homophily_results, colors, ...) {
    # handle list of results
    if (is.list(homophily_results) && !is.data.frame(homophily_results)) {
        # convert list to data frame efficiently
        # pre-allocate size if possible for better performance
        n_items <- length(homophily_results)
        result_list <- vector("list", n_items)

        # iterate through each item in the list
        for (i in seq_along(homophily_results)) {
            df <- homophily_results[[i]]

            # check if the current item is a data frame and has rows
            if (is.data.frame(df) && nrow(df) > 0) {
                # add the attribute name to the data frame
                df$attribute_name <- names(homophily_results)[i]

                # store the first row of the data frame in the result list
                result_list[[i]] <- df[1, ]
            }
        }

        # remove null entries from the result list
        result_list <- result_list[!sapply(result_list, is.null)]

        # combine all data frames in the list into a single data frame
        if (length(result_list) > 0) {
            comparison_data <- do.call(rbind, result_list)
        } else {
            cli::cli_abort(c(
                "x" = "No valid data frames found in the list.",
                "!" = "Ensure the list contains data frames with rows."
            ))
        }
    } else if (is.data.frame(homophily_results)) {
        # if input is already a data frame, use it directly
        comparison_data <- homophily_results

        # add attribute_name column if it doesn't exist
        if (!"attribute_name" %in% names(comparison_data)) {
            if ("attribute" %in% names(comparison_data)) {
                comparison_data$attribute_name <- comparison_data$attribute
            } else {
                # If no attribute column, use row names or index
                comparison_data$attribute_name <- rownames(comparison_data)
                if (is.null(comparison_data$attribute_name)) {
                    comparison_data$attribute_name <- paste0("Attribute_", seq_len(nrow(comparison_data)))
                }
            }
        }
    } else {
        cli::cli_abort(
            c(
                "x" = "homophily_results must be a data frame or list of data frames.",
                "!" = "Please provide valid input data."
            )
        )
    }

    # add a col to indicate stat sig - always recalculate to ensure accuracy
    # First check if there's already a 'significant' column and remove it to avoid conflicts
    if ("significant" %in% names(comparison_data)) {
        comparison_data$significant <- NULL
    }

    # Check if p_value exists, if not check for p.value (different naming conventions)
    if ("p_value" %in% names(comparison_data)) {
        comparison_data$significant <- comparison_data$p_value < 0.05
    } else if ("p.value" %in% names(comparison_data)) {
        comparison_data$significant <- comparison_data$p.value < 0.05
    } else {
        # If no p-value column found, default to FALSE
        cli::cli_alert_warning("No p-value column found in homophily results. Setting all to non-significant.")
        comparison_data$significant <- FALSE
    }

    # Create unique labels if we have layers
    if ("layer" %in% names(comparison_data) && length(unique(comparison_data$layer)) > 1) {
        comparison_data$plot_label <- paste0(comparison_data$attribute_name, " (", comparison_data$layer, ")")
    } else {
        comparison_data$plot_label <- comparison_data$attribute_name
    }

    # reorder rows by homophily correlation for better visualization
    ord <- order(comparison_data$homophily_correlation)
    comparison_data$plot_label <- factor(
        comparison_data$plot_label,
        levels = unique(comparison_data$plot_label[ord])
    )

    # make viz
    p <- ggplot(
        comparison_data,
        aes(
            x = .data$homophily_correlation,
            y = .data$plot_label
        )
    ) +

        # add horizontal segments to represent confidence intervals
        geom_segment(
            aes(
                x = .data$ci_lower, xend = .data$ci_upper,
                y = .data$plot_label, yend = .data$plot_label
            ),
            color = "gray50", linewidth = 2, alpha = 0.5
        ) +

        # add points to represent homophily correlations
        geom_point(aes(color = .data$significant), size = 4) +

        # add a vertical dashed line at x = 0 for reference
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +

        # customize the color scale for significance
        scale_color_manual(
            values = c("FALSE" = "gray60", "TRUE" = colors[1]),
            labels = c("FALSE" = "Not significant", "TRUE" = "Significant (p < 0.05)")
        ) +

        # add titles and axis labels to the plot
        labs(
            title = "Homophily Analysis: Attribute Comparison",
            subtitle = "Correlation between attribute similarity and network connections",
            x = "Homophily Correlation",
            y = "Attribute",
            color = "Statistical Significance"
        ) +

        # apply a minimal theme to the plot
        theme_minimal() +

        # customize the theme for better readability
        theme(
            panel.grid.major.y = element_blank(),
            legend.position = "bottom"
        )

    #
    return(p)
}

#' Create temporal plot for homophily analysis
#'
#' Internal function to create temporal plots showing homophily
#' evolution over time
#'
#' @param homophily_results Data frame with multiple time periods
#' @param colors Character vector of colors
#' @param ... Additional arguments
#'
#' @return A ggplot2 object
#'
#' @keywords internal
#' @importFrom rlang .data
#' @noRd
plot_homophily_temporal <- function(homophily_results, colors, ...) {
    # check if the input is a data frame, if not, throw an error
    if (!is.data.frame(homophily_results)) {
        cli::cli_abort(
            c(
                "x" = "For temporal plots, `homophily_results` must be a data frame with multiple time periods.",
                "!" = "Please provide a valid data frame."
            )
        )
    }

    # make sure there's data from multiple time periods
    if (!"net" %in% names(homophily_results) || length(unique(homophily_results$net)) < 2) {
        cli::cli_abort(
            c(
                "x" = "Temporal plots require data from multiple time periods.",
                "i" = "`homophily_results` must have a `net` column with at least two unique values."
            )
        )
    }

    # try to convert 'net' to numeric (assuming it's like years or something)
    homophily_results$time <- as.numeric(homophily_results$net)
    if (any(is.na(homophily_results$time))) {
        cli::cli_alert_info(
            "Time variable is non-numeric. Converting to a factor and then numeric. Sorting alphabetically."
        )
        homophily_results$time <- as.numeric(factor(homophily_results$net))
    }

    # set up the plot with ggplot
    p <- ggplot(
        homophily_results,
        aes(
            x = .data$time, y = .data$homophily_correlation,
            color = .data$attribute, fill = .data$attribute
        )
    ) +
        # add a ribbon for confidence intervals
        geom_ribbon(aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
            alpha = 0.2, color = NA
        ) +
        # add lines to connect points
        geom_line(linewidth = 1.2) +
        # add points for each time period
        geom_point(size = 3) +
        # add a dashed horizontal line at y=0 for reference
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        # add titles and axis labels
        labs(
            title = "Temporal Evolution of Homophily Patterns",
            subtitle = "How attribute-based connection preferences change over time",
            x = "Time Period",
            y = "Homophily Correlation",
            color = "Attribute",
            fill = "Attribute"
        ) +
        # use a clean, minimal theme
        theme_minimal() +
        # move the legend to the bottom
        theme(legend.position = "bottom")

    # if there are only 1-2 attributes, use custom colors
    n_attrs <- length(unique(homophily_results$attribute))
    if (n_attrs <= 2) {
        cli::cli_alert_info(
            "Using custom colors for {n_attrs} attributes."
        )
        p <- p +
            scale_color_manual(values = colors[1:n_attrs]) +
            scale_fill_manual(values = colors[1:n_attrs])
    }

    #
    return(p)
}

#' Extract similarity data from netify object
#'
#' Internal function to extract similarity scores and connection status
#' from a netify object for visualization
#'
#' @param netlet A netify object
#' @param attribute Character string of attribute name
#' @param method Character string of similarity method
#'
#' @return Data frame with similarity and connected columns
#'
#' @keywords internal
#' @noRd

extract_similarity_data <- function(netlet, attribute, method) {
    # grab object attributes
    obj_attrs <- attributes(netlet)
    nodal_data <- obj_attrs$nodal_data
    netify_type <- obj_attrs$netify_type
    layers <- obj_attrs$layers

    # only handle cross-sectional data for now
    if (netify_type != "cross_sec") {
        cli::cli_alert_warning("Distribution plots currently only support cross-sectional networks. Using first time period.")

        # extract first time period if it's longitudinal
        if (netify_type == "longit_array") {
            netlet <- netlet[, , 1]
            # Also extract nodal data for first time period
            if (!is.null(nodal_data) && is.list(nodal_data)) {
                time_names <- names(nodal_data)
                if (length(time_names) > 0) {
                    nodal_data <- nodal_data[[1]]
                }
            }
        } else if (netify_type == "longit_list") {
            netlet <- netlet[[1]]
            # Also extract nodal data for first time period
            if (!is.null(nodal_data) && is.list(nodal_data)) {
                time_names <- names(nodal_data)
                if (length(time_names) > 0) {
                    nodal_data <- nodal_data[[1]]
                }
            }
        }
    }

    # Handle multilayer networks - use first layer
    if (length(layers) > 1 && netify_type == "cross_sec") {
        cli::cli_alert_info("Distribution plots for multilayer networks will use the first layer: {layers[1]}")
        # Extract first layer as matrix
        net_matrix <- netlet[, , 1]
        # Ensure row/column names are preserved
        if (is.null(rownames(net_matrix))) {
            rownames(net_matrix) <- dimnames(netlet)[[1]]
            colnames(net_matrix) <- dimnames(netlet)[[2]]
        }
    } else {
        # convert the network object to a matrix
        net_matrix <- as.matrix(netlet)
    }

    # check if node attributes exist and if the specified attribute is valid
    if (is.null(nodal_data) || !attribute %in% names(nodal_data)) {
        cli::cli_alert_danger("Attribute not found in nodal data. Returning NULL.")
        return(NULL)
    }

    #
    node_attrs <- nodal_data[[attribute]]
    actors <- nodal_data$actor

    # match actors to the rows of the matrix
    matrix_actors <- rownames(net_matrix)
    if (is.null(matrix_actors)) {
        # generate default row names if none exist
        matrix_actors <- as.character(seq_len(nrow(net_matrix)))
    }

    # find indices of actors in the nodal data
    attr_indices <- match(matrix_actors, actors)
    node_attrs <- node_attrs[attr_indices]

    # filter out missing values
    complete_idx <- which(!is.na(node_attrs))
    net_matrix <- net_matrix[complete_idx, complete_idx]
    node_attrs <- node_attrs[complete_idx]

    # calculate similarity matrix (uses some external function)
    similarity_matrix <- calculate_similarity_matrix(node_attrs, method)

    # IMPORTANT: Set row/column names to match the network matrix
    # The calculate_similarity_matrix function returns a matrix without names
    rownames(similarity_matrix) <- rownames(net_matrix)
    colnames(similarity_matrix) <- colnames(net_matrix)

    # turn the network into a binary matrix
    binary_net <- (net_matrix > 0) & !is.na(net_matrix)

    # Debug: check matrix dimensions before melting
    cli::cli_alert_info("Similarity matrix: {nrow(similarity_matrix)}x{ncol(similarity_matrix)}")
    cli::cli_alert_info("Binary network: {nrow(binary_net)}x{ncol(binary_net)}")

    # extract upper triangle of similarity and binary matrices
    similarity_melted <- melt_matrix(similarity_matrix, remove_zeros = FALSE, remove_diagonal = TRUE)
    binary_melted <- melt_matrix(binary_net, remove_zeros = FALSE, remove_diagonal = TRUE)

    # Check if melting produced valid results
    if (nrow(similarity_melted) == 0 || nrow(binary_melted) == 0) {
        cli::cli_alert_danger("Matrix melting produced empty results.")
        return(NULL)
    }

    # Debug: Check column names
    if (!"Var1" %in% names(similarity_melted) || !"Var2" %in% names(similarity_melted)) {
        cli::cli_alert_danger("Similarity melted data missing Var1/Var2 columns. Columns: {paste(names(similarity_melted), collapse=', ')}")
        return(NULL)
    }
    if (!"Var1" %in% names(binary_melted) || !"Var2" %in% names(binary_melted)) {
        cli::cli_alert_danger("Binary melted data missing Var1/Var2 columns. Columns: {paste(names(binary_melted), collapse=', ')}")
        return(NULL)
    }

    # create unique keys for matching rows between melted matrices
    sim_keys <- paste(similarity_melted$Var1, similarity_melted$Var2, sep = "_")
    bin_keys <- paste(binary_melted$Var1, binary_melted$Var2, sep = "_")

    # Debug: check key lengths
    cli::cli_alert_info("Similarity keys: {length(sim_keys)}, Binary keys: {length(bin_keys)}")

    # find matching rows
    match_idx <- match(sim_keys, bin_keys)

    # Debug: check match results
    n_matched <- sum(!is.na(match_idx))
    n_unmatched <- sum(is.na(match_idx))
    cli::cli_alert_info("Matched: {n_matched}, Unmatched: {n_unmatched}")

    # check for any unmatched rows
    if (any(is.na(match_idx))) {
        cli::cli_alert_warning("Some similarity pairs could not be matched to binary network data.")
        # remove unmatched rows
        valid_idx <- !is.na(match_idx)
        similarity_melted <- similarity_melted[valid_idx, ]
        match_idx <- match_idx[valid_idx]
    }

    # Check if we have any valid matches
    if (length(match_idx) == 0 || nrow(similarity_melted) == 0) {
        cli::cli_alert_danger("No valid similarity-network pairs found.")
        return(NULL)
    }

    # Check that match_idx values are within bounds
    if (any(match_idx > nrow(binary_melted))) {
        cli::cli_alert_danger("Match indices exceed binary matrix bounds.")
        return(NULL)
    }

    # Check that binary_melted has the expected structure
    if (!"value" %in% names(binary_melted)) {
        cli::cli_alert_danger("Binary network data doesn't have expected 'value' column.")
        return(NULL)
    }

    # Extract connection values safely
    connection_values <- binary_melted$value[match_idx]

    # Check if we got valid connection values
    if (length(connection_values) == 0 || all(is.na(connection_values))) {
        cli::cli_alert_danger("No valid connection values found after matching.")
        return(NULL)
    }

    # build the final data frame
    # First create the connected labels
    connected_labels <- ifelse(connection_values == 1, "Connected", "Not Connected")

    # Check that we have both levels
    if (!("Connected" %in% connected_labels) || !("Not Connected" %in% connected_labels)) {
        cli::cli_alert_warning("Data may only contain one type of connection status.")
    }

    similarity_data <- data.frame(
        similarity = similarity_melted$value,
        connected = factor(
            connected_labels,
            levels = c("Connected", "Not Connected")
        ),
        stringsAsFactors = FALSE
    )

    # drop rows with any missing values
    complete_idx <- which(complete.cases(similarity_data))
    similarity_data <- similarity_data[complete_idx, ]

    # Final check: ensure we have some data to return
    if (nrow(similarity_data) == 0) {
        cli::cli_alert_danger("No valid similarity data remaining after filtering.")
        return(NULL)
    }

    # Ensure factor levels are properly set
    similarity_data$connected <- droplevels(similarity_data$connected)

    # One more check
    if (length(levels(similarity_data$connected)) == 0) {
        cli::cli_alert_danger("Connected factor has no levels.")
        return(NULL)
    }

    #
    return(similarity_data)
}
