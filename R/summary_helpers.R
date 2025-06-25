#' Calculate graph-level statistics for a single netlet
#'
#' `graph_stats_for_netlet` calculates various graph-level statistics for a single netlet (network slice) within a netify object. This function is designed to be used internally by the `summary.netify` function to process each netlet separately.
#'
#' @param mat A matrix representing a single netlet (network slice) from a netify object.
#' @param obj_attrs A list of attributes associated with the netify object, including information about the network type, mode, and other properties.
#' @param summary_args A list of additional arguments passed from the `summary.netify` function, which can include user-defined statistical functions.
#'
#' @return A named vector containing the calculated graph-level statistics for the given netlet. The specific statistics included depend on the network type and attributes.
#'
#' @details This function is not meant to be called directly by users but is instead used internally by the `summary.netify` function to process each netlet separately. It calculates a wide range of graph-level statistics based on the input matrix and the attributes of the netify object.
#'
#' @importFrom stats median
#'
#' @keywords internal
#' @noRd

graph_stats_for_netlet <- function(mat, obj_attrs, summary_args) {
    # Cache dimensions
    n_row <- nrow(mat)
    n_col <- ncol(mat)

    # Vectorize matrix operations early
    vec_mat <- c(mat)
    na_mask <- is.na(vec_mat)
    non_na_vec <- vec_mat[!na_mask]

    # Binary matrix and vector
    bin_mat <- mat > 0
    bin_mat[is.na(mat)] <- FALSE
    vec_bin_mat <- c(bin_mat)

    # Calculate means more efficiently
    row_means <- rowMeans(mat, na.rm = TRUE)
    col_means <- colMeans(mat, na.rm = TRUE)

    # Calculate proportion missing efficiently
    n_na <- sum(na_mask)
    prop_edges_missing <- if (obj_attrs$diag_to_NA) {
        (n_na - n_row) / length(vec_mat)
    } else {
        n_na / length(vec_mat)
    }

    # Calculate all statistics on non-NA values at once
    if (length(non_na_vec) > 0) {
        min_edge_weight <- min(non_na_vec)
        max_edge_weight <- max(non_na_vec)
        median_edge_weight <- median(non_na_vec)
        mean_edge_weight <- mean(non_na_vec)
        sd_edge_weight <- sd(non_na_vec)
    } else {
        min_edge_weight <- max_edge_weight <- median_edge_weight <- NA
        mean_edge_weight <- sd_edge_weight <- NA
    }

    # Graph level summary measures
    density <- mean(vec_bin_mat)
    num_edges <- sum(vec_bin_mat)

    # Competition measures - vectorized
    row_sums <- row_means * n_col
    col_sums <- col_means * n_row
    row_props <- row_sums / sum(row_sums)
    col_props <- col_sums / sum(col_sums)
    competition_row <- sum(row_props^2)
    competition_col <- sum(col_props^2)

    # Standard deviations
    sd_of_row_means <- sd(row_means, na.rm = TRUE)
    sd_of_col_means <- sd(col_means, na.rm = TRUE)

    # Conditional calculations based on network type
    if (obj_attrs$mode == "unipartite" && !obj_attrs$symmetric) {
        covar_of_row_col_means <- cor(row_means, col_means, use = "pairwise.complete.obs")
        # More efficient reciprocity calculation
        reciprocity <- cor(vec_mat, c(t(mat)), use = "pairwise.complete.obs")
    } else {
        covar_of_row_col_means <- reciprocity <- NA
    }

    # Create igraph object only when needed
    g <- netify_to_igraph(mat)
    transitivity <- igraph::transitivity(g)

    # Build output vector
    out <- c(
        num_row_actors = n_row,
        num_col_actors = n_col,
        density = density,
        num_edges = num_edges,
        prop_edges_missing = prop_edges_missing,
        mean_edge_weight = mean_edge_weight,
        sd_edge_weight = sd_edge_weight,
        median_edge_weight = median_edge_weight,
        min_edge_weight = min_edge_weight,
        max_edge_weight = max_edge_weight,
        competition_row = competition_row,
        competition_col = competition_col,
        sd_of_row_means = sd_of_row_means,
        sd_of_col_means = sd_of_col_means,
        covar_of_row_col_means = covar_of_row_col_means,
        reciprocity = reciprocity,
        transitivity = transitivity
    )

    # Calculate user-supplied stats if present
    if (!is.null(summary_args$other_stats)) {
        # Use lapply to handle functions that return multiple values
        other_results <- lapply(summary_args$other_stats, function(stat) stat(mat))

        # Build the output vector with proper names
        other_out <- numeric(0)
        out_names <- character(0)

        for (i in seq_along(other_results)) {
            func_name <- names(summary_args$other_stats)[i]
            result <- other_results[[i]]
            result_length <- length(result)

            # Add values to output
            other_out <- c(other_out, result)

            # Handle naming based on whether result has names and length
            if (!is.null(names(result))) {
                # Result has names - use them
                if (length(summary_args$other_stats) > 1) {
                    # Multiple functions - prefix with function name
                    out_names <- c(out_names, paste0(func_name, ".", names(result)))
                } else {
                    # Single function - use names as is
                    out_names <- c(out_names, names(result))
                }
            } else {
                # Result has no names
                if (result_length == 1) {
                    # Single value - use function name
                    out_names <- c(out_names, func_name)
                } else {
                    # Multiple values - append indices
                    out_names <- c(out_names, paste0(func_name, "_", seq_len(result_length)))
                }
            }
        }

        names(other_out) <- out_names
        out <- c(out, other_out)
    }

    return(out)
}

#' Calculate actor-level statistics for a single netlet
#'
#' `actor_stats_for_netlet` calculates various actor-level statistics for a single netlet (network slice) within a netify object. This function is designed to be used internally by the `summary_actor` function to process each netlet separately.
#'
#' @param mat A matrix representing a single netlet (network slice) from a netify object.
#' @param obj_attrs A list of attributes associated with the netify object, including information about the network type, mode, and other properties.
#' @param invert_weights_for_igraph Logical; if TRUE, the weights of the edges are inverted before being used in the calculation of closeness or betweenness centrality. This is because igraph treats edge weights as distances. Inverting weights can be crucial when higher weights should imply stronger (or more valuable) connections rather than longer distances. Default is TRUE.
#' @param other_stats A named list of functions that take a matrix and return additional actor-level statistics to be included in the output. Each function should accept a matrix as input and return a vector or single value per actor. This allows for the inclusion of custom metrics in the summary output.
#'
#' @return A data frame containing the calculated actor-level statistics for the given netlet. The specific statistics included depend on the network type and attributes.
#'
#' @details This function is not meant to be called directly by users but is instead used internally by the `summary_actor` function to process each netlet separately. It calculates a wide range of actor-level statistics based on the input matrix and the attributes of the netify object.
#'
#' @keywords internal
#' @noRd

actor_stats_for_netlet <- function(mat, obj_attrs, invert_weights_for_igraph = TRUE, other_stats = NULL) {
    # Cache frequently used values
    is_symmetric <- obj_attrs$symmetric
    is_weighted <- !obj_attrs$weight_binary
    is_bipartite <- obj_attrs$mode == "bipartite"

    # Pre-calculate binary matrix for all cases
    bin_mat <- mat > 0
    bin_mat[is.na(mat)] <- FALSE

    # Get igraph object and process weights only once
    g <- netify_to_igraph(mat)

    # Process weights efficiently
    g_wgts <- NULL
    if (is_weighted) {
        g_wgts <- igraph::E(g)$weight
        if (length(g_wgts) > 0) {
            # Handle negative weights
            if (any(g_wgts < 0)) {
                min_wgt <- min(g_wgts)
                g_wgts <- g_wgts - min_wgt + 1
            }
            # Invert if necessary
            if (invert_weights_for_igraph) {
                g_wgts <- 1 / g_wgts
                g_wgts[is.infinite(g_wgts)] <- 0
            }
        }
    }

    # Calculate statistics based on network type
    if (is_symmetric) {
        # Common calculations for symmetric networks
        degree <- rowSums(bin_mat, na.rm = TRUE)
        prop_ties <- rowMeans(bin_mat, na.rm = TRUE)

        # Network share and igraph stats
        network_share <- degree / sum(degree, na.rm = TRUE)
        closeness <- igraph::closeness(g, normalized = TRUE, weights = g_wgts)
        betweenness <- igraph::betweenness(g, normalized = TRUE, weights = g_wgts)
        eigen_vector <- igraph::eigen_centrality(g)$vector

        if (is_weighted) {
            # Additional weighted statistics
            strength_sum <- rowSums(mat, na.rm = TRUE)
            strength_avg <- rowMeans(mat, na.rm = TRUE)
            strength_std <- apply(mat, 1, sd, na.rm = TRUE)
            strength_median <- apply(mat, 1, median, na.rm = TRUE)
            network_share <- strength_sum / sum(strength_sum, na.rm = TRUE)

            out <- data.frame(
                degree = degree,
                prop_ties = prop_ties,
                strength_sum = strength_sum,
                strength_avg = strength_avg,
                strength_std = strength_std,
                strength_median = strength_median,
                network_share = network_share,
                closeness = closeness,
                betweenness = betweenness,
                eigen_vector = eigen_vector
            )
        } else {
            out <- data.frame(
                degree = degree,
                prop_ties = prop_ties,
                network_share = network_share,
                closeness = closeness,
                betweenness = betweenness,
                eigen_vector = eigen_vector
            )
        }
    } else {
        # Asymmetric networks - calculate base statistics
        degree_out <- rowSums(bin_mat, na.rm = TRUE)
        degree_in <- colSums(bin_mat, na.rm = TRUE)
        prop_ties_out <- rowMeans(bin_mat, na.rm = TRUE)
        prop_ties_in <- colMeans(bin_mat, na.rm = TRUE)

        # Calculate totals based on bipartite status
        if (!is_bipartite) {
            degree_total <- degree_in + degree_out
            prop_ties_total <- sum(degree_total > 0, na.rm = TRUE) / length(degree_total)
        } else {
            degree_total <- c(degree_in, degree_out)
            prop_ties_total <- c(prop_ties_in, prop_ties_out)
        }

        # igraph centrality measures
        closeness_in <- igraph::closeness(g, mode = "in", normalized = TRUE, weights = g_wgts)
        closeness_out <- igraph::closeness(g, mode = "out", normalized = TRUE, weights = g_wgts)
        closeness_all <- igraph::closeness(g, mode = "all", normalized = TRUE, weights = g_wgts)
        betweenness <- igraph::betweenness(g, normalized = TRUE, weights = g_wgts)
        authority_score <- igraph::hits_scores(g)$authority
        hub_score <- igraph::hits_scores(g)$hub
        # authority_score <- igraph::authority_score(g)$vector
        # hub_score <- igraph::hub_score(g)$vector

        if (is_weighted) {
            # Additional weighted statistics for asymmetric networks
            strength_sum_out <- rowSums(mat, na.rm = TRUE)
            strength_sum_in <- colSums(mat, na.rm = TRUE)
            strength_avg_out <- rowMeans(mat, na.rm = TRUE)
            strength_avg_in <- colMeans(mat, na.rm = TRUE)
            strength_median_out <- apply(mat, 1, median, na.rm = TRUE)
            strength_median_in <- apply(mat, 2, median, na.rm = TRUE)
            strength_std_out <- apply(mat, 1, sd, na.rm = TRUE)
            strength_std_in <- apply(mat, 2, sd, na.rm = TRUE)

            # Calculate totals and network shares
            if (!is_bipartite) {
                strength_sum_total <- strength_sum_in + strength_sum_out
                strength_avg_total <- (strength_avg_in + strength_avg_out) / 2
                strength_median_total <- (strength_median_in + strength_median_out) / 2
                strength_std_total <- (strength_std_in + strength_std_out) / 2
            } else {
                strength_sum_total <- c(strength_sum_in, strength_sum_out)
                strength_avg_total <- c(strength_avg_in, strength_avg_out)
                strength_median_total <- c(strength_median_in, strength_median_out)
                strength_std_total <- c(strength_std_in, strength_std_out)
            }

            network_share_in <- strength_sum_in / sum(strength_sum_in, na.rm = TRUE)
            network_share_out <- strength_sum_out / sum(strength_sum_out, na.rm = TRUE)
            network_share_total <- strength_sum_total / sum(strength_sum_total, na.rm = TRUE)
        } else {
            # Binary network shares
            network_share_in <- degree_in / sum(degree_in, na.rm = TRUE)
            network_share_out <- degree_out / sum(degree_out, na.rm = TRUE)
            network_share_total <- degree_total / sum(degree_total, na.rm = TRUE)
        }

        # Build output data frame based on type
        if (is_bipartite) {
            # Bipartite output
            base_cols <- data.frame(
                degree_total = degree_total,
                prop_ties_total = prop_ties_total,
                network_share_total = network_share_total,
                closeness_all = closeness_all,
                betweenness = betweenness,
                authority_score = authority_score,
                hub_score = hub_score
            )

            if (is_weighted) {
                out <- cbind(base_cols,
                    strength_sum_total = strength_sum_total,
                    strength_avg_total = strength_avg_total,
                    strength_std_total = strength_std_total,
                    strength_median_total = strength_median_total
                )
            } else {
                out <- base_cols
            }
        } else {
            # Non-bipartite output
            base_cols <- data.frame(
                degree_in = degree_in,
                degree_out = degree_out,
                degree_total = degree_total,
                prop_ties_in = prop_ties_in,
                prop_ties_out = prop_ties_out,
                prop_ties_total = prop_ties_total,
                network_share_in = network_share_in,
                network_share_out = network_share_out,
                network_share_total = network_share_total,
                closeness_in = closeness_in,
                closeness_out = closeness_out,
                closeness_all = closeness_all,
                betweenness = betweenness,
                authority_score = authority_score,
                hub_score = hub_score
            )

            if (is_weighted) {
                out <- cbind(base_cols,
                    strength_sum_in = strength_sum_in,
                    strength_sum_out = strength_sum_out,
                    strength_sum_total = strength_sum_total,
                    strength_avg_in = strength_avg_in,
                    strength_avg_out = strength_avg_out,
                    strength_avg_total = strength_avg_total,
                    strength_std_in = strength_std_in,
                    strength_std_out = strength_std_out,
                    strength_std_total = strength_std_total,
                    strength_median_in = strength_median_in,
                    strength_median_out = strength_median_out,
                    strength_median_total = strength_median_total
                )
            } else {
                out <- base_cols
            }
        }
    }

    # Add user-supplied stats efficiently
    if (!is.null(other_stats)) {
        other_results <- lapply(other_stats, function(stat) stat(mat))
        out <- cbind(out, as.data.frame(other_results))
    }

    return(out)
}
