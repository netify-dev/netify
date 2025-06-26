#' Create attribute mixing matrices for network data
#'
#' Creates cross-tabulation matrices showing how connections are distributed across
#' different attribute values. This reveals mixing patterns and assortativity in
#' networks by examining the frequency of ties between actors with different
#' attribute combinations.
#'
#' @param netlet A netify object containing network data.
#' @param attribute Character string specifying the nodal attribute to analyze.
#' @param row_attribute Optional different attribute for matrix rows. If NULL, uses
#'   the same attribute for both dimensions.
#' @param normalized Logical. Whether to return proportions instead of raw counts.
#'   Default TRUE.
#' @param by_row Logical. If TRUE and normalized=TRUE, normalizes by row. Default FALSE.
#' @param include_weights Logical. Whether to use edge weights. Default FALSE.
#' @param other_stats Named list of custom functions for additional statistics.
#' @param ... Additional arguments passed to custom functions.
#'
#' @return List containing:
#'   \describe{
#'     \item{\code{mixing_matrices}}{Named list of mixing matrices per time/layer}
#'     \item{\code{summary_stats}}{Data frame with mixing statistics:}
#'     \describe{
#'       \item{\code{net}}{Network/time identifier}
#'       \item{\code{layer}}{Layer name}
#'       \item{\code{attribute}}{Name of analyzed attribute(s)}
#'       \item{\code{assortativity}}{Assortativity coefficient (-1 to 1)}
#'       \item{\code{diagonal_proportion}}{Proportion of within-group ties}
#'       \item{\code{entropy}}{Shannon entropy of mixing pattern}
#'       \item{\code{modularity}}{Modularity based on attribute groups}
#'       \item{\code{n_groups}}{Number of attribute categories}
#'       \item{\code{total_ties}}{Total number of ties analyzed}
#'     }
#'   }
#'
#' @details
#' Mixing matrix elements represent ties between actors with attribute
#' values i and j. For undirected networks, matrices are symmetrized.
#' Assortativity ranges from -1 (disassortative) to 1 (assortative).
#' 
#' @author Casy Dorff, Shahryar Minhas
#'
#' @export mixing_matrix

mixing_matrix <- function(
    netlet,
    attribute,
    row_attribute = NULL,
    normalized = TRUE,
    by_row = FALSE,
    include_weights = FALSE,
    other_stats = NULL,
    ...
    ){
    
    # input validation
    netify_check(netlet)
    checkmate::assert_string(attribute)
    checkmate::assert_string(row_attribute, null.ok = TRUE)
    checkmate::assert_logical(normalized, len = 1)
    checkmate::assert_logical(by_row, len = 1)
    checkmate::assert_logical(include_weights, len = 1)

    # extract object attributes
    obj_attrs <- attributes(netlet)
    layers <- obj_attrs$layers
    nodal_data <- obj_attrs$nodal_data
    netify_type <- obj_attrs$netify_type

    # check if attributes exist in nodal data
    if (is.null(nodal_data) || !attribute %in% names(nodal_data)) {
        cli::cli_abort("Attribute '{attribute}' not found in nodal_data. Available attributes: {paste(names(nodal_data), collapse = ', ')}")
    }

    if (!is.null(row_attribute) && !row_attribute %in% names(nodal_data)) {
        cli::cli_abort("Row attribute '{row_attribute}' not found in nodal_data. Available attributes: {paste(names(nodal_data), collapse = ', ')}")
    }

    # use same attribute for rows if not specified
    if (is.null(row_attribute)) {
        row_attribute <- attribute
    }

    # initialize results storage
    mixing_matrices <- list()
    summary_data <- list()

    # process each layer
    for (layer in layers) {
        # convert to list format for processing
        netlet_list <- switch(netify_type,
            "cross_sec" = list("1" = netlet),
            "longit_array" = {
                # extract time periods from array
                time_names <- dimnames(netlet)[[3]]
                if (is.null(time_names)) {
                    time_names <- as.character(seq_len(dim(netlet)[3]))
                }
                net_list <- list()
                for (t in seq_along(time_names)) {
                    net_list[[time_names[t]]] <- netlet[,,t]
                }
                net_list
            },
            "longit_list" = netlet
        )

        # process each time period
        for (time_id in names(netlet_list)) {
            # get network matrix for this time period
            net_matrix <- netlet_list[[time_id]]
            if (netify_type == "longit_array" && length(dim(netlet)) == 4) {
                net_matrix <- net_matrix[, , layer]
            }

            # get nodal attributes for this time period
            if (netify_type == "cross_sec") {
                col_attrs <- nodal_data[[attribute]]
                row_attrs <- nodal_data[[row_attribute]]
                actors <- nodal_data$actor
            } else {
                time_data <- nodal_data[nodal_data$time == time_id, ]
                col_attrs <- time_data[[attribute]]
                row_attrs <- time_data[[row_attribute]]
                actors <- time_data$actor
            }

            # match actors to matrix rows/columns
            matrix_actors <- rownames(net_matrix)
            if (is.null(matrix_actors)) {
                matrix_actors <- as.character(seq_len(nrow(net_matrix)))
            }

            # subset attributes to match matrix
            attr_indices <- match(matrix_actors, actors)
            if (any(is.na(attr_indices))) {
                cli::cli_warn("Some actors in network matrix not found in nodal data for time {time_id}")
            }
            col_attrs <- col_attrs[attr_indices]
            row_attrs <- row_attrs[attr_indices]

            # remove actors with missing attributes
            complete_cases <- !is.na(col_attrs) & !is.na(row_attrs)
            if (sum(complete_cases) < 2) {
                cli::cli_warn("Insufficient non-missing attribute data for time {time_id}")
                next
            }

            net_matrix <- net_matrix[complete_cases, complete_cases]
            col_attrs <- col_attrs[complete_cases]
            row_attrs <- row_attrs[complete_cases]

            # create mixing matrix
            mixing_result <- create_mixing_matrix(
                net_matrix, row_attrs, col_attrs,
                normalized, by_row, include_weights
            )

            # calculate summary statistics
            summary_stats <- calculate_mixing_stats(
                mixing_result$matrix, mixing_result$raw_matrix,
                row_attrs, col_attrs
            )

            # add custom statistics if provided
            if (!is.null(other_stats)) {
                custom_stats <- lapply(other_stats, function(f) {
                    tryCatch(f(mixing_result$matrix, ...),
                        error = function(e) {
                            cli::cli_warn("Error in custom statistic: {e$message}")
                            NA
                        }
                    )
                })
                summary_stats <- c(summary_stats, unlist(custom_stats))
            }

            # store results
            matrix_key <- paste(time_id, layer, sep = "_")
            mixing_matrices[[matrix_key]] <- mixing_result$matrix

            summary_data[[matrix_key]] <- data.frame(
                net = time_id,
                layer = layer,
                attribute = ifelse(attribute == row_attribute, attribute,
                    paste(row_attribute, "x", attribute)
                ),
                summary_stats,
                stringsAsFactors = FALSE
            )
        }
    }

    # combine summary statistics
    summary_df <- do.call(rbind, summary_data)
    rownames(summary_df) <- NULL

    return(list(
        mixing_matrices = mixing_matrices,
        summary_stats = summary_df
    ))
}

# helper function to create mixing matrix
create_mixing_matrix <- function(net_matrix, row_attrs, col_attrs,
                                 normalized, by_row, include_weights) {
    # get unique attribute values
    row_levels <- sort(unique(row_attrs))
    col_levels <- sort(unique(col_attrs))

    # initialize mixing matrix
    mixing_matrix <- matrix(0,
        nrow = length(row_levels),
        ncol = length(col_levels),
        dimnames = list(row_levels, col_levels)
    )

    # fill mixing matrix
    n <- length(row_attrs)
    for (i in seq_len(n)) {
        for (j in seq_len(n)) {
            if (i != j || !is.na(net_matrix[i, j])) { # Allow self-loops if they exist
                tie_value <- net_matrix[i, j]

                if (!is.na(tie_value) && (include_weights || tie_value > 0)) {
                    row_level <- as.character(row_attrs[i])
                    col_level <- as.character(col_attrs[j])

                    weight <- ifelse(include_weights, tie_value, 1)
                    mixing_matrix[row_level, col_level] <- mixing_matrix[row_level, col_level] + weight
                }
            }
        }
    }

    # store raw matrix before normalization
    raw_matrix <- mixing_matrix

    # for undirected networks, symmetrize the matrix
    is_symmetric <- isSymmetric(net_matrix, tol = .Machine$double.eps^0.5)
    if (is_symmetric && identical(row_attrs, col_attrs)) {
        mixing_matrix <- (mixing_matrix + t(mixing_matrix)) / 2
    }

    # normalize if requested
    if (normalized) {
        total_sum <- sum(mixing_matrix)
        if (total_sum > 0) {
            if (by_row) {
                # normalize by row
                row_sums <- rowSums(mixing_matrix)
                for (i in seq_len(nrow(mixing_matrix))) {
                    if (row_sums[i] > 0) {
                        mixing_matrix[i, ] <- mixing_matrix[i, ] / row_sums[i]
                    }
                }
            } else {
                # normalize by total
                mixing_matrix <- mixing_matrix / total_sum
            }
        }
    }

    return(list(matrix = mixing_matrix, raw_matrix = raw_matrix))
}

# helper function to calculate mixing statistics
calculate_mixing_stats <- function(mixing_matrix, raw_matrix, row_attrs, col_attrs) {
    # basic counts
    n_row_groups <- length(unique(row_attrs))
    n_col_groups <- length(unique(col_attrs))
    total_ties <- sum(raw_matrix)

    # for square matrices (same attribute), calculate additional stats
    if (identical(row_attrs, col_attrs) && nrow(mixing_matrix) == ncol(mixing_matrix)) {
        # assortativity coefficient
        assortativity <- calculate_assortativity(raw_matrix)

        # diagonal proportion (within-group ties)
        diagonal_sum <- sum(diag(raw_matrix))
        diagonal_proportion <- ifelse(total_ties > 0, diagonal_sum / total_ties, 0)

        # modularity
        modularity <- calculate_modularity(raw_matrix)

        n_groups <- n_row_groups
    } else {
        # for non-square matrices
        assortativity <- NA
        diagonal_proportion <- NA
        modularity <- NA
        n_groups <- max(n_row_groups, n_col_groups)
    }

    # shannon entropy of mixing pattern
    probs <- as.vector(mixing_matrix)
    probs <- probs[probs > 0] # Remove zeros for log calculation
    entropy <- ifelse(length(probs) > 1, -sum(probs * log(probs)), 0)

    return(list(
        assortativity = assortativity,
        diagonal_proportion = diagonal_proportion,
        entropy = entropy,
        modularity = modularity,
        n_groups = n_groups,
        total_ties = total_ties
    ))
}

# helper function to calculate assortativity coefficient
calculate_assortativity <- function(mixing_matrix) {
    if (nrow(mixing_matrix) != ncol(mixing_matrix)) {
        return(NA)
    }

    # normalize to get probabilities
    total <- sum(mixing_matrix)
    if (total == 0) {
        return(NA)
    }

    e <- mixing_matrix / total # Joint probabilities
    a <- rowSums(e) # Marginal probabilities (rows)
    b <- colSums(e) # Marginal probabilities (columns)

    # assortativity coefficient: (trace - sum(a*b)) / (1 - sum(a*b))
    trace_e <- sum(diag(e))
    sum_ab <- sum(a * b)

    if (sum_ab >= 1) {
        return(NA) # Undefined
    }

    assortativity <- (trace_e - sum_ab) / (1 - sum_ab)
    return(assortativity)
}

# helper function to calculate modularity
calculate_modularity <- function(mixing_matrix) {
    if (nrow(mixing_matrix) != ncol(mixing_matrix)) {
        return(NA)
    }

    total <- sum(mixing_matrix)
    if (total == 0) {
        return(NA)
    }

    # modularity calculation
    k_in <- diag(mixing_matrix) # Within-group edges
    k_out <- rowSums(mixing_matrix) - k_in # Between-group edges from group
    k_in_total <- colSums(mixing_matrix) - k_in # Between-group edges to group

    expected_within <- (k_out + k_in) * (k_in_total + k_in) / total

    modularity <- sum((k_in - expected_within) / total)
    return(modularity)
}
