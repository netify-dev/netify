#' Analyze homophily in network data
#'
#' Tests whether connected actors have similar attributes (homophily). Calculates
#' the correlation between attribute similarity and tie presence, with support for
#' multiple similarity metrics and significance testing.
#'
#' @param netlet A netify object containing network data.
#' @param attribute Character string specifying the nodal attribute to analyze.
#' @param method Character string specifying the similarity metric:
#'   \describe{
#'     \item{"correlation"}{Negative absolute difference for continuous data (default)}
#'     \item{"euclidean"}{Negative euclidean distance for continuous data}
#'     \item{"manhattan"}{Negative Manhattan/city-block distance for continuous data}
#'     \item{"cosine"}{Cosine similarity for continuous data}
#'     \item{"categorical"}{Binary similarity (0/1) for categorical data}
#'     \item{"jaccard"}{Jaccard similarity for binary/presence-absence data}
#'     \item{"hamming"}{Negative Hamming distance for categorical data}
#'   }
#' @param threshold Numeric value or function to determine tie presence in weighted networks.
#'   If numeric, edges with weights > threshold are considered ties. If a function,
#'   it should take the network matrix and return a logical matrix. Default is 0
#'   (any positive weight is a tie). Common values: 0 (default), mean(weights),
#'   median(weights), or quantile-based thresholds. For pre-binarized networks,
#'   consider using \code{mutate_weights()} first.
#' @param significance_test Logical. Whether to perform permutation test. Default TRUE.
#' @param n_permutations Number of permutations for significance testing. Default 1000.
#' @param alpha Significance level for confidence intervals. Default 0.05.
#' @param other_stats Named list of custom functions for additional statistics.
#' @param ... Additional arguments passed to custom functions.
#'
#' @return Data frame with homophily statistics per network/time period:
#'   \describe{
#'     \item{\code{net}}{Network/time identifier}
#'     \item{\code{layer}}{Layer name}
#'     \item{\code{attribute}}{Analyzed attribute name}
#'     \item{\code{method}}{Similarity method used}
#'     \item{\code{threshold_value}}{Threshold used for determining ties (NA for binary networks)}
#'     \item{\code{homophily_correlation}}{Correlation between similarity and tie presence (binary: tie/no tie)}
#'     \item{\code{mean_similarity_connected}}{Mean similarity among connected pairs (weight > threshold)}
#'     \item{\code{mean_similarity_unconnected}}{Mean similarity among unconnected pairs (weight <= threshold or missing)}
#'     \item{\code{similarity_difference}}{Difference between connected and unconnected mean similarities}
#'     \item{\code{p_value}}{Permutation test p-value}
#'     \item{\code{ci_lower}, \code{ci_upper}}{Confidence interval bounds}
#'     \item{\code{n_connected_pairs}}{Number of connected pairs}
#'     \item{\code{n_unconnected_pairs}}{Number of unconnected pairs}
#'   }
#'
#' @details
#' \strong{Similarity Metrics:}
#'
#' For continuous attributes:
#' \itemize{
#'   \item \code{correlation}: Based on absolute difference, good general purpose metric
#'   \item \code{euclidean}: Similar to correlation for single attributes
#'   \item \code{manhattan}: Less sensitive to outliers than euclidean
#'   \item \code{cosine}: Useful for normalized data or when sign matters
#' }
#'
#' For categorical/binary attributes:
#' \itemize{
#'   \item \code{categorical}: Simple matching (1 if same, 0 if different)
#'   \item \code{jaccard}: For binary data, emphasizes shared presence over shared absence
#'   \item \code{hamming}: Counts positions where values differ (negated for similarity)
#' }
#'
#' \strong{Threshold Parameter:}
#'
#' For weighted networks, the \code{threshold} parameter determines what edge weights
#' constitute a "connection". You can specify:
#' \itemize{
#'   \item A numeric value: edges with weight > threshold are ties
#'   \item A function: should take a matrix and return a single numeric threshold
#'   \item Common threshold functions:
#'     \itemize{
#'       \item \code{function(x) mean(x, na.rm = TRUE)} - mean weight
#'       \item \code{function(x) median(x, na.rm = TRUE)} - median weight
#'       \item \code{function(x) quantile(x, 0.75, na.rm = TRUE)} - 75th percentile
#'     }
#' }
#'
#' For more complex binarization needs (e.g., different thresholds by time period),
#' consider using \code{mutate_weights()} to pre-process your network.
#'
#' @examples
#' \dontrun{
#' # Basic homophily analysis with default threshold (> 0)
#' homophily_default <- homophily(net, attribute = "group")
#'
#' # Using different similarity metrics for continuous data
#' homophily_manhattan <- homophily(
#'     net,
#'     attribute = "age",
#'     method = "manhattan" # Less sensitive to outliers
#' )
#'
#' # For binary attributes (e.g., gender, membership)
#' homophily_jaccard <- homophily(
#'     net,
#'     attribute = "member",
#'     method = "jaccard" # Better for binary data than correlation
#' )
#'
#' # For categorical attributes
#' homophily_categorical <- homophily(
#'     net,
#'     attribute = "department",
#'     method = "categorical"
#' )
#'
#' # Combining method and threshold
#' homophily_combined <- homophily(
#'     net,
#'     attribute = "score",
#'     method = "manhattan",
#'     threshold = function(x) quantile(x, 0.75, na.rm = TRUE)
#' )
#' }
#'
#' @export homophily

homophily <- function(
    netlet, attribute,
    method = "correlation",
    threshold = 0,
    significance_test = TRUE,
    n_permutations = 1000,
    alpha = 0.05,
    other_stats = NULL,
    ...){

    # input validation
    netify_check(netlet)
    checkmate::assert_string(attribute)
    checkmate::assert_choice(method, c(
        "correlation", "euclidean", "manhattan",
        "cosine", "categorical", "jaccard", "hamming"
    ))
    if (!is.numeric(threshold) && !is.function(threshold)) {
        cli::cli_abort("threshold must be numeric or a function that returns a numeric value")
    }
    checkmate::assert_logical(significance_test, len = 1)
    checkmate::assert_count(n_permutations, positive = TRUE)
    checkmate::assert_number(alpha, lower = 0, upper = 1)

    # extract object attributes
    obj_attrs <- attributes(netlet)
    layers <- obj_attrs$layers
    nodal_data <- obj_attrs$nodal_data
    netify_type <- obj_attrs$netify_type

    # handle is_binary which might be a vector for multilayer networks
    is_binary <- obj_attrs$is_binary %||% FALSE
    if (length(is_binary) > 1) {
        # for multilayer, take the first value or check if all are the same
        is_binary_network <- is_binary[1]
    } else {
        is_binary_network <- is_binary
    }

    # check if attribute exists in nodal data
    if (is.null(nodal_data)) {
        cli::cli_abort("No nodal_data found in netify object.")
    }

    # check attr availability depending on data struct
    if (netify_type == "cross_sec") {
        if (!attribute %in% names(nodal_data)) {
            cli::cli_abort(
                "Attribute '{attribute}' not found in nodal_data. Available attributes: {paste(names(nodal_data), collapse = ', ')}"
            )
        }
    } else {
        # for longit data, nodal_data can be either a list or a data.frame
        if (is.data.frame(nodal_data)) {
            # deprc format with time column
            if (!attribute %in% names(nodal_data)) {
                cli::cli_abort(
                    "Attribute '{attribute}' not found in nodal_data. Available attributes: {paste(setdiff(names(nodal_data), c('actor', 'time')), collapse = ', ')}"
                )
            }
        } else if (is.list(nodal_data)) {
            # list format - check in the first time period
            first_time <- names(nodal_data)[1]
            if (!is.null(first_time) && !attribute %in% names(nodal_data[[first_time]])) {
                cli::cli_abort(
                    "Attribute '{attribute}' not found in nodal_data. Available attributes: {paste(names(nodal_data[[first_time]]), collapse = ', ')}"
                )
            }
        }
    }

    # process each layer
    results <- lapply(seq_along(layers), function(layer_index) {
        layer <- layers[layer_index]
        # convert to list format for processing
        netlet_list <- switch(netify_type,
            "cross_sec" = list("1" = netlet),
            "longit_array" = {
                # check if this is multilayer longitudinal (4D) or single layer (3D)
                if (length(dim(netlet)) == 4) {
                    # multilayer longit: extract time periods from 4th dimension
                    time_names <- dimnames(netlet)[[4]]
                    if (is.null(time_names)) {
                        time_names <- as.character(seq_len(dim(netlet)[4]))
                    }
                    net_list <- list()
                    for (t in seq_along(time_names)) {
                        net_list[[time_names[t]]] <- netlet[, , , t]
                    }
                } else {
                    # single layer longit: extract from 3rd dimension
                    time_names <- dimnames(netlet)[[3]]
                    if (is.null(time_names)) {
                        time_names <- as.character(seq_len(dim(netlet)[3]))
                    }
                    net_list <- list()
                    for (t in seq_along(time_names)) {
                        net_list[[time_names[t]]] <- netlet[, , t]
                    }
                }
                net_list
            },
            "longit_list" = netlet
        )

        # calculate homophily for each time period
        time_results <- lapply(names(netlet_list), function(time_id) {
            # get network matrix for this time period
            net_matrix <- netlet_list[[time_id]]
            # extract specific layer for multilayer networks
            if (length(layers) > 1) {
                if (netify_type == "cross_sec") {
                    # for cross-sec multilayer: 3D array [actors, actors, layers]
                    net_matrix <- netlet[, , layer_index]
                } else if (netify_type == "longit_array" && length(dim(netlet)) == 4) {
                    # for longit multilayer: 4D array [actors, actors, layers, time]
                    net_matrix <- net_matrix[, , layer_index]
                }
            }

            # get nodal attributes for this time period
            if (netify_type == "cross_sec") {
                node_attrs <- nodal_data[[attribute]]
                actors <- nodal_data$actor
            } else {
                # longit data, nodal_data is a list by time period
                if (is.list(nodal_data) && time_id %in% names(nodal_data)) {
                    time_data <- nodal_data[[time_id]]
                    node_attrs <- time_data[[attribute]]
                    actors <- time_data$actor
                } else if (is.data.frame(nodal_data)) {
                    # deprc format with time column
                    time_data <- nodal_data[nodal_data$time == time_id, ]
                    node_attrs <- time_data[[attribute]]
                    actors <- time_data$actor
                } else {
                    cli::cli_warn("Could not extract nodal data for time {time_id}")
                    return(NULL)
                }
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
            node_attrs <- node_attrs[attr_indices]

            # track missing values
            n_missing_attrs <- sum(is.na(node_attrs))
            n_total_actors <- length(node_attrs)

            # remove actors with missing attributes
            complete_cases <- !is.na(node_attrs)
            if (sum(complete_cases) < 2) {
                cli::cli_warn("Insufficient non-missing attribute data for time {time_id}")
                return(NULL)
            }

            net_matrix <- net_matrix[complete_cases, complete_cases]
            node_attrs <- node_attrs[complete_cases]

            # calculate similarity matrix
            similarity_matrix <- calculate_similarity_matrix(node_attrs, method)

            # determine threshold value for this time period/layer
            # for multilayer networks, check is_binary for this specific layer
            if (length(is_binary) > 1) {
                layer_is_binary <- is_binary[layer_index]
            } else {
                layer_is_binary <- is_binary
            }

            if (layer_is_binary) {
                # for binary networks, threshold is not applicable
                threshold_value <- NA
                binary_net <- net_matrix
            } else {
                # for weighted networks, apply threshold
                if (is.function(threshold)) {
                    # calculate threshold from the network matrix
                    threshold_value <- threshold(net_matrix)
                    if (!is.numeric(threshold_value) || length(threshold_value) != 1) {
                        cli::cli_abort("threshold function must return a single numeric value")
                    }
                } else {
                    threshold_value <- threshold
                }

                # create binary network based on threshold
                binary_net <- (net_matrix > threshold_value) & !is.na(net_matrix)
            }

            # calculate homophily statistics with the binary network
            homophily_stats <- calculate_homophily_stats(
                similarity_matrix, binary_net, significance_test,
                n_permutations, alpha
            )

            # add custom statistics if provided
            if (!is.null(other_stats)) {
                custom_stats <- lapply(other_stats, function(f) {
                    tryCatch(f(similarity_matrix, binary_net, ...),
                        error = function(e) {
                            cli::cli_warn("Error in custom statistic: {e$message}")
                            NA
                        }
                    )
                })
                homophily_stats <- c(homophily_stats, unlist(custom_stats))
            }

            # format results
            result_df <- data.frame(
                net = time_id,
                layer = layer,
                attribute = attribute,
                method = method,
                threshold_value = threshold_value,
                homophily_stats,
                stringsAsFactors = FALSE
            )

            # add missing data info
            result_df$n_missing <- n_missing_attrs
            result_df$n_pairs <- n_total_actors * (n_total_actors - 1) / 2

            result_df
        })

        # combine results for this layer
        valid_results <- time_results[!sapply(time_results, is.null)]
        if (length(valid_results) > 0) {
            do.call(rbind, valid_results)
        } else {
            NULL
        }
    })

    # combine results across layers
    # filter out NULL results
    results <- results[!sapply(results, is.null)]

    if (length(results) == 0) {
        cli::cli_warn("No valid results obtained from homophily analysis")
        # return empty data frame with expected structure instead of NULL
        return(data.frame(
            net = character(0),
            layer = character(0),
            attribute = character(0),
            method = character(0),
            threshold_value = numeric(0),
            homophily_correlation = numeric(0),
            mean_similarity_connected = numeric(0),
            mean_similarity_unconnected = numeric(0),
            similarity_difference = numeric(0),
            p_value = numeric(0),
            ci_lower = numeric(0),
            ci_upper = numeric(0),
            n_connected_pairs = integer(0),
            n_unconnected_pairs = integer(0),
            stringsAsFactors = FALSE
        ))
    }

    final_results <- do.call(rbind, results)
    rownames(final_results) <- NULL

    return(final_results)
}

#' Calculate similarity matrix between node attributes
#'
#' Internal function that computes pairwise similarity scores between node attributes
#' using various similarity metrics. Used by \code{homophily} to create
#' similarity matrices for homophily analysis.
#'
#' @param attributes Numeric or character vector of node attributes. Length should
#'   match the number of nodes in the network.
#' @param method Character string specifying the similarity metric:
#'   \describe{
#'     \item{"correlation"}{Negative absolute difference. For continuous attributes,
#'       returns -|x_i - x_j|. Higher values indicate greater similarity.}
#'     \item{"euclidean"}{Negative Euclidean distance. Returns -sqrt((x_i - x_j)^2).
#'       Equivalent to correlation method for univariate attributes.}
#'     \item{"categorical"}{Binary similarity for categorical variables. Returns 1
#'       if attributes match, 0 otherwise.}
#'     \item{"cosine"}{Cosine similarity treating each attribute as a 1D vector.
#'       Returns (x_i * x_j) / (|x_i| * |x_j|). Ranges from -1 to 1.}
#'   }
#'
#' @return Square numeric matrix of dimension n x n where n is the length of
#'   attributes. Diagonal elements are set to 0. For correlation and euclidean
#'   methods, values are non-positive with 0 indicating perfect similarity.
#'   For categorical method, values are 0 or 1. For cosine method, values
#'   range from -1 to 1.
#'
#' @details
#' The function handles each similarity metric differently:
#' \itemize{
#'   \item Correlation and Euclidean methods are designed for continuous attributes
#'   \item Categorical method should be used for discrete/factor attributes
#'   \item Cosine similarity treats attributes as vectors, useful for normalized data
#' }
#'
#' Missing values in attributes will propagate to the similarity matrix.
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
calculate_similarity_matrix <- function(attributes, method) {
    # convert factors/characters to numeric for categorical method
    if (method == "categorical" && !is.numeric(attributes)) {
        attributes <- as.numeric(as.factor(attributes))
    }

    # ensure attributes is numeric
    if (!is.numeric(attributes)) {
        stop("Attributes must be numeric or convertible to numeric")
    }

    # call the cpp version
    calculate_similarity_matrix_cpp(attributes, method)
}

#' Calculate homophily statistics with significance testing
#'
#' Internal function that computes homophily correlation and related statistics
#' from similarity and network matrices. Performs permutation-based significance
#' testing and bootstrap confidence intervals.
#'
#' @param similarity_matrix Square numeric matrix of pairwise node similarities
#'   as produced by \code{calculate_similarity_matrix}.
#' @param net_matrix Square network adjacency matrix. Can be binary or weighted;
#'   will be converted to binary for analysis (positive values become 1).
#' @param significance_test Logical. Whether to perform permutation test for
#'   significance and compute confidence intervals.
#' @param n_permutations Integer. Number of permutations for significance testing
#'   and bootstrap confidence intervals.
#' @param alpha Numeric between 0 and 1. Significance level for confidence intervals.
#'   Default 0.05 gives 95% confidence intervals.
#'
#' @return Named list containing:
#'   \describe{
#'     \item{homophily_correlation}{Pearson correlation between similarity scores
#'       and tie presence. Positive values indicate homophily.}
#'     \item{mean_similarity_connected}{Mean similarity among connected dyads.}
#'     \item{mean_similarity_unconnected}{Mean similarity among unconnected dyads.}
#'     \item{similarity_difference}{Difference between mean similarities
#'       (connected - unconnected).}
#'     \item{p_value}{Two-tailed p-value from permutation test. NA if test not performed.}
#'     \item{ci_lower}{Lower bound of bootstrap confidence interval.}
#'     \item{ci_upper}{Upper bound of bootstrap confidence interval.}
#'     \item{n_connected_pairs}{Number of connected dyad pairs analyzed.}
#'     \item{n_unconnected_pairs}{Number of unconnected dyad pairs analyzed.}
#'   }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Converts the network to binary (tie/no tie)
#'   \item Extracts upper triangle to avoid counting dyads twice
#'   \item Calculates correlation between similarity and tie presence
#'   \item If requested, performs permutation test by shuffling similarities
#'   \item Computes bootstrap confidence intervals by resampling dyad pairs
#' }
#'
#' Diagonal elements (self-ties) are excluded from analysis. Missing values
#' in either matrix are removed pairwise.
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
calculate_homophily_stats <- function(
    similarity_matrix, net_matrix,
    significance_test, n_permutations, alpha) {
    # convert network to logical matrix for cpp function
    binary_net <- (net_matrix > 0) & !is.na(net_matrix)

    # ensure diagonals are NA for the cpp function
    diag(similarity_matrix) <- NA
    diag(binary_net) <- NA

    # call the cpp version
    calculate_homophily_stats_cpp(
        similarity_matrix,
        binary_net,
        significance_test,
        n_permutations,
        alpha
    )
}
