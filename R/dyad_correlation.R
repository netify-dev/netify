#' Analyze correlations between dyadic attributes and network ties
#'
#' Examines relationships between dyadic (pairwise) attributes and network
#' connections. Calculates correlations between dyadic variables and edge
#' weights/presence, with support for multiple correlation methods and
#' significance testing.
#'
#' @param netlet A netify object containing network data.
#' @param dyad_vars Character vector of dyadic attribute names to analyze.
#'   If NULL, analyzes all available dyadic variables.
#' @param edge_vars Character vector of edge variables to correlate with.
#'   If NULL, uses the main network matrix.
#' @param method Character string specifying correlation method:
#'   \describe{
#'     \item{"pearson"}{Pearson product-moment correlation (default)}
#'     \item{"spearman"}{Spearman rank correlation}
#'     \item{"kendall"}{Kendall's tau correlation}
#'   }
#' @param binary_network Logical. Whether to convert ties to binary before
#'   correlation. Default FALSE.
#' @param remove_diagonal Logical. Whether to exclude diagonal elements. Default TRUE.
#' @param significance_test Logical. Whether to calculate P-values and confidence
#'   intervals. Default TRUE.
#' @param alpha Significance level for confidence intervals. Default 0.05.
#' @param partial_correlations Logical. Whether to calculate partial correlations
#'   controlling for other dyadic variables. Default FALSE.
#' @param other_stats Named list of custom functions for additional statistics.
#' @param ... Additional arguments passed to custom functions.
#'
#' @return Data frame with one row per dyadic variable per network/time period:
#'   \describe{
#'     \item{\code{net}}{Network/time identifier}
#'     \item{\code{layer}}{Layer name}
#'     \item{\code{dyad_var}}{Name of dyadic variable}
#'     \item{\code{edge_var}}{Name of edge variable}
#'     \item{\code{correlation}}{Correlation coefficient}
#'     \item{\code{p_value}}{P-value for correlation significance}
#'     \item{\code{ci_lower}, \code{ci_upper}}{Confidence interval bounds}
#'     \item{\code{n_pairs}}{Number of dyad pairs included}
#'     \item{\code{method}}{Correlation method used}
#'     \item{\code{mean_dyad_var}}{Mean value of dyadic variable}
#'     \item{\code{sd_dyad_var}}{Standard deviation of dyadic variable}
#'     \item{\code{mean_edge_var}}{Mean value of edge variable}
#'     \item{\code{sd_edge_var}}{Standard deviation of edge variable}
#'   }
#'
#' @details
#' Extracts dyadic variables from dyad_data attribute and correlates them with
#' network ties. For longitudinal networks, correlations are calculated separately
#' for each time period. Dyadic variables should be stored as matrices with rows
#' and columns corresponding to network actors. Missing values are handled using
#' pairwise complete observations.
#' 
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export dyad_correlation

dyad_correlation <- function(
    netlet,
    dyad_vars = NULL,
    edge_vars = NULL,
    method = "pearson",
    binary_network = FALSE,
    remove_diagonal = TRUE,
    significance_test = TRUE,
    alpha = 0.05,
    partial_correlations = FALSE,
    other_stats = NULL,
    ...
    ){

    # input validation
    netify_check(netlet)
    checkmate::assert_character(dyad_vars, null.ok = TRUE)
    checkmate::assert_character(edge_vars, null.ok = TRUE)
    checkmate::assert_choice(method, c("pearson", "spearman", "kendall"))
    checkmate::assert_logical(binary_network, len = 1)
    checkmate::assert_logical(remove_diagonal, len = 1)
    checkmate::assert_logical(significance_test, len = 1)
    checkmate::assert_number(alpha, lower = 0, upper = 1)
    checkmate::assert_logical(partial_correlations, len = 1)

    # extract object attributes
    obj_attrs <- attributes(netlet)
    layers <- obj_attrs$layers
    dyad_data <- obj_attrs$dyad_data
    netify_type <- obj_attrs$netify_type

    # check if dyadic data exists
    if (is.null(dyad_data)) {
        cli::cli_abort("No dyadic data found in netify object. Use add_dyad_vars() to add dyadic attributes.")
    }

    # get available dyadic variables if not specified
    if (is.null(dyad_vars)) {
        first_time <- names(dyad_data)[1]
        dyad_vars <- names(dyad_data[[first_time]])
    }

    # check if specified dyadic variables exist
    first_time <- names(dyad_data)[1]
    available_vars <- names(dyad_data[[first_time]])
    missing_vars <- setdiff(dyad_vars, available_vars)
    if (length(missing_vars) > 0) {
        cli::cli_abort("Dyadic variables not found: {paste(missing_vars, collapse = ', ')}. Available: {paste(available_vars, collapse = ', ')}")
    }

    # set edge variables to layers if not specified
    if (is.null(edge_vars)) {
        edge_vars <- layers
    }

    # initialize results storage
    results_list <- list()

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

            # get dyadic data for this time period
            if (time_id %in% names(dyad_data)) {
                time_dyad_data <- dyad_data[[time_id]]
            } else {
                cli::cli_warn("No dyadic data found for time period {time_id}")
                next
            }

            # process each edge variable
            for (edge_var in edge_vars) {
                # get edge matrix (either from layer or use main network)
                if (edge_var == layer || edge_var == "network") {
                    edge_matrix <- net_matrix
                } else if (netify_type == "longit_array" && edge_var %in% layers) {
                    edge_matrix <- netlet_list[[time_id]][, , edge_var]
                } else {
                    cli::cli_warn("Edge variable {edge_var} not found for time {time_id}")
                    next
                }

                # convert to binary if requested
                if (binary_network) {
                    edge_matrix <- (edge_matrix > 0) & !is.na(edge_matrix)
                    edge_matrix <- as.numeric(edge_matrix)
                }

                # process each dyadic variable
                for (dyad_var in dyad_vars) {
                    if (!dyad_var %in% names(time_dyad_data)) {
                        next
                    }

                    dyad_matrix <- time_dyad_data[[dyad_var]]

                    # ensure matrices have same dimensions
                    if (!identical(dim(dyad_matrix), dim(edge_matrix))) {
                        cli::cli_warn("Dimension mismatch between dyadic variable {dyad_var} and edge variable {edge_var} for time {time_id}")
                        next
                    }

                    # calculate correlations
                    corr_result <- calculate_dyadic_correlation(
                        dyad_matrix, edge_matrix, method, remove_diagonal,
                        significance_test, alpha, partial_correlations,
                        if (partial_correlations) time_dyad_data else NULL,
                        dyad_var
                    )

                    # add custom statistics if provided
                    if (!is.null(other_stats)) {
                        custom_stats <- lapply(other_stats, function(f) {
                            tryCatch(f(dyad_matrix, edge_matrix, ...),
                                error = function(e) {
                                    cli::cli_warn("Error in custom statistic: {e$message}")
                                    NA
                                }
                            )
                        })
                        corr_result <- c(corr_result, unlist(custom_stats))
                    }

                    # store results
                    result_row <- data.frame(
                        net = time_id,
                        layer = layer,
                        dyad_var = dyad_var,
                        edge_var = edge_var,
                        corr_result,
                        method = method,
                        stringsAsFactors = FALSE
                    )

                    results_list[[length(results_list) + 1]] <- result_row
                }
            }
        }
    }

    # combine all results
    if (length(results_list) == 0) {
        cli::cli_warn("No correlations calculated - check that dyadic variables and network data are properly aligned")
        return(data.frame())
    }

    final_results <- do.call(rbind, results_list)
    rownames(final_results) <- NULL

    return(final_results)
}

# helper function to calculate dyadic correlations
calculate_dyadic_correlation <- function(dyad_matrix, edge_matrix, method,
                                         remove_diagonal, significance_test, alpha,
                                         partial_correlations, all_dyad_data,
                                         current_var) {
    # remove diagonal if requested
    if (remove_diagonal) {
        diag(dyad_matrix) <- NA
        diag(edge_matrix) <- NA
    }

    # extract vectors for correlation
    dyad_vec <- as.vector(dyad_matrix)
    edge_vec <- as.vector(edge_matrix)

    # remove missing values
    complete_cases <- !is.na(dyad_vec) & !is.na(edge_vec)
    dyad_vec <- dyad_vec[complete_cases]
    edge_vec <- edge_vec[complete_cases]

    if (length(dyad_vec) < 3) {
        return(list(
            correlation = NA,
            p_value = NA,
            ci_lower = NA,
            ci_upper = NA,
            n_pairs = length(dyad_vec),
            mean_dyad_var = NA,
            sd_dyad_var = NA,
            mean_edge_var = NA,
            sd_edge_var = NA
        ))
    }

    # calculate basic correlation
    if (partial_correlations && !is.null(all_dyad_data) && length(all_dyad_data) > 1) {
        # calculate partial correlation
        cor_result <- calculate_partial_correlation(
            dyad_vec, edge_vec, dyad_matrix, all_dyad_data, current_var,
            remove_diagonal, method
        )
    } else {
        # calculate simple correlation
        cor_result <- tryCatch(
            {
                cor_test <- stats::cor.test(dyad_vec, edge_vec, method = method)
                list(
                    correlation = cor_test$estimate,
                    p_value = if (significance_test) cor_test$p.value else NA,
                    ci_lower = if (significance_test && !is.null(cor_test$conf.int)) cor_test$conf.int[1] else NA,
                    ci_upper = if (significance_test && !is.null(cor_test$conf.int)) cor_test$conf.int[2] else NA
                )
            },
            error = function(e) {
                list(
                    correlation = NA,
                    p_value = NA,
                    ci_lower = NA,
                    ci_upper = NA
                )
            }
        )
    }

    # calculate descriptive statistics
    descriptive_stats <- list(
        n_pairs = length(dyad_vec),
        mean_dyad_var = mean(dyad_vec, na.rm = TRUE),
        sd_dyad_var = stats::sd(dyad_vec, na.rm = TRUE),
        mean_edge_var = mean(edge_vec, na.rm = TRUE),
        sd_edge_var = stats::sd(edge_vec, na.rm = TRUE)
    )

    return(c(cor_result, descriptive_stats))
}

# helper function to calculate partial correlations
calculate_partial_correlation <- function(dyad_vec, edge_vec, dyad_matrix,
                                          all_dyad_data, current_var,
                                          remove_diagonal, method) {
    # get other dyadic variables for partial correlation
    other_vars <- setdiff(names(all_dyad_data), current_var)

    if (length(other_vars) == 0) {
        # fall back to simple correlation if no other variables
        cor_test <- stats::cor.test(dyad_vec, edge_vec, method = method)
        return(list(
            correlation = cor_test$estimate,
            p_value = cor_test$p.value,
            ci_lower = if (!is.null(cor_test$conf.int)) cor_test$conf.int[1] else NA,
            ci_upper = if (!is.null(cor_test$conf.int)) cor_test$conf.int[2] else NA
        ))
    }

    # create data frame with all variables
    control_matrices <- lapply(other_vars, function(var) {
        control_matrix <- all_dyad_data[[var]]
        if (remove_diagonal) {
            diag(control_matrix) <- NA
        }
        as.vector(control_matrix)
    })

    # combine all vectors
    all_vectors <- do.call(cbind, c(list(dyad_vec, edge_vec), control_matrices))
    colnames(all_vectors) <- c("target", "outcome", other_vars)

    # remove incomplete cases
    complete_cases <- complete.cases(all_vectors)
    all_vectors <- all_vectors[complete_cases, , drop = FALSE]

    if (nrow(all_vectors) < (ncol(all_vectors) + 5)) { # Need enough observations
        return(list(
            correlation = NA,
            p_value = NA,
            ci_lower = NA,
            ci_upper = NA
        ))
    }

    # calculate partial correlation using linear regression approach
    tryCatch(
        {
            # regress target variable on control variables
            if (ncol(all_vectors) > 2) {
                target_model <- stats::lm(target ~ ., data = data.frame(all_vectors[, -2, drop = FALSE]))
                outcome_model <- stats::lm(outcome ~ ., data = data.frame(all_vectors[, -1, drop = FALSE]))

                target_residuals <- stats::residuals(target_model)
                outcome_residuals <- stats::residuals(outcome_model)
            } else {
                target_residuals <- all_vectors[, "target"]
                outcome_residuals <- all_vectors[, "outcome"]
            }

            # correlate residuals
            cor_test <- stats::cor.test(target_residuals, outcome_residuals, method = method)

            list(
                correlation = cor_test$estimate,
                p_value = cor_test$p.value,
                ci_lower = if (!is.null(cor_test$conf.int)) cor_test$conf.int[1] else NA,
                ci_upper = if (!is.null(cor_test$conf.int)) cor_test$conf.int[2] else NA
            )
        },
        error = function(e) {
            list(
                correlation = NA,
                p_value = NA,
                ci_lower = NA,
                ci_upper = NA
            )
        }
    )
}
