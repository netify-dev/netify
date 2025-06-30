#' Comprehensive summary of network-attribute relationships
#'
#' Provides comprehensive analysis of how nodal and dyadic attributes relate to
#' network structure. Combines multiple analytical approaches including homophily
#' analysis, mixing patterns, dyadic correlations, and network position-based
#' attribute summaries.
#'
#' @param netlet A netify object containing network data.
#' @param node_vars Character vector of nodal attributes to analyze. If NULL,
#'   analyzes all available nodal variables except actor and time.
#' @param dyad_vars Character vector of dyadic attributes to analyze. If NULL,
#'   analyzes all available dyadic variables.
#' @param include_centrality Logical. Whether to calculate attribute-centrality
#'   relationships. Default TRUE.
#' @param include_homophily Logical. Whether to perform homophily analysis. Default TRUE.
#' @param include_mixing Logical. Whether to create mixing matrices for categorical
#'   attributes. Default TRUE.
#' @param include_dyadic_correlations Logical. Whether to calculate dyadic correlations.
#'   Default TRUE.
#' @param centrality_measures Character vector of centrality measures to calculate.
#'   Options: "degree", "betweenness", "closeness", "eigenvector". Default c("degree", "betweenness").
#' @param categorical_threshold Maximum number of unique values for categorical
#'   treatment. Default 10.
#' @param significance_test Logical. Whether to perform significance tests. Default TRUE.
#' @param other_stats Named list of custom functions for additional statistics.
#' @param ... Additional arguments passed to component functions.
#'
#' @return List containing:
#'   \describe{
#'     \item{\code{homophily_analysis}}{Results from homophily analysis for nodal attributes}
#'     \item{\code{mixing_analysis}}{Results from mixing matrix analysis for categorical attributes}
#'     \item{\code{dyadic_correlations}}{Results from dyadic correlation analysis}
#'     \item{\code{centrality_correlations}}{Correlations between nodal attributes and centrality}
#'     \item{\code{attribute_summaries}}{Descriptive statistics for attributes}
#'     \item{\code{overall_summary}}{High-level summary of key findings}
#'   }
#'
#' @details
#' Serves as comprehensive wrapper around exploratory analysis functions.
#' Automatically determines appropriate analysis methods based on attribute types.
#' For large networks or many attributes, consider setting some components to FALSE
#' for faster computation. Centrality measures use igraph functions.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export attribute_report

attribute_report <- function(
    netlet,
    node_vars = NULL,
    dyad_vars = NULL,
    include_centrality = TRUE,
    include_homophily = TRUE,
    include_mixing = TRUE,
    include_dyadic_correlations = TRUE,
    centrality_measures = c("degree", "betweenness"),
    categorical_threshold = 10,
    significance_test = TRUE,
    other_stats = NULL,
    ...) {
    # input validation
    netify_check(netlet)
    checkmate::assert_character(node_vars, null.ok = TRUE)
    checkmate::assert_character(dyad_vars, null.ok = TRUE)
    checkmate::assert_logical(include_centrality, len = 1)
    checkmate::assert_logical(include_homophily, len = 1)
    checkmate::assert_logical(include_mixing, len = 1)
    checkmate::assert_logical(include_dyadic_correlations, len = 1)
    checkmate::assert_subset(centrality_measures, c("degree", "betweenness", "closeness", "eigenvector"))
    checkmate::assert_count(categorical_threshold, positive = TRUE)
    checkmate::assert_logical(significance_test, len = 1)

    # extract object attributes
    obj_attrs <- attributes(netlet)
    nodal_data <- obj_attrs$nodal_data
    dyad_data <- obj_attrs$dyad_data
    netify_type <- obj_attrs$netify_type

    # determine available variables
    if (!is.null(nodal_data)) {
        available_node_vars <- setdiff(names(nodal_data), c("actor", "time"))
        if (is.null(node_vars)) {
            node_vars <- available_node_vars
        } else {
            missing_node_vars <- setdiff(node_vars, available_node_vars)
            if (length(missing_node_vars) > 0) {
                cli::cli_warn("Node variables not found: {paste(missing_node_vars, collapse = ', ')}")
                node_vars <- intersect(node_vars, available_node_vars)
            }
        }
    } else {
        node_vars <- character(0)
        if (include_centrality || include_homophily || include_mixing) {
            cli::cli_warn("No nodal data available - skipping node-based analyses")
            include_centrality <- include_homophily <- include_mixing <- FALSE
        }
    }

    if (!is.null(dyad_data)) {
        first_time <- names(dyad_data)[1]
        available_dyad_vars <- names(dyad_data[[first_time]])
        if (is.null(dyad_vars)) {
            dyad_vars <- available_dyad_vars
        } else {
            missing_dyad_vars <- setdiff(dyad_vars, available_dyad_vars)
            if (length(missing_dyad_vars) > 0) {
                cli::cli_warn("Dyadic variables not found: {paste(missing_dyad_vars, collapse = ', ')}")
                dyad_vars <- intersect(dyad_vars, available_dyad_vars)
            }
        }
    } else {
        dyad_vars <- character(0)
        if (include_dyadic_correlations) {
            cli::cli_warn("No dyadic data available - skipping dyadic correlation analysis")
            include_dyadic_correlations <- FALSE
        }
    }

    # initialize results list
    results <- list()

    # 1. homophily analysis
    if (include_homophily && length(node_vars) > 0) {
        cli::cli_alert_info("Calculating homophily analysis...")

        homophily_results <- list()
        for (var in node_vars) {
            tryCatch(
                {
                    # Determine appropriate similarity method based on variable type
                    if (!is.null(nodal_data)) {
                        # Extract variable values depending on data structure
                        if (netify_type == "cross_sec") {
                            var_values <- nodal_data[[var]]
                        } else if (is.list(nodal_data)) {
                            # For longitudinal, get values from first time period
                            first_time <- names(nodal_data)[1]
                            var_values <- nodal_data[[first_time]][[var]]
                        } else {
                            var_values <- NULL
                        }

                        if (!is.null(var_values)) {
                            n_unique <- length(unique(var_values[!is.na(var_values)]))

                            method <- if (is.character(var_values) || is.factor(var_values) || n_unique <= categorical_threshold) {
                                "categorical"
                            } else {
                                "correlation"
                            }

                            homophily_result <- homophily(
                                netlet,
                                attribute = var, method = method,
                                significance_test = significance_test, ...
                            )
                            homophily_results[[var]] <- homophily_result
                        }
                    }
                },
                error = function(e) {
                    cli::cli_warn("Error in homophily analysis for {var}: {e$message}")
                }
            )
        }
        results$homophily_analysis <- do.call(rbind, homophily_results)
    }

    # 2. mixing analysis
    if (include_mixing && length(node_vars) > 0) {
        cli::cli_alert_info("Calculating mixing matrices...")

        # identify categorical variables
        categorical_vars <- character(0)
        if (!is.null(nodal_data)) {
            for (var in node_vars) {
                var_values <- nodal_data[[var]]
                n_unique <- length(unique(var_values[!is.na(var_values)]))
                if (is.character(var_values) || is.factor(var_values) || n_unique <= categorical_threshold) {
                    categorical_vars <- c(categorical_vars, var)
                }
            }
        }

        mixing_results <- list()
        for (var in categorical_vars) {
            tryCatch(
                {
                    mixing_result <- mixing_matrix(
                        netlet,
                        attribute = var,
                        normalized = TRUE, ...
                    )
                    mixing_results[[var]] <- mixing_result
                },
                error = function(e) {
                    cli::cli_warn("Error in mixing analysis for {var}: {e$message}")
                }
            )
        }
        results$mixing_analysis <- mixing_results
    }

    # 3. dyadic correlations
    if (include_dyadic_correlations && length(dyad_vars) > 0) {
        cli::cli_alert_info("Calculating dyadic correlations...")

        tryCatch(
            {
                dyadic_result <- dyad_correlation(
                    netlet,
                    dyad_vars = dyad_vars,
                    significance_test = significance_test, ...
                )
                results$dyadic_correlations <- dyadic_result
            },
            error = function(e) {
                cli::cli_warn("Error in dyadic correlation analysis: {e$message}")
            }
        )
    }

    # 4. centrality correlations
    if (include_centrality && length(node_vars) > 0) {
        cli::cli_alert_info("Calculating centrality correlations...")

        tryCatch(
            {
                centrality_result <- calculate_centrality_correlations(
                    netlet, node_vars, centrality_measures, significance_test
                )
                results$centrality_correlations <- centrality_result
            },
            error = function(e) {
                cli::cli_warn("Error in centrality correlation analysis: {e$message}")
            }
        )
    }

    # 5. attribute summaries
    cli::cli_alert_info("Calculating attribute summaries...")
    attribute_summaries <- calculate_attribute_summaries(
        netlet, node_vars, dyad_vars
    )
    results$attribute_summaries <- attribute_summaries

    # 6. overall summary
    overall_summary <- create_overall_summary(results, node_vars, dyad_vars)
    results$overall_summary <- overall_summary

    # add custom statistics if provided
    if (!is.null(other_stats)) {
        cli::cli_alert_info("Calculating custom statistics...")
        custom_results <- list()
        for (stat_name in names(other_stats)) {
            tryCatch(
                {
                    custom_results[[stat_name]] <- other_stats[[stat_name]](netlet, ...)
                },
                error = function(e) {
                    cli::cli_warn("Error in custom statistic {stat_name}: {e$message}")
                }
            )
        }
        results$custom_statistics <- custom_results
    }

    cli::cli_alert_success("Network-attribute analysis complete!")
    return(results)
}

# helper function to calculate centrality correlations
calculate_centrality_correlations <- function(netlet, node_vars, centrality_measures, significance_test) {
    # extract network data
    obj_attrs <- attributes(netlet)
    nodal_data <- obj_attrs$nodal_data
    layers <- obj_attrs$layers
    netify_type <- obj_attrs$netify_type

    # convert to list format for processing
    netlet_list <- switch(netify_type,
        "cross_sec" = list("1" = netlet),
        "longit_array" = {
            # Check if this is multilayer longitudinal (4D) or single layer (3D)
            if (length(dim(netlet)) == 4) {
                # Multilayer longitudinal: extract time periods from 4th dimension
                time_names <- dimnames(netlet)[[4]]
                if (is.null(time_names)) {
                    time_names <- as.character(seq_len(dim(netlet)[4]))
                }
                net_list <- list()
                for (t in seq_along(time_names)) {
                    net_list[[time_names[t]]] <- netlet[, , , t]
                }
                net_list
            } else {
                # Single layer longitudinal: extract from 3rd dimension
                time_names <- dimnames(netlet)[[3]]
                if (is.null(time_names)) {
                    time_names <- as.character(seq_len(dim(netlet)[3]))
                }
                net_list <- list()
                for (t in seq_along(time_names)) {
                    net_list[[time_names[t]]] <- netlet[, , t]
                }
                net_list
            }
        },
        "longit_list" = netlet
    )

    results_list <- list()

    for (layer_index in seq_along(layers)) {
        layer <- layers[layer_index]
        for (time_id in names(netlet_list)) {
            # get network matrix
            net_matrix <- netlet_list[[time_id]]
            # Extract specific layer for multilayer networks
            if (length(layers) > 1) {
                if (netify_type == "cross_sec") {
                    # For cross-sectional multilayer: 3D array [actors, actors, layers]
                    net_matrix <- netlet[, , layer_index]
                } else if (netify_type == "longit_array" && length(dim(netlet)) == 4) {
                    # For longitudinal multilayer: 4D array [actors, actors, layers, time]
                    net_matrix <- net_matrix[, , layer_index]
                }
            }

            # calculate centrality measures
            centralities <- calculate_centrality_measures(net_matrix, centrality_measures)

            # get nodal attributes for this time
            if (netify_type == "cross_sec") {
                time_nodal_data <- nodal_data
            } else {
                time_nodal_data <- nodal_data[nodal_data$time == time_id, ]
            }

            # match actors
            matrix_actors <- rownames(net_matrix)
            if (is.null(matrix_actors)) {
                matrix_actors <- as.character(seq_len(nrow(net_matrix)))
            }

            # calculate correlations between attributes and centrality
            for (node_var in node_vars) {
                if (node_var %in% names(time_nodal_data)) {
                    node_values <- time_nodal_data[[node_var]]
                    actors <- time_nodal_data$actor

                    # Match to matrix
                    attr_indices <- match(matrix_actors, actors)
                    node_values <- node_values[attr_indices]

                    for (cent_measure in names(centralities)) {
                        cent_values <- centralities[[cent_measure]]

                        # calculate correlation
                        complete_cases <- !is.na(node_values) & !is.na(cent_values)
                        if (sum(complete_cases) > 2) {
                            corr_result <- tryCatch(
                                {
                                    if (significance_test) {
                                        cor_test <- stats::cor.test(node_values[complete_cases], cent_values[complete_cases])
                                        list(
                                            correlation = cor_test$estimate,
                                            p_value = cor_test$p.value,
                                            ci_lower = if (!is.null(cor_test$conf.int)) cor_test$conf.int[1] else NA,
                                            ci_upper = if (!is.null(cor_test$conf.int)) cor_test$conf.int[2] else NA
                                        )
                                    } else {
                                        list(
                                            correlation = stats::cor(node_values[complete_cases], cent_values[complete_cases]),
                                            p_value = NA,
                                            ci_lower = NA,
                                            ci_upper = NA
                                        )
                                    }
                                },
                                error = function(e) {
                                    list(correlation = NA, p_value = NA, ci_lower = NA, ci_upper = NA)
                                }
                            )

                            result_row <- data.frame(
                                net = time_id,
                                layer = layer,
                                node_var = node_var,
                                centrality_measure = cent_measure,
                                corr_result,
                                n_observations = sum(complete_cases),
                                stringsAsFactors = FALSE
                            )

                            results_list[[length(results_list) + 1]] <- result_row
                        }
                    }
                }
            }
        }
    }

    if (length(results_list) > 0) {
        return(do.call(rbind, results_list))
    } else {
        return(data.frame())
    }
}

# helper function to calculate centrality measures
calculate_centrality_measures <- function(net_matrix, centrality_measures) {
    # convert to igraph object
    g <- tryCatch(
        {
            # remove self-loops and convert to binary for centrality calculation
            binary_matrix <- (net_matrix > 0) & !is.na(net_matrix)
            diag(binary_matrix) <- FALSE # Remove self-loops
            igraph::graph_from_adjacency_matrix(binary_matrix, mode = "directed")
        },
        error = function(e) {
            return(NULL)
        }
    )

    if (is.null(g)) {
        return(list())
    }

    centralities <- list()

    if ("degree" %in% centrality_measures) {
        centralities$degree <- igraph::degree(g, mode = "total")
    }

    if ("betweenness" %in% centrality_measures) {
        centralities$betweenness <- tryCatch(
            {
                igraph::betweenness(g, directed = TRUE)
            },
            error = function(e) rep(NA, igraph::vcount(g))
        )
    }

    if ("closeness" %in% centrality_measures) {
        centralities$closeness <- tryCatch(
            {
                igraph::closeness(g, mode = "total")
            },
            error = function(e) rep(NA, igraph::vcount(g))
        )
    }

    if ("eigenvector" %in% centrality_measures) {
        centralities$eigenvector <- tryCatch(
            {
                igraph::eigen_centrality(g, directed = TRUE)$vector
            },
            error = function(e) rep(NA, igraph::vcount(g))
        )
    }

    return(centralities)
}

# helper function to calculate attribute summaries
calculate_attribute_summaries <- function(netlet, node_vars, dyad_vars) {
    obj_attrs <- attributes(netlet)
    nodal_data <- obj_attrs$nodal_data
    dyad_data <- obj_attrs$dyad_data

    summaries <- list()

    # node variable summaries
    if (!is.null(nodal_data) && length(node_vars) > 0) {
        node_summaries <- list()
        for (var in node_vars) {
            if (var %in% names(nodal_data)) {
                var_data <- nodal_data[[var]]
                node_summaries[[var]] <- list(
                    n_observations = length(var_data),
                    n_missing = sum(is.na(var_data)),
                    n_unique = length(unique(var_data[!is.na(var_data)])),
                    mean = if (is.numeric(var_data)) mean(var_data, na.rm = TRUE) else NA,
                    sd = if (is.numeric(var_data)) stats::sd(var_data, na.rm = TRUE) else NA,
                    min = if (is.numeric(var_data)) min(var_data, na.rm = TRUE) else NA,
                    max = if (is.numeric(var_data)) max(var_data, na.rm = TRUE) else NA
                )
            }
        }
        summaries$node_variables <- node_summaries
    }

    # dyad variable summaries
    if (!is.null(dyad_data) && length(dyad_vars) > 0) {
        dyad_summaries <- list()
        first_time <- names(dyad_data)[1]

        for (var in dyad_vars) {
            if (var %in% names(dyad_data[[first_time]])) {
                var_matrix <- dyad_data[[first_time]][[var]]
                var_vector <- as.vector(var_matrix)
                dyad_summaries[[var]] <- list(
                    n_observations = length(var_vector),
                    n_missing = sum(is.na(var_vector)),
                    n_unique = length(unique(var_vector[!is.na(var_vector)])),
                    mean = if (is.numeric(var_vector)) mean(var_vector, na.rm = TRUE) else NA,
                    sd = if (is.numeric(var_vector)) stats::sd(var_vector, na.rm = TRUE) else NA,
                    min = if (is.numeric(var_vector)) min(var_vector, na.rm = TRUE) else NA,
                    max = if (is.numeric(var_vector)) max(var_vector, na.rm = TRUE) else NA
                )
            }
        }
        summaries$dyadic_variables <- dyad_summaries
    }

    return(summaries)
}

# helper function to create overall summary
create_overall_summary <- function(results, node_vars, dyad_vars) {
    summary_text <- character(0)

    # homophily summary
    if ("homophily_analysis" %in% names(results) && nrow(results$homophily_analysis) > 0) {
        homophily_data <- results$homophily_analysis
        significant_homophily <- homophily_data[!is.na(homophily_data$p_value) & homophily_data$p_value < 0.05, ]

        if (nrow(significant_homophily) > 0) {
            summary_text <- c(
                summary_text,
                sprintf(
                    "Significant homophily detected for %d/%d node attributes",
                    length(unique(significant_homophily$attribute)),
                    length(unique(homophily_data$attribute))
                )
            )
        }
    }

    # mixing summary
    if ("mixing_analysis" %in% names(results) && length(results$mixing_analysis) > 0) {
        high_assortativity <- character(0)
        for (var in names(results$mixing_analysis)) {
            mixing_stats <- results$mixing_analysis[[var]]$summary_stats
            if (!is.null(mixing_stats) && nrow(mixing_stats) > 0) {
                max_assort <- max(mixing_stats$assortativity, na.rm = TRUE)
                if (!is.na(max_assort) && max_assort > 0.3) {
                    high_assortativity <- c(high_assortativity, var)
                }
            }
        }

        if (length(high_assortativity) > 0) {
            summary_text <- c(
                summary_text,
                sprintf(
                    "High assortativity (>0.3) found for: %s",
                    paste(high_assortativity, collapse = ", ")
                )
            )
        }
    }

    # dyadic correlation summary
    if ("dyadic_correlations" %in% names(results) && nrow(results$dyadic_correlations) > 0) {
        dyadic_data <- results$dyadic_correlations
        strong_correlations <- dyadic_data[!is.na(dyadic_data$correlation) & abs(dyadic_data$correlation) > 0.3, ]

        if (nrow(strong_correlations) > 0) {
            summary_text <- c(
                summary_text,
                sprintf(
                    "Strong dyadic correlations (|r|>0.3) found for %d dyadic variables",
                    length(unique(strong_correlations$dyad_var))
                )
            )
        }
    }

    # centrality correlation summary
    if ("centrality_correlations" %in% names(results) && nrow(results$centrality_correlations) > 0) {
        cent_data <- results$centrality_correlations
        strong_cent_corr <- cent_data[!is.na(cent_data$correlation) & abs(cent_data$correlation) > 0.3, ]

        if (nrow(strong_cent_corr) > 0) {
            summary_text <- c(
                summary_text,
                sprintf(
                    "Strong centrality correlations (|r|>0.3) found for %d node attributes",
                    length(unique(strong_cent_corr$node_var))
                )
            )
        }
    }

    if (length(summary_text) == 0) {
        summary_text <- "No strong attribute-network relationships detected with current thresholds"
    }

    list(
        summary_points = summary_text,
        analysis_components = names(results),
        node_variables_analyzed = node_vars,
        dyadic_variables_analyzed = dyad_vars,
        timestamp = Sys.time()
    )
}
