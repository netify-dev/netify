#' Summary of network-attribute relationships
#'
#' summarizes how nodal and dyadic attributes relate to network structure.
#' combines homophily analysis, mixing patterns, dyadic correlations, and
#' network position-based attribute summaries.
#'
#' @param netlet a netify object containing network data.
#' @param node_vars character vector of nodal attributes to analyze. if NULL,
#'   analyzes all available nodal variables except actor and time.
#' @param dyad_vars character vector of dyadic attributes to analyze. if NULL,
#'   analyzes all available dyadic variables.
#' @param include_centrality logical. whether to calculate attribute-centrality
#'   relationships. default TRUE.
#' @param include_homophily logical. whether to perform homophily analysis. default TRUE.
#' @param include_mixing logical. whether to create mixing matrices for categorical
#'   attributes. default TRUE.
#' @param include_dyadic_correlations logical. whether to calculate dyadic correlations.
#'   default TRUE.
#' @param centrality_measures character vector of centrality measures to calculate.
#'   options: "degree", "betweenness", "closeness", "eigenvector". default c("degree", "betweenness").
#' @param categorical_threshold maximum number of unique values for categorical
#'   treatment. default 10.
#' @param significance_test logical. whether to perform significance tests. default TRUE.
#' @param other_stats named list of custom functions for additional statistics.
#' @param ... additional arguments passed to component functions.
#'
#' @return list containing:
#'   \describe{
#'     \item{\code{homophily_analysis}}{results from homophily analysis for nodal attributes}
#'     \item{\code{mixing_analysis}}{results from mixing matrix analysis for categorical attributes}
#'     \item{\code{dyadic_correlations}}{results from dyadic correlation analysis}
#'     \item{\code{centrality_correlations}}{correlations between nodal attributes and centrality}
#'     \item{\code{attribute_summaries}}{descriptive statistics for attributes}
#'     \item{\code{overall_summary}}{brief summary of key findings}
#'   }
#'
#' @details
#' wraps the exploratory analysis functions and chooses methods based on
#' attribute types.
#' for large networks or many attributes, consider setting some components to FALSE
#' for faster computation. centrality measures use igraph functions.
#'
#' @author cassy dorff, shahryar minhas
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
	requested_node_vars <- node_vars
	requested_dyad_vars <- dyad_vars
	checkmate::assert_character(node_vars, null.ok = TRUE)
	checkmate::assert_character(dyad_vars, null.ok = TRUE)
	checkmate::assert_logical(include_centrality, len = 1)
	checkmate::assert_logical(include_homophily, len = 1)
	checkmate::assert_logical(include_mixing, len = 1)
	checkmate::assert_logical(include_dyadic_correlations, len = 1)
	checkmate::assert_subset(centrality_measures, c("degree", "betweenness", "closeness", "eigenvector"))
	checkmate::assert_count(categorical_threshold, positive = TRUE)
	checkmate::assert_logical(significance_test, len = 1)
	if (!is.null(other_stats)) {
		if (!is.list(other_stats) || is.null(names(other_stats)) ||
			anyNA(names(other_stats)) || any(names(other_stats) == "") ||
			anyDuplicated(names(other_stats))) {
			cli::cli_abort("other_stats must be a named list of functions with unique, non-empty names.")
		}
		if (!all(vapply(other_stats, is.function, logical(1)))) {
			cli::cli_abort("All elements of other_stats must be functions.")
		}
	}

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
		if (!is.null(requested_node_vars) && length(node_vars) == 0L &&
			(include_centrality || include_homophily || include_mixing)) {
			cli::cli_abort("None of the requested node variables are available.")
		}
	} else {
		node_vars <- character(0)
		if (include_centrality || include_homophily || include_mixing) {
			cli::cli_warn("No nodal data available - skipping node-based analyses")
			include_centrality <- include_homophily <- include_mixing <- FALSE
		}
	}

	if (!is.null(dyad_data)) {
		available_dyad_vars <- unique(unlist(lapply(dyad_data, names), use.names = FALSE))
		if (is.null(dyad_vars)) {
			dyad_vars <- available_dyad_vars
		} else {
			missing_dyad_vars <- setdiff(dyad_vars, available_dyad_vars)
			if (length(missing_dyad_vars) > 0) {
				cli::cli_warn("Dyadic variables not found: {paste(missing_dyad_vars, collapse = ', ')}")
				dyad_vars <- intersect(dyad_vars, available_dyad_vars)
			}
		}
		if (!is.null(requested_dyad_vars) && length(dyad_vars) == 0L &&
			include_dyadic_correlations) {
			cli::cli_abort("None of the requested dyadic variables are available.")
		}
	} else {
		dyad_vars <- character(0)
		if (include_dyadic_correlations) {
			cli::cli_warn("No dyadic data available - skipping dyadic correlation analysis")
			include_dyadic_correlations <- FALSE
		}
	}

	results <- list()

	# homophily analysis
	if (include_homophily && length(node_vars) > 0) {
		cli::cli_alert_info("Calculating homophily analysis...")

		homophily_results <- list()
		for (var in node_vars) {
			# pick similarity method based on variable type
			if (!is.null(nodal_data)) {
				# pull variable values for the right data structure
				if (netify_type == "cross_sec") {
					var_values <- nodal_data[[var]]
				} else if (is.data.frame(nodal_data)) {
					var_values <- nodal_data[[var]]
				} else if (is.list(nodal_data)) {
					first_time <- names(nodal_data)[1]
					var_values <- nodal_data[[first_time]][[var]]
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
		}
			if (length(homophily_results) == 0L) {
				cli::cli_warn("No homophily analyses completed. Check the requested nodal variables and network mode.")
				results$homophily_analysis <- NULL
			} else {
				results$homophily_analysis <- do.call(rbind, homophily_results)
			}
		}

	# mixing analysis
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
			mixing_result <- mixing_matrix(
				netlet,
				attribute = var,
				normalized = TRUE, ...
			)
			mixing_results[[var]] <- mixing_result
		}
		results$mixing_analysis <- mixing_results
	}

	# dyadic correlations
	if (include_dyadic_correlations && length(dyad_vars) > 0) {
		cli::cli_alert_info("Calculating dyadic correlations...")

		dyadic_result <- dyad_correlation(
			netlet,
			dyad_vars = dyad_vars,
			significance_test = significance_test, ...
		)
		results$dyadic_correlations <- dyadic_result
	}

	# centrality correlations
	if (include_centrality && length(node_vars) > 0) {
		cli::cli_alert_info("Calculating centrality correlations...")

		centrality_result <- calculate_centrality_correlations(
			netlet, node_vars, centrality_measures, significance_test
		)
		results$centrality_correlations <- centrality_result
	}

	# attribute summaries
	cli::cli_alert_info("Calculating attribute summaries...")
	attribute_summaries <- calculate_attribute_summaries(
		netlet, node_vars, dyad_vars
	)
	results$attribute_summaries <- attribute_summaries

	# overall summary
	overall_summary <- create_overall_summary(results, node_vars, dyad_vars)
	results$overall_summary <- overall_summary

	# add custom statistics if provided
	if (!is.null(other_stats)) {
		cli::cli_alert_info("Calculating custom statistics...")
		custom_results <- list()
		for (stat_name in names(other_stats)) {
			custom_results[[stat_name]] <- other_stats[[stat_name]](netlet, ...)
		}
		results$custom_statistics <- custom_results
	}

	cli::cli_alert_success("Network-attribute analysis complete!")
	return(results)
}

# calculate correlations between attributes and centrality
calculate_centrality_correlations <- function(netlet, node_vars, centrality_measures, significance_test) {
	obj_attrs <- attributes(netlet)
	nodal_data <- obj_attrs$nodal_data
	layers <- obj_attrs$layers
	netify_type <- obj_attrs$netify_type

	# normalize to a list of time slices for processing
	netlet_list <- switch(netify_type,
		"cross_sec" = list("1" = netlet),
		"longit_array" = {
			# 4d = multilayer longitudinal, 3d = single layer longitudinal
			if (length(dim(netlet)) == 4) {
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
			net_matrix <- netlet_list[[time_id]]
			# slice the requested layer for multilayer networks
			if (length(layers) > 1) {
				if (netify_type == "cross_sec") {
					net_matrix <- netlet[, , layer_index]
				} else if (netify_type == "longit_array" && length(dim(netlet)) == 4) {
					net_matrix <- net_matrix[, , layer_index]
				}
			}

			centralities <- calculate_centrality_measures(net_matrix, centrality_measures)

			# nodal attributes for this time
			if (netify_type == "cross_sec") {
				time_nodal_data <- nodal_data
			} else {
				time_nodal_data <- nodal_data[nodal_data$time == time_id, ]
			}

			matrix_actors <- rownames(net_matrix)
			if (is.null(matrix_actors)) {
				matrix_actors <- as.character(seq_len(nrow(net_matrix)))
			}

				# correlate attributes with centralities
				for (node_var in node_vars) {
					if (node_var %in% names(time_nodal_data)) {
						node_values <- time_nodal_data[[node_var]]
						if (!is.numeric(node_values)) {
							next
						}
						actors <- time_nodal_data$actor

						attr_indices <- match(matrix_actors, actors)
						node_values <- node_values[attr_indices]

						for (cent_measure in names(centralities)) {
							cent_values <- centralities[[cent_measure]]

							complete_cases <- !is.na(node_values) & is.finite(node_values) &
								!is.na(cent_values) & is.finite(cent_values)
							if (sum(complete_cases) > 2 &&
								length(unique(node_values[complete_cases])) > 1L &&
								length(unique(cent_values[complete_cases])) > 1L) {
							# surface cor.test failures rather than letting them become silent nas
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
									cli::cli_warn(c(
										"!" = "cor.test failed for {.val {node_var}} vs {.val {cent_measure}}: {conditionMessage(e)}",
										"i" = "Returning NA for this pair."
									))
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

# compute requested centralities via igraph
calculate_centrality_measures <- function(net_matrix, centrality_measures) {
	# build an igraph from the binary version of the matrix
	binary_matrix <- (net_matrix != 0) & !is.na(net_matrix)
	diag(binary_matrix) <- FALSE
	g <- tryCatch(
		igraph::graph_from_adjacency_matrix(binary_matrix, mode = "directed"),
		error = function(e) {
			cli::cli_warn(c(
				"!" = "Could not build igraph for centrality measures: {conditionMessage(e)}",
				"i" = "Skipping centrality calculations for this slice."
			))
			NULL
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
			error = function(e) {
				cli::cli_warn("Could not calculate betweenness centrality: {conditionMessage(e)}")
				rep(NA, igraph::vcount(g))
			}
		)
	}

	if ("closeness" %in% centrality_measures) {
		centralities$closeness <- tryCatch(
			{
				igraph::closeness(g, mode = "total")
			},
			error = function(e) {
				cli::cli_warn("Could not calculate closeness centrality: {conditionMessage(e)}")
				rep(NA, igraph::vcount(g))
			}
		)
	}

	if ("eigenvector" %in% centrality_measures) {
		centralities$eigenvector <- tryCatch(
			{
				igraph::eigen_centrality(g, directed = TRUE)$vector
			},
			error = function(e) {
				cli::cli_warn("Could not calculate eigenvector centrality: {conditionMessage(e)}")
				rep(NA, igraph::vcount(g))
			}
		)
	}

	return(centralities)
}

# descriptive summaries for nodal and dyadic variables
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

	# build the findings summary
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
