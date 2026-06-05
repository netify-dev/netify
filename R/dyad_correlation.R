#' Analyze correlations between dyadic attributes and network ties
#'
#' examines relationships between dyadic (pairwise) attributes and network
#' connections. calculates correlations between dyadic variables and edge
#' weights/presence, with support for multiple correlation methods and
#' significance testing.
#'
#' @param netlet a netify object containing network data.
#' @param dyad_vars character vector of dyadic attribute names to analyze.
#'   if NULL, analyzes all available dyadic variables.
#' @param edge_vars character vector of edge variables to correlate with.
#'   if NULL, uses the main network matrix.
#' @param method character string specifying correlation method:
#'   \describe{
#'     \item{"pearson"}{pearson product-moment correlation (default)}
#'     \item{"spearman"}{spearman rank correlation}
#'     \item{"kendall"}{kendall's tau correlation}
#'   }
#' @param binary_network logical. whether to convert ties to binary before
#'   correlation. default FALSE.
#' @param remove_diagonal logical. whether to exclude diagonal elements. default TRUE.
#' @param significance_test logical. whether to calculate ordinary correlation
#'   p-values and confidence intervals on the dyad vectors. default TRUE.
#' @param alpha significance level for confidence intervals. default 0.05.
#' @param partial_correlations logical. whether to calculate partial correlations
#'   controlling for other dyadic variables. default FALSE.
#' @param other_stats named list of custom functions for additional statistics.
#' @param ... additional arguments passed to custom functions.
#'
#' @return data frame with one row per dyadic variable per network/time period:
#'   \describe{
#'     \item{\code{net}}{network/time identifier}
#'     \item{\code{layer}}{layer name}
#'     \item{\code{dyad_var}}{name of dyadic variable}
#'     \item{\code{edge_var}}{name of edge variable}
#'     \item{\code{correlation}}{correlation coefficient}
#'     \item{\code{p_value}}{p-value for correlation significance}
#'     \item{\code{ci_lower}, \code{ci_upper}}{confidence interval bounds}
#'     \item{\code{n_pairs}}{number of dyad pairs included}
#'     \item{\code{method}}{correlation method used}
#'     \item{\code{mean_dyad_var}}{mean value of dyadic variable}
#'     \item{\code{sd_dyad_var}}{standard deviation of dyadic variable}
#'     \item{\code{mean_edge_var}}{mean value of edge variable}
#'     \item{\code{sd_edge_var}}{standard deviation of edge variable}
#'   }
#'
#' @details
#' extracts dyadic variables from dyad_data attribute and correlates them with
#' network ties. for longitudinal networks, correlations are calculated separately
#' for each time period. dyadic variables should be stored as matrices with rows
#' and columns corresponding to network actors. missing values are handled using
#' pairwise complete observations.
#'
#' the reported p-values and confidence intervals are the standard tests from
#' \code{stats::cor.test()} applied to the dyad vectors. they are useful as
#' descriptive screens, but they do not model network dependence among dyads.
#'
#' @author cassy dorff, shahryar minhas
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
	...) {
	# input validation
	netify_check(netlet)
	checkmate::assert_character(dyad_vars, null.ok = TRUE)
	checkmate::assert_character(edge_vars, null.ok = TRUE)
	checkmate::assert_choice(method, c("pearson", "spearman", "kendall"))
	checkmate::assert_logical(binary_network, len = 1)
	checkmate::assert_logical(remove_diagonal, len = 1)
	checkmate::assert_logical(significance_test, len = 1)
	checkmate::assert_number(alpha, lower = 0, upper = 1)
	if (alpha <= 0 || alpha >= 1) {
		cli::cli_abort("{.arg alpha} must be in (0, 1).")
	}
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
	for (layer_index in seq_along(layers)) {
		layer <- layers[layer_index]
		# convert to list format for processing
		netlet_list <- switch(netify_type,
			"cross_sec" = list("1" = netlet),
			"longit_array" = {
				# check if this is multilayer longitudinal (4d) or single layer (3d)
				if (length(dim(netlet)) == 4) {
					# multilayer longitudinal: extract time periods from 4th dimension
					time_names <- dimnames(netlet)[[4]]
					if (is.null(time_names)) {
						time_names <- as.character(seq_len(dim(netlet)[4]))
					}
					net_list <- list()
					for (t in seq_along(time_names)) {
						net_list[[time_names[t]]] <- netlet[, , , t]
					}
				} else {
					# single layer longitudinal: extract from 3rd dimension
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

		# process each time period
		for (time_id in names(netlet_list)) {
			# get network matrix for this time period
			net_matrix <- netlet_list[[time_id]]

			# extract specific layer for multilayer networks
			if (length(layers) > 1) {
				if (netify_type == "cross_sec") {
					# for cross-sectional multilayer: 3d array [actors, actors, layers]
					net_matrix <- netlet[, , layer_index]
				} else if (netify_type == "longit_array" && length(dim(netlet)) == 4) {
					# for longitudinal multilayer: 4d array [actors, actors, layers, time]
					net_matrix <- net_matrix[, , layer_index]
				}
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
				} else if (edge_var %in% layers) {
					# extract a different layer as edge variable
					edge_layer_idx <- which(layers == edge_var)
					if (netify_type == "cross_sec") {
						edge_matrix <- netlet[, , edge_layer_idx]
					} else if (netify_type == "longit_array" && length(dim(netlet)) == 4) {
						edge_matrix <- netlet_list[[time_id]][, , edge_layer_idx]
					} else if (netify_type == "longit_list") {
						edge_matrix <- netlet_list[[time_id]][, , edge_layer_idx]
					}
				} else {
					cli::cli_warn("Edge variable {edge_var} not found for time {time_id}")
					next
				}

				# convert to binary if requested -- non-zero counts as edge so
				# signed weights are not silently dropped
				if (binary_network) {
					binary_edge_matrix <- ifelse(is.na(edge_matrix), NA_real_,
						as.numeric(edge_matrix != 0))
					dim(binary_edge_matrix) <- dim(edge_matrix)
					dimnames(binary_edge_matrix) <- dimnames(edge_matrix)
					edge_matrix <- binary_edge_matrix
				}

				# process each dyadic variable
				for (dyad_var in dyad_vars) {
					if (!dyad_var %in% names(time_dyad_data)) {
						next
					}

					dyad_matrix <- time_dyad_data[[dyad_var]]

					# require matching dimensions
					if (!identical(dim(dyad_matrix), dim(edge_matrix))) {
						cli::cli_warn("Dimension mismatch between dyadic variable {dyad_var} and edge variable {edge_var} for time {time_id}")
						next
					}

					# calculate correlations
						layer_symmetric <- obj_attrs$symmetric %||% FALSE
						if (length(layer_symmetric) > 1) {
							layer_symmetric <- layer_symmetric[layer_index]
						}
							corr_result <- calculate_dyadic_correlation(
								dyad_matrix, edge_matrix, method, remove_diagonal,
								significance_test, alpha, partial_correlations,
								if (partial_correlations) time_dyad_data else NULL,
								dyad_var, is_symmetric = isTRUE(layer_symmetric),
								is_bipartite = identical(obj_attrs$mode, "bipartite")
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

	# dedupe: when multiple layers and edge_vars are specified, the outer
	# layer loop produces redundant rows because edge_matrix is keyed off
	# edge_var (not layer). collapse to one row per (net, dyad_var, edge_var)
	# and prefer the row where layer == edge_var so the layer label is
	# coherent with the matrix actually correlated.
	if (length(layers) > 1 && nrow(final_results) > 0) {
		final_results$.layer_matches_edge <- final_results$layer == final_results$edge_var
		# order so rows with layer == edge_var come first within each group
		ord <- order(final_results$net, final_results$dyad_var,
			final_results$edge_var, -final_results$.layer_matches_edge)
		final_results <- final_results[ord, , drop = FALSE]
		dup_key <- paste(final_results$net, final_results$dyad_var, final_results$edge_var, sep = "\r")
		final_results <- final_results[!duplicated(dup_key), , drop = FALSE]
		final_results$.layer_matches_edge <- NULL
		rownames(final_results) <- NULL
	}

	return(final_results)
}

# helper function to calculate dyadic correlations
calculate_dyadic_correlation <- function(dyad_matrix, edge_matrix, method,
									 remove_diagonal, significance_test, alpha,
									 partial_correlations, all_dyad_data,
									 current_var, is_symmetric = FALSE,
									 is_bipartite = FALSE) {
	# remove diagonal if requested
	if (remove_diagonal && !isTRUE(is_bipartite)) {
		diag(dyad_matrix) <- NA
		diag(edge_matrix) <- NA
	}

	if (!isTRUE(is_bipartite) && isTRUE(is_symmetric) &&
		nrow(dyad_matrix) == ncol(dyad_matrix)) {
		dyad_matrix[lower.tri(dyad_matrix)] <- NA
		edge_matrix[lower.tri(edge_matrix)] <- NA
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
					remove_diagonal && !isTRUE(is_bipartite), method, alpha,
					significance_test, is_symmetric && !isTRUE(is_bipartite),
					complete_cases
				)
	} else {
		# calculate simple correlation; warn on failure so cor.test errors
		# (zero variance, too few points, etc.) do not silently become na
		cor_result <- tryCatch(
			{
				cor_test <- stats::cor.test(dyad_vec, edge_vec, method = method,
					conf.level = 1 - alpha)
				list(
					correlation = cor_test$estimate,
					p_value = if (significance_test) cor_test$p.value else NA,
					ci_lower = if (significance_test && !is.null(cor_test$conf.int)) cor_test$conf.int[1] else NA,
					ci_upper = if (significance_test && !is.null(cor_test$conf.int)) cor_test$conf.int[2] else NA
				)
			},
			error = function(e) {
				cli::cli_warn(c(
					"!" = "cor.test failed for {.val {current_var}}: {conditionMessage(e)}",
					"i" = "Returning NA. Often caused by zero variance or too few non-NA dyads."
				))
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
										  remove_diagonal, method, alpha,
										  significance_test = TRUE,
										  is_symmetric = FALSE,
										  dyad_complete_cases = NULL) {
	# get other dyadic variables for partial correlation
	other_vars <- setdiff(names(all_dyad_data), current_var)

	if (length(other_vars) == 0) {
		# fall back to simple correlation if no other variables
		cor_test <- stats::cor.test(dyad_vec, edge_vec, method = method,
			conf.level = 1 - alpha)
		return(list(
			correlation = cor_test$estimate,
			p_value = if (significance_test) cor_test$p.value else NA,
			ci_lower = if (significance_test && !is.null(cor_test$conf.int)) cor_test$conf.int[1] else NA,
			ci_upper = if (significance_test && !is.null(cor_test$conf.int)) cor_test$conf.int[2] else NA
		))
	}

	# create data frame with all variables
	control_matrices <- lapply(other_vars, function(var) {
		control_matrix <- all_dyad_data[[var]]
		if (remove_diagonal) {
			diag(control_matrix) <- NA
		}
			if (isTRUE(is_symmetric) && nrow(control_matrix) == ncol(control_matrix)) {
				control_matrix[lower.tri(control_matrix)] <- NA
			}
			control_vec <- as.vector(control_matrix)
			if (!is.null(dyad_complete_cases)) {
				control_vec <- control_vec[dyad_complete_cases]
			}
			control_vec
		})

	# combine all vectors
	all_vectors <- do.call(cbind, c(list(dyad_vec, edge_vec), control_matrices))
	colnames(all_vectors) <- c("target", "outcome", other_vars)

	# remove incomplete cases
	complete_cases <- complete.cases(all_vectors)
	all_vectors <- all_vectors[complete_cases, , drop = FALSE]

	if (nrow(all_vectors) < (ncol(all_vectors) + 5)) { # need enough observations
		return(list(
			correlation = NA,
			p_value = NA,
			ci_lower = NA,
			ci_upper = NA
		))
	}

	# fit residual models for partial correlation
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
			correlation <- suppressWarnings(stats::cor(
				target_residuals, outcome_residuals, method = method
			))
			if (!is.finite(correlation)) {
				return(list(
					correlation = NA,
					p_value = NA,
					ci_lower = NA,
					ci_upper = NA
				))
			}

			p_value <- NA_real_
			ci_lower <- NA_real_
			ci_upper <- NA_real_
			if (significance_test) {
				if (method == "pearson") {
					n_controls <- ncol(all_vectors) - 2L
					df <- length(target_residuals) - n_controls - 2L
					if (df > 0 && abs(correlation) < 1) {
						t_stat <- correlation * sqrt(df / (1 - correlation^2))
						p_value <- 2 * stats::pt(abs(t_stat), df = df, lower.tail = FALSE)
					} else if (df > 0 && abs(correlation) == 1) {
						p_value <- 0
					}
					ci_df <- length(target_residuals) - n_controls - 3L
					if (ci_df > 0 && abs(correlation) < 1) {
						z <- atanh(correlation)
						se <- 1 / sqrt(ci_df)
						crit <- stats::qnorm(1 - alpha / 2)
						ci_lower <- tanh(z - crit * se)
						ci_upper <- tanh(z + crit * se)
					}
				} else {
					cor_test <- stats::cor.test(target_residuals, outcome_residuals,
						method = method, conf.level = 1 - alpha)
					p_value <- cor_test$p.value
					if (!is.null(cor_test$conf.int)) {
						ci_lower <- cor_test$conf.int[1]
						ci_upper <- cor_test$conf.int[2]
					}
				}
			}

			list(
				correlation = correlation,
				p_value = p_value,
				ci_lower = ci_lower,
				ci_upper = ci_upper
			)
		},
		error = function(e) {
			cli::cli_warn(c(
				"!" = "Partial correlation failed for {.val {current_var}}: {conditionMessage(e)}",
				"i" = "Returning NA. Check for collinearity among control dyad vars."
			))
			list(
				correlation = NA,
				p_value = NA,
				ci_lower = NA,
				ci_upper = NA
			)
		}
	)
}
