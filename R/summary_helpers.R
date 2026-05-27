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
	# keep the netify wrapper for igraph conversion, extract plain matrix for math
	netify_obj <- NULL
	if (inherits(mat, "netify")) {
		netify_obj <- mat
		mat <- unclass(mat)
		attributes(mat) <- attributes(mat)[c("dim", "dimnames")]
	}

	# cache dimensions
	n_row <- nrow(mat)
	n_col <- ncol(mat)

	# vectorize matrix operations early
	vec_mat <- c(mat)
	na_mask <- is.na(vec_mat)
	non_na_vec <- vec_mat[!na_mask]

	# binary matrix: any non-zero entry counts as an edge (signed networks too)
	bin_mat <- mat != 0
	bin_mat[is.na(mat)] <- FALSE
	vec_bin_mat <- c(bin_mat)

	# calculate means more efficiently
	row_means <- rowMeans(mat, na.rm = TRUE)
	col_means <- colMeans(mat, na.rm = TRUE)

	# proportion missing uses the same potential-edge denominator as density
	n_na <- sum(na_mask)
	is_symm_for_miss <- if (length(obj_attrs$symmetric) > 1) {
		obj_attrs$symmetric[1]
	} else {
		obj_attrs$symmetric
	}
	if (n_row == n_col) {
		diag_na <- sum(is.na(diag(mat)))
		off_diag_na <- n_na - diag_na
		if (obj_attrs$diag_to_NA) {
			# diagonal excluded from potential edges
			if (isTRUE(is_symm_for_miss)) {
				na_count <- off_diag_na / 2
				poss <- n_row * (n_row - 1) / 2
			} else {
				na_count <- off_diag_na
				poss <- n_row * (n_row - 1)
			}
		} else {
			# self-loops allowed; include diagonal
			if (isTRUE(is_symm_for_miss)) {
				na_count <- off_diag_na / 2 + diag_na
				poss <- n_row * (n_row + 1) / 2
			} else {
				na_count <- n_na
				poss <- n_row * n_row
			}
		}
		prop_edges_missing <- na_count / max(poss, 1)
	} else {
		# bipartite: every cell is a potential edge
		prop_edges_missing <- n_na / length(vec_mat)
	}

	# only meaningful when missing_to_zero = FALSE
	mtz <- isTRUE(obj_attrs$missing_to_zero)
	prop_unknown_edges <- if (mtz) 0 else prop_edges_missing

	# calculate all statistics on non-NA values at once
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

	# graph-level summary measures
	is_symm <- if (length(obj_attrs$symmetric) > 1) obj_attrs$symmetric[1] else obj_attrs$symmetric
	raw_nonzero <- sum(vec_bin_mat)
	# symmetric square: dedupe off-diagonal double-counts, keep diagonal once
	if (is_symm && n_row == n_col) {
		diag_nonzero <- sum(diag(bin_mat))
		off_diag_nonzero <- raw_nonzero - diag_nonzero
		num_edges <- off_diag_nonzero / 2 + diag_nonzero
	} else {
		num_edges <- raw_nonzero
	}
	if (obj_attrs$diag_to_NA && n_row == n_col) {
		max_possible <- if (is_symm) {
			n_row * (n_row - 1) / 2
		} else {
			n_row * (n_row - 1)
		}
		density <- num_edges / max(max_possible, 1)
	} else if (is_symm && n_row == n_col) {
		# symmetric with diag included: off-diagonal density
		max_possible <- n_row * (n_row - 1) / 2
		off_diag_edges <- off_diag_nonzero / 2
		density <- off_diag_edges / max(max_possible, 1)
	} else {
		density <- mean(vec_bin_mat)
	}

	# competition measures - vectorized
	row_sums <- row_means * n_col
	col_sums <- col_means * n_row
	row_props <- row_sums / sum(row_sums)
	col_props <- col_sums / sum(col_sums)
	competition_row <- sum(row_props^2)
	competition_col <- sum(col_props^2)

	# standard deviations
	sd_of_row_means <- sd(row_means, na.rm = TRUE)
	sd_of_col_means <- sd(col_means, na.rm = TRUE)

	# conditional calculations based on network type
	if (obj_attrs$mode == "unipartite" && !is_symm) {
		covar_of_row_col_means <- cor(row_means, col_means, use = "pairwise.complete.obs")
		# correlation-based reciprocity
		reciprocity <- cor(vec_mat, c(t(mat)), use = "pairwise.complete.obs")
		# traditional mutual dyad proportion
		bin_mat <- (mat != 0 & !is.na(mat)) * 1
		diag(bin_mat) <- 0
		mutual_dyads <- sum(bin_mat & t(bin_mat)) / 2
		total_dyads <- sum(bin_mat | t(bin_mat)) / 2
		mutual <- if (total_dyads > 0) mutual_dyads / total_dyads else NA
	} else {
		covar_of_row_col_means <- reciprocity <- mutual <- NA
	}

	# build igraph for transitivity; .quiet_na keeps the NA->0 alert local
	if (!is.null(netify_obj)) {
		g <- netify_to_igraph(netify_obj, .quiet_na = TRUE)
	} else {
		# wrap bare matrix as a temporary netify for conversion
		temp_netify <- mat
		class(temp_netify) <- "netify"
		attr(temp_netify, "netify_type") <- "cross_sec"
		attr(temp_netify, "symmetric") <- is_symm
		attr(temp_netify, "mode") <- obj_attrs$mode
		g <- netify_to_igraph(temp_netify, .quiet_na = TRUE)
	}
	transitivity <- igraph::transitivity(g)

	# build output vector
	out <- c(
		num_row_actors = n_row,
		num_col_actors = n_col,
		density = density,
		num_edges = num_edges,
		prop_edges_missing = prop_edges_missing,
		prop_unknown_edges = prop_unknown_edges,
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
		mutual = mutual,
		transitivity = transitivity
	)

	# user-supplied stats
	if (!is.null(summary_args$other_stats)) {
		# pass the netify when available, else the bare matrix
		stat_input <- if (!is.null(netify_obj)) netify_obj else mat
		other_results <- lapply(
			summary_args$other_stats, function(stat) stat(stat_input)
		)

		other_out <- numeric(0)
		out_names <- character(0)

		for (i in seq_along(other_results)) {
			func_name <- names(summary_args$other_stats)[i]
			result <- other_results[[i]]
			result_length <- length(result)

			other_out <- c(other_out, result)

			if (!is.null(names(result))) {
				if (length(summary_args$other_stats) > 1) {
					out_names <- c(out_names, paste0(func_name, ".", names(result)))
				} else {
					out_names <- c(out_names, names(result))
				}
			} else {
				if (result_length == 1) {
					out_names <- c(out_names, func_name)
				} else {
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

actor_stats_for_netlet <- function(mat, obj_attrs, invert_weights_for_igraph = TRUE,
	other_stats = NULL, fast = FALSE) {
	# cache frequently used values
	is_symmetric <- if (length(obj_attrs$symmetric) > 1) obj_attrs$symmetric[1] else obj_attrs$symmetric
	is_weighted <- !obj_attrs$is_binary
	is_bipartite <- obj_attrs$mode == "bipartite"

	# binary matrix: non-zero means tie (keeps signed networks intact)
	bin_mat <- mat != 0
	bin_mat[is.na(mat)] <- FALSE

	# fast path skips igraph centralities entirely
	g <- NULL
	g_wgts <- NULL
	if (!fast) {
		g <- netify_to_igraph(mat, .quiet_na = TRUE)

		if (is_weighted) {
			g_wgts <- igraph::E(g)$weight
			if (length(g_wgts) > 0) {
				# shift to keep weights positive
				if (any(g_wgts < 0)) {
					min_wgt <- min(g_wgts)
					g_wgts <- g_wgts - min_wgt + 1
				}
				# invert weights when they represent strength rather than distance
				if (invert_weights_for_igraph) {
					g_wgts <- 1 / g_wgts
					tiny <- .Machine$double.eps
					g_wgts[is.infinite(g_wgts) | is.nan(g_wgts)] <- tiny
					g_wgts[g_wgts <= 0] <- tiny
				} else {
					g_wgts[g_wgts <= 0] <- .Machine$double.eps
				}
			}
		}
	}

	# calculate statistics based on network type
	if (is_symmetric) {
		# common calculations for symmetric networks
		degree <- rowSums(bin_mat, na.rm = TRUE)
		prop_ties <- rowMeans(bin_mat, na.rm = TRUE)

		# network share and igraph stats
		network_share <- degree / sum(degree, na.rm = TRUE)

		# fast path skips igraph centralities
		if (fast) {
			closeness <- betweenness <- eigen_vector <- NULL
		} else {
			closeness <- igraph::closeness(g, normalized = TRUE, weights = g_wgts)
			betweenness <- igraph::betweenness(g, normalized = TRUE, weights = g_wgts)
			eigen_vector <- igraph::eigen_centrality(g)$vector
		}

		if (is_weighted) {
			# additional weighted statistics
			strength_sum <- rowSums(mat, na.rm = TRUE)
			strength_avg <- rowMeans(mat, na.rm = TRUE)
			strength_std <- apply(mat, 1, sd, na.rm = TRUE)
			strength_median <- apply(mat, 1, median, na.rm = TRUE)
			network_share <- strength_sum / sum(strength_sum, na.rm = TRUE)

			if (fast) {
				out <- data.frame(
					degree = degree,
					prop_ties = prop_ties,
					strength_sum = strength_sum,
					strength_avg = strength_avg,
					strength_std = strength_std,
					strength_median = strength_median,
					network_share = network_share
				)
			} else {
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
			}
		} else {
			if (fast) {
				out <- data.frame(
					degree = degree,
					prop_ties = prop_ties,
					network_share = network_share
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
		}
	} else {
		# asymmetric networks - calculate base statistics
		degree_out <- rowSums(bin_mat, na.rm = TRUE)
		degree_in <- colSums(bin_mat, na.rm = TRUE)
		prop_ties_out <- rowMeans(bin_mat, na.rm = TRUE)
		prop_ties_in <- colMeans(bin_mat, na.rm = TRUE)

		# totals
		if (!is_bipartite) {
			degree_total <- degree_in + degree_out
			prop_ties_total <- sum(degree_total > 0, na.rm = TRUE) / length(degree_total)
		} else {
			# bipartite: row-then-col order matches igraph vertex order
			degree_total <- c(degree_out, degree_in)
			prop_ties_total <- c(prop_ties_out, prop_ties_in)
		}

		# igraph centralities, skipped on fast path
		if (fast) {
			closeness_in <- closeness_out <- closeness_all <- NULL
			betweenness <- authority_score <- hub_score <- NULL
		} else {
			closeness_in <- igraph::closeness(g, mode = "in", normalized = TRUE, weights = g_wgts)
			closeness_out <- igraph::closeness(g, mode = "out", normalized = TRUE, weights = g_wgts)
			closeness_all <- igraph::closeness(g, mode = "all", normalized = TRUE, weights = g_wgts)
			betweenness <- igraph::betweenness(g, normalized = TRUE, weights = g_wgts)
			hits <- igraph::hits_scores(g)
			authority_score <- hits$authority
			hub_score <- hits$hub
		}

		if (is_weighted) {
			# additional weighted statistics for asymmetric networks
			strength_sum_out <- rowSums(mat, na.rm = TRUE)
			strength_sum_in <- colSums(mat, na.rm = TRUE)
			strength_avg_out <- rowMeans(mat, na.rm = TRUE)
			strength_avg_in <- colMeans(mat, na.rm = TRUE)
			strength_median_out <- apply(mat, 1, median, na.rm = TRUE)
			strength_median_in <- apply(mat, 2, median, na.rm = TRUE)
			strength_std_out <- apply(mat, 1, sd, na.rm = TRUE)
			strength_std_in <- apply(mat, 2, sd, na.rm = TRUE)

			# calculate totals and network shares
			if (!is_bipartite) {
				strength_sum_total <- strength_sum_in + strength_sum_out
				strength_avg_total <- (strength_avg_in + strength_avg_out) / 2
				strength_median_total <- (strength_median_in + strength_median_out) / 2
				strength_std_total <- (strength_std_in + strength_std_out) / 2
			} else {
				# bipartite: row-then-col order matches igraph vertex order
				strength_sum_total <- c(strength_sum_out, strength_sum_in)
				strength_avg_total <- c(strength_avg_out, strength_avg_in)
				strength_median_total <- c(strength_median_out, strength_median_in)
				strength_std_total <- c(strength_std_out, strength_std_in)
			}

			network_share_in <- strength_sum_in / sum(strength_sum_in, na.rm = TRUE)
			network_share_out <- strength_sum_out / sum(strength_sum_out, na.rm = TRUE)
			network_share_total <- strength_sum_total / sum(strength_sum_total, na.rm = TRUE)
		} else {
			# binary network shares
			network_share_in <- degree_in / sum(degree_in, na.rm = TRUE)
			network_share_out <- degree_out / sum(degree_out, na.rm = TRUE)
			network_share_total <- degree_total / sum(degree_total, na.rm = TRUE)
		}

		# build output data frame based on type
		if (is_bipartite) {
			# preserve row/col actor names even when ids overlap
			bipart_actors <- c(names(degree_out), names(degree_in))
			if (anyDuplicated(bipart_actors) > 0) {
				bipart_actors <- c(
					paste0("row:", names(degree_out)),
					paste0("col:", names(degree_in))
				)
			}

			if (fast) {
				base_cols <- data.frame(
					degree_total = degree_total,
					prop_ties_total = prop_ties_total,
					network_share_total = network_share_total,
					row.names = bipart_actors,
					check.names = FALSE
				)
			} else {
				base_cols <- data.frame(
					degree_total = degree_total,
					prop_ties_total = prop_ties_total,
					network_share_total = network_share_total,
					closeness_all = closeness_all,
					betweenness = betweenness,
					authority_score = authority_score,
					hub_score = hub_score,
					row.names = bipart_actors,
					check.names = FALSE
				)
			}

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
			# non-bipartite output
			if (fast) {
				base_cols <- data.frame(
					degree_in = degree_in,
					degree_out = degree_out,
					degree_total = degree_total,
					prop_ties_in = prop_ties_in,
					prop_ties_out = prop_ties_out,
					prop_ties_total = prop_ties_total,
					network_share_in = network_share_in,
					network_share_out = network_share_out,
					network_share_total = network_share_total
				)
			} else {
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
			}

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

	# add user-supplied stats efficiently
	if (!is.null(other_stats)) {
		other_results <- lapply(other_stats, function(stat) stat(mat))
		out <- cbind(out, as.data.frame(other_results))
	}

	return(out)
}
