# helper functions for compare_networks
# this file contains internal functions used by compare_networks()

#' compare edge patterns between networks
#'
#' `compare_edges` performs pairwise comparisons of edge patterns between networks using multiple similarity metrics including correlation, jaccard similarity, hamming distance, and qap tests.
#'
#' @param nets_list a list of netify objects to compare.
#' @param method character string specifying comparison method(s): "correlation", "jaccard", "hamming", "qap", or "all".
#' @param test logical; whether to perform significance testing using qap.
#' @param n_permutations integer; number of permutations for qap test.
#' @param include_diagonal logical; whether to include diagonal values in comparison.
#' @param edge_threshold numeric or function; threshold for determining edge presence in weighted networks.
#' @param return_details logical; whether to return detailed comparison matrices.
#' @param spectral_rank integer; number of eigenvalues to use for spectral distance (0 = all).
#'
#' @return a list containing comparison results including summary statistics, edge changes, and optionally detailed matrices.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @importFrom stats var p.adjust
#' @keywords internal
#' @noRd

align_bipartite_matrices <- function(mats) {
	normalise_dimnames <- function(mat, idx) {
		if (is.null(rownames(mat))) {
			rownames(mat) <- paste0("r", seq_len(nrow(mat)))
		}
		if (is.null(colnames(mat))) {
			colnames(mat) <- paste0("c", seq_len(ncol(mat)))
		}
		mat
	}
	mats <- Map(normalise_dimnames, mats, seq_along(mats))
	row_actors <- sort(unique(unlist(lapply(mats, rownames), use.names = FALSE)))
	col_actors <- sort(unique(unlist(lapply(mats, colnames), use.names = FALSE)))

	lapply(mats, function(mat) {
		aligned <- matrix(0,
			nrow = length(row_actors),
			ncol = length(col_actors),
			dimnames = list(row_actors, col_actors)
		)
		aligned[rownames(mat), colnames(mat)] <- mat
		aligned
	})
}

compare_edges <- function(
	nets_list, method, test, n_permutations,
	include_diagonal, edge_threshold, return_details,
	permutation_type = "classic",
	correlation_type = "pearson",
	binary_metric = "phi",
	p_adjust = "none",
	adaptive_stop = FALSE,
	alpha = 0.05,
	max_permutations = 20000,
	seed_used = NULL,
	spectral_rank = 0,
	other_stats = NULL) {
	
	# handle adaptive stopping warning
	if (adaptive_stop) {
		warning("Adaptive stopping is not yet implemented. Using fixed n_permutations.")
		adaptive_stop <- FALSE
	}
	qap_requested <- method %in% c("qap", "all") && isTRUE(test)
	if (qap_requested && correlation_type != "pearson") {
		cli::cli_abort(c(
			"x" = "QAP significance testing currently uses Pearson edge correlation.",
			"i" = "Use {.code correlation_type = 'pearson'} with {.code method = 'qap'} or {.code method = 'all'}."
		))
	}
	
	# check binary requirement for degree_preserving
	if (permutation_type == "degree_preserving") {
		# check first network is binary
		mat1 <- extract_matrix(nets_list[[1]])
		vec1 <- as.vector(mat1)
		vec1 <- vec1[!is.na(vec1)]
		if (!all(vec1 %in% c(0, 1))) {
			stop("Degree-preserving permutation requires the first network to be binary (containing only 0/1 values). ",
				 "The first network in your list contains non-binary values. ",
				 "Please ensure the first network is binary or use a different permutation_type.")
		}
	}
	# grab network names if they exist
	net_names <- names(nets_list)
	if (is.null(net_names)) {
		net_names <- paste0("net", seq_along(nets_list))
	}

	# set up comparison matrices for all the metrics
	n_nets <- length(nets_list)
	correlation_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
	jaccard_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
	hamming_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
	qap_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
	qap_pval_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
	qap_valid_perm_mat <- matrix(NA_integer_, n_nets, n_nets, dimnames = list(net_names, net_names))
	spectral_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))

	# track edge changes between networks
	edge_changes <- list()
	
	# helper switches for correlation functions
	cor_fun <- switch(correlation_type,
					  pearson = stats::cor,
					  spearman = function(x, y) stats::cor(x, y, method = "spearman"))
	
	bin_cor <- switch(binary_metric,
					  phi = function(a, b) {
						  # handle constant vectors
						  if (length(unique(a)) == 1 && length(unique(b)) == 1) {
							  # both constant - if same value then 1, else 0
							  return(ifelse(a[1] == b[1], 1, 0))
						  } else if (length(unique(a)) == 1 || length(unique(b)) == 1) {
							  # one constant - correlation is 0
							  return(0)
						  }
						  stats::cor(a, b)
					  },
					  simple_matching = function(a, b) mean(a == b),
					  mean_centered = function(a, b) {
						  a <- a - mean(a); b <- b - mean(b)
						  # handle case where centering makes them all 0
						  if (all(a == 0) || all(b == 0)) return(0)
						  stats::cor(a, b)
					  })
	
	qap_fun <- switch(permutation_type,
					  classic = qap_correlation_cpp,
					  degree_preserving = function(A, B, ...) qap_degree_cpp(A, B, ..., swaps_factor = 10),
					  stop("Unknown permutation_type"))

	# always pre-align all matrices to avoid redundant work in the loop
	# use one pass per network pair
	mats <- lapply(nets_list, extract_matrix)
	if (any(vapply(mats, function(mat) any(!is.na(mat) & !is.finite(mat)), logical(1)))) {
		cli::cli_abort("{.fn compare_networks} requires finite edge weights or {.val NA}.")
	}
	net_modes <- vapply(nets_list, function(net) attr(net, "mode") %||% "unipartite", character(1))
	if (any(net_modes == "bipartite")) {
		if (!all(net_modes == "bipartite")) {
			cli::cli_abort("Edge comparison requires all inputs to share the same {.arg mode}.")
		}
		if (method %in% c("qap", "all") && isTRUE(test)) {
			cli::cli_abort(c(
				"x" = "QAP edge comparison is not implemented for bipartite networks.",
				"i" = "Use {.code method = 'correlation'}, {.code 'jaccard'}, or {.code 'hamming'} for bipartite edge comparisons."
			))
		}
		if (method %in% c("spectral", "all")) {
			cli::cli_abort(c(
				"x" = "Spectral edge comparison requires square unipartite matrices.",
				"i" = "Use {.code method = 'correlation'}, {.code 'jaccard'}, or {.code 'hamming'} for bipartite edge comparisons."
			))
		}
		aligned_list <- align_bipartite_matrices(mats)
	} else {
		# pre-compute all actors for square unipartite alignment
		all_actors <- get_all_actors(nets_list)
		aligned_list <- batch_align_matrices_cpp(mats, all_actors, include_diagonal)
	}
	
	# calculate custom statistics if provided
	custom_stats_list <- NULL
	if (!is.null(other_stats)) {
		custom_stats_list <- lapply(seq_along(mats), function(i) {
			mat <- mats[[i]]
			stats_result <- list()
			
			# apply each custom function
			for (stat_name in names(other_stats)) {
				# call the custom function with the matrix
				custom_result <- other_stats[[stat_name]](mat)

				# require named results
				if (is.null(names(custom_result))) {
					names(custom_result) <- paste0(stat_name, "_", seq_along(custom_result))
				} else {
					# prefix names with stat name if not already
					if (!all(grepl(paste0("^", stat_name), names(custom_result)))) {
						names(custom_result) <- paste0(stat_name, "_", names(custom_result))
					}
				}

				stats_result[[stat_name]] <- custom_result
			}
			
			unlist(stats_result)
		})
		names(custom_stats_list) <- net_names
	}

	# compare all pairs of networks
	for (i in 1:(n_nets - 1)) {
		for (j in (i + 1):n_nets) {
			# check if networks have different loop settings
			loops1 <- attributes(nets_list[[i]])$loops %||% FALSE
			loops2 <- attributes(nets_list[[j]])$loops %||% FALSE

			# include the diagonal when the two networks disagree on loops
			force_include_diagonal <- include_diagonal || (loops1 != loops2)

			# pull the pre-aligned matrices
			mat1 <- aligned_list[[i]]
			mat2 <- aligned_list[[j]]

			# re-align when the diagonal policy just shifted
			if (force_include_diagonal && !include_diagonal) {
				# re-align with diagonal included
				mats <- align_matrices(nets_list[[i]], nets_list[[j]],
					include_diagonal = force_include_diagonal
				)
				mat1 <- mats$mat1
				mat2 <- mats$mat2
			}

			# figure out edge thresholds
			if (is.function(edge_threshold)) {
				threshold1 <- edge_threshold(mat1)
				threshold2 <- edge_threshold(mat2)
			} else {
				threshold1 <- threshold2 <- edge_threshold
			}

			# calculate similarity metrics based on method
			if (method %in% c("correlation", "all")) {
				# flatten matrices and calc correlation
				vec1 <- as.vector(mat1)
				vec2 <- as.vector(mat2)
				complete <- !is.na(vec1) & !is.na(vec2) &
					is.finite(vec1) & is.finite(vec2)
				if (sum(complete) >= 3) {
					# check if both are binary
					is_binary1 <- all(vec1[complete] %in% c(0, 1))
					is_binary2 <- all(vec2[complete] %in% c(0, 1))
					
					if (is_binary1 && is_binary2) {
						# use binary-specific correlation
						correlation_mat[i, j] <- correlation_mat[j, i] <- 
							bin_cor(vec1[complete], vec2[complete])
					} else {
						# use standard correlation
						# check for constant vectors (e.g., all zeros or all ones)
						var1 <- var(vec1[complete])
						var2 <- var(vec2[complete])

						if (var1 > 0 && var2 > 0) {
							correlation_mat[i, j] <- correlation_mat[j, i] <-
								cor_fun(vec1[complete], vec2[complete])
						} else if (var1 == 0 && var2 == 0) {
							# both constant - if same value then perfect correlation, else 0
							if (all(vec1[complete] == vec1[complete][1]) &&
								all(vec2[complete] == vec2[complete][1]) &&
								vec1[complete][1] == vec2[complete][1]) {
								correlation_mat[i, j] <- correlation_mat[j, i] <- 1
							} else {
								correlation_mat[i, j] <- correlation_mat[j, i] <- 0
							}
						} else {
							# one constant, one varying - correlation is 0
							correlation_mat[i, j] <- correlation_mat[j, i] <- 0
						}
					}
				} else {
					# not enough data points - set to 0 for empty networks
					if (sum(vec1 != 0, na.rm = TRUE) == 0 || sum(vec2 != 0, na.rm = TRUE) == 0) {
						correlation_mat[i, j] <- correlation_mat[j, i] <- 0
					}
				}
			}

			if (method %in% c("jaccard", "all")) {
				jaccard_mat[i, j] <- jaccard_mat[j, i] <-
					calculate_jaccard_cpp(mat1, mat2, threshold1, threshold2)
			}

			if (method %in% c("hamming", "all")) {
				hamming_mat[i, j] <- hamming_mat[j, i] <-
					calculate_hamming_cpp(mat1, mat2, threshold1, threshold2)
			}

			if (method %in% c("qap", "all") && test) {
				qap_result <- qap_fun(mat1, mat2, n_permutations,
								  seed = if(is.null(seed_used)) -1 else seed_used)
				qap_mat[i, j] <- qap_mat[j, i] <- qap_result$correlation
				qap_pval_mat[i, j] <- qap_pval_mat[j, i] <- qap_result$p_value
				n_valid <- if ("n_valid_permutations" %in% names(qap_result)) {
					qap_result$n_valid_permutations
				} else {
					n_permutations
				}
				qap_valid_perm_mat[i, j] <- qap_valid_perm_mat[j, i] <- as.integer(n_valid)
			}

			if (method %in% c("spectral", "all")) {
				spectral_mat[i, j] <- spectral_mat[j, i] <-
					calculate_spectral_distance_cpp(mat1, mat2, spectral_rank)
			}

			# track edge changes for this pair
			edge_key <- paste(net_names[i], net_names[j], sep = "_vs_")
			edge_changes[[edge_key]] <- calculate_edge_changes_cpp(mat1, mat2, threshold1, threshold2)
		}
	}

	# fill in diagonals
	diag(correlation_mat) <- 1
	diag(jaccard_mat) <- 1
	diag(hamming_mat) <- 0
	diag(spectral_mat) <- 0
	if (test) {
		diag(qap_mat) <- 1
		diag(qap_pval_mat) <- NA_real_
		
		# store raw p-values before adjustment
		qap_pval_mat_raw <- qap_pval_mat
		
		# adjust qap p-values if requested
		if (p_adjust != "none" && n_nets > 2) {
			flat <- qap_pval_mat[lower.tri(qap_pval_mat)]
			adjusted <- p.adjust(flat, method = p_adjust)
			qap_pval_mat[lower.tri(qap_pval_mat)] <- adjusted
			qap_pval_mat[upper.tri(qap_pval_mat)] <- t(qap_pval_mat)[upper.tri(qap_pval_mat)]
		}
	}

	# create summary dataframe
	summary_df <- create_edge_summary(
		correlation_mat, jaccard_mat, hamming_mat,
		qap_mat, qap_pval_mat, spectral_mat, method
	)

	# build results list - make sure n_networks is included!
	results <- list(
		# comparison_type = "edges",
		method = method,
		n_networks = n_nets,
		summary = summary_df,
		edge_changes = edge_changes,
		permutation_type = permutation_type,
		correlation_type = correlation_type,
		binary_metric = binary_metric,
		p_adjust = p_adjust,
		seed_used = seed_used,
		spectral_rank = spectral_rank
	)
	
	# add custom statistics if calculated
	if (!is.null(custom_stats_list)) {
		# convert to data frame for easier viewing
		custom_stats_df <- do.call(rbind, lapply(custom_stats_list, function(x) {
			if (is.null(x)) return(NULL)
			as.data.frame(t(x), stringsAsFactors = FALSE)
		}))
		if (!is.null(custom_stats_df) && nrow(custom_stats_df) > 0) {
			custom_stats_df$network <- names(custom_stats_list)
			# reorder columns to put network first
			custom_stats_df <- custom_stats_df[, c("network", setdiff(names(custom_stats_df), "network"))]
			results$custom_stats <- custom_stats_df
		}
	}

	# add significance tests if requested and qap was actually performed
	if (test && method %in% c("qap", "all")) {
		# add n_permutations as attribute to qap_pvalues
		attr(qap_pval_mat, "n_perm") <- n_permutations
		attr(qap_valid_perm_mat, "n_perm") <- n_permutations
		valid_counts <- qap_valid_perm_mat[lower.tri(qap_valid_perm_mat)]
		if (any(!is.na(valid_counts) & valid_counts < n_permutations)) {
			cli::cli_warn(c(
				"!" = "Some QAP permutations were not usable after missing-dyad filtering.",
				"i" = "Inspect {.field significance_tests$qap_valid_permutations} for effective permutation counts."
			))
		}
		
		results$significance_tests <- list(
			qap_correlations = qap_mat,
			qap_pvalues = qap_pval_mat,
			qap_valid_permutations = qap_valid_perm_mat
		)
		
		# store raw p-values if adjustment was applied
		if (p_adjust != "none" && n_nets > 2) {
			attr(qap_pval_mat_raw, "n_perm") <- n_permutations
			results$significance_tests$qap_pvalues_raw = qap_pval_mat_raw
		}
	}

	# add detailed matrices if user wants them
	if (return_details) {
		# return named full matrices
		results$details <- list(
			correlation_matrix = correlation_mat,
			jaccard_matrix = jaccard_mat,
			hamming_matrix = hamming_mat,
			spectral_matrix = spectral_mat
		)
	}

	return(results)
}

#' compare structural properties between networks
#'
#' `compare_structure` calculates and compares network-level structural properties such as density, reciprocity, transitivity, and mean degree across networks.
#'
#' @param nets_list a list of netify objects to compare.
#' @param test logical; whether to perform significance testing (currently not implemented).
#'
#' @return a list containing structural properties for each network and percent changes if comparing two networks.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

compare_structure <- function(nets_list, test, other_stats = NULL) {
	# get network names
	net_names <- names(nets_list)
	if (is.null(net_names)) {
		net_names <- paste0("net", seq_along(nets_list))
	}

	# calc structural properties for each network using summary()
	struct_props <- lapply(seq_along(nets_list), function(i) {
		net <- nets_list[[i]]

		# use netify's built-in summary function to get all stats
		net_summary <- summary(net, other_stats = other_stats)
		
		# for cross-sectional networks, summary returns a single row
		# for longitudinal networks, it might return multiple rows
		if (nrow(net_summary) == 1) {
			# simple case - cross-sectional network
			props <- net_summary
			props$network <- net_names[i]
			# remove 'net' column if it exists (from summary output)
			if ("net" %in% names(props)) {
				props$net <- NULL
			}
		} else {
			# longitudinal network - aggregate or warn
			cli::cli_warn("Network {net_names[i]} appears to be longitudinal. Using first time period for structural comparison.")
			props <- net_summary[1, , drop = FALSE]
			props$network <- net_names[i]
			if ("net" %in% names(props)) {
				props$net <- NULL
			}
		}
		
		# add mean_degree since summary.netify doesn't include it
		# extract matrix to calculate mean degree
		mat <- extract_matrix(net)
		n_actors <- props$num_actors %||% props$num_row_actors %||% nrow(mat)
		n_edges <- props$num_edges
		
		# calculate mean degree based on network type
		is_directed <- !all(attributes(net)$symmetric %||% FALSE)
		is_bipartite <- attributes(net)$mode == "bipartite"
		
		if (!is_bipartite) {
			if (is_directed) {
				props$mean_degree <- n_edges / n_actors
			} else {
				# for undirected, edges are counted once but each contributes to 2 degrees
				props$mean_degree <- (2 * n_edges) / n_actors
			}
		}
		
		# keep network name first
		col_order <- c("network", setdiff(names(props), "network"))
		props <- props[, col_order, drop = FALSE]
		
		props
	})

	# combine into single dataframe - pad columns for mixed directedness
	all_struct_cols <- unique(unlist(lapply(struct_props, names)))
	struct_props <- lapply(struct_props, function(df) {
		missing_cols <- setdiff(all_struct_cols, names(df))
		if (length(missing_cols) > 0) df[missing_cols] <- NA
		df[all_struct_cols]
	})
	struct_df <- do.call(rbind, struct_props)
	
	# if only 2 networks, calculate changes between them
	if (length(nets_list) == 2) {
		props1 <- struct_props[[1]]
		props2 <- struct_props[[2]]

		# get all numeric columns (excluding 'network' and 'layer' if present)
		exclude_cols <- c("network", "layer")
		all_cols <- setdiff(names(props1), exclude_cols)
		
		# filter to numeric columns that exist in both dataframes
		numeric_cols <- character()
		for (col in all_cols) {
			if (col %in% names(props2) && 
				is.numeric(props1[[col]]) && 
				is.numeric(props2[[col]])) {
				numeric_cols <- c(numeric_cols, col)
			}
		}
		
		if (length(numeric_cols) > 0) {
			changes <- data.frame(
				metric = numeric_cols,
				value_net1 = unlist(props1[numeric_cols]),
				value_net2 = unlist(props2[numeric_cols]),
				stringsAsFactors = FALSE
			)
			changes$absolute_change <- changes$value_net2 - changes$value_net1
			changes$percent_change <- ifelse(changes$value_net1 != 0 & !is.na(changes$value_net1),
				100 * changes$absolute_change / abs(changes$value_net1),
				NA
			)
			
			struct_df <- list(
				properties = struct_df,
				changes = changes
			)
		}
	}

	# build results
	results <- list(
		# comparison_type = "structure",
		method = "structural_comparison",
		n_networks = length(nets_list),
		# if struct_df is a list (with properties and changes), use properties
		summary = if (is.list(struct_df) && "properties" %in% names(struct_df)) {
			struct_df$properties
		} else {
			struct_df
		}
	)

	# if we have changes data (2 networks), add it separately
	if (is.list(struct_df) && "changes" %in% names(struct_df)) {
		results$changes <- struct_df$changes
	}

	return(results)
}

#' compare node composition between networks
#'
#' `compare_nodes` tracks which actors are present in each network and calculates node overlap statistics including jaccard similarity of node sets.
#'
#' @param nets_list a list of netify objects to compare.
#' @param return_details logical; whether to return detailed node sets and overlap matrices.
#'
#' @return a list containing node composition summary and changes between networks.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

compare_nodes <- function(nets_list, return_details = FALSE, other_stats = NULL) {
	# get network names
	net_names <- names(nets_list)
	if (is.null(net_names)) {
		net_names <- paste0("net", seq_along(nets_list))
	}

	# extract node sets from each network
	node_sets <- lapply(nets_list, function(net) {
		mat <- extract_matrix(net)
		unique(c(rownames(mat), colnames(mat)))
	})

	# set up matrices to track node overlap
	n_nets <- length(nets_list)
	overlap_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
	jaccard_node_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))

	# track node changes
	node_changes <- list()

	# compare all pairs
	for (i in 1:(n_nets - 1)) {
		for (j in (i + 1):n_nets) {
			nodes1 <- node_sets[[i]]
			nodes2 <- node_sets[[j]]

			# calc overlap
			common <- length(intersect(nodes1, nodes2))
			total <- length(union(nodes1, nodes2))

			overlap_mat[i, j] <- overlap_mat[j, i] <- common
			jaccard_node_mat[i, j] <- jaccard_node_mat[j, i] <-
				if (total > 0) common / total else 0

			# track edge changes
			node_key <- paste(net_names[i], net_names[j], sep = "_vs_")
			node_changes[[node_key]] <- list(
				added = setdiff(nodes2, nodes1),
				removed = setdiff(nodes1, nodes2),
				maintained = intersect(nodes1, nodes2),
				n_added = length(setdiff(nodes2, nodes1)),
				n_removed = length(setdiff(nodes1, nodes2)),
				n_maintained = common
			)
		}
	}

	# fill diagonals
	diag(overlap_mat) <- sapply(node_sets, length)
	diag(jaccard_node_mat) <- 1
	
	# calculate custom statistics if provided
	custom_stats_list <- NULL
	if (!is.null(other_stats)) {
		custom_stats_list <- lapply(seq_along(nets_list), function(i) {
			net <- nets_list[[i]]
			stats_result <- list()
			
			# apply each custom function
			for (stat_name in names(other_stats)) {
				# call the custom function with the netify object
				custom_result <- other_stats[[stat_name]](net)

				# require named results
				if (is.null(names(custom_result))) {
					names(custom_result) <- paste0(stat_name, "_", seq_along(custom_result))
				} else {
					# prefix names with stat name if not already
					if (!all(grepl(paste0("^", stat_name), names(custom_result)))) {
						names(custom_result) <- paste0(stat_name, "_", names(custom_result))
					}
				}

				stats_result[[stat_name]] <- custom_result
			}
			
			unlist(stats_result)
		})
		names(custom_stats_list) <- net_names
	}

	# create summary
	if (n_nets == 2) {
		summary_df <- data.frame(
			comparison = paste(net_names[1], "vs", net_names[2]),
			nodes_net1 = length(node_sets[[1]]),
			nodes_net2 = length(node_sets[[2]]),
			common_nodes = overlap_mat[1, 2],
			jaccard_similarity = jaccard_node_mat[1, 2],
			nodes_added = node_changes[[1]]$n_added,
			nodes_removed = node_changes[[1]]$n_removed,
			stringsAsFactors = FALSE
		)
	} else {
		# summary for multiple networks
		# calculate mean jaccard excluding self-comparisons
		mean_jaccard_vec <- numeric(n_nets)
		for (i in 1:n_nets) {
			# get jaccard values for network i, excluding diagonal
			jaccard_values <- jaccard_node_mat[i, -i]
			mean_jaccard_vec[i] <- mean(jaccard_values, na.rm = TRUE)
		}
		
		summary_df <- data.frame(
			network = net_names,
			n_nodes = sapply(node_sets, length),
			mean_overlap = rowMeans(overlap_mat, na.rm = TRUE),
			mean_jaccard = mean_jaccard_vec,
			stringsAsFactors = FALSE
		)
	}

	results <- list(
		# comparison_type = "nodes",
		method = "node_composition",
		n_networks = n_nets,
		summary = summary_df,
		node_changes = node_changes
	)
	
	# add custom statistics if calculated
	if (!is.null(custom_stats_list)) {
		# convert to data frame for easier viewing
		custom_stats_df <- do.call(rbind, lapply(custom_stats_list, function(x) {
			if (is.null(x)) return(NULL)
			as.data.frame(t(x), stringsAsFactors = FALSE)
		}))
		if (!is.null(custom_stats_df) && nrow(custom_stats_df) > 0) {
			custom_stats_df$network <- names(custom_stats_list)
			# reorder columns to put network first
			custom_stats_df <- custom_stats_df[, c("network", setdiff(names(custom_stats_df), "network"))]
			results$custom_stats <- custom_stats_df
		}
	}

	if (return_details) {
		results$details <- list(
			overlap_matrix = overlap_mat,
			jaccard_matrix = jaccard_node_mat,
			node_sets = node_sets
		)
	}

	return(results)
}

#' compare nodal attribute distributions between networks
#'
#' `compare_attributes` compares the distributions of nodal attributes across networks using correlation for continuous attributes and total variation distance for categorical attributes.
#'
#' @param nets_list a list of netify objects to compare.
#' @param test logical; whether to perform ks tests for continuous attributes.
#' @param n_permutations integer; number of permutations for significance testing (currently unused).
#' @param return_details logical; whether to return detailed attribute values and test matrices.
#' @param attr_metric character string specifying method for continuous attribute comparison.
#'
#' @return a list containing attribute similarity comparisons across networks.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

compare_attributes <- function(
	nets_list, test = TRUE, n_permutations = 1000,
	return_details = FALSE, attr_metric = "ecdf_cor", other_stats = NULL) {
	# get network names
	net_names <- names(nets_list)
	if (is.null(net_names)) {
		net_names <- paste0("net", seq_along(nets_list))
	}

	# extract nodal attributes from each network
	attrs_list <- lapply(nets_list, function(net) {
		attrs <- attributes(net)
		attrs$nodal_data
	})

	# find common attributes across networks
	all_attr_names <- unique(unlist(lapply(attrs_list, names)))
	common_attrs <- setdiff(all_attr_names, c("actor", "time", "layer"))

	if (length(common_attrs) == 0) {
		cli::cli_alert_warning("No common nodal attributes found across networks")
		return(list(
			# comparison_type = "attributes",
			method = "attribute_comparison",
			n_networks = length(nets_list),
			summary = data.frame(message = "No common attributes to compare")
		))
	}

	# compare networks based on each attribute
	attr_comparisons <- list()

	for (attr in common_attrs) {
		attr_result <- compare_single_attribute(
			nets_list, attrs_list, attr,
			test, n_permutations, attr_metric
		)
		attr_comparisons[[attr]] <- attr_result
	}

	# compile summary; rbind with column union so categorical
	# (no ks_pvalue) and numeric (with ks_pvalue) attribute rows stack
	summary_list <- lapply(names(attr_comparisons), function(attr) {
		comp <- attr_comparisons[[attr]]
		if (!is.null(comp$summary)) {
			cbind(attribute = attr, comp$summary)
		} else {
			NULL
		}
	})
	summary_list <- summary_list[!vapply(summary_list, is.null, logical(1))]
	if (length(summary_list) == 0L) {
		summary_df <- NULL
	} else {
		all_cols <- unique(unlist(lapply(summary_list, names)))
		summary_padded <- lapply(summary_list, function(df) {
			missing_c <- setdiff(all_cols, names(df))
			for (m in missing_c) df[[m]] <- NA
			df[, all_cols, drop = FALSE]
		})
		summary_df <- do.call(rbind, c(summary_padded, list(make.row.names = FALSE)))
	}
	
	# calculate custom statistics if provided
	custom_stats_list <- NULL
	if (!is.null(other_stats)) {
		custom_stats_list <- lapply(seq_along(nets_list), function(i) {
			net <- nets_list[[i]]
			stats_result <- list()
			
			# apply each custom function
			for (stat_name in names(other_stats)) {
				# call the custom function with the netify object
				custom_result <- other_stats[[stat_name]](net)

				# require named results
				if (is.null(names(custom_result))) {
					names(custom_result) <- paste0(stat_name, "_", seq_along(custom_result))
				} else {
					# prefix names with stat name if not already
					if (!all(grepl(paste0("^", stat_name), names(custom_result)))) {
						names(custom_result) <- paste0(stat_name, "_", names(custom_result))
					}
				}

				stats_result[[stat_name]] <- custom_result
			}
			
			unlist(stats_result)
		})
		names(custom_stats_list) <- net_names
	}

	results <- list(
		# comparison_type = "attributes",
		method = "attribute_comparison",
		n_networks = length(nets_list),
		summary = summary_df,
		by_attribute = attr_comparisons
	)
	
	# add custom statistics if calculated
	if (!is.null(custom_stats_list)) {
		# convert to data frame for easier viewing
		custom_stats_df <- do.call(rbind, lapply(custom_stats_list, function(x) {
			if (is.null(x)) return(NULL)
			as.data.frame(t(x), stringsAsFactors = FALSE)
		}))
		if (!is.null(custom_stats_df) && nrow(custom_stats_df) > 0) {
			custom_stats_df$network <- names(custom_stats_list)
			# reorder columns to put network first
			custom_stats_df <- custom_stats_df[, c("network", setdiff(names(custom_stats_df), "network"))]
			results$custom_stats <- custom_stats_df
		}
	}

	if (return_details) {
		results$details <- lapply(attr_comparisons, function(x) x$details)
	}

	return(results)
}

#' extract network list from netify object
#'
#' `extract_network_list` converts different netify object types (cross-sectional, longitudinal array, or longitudinal list) into a standardized list format for comparison.
#'
#' @param net a netify object of any type.
#'
#' @return a list of netify objects, one for each time period or layer.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

extract_network_list <- function(net) {
	#
	attrs <- attributes(net)

	# check if this is a multilayer network
	has_layers <- !is.null(attrs$layers)

	if (attrs$netify_type == "longit_list") {
		# for longitudinal list, check if it's multilayer
		if (has_layers && length(dim(net[[1]])) == 3) {
			# multilayer longitudinal list - extract layers from first time period
			layer_names <- attrs$layers
			n_layers <- length(layer_names)

			# extract each layer across all time periods
			layer_list <- vector("list", n_layers)
			names(layer_list) <- layer_names

			for (l in seq_len(n_layers)) {
				# create longitudinal list for this layer
				time_list <- vector("list", length(net))
				names(time_list) <- names(net)

				for (t in seq_along(net)) {
					# extract layer l from time t
					layer_slice <- net[[t]][, , l]

					# preserve attributes
					attr(layer_slice, "netify_type") <- "cross_sec"
					attr(layer_slice, "symmetric") <- if (length(attrs$symmetric) > 1) attrs$symmetric[l] else attrs$symmetric
					attr(layer_slice, "mode") <- attrs$mode
					attr(layer_slice, "layers") <- layer_names[l]

					# handle weight attributes - might be layer-specific
					if (is.character(attrs[["weight"]]) && length(attrs[["weight"]]) > 1) {
						attr(layer_slice, "weight") <- attrs[["weight"]][l]
						attr(layer_slice, "detail_weight") <- paste("Weights from `", attrs[["weight"]][l], "`", sep = "")
					} else {
						attr(layer_slice, "weight") <- attrs[["weight"]]
						attr(layer_slice, "detail_weight") <- attrs$detail_weight
					}

					# handle is_binary which might be a vector
					if (length(attrs[["is_binary"]]) > 1) {
						attr(layer_slice, "is_binary") <- attrs[["is_binary"]][l]
					} else {
						attr(layer_slice, "is_binary") <- attrs[["is_binary"]]
					}

					# handle other attributes that might be vectors for multilayer
					if (length(attrs$diag_to_NA) > 1) {
						attr(layer_slice, "diag_to_NA") <- attrs$diag_to_NA[l]
					} else {
						attr(layer_slice, "diag_to_NA") <- attrs$diag_to_NA %||% TRUE
					}
					if (length(attrs$missing_to_zero) > 1) {
						attr(layer_slice, "missing_to_zero") <- attrs$missing_to_zero[l]
					} else {
						attr(layer_slice, "missing_to_zero") <- attrs$missing_to_zero %||% TRUE
					}
					if (length(attrs$sum_dyads) > 1) {
						attr(layer_slice, "sum_dyads") <- attrs$sum_dyads[l]
					} else {
						attr(layer_slice, "sum_dyads") <- attrs$sum_dyads %||% FALSE
					}

					class(layer_slice) <- "netify"

					time_list[[t]] <- layer_slice
				}

				# wrap as longitudinal list with all required attributes
				# must set class first before adding attributes
				class(time_list) <- "netify"
				attr(time_list, "netify_type") <- "longit_list"
				attr(time_list, "symmetric") <- if (length(attrs$symmetric) > 1) attrs$symmetric[l] else attrs$symmetric
				attr(time_list, "mode") <- attrs$mode
				# store the layer-specific weight name
				if (is.character(attrs[["weight"]]) && length(attrs[["weight"]]) > 1) {
					attr(time_list, "weight") <- attrs[["weight"]][l]
					attr(time_list, "detail_weight") <- paste("Weights from `", attrs[["weight"]][l], "`", sep = "")
				} else {
					attr(time_list, "weight") <- attrs[["weight"]]
					attr(time_list, "detail_weight") <- attrs$detail_weight
				}
				# handle is_binary which might be a vector
				if (length(attrs[["is_binary"]]) > 1) {
					attr(time_list, "is_binary") <- attrs[["is_binary"]][l]
				} else {
					attr(time_list, "is_binary") <- attrs[["is_binary"]]
				}
				attr(time_list, "layers") <- layer_names[l]
				attr(time_list, "loops") <- attrs$loops %||% FALSE
				attr(time_list, "actor_time_uniform") <- attrs$actor_time_uniform %||% TRUE
				attr(time_list, "actor_pds") <- attrs$actor_pds
				# handle attributes that might be vectors for multilayer
				if (length(attrs$diag_to_NA) > 1) {
					attr(time_list, "diag_to_NA") <- attrs$diag_to_NA[l]
				} else {
					attr(time_list, "diag_to_NA") <- attrs$diag_to_NA %||% TRUE
				}
				if (length(attrs$missing_to_zero) > 1) {
					attr(time_list, "missing_to_zero") <- attrs$missing_to_zero[l]
				} else {
					attr(time_list, "missing_to_zero") <- attrs$missing_to_zero %||% TRUE
				}
				if (length(attrs$sum_dyads) > 1) {
					attr(time_list, "sum_dyads") <- attrs$sum_dyads[l]
				} else {
					attr(time_list, "sum_dyads") <- attrs$sum_dyads %||% FALSE
				}

				layer_list[[l]] <- time_list
			}

			return(layer_list)
		} else {
			# regular longit_list: per-period netifies with sliced
			# nodal_data and dyad_data so downstream helpers
			# (compare_attributes, etc.) see the right covariates
			parent_nd <- attrs$nodal_data
			parent_dd <- attrs$dyad_data
			per_period <- vector("list", length(net))
			names(per_period) <- names(net)
			for (k in seq_along(net)) {
				slice <- net[[k]]
				attr(slice, "netify_type") <- "cross_sec"
				attr(slice, "mode") <- attrs$mode
				attr(slice, "symmetric") <- attrs$symmetric
				attr(slice, "weight") <- attrs[["weight"]]
				attr(slice, "is_binary") <- attrs[["is_binary"]]
				attr(slice, "diag_to_NA") <- attrs$diag_to_NA %||% TRUE
				attr(slice, "missing_to_zero") <- attrs$missing_to_zero %||% TRUE
				attr(slice, "sum_dyads") <- attrs$sum_dyads %||% FALSE
				attr(slice, "detail_weight") <- attrs$detail_weight
				attr(slice, "layers") <- attrs$layers
				if (!is.null(parent_nd) && is.data.frame(parent_nd) &&
					"time" %in% names(parent_nd)) {
					sub_nd <- parent_nd[
						as.character(parent_nd$time) == as.character(names(net)[k]),
						, drop = FALSE
					]
					attr(slice, "nodal_data") <- sub_nd
				} else if (!is.null(parent_nd)) {
					attr(slice, "nodal_data") <- parent_nd
				}
				if (!is.null(parent_dd) && is.list(parent_dd) &&
					!is.null(parent_dd[[names(net)[k]]])) {
					attr(slice, "dyad_data") <- list(parent_dd[[names(net)[k]]])
				}
				class(slice) <- "netify"
				per_period[[k]] <- slice
			}
			return(per_period)
		}
	} else if (attrs$netify_type == "longit_array") {
		# check if multilayer (4d array)
		if (has_layers && length(dim(net)) == 4) {
			# multilayer longitudinal array [actors x actors x layers x time]
			dims <- dim(net)
			layer_names <- attrs$layers
			time_names <- dimnames(net)[[4]]
			if (is.null(time_names)) {
				time_names <- as.character(1:dims[4])
			}

			# extract each layer
			layer_list <- vector("list", dims[3])
			names(layer_list) <- layer_names

			for (l in seq_len(dims[3])) {
				# extract 3d array for this layer [actors x actors x time]
				layer_array <- net[, , l, ]

				# set proper dimensions
				dim(layer_array) <- c(dims[1], dims[2], dims[4])
				dimnames(layer_array) <- list(
					dimnames(net)[[1]],
					dimnames(net)[[2]],
					time_names
				)

				# preserve attributes
				attr(layer_array, "netify_type") <- "longit_array"
				attr(layer_array, "symmetric") <- if (length(attrs$symmetric) > 1) attrs$symmetric[l] else attrs$symmetric
				attr(layer_array, "mode") <- attrs$mode
				# weight + detail_weight may be layer-specific
				if (is.character(attrs[["weight"]]) && length(attrs[["weight"]]) > 1) {
					attr(layer_array, "weight") <- attrs[["weight"]][l]
					attr(layer_array, "detail_weight") <- paste("Weights from `", attrs[["weight"]][l], "`", sep = "")
				} else {
					attr(layer_array, "weight") <- attrs[["weight"]]
					attr(layer_array, "detail_weight") <- attrs$detail_weight
				}
				# is_binary may be a vector across layers
				if (length(attrs[["is_binary"]]) > 1) {
					attr(layer_array, "is_binary") <- attrs[["is_binary"]][l]
				} else {
					attr(layer_array, "is_binary") <- attrs[["is_binary"]]
				}
				# attributes that summary/print downstream need
				if (length(attrs$diag_to_NA) > 1) {
					attr(layer_array, "diag_to_NA") <- attrs$diag_to_NA[l]
				} else {
					attr(layer_array, "diag_to_NA") <- attrs$diag_to_NA %||% TRUE
				}
				if (length(attrs$missing_to_zero) > 1) {
					attr(layer_array, "missing_to_zero") <- attrs$missing_to_zero[l]
				} else {
					attr(layer_array, "missing_to_zero") <- attrs$missing_to_zero %||% TRUE
				}
				if (length(attrs$sum_dyads) > 1) {
					attr(layer_array, "sum_dyads") <- attrs$sum_dyads[l]
				} else {
					attr(layer_array, "sum_dyads") <- attrs$sum_dyads %||% FALSE
				}
				attr(layer_array, "loops") <- attrs$loops %||% FALSE
				attr(layer_array, "actor_time_uniform") <- attrs$actor_time_uniform %||% TRUE
				attr(layer_array, "actor_pds") <- attrs$actor_pds
				attr(layer_array, "layers") <- layer_names[l]
				class(layer_array) <- "netify"

				layer_list[[l]] <- layer_array
			}

			return(layer_list)
		} else {
			# regular longitudinal array (3d)
			dims <- dim(net)
			time_names <- dimnames(net)[[3]]
			if (is.null(time_names)) {
				time_names <- as.character(1:dims[3])
			}

			# check sparsity
			total_elements <- prod(dims)
			non_zero <- sum(net != 0, na.rm = TRUE)
			sparsity <- non_zero / total_elements

			# use sparse operations if very sparse
			if (sparsity < 0.1 && dims[1] > 100) {
				# for very sparse large networks, use sparse melting
				sparse_data <- melt_array_sparse(net)

				# reconstruct list from sparse data
				net_list <- lapply(time_names, function(t) {
					slice_data <- sparse_data[sparse_data$L1 == t, ]
					if (nrow(slice_data) == 0) {
						mat <- matrix(0, dims[1], dims[2],
							dimnames = list(dimnames(net)[[1]], dimnames(net)[[2]])
						)
					} else {
						mat <- matrix(0, dims[1], dims[2],
							dimnames = list(dimnames(net)[[1]], dimnames(net)[[2]])
						)
						for (i in 1:nrow(slice_data)) {
							mat[slice_data$Var1[i], slice_data$Var2[i]] <- slice_data$value[i]
						}
					}
					# preserve attributes
					attr(mat, "netify_type") <- "cross_sec"
					attr(mat, "symmetric") <- attrs$symmetric
					attr(mat, "mode") <- attrs$mode
					attr(mat, "weight") <- attrs[["weight"]]
					attr(mat, "is_binary") <- attrs[["is_binary"]]
					attr(mat, "diag_to_NA") <- attrs$diag_to_NA %||% TRUE
					attr(mat, "missing_to_zero") <- attrs$missing_to_zero %||% TRUE
					attr(mat, "sum_dyads") <- attrs$sum_dyads %||% FALSE
					attr(mat, "detail_weight") <- attrs$detail_weight
					attr(mat, "actor_time_uniform") <- attrs$actor_time_uniform %||% TRUE
					attr(mat, "loops") <- attrs$loops %||% FALSE
						# mark this matrix as a single layer
					attr(mat, "layers") <- TRUE
					class(mat) <- "netify"
					mat
				})
			} else {
				# standard path for denser networks
				net_list <- vector("list", dims[3])
				for (t in 1:dims[3]) {
					# extract time slice and preserve as matrix
					time_slice <- net[, , t]
					# preserve attributes from original
					attr(time_slice, "netify_type") <- "cross_sec"
					attr(time_slice, "symmetric") <- attrs$symmetric
					attr(time_slice, "mode") <- attrs$mode
					attr(time_slice, "weight") <- attrs[["weight"]]
					attr(time_slice, "is_binary") <- attrs[["is_binary"]]
					attr(time_slice, "diag_to_NA") <- attrs$diag_to_NA %||% TRUE
					attr(time_slice, "missing_to_zero") <- attrs$missing_to_zero %||% TRUE
					attr(time_slice, "sum_dyads") <- attrs$sum_dyads %||% FALSE
					attr(time_slice, "detail_weight") <- attrs$detail_weight
					attr(time_slice, "actor_time_uniform") <- attrs$actor_time_uniform %||% TRUE
					attr(time_slice, "loops") <- attrs$loops %||% FALSE
						# mark this matrix as a single layer
					attr(time_slice, "layers") <- TRUE
					class(time_slice) <- "netify"

					net_list[[t]] <- time_slice
				}
			}

			names(net_list) <- time_names
			return(net_list)
		}
	} else if (attrs$netify_type == "cross_sec") {
		# check if multilayer (3d array)
		if (has_layers && length(dim(net)) == 3) {
			# multilayer cross-sectional [actors x actors x layers]
			dims <- dim(net)
			layer_names <- attrs$layers

			# extract each layer
			layer_list <- vector("list", dims[3])
			names(layer_list) <- layer_names

			for (l in seq_len(dims[3])) {
				# extract layer slice
				layer_slice <- net[, , l]

				# preserve all required attributes
				attr(layer_slice, "netify_type") <- "cross_sec"
				attr(layer_slice, "symmetric") <- if (length(attrs$symmetric) > 1) attrs$symmetric[l] else attrs$symmetric
				attr(layer_slice, "mode") <- attrs$mode
				# store the layer-specific weight name
				if (is.character(attrs[["weight"]]) && length(attrs[["weight"]]) > 1) {
					attr(layer_slice, "weight") <- attrs[["weight"]][l]
					attr(layer_slice, "detail_weight") <- paste("Weights from `", attrs[["weight"]][l], "`", sep = "")
				} else {
					attr(layer_slice, "weight") <- attrs[["weight"]]
					attr(layer_slice, "detail_weight") <- attrs$detail_weight
				}
				# handle is_binary which might be a vector
				if (length(attrs[["is_binary"]]) > 1) {
					attr(layer_slice, "is_binary") <- attrs[["is_binary"]][l]
				} else {
					attr(layer_slice, "is_binary") <- attrs[["is_binary"]]
				}
				attr(layer_slice, "loops") <- attrs$loops %||% FALSE
				attr(layer_slice, "layers") <- layer_names[l]
				attr(layer_slice, "actor_time_uniform") <- attrs$actor_time_uniform %||% TRUE
				attr(layer_slice, "actor_pds") <- attrs$actor_pds
				# handle attributes that might be vectors for multilayer
				if (length(attrs$diag_to_NA) > 1) {
					attr(layer_slice, "diag_to_NA") <- attrs$diag_to_NA[l]
				} else {
					attr(layer_slice, "diag_to_NA") <- attrs$diag_to_NA %||% TRUE
				}
				if (length(attrs$missing_to_zero) > 1) {
					attr(layer_slice, "missing_to_zero") <- attrs$missing_to_zero[l]
				} else {
					attr(layer_slice, "missing_to_zero") <- attrs$missing_to_zero %||% TRUE
				}
				if (length(attrs$sum_dyads) > 1) {
					attr(layer_slice, "sum_dyads") <- attrs$sum_dyads[l]
				} else {
					attr(layer_slice, "sum_dyads") <- attrs$sum_dyads %||% FALSE
				}
				class(layer_slice) <- "netify"

				layer_list[[l]] <- layer_slice
			}

			return(layer_list)
		} else {
			# single cross-sectional network
			return(list(network = net))
		}
	} else {
		# unknown type - return as single network
		return(list(network = net))
	}
}

#' prepare networks for by-group comparison
#'
#' `prepare_by_group_networks` subsets a network based on a nodal attribute
#' to produce one sub-network per group value. each sub-network keeps only
#' the actors that share the same attribute value, so downstream
#' `compare_networks` calls can ask how within-group ties differ from one
#' group to another.
#'
#' @param net a netify object containing nodal attributes.
#' @param by character string specifying the nodal attribute to group by.
#'
#' @return named list of netify objects, one per non-na group value.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

prepare_by_group_networks <- function(net, by) {
	# extract attributes
	attrs <- attributes(net)
	nodal_data <- attrs$nodal_data

	# resolve the per-actor lookup frame, handling both cross-sectional
	# (data.frame) and longitudinal (list or stacked data.frame) shapes
	lookup_df <- NULL
	if (is.data.frame(nodal_data)) {
		lookup_df <- nodal_data
	} else if (is.list(nodal_data) && length(nodal_data) > 0) {
		# stack longitudinal slices; an actor's group can change over time
		# so we keep the union and warn when group assignment is unstable
		lookup_df <- do.call(rbind, lapply(nodal_data, function(d) {
			if (is.data.frame(d)) d else NULL
		}))
	}

	if (is.null(lookup_df) || !by %in% names(lookup_df)) {
		avail <- if (is.null(lookup_df)) character(0) else setdiff(names(lookup_df), c("actor", "time"))
		cli::cli_abort(
			c(
				"x" = "Attribute {.val {by}} not found in nodal data.",
				"i" = "Available attributes: {.val {avail}}",
				"!" = "Pass a valid nodal attribute name to {.arg by}."
			)
		)
	}

	# build actor -> group map; drop nas so empty/unknown buckets do not
	# silently degrade the comparison
	actor_col <- if ("actor" %in% names(lookup_df)) "actor" else names(lookup_df)[1]
	keep <- !is.na(lookup_df[[by]])
	actors <- as.character(lookup_df[[actor_col]][keep])
	grp_vals <- as.character(lookup_df[[by]][keep])

	# warn on multi-group actors (longitudinal attributes can drift)
	dup_actors <- unique(actors[duplicated(actors)])
	if (length(dup_actors) > 0L) {
		shifters <- vapply(dup_actors, function(a) {
			length(unique(grp_vals[actors == a])) > 1L
		}, logical(1))
		if (any(shifters)) {
			cli::cli_inform(c(
				"!" = "{sum(shifters)} actor{?s} changed {.arg {by}} value across time; using the first observed group."
			))
		}
	}

	# collapse to one row per actor (first observation wins)
	first_idx <- !duplicated(actors)
	actors <- actors[first_idx]
	grp_vals <- grp_vals[first_idx]

	groups <- sort(unique(grp_vals))
	if (length(groups) < 2L) {
		cli::cli_abort(c(
			"x" = "Need >= 2 groups in {.arg by = {by}} to compare; found {length(groups)}.",
			"i" = "Pass an attribute with multiple categories."
		))
	}

	# build one sub-network per group
	dropped_groups <- character(0)
	nets_list <- lapply(groups, function(g) {
		members <- actors[grp_vals == g]
		if (length(members) < 2L) {
			dropped_groups <<- c(dropped_groups, g)
			return(NULL)
		}
		tryCatch(
			subset_netify(netlet = net, actors = members),
			error = function(e) {
				dropped_groups <<- c(dropped_groups, g)
				cli::cli_warn("Skipping group {.val {g}}: {e$message}")
				NULL
			}
		)
	})
	names(nets_list) <- groups

	# drop unusable groups
	nets_list <- Filter(Negate(is.null), nets_list)
	if (length(dropped_groups) > 0L) {
		cli::cli_warn(c(
			"!" = "Dropped {length(unique(dropped_groups))} group{?s} that could not form a network: {paste(unique(dropped_groups), collapse = ', ')}",
			"i" = "Each compared group needs at least 2 actors after subsetting."
		))
	}
	if (length(nets_list) < 2L) {
		cli::cli_abort(c(
			"x" = "After subsetting, fewer than 2 groups have enough actors to compare."
		))
	}

	return(nets_list)
}

#' analyze network comparisons by group
#'
#' `analyze_by_group` performs additional analysis on comparison results when networks are grouped by nodal attributes (currently not fully implemented).
#'
#' @param results a netify_comparison object from compare_networks.
#' @param nets_list the original list of networks being compared.
#' @param by character string specifying the grouping attribute.
#'
#' @return a list containing by-group analysis results (currently returns placeholder).
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

analyze_by_group <- function(results, nets_list, by) {
	# count actors per group so callers can see how each subset was sized
	n_actors_per_group <- vapply(nets_list, function(n) {
		mst <- tryCatch(netify_measurements(n), error = function(e) NULL)
		if (is.null(mst)) return(NA_integer_)
		# unipartite networks have matching row/col actor counts
		val <- mst$n_row_actors %||% mst$n_actors
		if (is.null(val)) return(NA_integer_)
		as.integer(val)
	}, integer(1))

	# pull pairwise similarity matrix if available, for downstream plots
	sim_mat <- NULL
	if (!is.null(results$details) && !is.null(results$details$correlation_matrix)) {
		sim_mat <- results$details$correlation_matrix
	}

	out <- list(
		by_attribute = by,
		groups = names(nets_list),
		n_actors_per_group = n_actors_per_group
	)
	if (!is.null(sim_mat)) out$similarity_matrix <- sim_mat
	out
}
#' create edge comparison summary
#'
#' `create_edge_summary` formats the results of edge comparisons into a summary data frame, handling both pairwise and multiple network comparisons.
#'
#' @param correlation_mat matrix of correlation coefficients between networks.
#' @param jaccard_mat matrix of jaccard similarities between networks.
#' @param hamming_mat matrix of hamming distances between networks.
#' @param qap_mat matrix of qap correlations between networks.
#' @param qap_pval_mat matrix of qap p-values between networks.
#' @param method character string specifying which metrics to include in summary.
#'
#' @return a data frame summarizing the comparison results.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

create_edge_summary <- function(
	correlation_mat, jaccard_mat, hamming_mat,
	qap_mat, qap_pval_mat, spectral_mat, method) {
	# create summary dataframe based on number of networks
	n <- nrow(correlation_mat)
	if (n == 2) {
		# simple two-network comparison
		summary_df <- data.frame(
			comparison = paste(
				rownames(correlation_mat)[1], "vs",
				rownames(correlation_mat)[2]
			),
			stringsAsFactors = FALSE
		)

		if (method %in% c("correlation", "all")) {
			summary_df$correlation <- correlation_mat[1, 2]
		}
		if (method %in% c("jaccard", "all")) {
			summary_df$jaccard <- jaccard_mat[1, 2]
		}
		if (method %in% c("hamming", "all")) {
			summary_df$hamming <- hamming_mat[1, 2]
		}
		if (method %in% c("qap", "all") && !is.na(qap_mat[1, 2])) {
			summary_df$qap_correlation <- qap_mat[1, 2]
			summary_df$qap_pvalue <- qap_pval_mat[1, 2]
		}
		if (method %in% c("spectral", "all")) {
			summary_df$spectral <- spectral_mat[1, 2]
		}
	} else {
		# multiple network comparison - return average similarities
		summary_df <- data.frame(
			metric = character(),
			mean = numeric(),
			sd = numeric(),
			min = numeric(),
			max = numeric(),
			stringsAsFactors = FALSE
		)

		if (method %in% c("correlation", "all")) {
			cors <- correlation_mat[lower.tri(correlation_mat)]
			summary_df <- rbind(summary_df, data.frame(
				metric = "correlation",
				mean = mean(cors, na.rm = TRUE),
				sd = sd(cors, na.rm = TRUE),
				min = min(cors, na.rm = TRUE),
				max = max(cors, na.rm = TRUE),
				stringsAsFactors = FALSE
			))
		}

		if (method %in% c("jaccard", "all")) {
			jacs <- jaccard_mat[lower.tri(jaccard_mat)]
			summary_df <- rbind(summary_df, data.frame(
				metric = "jaccard",
				mean = mean(jacs, na.rm = TRUE),
				sd = sd(jacs, na.rm = TRUE),
				min = min(jacs, na.rm = TRUE),
				max = max(jacs, na.rm = TRUE),
				stringsAsFactors = FALSE
			))
		}

		if (method %in% c("hamming", "all")) {
			hams <- hamming_mat[lower.tri(hamming_mat)]
			summary_df <- rbind(summary_df, data.frame(
				metric = "hamming",
				mean = mean(hams, na.rm = TRUE),
				sd = sd(hams, na.rm = TRUE),
				min = min(hams, na.rm = TRUE),
				max = max(hams, na.rm = TRUE),
				stringsAsFactors = FALSE
			))
		}

		# include qap row when qap was the requested method (or "all").
		# without this branch, method="qap" with >2 networks produced an
		# empty $summary frame and an empty "edge comparison summary"
		# table in print.netify_comparison.
		if (method %in% c("qap", "all") && !is.null(qap_mat) && !all(is.na(qap_mat))) {
			qaps <- qap_mat[lower.tri(qap_mat)]
			if (length(qaps) && any(!is.na(qaps))) {
				summary_df <- rbind(summary_df, data.frame(
					metric = "qap_correlation",
					mean = mean(qaps, na.rm = TRUE),
					sd = sd(qaps, na.rm = TRUE),
					min = min(qaps, na.rm = TRUE),
					max = max(qaps, na.rm = TRUE),
					stringsAsFactors = FALSE
				))
			}
		}

		if (method %in% c("spectral", "all")) {
			specs <- spectral_mat[lower.tri(spectral_mat)]
			summary_df <- rbind(summary_df, data.frame(
				metric = "spectral",
				mean = mean(specs, na.rm = TRUE),
				sd = sd(specs, na.rm = TRUE),
				min = min(specs, na.rm = TRUE),
				max = max(specs, na.rm = TRUE),
				stringsAsFactors = FALSE
			))
		}
	}

	return(summary_df)
}

#' compare single attribute between networks
#'
#' `compare_single_attribute` compares the distribution of a specific nodal attribute across networks using appropriate similarity measures and statistical tests.
#'
#' @param nets_list a list of netify objects to compare.
#' @param attrs_list list of nodal attribute data frames from each network.
#' @param attribute character string specifying which attribute to compare.
#' @param test logical; whether to perform statistical testing.
#' @param n_permutations integer; number of permutations (currently unused).
#' @param attr_metric character string specifying method for continuous comparison.
#'
#' @return a list containing similarity comparisons and optional test results.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @importFrom stats ks.test
#'
#' @keywords internal
#' @noRd

compare_single_attribute <- function(
	nets_list, attrs_list, attribute,
	test = TRUE, n_permutations = 1000, attr_metric = "ecdf_cor") {
	n_nets <- length(nets_list)
	net_names <- names(nets_list)
	if (is.null(net_names)) {
		net_names <- paste0("net", seq_along(nets_list))
	}

	# extract attribute values
	attr_values_list <- list()

	for (i in 1:n_nets) {
		if (!is.null(attrs_list[[i]]) && attribute %in% names(attrs_list[[i]])) {
			# get attribute values
			attr_vals <- attrs_list[[i]][[attribute]]
			actors <- attrs_list[[i]]$actor

			# remove missing/non-finite numeric values
			complete <- if (is.numeric(attr_vals)) {
				!is.na(attr_vals) & is.finite(attr_vals)
			} else {
				!is.na(attr_vals)
			}
			attr_vals <- attr_vals[complete]
			actors <- actors[complete]

			attr_values_list[[i]] <- list(values = attr_vals, actors = actors)
		}
	}

	# compare attribute distributions pairwise
	comparison_mat <- matrix(NA, n_nets, n_nets,
		dimnames = list(net_names, net_names)
	)
	ks_test_mat <- matrix(NA, n_nets, n_nets,
		dimnames = list(net_names, net_names)
	)

	for (i in 1:(n_nets - 1)) {
		for (j in (i + 1):n_nets) {
			if (!is.null(attr_values_list[[i]]) && !is.null(attr_values_list[[j]])) {
				vals1 <- attr_values_list[[i]]$values
				vals2 <- attr_values_list[[j]]$values

					if (is.numeric(vals1) && is.numeric(vals2)) {
						# compare numeric attribute distributions
					if (test && length(vals1) > 1 && length(vals2) > 1) {
						ks_result <- ks.test(vals1, vals2)
						ks_test_mat[i, j] <- ks_test_mat[j, i] <- ks_result$p.value
					}

					# compare distributions
					if (attr_metric == "ecdf_cor") {
						comparison_mat[i, j] <- comparison_mat[j, i] <-
							compare_distributions(vals1, vals2)
					} else if (attr_metric == "wasserstein") {
						# wasserstein distance - convert to similarity (1 / (1 + distance))
						w_dist <- calculate_wasserstein_cpp(vals1, vals2)
						comparison_mat[i, j] <- comparison_mat[j, i] <-
							1 / (1 + w_dist)
					}
				} else {
					# for categorical, compare frequency distributions
					comparison_mat[i, j] <- comparison_mat[j, i] <-
						compare_categorical_distributions(vals1, vals2)
				}
			}
		}
	}

	# set diagonal
	diag(comparison_mat) <- 1
	if (test) diag(ks_test_mat) <- 1

		# keep one row for each pairwise comparison
	pair_rows <- list()
	for (i in 1:(n_nets - 1)) {
		for (j in (i + 1):n_nets) {
			row <- data.frame(
				comparison = paste(net_names[i], "vs", net_names[j]),
				similarity = comparison_mat[i, j],
				stringsAsFactors = FALSE
			)
			if (test && !is.na(ks_test_mat[i, j])) {
				row$ks_pvalue <- ks_test_mat[i, j]
			}
			pair_rows[[length(pair_rows) + 1L]] <- row
		}
	}
	all_cols <- unique(unlist(lapply(pair_rows, names)))
	pair_padded <- lapply(pair_rows, function(df) {
		missing_c <- setdiff(all_cols, names(df))
		for (m in missing_c) df[[m]] <- NA
		df[, all_cols, drop = FALSE]
	})
	summary <- do.call(rbind, c(pair_padded, list(make.row.names = FALSE)))

	return(list(
		summary = summary,
		details = list(
			similarity_matrix = comparison_mat,
			ks_test_matrix = if (test) ks_test_mat else NULL,
			attribute_values = attr_values_list
		)
	))
}

#' compare continuous distributions
#'
#' `compare_distributions` calculates similarity between two continuous distributions by correlating their empirical cumulative distribution functions.
#'
#' @param vals1 numeric vector of values from first distribution.
#' @param vals2 numeric vector of values from second distribution.
#'
#' @return numeric correlation coefficient between the two ecdfs.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @importFrom stats ecdf cor
#'
#' @keywords internal
#' @noRd

compare_distributions <- function(vals1, vals2) {
	# correlate the empirical cdfs of the two value vectors

	# check for empty or all-na values first
	if (length(vals1) == 0 || all(is.na(vals1)) ||
		length(vals2) == 0 || all(is.na(vals2))) {
		return(NA)
	}

	# remove nas
	vals1 <- vals1[!is.na(vals1)]
	vals2 <- vals2[!is.na(vals2)]

	# check again after removing nas
	if (length(vals1) == 0 || length(vals2) == 0) {
		return(NA)
	}

	# create empirical cdfs
	all_vals <- sort(unique(c(vals1, vals2)))

	# handle case where all values are identical
	if (length(all_vals) == 1) {
		return(1) # perfect similarity if all values are the same
	}

	ecdf1 <- ecdf(vals1)
	ecdf2 <- ecdf(vals2)

	# evaluate at common points
	cdf1 <- ecdf1(all_vals)
	cdf2 <- ecdf2(all_vals)

	# return correlation of cdfs; guard against zero-variance ramps
	# (e.g., a constant covariate within each period) which would otherwise
	# produce a `cor` zero-sd warning and na result
	if (length(all_vals) > 1) {
		if (stats::sd(cdf1) == 0 || stats::sd(cdf2) == 0) {
			# both constant -> identical step functions -> perfect similarity
			if (stats::sd(cdf1) == 0 && stats::sd(cdf2) == 0 &&
				isTRUE(all.equal(cdf1, cdf2))) {
				return(1)
			}
			return(NA_real_)
		}
		return(cor(cdf1, cdf2))
	} else {
		return(NA)
	}
}

#' compare categorical distributions
#'
#' `compare_categorical_distributions` calculates similarity between two categorical distributions using total variation distance.
#'
#' @param vals1 vector of categorical values from first distribution.
#' @param vals2 vector of categorical values from second distribution.
#'
#' @return numeric similarity score (1 - total variation distance).
#'
#' @author cassy dorff, shahryar minhas
#'
#' @keywords internal
#' @noRd

compare_categorical_distributions <- function(vals1, vals2) {
	# get frequency tables
	tab1 <- table(vals1)
	tab2 <- table(vals2)

	# get all categories
	all_cats <- sort(unique(c(names(tab1), names(tab2))))

	# create aligned frequency vectors
	freq1 <- numeric(length(all_cats))
	freq2 <- numeric(length(all_cats))

	names(freq1) <- names(freq2) <- all_cats

	freq1[names(tab1)] <- tab1
	freq2[names(tab2)] <- tab2

	# normalize to proportions
	prop1 <- freq1 / sum(freq1)
	prop2 <- freq2 / sum(freq2)

	# calc similarity (1 - total variation distance)
	tv_distance <- 0.5 * sum(abs(prop1 - prop2))
	similarity <- 1 - tv_distance

	return(similarity)
}


#' calculate spectral distance between networks
#'
#' computes the spectral distance between two networks based on their eigenvalue
#' spectra. the spectral distance quantifies how different two graphs are by
#' comparing their eigenvalues.
#'
#' @param mat1 first adjacency matrix
#' @param mat2 second adjacency matrix
#' @param laplacian logical; whether to use laplacian eigenvalues (default TRUE)
#'   instead of adjacency matrix eigenvalues
#'
#' @return numeric spectral distance between 0 and sqrt(2*n) where n is the
#'   number of nodes. lower values indicate more similar networks.
#'
#' @details
#' the spectral distance is calculated as:
#' \deqn{d_{spectral}(g_1, g_2) = \sqrt{\sum_{i=1}^n (\lambda_i^{(1)} - \lambda_i^{(2)})^2}}
#'
#' where \eqn{\lambda_i^{(1)}} and \eqn{\lambda_i^{(2)}} are the sorted eigenvalues
#' of the two networks' laplacian (or adjacency) matrices.
#'
#' for directed networks, we symmetrize by averaging with the transpose.
#' missing values are replaced with zeros.
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
calculate_spectral_distance <- function(mat1, mat2, laplacian = TRUE) {
	# handle missing values
	mat1[is.na(mat1)] <- 0
	mat2[is.na(mat2)] <- 0

	# require matching dimensions
	n1 <- nrow(mat1)
	n2 <- nrow(mat2)

	if (n1 != n2) {
		# pad smaller matrix with zeros
		n <- max(n1, n2)
		if (n1 < n) {
			mat1_new <- matrix(0, n, n)
			mat1_new[1:n1, 1:n1] <- mat1
			mat1 <- mat1_new
		}
		if (n2 < n) {
			mat2_new <- matrix(0, n, n)
			mat2_new[1:n2, 1:n2] <- mat2
			mat2 <- mat2_new
		}
	}

	# symmetrize if not symmetric (for directed networks)
	if (!isSymmetric(mat1)) {
		mat1 <- (mat1 + t(mat1)) / 2
	}
	if (!isSymmetric(mat2)) {
		mat2 <- (mat2 + t(mat2)) / 2
	}

	# calculate eigenvalues
	if (laplacian) {
		# compute laplacian matrices
		deg1 <- rowSums(mat1)
		deg2 <- rowSums(mat2)
		L1 <- diag(deg1) - mat1
		L2 <- diag(deg2) - mat2

		# get eigenvalues
		eigen1 <- eigen(L1, symmetric = TRUE, only.values = TRUE)$values
		eigen2 <- eigen(L2, symmetric = TRUE, only.values = TRUE)$values
	} else {
		# use adjacency matrix eigenvalues
		eigen1 <- eigen(mat1, symmetric = TRUE, only.values = TRUE)$values
		eigen2 <- eigen(mat2, symmetric = TRUE, only.values = TRUE)$values
	}

	# sort eigenvalues in decreasing order
	eigen1 <- sort(eigen1, decreasing = TRUE)
	eigen2 <- sort(eigen2, decreasing = TRUE)

	# calculate spectral distance
	spectral_dist <- sqrt(sum((eigen1 - eigen2)^2))

	return(spectral_dist)
}


#' build a tidy long-format per-pair comparison frame
#'
#' `build_comparisons_frame` reshapes the comparison artifacts into one row
#' per (net_i, net_j, metric). handles the cross-network, multilayer, and
#' longitudinal cases by reading from `$summary`, `$edge_changes`, and
#' `$significance_tests` matrices when available.
#'
#' @param results the partially-built `netify_comparison` list.
#' @param nets_list the list of netify objects being compared.
#'
#' @return a data frame with columns `net_i`, `net_j`, `metric`, `value`,
#'   `p_value`, or NULL if nothing usable is available.
#'
#' @keywords internal
#' @noRd
build_comparisons_frame <- function(results, nets_list) {
	net_names <- names(nets_list)
	if (is.null(net_names) || length(net_names) < 2) return(NULL)

	# wide summary path: cross_network / multilayer / by_group return one
	# row per pair with one column per metric, so we can pivot directly
	wide_metrics <- c("correlation", "jaccard", "hamming",
		"qap_correlation", "qap_pvalue", "spectral")
	summary_df <- results$summary
	if (!is.null(summary_df) && is.data.frame(summary_df) &&
		"comparison" %in% names(summary_df)) {
		rows <- list()
		for (k in seq_len(nrow(summary_df))) {
			comp_label <- summary_df$comparison[k]
			pair <- strsplit(as.character(comp_label), " vs ", fixed = TRUE)[[1]]
			if (length(pair) != 2) next
			net_i <- pair[1]
			net_j <- pair[2]
			qap_p <- if ("qap_pvalue" %in% names(summary_df)) summary_df$qap_pvalue[k] else NA_real_
			for (m in wide_metrics) {
				if (m == "qap_pvalue") next
				if (!m %in% names(summary_df)) next
				val <- summary_df[[m]][k]
				if (is.na(val)) next
				pv <- if (m == "qap_correlation") qap_p else NA_real_
				rows[[length(rows) + 1L]] <- data.frame(
					net_i = net_i, net_j = net_j,
					metric = m, value = val, p_value = pv,
					stringsAsFactors = FALSE
				)
			}
		}
		if (length(rows) > 0L) return(do.call(rbind, rows))
	}

	# longitudinal path: $summary is aggregated (mean/sd/min/max). use the
	# per-pair qap matrix in $significance_tests, plus weight_correlation
	# and edge_change tallies from $edge_changes
	rows <- list()
	qap_mat <- results$significance_tests$qap_correlations
	qap_p_mat <- results$significance_tests$qap_pvalues
	if (!is.null(qap_mat) && is.matrix(qap_mat)) {
		mn <- rownames(qap_mat)
		if (is.null(mn)) mn <- net_names
		for (i in seq_len(nrow(qap_mat) - 1L)) {
			for (j in (i + 1L):ncol(qap_mat)) {
				val <- qap_mat[i, j]
				pv <- if (!is.null(qap_p_mat)) qap_p_mat[i, j] else NA_real_
				rows[[length(rows) + 1L]] <- data.frame(
					net_i = mn[i], net_j = mn[j],
					metric = "qap_correlation", value = val, p_value = pv,
					stringsAsFactors = FALSE
				)
			}
		}
	}

	ec <- results$edge_changes
	if (!is.null(ec) && length(ec) > 0L && !is.null(names(ec))) {
		for (nm in names(ec)) {
			pair <- strsplit(nm, "_vs_", fixed = TRUE)[[1]]
			if (length(pair) != 2L) next
			entry <- ec[[nm]]
			wc <- entry$weight_correlation
			if (!is.null(wc) && !is.na(wc)) {
				rows[[length(rows) + 1L]] <- data.frame(
					net_i = pair[1], net_j = pair[2],
					metric = "weight_correlation", value = wc,
					p_value = NA_real_, stringsAsFactors = FALSE
				)
			}
		}
	}

	if (length(rows) == 0L) return(NULL)
	do.call(rbind, rows)
}
