#' Analyze homophily in network data
#'
#' tests whether connected actors have similar attributes (homophily). calculates
#' the correlation between attribute similarity and tie presence, with support for
#' multiple similarity metrics and significance testing.
#'
#' @param netlet a netify object containing network data.
#' @param attribute character string specifying the nodal attribute to analyze.
#' @param method character string specifying the similarity metric:
#'   \describe{
#'     \item{"correlation"}{negative absolute difference for continuous data (default)}
#'     \item{"euclidean"}{negative euclidean distance for continuous data}
#'     \item{"manhattan"}{negative manhattan/city-block distance for continuous data}
#'     \item{"cosine"}{cosine similarity for continuous data}
#'     \item{"categorical"}{binary similarity (0/1) for categorical data}
#'     \item{"jaccard"}{jaccard similarity for binary/presence-absence data}
#'     \item{"hamming"}{negative hamming distance for categorical data}
#'   }
#' @param threshold numeric value or function to determine tie presence in weighted networks.
#'   if numeric, edges with weights > threshold are considered ties. if a function,
#'   it should take the network matrix and return a logical matrix. default is 0
#'   (any positive weight is a tie). common values: 0 (default), mean(weights),
#'   median(weights), or quantile-based thresholds. for pre-binarized networks,
#'   consider using \code{mutate_weights()} first.
#' @param signed_handling character. strategy for signed (negative-weight) edges:
#'   \describe{
#'     \item{\code{"abs"}}{(default) take absolute values before thresholding so
#'       any tie magnitude -- positive or negative -- can become a connection.}
#'     \item{\code{"drop_negative"}}{set negative weights to zero before
#'       thresholding (positive ties only).}
#'     \item{\code{"preserve_sign"}}{treat any non-zero entry (positive or
#'       negative) as a connection; \code{threshold} is ignored for sign decisions.}
#'   }
#'   ignored when the network has no negative weights.
#' @param significance_test logical. whether to perform a dyad-level
#'   permutation test. default TRUE.
#' @param n_permutations number of permutations for significance testing. default 1000.
#' @param alpha significance level for confidence intervals. default 0.05.
#' @param other_stats named list of custom functions for additional statistics.
#' @param ... additional arguments passed to custom functions.
#'
#' @return data frame with homophily statistics per network/time period:
#'   \describe{
#'     \item{\code{net}}{network/time identifier}
#'     \item{\code{layer}}{layer name}
#'     \item{\code{attribute}}{analyzed attribute name}
#'     \item{\code{method}}{similarity method used}
#'     \item{\code{threshold_value}}{threshold used for determining ties (na for binary networks)}
#'     \item{\code{homophily_correlation}}{correlation between similarity and tie presence (binary: tie/no tie)}
#'     \item{\code{mean_similarity_connected}}{mean similarity among connected pairs (weight > threshold)}
#'     \item{\code{mean_similarity_unconnected}}{mean similarity among unconnected pairs (weight <= threshold or missing)}
#'     \item{\code{similarity_difference}}{difference between connected and unconnected mean similarities}
#'     \item{\code{p_value}}{permutation test p-value}
#'     \item{\code{ci_lower}, \code{ci_upper}}{confidence interval bounds}
#'     \item{\code{n_connected_pairs}}{number of connected pairs}
#'     \item{\code{n_unconnected_pairs}}{number of unconnected pairs}
#'   }
#'
#' @details
#' \strong{auto-promotion to \code{categorical}:}
#'
#' if you leave \code{method} at its default and pass a \code{character},
#' \code{factor}, or \code{logical} attribute, \code{homophily()} will
#' switch to \code{method = "categorical"} automatically and inform you
#' once per attribute. this avoids the c++-level error that would
#' otherwise come from feeding non-numeric data to the correlation-based
#' similarity routine. pass \code{method = "categorical"} (or any other
#' explicit choice) to silence the message.
#'
#' \strong{similarity metrics:}
#'
#' for continuous attributes:
#' \itemize{
#'   \item \code{correlation}: based on absolute difference, good general purpose metric
#'   \item \code{euclidean}: similar to correlation for single attributes
#'   \item \code{manhattan}: less sensitive to outliers than euclidean
#'   \item \code{cosine}: useful for normalized data or when sign matters
#' }
#'
#' for categorical/binary attributes:
#' \itemize{
#'   \item \code{categorical}: simple matching (1 if same, 0 if different)
#'   \item \code{jaccard}: for binary data, emphasizes shared presence over shared absence
#'   \item \code{hamming}: counts positions where values differ (negated for similarity)
#' }
#'
#' \strong{threshold parameter:}
#'
#' for weighted networks, the \code{threshold} parameter determines what edge weights
#' constitute a "connection". you can specify:
#' \itemize{
#'   \item a numeric value: edges with weight > threshold are ties
#'   \item a function: should take a matrix and return a single numeric threshold
#'   \item common threshold functions:
#'     \itemize{
#'       \item \code{function(x) mean(x, na.rm = TRUE)} - mean weight
#'       \item \code{function(x) median(x, na.rm = TRUE)} - median weight
#'       \item \code{function(x) quantile(x, 0.75, na.rm = TRUE)} - 75th percentile
#'     }
#' }
#'
#' for more complex binarization needs (e.g., different thresholds by time period),
#' consider using \code{mutate_weights()} to pre-process your network.
#'
#' \strong{permutation test:}
#'
#' when \code{significance_test = TRUE}, \code{homophily()} holds the tie
#' indicators fixed and permutes the dyad-level similarity values to form an
#' exploratory reference distribution. the resulting p-value and confidence
#' interval summarize how unusual the observed dyad-level association is under
#' that exchangeability assumption. they are not a node-label permutation test
#' and should not be read as a causal estimate of tie formation.
#'
#' @examples
#' # quick homophily check on the bundled classroom friendship data
#' data(classroom_edges)
#' data(classroom_nodes)
#' net <- netify(
#'     classroom_edges,
#'     actor1 = "from", actor2 = "to",
#'     symmetric = TRUE,
#'     nodal_data = classroom_nodes
#' )
#' # do students cluster by gender?
#' homophily(net, attribute = "gender", method = "categorical")
#'
#' \dontrun{
#' # basic homophily analysis with default threshold (> 0)
#' homophily_default <- homophily(net, attribute = "group")
#'
#' # using different similarity metrics for continuous data
#' homophily_manhattan <- homophily(
#'     net,
#'     attribute = "age",
#'     method = "manhattan" # less sensitive to outliers
#' )
#'
#' # for binary attributes (e.g., gender, membership)
#' homophily_jaccard <- homophily(
#'     net,
#'     attribute = "member",
#'     method = "jaccard" # better for binary data than correlation
#' )
#'
#' # for categorical attributes
#' homophily_categorical <- homophily(
#'     net,
#'     attribute = "department",
#'     method = "categorical"
#' )
#'
#' # combining method and threshold
#' homophily_combined <- homophily(
#'     net,
#'     attribute = "score",
#'     method = "manhattan",
#'     threshold = function(x) quantile(x, 0.75, na.rm = TRUE)
#' )
#' }
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export homophily

homophily <- function(
	netlet, attribute,
	method = "correlation",
	threshold = 0,
	signed_handling = c("abs", "drop_negative", "preserve_sign"),
	significance_test = TRUE,
	n_permutations = 1000,
	alpha = 0.05,
	other_stats = NULL,
	...){

	# input validation
	netify_check(netlet)
	if (missing(attribute) || is.null(attribute)) {
		nodal_data <- attr(netlet, "nodal_data")
		avail <- NULL
		if (is.data.frame(nodal_data)) {
			avail <- setdiff(names(nodal_data), c("actor", "time"))
		} else if (is.list(nodal_data) && length(nodal_data) > 0) {
			avail <- setdiff(names(nodal_data[[1]]), c("actor", "time"))
		}
		msg <- c(
			"!" = "{.arg attribute} is required: which nodal attribute should be tested for homophily?",
			"i" = "Example: {.code homophily(net, attribute = \"gender\", method = \"categorical\")}."
		)
		if (length(avail) > 0) {
			msg <- c(msg, "i" = "Available nodal attributes: {.val {avail}}.")
		} else {
			msg <- c(msg, "i" = "No nodal attributes attached yet -- use {.fn add_node_vars} first.")
		}
		cli::cli_abort(msg)
	}
	checkmate::assert_string(attribute)
	# track whether the caller supplied method
	method_was_default <- missing(method)
	checkmate::assert_choice(method, c(
		"correlation", "euclidean", "manhattan",
		"cosine", "categorical", "jaccard", "hamming"
	))
	if (!is.numeric(threshold) && !is.function(threshold)) {
		cli::cli_abort("threshold must be numeric or a function that returns a numeric value")
	}
	# how to treat negative edge weights when building the binary tie matrix
	signed_handling <- match.arg(signed_handling)
	checkmate::assert_logical(significance_test, len = 1)
	checkmate::assert_count(n_permutations, positive = TRUE)
	checkmate::assert_number(alpha, lower = 0, upper = 1)

	# extract object attributes
	obj_attrs <- attributes(netlet)
	layers <- obj_attrs$layers
	nodal_data <- obj_attrs$nodal_data
	netify_type <- obj_attrs$netify_type
	if (identical(obj_attrs$mode, "bipartite")) {
		cli::cli_abort(c(
			"x" = "{.fn homophily} currently supports one-mode/unipartite netify objects only.",
			"i" = "For bipartite data, analyze row-mode and column-mode attributes separately after projecting or use a two-mode mixing workflow."
		))
	}

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
			# data.frame format with time column
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

	# auto-promote to categorical for non-numeric attributes when method is default
	if (method_was_default) {
		probe_vals <- NULL
		if (is.data.frame(nodal_data)) {
			probe_vals <- nodal_data[[attribute]]
		} else if (is.list(nodal_data)) {
			first_time <- names(nodal_data)[1]
			if (!is.null(first_time)) {
				probe_vals <- nodal_data[[first_time]][[attribute]]
			}
		}
		if (!is.null(probe_vals) &&
			(is.character(probe_vals) || is.factor(probe_vals) || is.logical(probe_vals))) {
			cli::cli_inform(c(
				"i" = "{.arg attribute} {.val {attribute}} is non-numeric; using {.val categorical} similarity. Pass {.code method=} explicitly to silence."
			),
			.frequency = "once",
			.frequency_id = paste0("homophily_auto_categorical_", attribute))
			method <- "categorical"
		}
	}

	# process each layer
	results <- lapply(seq_along(layers), function(layer_index) {
		layer <- layers[layer_index]
		# convert to list format for processing
		netlet_list <- switch(netify_type,
			"cross_sec" = list("1" = netlet),
			"longit_array" = {
				# check if this is multilayer longitudinal (4d) or single layer (3d)
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
					# for cross-sec multilayer: 3d array [actors, actors, layers]
					net_matrix <- netlet[, , layer_index]
				} else if (netify_type == "longit_array" && length(dim(netlet)) == 4) {
					# for longit multilayer: 4d array [actors, actors, layers, time]
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
					# data.frame format with time column
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

			# track attribute values that cannot be analyzed
			attr_valid <- !is.na(node_attrs)
			if (is.numeric(node_attrs)) {
				attr_valid <- attr_valid & is.finite(node_attrs)
			}
			n_missing_attrs <- sum(!attr_valid)
			n_total_actors <- length(node_attrs)

			# remove actors with missing or non-finite attributes
			complete_cases <- attr_valid
			if (sum(complete_cases) < 2) {
				cli::cli_warn("Insufficient non-missing attribute data for time {time_id}")
				return(NULL)
			}

				net_matrix <- net_matrix[complete_cases, complete_cases]
				node_attrs <- node_attrs[complete_cases]
				n_analyzed_actors <- length(node_attrs)

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
					binary_net <- binary_matrix_preserve_na(net_matrix)
				} else {
				# reshape signed weights per signed_handling before thresholding
				net_matrix_t <- net_matrix
				finite_w <- net_matrix_t[is.finite(net_matrix_t)]
				has_negative <- length(finite_w) > 0 && any(finite_w < 0)
				if (has_negative) {
					net_matrix_t <- switch(signed_handling,
						"abs" = abs(net_matrix_t),
						"drop_negative" = {
							tmp <- net_matrix_t
							tmp[!is.na(tmp) & tmp < 0] <- 0
							tmp
						},
						"preserve_sign" = net_matrix_t
					)
				}

				# for weighted networks, apply threshold
				if (is.function(threshold)) {
					# calculate threshold from the matrix
					threshold_value <- threshold(net_matrix_t)
					if (!is.numeric(threshold_value) || length(threshold_value) != 1) {
						cli::cli_abort("threshold function must return a single numeric value")
					}
				} else {
					threshold_value <- threshold
				}

				# binarize on threshold; preserve_sign treats any non-zero as connected
				observed_dyads <- !is.na(net_matrix_t)
				binary_net <- matrix(NA,
					nrow = nrow(net_matrix_t), ncol = ncol(net_matrix_t),
					dimnames = dimnames(net_matrix_t))
				if (has_negative && signed_handling == "preserve_sign") {
					binary_net[observed_dyads] <- net_matrix_t[observed_dyads] != 0
				} else {
					binary_net[observed_dyads] <- net_matrix_t[observed_dyads] > threshold_value
				}
			}

				layer_symmetric <- obj_attrs$symmetric %||% FALSE
				if (length(layer_symmetric) > 1) {
					layer_symmetric <- layer_symmetric[layer_index]
				}
				directed_dyads <- !isTRUE(layer_symmetric)

				# calculate homophily statistics with the binary network
				homophily_stats <- calculate_homophily_stats(
					similarity_matrix, binary_net, significance_test,
					n_permutations, alpha, directed = directed_dyads
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
						result_df$n_pairs <- if (directed_dyads) {
							n_analyzed_actors * (n_analyzed_actors - 1)
						} else {
							n_analyzed_actors * (n_analyzed_actors - 1) / 2
						}

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
	# filter out null results
	results <- results[!sapply(results, is.null)]

	if (length(results) == 0) {
		cli::cli_warn("No valid results obtained from homophily analysis")
		# return an empty result frame
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

#' calculate similarity matrix between node attributes
#'
#' internal function that computes pairwise similarity scores between node attributes
#' using various similarity metrics. used by \code{homophily} to create
#' similarity matrices for homophily analysis.
#'
#' @param attributes numeric or character vector of node attributes. length should
#'   match the number of nodes in the network.
#' @param method character string specifying the similarity metric:
#'   \describe{
#'     \item{"correlation"}{negative absolute difference. for continuous attributes,
#'       returns -|x_i - x_j|. higher values indicate greater similarity.}
#'     \item{"euclidean"}{negative euclidean distance. returns -sqrt((x_i - x_j)^2).
#'       equivalent to correlation method for univariate attributes.}
#'     \item{"categorical"}{binary similarity for categorical variables. returns 1
#'       if attributes match, 0 otherwise.}
#'     \item{"cosine"}{cosine similarity treating each attribute as a 1d vector.
#'       returns (x_i * x_j) / (|x_i| * |x_j|). ranges from -1 to 1.}
#'   }
#'
#' @return square numeric matrix of dimension n x n where n is the length of
#'   attributes. diagonal elements are set to 0. for correlation and euclidean
#'   methods, values are non-positive with 0 indicating perfect similarity.
#'   for categorical method, values are 0 or 1. for cosine method, values
#'   range from -1 to 1.
#'
#' @details
#' the function handles each similarity metric differently:
#' \itemize{
#'   \item correlation and euclidean methods are designed for continuous attributes
#'   \item categorical method should be used for discrete/factor attributes
#'   \item cosine similarity treats attributes as vectors, useful for normalized data
#' }
#'
#' missing values in attributes will propagate to the similarity matrix.
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
calculate_similarity_matrix <- function(attributes, method) {
	# convert factors/characters to numeric for categorical method
	if (method == "categorical" && !is.numeric(attributes)) {
		attributes <- as.numeric(as.factor(attributes))
	}

	# require numeric attributes
	if (!is.numeric(attributes)) {
		stop("Attributes must be numeric or convertible to numeric")
	}

	# call the cpp version
	calculate_similarity_matrix_cpp(attributes, method)
}

#' calculate homophily statistics with significance testing
#'
#' internal function that computes homophily correlation and related statistics
#' from similarity and network matrices. performs permutation-based significance
#' testing and bootstrap confidence intervals.
#'
#' @param similarity_matrix square numeric matrix of pairwise node similarities
#'   as produced by \code{calculate_similarity_matrix}.
#' @param net_matrix square network adjacency matrix. can be binary or weighted;
#'   will be converted to binary for analysis (positive values become 1).
#' @param significance_test logical. whether to perform permutation test for
#'   significance and compute confidence intervals.
#' @param n_permutations integer. number of permutations for significance testing
#'   and bootstrap confidence intervals.
#' @param alpha numeric between 0 and 1. significance level for confidence intervals.
#'   default 0.05 gives 95% confidence intervals.
#'
#' @return named list containing:
#'   \describe{
#'     \item{homophily_correlation}{pearson correlation between similarity scores
#'       and tie presence. positive values indicate homophily.}
#'     \item{mean_similarity_connected}{mean similarity among connected dyads.}
#'     \item{mean_similarity_unconnected}{mean similarity among unconnected dyads.}
#'     \item{similarity_difference}{difference between mean similarities
#'       (connected - unconnected).}
#'     \item{p_value}{two-tailed p-value from permutation test. na if test not performed.}
#'     \item{ci_lower}{lower bound of bootstrap confidence interval.}
#'     \item{ci_upper}{upper bound of bootstrap confidence interval.}
#'     \item{n_connected_pairs}{number of connected dyad pairs analyzed.}
#'     \item{n_unconnected_pairs}{number of unconnected dyad pairs analyzed.}
#'   }
#'
#' @details
#' the function:
#' \enumerate{
#'   \item converts the network to binary (tie/no tie)
#'   \item extracts upper triangle for undirected networks and all ordered
#'     off-diagonal dyads for directed networks
#'   \item calculates correlation between similarity and tie presence
#'   \item if requested, performs permutation test by shuffling similarities
#'   \item computes bootstrap confidence intervals by resampling dyad pairs
#' }
#'
#' diagonal elements (self-ties) are excluded from analysis. missing values
#' in either matrix are removed pairwise.
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
calculate_homophily_stats <- function(
	similarity_matrix, net_matrix,
	significance_test, n_permutations, alpha, directed = FALSE) {
	# convert network to logical matrix; non-zero counts as a tie
	binary_net <- binary_matrix_preserve_na(net_matrix)

	# mark diagonals for the cpp function
	diag(similarity_matrix) <- NA
	diag(binary_net) <- NA

	# call the cpp version
	calculate_homophily_stats_cpp(
		similarity_matrix,
			binary_net,
			significance_test,
			n_permutations,
			alpha,
			directed
	)
}

#' convert a matrix to logical tie indicators while preserving unknown dyads
#'
#' @keywords internal
#' @noRd
binary_matrix_preserve_na <- function(net_matrix) {
	binary_net <- matrix(NA,
		nrow = nrow(net_matrix), ncol = ncol(net_matrix),
		dimnames = dimnames(net_matrix))
	observed_dyads <- !is.na(net_matrix)
	binary_net[observed_dyads] <- net_matrix[observed_dyads] != 0
	binary_net
}
