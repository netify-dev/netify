#' Compare networks across time, layers, or attributes
#'
#' compares networks to identify similarities and differences. supports
#' comparison of edge patterns, structural properties, and node compositions
#' across multiple networks or within subgroups.
#'
#' @param nets either a list of netify objects to compare, or a single netify
#'   object (for longitudinal, multilayer, or by-group comparisons).
#' @param method character string specifying comparison method:
#'   \describe{
#'     \item{"correlation"}{pearson correlation of edge weights (default)}
#'     \item{"jaccard"}{jaccard similarity for binary networks}
#'     \item{"hamming"}{hamming distance (proportion of differing edges)}
#'     \item{"qap"}{quadratic assignment procedure with permutation test}
#'     \item{"spectral"}{spectral distance based on eigenvalue spectra. measures
#'       global structural differences by comparing the sorted eigenvalues of network
#'       laplacian matrices. useful for detecting fundamental structural changes.}
#'     \item{"all"}{applies all applicable methods}
#'   }
#' @param by character vector of nodal attributes. if specified, compares networks
#'   within and between attribute groups rather than across input networks.
#'   (note: currently not fully implemented)
#' @param what character string specifying what to compare:
#'   \describe{
#'     \item{"edges"}{edge-level comparison (default)}
#'     \item{"structure"}{structural properties (density, clustering, etc.)}
#'     \item{"nodes"}{node composition and attributes}
#'     \item{"attributes"}{compare networks based on node attribute distributions}
#'   }
#' @param test logical. whether to perform qap significance testing for
#'   edge comparisons when \code{method = "qap"} or \code{method = "all"}.
#'   other comparison metrics are returned as descriptive summaries.
#'   default TRUE.
#' @param n_permutations integer. number of permutations for qap test. default 5000.
#' @param include_diagonal logical. whether to include diagonal in comparison. default FALSE.
#' @param return_details logical. whether to return detailed comparison matrices. default FALSE.
#' @param edge_threshold numeric or function. for weighted networks, threshold to
#'   determine edge presence in jaccard, hamming, and edge-change summaries.
#'   default is 0 (positive weights are treated as present ties).
#' @param permutation_type character string specifying permutation scheme:
#'   \describe{
#'     \item{"classic"}{standard label permutation qap (default)}
#'     \item{"degree_preserving"}{preserves the first network's degree sequence
#'       for binary directed or symmetric networks}
#'   }
#' @param correlation_type character string. "pearson" (default) or "spearman" for
#'   rank-based edge correlation. qap significance testing currently uses pearson.
#' @param binary_metric character string for binary network correlation:
#'   \describe{
#'     \item{"phi"}{standard phi coefficient (default)}
#'     \item{"simple_matching"}{proportion of matching edges}
#'     \item{"mean_centered"}{mean-centered phi coefficient}
#'   }
#' @param seed integer. random seed for reproducible permutations. default NULL.
#' @param p_adjust character string for multiple testing correction:
#'   "none" (default), "holm", "bh" (benjamini-hochberg), or "by".
#' @param adaptive_stop logical. whether to use adaptive stopping for permutations.
#'   default FALSE. (currently not implemented)
#' @param alpha numeric. significance level for adaptive stopping. default 0.05.
#' @param max_permutations integer. maximum permutations for adaptive stopping. default 20000.
#' @param spectral_rank integer. number of eigenvalues to use for spectral distance.
#'   default 0 (use all). set to a smaller value (e.g., 50-100) for large networks
#'   to improve performance while maintaining accuracy.
#' @param attr_metric character string for continuous attribute comparison:
#'   \describe{
#'     \item{"ecdf_cor"}{correlation of empirical cdfs (default)}
#'     \item{"wasserstein"}{wasserstein-1 (earth mover's) distance}
#'   }
#' @param other_stats named list of custom functions to calculate additional
#'   comparison statistics. each function should accept a netify object (or matrix
#'   for edge comparisons) and return a named vector of scalar values. the specific
#'   input depends on the \code{what} parameter:
#'   \describe{
#'     \item{for \code{what = "edges"}}{functions receive adjacency matrices}
#'     \item{for \code{what = "structure"}}{functions receive netify objects}
#'     \item{for \code{what = "nodes"}}{functions receive netify objects}
#'     \item{for \code{what = "attributes"}}{functions receive netify objects}
#'   }
#'   example: \code{list(connectivity = function(net) \{ g <- to_igraph(net);
#'   c(vertex_conn = igraph::vertex_connectivity(g)) \})}
#'
#' @return a list of class "netify_comparison" containing:
#'   \describe{
#'     \item{comparison_type}{character string: "cross_network", "temporal", "multilayer", or "by_group"}
#'     \item{method}{comparison method(s) used}
#'     \item{n_networks}{number of networks compared}
#'     \item{summary}{data frame with comparison statistics}
#'     \item{edge_changes}{list detailing added, removed, and maintained edges (for edge comparisons)}
#'     \item{node_changes}{list detailing added, removed, and maintained nodes (for node comparisons)}
#'     \item{significance_tests}{qap test results when edge qap testing was run}
#'     \item{details}{detailed comparison matrices if return_details = TRUE}
#'     \item{comparisons}{long-format data frame for \code{what = "edges"}
#'       with one row per (network pair, metric) triple. columns:
#'       \code{net_i}, \code{net_j} (the two network names),
#'       \code{metric} (e.g. \code{"correlation"}, \code{"jaccard"},
#'       \code{"hamming"}, \code{"qap_correlation"}, \code{"spectral"},
#'       \code{"weight_correlation"}), \code{value} (scalar metric),
#'       \code{p_value} (only populated for \code{qap_correlation};
#'       \code{na} otherwise). coerce to a tibble with
#'       \code{tibble::as_tibble(comp)} (the \code{as_tibble} s3 method
#'       returns this frame directly).}
#'   }
#'
#' @details
#' the function supports four types of comparisons:
#'
#' \strong{edge comparison (what = "edges"):}
#' compares edge patterns between networks using correlation, jaccard similarity,
#' hamming distance, spectral distance, or qap permutation tests. returns detailed
#' edge changes showing which edges are added, removed, or maintained between networks.
#'
#' \strong{structure comparison (what = "structure"):}
#' compares network-level properties like density, reciprocity, transitivity, and
#' mean degree. for two networks, also provides percent change calculations.
#'
#' \strong{node comparison (what = "nodes"):}
#' analyzes changes in actor composition between networks, tracking which actors
#' enter or exit the network.
#'
#' \strong{attribute comparison (what = "attributes"):}
#' compares distributions of node attributes across networks using appropriate
#' statistical tests (ks test for continuous, total variation distance for categorical).
#'
#' \strong{automatic handling of longitudinal data:}
#' when passed a single longitudinal netify object, the function automatically
#' extracts time periods and performs pairwise comparisons between them.
#'
#' \strong{automatic handling of multilayer networks:}
#' when passed a single multilayer netify object (created with \code{layer_netify()}),
#' the function automatically extracts layers and performs pairwise comparisons between
#' them. this works for cross-sectional multilayer (3d arrays), longitudinal multilayer
#' (4d arrays), and longitudinal list multilayer formats.
#'
#' \strong{summary statistics interpretation:}
#' \itemize{
#'   \item correlation: ranges from -1 to 1, measuring linear relationship between edge weights
#'   \item jaccard: ranges from 0 to 1, proportion of edges present in both networks
#'   \item hamming: ranges from 0 to 1, proportion of differing edges
#'   \item qap p-value: observed pearson edge correlation compared to the selected permutation reference distribution
#' }
#'
#' \strong{permutation methods:}
#' when \code{permutation_type = "degree_preserving"}, the first network must be
#' binary (0/1). directed inputs use directed edge swaps that preserve in- and
#' out-degree. symmetric inputs use undirected edge swaps that preserve the
#' undirected degree sequence.
#' @examples
#' # load example data
#' data(icews)
#'
#' # create networks for different years
#' net_2002 <- netify(icews[icews$year == 2002, ],
#'     actor1 = "i", actor2 = "j",
#'     weight = "matlConf"
#' )
#' net_2003 <- netify(icews[icews$year == 2003, ],
#'     actor1 = "i", actor2 = "j",
#'     weight = "matlConf"
#' )
#'
#' # basic edge comparison
#' comp <- compare_networks(list("2002" = net_2002, "2003" = net_2003))
#' print(comp)
#'
#' # structural comparison
#' struct_comp <- compare_networks(
#'     list(net_2002, net_2003),
#'     what = "structure"
#' )
#'
#' \donttest{
#' # create longitudinal network for automatic temporal comparison
#' longit_net <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j",
#'     time = "year",
#'     weight = "verbCoop",
#'     output_format = "longit_list"
#' )
#'
#' # automatic temporal comparison
#' temporal_comp <- compare_networks(longit_net, method = "all")
#'
#' # create multilayer network example
#' verbal_coop <- netify(
#'     icews[icews$year == 2010, ],
#'     actor1 = "i", actor2 = "j",
#'     weight = "verbCoop"
#' )
#'
#' material_coop <- netify(
#'     icews[icews$year == 2010, ],
#'     actor1 = "i", actor2 = "j",
#'     weight = "matlCoop"
#' )
#'
#' # combine into multilayer network
#' multilayer <- layer_netify(
#'     list(verbal = verbal_coop, material = material_coop)
#' )
#'
#' # automatic multilayer comparison
#' layer_comp <- compare_networks(multilayer, method = "all")
#' print(layer_comp)
#'
#' # get detailed matrices
#' detailed_comp <- compare_networks(
#'     list(net_2002, net_2003),
#'     return_details = TRUE
#' )
#' names(detailed_comp$details) # shows available matrices
#' }
#'
#' # compare with custom statistics
#' \dontrun{
#' library(igraph)
#'
#' # define custom connectivity function
#' connectivity_stats <- function(net) {
#'     g <- to_igraph(net)
#'     c(vertex_connectivity = vertex_connectivity(g),
#'       edge_connectivity = edge_connectivity(g),
#'       diameter = diameter(g, directed = FALSE))
#' }
#'
#' # apply to structural comparison
#' struct_comp_custom <- compare_networks(
#'     list("2002" = net_2002, "2003" = net_2003),
#'     what = "structure",
#'     other_stats = list(connectivity = connectivity_stats)
#' )
#'
#' # custom stats will appear in the summary
#' print(struct_comp_custom$summary)
#' }
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export compare_networks

compare_networks <- function(
	nets,
	method = "correlation",
	by = NULL,
	what = "edges",
	test = TRUE,
	n_permutations = 5000,
	include_diagonal = FALSE,
	return_details = FALSE,
	edge_threshold = 0,
	permutation_type = c("classic", "degree_preserving"),
	correlation_type = c("pearson", "spearman"),
	binary_metric = c("phi", "simple_matching", "mean_centered"),
	seed = NULL,
	p_adjust = c("none", "holm", "BH", "BY"),
	adaptive_stop = FALSE,
	alpha = 0.05,
	max_permutations = 20000,
	spectral_rank = 0,
	attr_metric = c("ecdf_cor", "wasserstein"),
	other_stats = NULL) {
	# match arguments
	permutation_type <- match.arg(permutation_type)
	correlation_type <- match.arg(correlation_type)
	binary_metric <- match.arg(binary_metric)
	p_adjust <- match.arg(p_adjust)
	attr_metric <- match.arg(attr_metric)

	if (!is.null(seed)) {
		restore_rng <- save_rng_state()
		on.exit(restore_rng(), add = TRUE)
		set.seed(seed)
	}

	# input validation
	if (!is.list(nets) && !is_netify(nets)) {
		cli::cli_abort("Input must be a list of netify objects or a single netify object")
	}

	# convert single netify to list if doing by-group comparison
	if (is_netify(nets) && !is.null(by)) {
		nets_list <- prepare_by_group_networks(nets, by)
		comparison_type <- "by_group"
	} else if (is_netify(nets)) {
		# single netify without 'by' - extract time periods or layers
		nets_list <- extract_network_list(nets)

		# determine comparison type based on what was extracted
		attrs <- attributes(nets)
		# check if this is a true multilayer network (not just having layers="true")
		is_multilayer <- !is.null(attrs$layers) &&
			is.character(attrs$layers) &&
			length(attrs$layers) > 1

		if (is_multilayer) {
			comparison_type <- "multilayer"
		} else if (attrs$netify_type %in% c("longit_array", "longit_list")) {
			comparison_type <- "temporal"
		} else {
			comparison_type <- "cross_network"
		}
	} else {
		# list of netify objects
		nets_list <- nets
		comparison_type <- "cross_network"
		# validate all elements are netify
		if (!all(sapply(nets_list, is_netify))) {
			cli::cli_abort("All elements of nets must be netify objects")
		}
	}

	# make sure we actually have things to compare
	if (length(nets_list) < 2) {
		cli::cli_abort("Need at least 2 networks to compare")
	}

	# validate params
	checkmate::assert_choice(method, c("correlation", "jaccard", "hamming", "qap", "spectral", "all"))
	checkmate::assert_choice(what, c("edges", "structure", "nodes", "attributes"))
	checkmate::assert_logical(test, len = 1)
	checkmate::assert_count(n_permutations, positive = TRUE)
	checkmate::assert_logical(include_diagonal, len = 1)
	checkmate::assert_logical(return_details, len = 1)

	# validate other_stats parameter
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

	# init results
	results <- list(
		comparison_type = comparison_type,
		comparison_method = method,  # store the algorithm (correlation, jaccard, etc.)
		what = what,
		n_networks = length(nets_list)
	)

	#
	if (what == "edges") {
		comp_results <- compare_edges(
			nets_list, method, test, n_permutations,
			include_diagonal, edge_threshold, return_details,
			permutation_type, correlation_type, binary_metric,
			p_adjust, adaptive_stop, alpha, max_permutations, seed_used = seed,
			spectral_rank = spectral_rank, other_stats = other_stats
		)
	} else if (what == "structure") {
		comp_results <- compare_structure(nets_list, test, other_stats = other_stats)
	} else if (what == "nodes") {
		comp_results <- compare_nodes(nets_list, return_details, other_stats = other_stats)
	} else if (what == "attributes") {
		comp_results <- compare_attributes(nets_list, test, n_permutations, return_details, attr_metric, other_stats = other_stats)
	}

	# merge comparison results with initial results
	# the comp_results will have its own 'method' field (the comparison
	# type like "structural_comparison" / "qap") that the print method
	# expects; drop any keys that would duplicate top-level entries to
	# avoid two `n_networks` / `method` slots in the returned list.
	dup_keys <- intersect(names(comp_results), names(results))
	# preserve `method` since print.netify_comparison renders it as the
	# inner method label, distinct from the top-level `comparison_method`
	dup_keys <- setdiff(dup_keys, "method")
	if (length(dup_keys)) {
		comp_results <- comp_results[setdiff(names(comp_results), dup_keys)]
	}
	results <- c(results, comp_results)

	#
	results$comparison_type <- comparison_type

	#
	if (!is.null(by) && comparison_type == "by_group") {
		results$by_group <- analyze_by_group(results, nets_list, by)
	}

	# build a tidy long-format per-pair comparison frame (one row per
	# net_i, net_j, metric). useful for downstream filtering/plotting,
	# especially in temporal / multilayer / by-group settings where the
	# wide $summary either collapses across pairs or has many columns.
	if (what == "edges") {
		results$comparisons <- build_comparisons_frame(results, nets_list)
	}

	#
	class(results) <- c("netify_comparison", "list")

	#
	return(results)
}
