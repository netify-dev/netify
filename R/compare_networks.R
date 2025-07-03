#' Compare networks across time, layers, or attributes
#'
#' Performs comprehensive comparison of networks to identify similarities and
#' differences. Supports comparison of edge patterns, structural properties,
#' and node compositions across multiple networks or within subgroups.
#'
#' @param nets Either a list of netify objects to compare, or a single netify
#'   object (for longitudinal, multilayer, or by-group comparisons).
#' @param method Character string specifying comparison method:
#'   \describe{
#'     \item{"correlation"}{Pearson correlation of edge weights (default)}
#'     \item{"jaccard"}{Jaccard similarity for binary networks}
#'     \item{"hamming"}{Hamming distance (proportion of differing edges)}
#'     \item{"qap"}{Quadratic Assignment Procedure with permutation test}
#'     \item{"spectral"}{Spectral distance based on eigenvalue spectra. Measures
#'       global structural differences by comparing the sorted eigenvalues of network
#'       Laplacian matrices. Useful for detecting fundamental structural changes.}
#'     \item{"all"}{Applies all applicable methods}
#'   }
#' @param by Character vector of nodal attributes. If specified, compares networks
#'   within and between attribute groups rather than across input networks.
#'   (Note: Currently not fully implemented)
#' @param what Character string specifying what to compare:
#'   \describe{
#'     \item{"edges"}{Edge-level comparison (default)}
#'     \item{"structure"}{Structural properties (density, clustering, etc.)}
#'     \item{"nodes"}{Node composition and attributes}
#'     \item{"attributes"}{Compare networks based on node attribute distributions}
#'   }
#' @param test Logical. Whether to perform significance testing. Default TRUE.
#' @param n_permutations Integer. Number of permutations for QAP test. Default 5000.
#' @param include_diagonal Logical. Whether to include diagonal in comparison. Default FALSE.
#' @param return_details Logical. Whether to return detailed comparison matrices. Default FALSE.
#' @param edge_threshold Numeric or function. For weighted networks, threshold to
#'   determine edge presence. Default is 0 (any positive weight).
#' @param permutation_type Character string specifying permutation scheme:
#'   \describe{
#'     \item{"classic"}{Standard label permutation QAP (default)}
#'     \item{"degree_preserving"}{Preserves degree sequence (binary networks only)}
#'     \item{"freedman_lane"}{Freeman-Lane MRQAP for controlling autocorrelation}
#'     \item{"dsp_mrqap"}{Double-Semi-Partialling MRQAP}
#'   }
#' @param correlation_type Character string. "pearson" (default) or "spearman" for 
#'   rank-based correlation.
#' @param binary_metric Character string for binary network correlation:
#'   \describe{
#'     \item{"phi"}{Standard phi coefficient (default)}
#'     \item{"simple_matching"}{Proportion of matching edges}
#'     \item{"mean_centered"}{Mean-centered phi coefficient}
#'   }
#' @param seed Integer. Random seed for reproducible permutations. Default NULL.
#' @param p_adjust Character string for multiple testing correction:
#'   "none" (default), "holm", "BH" (Benjamini-Hochberg), or "BY".
#' @param adaptive_stop Logical. Whether to use adaptive stopping for permutations.
#'   Default FALSE. (Currently not implemented)
#' @param alpha Numeric. Significance level for adaptive stopping. Default 0.05.
#' @param max_permutations Integer. Maximum permutations for adaptive stopping. Default 20000.
#' @param spectral_rank Integer. Number of eigenvalues to use for spectral distance.
#'   Default 0 (use all). Set to a smaller value (e.g., 50-100) for large networks
#'   to improve performance while maintaining accuracy.
#' @param attr_metric Character string for continuous attribute comparison:
#'   \describe{
#'     \item{"ecdf_cor"}{Correlation of empirical CDFs (default)}
#'     \item{"wasserstein"}{Wasserstein-1 (Earth Mover's) distance}
#'   }
#' @param other_stats Named list of custom functions to calculate additional
#'   comparison statistics. Each function should accept a netify object (or matrix
#'   for edge comparisons) and return a named vector of scalar values. The specific
#'   input depends on the \code{what} parameter:
#'   \describe{
#'     \item{For \code{what = "edges"}}{Functions receive adjacency matrices}
#'     \item{For \code{what = "structure"}}{Functions receive netify objects}
#'     \item{For \code{what = "nodes"}}{Functions receive netify objects}
#'     \item{For \code{what = "attributes"}}{Functions receive netify objects}
#'   }
#'   Example: \code{list(connectivity = function(net) \{ g <- to_igraph(net); 
#'   c(vertex_conn = igraph::vertex_connectivity(g)) \})}
#'
#' @return A list of class "netify_comparison" containing:
#'   \describe{
#'     \item{comparison_type}{Character string: "cross_network", "temporal", "multilayer", or "by_group"}
#'     \item{method}{Comparison method(s) used}
#'     \item{n_networks}{Number of networks compared}
#'     \item{summary}{Data frame with comparison statistics}
#'     \item{edge_changes}{List detailing added, removed, and maintained edges (for edge comparisons)}
#'     \item{node_changes}{List detailing added, removed, and maintained nodes (for node comparisons)}
#'     \item{significance_tests}{QAP test results if test = TRUE}
#'     \item{details}{Detailed comparison matrices if return_details = TRUE}
#'   }
#'
#' @details
#' The function supports four types of comparisons:
#'
#' \strong{Edge comparison (what = "edges"):}
#' Compares edge patterns between networks using correlation, Jaccard similarity,
#' Hamming distance, spectral distance, or QAP permutation tests. Returns detailed
#' edge changes showing which edges are added, removed, or maintained between networks.
#'
#' \strong{Structure comparison (what = "structure"):}
#' Compares network-level properties like density, reciprocity, transitivity, and
#' mean degree. For two networks, also provides percent change calculations.
#'
#' \strong{Node comparison (what = "nodes"):}
#' Analyzes changes in actor composition between networks, tracking which actors
#' enter or exit the network.
#'
#' \strong{Attribute comparison (what = "attributes"):}
#' Compares distributions of node attributes across networks using appropriate
#' statistical tests (KS test for continuous, total variation distance for categorical).
#'
#' \strong{Automatic handling of longitudinal data:}
#' When passed a single longitudinal netify object, the function automatically
#' extracts time periods and performs pairwise comparisons between them.
#'
#' \strong{Automatic handling of multilayer networks:}
#' When passed a single multilayer netify object (created with \code{layer_netify()}),
#' the function automatically extracts layers and performs pairwise comparisons between
#' them. This works for cross-sectional multilayer (3D arrays), longitudinal multilayer
#' (4D arrays), and longitudinal list multilayer formats.
#'
#' \strong{Summary statistics interpretation:}
#' \itemize{
#'   \item Correlation: Ranges from -1 to 1, measuring linear relationship between edge weights
#'   \item Jaccard: Ranges from 0 to 1, proportion of edges present in both networks
#'   \item Hamming: Ranges from 0 to 1, proportion of differing edges
#'   \item QAP p-value: Significance of observed correlation under random permutation
#' }
#' 
#' \strong{Permutation methods:}
#' When \code{permutation_type = "degree_preserving"}, the first network must be 
#' binary (0/1). This method preserves the degree sequence during permutations, 
#' which is important when degree heterogeneity could inflate Type I error rates.
#'
#' @examples
#' # Load example data
#' data(icews)
#'
#' # Create networks for different years
#' net_2002 <- netify(icews[icews$year == 2002, ],
#'     actor1 = "i", actor2 = "j",
#'     weight = "matlConf"
#' )
#' net_2003 <- netify(icews[icews$year == 2003, ],
#'     actor1 = "i", actor2 = "j",
#'     weight = "matlConf"
#' )
#'
#' # Basic edge comparison
#' comp <- compare_networks(list("2002" = net_2002, "2003" = net_2003))
#' print(comp)
#'
#' # Structural comparison
#' struct_comp <- compare_networks(
#'     list(net_2002, net_2003),
#'     what = "structure"
#' )
#'
#' # Create longitudinal network for automatic temporal comparison
#' longit_net <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j",
#'     time = "year",
#'     weight = "verbCoop",
#'     output_format = "longit_list"
#' )
#'
#' # Automatic temporal comparison
#' temporal_comp <- compare_networks(longit_net, method = "all")
#'
#' # Create multilayer network example
#' \dontrun{
#' # Create separate networks for different interaction types
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
#' # Combine into multilayer network
#' multilayer <- layer_netify(
#'     list(verbal = verbal_coop, material = material_coop)
#' )
#'
#' # Automatic multilayer comparison
#' layer_comp <- compare_networks(multilayer, method = "all")
#' print(layer_comp)
#' # Will show comparison between verbal and material cooperation layers
#' }
#'
#' # Get detailed matrices
#' detailed_comp <- compare_networks(
#'     list(net_2002, net_2003),
#'     return_details = TRUE
#' )
#' names(detailed_comp$details) # Shows available matrices
#'
#' # Compare with custom statistics
#' \dontrun{
#' library(igraph)
#' 
#' # Define custom connectivity function
#' connectivity_stats <- function(net) {
#'     g <- to_igraph(net)
#'     c(vertex_connectivity = vertex_connectivity(g),
#'       edge_connectivity = edge_connectivity(g),
#'       diameter = diameter(g, directed = FALSE))
#' }
#' 
#' # Apply to structural comparison
#' struct_comp_custom <- compare_networks(
#'     list("2002" = net_2002, "2003" = net_2003),
#'     what = "structure",
#'     other_stats = list(connectivity = connectivity_stats)
#' )
#' 
#' # Custom stats will appear in the summary
#' print(struct_comp_custom$summary)
#' }
#'
#' @author Cassy Dorff, Shahryar Minhas
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
    permutation_type = c("classic", "degree_preserving", 
                         "freedman_lane", "dsp_mrqap"),
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
    
    # RNG handling - generate seed if not provided for reproducibility
    if (is.null(seed)) {
        seed <- sample.int(.Machine$integer.max, 1)
    }
    
    # Save and restore RNG state
    old_rng <- if (exists(".Random.seed", .GlobalEnv)) .Random.seed else NULL
    set.seed(seed)
    on.exit({
        if (!is.null(old_rng)) {
            assign(".Random.seed", old_rng, envir = .GlobalEnv)
        } else if (exists(".Random.seed", .GlobalEnv)) {
            rm(.Random.seed, envir = .GlobalEnv)
        }
    })
    
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
        # check if this is a true multilayer network (not just having layers="TRUE")
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
        # calidate all elements are netify
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
        if (!is.list(other_stats) || is.null(names(other_stats))) {
            cli::cli_abort("other_stats must be a named list of functions")
        }
        if (!all(sapply(other_stats, is.function))) {
            cli::cli_abort("All elements of other_stats must be functions")
        }
    }

    # init results
    results <- list(
        comparison_type = comparison_type,
        comparison_method = method,  # Store the algorithm (correlation, jaccard, etc.)
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

    # Merge comparison results with initial results
    # The comp_results will have its own 'method' field that indicates the comparison type
    results <- c(results, comp_results)
    
    #
    results$comparison_type <- comparison_type

    #
    if (!is.null(by) && comparison_type == "by_group") {
        results$by_group <- analyze_by_group(results, nets_list, by)
    }

    #
    class(results) <- c("netify_comparison", "list")

    #
    return(results)
}
