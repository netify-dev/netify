#' Calculate graph-level statistics for netify objects
#'
#' Computes comprehensive graph-level statistics for netify objects, including 
#' density, reciprocity, centralization measures, and custom metrics. Handles 
#' cross-sectional and longitudinal networks, as well as multilayer structures.
#'
#' @param object A netify object containing network data
#' @param ... Additional arguments, including:
#'   \describe{
#'     \item{\code{other_stats}}{Named list of custom functions to calculate 
#'       additional graph-level statistics. Each function should accept a matrix 
#'       and return a named vector of scalar values.}
#'   }
#'
#' @return A data frame with one row per network/time period containing:
#'   
#'   \strong{Basic network properties:}
#'   \describe{
#'     \item{\code{net}}{Network/time identifier}
#'     \item{\code{layer}}{Layer name (for multilayer networks)}
#'     \item{\code{num_actors}}{Number of actors (or \code{num_row_actors} and 
#'       \code{num_col_actors} for bipartite networks)}
#'     \item{\code{density}}{Proportion of possible ties that exist}
#'     \item{\code{num_edges}}{Total number of edges (unweighted count)}
#'     \item{\code{prop_edges_missing}}{Proportion of potential edges that are NA}
#'   }
#'   
#'   \strong{For weighted networks only:}
#'   \describe{
#'     \item{\code{mean_edge_weight}}{Average weight of existing edges}
#'     \item{\code{sd_edge_weight}}{Standard deviation of edge weights}
#'     \item{\code{median_edge_weight}}{Median edge weight}
#'     \item{\code{min_edge_weight}, \code{max_edge_weight}}{Range of edge weights}
#'   }
#'   
#'   \strong{Structural measures:}
#'   \describe{
#'     \item{\code{competition} (or \code{competition_row}/\code{competition_col})}{
#'       Herfindahl-Hirschman Index measuring concentration of ties. Calculated as 
#'       \eqn{\sum_{i=1}^{n} (s_i)^2} where \eqn{s_i} is actor i's share of total 
#'       ties. Ranges from 1/n (equal distribution) to 1 (one actor has all ties).}
#'     \item{\code{sd_of_actor_means} (or \code{sd_of_row_means}/\code{sd_of_col_means})}{
#'       Standard deviation of actors' average tie strengths, measuring heterogeneity 
#'       in actor activity levels}
#'     \item{\code{transitivity}}{Global clustering coefficient (probability that 
#'       two neighbors of a node are connected)}
#'   }
#'   
#'   \strong{For directed networks only:}
#'   \describe{
#'     \item{\code{covar_of_row_col_means}}{Covariance between actors' sending and 
#'       receiving patterns}
#'     \item{\code{reciprocity}}{Correlation between adjacency matrix and its 
#'       transpose, measuring tendency for mutual ties}
#'   }
#'
#' @details
#' The function automatically adapts calculations based on network properties:
#' \itemize{
#'   \item \strong{Bipartite networks}: Reports row and column actors separately
#'   \item \strong{Directed networks}: Calculates separate statistics for in/out ties
#'   \item \strong{Weighted networks}: Includes weight-based statistics
#'   \item \strong{Multilayer networks}: Processes each layer independently
#'   \item \strong{Longitudinal networks}: Calculates statistics for each time period
#' }
#' 
#' \strong{Competition Index Interpretation:}
#' 
#' The competition measure (HHI) captures how concentrated network ties are among 
#' actors. Low values indicate distributed activity across many actors (competitive), 
#' while high values indicate concentration among few actors (monopolistic). This 
#' is particularly useful for analyzing power dynamics or resource distribution in 
#' networks.
#' 
#' \strong{Custom Statistics:}
#' 
#' Add custom graph-level metrics using the \code{other_stats} parameter:
#' 
#' \preformatted{
#' # Example: Community detection
#' modularity_stat <- function(mat) {
#'   g <- netify_to_igraph(mat)
#'   comm <- igraph::cluster_walktrap(g)
#'   c(modularity = igraph::modularity(comm),
#'     n_communities = length(unique(comm$membership)))
#' }
#' 
#' summary(net, other_stats = list(community = modularity_stat))
#' }
#'
#' @note 
#' For large longitudinal or multilayer networks, computation can be intensive. 
#' Consider using \code{\link{subset_netify}} to analyze specific time periods 
#' or layers.
#' 
#' Missing edges (NA values) are excluded from density calculations but tracked 
#' in the \code{prop_edges_missing} statistic.
#'
#' @references
#' Dorff, C., Gallop, M., & Minhas, S. (2023). "Networks of violence: Predicting 
#' conflict in Nigeria." \emph{Journal of Politics}, 85(1).
#'
#' @examples
#' # Load example data
#' data(icews)
#'
#' # Basic usage
#' net <- netify(
#'   icews, 
#'   actor1 = 'i', actor2 = 'j', time = 'year',
#'   symmetric = FALSE, 
#'   weight = 'verbCoop'
#' )
#' 
#' # get summary
#' summary(net)
#'
#' \dontrun{
#' # Add custom statistics - community detection
#' comm_stats <- function(mat) {
#'   g <- netify_to_igraph(mat)
#'   comm <- igraph::cluster_spinglass(g)
#'   c(n_communities = length(comm$csize),
#'     modularity = comm$modularity)
#' }
#' 
#' # Apply to subset for efficiency
#' sub_net <- subset_netify(net, time = as.character(2013:2014))
#' summary(sub_net, other_stats = list(community = comm_stats))
#' }
#' 
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @importFrom igraph transitivity
#' 
#' @export

summary.netify <- function(object, ...){

	######################
	# prelim checks
	# check if netify object
	netify_check(object)

	# get summary args
	summary_args <- list(...)

	# cache attributes
	obj_attrs <- attributes(object)
	layers <- obj_attrs$layers
	n_layers <- length(layers)
	netlet_type <- obj_attrs$netify_type
	is_cross_sec <- netlet_type == 'cross_sec'
	is_symmetric <- obj_attrs$symmetric
	is_unipartite <- obj_attrs$mode == 'unipartite'
	is_all_binary <- all(obj_attrs$weight_binary)
	
	# Check for ego network
	ego_netlet <- !is.null(obj_attrs$ego_netlet) && obj_attrs$ego_netlet
	if(ego_netlet) {
		ego_vec <- obj_attrs$ego_vec
		ego_longit <- obj_attrs$ego_longit
	}
	######################

	######################
	# Pre-allocate results list
	net_stats_l_multi <- vector("list", n_layers)
	
	# iterate through each layer
	for(i in seq_along(layers)){
		layer <- layers[i]
		
		######################
		# Extract layer efficiently
		if(n_layers == 1){ 
			netlet <- object 
		} else {
			netlet <- subset_netify(object, layers = layer)
			obj_attrs <- attributes(netlet)
		}
		
		# Convert to list format for processing
		netlet_list <- switch(netlet_type,
			'cross_sec' = list(netlet),
			'longit_array' = array_to_list(netlet),
			'longit_list' = netlet
		)
		######################		

		######################
		# calc stats across netlet(s) - vectorized where possible
		net_stats_list <- lapply(netlet_list, function(mat){
			graph_stats_for_netlet(mat, obj_attrs, summary_args)
		})
		######################

		######################
		# Efficient data frame creation
		net_stats <- do.call('rbind', net_stats_list)
		net_names <- names(netlet_list) %||% rownames(net_stats)
		
		net_stats <- as.data.frame(net_stats, stringsAsFactors = FALSE)
		net_stats$net <- net_names
		net_stats$layer <- layer
		
		# Store in pre-allocated list
		net_stats_l_multi[[i]] <- net_stats
	}
	######################

	######################
	# Combine all results efficiently
	net_stats <- do.call('rbind', net_stats_l_multi)
	rownames(net_stats) <- NULL
	
	# Reorder columns - check if columns exist first
	id_cols <- c('net', 'layer')
	# Only include id_cols that actually exist in net_stats
	existing_id_cols <- id_cols[id_cols %in% names(net_stats)]
	stat_cols <- setdiff(names(net_stats), id_cols)

	# Only reorder if we have the expected columns
	if(length(existing_id_cols) > 0) {
		net_stats <- net_stats[, c(existing_id_cols, stat_cols)]
	}
	
	# Drop layer column if only one layer
	if(n_layers == 1){
		net_stats$layer <- NULL
	}

	# Simplify column names based on network type
	if(is_unipartite){
		# Rename num_row_actors to num_actors
		names(net_stats)[names(net_stats) == 'num_row_actors'] <- 'num_actors'
		# Remove num_col_actors
		net_stats$num_col_actors <- NULL
	}

	# Simplify for undirected networks
	if(is_symmetric){
		# Rename competition and sd measures
		names(net_stats)[names(net_stats) == 'competition_row'] <- 'competition'
		names(net_stats)[names(net_stats) == 'sd_of_row_means'] <- 'sd_of_actor_means'
		
		# Remove directed-only statistics
		cols_to_remove <- c('competition_col', 'sd_of_col_means', 
		                   'covar_of_row_col_means', 'reciprocity')
		net_stats[cols_to_remove] <- NULL
	}

	# Remove weight statistics for binary networks
	if(is_all_binary){
		weight_cols <- c('mean_edge_weight', 'sd_edge_weight', 
		                'min_edge_weight', 'max_edge_weight', 
		                'median_edge_weight')
		net_stats[weight_cols] <- NULL
	}
	######################

	######################
	# Handle ego networks efficiently
	if(ego_netlet){
		if(!ego_longit){
			# Non-longitudinal ego network
			if(is_cross_sec){
				net_stats$layer <- ego_vec
			} else {
				net_stats$layer <- net_stats$net
			}
			net_stats$net <- 1
		} else {
			# Longitudinal ego network
			net_split <- strsplit(net_stats$net, '__', fixed = TRUE)
			ego_units <- vapply(net_split, `[`, character(1), 1)
			ego_pds <- vapply(net_split, `[`, character(1), 2)
			
			net_stats$net <- ego_pds
			net_stats$layer <- ego_units
		}
		
		# Ensure correct column order
		net_stats <- net_stats[, c('net', 'layer', 
		                           setdiff(names(net_stats), c('net', 'layer')))]
	}
	######################	

	######################	
	return(net_stats)
	######################		
}
