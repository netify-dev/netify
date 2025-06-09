#' Calculate actor-level network statistics
#'
#' Computes comprehensive actor-level statistics for netify objects, including 
#' degree, strength, centrality measures, and custom metrics. Handles different 
#' network types (directed/undirected, weighted/unweighted) appropriately.
#'
#' @param netlet A netify object containing network data
#' @param invert_weights_for_igraph Logical. If TRUE (default), inverts edge weights 
#'   when calculating closeness and betweenness centrality, as igraph interprets 
#'   weights as distances. Set to FALSE if your weights already represent distances.
#' @param other_stats Optional named list of custom functions to calculate additional 
#'   actor-level statistics. Each function should accept a matrix and return a 
#'   vector with one value per actor.
#'
#' @return A data frame with actor-level statistics. Columns always include:
#'   \describe{
#'     \item{\code{actor}}{Actor name/identifier}
#'     \item{\code{time}}{Time period (for longitudinal networks)}
#'     \item{\code{layer}}{Layer name (for multilayer networks)}
#'   }
#'   
#'   Additional columns depend on network type:
#'   
#'   \strong{For undirected networks:}
#'   \describe{
#'     \item{\code{degree}}{Number of connections. Calculated as \eqn{d_i = \sum_{j=1}^{n} a_{ij}}, where \eqn{a_{ij}} is the adjacency matrix element.}
#'     \item{\code{prop_ties}}{Proportion of possible ties realized. Calculated as \eqn{p_i = \frac{d_i}{n-1}}, where \eqn{d_i} is the degree and \eqn{n} is the total number of actors.}
#'     \item{\code{net_share}}{Actor's share of total network connections. Calculated as \eqn{s_i = \frac{d_i}{\sum_{j=1}^{n} d_j}}.}
#'     \item{\code{closeness}}{Closeness centrality. Calculated as \eqn{C_i = \frac{1}{\sum_{j} d(i, j)}}, where \eqn{d(i, j)} is the shortest path distance to every other actor \eqn{j}.}
#'     \item{\code{betweenness}}{Betweenness centrality. Calculated as \eqn{B_i = \sum_{s \neq i \neq t} \frac{\sigma_{st}(i)}{\sigma_{st}}}, where \eqn{\sigma_{st}} is the total number of shortest paths from node \eqn{s} to node \eqn{t} and \eqn{\sigma_{st}(i)} is the number of those paths that pass through \eqn{i}.}
#'     \item{\code{eigen_centrality}}{Eigenvector centrality, based on the principal eigenvector of the adjacency matrix.}
#'   }
#'   
#'   \strong{For directed networks:}
#'   \describe{
#'     \item{\code{in_degree}, \code{out_degree}}{Incoming and outgoing connections. \eqn{d_i^{in} = \sum_{j=1}^{n} a_{ji}} and \eqn{d_i^{out} = \sum_{j=1}^{n} a_{ij}}.}
#'     \item{\code{in_prop_ties}, \code{out_prop_ties}}{Proportion of possible in/out ties}
#'     \item{\code{total_degree}}{Sum of in and out degree}
#'     \item{\code{in_closeness}, \code{out_closeness}}{Directed closeness centrality}
#'     \item{\code{betweenness}}{Betweenness centrality}
#'     \item{\code{authority}, \code{hub}}{Authority and hub scores (asymmetric only)}
#'   }
#'   
#'   \strong{For weighted networks, additional columns:}
#'   \describe{
#'     \item{\code{strength_sum}}{Sum of edge weights. For undirected: \eqn{s_i^{sum} = \sum_{j=1}^{n} w_{ij}}. For directed: separate in/out sums.}
#'     \item{\code{strength_avg}}{Average edge weight. Calculated as \eqn{s_i^{avg} = \frac{s_i^{sum}}{d_i}}.}
#'     \item{\code{strength_sd}}{Standard deviation of edge weights. Calculated as \eqn{s_i^{sd} = \sqrt{\frac{1}{d_i} \sum_{j=1}^{n} (w_{ij} - s_i^{avg})^2}}.}
#'     \item{\code{strength_median}}{Median edge weight}
#'   }
#'
#' @details
#' The function automatically adapts calculations based on network properties:
#' 
#' \strong{Centrality Measures:}
#' \itemize{
#'   \item \strong{Degree}: Count of direct connections. For directed networks, 
#'     calculated separately for incoming (in-degree) and outgoing (out-degree) ties.
#'   \item \strong{Closeness}: Measures how quickly an actor can reach all others. 
#'     Based on the inverse of the sum of shortest path distances.
#'   \item \strong{Betweenness}: Measures how often an actor lies on shortest paths 
#'     between other actors, indicating brokerage potential.
#'   \item \strong{Eigenvector}: Measures importance based on connections to other 
#'     important actors. Computed using the principal eigenvector of the adjacency matrix.
#'   \item \strong{Authority/Hub}: For directed networks only. Authority scores measure 
#'     importance as targets of ties from important sources. Hub scores measure 
#'     importance as sources of ties to important targets.
#' }
#' 
#' \strong{Weight Handling:}
#' 
#' By default, the function assumes larger weights indicate stronger relationships. 
#' When calculating closeness and betweenness centrality, weights are inverted 
#' (1/weight) because igraph treats edge weights as distances. For distance-based 
#' networks where larger values already represent distances or weaker relationships, 
#' set \code{invert_weights_for_igraph = FALSE}.
#' 
#' \strong{Custom Statistics:}
#' 
#' Add custom metrics using the \code{other_stats} parameter. Each function receives 
#' the adjacency matrix and should return a vector with one value per actor:
#' 
#' \preformatted{
#' # Example: Maximum tie weight for each actor
#' max_out <- function(mat) apply(mat, 1, max, na.rm = TRUE)
#' max_in <- function(mat) apply(mat, 2, max, na.rm = TRUE)
#' 
#' stats <- summary_actor(net, 
#'   other_stats = list(max_out = max_out, max_in = max_in))
#' }
#' 
#' \strong{Mathematical Formulations:}
#' 
#' For symmetric unweighted networks:
#' \itemize{
#'   \item Degree: \eqn{d_i = \sum_{j=1}^{n} a_{ij}}
#'   \item Proportion of ties: \eqn{p_i = \frac{d_i}{n-1}}
#'   \item Network share: \eqn{s_i = \frac{d_i}{\sum_{j=1}^{n} d_j}}
#'   \item Closeness: \eqn{C_i = \frac{1}{\sum_{j} d(i, j)}}
#'   \item Betweenness: \eqn{B_i = \sum_{s \neq i \neq t} \frac{\sigma_{st}(i)}{\sigma_{st}}}
#' }
#' 
#' For symmetric weighted networks, additional measures:
#' \itemize{
#'   \item Strength sum: \eqn{s_i^{sum} = \sum_{j=1}^{n} w_{ij}}
#'   \item Strength average: \eqn{s_i^{avg} = \frac{s_i^{sum}}{d_i}}
#'   \item Strength standard deviation: \eqn{s_i^{sd} = \sqrt{\frac{1}{d_i} \sum_{j=1}^{n} (w_{ij} - s_i^{avg})^2}}
#' }
#' 
#' For asymmetric networks, statistics are calculated separately for rows (out) 
#' and columns (in), with totals where applicable.
#'
#' @note 
#' For longitudinal networks, statistics are calculated separately for each time 
#' period. For multilayer networks, statistics are calculated separately for each 
#' layer unless layers have been aggregated beforehand.
#' 
#' Missing values (NA) in the network are excluded from calculations. Isolates 
#' (actors with no connections) receive appropriate values (0 for degree, NA for 
#' some centrality measures).
#' 
#' The function handles both cross-sectional and longitudinal data structures, 
#' as well as single-layer and multilayer networks. For ego networks created 
#' with netify, the function appropriately handles the ego-alter structure.
#'
#' @examples
#' # Load example data
#' data(icews)
#' 
#' # Basic usage with directed network
#' net <- netify(
#'   icews, 
#'   actor1 = 'i', actor2 = 'j', time = 'year',
#'   symmetric = FALSE, 
#'   weight = 'verbCoop'
#' )
#' 
#' # Get actor statistics
#' actor_stats <- summary_actor(net)
#' head(actor_stats)
#' 
#' # Add custom statistics
#' # Maximum incoming and outgoing tie weights
#' max_out <- function(mat) apply(mat, 1, max, na.rm = TRUE)
#' max_in <- function(mat) apply(mat, 2, max, na.rm = TRUE)
#' 
#' actor_stats_custom <- summary_actor(
#'   net, 
#'   other_stats = list(
#'     max_out = max_out, 
#'     max_in = max_in
#'   )
#' )
#' head(actor_stats_custom)
#' 
#' # For networks where weights represent distances
#' # (larger values = weaker relationships)
#' distance_net <- netify(
#'   icews, 
#'   actor1 = 'i', actor2 = 'j',
#'   weight = 'matlConf'  # conflict measure
#' )
#' 
#' # Don't invert weights for centrality calculations
#' actor_stats_dist <- summary_actor(
#'   distance_net, 
#'   invert_weights_for_igraph = FALSE
#' )
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @importFrom igraph closeness betweenness eigen_centrality hits_scores
#' 
#' @export summary_actor
#' 

summary_actor <- function(netlet, invert_weights_for_igraph=TRUE, other_stats=NULL) {

  ######################
  # check if netify object
  netify_check(netlet)

  # pull out attrs
  obj_attrs <- attributes(netlet)

  # pull out number of layers
  layers <- obj_attrs$layers
  n_layers <- length(layers)

  # if bipartite then set symmetric parameter
  # in obj_attrs to FALSE
  if(obj_attrs$mode == 'bipartite'){
    obj_attrs$symmetric <- FALSE
  }

  # get type
  netify_type <- obj_attrs$netify_type
  is_longit <- netify_type != 'cross_sec'
  ######################

  ######################
  # save original
  netlet_base <- netlet

  # Pre-allocate list for efficiency
  net_stats_l_mutli <- vector("list", n_layers)
  
  # iterate through each layer
  for(i in seq_along(layers)){
    layer <- layers[i]
    
    ######################
    # if just one layer then just set netlet to user input,
    # if more than one then subset
    if(n_layers == 1){ 
      netlet <- netlet_base 
    } else {
      netlet <- subset_netify(
        netlet_base, 
        layers = layer)
      obj_attrs <- attributes(netlet)
    }

    # convert to list so we can have one 
    # set of code for each type
    object <- switch(netify_type,
      'cross_sec' = list(netlet),
      'longit_array' = array_to_list(netlet),
      'longit_list' = netlet
    )
    ######################    

    ######################
    # calc stats across netlet(s) - more efficient with lapply
    netStats_actor_list <- lapply(object, function(mat){
      actor_stats_for_netlet(
        mat, obj_attrs, 
        invert_weights_for_igraph = invert_weights_for_igraph, 
        other_stats = other_stats)
    })
    ######################

    ######################
    # More efficient data frame creation
    # Combine all matrices at once, then convert to data frame
    netStats_actor <- do.call('rbind', netStats_actor_list)
    
    # Convert to data frame with actor names efficiently
    actor_names <- rownames(netStats_actor)
    netStats_actor <- as.data.frame(netStats_actor, stringsAsFactors = FALSE)
    netStats_actor$actor <- actor_names
    netStats_actor$layer <- layer
    
    # Store in pre-allocated list
    net_stats_l_mutli[[i]] <- netStats_actor
  }
  ######################    

  ######################
  # bind into one data frame - more efficient than repeated rbind
  netStats_actor <- do.call('rbind', net_stats_l_mutli)
  rownames(netStats_actor) <- NULL

  # drop layer column if only one layer
  if(n_layers == 1){
    netStats_actor$layer <- NULL
  }

  # if longitudinal, actor contains both year and name information
  # More efficient string splitting
  if(is_longit){
    actor_split <- strsplit(netStats_actor$actor, '.', fixed = TRUE)
    netStats_actor$time <- vapply(actor_split, `[`, character(1), 1)
    netStats_actor$actor <- vapply(actor_split, `[`, character(1), 2)
  }

  # cleanup - more efficient column reordering
  id_vars <- c('actor', 'layer', 'time')
  existing_id_vars <- intersect(id_vars, names(netStats_actor))
  stat_vars <- setdiff(names(netStats_actor), id_vars)
  netStats_actor <- netStats_actor[, c(existing_id_vars, stat_vars)]
  ######################

  ######################
  # pull out some ego info if it's there
  ego_netlet <- !is.null(obj_attrs$ego_netlet) && obj_attrs$ego_netlet
  
  if(ego_netlet){
    ego_vec <- obj_attrs$ego_vec
    ego_longit <- obj_attrs$ego_longit

    # if ego netlet then make some changes
    # layer will be added and used for ego
    # net will stand in for time if ego data is longit
    
    # if no longit info for ego then need to modify id vars
    if(!ego_longit){
      # if just one time point and one ego then layer is just ego_vec
      # otherwise the net column will have multiple egos
      if(obj_attrs$netify_type == 'cross_sec'){
        netStats_actor$layer <- ego_vec
      } else {
        netStats_actor$layer <- netStats_actor$time
      }
      
      # set id vars and drop time if present
      netStats_actor$time <- NULL
      existing_id_vars <- c('actor', 'layer')
    }

    # if longit info for ego
    if(ego_longit){
      # More efficient string splitting
      time_split <- strsplit(netStats_actor$time, '__', fixed = TRUE)
      netStats_actor$layer <- vapply(time_split, `[`, character(1), 1)
      netStats_actor$time <- vapply(time_split, `[`, character(1), 2)
      
      # set id vars
      existing_id_vars <- c('actor', 'layer', 'time')
    }

    # organize - reorder columns efficiently
    stat_vars <- setdiff(names(netStats_actor), existing_id_vars)
    netStats_actor <- netStats_actor[, c(existing_id_vars, stat_vars)]
  }
  ######################

  ######################
  return(netStats_actor)
  ######################  
}