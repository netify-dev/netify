#' Summary method to get actor level statistics for netify objects
#'
#' `summary_actor` provides detailed actor-level statistics for `netify` objects, handling different network structures and weight conditions. It produces a data frame summarizing various network metrics like degree, strength, closeness, betweenness, and centrality measures.
#'
#' @param netlet Object of class `netify`, typically produced by `get_adjacency` or other network creation functions within the package.
#' @param invert_weights_for_igraph Logical; if TRUE, the weights of the edges are inverted before
#'        being used in the calculation of closeness or betweenness centrality. This is because
#'        igraph treats edge weights as distances. Inverting weights can be crucial when higher weights
#'        should imply stronger (or more valuable) connections rather than longer distances. Default is TRUE.
#' @param other_stats A named list of functions that take a matrix and return additional actor-level statistics to be included in the output. Each function should accept a matrix as input and return a vector or single value per actor. This allows for the inclusion of custom metrics in the summary output.
#' @return A `data.frame` object summarizing actor-level statistics of the network(s). Depending on the structure and attributes of the `netify` object, the output includes:
#' - **Symmetric Unweighted**: Degree, proportion of ties, closeness, betweenness, and eigenvector centrality.
#' - **Symmetric Weighted**: Degree, proportion of ties, sum and average of strengths, closeness (weighted and normalized), betweenness (weighted and normalized), and eigenvector centrality.
#' - **Asymmetric Unweighted**: In-degree, out-degree, total degree, in-propensity of ties, out-propensity of ties, total propensity of ties, in-closeness, out-closeness, total closeness, in-betweenness, out-betweenness, total betweenness, authority scores, and hub scores.
#' - **Asymmetric Weighted**: Similar metrics as asymmetric unweighted but includes strength sums and averages, and all closeness and betweenness measures are weighted.
#' Metrics are calculated based on the network type (symmetric or asymmetric) and whether the network is weighted.
#' @details The function automatically adjusts calculations based on network symmetry and weight attributes, offering tailored statistical outputs for comprehensive network analysis.
#' It supports handling of both cross-sectional and longitudinal network data, ensuring that each actor's metrics are accurately computed over time if applicable.
#' Examples of additional computations (like authority or hub scores) are provided only for asymmetric networks.
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @examples
#' # load icews data
#' data(icews)
#' 
#' # create a netify object
#' netlet = netify(
#'     dyad_data=icews, actor1='i', actor2='j',
#'     time = 'year',
#'     symmetric=FALSE, weight='verbCoop',
#'     mode='unipartite', sum_dyads=FALSE,
#'     actor_time_uniform=TRUE, actor_pds=NULL,
#'     diag_to_NA=TRUE, missing_to_zero=TRUE
#' )
#' 
#' # get actor-level statistics
#' actor_stats <- summary_actor(netlet)
#' head(actor_stats)
#' 
#' # add statistic that get 
#' # the max incoming and outgoing tie
#' max_out <- function(mat){ apply(mat, 1, max, na.rm=TRUE) }
#' max_in <- function(mat){ apply(mat, 2, max, na.rm=TRUE) }
#' actor_stats_custom <- summary_actor(netlet, other_stats = list(max_out = max_out, max_in = max_in))
#' head(actor_stats_custom)
#' 
#' @importFrom igraph closeness betweenness eigen_centrality authority_score hub_score
#' @importFrom cli cli_alert_danger
#' 
#' @export
#' @export summary_actor

summary_actor <- function( netlet, invert_weights_for_igraph=TRUE, other_stats=NULL) {

  # check if netify object
  netify_check(netlet)

	# if more than one layer tell user they must specify a single layer
	if(length(attributes(netlet)$layers) > 1){
		cli::cli_alert_danger(
			'Error: This object has multiple layers. 
			`summary_actor` does not currently support multilayer `netify` inputs.
			Please use the `filter_layers` function to create a `netify` object with a single layer.' )
		stop() }

  # pull out attrs
  obj_attrs <- attributes(netlet)

  # get type
  netify_type <- obj_attrs$netify_type

  # convert to list so we can have one 
  # set of code for each type
  if(netify_type == 'cross_sec'){ object <- list(netlet)}
  if(netify_type == 'longit_array'){ object <- array_to_list(netlet)}
  if(netify_type == 'longit_list'){ object <- netlet}

  # iterate through list object and get stats
  netStats_actor <- lapply(object, function(mat){

    # get igraph version for igraph stats
    g = prep_for_igraph(mat)

    # get weights, force positive, and invert if necessary
    g_wgts = igraph::E(g)$weight
    if(any(g_wgts < 0)){
      g_wgts = g_wgts - min(g_wgts) + 1 }
    if(invert_weights_for_igraph){
      g_wgts = 1 / g_wgts
      g_wgts = ifelse(is.infinite(g_wgts), 0, g_wgts) }

    # symmetric / bin edge case start
    if(obj_attrs$symmetric & obj_attrs$weight_binary){

      # simple mat stats
      degree <- rowSums(mat>0, na.rm=TRUE)
      prop_ties <- rowMeans(mat>0, na.rm=TRUE)

      # add check for bipartite, if bipartite
      # then need to append column stats
      # to rows for simple mat stats
      if(obj_attrs$mode == 'bipartite'){
        degree = c( degree, 
          colSums(mat>0, na.rm=TRUE) )
        prop_ties = c( prop_ties, 
          colMeans(mat>0, na.rm=TRUE) ) }

      # igraph stats
      closeness <- igraph::closeness(g, normalized=TRUE)
      betweenness <- igraph::betweenness(g, normalized=TRUE)
      eigen_vector <- igraph::eigen_centrality(g)$vector

      # organize output
      out <- data.frame(
        degree=degree,
        prop_ties=prop_ties,
        closeness=closeness,
        betweenness=betweenness,
        eigen_vector=eigen_vector ) } # symmetric / bin edge case done

    # symmetric / weighted case start
    if(obj_attrs$symmetric & !obj_attrs$weight_binary){

      # simple mat stats
      degree <- rowSums(mat>0, na.rm=TRUE)
      prop_ties <- rowMeans(mat>0, na.rm=TRUE)
      strength_sum <- rowSums(mat, na.rm=TRUE)
      strength_avg <- rowMeans(mat, na.rm=TRUE)

      # add check for bipartite, if bipartite
      # then need to append column stats
      # to rows for simple mat stats
      if(obj_attrs$mode == 'bipartite'){
        degree = c( degree, 
          colSums(mat>0, na.rm=TRUE) )
        prop_ties = c( prop_ties, 
          colMeans(mat>0, na.rm=TRUE) )
        strength_sum = c( strength_sum,
          colSums(mat, na.rm=TRUE) )
        strength_avg = c( strength_avg,
          colMeans(mat, na.rm=TRUE) ) }

      # igraph stats
      closeness <- igraph::closeness(g, normalized=TRUE, weights = g_wgts)
      betweenness <- igraph::betweenness(g, normalized=TRUE, weights = g_wgts)
      eigen_vector <- igraph::eigen_centrality(g)$vector

      # organize output
      out <- data.frame(
        degree=degree,
        prop_ties=prop_ties,
        strength_sum=strength_sum,
        strength_avg=strength_avg,
        closeness=closeness,
        betweenness=betweenness,
        eigen_vector=eigen_vector ) } # symmetric / weighted case end

    # not symmetric / bin edge case start
    if(!obj_attrs$symmetric & obj_attrs$weight_binary){

      # simple mat stats
      degree_in <- rowSums(mat>0, na.rm=TRUE)
      degree_out <- colSums(mat>0, na.rm=TRUE)
      if(obj_attrs$mode !='bipartite'){
        degree_total <- degree_in + degree_out } else {
        degree_total <- c(degree_in, degree_out) }
      prop_ties_in <- rowMeans(mat>0, na.rm=TRUE)
      prop_ties_out <- colMeans(mat>0, na.rm=TRUE)
      if(obj_attrs$mode != 'bipartite'){
        prop_ties_total <- sum(degree_total > 0, na.rm=TRUE)/length(degree_total) } else {
        prop_ties_total <- c(prop_ties_in, prop_ties_out) }

      # igraph stats
      closeness_in <- igraph::closeness(g, mode = 'in', normalized=TRUE)
      closeness_out <- igraph::closeness(g, mode = 'out', normalized=TRUE)
      closeness_all = igraph::closeness(g, mode = 'all', normalized=TRUE)
      betweenness_in <- igraph::betweenness(g, normalized=TRUE)
      betweenness_out <- igraph::betweenness(g, normalized=TRUE)
      betweenness_all = igraph::betweenness(g, normalized=TRUE)
      authority_score = igraph::authority_score(g)$vector
      hub_score = igraph::hub_score(g)$vector

      # organize output
      out <- data.frame(
        degree_in=degree_in,
        degree_out=degree_out,
        degree_total=degree_total,
        prop_ties_in=prop_ties_in,
        prop_ties_out=prop_ties_out,
        prop_ties_total=prop_ties_total,
        closeness_in=closeness_in,
        closeness_out=closeness_out,
        closeness_all=closeness_all,
        betweenness_in=betweenness_in,
        betweenness_out=betweenness_out,
        betweenness_all=betweenness_all,
        authority_score=authority_score,
        hub_score = hub_score ) } # not symmetric / bin edge case done

    # not symmetric / weighted case start
    if(!obj_attrs$symmetric & !obj_attrs$weight_binary){

      # simple mat stats
      degree_in <- rowSums(mat>0, na.rm=TRUE)
      degree_out <- colSums(mat>0, na.rm=TRUE)
      if(obj_attrs$mode !='bipartite'){
        degree_total <- degree_in + degree_out } else {
        degree_total <- c(degree_in, degree_out) }
      prop_ties_in <- rowMeans(mat>0, na.rm=TRUE)
      prop_ties_out <- colMeans(mat>0, na.rm=TRUE)
      if(obj_attrs$mode != 'bipartite'){
        prop_ties_total <- sum(degree_total > 0, na.rm=TRUE)/length(degree_total) } else {
        prop_ties_total <- c(prop_ties_in, prop_ties_out) }
      strength_sum_in <- rowSums(mat, na.rm=TRUE)
      strength_sum_out <- colSums(mat, na.rm=TRUE)
      if(obj_attrs$mode != 'bipartite'){
        strength_sum_total <- strength_sum_in + strength_sum_out } else {
        strength_sum_total <- c(strength_sum_in, strength_sum_out) }
      strength_avg_in <- rowMeans(mat, na.rm=TRUE)
      strength_avg_out <- colMeans(mat, na.rm=TRUE)
      if(obj_attrs$mode != 'bipartite'){
        strength_avg_total <- (strength_avg_in + strength_avg_out)/2 } else {
        strength_avg_total <- c(strength_avg_in, strength_avg_out) }

      # igraph stats
      closeness_in <- igraph::closeness(g, mode = 'in', normalized=TRUE, weights = g_wgts)
      closeness_out <- igraph::closeness(g, mode = 'out', normalized=TRUE, weights = g_wgts)
      closeness_all = igraph::closeness(g, mode = 'all', normalized=TRUE, weights = g_wgts)
      betweenness_in <- igraph::betweenness(g, normalized=TRUE, weights = g_wgts)
      betweenness_out <- igraph::betweenness(g, normalized=TRUE, weights = g_wgts)
      betweenness_all = igraph::betweenness(g, normalized=TRUE, weights = g_wgts)
      authority_score = igraph::authority_score(g)$vector
      hub_score = igraph::hub_score(g)$vector      

      # organize output
      out <- data.frame(
        degree_in=degree_in,
        degree_out=degree_out,
        degree_total=degree_total,
        prop_ties_in=prop_ties_in,
        prop_ties_out=prop_ties_out,
        prop_ties_total=prop_ties_total,
        strength_sum_in=strength_sum_in,
        strength_sum_out=strength_sum_out,
        strength_sum_total=strength_sum_total,
        strength_avg_in=strength_avg_in,
        strength_avg_out=strength_avg_out,
        strength_avg_total=strength_avg_total,
        closeness_in=closeness_in,
        closeness_out=closeness_out,
        closeness_all=closeness_all,
        betweenness_in=betweenness_in,
        betweenness_out=betweenness_out,
        betweenness_all=betweenness_all,
        authority_score=authority_score,
        hub_score=hub_score ) } # not symmetric / weighted case done

    # calculate any userstats
    if( !is.null(other_stats) ){
      other_out <- lapply(other_stats, function(stat){ stat(mat) })
      out <- cbind(out, do.call('cbind', other_out)) }

    # 
    return(out) })

  # organize all computed actors stats into a data.frame
  netStats_actor <- do.call('rbind', netStats_actor)
  netStats_actor <- data.frame(netStats_actor, stringsAsFactors=FALSE)

  # move variable label to be a column
  netStats_actor$actor <- rownames(netStats_actor)
  rownames(netStats_actor) <- NULL

  # if cross-sectional then cleanup
  if(netify_type == 'cross_sec'){
    netStats_actor <- netStats_actor[,c('actor', setdiff(names(netStats_actor), 'actor'))]
    return(netStats_actor)}

  # if longitudinal, `actor`` contains both year and name information
  if(netify_type != 'cross_sec'){
    netStats_actor$time = unlist(lapply(strsplit(netStats_actor$actor, '.', fixed=TRUE), function(x){ x[1] }))
    netStats_actor$actor = unlist(lapply(strsplit(netStats_actor$actor, '.', fixed=TRUE), function(x){ x[2] }))
    netStats_actor$time <- as.numeric(netStats_actor$time)
  }

  # cleanup
  vars <- c('actor', "time")
  netStats_actor <- netStats_actor[,c(vars, setdiff(names(netStats_actor), vars))]

  #
  return(netStats_actor)
}

# max_actor_val = function(mat){
#   apply(mat, 1, max, na.rm=TRUE)
# }

# min_actor_val = function(mat){
#   apply(mat, 1, min, na.rm=TRUE)
# }

# other_stats = list(
#   max_actor_val  = max_actor_val,
#   min_actor_val  = min_actor_val
#   )


# closeness_igraph = function(
#   netlet
# ){
#   g = prep_for_igraph(netlet)
#   closeness = igraph::closeness(g, mode = 'all', normalized = TRUE)
#   return(closeness)
# }

# other_stats=list(closeness=closeness_igraph)

    # stats
    # degree, prop_ties, strength_sum, strength_avg, 
    # closeness, betweenness, 
    # eigenvector, authority, hub

    # symmetric / bin edge: 
      # degree, prop_ties,  
      # closeness, betweenness, 
      # eigenvector

    # symmetric / weighted:
      # degree, prop_ties, strength_sum, strength_avg,
      # closeness wgt nrm, betweenness wgt nrm,
      # eigenvector

    # not symmetric / bin edge: 
      # degree_in, degree_out, prop_ties_in, prop_ties_out
      # closeness_in, closeness_out, closeness_all
      # betweenness_in, betweenness_out, betweenness_all
      # authority, hub

    # not symmetric / weighted:
      # degree_in, degree_out, prop_ties_in, prop_ties_out
      # strength_sum_in, strength_sum_out, strength_sum_total
      # strength_avg_in, strength_avg_out, strength_avg_total
      # closeness_in, closeness_out, closeness_all
      # betweenness_in, betweenness_out, betweenness_all
      # authority, hub