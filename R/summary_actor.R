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
#' - **Symmetric Unweighted**: Various network measures for actor \(i\) include:
#'   - *Degree*: The count of unique actors that actor \(i\) is directly connected to -- calculated as \eqn{d_i = \sum_{j=1}^{n} a_{ij}}, where \eqn{a_{ij}} is the adjacency matrix element indicating the presence (1) or absence (0) of a tie between actors \(i\) and \(j\).
#'   - *Proportion of ties*: The percentage of actors in the network with whom actor \(i\) has a direct relationship -- calculated as \eqn{p_i = \frac{d_i}{n-1}}, where \eqn{d_i} is the degree of actor \(i\) and \(n\) is the total number of actors in the network.
#'   - *Network share*: The fraction of the network's total connections that include actor \(i\) -- calculated as \eqn{s_i = \frac{d_i}{\sum_{j=1}^{n} d_j}}, where \eqn{d_i} is the degree of actor \(i\) and \eqn{\sum_{j=1}^{n} d_j} is the total number of ties in the network.
#'   - *Closeness* (\(C_i\)): A measure of how close actor \(i\) is to all other actors in the network -- calculated as \eqn{C_i = \frac{1}{\sum_{j} d(i, j)}}, where \eqn{d(i, j)} is the distance to every other actor \(j\).
#'   - *Betweenness* (\(B_i\)): A measure of actor \(i\)'s importance in connecting different parts of the network -- calculated as \eqn{B_i = \sum_{s \neq i \neq t} \frac{\sigma_{st}(i)}{\sigma_{st}}}, where \eqn{\sigma_{st}} is the total number of shortest paths from node \(s\) to node \(t\) and \eqn{\sigma_{st}(i)} is the number of those paths that pass through \(i\).
#'   - *Eigenvector centrality* (\(EC_i\)): A measure of actor \(i\)'s influence based on their connections to other highly connected actors in the network, calculated using the principal eigenvector of the network's adjacency matrix.
#' - **Symmetric Weighted**: Includes the same statistics as the symmetric unweighted case, with additional measures accounting for the weight of connections:
#'   - *Strength sum*: The total weight of the ties connected to actor \(i\) in the network -- calculated as \eqn{s_i^{sum} = \sum_{j=1}^{n} w_{ij}}, where \eqn{w_{ij}} is the weight of the tie between actors \(i\) and \(j\).
#'   - *Strength average*: The average weight of the ties connected to actor \(i\) -- calculated as \eqn{s_i^{avg} = \frac{s_i^{sum}}{d_i}}, where \eqn{s_i^{sum}} is the strength sum of actor \(i\) and \eqn{d_i} is the degree of actor \(i\).
#' - *Strength standard deviation*: The variability in the weights of ties connected to actor \(i\) -- calculated as \eqn{s_i^{sd} = \sqrt{\frac{1}{d_i} \sum_{j=1}^{n} (w_{ij} - s_i^{avg})^2}}, where \eqn{w_{ij}} is the weight of the tie between actors \(i\) and \(j\).
#'  - *Strength median*: The median weight of the ties connected to actor \(i\) -- calculated as the middle value of the sorted weights.
#'   - For *closeness* and *betweenness*, edge weights are typically inverted to treat them as distances, following the convention in `igraph`. This behavior can be modified by setting the `invert_weights_for_igraph` parameter in the `summary_actor` function to FALSE.
#' - **Asymmetric Unweighted**: Same as the symmetric case but now a statistics for the row and column are calculated separately; when relevant a total statistic is calculated as well.
#' - **Asymmetric Weighted**: Same as the symmetric case but now a statistics for the row and column are calculated separately; when relevant a total statistic is calculated as well.
#' 
#' @details The function automatically adjusts calculations based on network symmetry and weight attributes, offering tailored statistical outputs for comprehensive network analysis.
#' It supports handling of both cross-sectional and longitudinal network data, ensuring that each actor's metrics are accurately computed over time if applicable.
#' Examples of additional computations (like authority or hub scores) are provided only for asymmetric networks.
#' 
#' 
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

  ######################
  # check if netify object
  netify_check(netlet)

  # pull out attrs
  obj_attrs <- attributes(netlet)

	# pull out number of layers
	layers = obj_attrs$layers

  # if bipartite then set symmetric parameter
  # in obj_attrs to FALSE
  if(obj_attrs$mode == 'bipartite'){
    obj_attrs$symmetric = FALSE }

  # get type
  netify_type <- obj_attrs$netify_type
	######################

	######################
  # save original
  netlet_base <- netlet

	# iterate through each layer and recombine
	net_stats_l_mutli = lapply(layers, function(layer){

		######################
		# if just one layer then just set netlet to user input,
		# if more than one then subset
		if(length(layers)==1){ netlet = netlet_base }
		if(length(layers)>1){
			netlet = subset_netlet(
				netlet_base, what_layers_to_subset = layer)
			obj_attrs <- attributes(netlet) }

    # convert to list so we can have one 
    # set of code for each type
    if(netify_type == 'cross_sec'){ object <- list(netlet)}
    if(netify_type == 'longit_array'){ object <- array_to_list(netlet)}
    if(netify_type == 'longit_list'){ object <- netlet}
		######################    

		######################
		# calc stats across netlet(s)
    netStats_actor <- lapply(object, function(mat){
      return(
        actor_stats_for_netlet(
          mat, obj_attrs, 
          invert_weights_for_igraph = invert_weights_for_igraph, 
          other_stats = other_stats)) 
      })
		######################

		######################
    # organize all computed actors stats into a data.frame
    netStats_actor <- do.call('rbind', netStats_actor)
    netStats_actor <- data.frame(netStats_actor, stringsAsFactors=FALSE)

    # move variable label to be a column
    netStats_actor$actor <- rownames(netStats_actor)
    rownames(netStats_actor) <- NULL

		# add layer info
		netStats_actor$layer = layer

		#
		return(netStats_actor) })
		######################    

	######################
	# bind into one matrix and start cleaning
	netStats_actor = do.call('rbind', net_stats_l_mutli)

	# drop layer column if only one layer
	if(length(layers)==1){
		netStats_actor = netStats_actor[
      ,setdiff(names(netStats_actor), 'layer')] }

  # if longitudinal, `actor`` contains both year and name information
  if(netify_type != 'cross_sec'){
    netStats_actor$time = unlist(lapply(strsplit(
      netStats_actor$actor, '.', fixed=TRUE), function(x){ x[1] }))
    netStats_actor$actor = unlist(lapply(strsplit(
      netStats_actor$actor, '.', fixed=TRUE), function(x){ x[2] }))
    # netStats_actor$time <- as.numeric(netStats_actor$time)
  }

  # cleanup
  vars <- c('actor', 'layer', "time")
  vars = vars[vars %in% names(netStats_actor)]
  netStats_actor <- netStats_actor[,
    c(vars, setdiff(names(netStats_actor), vars))]
  ######################

  ######################
	# pull out some ego info if it's there,
	# assume FALSE
	ego_netlet = FALSE ; ego_longit = FALSE
	include_ego = FALSE
	if(!is.null(obj_attrs$ego_netlet)){
		if(obj_attrs$ego_netlet){
			ego_netlet = TRUE
			ego_vec = obj_attrs$ego_vec			
			ego_longit = obj_attrs$ego_longit } }

	# if ego netlet then make some changes
	# layer will be added and used for ego
	# net will stand in for time if ego data is longit
	if(ego_netlet){

		# if no longit info for ego then need to modify id vars
		if(!ego_longit){

			# if just one time point and one ego then layer is just ego_vec
			# otherwise the net column will have multiple egos
			if(!ego_longit & obj_attrs$netify_type=='cross_sec'){
				netStats_actor$layer = ego_vec
			} else {
				netStats_actor$layer = netStats_actor$time }
			
      # set id vars
      vars = c('actor', 'layer')

      # drop time var if present
      if('time' %in% names(netStats_actor)){
        toDrop = which(names(netStats_actor) == 'time')
        netStats_actor = netStats_actor[,-toDrop] }
		}

		# if longit info for ego
		if(ego_longit){

			# extract units and pds from net column
			ego_units = unlist( lapply( strsplit(
					netStats_actor$time, '__'), function(x){ x[1] }))
			ego_pds = unlist( lapply( strsplit(
					netStats_actor$time, '__'), function(x){ x[2] }))

			# add relev vars
			netStats_actor$time = ego_pds
			netStats_actor$layer = ego_units

      # set id vars
      vars = c('actor', 'layer', 'time')      
		}

		# organize
    stat_vars = setdiff(names(netStats_actor), vars)
		netStats_actor = netStats_actor[,c(vars, stat_vars)]
	}      
  ######################

  ######################
  #
  return(netStats_actor)
	######################  
}
