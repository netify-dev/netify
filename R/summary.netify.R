#' Summary method to get graph level statistics for netify objects
#'
#' `summary.netify` processes a netify object to calculate and return a data frame of graph-level statistics. This function is designed to work with both cross-sectional and longitudinal netify data structures, providing a comprehensive overview of network characteristics such as density, reciprocity, and standard deviations of sending and receiving effects.
#'
#' @param object An object of class netify, which should have been created using the function `netify`. This object contains the network data structured for analysis.
#' @param ... Additional parameters which can include user-defined statistical functions. These functions should take a matrix as input and return a scalar value. They will be applied to each network slice individually if the netify object represents longitudinal data.
#'   - `other_stats`: A named list of functions that take a matrix and return additional actor-level statistics to be included in the output. Each function should accept a matrix as input and return a vector or single value per actor. This allows for the inclusion of custom metrics in the summary output.
#'
#' @return A data frame where each row represents the network-level statistics for a single network or a single time point in a longitudinal study. Depending on the network type and data attributes, the columns can include:
#' - `num_actors`: Number of actors in the network - for bipartite networks number of row and column actors are reported separately.
#' - `density`: The proportion of possible connections that are actual connections within the network.
#' - `num_edges`: The total number of edges in the network (does not take edge weight into account).
#' - `mean_edge_weight`: The average weight of edges in the network, provided only for weighted networks.
#' - `sd_edge_weight`: The standard deviation of edge weights in the network, provided only for weighted networks.
#' - `prop_edges_missing`: The proportion of potential edges that are missing.
#' - `min_edge_value` and `max_edge_value`: The minimum and maximum edge weights observed in the network, provided only for weighted networks.
#' - `competition_row` and `competition_col` (defaults to `competition` for undirected networks): Measures network competitiveness using the Herfindahl-Hirschman Index (HHI), defined as \eqn{\sum_{i=1}^{n} (s_i)^2}, where \eqn{s_i} is the proportion of interactions by actor \eqn{i} and \eqn{n} is the total number of actors. The index ranges from 1/n (indicating high diversity and competitive interaction across actors) to 1 (one actor dominates all interactions). Refer to Dorff, Gallop, & Minhas (2023) for an application of this measure in conflict networks.
#' - `sd_of_row_means` and `sd_of_col_means`: Standard deviations of the sending and receiving effects (row and column means). These statistics are meant to describe the variability in actor behavior across the network.
#' - `covar_of_row_col_means`: The covariance between sending and receiving effects, always takes weights into account and is only calculated for unipartite networks.
#' - `reciprocity`: The reciprocity of the network, defined as the correlation between the adjacency matrix and its transpose, always takes weights into account and is only calculated for unipartite networks.
#' - `transitivity`: The overall transitivity or clustering coefficient of the network, reflecting the likelihood that two neighbors of a node are connected (calculated using `transitivity` function from `igraph`).
#'
#' @details This function is especially useful to simplify the process of extracting key network statistics across multiple networks in a netify object. It is capable of handling both weighted and unweighted networks and adjusts its calculations based on the nature of the network data (cross-sectional vs. longitudinal).
#' 
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @examples
#' 
#' # load icews data
#' data(icews)
#'
#' # create netlet
#' netlet = netify(
#'     dyad_data=icews, actor1='i', actor2='j',
#'     time = 'year', symmetric=FALSE, weight='verbCoop' )
#'
#' # calculate default summary stats
#' summ_graph = summary(netlet)
#' head(summ_graph)
#'
#' # add custom summary stat
#' spinglass_ig = function(mat){
#'     g = prep_for_igraph(mat)
#'     comm = igraph::cluster_spinglass(g)
#'     num_comm = length(comm$csize)
#'     return( c(
#'         num_comm=num_comm,
#'         comm_modul = comm$modularity
#'     ) )
#' }
#' 
#' # since calculating communities can be intensive
#' # lets take subset of time periods
#' sub_net = subset_netlet(netlet, when_to_subset = as.character(2013:2014))
#'
#' # feed custom summary stat into summary
#' summary(sub_net, 
#'     other_stats=list(spinglass_ig=spinglass_ig)) 
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

	# placeholder
	netlet_base <- object ; rm(object)

	# pull out attrs
	obj_attrs <- attributes(netlet_base)

	# pull out number of layers
	layers = obj_attrs$layers

	# get type
	netlet_type <- obj_attrs$netify_type

	# iterate through each layer and recombine
	net_stats_l_mutli = lapply(layers, function(layer){

		# if just one layer then just ignore
		if(length(layers)==1){ netlet = netlet_base }
		if(length(layers)>1){
			netlet = subset_netlet(
				netlet_base, what_layers_to_subset = layer)
			obj_attrs <- attributes(netlet) }
		
		# if cross sec convert to a list object so that
		# we can use lapply
		if(netlet_type == 'cross_sec'){ netlet <- list(netlet) }
		if(netlet_type == 'longit_array'){ netlet <- array_to_list(netlet) }

		######################
		# calc stats across netlet(s)
		net_stats_l <- lapply(netlet, function(mat){

			######################
			# prep some objects

			# get igraph version
			g = prep_for_igraph(mat)

			# bin_mat
			bin_mat = +(mat > 0)

			# vectorized matrix
			vec_mat <- c(mat)
			vec_bin_mat <- c(bin_mat)
			######################

			######################
			# start calcs

			# actor variability
			row_means <- rowMeans(mat, na.rm=TRUE)
			col_means <- colMeans(mat, na.rm=TRUE)

			# number of actors
			num_row_actors <- nrow(mat)
			num_col_actors <- ncol(mat)

			# get proportion missing
			if(obj_attrs$diag_to_NA){
				prop_edges_missing = (sum(is.na(vec_mat))+num_row_actors)/length(vec_mat) }
			if(!obj_attrs$diag_to_NA){
				prop_edges_missing = sum(is.na(vec_mat))/length(vec_mat) }

			# min/max scores
			min_edge_value <- min(vec_mat, na.rm=TRUE)
			max_edge_value <- max(vec_mat, na.rm=TRUE)

			# graph level summary measures
			density <- mean(vec_bin_mat, na.rm=TRUE)
			mean_edge_weight <- mean(vec_mat, na.rm=TRUE)
			sd_edge_weight <- sd(vec_mat, na.rm=TRUE)
			num_edges <- sum(vec_bin_mat, na.rm=TRUE)

			# competition measures
			row_sums = row_means * num_row_actors
			col_sums = col_means * num_col_actors
			competition_row = sum( (row_sums / sum(row_sums))^2 )
			competition_col = sum( (col_sums / sum(col_sums))^2 )		

			# symmetric case
			competition <- row_sums
			
			# getting at actor variability
			sd_of_row_means <- sd( row_means, na.rm=TRUE )
			sd_of_col_means <- sd( col_means, na.rm=TRUE )

			# covar and recip calcs for one mode nets
			if(obj_attrs$mode=='unipartite'){
				covar_of_row_col_means <- cor(row_means, col_means, use='pairwise.complete.obs')
				reciprocity <- cor(vec_mat, c(t(mat)), use='pairwise.complete.obs') }

			# place holder val for bipartite
			if(obj_attrs$mode=='bipartite'){
				covar_of_row_col_means <- NA ; reciprocity <- NA }

			# get igraph clustering coef
			transitivity = igraph::transitivity(g)
			######################

			######################
			# organize
			out <- c(
				num_row_actors = num_row_actors,
				num_col_actors = num_col_actors,
				density=density,
				num_edges = num_edges,
				mean_edge_weight=mean_edge_weight,
				sd_edge_weight=sd_edge_weight,
				prop_edges_missing = prop_edges_missing,			
				min_edge_value = min_edge_value, 
				max_edge_value = max_edge_value, 
				competition_row=competition_row,
				competition_col=competition_col,
				sd_of_row_means=sd_of_row_means, 
				sd_of_col_means=sd_of_col_means,
				covar_of_row_col_means=covar_of_row_col_means,
				reciprocity=reciprocity,
				transitivity=transitivity
				)
			######################

			######################
			# calculate any user supplied stats
			if( 'other_stats' %in% names(summary_args) ){
			other_out <- lapply(summary_args$'other_stats', function(stat){ stat(mat) })
			out <- append(out, unlist(other_out)) }
			######################
			
			#
			return(out) })
		######################

		######################
		# bind into one matrix
		net_stats <- do.call('rbind', net_stats_l)
		net_stats <- data.frame(net_stats, stringsAsFactors=FALSE)

		# move variable label to be a column
		net_stats$net <- rownames(net_stats)
		rownames(net_stats) <- NULL

		# add layer info
		net_stats$layer = layer

		#
		return(net_stats) })

	# bind into one matrix
	net_stats = do.call('rbind', net_stats_l_mutli)

	# reorder cols
	net_stats <- net_stats[,
		c( 'net', 'layer', 
			setdiff(names(net_stats), c('net', 'layer')))
		]

	# drop layer column if only one layer
	if(length(layers)==1){
		net_stats = net_stats[,setdiff(names(net_stats), 'layer')] }
	######################

	######################
	# clean up default stats based on netlet type

	# if unipartite then collapse num_row_actors and num_col_actors
	# into one column: num_actors
	if(obj_attrs$mode=='unipartite'){

		# find num_row_actors
		n_r_i = which(names(net_stats) == 'num_row_actors')
		names(net_stats)[n_r_i] = 'num_actors'

		# drop num_col_actors
		net_stats <- net_stats[,setdiff(names(net_stats), 'num_col_actors')]
	}

	# if undirected then collapse row and col stats into one
	if(obj_attrs$symmetric){

		# find competition_row
		c_r_i = which(names(net_stats) == 'competition_row')
		names(net_stats)[c_r_i] = 'competition'

		# find sd_of_row_means
		sd_r_i = which(names(net_stats) == 'sd_of_row_means')
		names(net_stats)[sd_r_i] = 'sd_of_actor_means'

		# drop covar_of_row_col_means and reciprocity
		to_drop = c(
			'competition_col', 'sd_of_col_means',
			'covar_of_row_col_means', 'reciprocity'
		)
		net_stats <- net_stats[,setdiff(names(net_stats), to_drop)]
	}

	# if not weighted then drop redundant stats
	if(all(obj_attrs$weight_binary)){
		# drop mean_edge_weight, sd_edge_weight,
		# min_edge_value, max_edge_value
		to_drop = c(
			'mean_edge_weight', 'sd_edge_weight', 
			'min_edge_value', 'max_edge_value')
		net_stats <- net_stats[,setdiff(names(net_stats), to_drop)]
	}
	######################

	#
	return(net_stats)
}

# library(netify)
# example(layer_netify)

# v = subset_netlet(
# 	icews_verbCoop_matlCoop, 
# 	what_layers_to_subset = 'verbCoop'
# )

# v

# vl = subset_netlet(
# 	icews_verbCoop_matlCoop_longit_l, 
# 	what_layers_to_subset = 'verbCoop'
# )

# vl

# va = subset_netlet(
# 	icews_verbCoop_matlCoop_longit_a, 
# 	what_layers_to_subset = 'verbCoop'
# )

# va

# ##
# summary.netify(v)
# summary.netify(vl)
# summary.netify(va)

# summary.netify(icews_verbCoop_matlCoop)
# summary.netify(icews_verbCoop_matlCoop_longit_l)
# summary.netify(icews_verbCoop_matlCoop_longit_a)

