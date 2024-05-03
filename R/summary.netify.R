#' Summary method to get graph level statistics for netify objects
#'
#' `summary.netify` processes a netify object to calculate and return a data frame of graph-level statistics. This function is designed to work with both cross-sectional and longitudinal netify data structures, providing a comprehensive overview of network characteristics such as density, reciprocity, and standard deviations of sending and receiving effects.
#'
#' @param object An object of class netify, which should have been created using the function `get_adjacency`. This object contains the network data structured for analysis.
#' @param ... Additional parameters which can include user-defined statistical functions. These functions should take a matrix as input and return a scalar value. They will be applied to each network slice individually if the netify object represents longitudinal data.
#'   - `other_stats`: A named list of functions that take a matrix and return additional actor-level statistics to be included in the output. Each function should accept a matrix as input and return a vector or single value per actor. This allows for the inclusion of custom metrics in the summary output.
#'
#' @return A data frame where each row represents the network-level statistics for a single network or a single time point in a longitudinal study. The columns include:
#' - `numActors`: The number of actors in the network.
#' - `dens`: The density of the network.
#' - `recip`: The reciprocity of the network calculated as the correlation between the adjacency matrix and its transpose.
#' - `sdSendEff`: The standard deviation of the sending effects (row means).
#' - `sdRecEff`: The standard deviation of the receiving effects (column means).
#' - `srCov`: The sender-receiver covariance calculated as the correlation between row means and column means.
#' Additional columns may appear if user-defined functions are passed in the `...` parameter.
#' Each row is labeled with a network identifier in the `net` column, which is especially useful for longitudinal data.
#'
#' @details This function simplifies the process of extracting key network statistics across potentially multiple networks within a netify object. It is capable of handling both weighted and unweighted networks and can adjust its calculations based on the nature of the network data (cross-sectional vs. longitudinal).
#'
#' @aliases summary_graph
#' 
#' @export

summary.netify <- function(object, ...){

  # check if netify object
  netify_check(object)

  # get summary args
  summary_args <- list(...)

  # placeholder
  netlet <- object ; rm(object)

	# pull out attrs
	obj_attrs <- attributes(netlet)

	# get type
	netlet_type <- obj_attrs$netify_type

	# if cross sec convert to a list object so that
	# we can use lapply
	if(netlet_type == 'cross_sec'){ netlet <- list(netlet) }
	if(netlet_type == 'longit_array'){ netlet <- array_to_list(netlet) }

	# calc some basic stats
	net_stats <- lapply(netlet, function(mat){

		# get binary version
		bin_mat = 1*(mat>0)

		# prelim calcs
		rowMu <- rowMeans(mat, na.rm=TRUE)
		colMu <- colMeans(mat, na.rm=TRUE)
		rowMu_bin <- rowMeans(bin_mat, na.rm=TRUE)
		colMu_bin <- colMeans(bin_mat, na.rm=TRUE)		

		# vectorized matrix
		vec_bin_mat <- c(bin_mat)
		vec_mat <- c(mat)

		# measures of interest
		numActors <- nrow(mat)
		minVal <- min(vec_mat, na.rm=TRUE)
		maxVal <- max(vec_mat, na.rm=TRUE)

		# agnostic
		prop_missing <- mean(is.na(vec_mat))		
		dens <- mean(vec_bin_mat, na.rm=TRUE)		

		# weighted stats (calc if edge can be something other than 0/1 only)
		avg_edge <- mean(vec_mat, na.rm=TRUE)
		recip <- cor(vec_mat, c(t(mat)), use='pairwise.complete.obs')
		sdSendEff <- sd( rowMu, na.rm=TRUE )
		sdRecEff <- sd( colMu, na.rm=TRUE )
		srCov <- cor(c(rowMu), c(colMu), use='pairwise.complete.obs')

		# binary stats (dont recalc if we have a binary net)
		recip_bin <- cor(vec_bin_mat, c(t(bin_mat)), use='pairwise.complete.obs')
		sdSendEff_bin <- sd( rowMu_bin, na.rm=TRUE )
		sdRecEff_bin <- sd( colMu_bin, na.rm=TRUE )
		srCov_bin <- cor(c(rowMu_bin), c(colMu_bin), use='pairwise.complete.obs')

		# organize
		out <- c(
			numActors = numActors,
			dens=dens, recip=recip,
			sdSendEff=sdSendEff, sdRecEff=sdRecEff, srCov=srCov
			)

    # calculate any userstats
    # if( 'other_stats' %in% names(summary_args) ){
    #   other_out <- lapply(summary_args$'other_stats', function(stat){ stat(mat) })
    #   out <- append(out, unlist(other_out))
    # }
    #
    #
		  other_out <- lapply(summary_args, function(stat){ stat(mat) })
		  out <- append(out, unlist(other_out))

    #
		return(out) })

  # bind into one matrix
	net_stats <- do.call('rbind', net_stats)
	net_stats <- data.frame(net_stats, stringsAsFactors=FALSE)

	# move variable label to be a column
	net_stats$net <- rownames(net_stats)
	rownames(net_stats) <- NULL

	# reorder cols
	net_stats <- net_stats[,c('net', setdiff(names(net_stats), 'net'))]

	#
	return(net_stats)
}

# numActors=numActors,
# dens=dens
# recip=recip
# sdSendEff=sdSendEff
# sdRecEff=sdRecEff
# srCov=srCov

# always: 
	# num_actors: number of actors
	# num_ties: number of ties (divide by two for symmetric case)
	# missing: proportion of missing edges	
	# density: proportion of non-zero ties
	# competition: tie dispersion. 

# symmetric: 
	# sd_in_actor_means: variation in actor means

# non-symmetric: 
	# sd_in_sender_means: variation in sender means
	# sd_in_receiver_means: variation in receiver means
	# sender_receiver_rho: correlation between sender and receiver means	
	# reciprocity: correlation between sending and receiving effects

# weighted: 
	# mean_edge_weight: average edge weight
	# sd_edge_weight: variation in edge weights



		# To do this, we estimate the relational, or network, competitiveness of the environment. If violence is concentrated around a single actor, where one actor dominates conflict initiation towards many others or receives conflict from many challengers, then civilian victimization will be less likely. If, however, all actors are likely to fight one another -- in an all against all competition -- civilian victimization will be at its highest. To investigate this, we conceptualize the overall strategic environment as a social network, wherein the nodes in this network are armed groups, and the edges are battles between these groups. We formulate our concept of network competition across each conflict network as follows:\footnote{This is a rescaled version of a commonly used measure for market share, the Herfindahl-Hirschman Index.}

		# \begin{equation}
		# \text{Network Competition} = 1 - \sum_{i = 1}{N} (CS_{i})^2
		# \end{equation}

		# $N$ denotes the number of actors and $CS_i$ is the conflict share of actor $i$, which is a measure of the proportion of battles an armed actor is involved in.\footnote{This type of measurement of competition has been used in a range of works such as in measuring ethnic and cultural fractionalization \citep{fearon:2003} to the competitiveness of party systems \citep{alfano:baraldi:2015}.} Our measure provides us with a representation of how dispersed conflict is in the network. Figure~\ref{fig:styleNet} provides a conceptual illustration of a low and high competition scenario.

		# The left-hand network exhibits low network competition. Here, conflict patterns are dominated by a single sender or receiver in the network. In this network, an armed group's strategic decision to victimize civilians is fairly straightforward -- victimization takes place if the coercive effect (causing more non-supporters to reluctantly support the group in charge) outweighs the resources that could be mobilized from non-supporters. In this environment, while there may initially be low levels of victimization, we will quickly approach an equilibrium where most civilians support the groups in control of their territory, and no victimization occurs. The right-hand network reveals a different picture. This competitive conflict network functions as a Hobbesian war of all against all, where each armed group is ready to attack each other armed group. Almost all actors are at risk of an attack, and they are at risk of an attack from multiple sources leading to even stronger incentives towards victimization. In this case, there is likely a fluid control of territory. Frequent changes in the system increase incentives for violence against civilians; an assumption supported by existing research \citep{wood:kathman:2015}. Some of the most intractable and dynamic conflicts--like the modern wars in Somalia-- are likely to exhibit this network structure. Our theoretical model, explained below, builds on existing work by formalizing the competitive process in a network environment and including the strategic behavior of civilians as a key predictor of violence against civilians during war.\footnote{\citep{konig:etal:2017} similarly uses a detailed theoretical model to study the interplay between network dynamics and conflict in civil conflict. The main difference is that their study is interested in conflict between armed groups, rather than how conflict between armed groups changes the incentives for one sided violence. In addition, their study focuses on different aspects of the network (actor centrality rather than the overall competitiveness of the network), and their study is an in-depth examination of conflict in the Congo, rather than a cross-national analysis.}