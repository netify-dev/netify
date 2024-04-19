#' Summary method to get graph level statistics for netify objects
#'
#' `summary.netify` takes in a netify object
#' and outputs a data.frame with graph level statistics.
#'
#' @param object object of class netify, produced by get_adjacency
#' @param ... additional parameters not used
#' @return a data.frame object summarizing graph level stats of the network(s)
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @export summary.netify
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

		# prelim calcs
		rowMu <- rowMeans(mat, na.rm=TRUE)
		colMu <- colMeans(mat, na.rm=TRUE)

    # vectorized matrix
    vec_mat <- c(mat)

		# measures of interest
    numActors <- nrow(mat)
    minVal <- min(vec_mat, na.rm=TRUE)
    maxVal <- max(vec_mat, na.rm=TRUE)
	dens <- mean(mat, na.rm=TRUE)
	recip <- cor(vec_mat, c(t(mat)), use='pairwise.complete.obs')
	sdSendEff <- sd( rowMu, na.rm=TRUE )
	sdRecEff <- sd( colMu, na.rm=TRUE )
	srCov <- cor(c(rowMu), c(colMu), use='pairwise.complete.obs')

    # organize
		out <- c(
      numActors = numActors,
			dens=dens, recip=recip,
			sdSendEff=sdSendEff, sdRecEff=sdRecEff, srCov=srCov )

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
