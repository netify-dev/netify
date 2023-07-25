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
  summaryArgs <- list(...)

  # placeholder
  netlet <- object ; rm(object)

	# pull out attrs
	objAttrs <- attributes(netlet)

	# get type
	cNetType <- objAttrs$netify_type

	# if cross sec convert to a list object so that
	# we can use lapply
	if(cNetType == 'cross_sec'){ netlet <- list(netlet) }
	if(cNetType == 'longit_array'){ netlet <- array_to_list(netlet) }

	# calc some basic stats
	netStats <- lapply(netlet, function(mat){

		# prelim calcs
		rowMu <- rowMeans(mat, na.rm=TRUE)
		colMu <- colMeans(mat, na.rm=TRUE)

    # vectorized matrix
    vecMat <- c(mat)

		# measures of interest
    numActors <- nrow(mat)
    minVal <- min(vecMat, na.rm=TRUE)
    maxVal <- max(vecMat, na.rm=TRUE)
	dens <- mean(mat, na.rm=TRUE)
	recip <- cor(vecMat, c(t(mat)), use='pairwise.complete.obs')
	sdSendEff <- sd( rowMu, na.rm=TRUE )
	sdRecEff <- sd( colMu, na.rm=TRUE )
	srCov <- cor(c(rowMu), c(colMu), use='pairwise.complete.obs')

    # organize
		out <- c(
      numActors = numActors,
			dens=dens, recip=recip,
			sdSendEff=sdSendEff, sdRecEff=sdRecEff, srCov=srCov )

    # calculate any userstats
    # if( 'other_stats' %in% names(summaryArgs) ){
    #   other_out <- lapply(summaryArgs$'other_stats', function(stat){ stat(mat) })
    #   out <- append(out, unlist(other_out))
    # }
    #
    #
		  other_out <- lapply(summaryArgs, function(stat){ stat(mat) })
		  out <- append(out, unlist(other_out))

    #
		return(out) })

  # bind into one matrix
	netStats <- do.call('rbind', netStats)
	netStats <- data.frame(netStats, stringsAsFactors=FALSE)

	# move variable label to be a column
	netStats$net <- rownames(netStats)
	rownames(netStats) <- NULL

	# reorder cols
	netStats <- netStats[,c('net', setdiff(names(netStats), 'net'))]

	#
	return(netStats)
}
