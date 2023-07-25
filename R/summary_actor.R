#' Summary method to get actor level statistics for netify objects
#'
#' `summary_actor` takes in a netify object
#' and outputs a data.frame with actor level statistics.
#'
#' @param netlet object of class netify, produced by get_adjacency
#' @param ... additional parameters not used
#' @return a data.frame object summarizing actor level stats of the network(s)
#' @author Ha Eun Choi, Shahryar Minhas
#'
#' @export summary_actor
#' @export

summary_actor <- function( netlet, ...) {

  # check if netify object
  netify_check(netlet)

  # get summary args
  summaryArgs <- list(...)

  # pull out attrs
  objAttrs <- attributes(netlet)

  # get type
  cNetType <- objAttrs$netify_type

  # if cross sec convert to a list object so that
  # we can use lapply
  if(cNetType == 'cross_sec'){ object <- list(netlet)}
  if(cNetType == 'longit_array'){ object <- array_to_list(netlet)}
  if(cNetType == 'longit_list'){ object <- netlet}

  netStats_actor <- lapply(object, function(mat){
    # degree centrality

    # average degree in
    average_degree_in <- rowMeans(mat, na.rm=TRUE)
    average_degree_out <- colMeans(mat, na.rm=TRUE)
    average_degree_total <- average_degree_in + average_degree_out

    # set diagonals to zero
    diag(mat) <- 0

    # if symmetric compute eigenvector centrality
    # scores for actors
    if(attr(netlet, "symmetric")){

      # calc decomp and pull out first eigenvec
      decomp <- eigen(mat)
      decompVectors <- decomp$vectors[,1]

      # if complex arguments detected then
      # return NA with warning
      if(any(is.complex(decompVectors))){
        eigenvec <- NA
        print('Eigenvector is complex')
      } else {
        eigenvec <- decompVectors/max(decompVectors, na.rm=TRUE)
      }

      # combine results for symmetric case
      out <- data.frame(
        average_degree_in=average_degree_in,
        average_degree_out=average_degree_out,
        average_degree_total=average_degree_total,
        eigenvec=eigenvec
      )
    }

    # if asymmetric compute svd centrality
    # scores for actors
    if(!attr(netlet, "symmetric")){

      # authority score
      decomp <- svd(mat) # singular value decomposition of mat
      decompV <- decomp$v[,1] # extract the right vectors
      authority <- scale(decompV) # scale the extracted matrix and save

      # hub score
      decompU <- decomp$u[,1] # extract the left vectors
      hub <- scale(decompU) # scale the extracted matrix and save

      # combine results for asymmetric case
      out <- data.frame(
        average_degree_in=average_degree_in,
        average_degree_out=average_degree_out,
        average_degree_total=average_degree_total,
        authority=authority,
        hub=hub
      )
    }

    # calculate any userstats
    if( 'other_stats' %in% names(summaryArgs) ){
      other_out <- lapply(summaryArgs$'other_stats', function(stat){ stat(mat) })
      out <- append(out, unlist(other_out))
    }

    return(out) })

  # organize all computed actors stats into a data.frame
  netStats_actor <- do.call('rbind', netStats_actor)
  netStats_actor <- data.frame(netStats_actor, stringsAsFactors=FALSE)

  # move variable label to be a column
  netStats_actor$actor <- rownames(netStats_actor)
  rownames(netStats_actor) <- NULL

  # if cross-sectional then cleanup
  if(cNetType == 'cross_sec'){
    netStats_actor <- netStats_actor[,c('actor', setdiff(names(netStats_actor), 'actor'))]
    return(netStats_actor)}

  # if longitudinal, `actor`` contains both year and name information
  if(cNetType != 'cross_sec'){
    netStats_actor$time = unlist(lapply(strsplit(netStats_actor$actor, '.', fixed=TRUE), function(x){ x[1] }))
    netStats_actor$actor = unlist(lapply(strsplit(netStats_actor$actor, '.', fixed=TRUE), function(x){ x[2] }))
    netStats_actor$time <- as.numeric(netStats_actor$time)
  }

  # cleanup?
  vars <- c('actor', "time")
  netStats_actor <- netStats_actor[,c(vars, setdiff(names(netStats_actor), vars))]
  return(netStats_actor)
}
