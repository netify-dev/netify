#' Converts a dyadic data into a network in various formats
#'
#' `get_adjacency` takes in a dyadic dataset and outputs an adjacency matrix representation.
#'
#' @param dyad_data a dyadic dataframe (or a tibble)
#' @param actor1 character: name of the actor 1 in dyad_data
#' @param actor2 character: name of the actor 2 in dyad_data
#' @param symmetric logical: whether ties are symmetric, default is TRUE
#' @param mode character: whether the network is unipartite or bipartite, default is unipartite
#' @param weight character: name of the weighted edges variable, default is NULL
#' @param sum_dyads logical: whether to sum up the `weight` value when there exists repeating dyads within the dataset
#' @param diag_to_NA logical: whether diagonals should be set to NA, default is TRUE
#' @param missing_to_zero logical: whether missing values should be set to zero, default is TRUE
#'
#' @return an adjacency matrix of class netify
#'
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @examples
#'
#' # load example directed event data from ICEWS
#' # this data comes in the form of a dyadic
#' # dataframe where all dyad pairs are listed
#' data(icews)
#' 
#' # subset to a particular year
#' icews <- icews[icews$year=='2010', ]
#' 
#' # generate a cross sectional, directed, and weighted network
#' # where the weights are verbCoop
#' icews_verbCoop <- get_adjacency(
#'   dyad_data=icews, actor1='i', actor2='j',
#'   symmetric=FALSE, weight='verbCoop' )

#' # generate a cross sectional, directed and weighted network
#' # where the weights are matlConf
#' icews_matlConf <- get_adjacency(
#'   dyad_data=icews, actor1='i', actor2='j',
#'   symmetric=FALSE, weight='matlConf' )
#' 
#' 
#' @export get_adjacency
#' 

get_adjacency <- function(
  dyad_data,
  actor1=NULL, actor2=NULL, 
  symmetric=TRUE, mode='unipartite',
  weight=NULL, sum_dyads=FALSE, 
  diag_to_NA=TRUE, missing_to_zero=TRUE
){

  # if bipartite network then force diag_to_NA to be FALSE
  # and force asymmetric, create copy to preserve user choice
  # for use in prep_ fns
  user_symmetric <- symmetric  
  if(mode=='bipartite'){
    diag_to_NA <- FALSE
    symmetric <- FALSE }

  # if mode bipartite is specified make sure that
  # actors in actor1 and actor2 columns are distinct
  if(mode=='bipartite'){
      if( length(intersect(dyad_data[,actor1], dyad_data[,actor2]))>0 ){
          cli::cli_alert_warning(
              "Warning: Mode has been inputted as bipartite but actors are not distinct across the modes.") } }

  # create weight string for storage as attribtue 
  # in netify object
  weight_label <- weight_string_label(weight, sum_dyads)

  # add weight if not supplied
  wOrig <- weight
  if(is.null(weight)){
    dyad_data$weight_var <- 1 ; weight <- 'weight_var' }

  # subset to relevant vars
  dyad_data <- dyad_data[,c(actor1, actor2, weight)]

  # get vector of actors
  actors_rows <- unique_vector(dyad_data[,actor1])
  actors_cols <- unique_vector(dyad_data[,actor2])
  actors <- unique_vector(actors_rows, actors_cols)
  if(mode=='unipartite'){ actors_rows <- actors_cols <- actors }

  # actor year info
  actor_pds <- data.frame(
    actor=actors, stringsAsFactors=FALSE)
  actor_pds$min_time <- 1
  actor_pds$max_time <- 1

  # check if there are repeating dyads
  num_repeat_dyads <- repeat_dyads_check(dyad_data, actor1, actor2)
  if(num_repeat_dyads>0){ edge_value_check(wOrig, sum_dyads, TRUE) }
   
  # aggregate data if sum dyads selected
  if(sum_dyads){
    dyad_data <- agg_across_units(dyad_data, actor1, actor2, NULL, weight, symmetric, missing_to_zero)
  }
  
  # dump zeros in df so we dont have to iterate through
  # as many rows, but can only do this as long as we can
  # assume that all dyads are present, so no possible NAs
  # and then below we can set NAs to 0
  if(missing_to_zero){
    dyad_data <- dyad_data[dyad_data[,weight] != 0, ] }

  # assign cross-section value for adjmat depending on user inputs
  value <- dyad_data[,weight]

  # create logical value that is TRUE if weight is just 0/1
  # and false otherwise
  weight_binary <- TRUE
  weight_vals <- unique(value)
  if(any(weight_vals != 0 & weight_vals != 1)){
    weight_binary <- FALSE }
  rm(weight_vals)

  # convert to adjacency matrix
  adj_out <- get_matrix(
      n_rows=length(actors_rows),
      n_cols=length(actors_cols),
      actors_rows=actors_rows,
      actors_cols=actors_cols,
      matRowIndices=match(dyad_data[,actor1], actors_rows),
      matColIndices=match(dyad_data[,actor2], actors_cols),
      value=value,
      symmetric=symmetric)

  # add zeros for non-relationships (this should be a logical)
  if(missing_to_zero){ adj_out[is.na(adj_out)] <- 0 }

  # set diagonals to NA
  if(diag_to_NA & mode=='unipartite' ){ diag(adj_out) <- NA }

  # if user left weight NULL and set sum_dyads
  # to FALSE then record weight as NULL for
  # attribute purposes
  if(!sum_dyads & is.null(wOrig)){ weight <- NULL }

  # layer label
  if(is.null(weight)){ layer_label <- 'weight1' } else{ layer_label <- weight }

  # add class info
  class(adj_out) <- 'netify'
  attr(adj_out, 'netify_type') <- 'cross_sec'
  attr(adj_out, 'actor_time_uniform') <- TRUE
  attr(adj_out, 'actor_pds') <- actor_pds
  attr(adj_out, 'weight') <- weight
  attr(adj_out, 'detail_weight') <- weight_label
  attr(adj_out, 'weight_binary') <- weight_binary
  attr(adj_out, 'symmetric') <- user_symmetric
  attr(adj_out, 'mode') <- mode
  attr(adj_out, 'layers') <- layer_label
  attr(adj_out, 'diag_to_NA') <- diag_to_NA
  attr(adj_out, 'missing_to_zero') <- missing_to_zero
  attr(adj_out, 'sum_dyads') <- sum_dyads  
  attr(adj_out, 'nodal_data') <- NULL
  attr(adj_out, 'dyad_data') <- NULL

  #
  return(adj_out)
}

