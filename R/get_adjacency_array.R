#' Longitudinal variants of `get_adjacency()` which returns a three-dimensional array
#'
#' `get_adjacency_array` takes in a dyadic dataset
#' and outputs an three-dimensional array with the first two dimensions corresponding to actors
#' and the third dimension corresponding to time. This function should be used only
#' when the actor composition remains constant throughout time.
#'
#' @param dyad_data a dyadic dataframe (or a tibble)
#' @param actor1 character: name of the actor 1 variable in dyad_data
#' @param actor2 character: name of the actor 2 variable in dyad_data
#' @param time character: name of the time variable in dyad_data, the values of the time variable itself should be numeric
#' @param symmetric logical: whether ties are symmetric, default is TRUE
#' @param mode character: whether the network is unipartite or bipartite, default is unipartite
#' @param weight character: name of the weighted edges variable, default is NULL
#' @param sum_dyads logical: whether to sum up the `weight` value when there exists repeating dyads
#' @param diag_to_NA logical: whether diagonals should be set to NA, default is TRUE
#' @param missing_to_zero logical: whether missing values should be set to zero, default is TRUE
#' @return an array of class netify
#' @author Cassy Dorff, Ha Eun Choi, Shahryar Minhas
#' @examples
#'
#' # load example directed event data from ICEWS
#' # this data comes in the form of a dyadic
#' # dataframe where all dyad pairs are listed
#' data(icews)
#' 
#' # generate a longitudinal, directed and weighted network
#' # where the weights are matlConf
#' icews_matlConf <- get_adjacency_array(
#'   dyad_data=icews, 
#'   actor1='i', actor2='j', time='year',
#'   symmetric=FALSE, weight='matlConf' )
#'
#' @export get_adjacency_array

get_adjacency_array <- function(
  dyad_data,
  actor1=NULL, actor2=NULL, time=NULL,
  symmetric=TRUE, mode='unipartite',
	weight=NULL, sum_dyads=FALSE,
  diag_to_NA=TRUE, missing_to_zero=TRUE
){

  # create weight string for storage as attribtue 
  # in netify object
  weight_label <- weight_string_label(weight, sum_dyads)

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

  # check to make sure time variable actually is numeric
  if(!is.numeric(dyad_data[,time])){
    cli::cli_alert_danger('Values in the time variable must be numeric.')
    stop() }

  # add weight if not supplied
  wOrig <- weight
  if(is.null(weight)){
    dyad_data$weight_var <- 1 ; weight <- 'weight_var' }

  # subset to relevant vars
  dyad_data <- dyad_data[,c(actor1, actor2, time, weight)]

	# get vector of time periods and convert to character
  time_pds <- char(unique_vector(dyad_data[,time]))

  # get vector of actors
  actors_rows <- unique_vector(dyad_data[,actor1])
  actors_cols <- unique_vector(dyad_data[,actor2])
  actors <- unique_vector(actors_rows, actors_cols)
  if(mode=='unipartite'){ actors_rows <- actors_cols <- actors }  

  # add info on actor time sample
  actor_pds <- data.frame(
    actor=actors, stringsAsFactors=FALSE)
  actor_pds$min_time <- time_pds[1]
  actor_pds$max_time <- time_pds[length(time_pds)]

  # check if there are repeating dyads
  num_repeat_dyads <- repeat_dyads_check(dyad_data, actor1, actor2, time)
  if(num_repeat_dyads>0){ edge_value_check(wOrig, sum_dyads, TRUE) }

  # aggregate data if sum dyads selected
  if(sum_dyads){
    dyad_data <- agg_across_units(dyad_data, actor1, actor2, time, weight, symmetric, missing_to_zero)
  }

	# organize array
	n_rows <- length(actors_rows)
	n_cols <- length(actors_cols)  
	t <- length(time_pds)
	adj_out <- array(NA, 
    dim=c(n_rows,n_cols,t), 
    dimnames=list(actors_rows,actors_cols,time_pds))

	# iterate through third mode and fill in
	for(time_pd in time_pds){

    # subset to time period
    slice <- dyad_data[dyad_data[,time] == time_pd,]
    
    # assign cross-section value for adjmat depending on user inputs
    value <- slice[,weight]
    
    # get adj mat filled in
    adj_mat <- get_matrix(
      n_rows=length(actors_rows),
      n_cols=length(actors_cols),
      actors_rows=actors_rows,
      actors_cols=actors_cols,
      matRowIndices=match(slice[,actor1], actors_rows),
      matColIndices=match(slice[,actor2], actors_cols),
      value=value,
      symmetric=user_symmetric )
    
  # add zeros for non-relationships (this should be a logical)
  if(missing_to_zero){ adj_mat[is.na(adj_mat)] <- 0 }

  # set diagonals to NA
  if(diag_to_NA & mode=='unipartite' ){ diag(adj_mat) <- NA }

  # insert into array
  adj_out[,,as.character(time_pd)] <- adj_mat }

  # if user left weight NULL and set sum_dyads
  # to FALSE then record weight as NULL for
  # attribute purposes
  if(!sum_dyads & is.null(wOrig)){ weight <- NULL }

  # layer label
  if(is.null(weight)){ layer_label <- 'weight1' } else{ layer_label <- weight }

  # add attributes to list
  class(adj_out) <- 'netify'
  attr(adj_out, 'netify_type') <- 'longit_array'
	attr(adj_out, 'actor_time_uniform') <- TRUE
	attr(adj_out, 'actor_pds') <- actor_pds
  attr(adj_out, 'weight') <- weight
  attr(adj_out, 'detail_weight') <- weight_label
  attr(adj_out, 'symmetric') <- user_symmetric
  attr(adj_out, 'mode') <- mode
  attr(adj_out, 'layers') <- layer_label  
  attr(adj_out, 'diag_to_NA') <- diag_to_NA
  attr(adj_out, 'missing_to_zero') <- missing_to_zero
  attr(adj_out, 'sum_dyads') <- sum_dyads  
  attr(adj_out, 'nodal_data') <- NULL
  attr(adj_out, 'dyad_data') <- NULL
  attr(adj_out, 'graph_data') <- NULL

#
return(adj_out)
}
