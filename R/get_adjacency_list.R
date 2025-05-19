#' Longitudinal variants of `get_adjacency()` which returns a list of adjacency matrices
#'
#' This function takes in a dyadic dataset
#' and outputs a list of adjacency matrices in which the actor composition in the network can vary over time.
#'
#' @param dyad_data a dyadic dataframe (or a tibble)
#' @param actor1 character: name of the actor 1 variable in dyad_data
#' @param actor2 character: name of the actor 2 variable in dyad_data
#' @param time character: name of the time variable in dyad_data, the values of the time variable itself should be numeric
#' @param symmetric logical: whether ties are symmetric, default is TRUE
#' @param mode character: whether the network is unipartite or bipartite, default is unipartite
#' @param weight character: name of the weighted edges variable, default is NULL
#' @param sum_dyads logical: whether to sum up the `weight` value when there exists repeating dyads within time periods
#' @param actor_time_uniform logical: whether to assume
#' actors are the same across the full time series observed in the data
#' TRUE means that actors are the same across the full time
#' series observed in the data
#' FALSE means that actors come in and out of the observed data and
#' their "existence" should be determined by the data, meaning that
#' their first year of existence will be determined by the time point
#' of their first event and their last year of existence by the
#' time point of their last event
#' @param actor_pds a data.frame indicating start and end time point for every
#' actor, this can be created manually (see example) or using `get_actor_time_info.R`, 
#' if provided then choice of `actor_time_uniform` is irrelevant.
#' @param diag_to_NA logical: whether diagonals should be set to NA, default is TRUE
#' @param missing_to_zero logical: whether missing values should be set to zero, default is TRUE
#'
#' @return a list of adjacency matrices of class netify
#'
#' @author Cassy Dorff, Ha Eun Choi, Shahryar Minhas, Tosin Salau
#'
#' @examples
#' 
#' 
#' # load example directed event data from ICEWS
#' # this data comes in the form of a dyadic
#' # dataframe where all dyad pairs are listed
#' data(icews)
#' 
#' # generate a longitudinal, directed, and weighted network
#' # where the weights are verbConf
#' # note that in longitudinal networks we can have all the 
#' # same actors in every year and if that's the case
#' # we set actor_time_uniform to TRUE, in the next
#' # example we'll show how to adjust when actors
#' # change over time
#' icews_verbConf <- get_adjacency_list(
#'   dyad_data=icews, 
#'   actor1='i', actor2='j', time='year',
#'   actor_time_uniform=TRUE,
#'   symmetric=FALSE, weight='verbConf' )
#' icews_verbConf
#' 
#' # another example using cow data
#' # gathered from the peacesciencer package
#' library(peacesciencer)
#' library(dplyr)
#' 
#' # create dyadic data set over time (NGOs)
#' cow_dyads <- create_dyadyears( 
#'     subset_years = c(1980:2001)
#'     ) %>%
#'     # add mids
#'     add_cow_mids()
#' 
#' # now lets create a network object in which
#' # we generate list of networks in which the
#' # cross-sections represent mid onset
#' # additionally note that the raw data involves
#' # country years in which we saw countries go in
#' # and out of existence so we set actor_time_uniform
#' # to FALSE
#' mid_network <- netify(
#'   cow_dyads,
#'   actor1='ccode1', actor2='ccode2', time='year',
#'   weight='cowmidonset', 
#'   actor_time_uniform=FALSE,
#'   sum_dyads=FALSE, symmetric=TRUE,
#'   diag_to_NA=TRUE, missing_to_zero=TRUE)
#' mid_network
#' 
#' # you can also supply your own set of actors
#' # with custom start and end years using the
#' # actor_pds argument, to use this first you 
#' # need to create a data.frame with information
#' # about actor composition as follows
#' actor_comp <- data.frame(
#'     actor = c(2, 365, 220, 710),
#'     min_time = c(1980, 1980, 1991, 1980),
#'     max_time = c(2001, 2001, 2001, 2001)
#' )
#' 
#' # now pass this actor_comp object to the 
#' # actor_pds argument
#' mid_network_subset <- netify(
#'   cow_dyads,
#'   actor1='ccode1', actor2='ccode2', time='year',
#'   weight='cowmidonset', 
#'   actor_pds=actor_comp,
#'   sum_dyads=FALSE, symmetric=TRUE,
#'   diag_to_NA=TRUE, missing_to_zero=TRUE)
#' mid_network_subset
#' 
#' 
#' @export get_adjacency_list
#' 

get_adjacency_list <- function(
    dyad_data,
    actor1=NULL, actor2=NULL, time=NULL, 
    symmetric=TRUE, mode='unipartite',
    weight=NULL, sum_dyads=FALSE,
    actor_time_uniform=FALSE,
    actor_pds=NULL,
    diag_to_NA=TRUE, missing_to_zero=TRUE,
    group_by=NULL  # NEW: Added group_by parameter
){
  
  if(!is.null(group_by)) {
    # Get unique values of the grouping variable
    groups <- unique(dyad_data[[group_by]])
    
    # Create a separate list of networks for each group
    network_list <- lapply(groups, function(g) {
      # Subset data for this group
      group_data <- dyad_data[dyad_data[[group_by]] == g, ]
      
      # Call get_adjacency_list recursively (without group_by to avoid infinite recursion)
      result <- get_adjacency_list(
        group_data, actor1, actor2, time, symmetric, mode, 
        weight, sum_dyads, actor_time_uniform, actor_pds,
        diag_to_NA, missing_to_zero
      )
      
      # Add group info as attributes
      attr(result, "group") <- g
      attr(result, "group_var") <- group_by
      return(result)
    })
    
    # Name the list elements by group
    names(network_list) <- as.character(groups)
    
    # Set class to both netify_grouped and list for proper method dispatch
    class(network_list) <- c("netify_grouped", "list")
    
    return(network_list)
  }
  
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
        "Warning: Mode has been inputted as bipartite but actors are not distinct across the modes."
      ) } }    
  
  # check if user supplied actor_pds
  if(!is.null(actor_pds)){ 
    user_actor_pds <- TRUE 
  } else { 
    user_actor_pds <- FALSE }
  
  # check to make sure time variable actually is numeric
  if(!is.numeric(dyad_data[,time])){
    cli::cli_alert_danger(
      'Values in the time variable must be numeric.'
    )
    stop() }
  
  # add weight if not supplied
  wOrig <- weight
  if(is.null(weight)){
    dyad_data$weight_var <- 1 ; weight <- 'weight_var' }
  
  # subset to relevant vars
  dyad_data <- dyad_data[,c(actor1, actor2, time, weight)]
  
  # get vector of time periods and convert to character
  time_pds <- char(unique_vector(dyad_data[,time]))
  
  # if no actor_pds provided then calculate based on actor_time_uniform
  if(is.null(actor_pds)){
    
    # if uniform and no actor_pds provided, then assume
    # actors exist for duration of data
    if(actor_time_uniform){
      actors <- unique_vector(dyad_data[,actor1], dyad_data[,actor2])
      actor_pds <- data.frame(actor=actors, stringsAsFactors=FALSE)
      actor_pds$min_time <- min(dyad_data[,time], na.rm=TRUE)
      actor_pds$max_time <- max(dyad_data[,time], na.rm=TRUE) }
    
    # if not uniform and no actor_pds provided, then calculate
    # entry and exit based on min and max in data
    if(!actor_time_uniform){
      actor_pds <- get_actor_time_info(dyad_data, actor1, actor2, time) }
    
    # define all possible actor1s and actor2s
    a1_all <- unique_vector(dyad_data[,actor1])
    a2_all <- unique_vector(dyad_data[,actor2])
    
    # else actor_pds is provided then subset data to only contain those actors
    # and years those actors were active        
  } else {
    
    # make sure actor_pds is a data.frame
    actor_pds <- df_check(actor_pds)
    
    # make sure every actor has only been entered once
    if(length(unique(actor_pds$actor))!=nrow(actor_pds)){
      cli::cli_alert_danger(
        "Actors are repeating in `actor_pds`. Every actor must show up only once with a unique `min_time` and `max_time`."
      )
      stop() }
    
    # rename first col to actor, second to min_time, third to max_time
    names(actor_pds) <- c('actor', 'min_time', 'max_time')
    
    # create vector of time periods based on entry
    time_pds <- char(unique_vector(actor_pds$min_time, actor_pds$max_time))
    
    # get vector of actors as explicitly defined by user
    actors_rows <- unique_vector(dyad_data[,actor1])
    actors_cols <- unique_vector(dyad_data[,actor2])
    actors <- unique_vector(actors_rows, actors_cols)
    if(mode=='unipartite'){ actors_rows <- actors_cols <- actors }  
    
    # subset dyad data to only include relevant actors and time periods
    dyad_data <- dyad_data[
      dyad_data[,actor1] %in% actors_rows & 
        dyad_data[,actor2] %in% actors_cols &
        dyad_data[,time] %in% time_pds, ]
    
    # stop process if no dyads remain
    if(nrow(dyad_data)==0){
      cli::cli_alert_danger(
        "No dyads remain after subsetting to the actors and years defined in `actor_pds`."
      )
      stop() }
    
    # further pair down data to ensure that we are only including observations
    # for an actor in the years that they were defined as active by the user
    actor_yrs <- unlist(lapply(1:nrow(actor_pds), function(ii){
      paste( 
        actor_pds$actor[ii], 
        actor_pds$min_time[ii]:actor_pds$max_time[ii], sep='_'  ) }))
    actor1_yr <- paste(dyad_data[,actor1], dyad_data[,time], sep='_')
    actor2_yr <- paste(dyad_data[,actor2], dyad_data[,time], sep='_')
    dyad_data <- dyad_data[
      actor1_yr %in% actor_yrs & 
        actor2_yr %in% actor_yrs,]
    
    # define all possible actor1s and actor2s based on user input and data availability
    a1_all <- unique_vector(dyad_data[,actor1])
    a2_all <- unique_vector(dyad_data[,actor2])
    
    # cleanup
    rm(actor1_yr, actor2_yr, actor_yrs)
  }
  
  # check if there are repeating dyads
  num_repeat_dyads <- repeat_dyads_check(dyad_data, actor1, actor2, time)
  if(num_repeat_dyads>0){ edge_value_check(wOrig, sum_dyads, TRUE) }
  
  # aggregate data if sum dyads selected
  if(sum_dyads){
    dyad_data <- agg_across_units(dyad_data, actor1, actor2, time, weight, symmetric, missing_to_zero)
  }
  
  # dump zeros in df so we dont have to iterate through
  # as many rows, but can only do this as long as we can
  # assume that all dyads are present, so no possible NAs
  # and then below we can set NAs to 0
  if(missing_to_zero){
    dyad_data <- dyad_data[dyad_data[,weight] != 0, ] }
  
  # iterate through time periods
  adj_out <- lapply( time_pds, function(time_pd){
    
    # subset to time period
    slice <- dyad_data[dyad_data[,time] == time_pd,]
    
    # determine if actor exists in selected time_pd
    actor_present <- apply(
      actor_pds[,2:3], 1, function(x){ time_pd %in% x[1]:x[2] })
    actors <- actor_pds$actor[actor_present]
    actors <- actors_rows <- actors_cols <- sort(actors)
    
    # break up into rows and cols 
    if(mode=='bipartite'){
      actors_rows <- sort( actor_pds$actor[actor_present & actor_pds$actor %in% a1_all] )
      actors_cols <- sort( actor_pds$actor[actor_present & actor_pds$actor %in% a2_all] )
    }
    
    # assign cross-section value for adjmat depending on user inputs
    value <- slice[,weight]
    
    # create logical value that is TRUE if weight is just 0/1
    # and false otherwise
    weight_binary <- TRUE
    weight_vals <- unique(value)
    if(any(weight_vals != 0 & weight_vals != 1)){
      weight_binary <- FALSE }
    rm(weight_vals)
    
    # get adj mat filled in
    adj_mat <- get_matrix(
      n_rows=length(actors_rows),
      n_cols=length(actors_cols),
      actors_rows=actors_rows,
      actors_cols=actors_cols,
      matRowIndices=match(slice[,actor1], actors_rows),
      matColIndices=match(slice[,actor2], actors_cols),
      value=value,
      symmetric=symmetric )
    
    # add zeros for non-relationships (this should be a logical)
    if(missing_to_zero){ adj_mat[is.na(adj_mat)] <- 0 }
    
    # set diagonals to NA
    if(diag_to_NA & mode=='unipartite' ){ diag(adj_mat) <- NA }
    
    # if user left weight NULL and set sum_dyads
    # to FALSE then record weight as NULL for
    # attribute purposes
    if(!sum_dyads & is.null(wOrig)){ weight <- NULL }
    
    # layer label
    if(is.null(weight)){ layer_label <- 'weight1' } else{ layer_label <- weight }
    
    # add class info
    class(adj_mat) <- 'netify'
    attr(adj_mat, 'netify_type') <- 'cross_sec'
    attr(adj_mat, 'actor_time_uniform') <- NULL
    attr(adj_mat, 'actor_pds') <- NULL
    attr(adj_mat, 'weight') <- weight
    attr(adj_mat, 'detail_weight') <- weight_label
    attr(adj_mat, 'weight_binary') <- weight_binary    
    attr(adj_mat, 'symmetric') <- user_symmetric
    attr(adj_mat, 'mode') <- mode
    attr(adj_mat, 'layers') <- layer_label
    attr(adj_mat, 'diag_to_NA') <- diag_to_NA
    attr(adj_mat, 'missing_to_zero') <- missing_to_zero
    attr(adj_mat, 'sum_dyads') <- sum_dyads    
    attr(adj_mat, 'nodal_data') <- NULL
    attr(adj_mat, 'dyad_data') <- NULL
    
    #
    return(adj_mat)
  } ) # close lapply
  
  # label list elements
  names(adj_out) <- time_pds
  
  # if user left weight NULL and set sum_dyads
  # to FALSE then record weight as NULL for
  # attribute purposes
  if(!sum_dyads & is.null(wOrig)){ weight <- NULL }
  
  # if user supplied actor_pds then set 
  # actor_time_uniform to FALSE
  if(user_actor_pds){ actor_time_uniform <- FALSE }
  
  # layer label
  if(is.null(weight)){ layer_label <- 'weight1' } else{ layer_label <- weight }
  
  # get info on binary weights
  bin_check <- unlist(lapply(adj_out, function(x){ attr(x, 'weight_binary') }))
  
  # add attributes to list
  class(adj_out) <- 'netify'
  attr(adj_out, 'netify_type') <- 'longit_list'
  attr(adj_out, 'actor_time_uniform') <- actor_time_uniform
  attr(adj_out, 'actor_pds') <- actor_pds
  attr(adj_out, 'weight') <- weight
  attr(adj_out, 'detail_weight') <- weight_label
  attr(adj_out, 'weight_binary') <- all(bin_check)
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
