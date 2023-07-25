#' Add dyad data to a netify object
#'
#' `add_dyad` takes in a dataframe and outputs a netify object.
#'
#' @param netlet a netify object
#' @param dyad_data a dataframe object
#' @param actor1 character: actor 1 in the data
#' @param actor2 character: actor 2 in the data
#' @param time a character object indicating which variable in dyad_data tracks time
#' @param dyad_vars a vector of which variables from dyad_data should be merged
#' @param dyad_vars_symmetric logical vector: whether ties are symmetric, default is TRUE
#' @param replace_existing a logical indicating whether to replace existing nodal data
#'
#' @return a netify object
#'
#' @examples 
#'  
#' data(icews)
#' 
#' # cross-sectional case
#' icews_10 <- icews[icews$year==2010,]
#' 
#' verbCoop_net <- netify(
#'   dyad_data=icews_10,
#'   actor1 = 'i', actor2 = 'j', 
#'   symmetric=FALSE, weight='verbCoop' )
#' 
#' verbCoop_net <- add_dyad(
#'   netlet=verbCoop_net, 
#'   dyad_data=icews_10, 
#'   actor1='i', actor2='j', 
#'   dyad_vars=c('matlCoop', 'verbConf', 'matlConf'),
#'   dyad_vars_symmetric = rep(FALSE, 3) )
#' 
#' # dyadic data is stored in the dyad_data attribute
#' # as an array, it can be accessed in the following way:
#' dyad_array <- attr(verbCoop_net, 'dyad_data')[[1]]
#' dim(dyad_array)
#' 
#' # the dimensions of the array are: nr x nc x pd, where
#' # nr is the number of row actors, nc is the number of column actors,
#' # and pd is the number of dyadic variables
#' 
#' # longitudinal case
#' verbCoop_longit_net <- netify(
#'     dyad_data=icews, 
#'     actor1='i', actor2='j', time='year',
#'     symmetric=FALSE,
#'     weight='verbCoop' )
#' 
#' verbCoop_longit_net <- add_dyad(
#'     netlet=verbCoop_longit_net,
#'     dyad_data=icews, 
#'     actor1='i', actor2='j', time='year',
#'     dyad_vars = c('matlCoop', 'verbConf', 'matlConf'),
#'     dyad_vars_symmetric=rep(FALSE, 3) )
#' 
#' # dyadic data in the longit case is still stored in
#' # the dyad_data attribute but now as a list of arrays, 
#' # it can be accessed in the following way:
#' dyad_array_list <- attr(verbCoop_longit_net, 'dyad_data')
#' dim(dyad_array_list[['2002']])
#' 
#' # the names of the list elements correspond to the time
#' # periods and each array within the list is of the same
#' # dimensions as the array in the cross-sectional case
#' 
#' @author Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @export add_dyad
#' 

add_dyad <- function(
  netlet, dyad_data, 
  actor1=NULL, actor2=NULL, time=NULL,
  dyad_vars=NULL, dyad_vars_symmetric=NULL,
  replace_existing=FALSE
  ){

  # user input checks
  netify_check(netlet)
  dyad_data <- df_check(dyad_data)
  actor_check(actor1, actor2, dyad_data)
  add_var_time_check(netlet, time)

  # some attribute info
  netlet_type <- attr(netlet, 'netify_type')
  netlet_mode <- attr(netlet, 'mode')
  actor_unif <- attr(netlet, 'actor_time_uniform')

  # determine variables to merge if specific ones are not provided
  if(is.null(dyad_vars)){ 
    dyad_vars = setdiff(names(dyad_data), c(actor1, actor2, time)) }

  # if user has not filled in dyadsVarsSymmetric logical then we need to choose for them
  # yell at user about supplying a choice for dyadsVarsSymmetric
  if( is.null( dyad_vars_symmetric ) & netlet_mode != 'bipartite' ){ 
    dyad_vars_symmetric = rep(attr(netlet, 'symmetric'), length(dyad_vars))
    cli::cli_alert_warning(
      "Warning: When adding dyadic variables it is best to specify whether the variable being added is symmetric or not") }
  if( netlet_mode == 'bipartite'  ){
    dyad_vars_symmetric = rep(FALSE, length(dyad_vars)) }

  # count up number of dyad_vars
  ndVars = length(dyad_vars)

  # check to see if there is already a dyad_data attribute in the
  # netify object
  dyad_data_0 <- attributes(netlet)$dyad_data
  dyad_data_attrib_exists <- !is.null( dyad_data_0 )

  # if dyad_data attribute already exists and replace_existing is TRUE
  # then remove any vars that are already in the netlet dyad data attrib
  if( dyad_data_attrib_exists & replace_existing ){
    to_keep <- !(dimnames(dyad_data_0[[1]])[[3]] %in% dyad_vars)
    dyad_data_0 <- lapply(dyad_data_0, function(x){ 
      x[,,to_keep,drop=FALSE] })
  }

  # get netlet measurements
  msrmnts <- netify_measurements(netlet)

  # if cross_sec put in a 1 for time
  if(is.null(msrmnts$time)) { msrmnts$time <- 1 }

  # construct arrays by year
  arrayList <- lapply( msrmnts$time, function(timePd){

    # actors that should be in the array at this time point
    if( netlet_type == 'longit_list'){
      actors_rows <- msrmnts$row_actors[[timePd]]
      actors_cols <- msrmnts$col_actors[[timePd]]
      n_actors_rows <- msrmnts$n_row_actors[[timePd]]
      n_actors_cols <- msrmnts$n_col_actors[[timePd]] }
    if( netlet_type %in% c('longit_array', 'cross_sec') ){
      actors_rows <- msrmnts$row_actors
      actors_cols <- msrmnts$col_actors
      n_actors_rows <- msrmnts$n_row_actors
      n_actors_cols <- msrmnts$n_col_actors }

    # construct array where data will be stored
    dyadVarArray = array(NA, 
      dim=c(n_actors_rows, n_actors_cols, ndVars), 
      dimnames=list(actors_rows, actors_cols, dyad_vars) )

    # now slice up dyad_data object
    if( !is.null(time) & attributes(netlet)$netify_type != 'cross_sec' ){
      slice = dyad_data[dyad_data[,time]==timePd,c(actor1,actor2,dyad_vars)]
    } else {
      slice <- dyad_data
    }

    # only keep rows in slice that are in the netlet object
    slice <- slice[ 
      slice[,actor1] %in% actors_rows & 
      slice[,actor2] %in% actors_cols, ]

    # iterate though variables from dyad_data and fill in array
    for(ii in 1:ndVars){
      dyadVarArray[,,dyad_vars[ii]] <- get_matrix(
        n_rows=length(actors_rows),
        n_cols=length(actors_cols),
        actors_rows=actors_rows,
        actors_cols=actors_cols,
        matRowIndices=match(slice[,actor1], actors_rows),
        matColIndices=match(slice[,actor2], actors_cols),
        value=slice[,dyad_vars[ii]],
        symmetric=dyad_vars_symmetric[ii] ) }

    # if dyad data attribute already existed, now combine it with the
    # new dyad data
    if( dyad_data_attrib_exists ){
      dyadVarArray <- abind::abind( dyad_data_0[[as.character(timePd)]], dyadVarArray, along=3 )
    }

    #
    return(dyadVarArray) })

  # add year labels to list
  names(arrayList) <- msrmnts$time

  # add selected nodal data as an attribute
  attr(netlet, 'dyad_data') <- arrayList

  # Return object
  return(netlet)
}
