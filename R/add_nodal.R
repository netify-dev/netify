#' Adds nodal data to a netify object
#'
#' `add_nodal` takes in a dataframe and outputs a netify object.
#'
#' @param netlet a netify object
#' @param node_data a dataframe object
#' @param actor a character object indicating which variable in node_data uniquely IDs each node
#' @param time a character object indicating which variable in node_data tracks time
#' @param node_vars a vector of which variables from node_data should be merged
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
#' 
#' verbCoop_net <- netify(
#'   dyad_data=icews_10,
#'   actor1 = 'i', actor2 = 'j', 
#'   symmetric=FALSE, weight='verbCoop' )
#' 
#' # nodal data should be a dataframe with one row per actor
#' # in the cross-sectional case and one row per actor per 
#' # time period in the longitudinal case, e.g.:
#' nvars = c(
#'   'i_polity2',
#'   'i_gdp', 'i_log_gdp',
#'   'i_pop', 'i_log_pop' )
#' nodeData <- unique(icews_10[,c('i', nvars)])
#' head(nodeData)
#' 
#' verbCoop_net <- add_nodal( 
#'   netlet=verbCoop_net, node_data=nodeData, 
#'   actor='i', node_vars=nvars )
#' 
#' # nodal data is stored in the nodal_data attribute
#' # as a data.frame, it can be accessed in the following way:
#' node_data <- attr(verbCoop_net, 'nodal_data')
#' head(node_data)
#' 
#' # longitudinal case
#' verbCoop_longit_net <- netify(
#'     dyad_data=icews, 
#'     actor1='i', actor2='j', time='year',
#'     symmetric=FALSE,
#'     weight='verbCoop' )
#' 
#' nodeData <- unique(icews[,c('i', 'year', nvars)])
#' 
#' verbCoop_longit_net <- add_nodal(
#'   netlet=verbCoop_longit_net,
#'   node_data=nodeData, 
#'   actor='i', time='year',
#'   node_vars = nvars )
#' 
#' # and in the longitudinal case, it can be accessed
#' # in the same way
#' node_data <- attr(verbCoop_longit_net, 'nodal_data')
#' head(node_data)
#' 
#' @author Colin Henry, Shahryar Minhas
#'
#' @export add_nodal

add_nodal <- function( 
  netlet, node_data, actor=NULL, 
  time=NULL, node_vars=NULL, 
  replace_existing=FALSE
  ){

  # Sanity check + error message
  netify_check(netlet)
  node_data <- df_check(node_data)  
  actor_check(actor, actor, node_data)  
  add_var_time_check(netlet, time)

  # determine variables to merge if specific ones are not provided
  if(is.null(node_vars)){ 
    node_vars <- setdiff(names(node_data), c(actor, time)) }

  # get underlying frame of netify object based on the
  # actor_pds attribute

  # if nodal data doesnt exist then create 
  # frame based on actor pds, we also have to do 
  # an adjustment in the time NULL case by removing
  # the autmoatically created time variable, we remove
  # the time column because in the time NULL already
  # present nodal data case there is no time column
  if(is.null(attr(netlet, 'nodal_data'))){
    frame <- actor_pds_to_frame(attributes(netlet)$actor_pds)
    if(is.null(time)){ frame <- frame[,-2,drop=FALSE] } }

  # if nodal data already present then pull out of netlet
  # and also use replace_existing logical to determine
  # whether variables should be replaced or not
  if(!is.null(attr(netlet, 'nodal_data'))){
    frame = attr(netlet, 'nodal_data')
    if(replace_existing){
      node_vars <- setdiff(node_vars, names(frame)) } }
  
  # create corresponding id in node_data
  if(is.null(time)){
    node_dataID = paste( node_data[,actor], '1', sep='_' )
    frame_dataID = paste( frame[,'actor'], '1', sep='_' ) }
  if(!is.null(time)){
    node_dataID = paste( 
      node_data[,actor], node_data[,time], sep='_' )
    frame_dataID = paste(
      frame[,'actor'], frame[,'time'], sep='_' ) }

  # add selected nodal data as an attribute
  nfcol = ncol(frame)
	frame = cbind(
    frame, 
    node_data[ 
      match(frame_dataID, node_dataID), 
      node_vars, drop=FALSE ] )
	names(frame)[(nfcol+1):ncol(frame)] <- node_vars

  # cleanup and add as attrib
  rownames(frame) <- NULL
  attr(netlet, 'nodal_data') <- frame

  # Return object
  return(netlet)
}
