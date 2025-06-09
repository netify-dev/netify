#' Add nodal variables to a netify object
#'
#' `add_node_vars` (also available as `add_vertex_attributes`) 
#' merges nodal (vertex-level) variables from a data.frame into an 
#' existing netify object. This function allows you to incrementally build up 
#' the nodal attributes of your network after initial creation, which is useful 
#' when actor-level variables come from different sources or need different 
#' preprocessing.
#'
#' @param netlet A netify object (class "netify") to which nodal variables will be added.
#' @param node_data A data.frame object containing the nodal variables to add. For 
#'   cross-sectional networks, must have one row per unique actor. For longitudinal 
#'   networks, must have one row per actor-time combination. Will be coerced to 
#'   data.frame if a tibble or data.table is provided.
#' @param actor Character string specifying the column name in node_data that uniquely 
#'   identifies each actor. This should contain the same actor identifiers used in 
#'   the original netify object.
#' @param time Character string specifying the column name in node_data for time periods. 
#'   Required for longitudinal netify objects. Should match the time specification 
#'   used when creating the netify object. Set to NULL for cross-sectional networks.
#' @param node_vars Character vector of column names from node_data to add as nodal 
#'   variables. If NULL (default), all columns except actor and time will be added.
#' @param replace_existing Logical scalar. If TRUE, existing nodal variables with the 
#'   same names will be replaced. If FALSE (default), attempting to add variables 
#'   that already exist will result in an error.
#'
#' @return A netify object (class "netify") with the additional nodal variables stored in the 
#'   'nodal_data' attribute as a data.frame. The structure includes:
#'   \itemize{
#'     \item \strong{actor}: Column with actor identifiers
#'     \item \strong{time}: Column with time periods (longitudinal only)
#'     \item \strong{nodal variables}: Columns for each variable specified in node_vars
#'   }
#'
#' @details
#' Nodal variables are stored as a data.frame where each row represents an actor 
#' (cross-sectional) or an actor-time combination (longitudinal). This format 
#' allows for efficient storage and easy manipulation of actor-level attributes.
#' 
#' The function automatically handles merging based on actor identifiers, ensuring 
#' that nodal attributes are properly aligned with the network structure. For 
#' longitudinal networks, the function matches both actor and time dimensions.
#' 
#' Missing actors in the node_data will result in NA values for those actors' 
#' attributes in the netify object. Similarly, if node_data contains actors not 
#' present in the network, those rows will be ignored.
#' 
#'
#' @note 
#' The input `node_data` must be a `data.frame` or an object that can be coerced 
#' into a `data.frame` (e.g., a `tibble` or `data.table`). Inputs such as matrices 
#' or arrays are not supported.
#' 
#' For longitudinal networks, ensure that node_data contains entries for all 
#' actor-time combinations you wish to have attributes for. Missing combinations 
#' will result in NA values for those actors at those time points.
#' 
#' When working with large networks, the nodal data storage is more memory-efficient 
#' than dyadic data, as it scales linearly with the number of actors rather than 
#' quadratically.
#'
#' @examples 
#' # Load example data
#' data(icews)
#' 
#' # Cross-sectional example
#' icews_10 <- icews[icews$year == 2010,]
#' 
#' # Create initial netify object
#' verbCoop_net <- netify(
#'   icews_10,  # data.frame input
#'   actor1 = 'i', actor2 = 'j', 
#'   symmetric = FALSE, 
#'   weight = 'verbCoop'
#' )
#' 
#' # Prepare nodal data - one row per unique actor
#' nvars <- c('i_polity2', 'i_gdp', 'i_log_gdp', 'i_pop', 'i_log_pop')
#' nodeData <- unique(icews_10[, c('i', nvars)])
#' class(nodeData)  # "data.frame"
#' nrow(nodeData)   # Number of unique actors
#' head(nodeData)
#' 
#' # Add nodal variables
#' verbCoop_net <- add_node_vars( 
#'   netlet = verbCoop_net,  # netify object
#'   node_data = nodeData,   # data.frame with actor attributes
#'   actor = 'i',            # column identifying actors
#'   node_vars = nvars       # variables to add
#' )
#' 
#' # Access nodal data (returns data.frame)
#' node_data_stored <- attr(verbCoop_net, 'nodal_data')
#' class(node_data_stored)  # "data.frame"
#' head(node_data_stored)
#' names(node_data_stored)  # "actor" plus variable names
#' 
#' # Longitudinal example
#' verbCoop_longit_net <- netify(
#'   icews,  # data.frame input
#'   actor1 = 'i', actor2 = 'j', time = 'year',
#'   symmetric = FALSE,
#'   weight = 'verbCoop'
#' )
#' 
#' # Prepare longitudinal nodal data - one row per actor-time combination
#' nodeData_longit <- unique(icews[, c('i', 'year', nvars)])
#' class(nodeData_longit)  # "data.frame"
#' nrow(nodeData_longit)   # Number of actor-time combinations
#' 
#' # Add nodal variables with time dimension
#' verbCoop_longit_net <- add_node_vars(
#'   netlet = verbCoop_longit_net,  # netify object
#'   node_data = nodeData_longit,   # data.frame with longitudinal data
#'   actor = 'i',                   # column identifying actors
#'   time = 'year',                 # column identifying time
#'   node_vars = nvars              # variables to add
#' )
#' 
#' # Access longitudinal nodal data
#' node_data_longit <- attr(verbCoop_longit_net, 'nodal_data')
#' class(node_data_longit)  # "data.frame"
#' head(node_data_longit)   # Now includes time column
#' 
#' # Filter to specific time period
#' node_data_2010 <- node_data_longit[node_data_longit$time == "2010", ]
#' nrow(node_data_2010)  # Number of actors in 2010
#' 
#' # Example: Add variables from external source
#' \dontrun{
#' # Suppose you have additional actor data
#' external_data <- data.frame(
#'   i = unique(icews_10$i),
#'   democracy_score = runif(length(unique(icews_10$i)), 0, 10),
#'   trade_openness = runif(length(unique(icews_10$i)), 0, 100)
#' )
#' 
#' verbCoop_net <- add_node_vars(
#'   netlet = verbCoop_net,
#'   node_data = external_data,
#'   actor = 'i',
#'   node_vars = c('democracy_score', 'trade_openness')
#' )
#' }
#'
#'
#' @author Colin Henry, Shahryar Minhas
#'
#' @export add_node_vars
#' @aliases add_vertex_attributes

add_node_vars <- function( 
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

#' @rdname add_node_vars
#' @export
add_vertex_attributes <- add_node_vars
