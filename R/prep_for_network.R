#' Convert netify object to a statnet network object
#'
#' @param netlet An R object
#' @param add_nodal_attribs Add any nodal attributes from netlet to igraph object. Default is TRUE.
#' @param add_dyad_attribs Add any dyad attributes from netlet to igraph object. Default is TRUE.
#' @return statnet network object
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @examples
#'
#' # load data
#' data(icews)
#' 
#' # cross-sectional case
#' icews_10 <- icews[icews$year==2010,]
#'
#' # create netify object
#' dvars = c( 'matlCoop', 'verbConf', 'matlConf' )
#' nvars = c( 'i_polity2','i_log_gdp', 'i_log_pop' )
#' verbCoop_net = netify( 
#'   icews_10,
#'   actor1='i', actor2='j', 
#'   symmetric=FALSE, 
#'   weight='verbCoop',
#'   dyad_vars = dvars,
#'   dyad_vars_symmetric=rep(FALSE, length(dvars)),
#'   nodal_vars = nvars )
#' 
#' # convert to a statnet network object
#' ntwk <- prep_for_statnet(verbCoop_net)
#' ntwk
#' 
#' # longitudinal case
#' verbCoop_longit_net = netify(
#'   icews,
#'   actor1='i', actor2='j', time='year',
#'   symmetric=FALSE, 
#'   weight='verbCoop',
#'   dyad_vars = dvars,
#'   dyad_vars_symmetric=rep(FALSE, length(dvars)),
#'   nodal_vars = nvars )
#'
#' # convert to a statnet network object
#' ntwk_longit <- prep_for_statnet(verbCoop_longit_net)
#' 
#' # output in the longitudinal case is 
#' # a list of statnet network objects
#' class(ntwk_longit)
#' names(ntwk_longit)
#' ntwk_longit[['2002']]
#'
#' @export prep_for_statnet

prep_for_statnet = function(
  netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE){

  # check if netify object
  netify_check(netlet)

# if more than one layer tell user they must specify a single layer
	if(length(attributes(netlet)$layers) > 1){
		cli::cli_alert_danger(
			'Error: This object has multiple layers. 
      `prep_for_statnet` does not currently support multilayer `netify` inputs. 
      Please use the `subset_netlet` function to create a `netify` object with a single layer.' )
		stop() }
	
  # assert dependencies for remapping data to network
  assert_dependency("network")

  ## three cases: cross-sec/matrix, longit list, longit array
  netlet_type <- attr(netlet, 'netify_type')

  # if type array convert to list since network
  # doesnt support arrays anyhow
  if(netlet_type == 'longit_array'){
    netlet <- array_to_list(netlet) }

  # check other attributes
  nodal_data_exist = !is.null( 
    attr(netlet, 'nodal_data')[[1]] )
  dyad_data_exist = !is.null( 
    attr(netlet, 'dyad_data')[[1]] )

  ## cross-sec case
  if(netlet_type == 'cross_sec'){

    # convert to a statnet network object
    ntwk <- netify_to_statnet(netlet)

    # process nodal attributes if exist
    if(nodal_data_exist & add_nodal_attribs){
      ntwk <- add_nodal_to_statnet(
        netlet, attr(netlet, 'nodal_data'), ntwk) }

    # process dyadic attributes if exist
    if(dyad_data_exist & add_dyad_attribs) {
      ntwk <- add_dyad_to_statnet(
        netlet, attr(netlet, 'dyad_data'), ntwk) }
  } # done with cross-sec case

  ## longit case
  if(netlet_type %in% c('longit_array','longit_list')){
 
    # iterate through netlet
    ntwk <- lapply(1:length(netlet), function(ii){

      # get netlet slice
      netlet_slice <- netlet[[ii]]

      # get time listing
      time_val = names(netlet)[ii]

      # convert to a statnet network object
      ntwk_slice <- netify_to_statnet(netlet_slice)

      # process nodal attributes if exist
      if(nodal_data_exist & add_nodal_attribs){
        ntwk_slice <- add_nodal_to_statnet(
          netlet_slice, attr(netlet, 'nodal_data'), 
          ntwk_slice, time_val) }

      # process dyadic attributes if exist
      if(dyad_data_exist & add_dyad_attribs) {
        ntwk_slice <- add_dyad_to_statnet(
          netlet_slice, attr(netlet, 'dyad_data'),
          ntwk_slice, time_val) }

      #
      return(ntwk_slice) })
    names(ntwk) = names(netlet)
  } # done with longit case

  #
  return(ntwk) }

#' netify_to_statnet
#' 
#' Convert netify object to a statnet network object
#' 
#' @param netlet netify object
#' @return statnet network object
#' @author Shahryar Minhas

netify_to_statnet <- function(netlet){

  # check if bipartite
  bipartite_logical <- ifelse(attr(netlet, 'mode') == 'bipartite', TRUE, FALSE)  

  # if weight is NULL then create logical to set value for ignore.eval
  if( is.null(attr(netlet, 'weight')) ){
    ignore_eval <- TRUE } else { ignore_eval <- FALSE }

  # convert to a statnet network object
  statnet_object <- network::network(
    get_raw(netlet), 
    matrix.type = 'adjacency', 
    directed = !attr(netlet, 'symmetric'),
    loops = !attr(netlet, 'diag_to_NA'),
    bipartite = bipartite_logical,
    names.eval=attr(netlet, 'weight'),
    ignore.eval=ignore_eval
    )

  # set as network attribute as well if weight provided
  if( !is.null(attr(netlet, 'weight')) ){
    network::set.network.attribute(
      statnet_object, attr(netlet, 'weight'), 
      get_raw(netlet) ) }

  #
  return(statnet_object) }

#' add_nodal_to_statnet
#' 
#' Add nodal attributes to a network object from netify object
#' 
#' @param netlet netify object
#' @param node_data node data from netlet object
#' @param statnet_object network object to modify
#' @param time time indicator for longit case
#' @return statnet network object with nodal attributes added
#' @author Shahryar Minhas

add_nodal_to_statnet <- function(
  netlet, node_data, statnet_object, time=NULL){

  # slice by time if relevant
  if(!is.null(time)){
    node_data = node_data[node_data[,2] == time,] }

  # loop through and add to a statnet network object
  node_var_start <- ifelse(is.null(time), 2, 3)
  for( ii in node_var_start:ncol(node_data) ){
    network::set.vertex.attribute(
      statnet_object, names(node_data)[ii], node_data[,ii] ) }

  #
  return(statnet_object) }

#' add_dyad_to_statnet
#' 
#' Add dyad attributes to a network object from netify object
#' 
#' @param netlet netify object
#' @param dyad_data_list dyad data from netlet object
#' @param statnet_object network object to modify
#' @param time time indicator for longit case
#' @return statnet network object with dyad attributes added
#' @author Shahryar Minhas

add_dyad_to_statnet <- function(
  netlet, dyad_data_list, statnet_object, time=NULL){

  # get dyadic array
  if(is.null(time)){
    dyad_data <- dyad_data_list[[1]]
  } else { dyad_data <- dyad_data_list[[time]]  }

  # get var names
  vars <- dimnames(dyad_data)[[3]]

  # iterate through dyadic vars and 
  # add into network object
  for( ii in 1:length(vars) ){

    # generate matrix form of dyad data var
    dData = dyad_data[,,ii] 

    # replace diagonal with 0s except if
    # netlet is bipartite or 
    # diag_to_NA is set to FALSE
    bipartite_logical <- ifelse(attr(netlet, 'mode') == 'bipartite', TRUE, FALSE)
    if( !bipartite_logical & attr(netlet, 'diag_to_NA')){
      diag(dData) = 0 }

    # set as edge attrib if not bipartite, weird sizing issue
    # when trying to add edge attribute to bipartite network
    # that is not clear to me
    if(!bipartite_logical){
      network::set.edge.value(
        statnet_object, paste0(vars[ii], '_e'), dData )
    }

    # set as network attrib
    network::set.network.attribute(
      statnet_object, vars[ii], dData ) }

  #
  return(statnet_object) }
