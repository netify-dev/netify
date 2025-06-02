#' Convert netify object to igraph object
#'
#' @aliases convert.to.igraph
#' @param netlet An R object
#' @param add_nodal_attribs Add any nodal attributes from netlet to igraph object. Default is TRUE.
#' @param add_dyad_attribs Add any dyad attributes from netlet to igraph object. Default is TRUE.
#' @return igraph object
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @keywords netify
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
#' # convert to igraph object
#' igrph <- prep_for_igraph(verbCoop_net)
#' igrph
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
#' # convert to igraph object
#' igrph_longit <- prep_for_igraph(verbCoop_longit_net)
#' 
#' # output in the longitudinal case is 
#' # a list of igraph objects
#' class(igrph_longit)
#' names(igrph_longit)
#' igrph_longit[['2002']]
#' 
#' @export prep_for_igraph
 
prep_for_igraph = function(
  netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE){

  # check if netify object
  netify_check(netlet)

# if more than one layer tell user they must specify a single layer
	if(length(attributes(netlet)$layers) > 1){
		cli::cli_alert_danger(
			'Error: This object has multiple layers. 
      `prep_for_igraph` does not currently support multilayer `netify` inputs. 
      Please use the `subset_netlet` function to create a `netify` object with a single layer.' )
		stop() }

  # assert dependencies for remapping data to igraph
  assert_dependency("igraph")

  ## three cases: cross-sec/matrix, longit list, longit array
  netlet_type = attr(netlet, 'netify_type')

  # if type array convert to list since igraph
  # doesnt support arrays anyhow
  if(netlet_type == 'longit_array'){
    netlet <- array_to_list(netlet) }

  # check other attributes
  nodal_data_exist = !is.null( 
    attr(netlet, 'nodal_data')[[1]] )
  dyad_data_exist = !is.null( 
    attr(netlet, 'dyad_data')[[1]] )

  # cross-sec case
  if(netlet_type == 'cross_sec'){

    # convert to igraph object
    igrph <- netify_to_igraph(netlet)

    # process nodal attributes if exist
    if(nodal_data_exist){
      igrph <- add_nodal_to_igraph(
        netlet, attr(netlet, 'nodal_data'), igrph) }

    # process dyadic attributes if exist
    if(dyad_data_exist) {
      igrph <- add_dyad_to_igraph(
        netlet, attr(netlet, 'dyad_data'), igrph) }
  } # done with cross-sec case    

  ## longit case
  if(netlet_type %in% c('longit_array','longit_list')){
 
    # iterate through netlet
    igrph <- lapply(1:length(netlet), function(ii){

      # get netlet slice
      netlet_slice <- netlet[[ii]]

      # get time listing
      time_val <- names(netlet)[ii]

      # convert to network object
      igrph_slice <- netify_to_igraph(netlet_slice)

      # process nodal attributes if exist
      if(nodal_data_exist & add_nodal_attribs){
        igrph_slice <- add_nodal_to_igraph(
          netlet_slice, attr(netlet, 'nodal_data'),
          igrph_slice, time_val) }

      # process dyadic attributes if exist
      if(dyad_data_exist & add_dyad_attribs) {
        igrph_slice <- add_dyad_to_igraph(
          netlet_slice, attr(netlet, 'dyad_data'),
          igrph_slice, time_val) }

      #
      return(igrph_slice) })
    names(igrph) = names(netlet)
  } # done with longit case

  #
  return(igrph) }

#' netify_to_igraph
#' 
#' Convert netify object to igraph object
#' 
#' @param netlet netify object
#' @return igraph object
#' @author Shahryar Minhas

netify_to_igraph <- function(netlet){

  # check if bipartite
  bipartite_logical <- ifelse(attr(netlet, 'mode') == 'bipartite', TRUE, FALSE)

  # weight logical
  if( !is.null(attr(netlet, 'weight')) ){
    weight_logical <- TRUE } else { weight_logical <- NULL }

  # strip netify attribs away
  raw_net <- get_raw(netlet)

  # convert to igraph_object
  if(!bipartite_logical){
    igraph_object <- igraph::graph_from_adjacency_matrix(
      raw_net, 
      mode = ifelse(attr(netlet, 'symmetric'), 'undirected', 'directed'),
      weighted = weight_logical,
      diag = !attr(netlet, 'diag_to_NA') ) }

  # bipartite case
  if(bipartite_logical){
    igraph_object <- igraph::graph_from_biadjacency_matrix(
      raw_net,
      directed = FALSE,
      weighted = weight_logical ) }

  # add dv as edge attribute as well
  if( !is.null(attr(netlet, 'weight')) ){

    # match edge positions between raw data and igraph
    ePosIgraph <- adj_igraph_positions(raw_net, igraph_object)

    # subset dyadic data matrix based on ids
    # and add edge attr
    igraph_object = igraph::set_edge_attr(
      igraph_object, 
      name=attr(netlet, 'weight'), 
      value= raw_net[ ePosIgraph ] ) }

  # 
  return(igraph_object) }

#' add_nodal_to_igraph
#' 
#' Add nodal attributes to an igraph object from netify object
#' 
#' @param netlet netify object
#' @param node_data node data from netlet object
#' @param igraph_object igraph object to modify
#' @param time time indicator for longit case
#' @return igraph object with nodal attributes added
#' @author Shahryar Minhas

add_nodal_to_igraph <- function(
  netlet, node_data, igraph_object, time=NULL){

  # slice by time if relevant
  if(!is.null(time)){

    # # if ego network, then time variable includes ego name
    # # modify to pull out year only since that' the only 
    # # thing that will match with the name of dyad_data_list
    # ego_netlet = attr(netlet, 'ego_netlet')
    # if(!is.null(ego_netlet)){
    #   if(ego_netlet){
    #     time = strsplit(time, '__')[[1]][2] } }

    #
    node_data <- node_data[node_data[,2] == time,]
  }

  # make sure order of nodes is the same
  igrph_nodes <- names(igraph::V(igraph_object))
  node_data <- node_data[
    match(igrph_nodes, node_data[,1]),]

  # loop through and add to network object
  node_var_start <- ifelse(is.null(time), 2, 3)
  for( ii in node_var_start:ncol(node_data) ){
    igraph_object = igraph::set_vertex_attr(
      igraph_object, 
      name=names(node_data)[ii], 
      value=node_data[,ii]) }      

  #
  return(igraph_object) }

#' add_dyad_to_igraph
#' 
#' Add dyad attributes to an igraph object from netify object
#' 
#' @param netlet netify object
#' @param dyad_data_list dyad data from netlet object
#' @param igraph_object igraph object to modify
#' @param time time indicator for longit case
#' @return igraph object with dyad attributes added
#' @author Shahryar Minhas

add_dyad_to_igraph <- function(
  netlet, dyad_data_list, igraph_object, time=NULL){

  # get dyadic array
  if(is.null(time)){
    dyad_data <- dyad_data_list[[1]]
  } else {

    # # if ego network, then time variable includes ego name
    # # modify to pull out year only since that' the only 
    # # thing that will match with the name of dyad_data_list
    # ego_netlet = attr(netlet, 'ego_netlet')
    # if(!is.null(ego_netlet)){
    #   if(ego_netlet){
    #     time = strsplit(time, '__')[[1]][2] } }

    # subset
    dyad_data <- dyad_data_list[[time]]
  }

  # get var names
  vars <- dimnames(dyad_data)[[3]]

  # iterate through dyadic vars and 
  # add into igrpah object
  for( ii in 1:length(vars) ){

    # generate matrix form of dyad data var
    dData = dyad_data[,,ii] 

    # replace diagonal with 0s
    bipartite_logical <- ifelse(attr(netlet, 'mode') == 'bipartite', TRUE, FALSE)
    if( !bipartite_logical & attr(netlet, 'diag_to_NA')){
      diag(dData) = 0 }

    # generate matrix that tells us where
    # in the rows (and presumably cols) an
    # actor falls
    ePosIgraph <- adj_igraph_positions(dData, igraph_object)

    # subset dyadic data matrix based on ids
    # and add edge attr
    igraph_object = igraph::set_edge_attr(
      igraph_object, name=vars[ii], value= dData[ ePosIgraph ] )
  }
  #
  return(igraph_object) }

#' adj_igraph_positions
#' 
#' Match igraph edge order with matrix data for the 
#' purpose of setting edge attributes
#' 
#' @param adj_mat adjacency matrix
#' @param igraph_object igraph object to modify
#' @return matrix object of how actor positions match
#' between the adj_mat and igraph_object
#' @author Shahryar Minhas

adj_igraph_positions <- function(adj_mat, igraph_object){

  # get row and column information from adj_mat
  ar <- rownames(adj_mat) ; nr <- length(ar)
  ac <- colnames(adj_mat) ; nc <- length(ac)
  arKey <- data.frame(
    id = 1:nr, lab=ar, stringsAsFactors=FALSE )
  acKey <- data.frame(
    id = 1:nc, lab=ac, stringsAsFactors=FALSE )

  # # get out edge ids from igraph, igraph
  # # wants you to merge in ids based on the
  # # order in which they show up
  # eVecIgraph = attributes(igraph::E(igraph_object))$vnames

  # # organize into a matrix object
  # eLabIgraph = do.call('rbind', strsplit(eVecIgraph, '|', fixed=TRUE))

  # can simplify the commented above via igraph::as_data_frame
  eLabIgraph = igraph::as_data_frame(igraph_object, what='edges')

  # get positions of each actor in eLabIgraph
  # based on where they fall in aKey
  ePosIgraph = cbind(
    row=arKey$id[match(eLabIgraph[,1], arKey$lab)],
    col=acKey$id[match(eLabIgraph[,2], acKey$lab)] )

  #
  return(ePosIgraph)
}
