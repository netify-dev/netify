#' Convert netify objects to igraph format
#'
#' Transforms netify network objects into igraph objects (also available as `netify_to_igraph`), 
#' preserving network structure and optionally including nodal and dyadic attributes as vertex 
#' and edge attributes.
#'
#' @param netlet A netify object containing network data. Currently supports 
#'   single-layer networks only. For multilayer networks, use 
#'   \code{\link{subset_netify}} to extract individual layers first.
#' @param add_nodal_attribs Logical. If TRUE (default), includes nodal attributes 
#'   from the netify object as vertex attributes in the igraph object. Set to 
#'   FALSE to create a network with structure only.
#' @param add_dyad_attribs Logical. If TRUE (default), includes dyadic attributes 
#'   from the netify object as edge attributes in the igraph object. Set to 
#'   FALSE to exclude edge covariates.
#'
#' @return An igraph object or list of igraph objects:
#'   \describe{
#'     \item{Cross-sectional networks}{Returns a single igraph object}
#'     \item{Longitudinal networks}{Returns a named list of igraph objects, 
#'       with names corresponding to time periods}
#'   }
#'   
#'   The resulting igraph object(s) will have:
#'   \itemize{
#'     \item Vertices named according to actors in the netify object
#'     \item Edge weights from the netify weight variable (if present)
#'     \item Vertex attributes for each nodal variable (if add_nodal_attribs = TRUE)
#'     \item Edge attributes for each dyadic variable (if add_dyad_attribs = TRUE)
#'   }
#'
#' @details
#' The conversion process handles different netify structures:
#' \itemize{
#'   \item \strong{Cross-sectional}: Direct conversion to a single igraph object
#'   \item \strong{Longitudinal arrays}: Internally converted to list format, then 
#'     each time slice becomes a separate igraph object
#'   \item \strong{Longitudinal lists}: Each time period converted to separate igraph object
#' }
#' 
#' For directed networks, the resulting igraph object will be directed. For 
#' undirected networks, the igraph object will be undirected. Edge weights in 
#' the netify object become edge weights in igraph.
#' 
#' When longitudinal data includes actors that appear or disappear over time, 
#' each time period's igraph object will contain only the actors present in 
#' that period.
#'
#' @note 
#' This function requires the igraph package to be installed. 
#'
#' @examples
#' # Load example data
#' data(icews)
#' 
#' # Example 1: Cross-sectional network with attributes
#' icews_10 <- icews[icews$year == 2010,]
#'
#' # Create netify object with attributes
#' dvars <- c('matlCoop', 'verbConf', 'matlConf')
#' nvars <- c('i_polity2','i_log_gdp', 'i_log_pop')
#' 
#' verbCoop_net <- netify( 
#'   icews_10,
#'   actor1 = 'i', actor2 = 'j', 
#'   symmetric = FALSE, 
#'   weight = 'verbCoop',
#'   dyad_vars = dvars,
#'   dyad_vars_symmetric = rep(FALSE, length(dvars)),
#'   nodal_vars = nvars
#' )
#' 
#' # Convert to igraph
#' ig <- netify_to_igraph(verbCoop_net)
#' 
#' # Examine the result
#' ig
#' igraph::vcount(ig)  # number of vertices
#' igraph::ecount(ig)  # number of edges
#' igraph::vertex_attr_names(ig)  # nodal attributes
#' igraph::edge_attr_names(ig)    # edge attributes
#' 
#' # Access specific attributes
#' igraph::V(ig)$i_polity2  # polity scores
#' igraph::E(ig)$matlCoop   # material cooperation
#' 
#' # Example 2: Longitudinal network
#' verbCoop_longit <- netify(
#'   icews,
#'   actor1 = 'i', actor2 = 'j', time = 'year',
#'   symmetric = FALSE, 
#'   weight = 'verbCoop',
#'   dyad_vars = dvars,
#'   dyad_vars_symmetric = rep(FALSE, length(dvars)),
#'   nodal_vars = nvars
#' )
#'
#' # Convert to list of igraph objects
#' ig_list <- netify_to_igraph(verbCoop_longit)
#' 
#' # Examine structure
#' length(ig_list)       # number of time periods
#' names(ig_list)        # time period labels
#' 
#' # Access specific time period
#' ig_2002 <- ig_list[['2002']]
#' ig_2002
#' 
#' # Example 3: Convert without attributes
#' ig_structure_only <- netify_to_igraph(
#'   verbCoop_net, 
#'   add_nodal_attribs = FALSE,
#'   add_dyad_attribs = FALSE
#' )
#' 
#' # Only network structure, no attributes
#' igraph::vertex_attr_names(ig_structure_only)  # only "name"
#' igraph::edge_attr_names(ig_structure_only)    # only "weight" (if present)
#'
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#' 
#' @export netify_to_igraph
#' @aliases to_igraph
 
netify_to_igraph = function(
  netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE
  ){

  # check if netify object
  netify_check(netlet)

# if more than one layer tell user they must specify a single layer
	if(length(attributes(netlet)$layers) > 1){
		cli::cli_alert_danger(
			'Error: This object has multiple layers. 
      `netify_to_igraph` does not currently support multilayer `netify` inputs. 
      Please use the `subset_netify` function to create a `netify` object with a single layer.' )
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
    igrph <- netify_net_to_igraph(netlet)

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
    
    # cache attributes once
    time_vals <- names(netlet)
    nodal_data_attr <- attr(netlet, 'nodal_data')
    dyad_data_attr <- attr(netlet, 'dyad_data')
    
    # if no attributes to add, simplify processing
    if ((!nodal_data_exist || !add_nodal_attribs) && 
        (!dyad_data_exist || !add_dyad_attribs)) {
      # just convert to igraph without attributes
      igrph <- lapply(netlet, netify_net_to_igraph)
      names(igrph) <- time_vals
    } else {
      # pre-process nodal data by time if needed
      nodal_data_by_time <- NULL
      if (nodal_data_exist && add_nodal_attribs && !is.null(nodal_data_attr)) {
        # create time-indexed lookup for faster access
        time_col <- nodal_data_attr[, 2]
        nodal_data_by_time <- split(nodal_data_attr, time_col)
      }
      
      # process all time periods
      igrph <- lapply(seq_along(netlet), function(ii){
        netlet_slice <- netlet[[ii]]
        time_val <- time_vals[ii]
        
        # convert to igraph
        igrph_slice <- netify_net_to_igraph(netlet_slice)
        
        # add nodal attributes if needed
        if (nodal_data_exist && add_nodal_attribs && !is.null(nodal_data_by_time[[time_val]])) {
          node_data_t <- nodal_data_by_time[[time_val]]
          
          # match order efficiently
          igrph_nodes <- names(igraph::V(igrph_slice))
          node_data_t <- node_data_t[match(igrph_nodes, node_data_t[,1]), ]
          
          # vectorized attribute setting
          node_var_start <- 3  # skip actor and time columns
          if (ncol(node_data_t) >= node_var_start) {
            for (col_idx in node_var_start:ncol(node_data_t)) {
              igrph_slice <- igraph::set_vertex_attr(
                igrph_slice,
                name = names(node_data_t)[col_idx],
                value = node_data_t[, col_idx]
              )
            }
          }
        }
        
        # add dyadic attributes if needed
        if (dyad_data_exist && add_dyad_attribs) {
          igrph_slice <- add_dyad_to_igraph(
            netlet_slice, 
            dyad_data_attr,
            igrph_slice, 
            time_val
          )
        }
        
        return(igrph_slice)
      })
      
      names(igrph) <- time_vals
    }
  } # done with longit case

  #
  return(igrph) }
 
netify_to_igraph = function(
  netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE
  ){

  # check if netify object
  netify_check(netlet)

# if more than one layer tell user they must specify a single layer
	if(length(attributes(netlet)$layers) > 1){
		cli::cli_alert_danger(
			'Error: This object has multiple layers. 
      `netify_to_igraph` does not currently support multilayer `netify` inputs. 
      Please use the `subset_netify` function to create a `netify` object with a single layer.' )
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
    igrph <- netify_net_to_igraph(netlet)

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
      igrph_slice <- netify_net_to_igraph(netlet_slice)

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

#' @rdname netify_to_igraph
#' @export
to_igraph <- netify_to_igraph

#' netify_net_to_igraph
#' 
#' Convert netify object to igraph object
#' 
#' @param netlet netify object
#' @return igraph object
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd

netify_net_to_igraph <- function(netlet){

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
#'
#' @keywords internal
#' @noRd

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
#'
#' @keywords internal
#' @noRd

add_dyad_to_igraph <- function(netlet, dyad_data_list, igraph_object, time = NULL) {

  # get dyadic data for specified time period
  if (is.null(time)) {
    var_matrices <- dyad_data_list[[1]]
  } else {
    var_matrices <- dyad_data_list[[time]]
  }
  
  # get var names from the list of matrices
  vars <- names(var_matrices)
  if (length(vars) == 0) return(igraph_object)
  
  # get attrs
  netlet_mode <- attr(netlet, 'mode')
  netlet_diag_to_NA <- attr(netlet, 'diag_to_NA')
  bipartite_logical <- netlet_mode == 'bipartite'
  
  # get edge positions
  ePosIgraph <- adj_igraph_positions(var_matrices[[1]], igraph_object)
  
  # go through dyad vars
  edge_attrs <- lapply(vars, function(var_name) {
    dData <- var_matrices[[var_name]]
    
    # replace diagonal with 0s if needed
    if (!bipartite_logical && netlet_diag_to_NA) {
      diag(dData) <- 0
    }
    
    # extract values using pre-computed positions
    return(dData[ePosIgraph])
  })
  
  # set all edge attributes at once
  for (i in seq_along(vars)) {
    igraph_object <- igraph::set_edge_attr(
      igraph_object, 
      name = vars[i], 
      value = edge_attrs[[i]]
    )
  }
  
  #
  return(igraph_object)
}

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
#'
#' @keywords internal
#' @noRd

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
