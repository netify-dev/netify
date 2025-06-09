#' Merge netlet attributes with layout information
#'
#' This function organizes `netlet` data by integrating x and y positions from `nodes_list` and
#' connection data from `edges_list` into the node and edge data structures returned by
#' `decompose_netify`. It supports both cross-sectional and longitudinal data formats.
#'
#' @param netlet A `netify` object containing the network data.
#' @param nodes_list A list of data frames (or a single data frame for cross-sectional data) containing
#'        node positions with columns `actor`, `x`, and `y`. For longitudinal data, each list element
#'        corresponds to a different time slice.
#' @param edges_list A list of data frames (or a single data frame for cross-sectional data) detailing
#'        the connections between nodes. Each data frame should include at least the columns `from`, `to`,
#'        and for longitudinal data, `time`.
#'
#' @return A list containing two data frames: `nodal_data` and `edge_data`. Each data frame incorporates
#'         the corresponding node or edge data along with their spatial coordinates (and time indices for
#'         longitudinal data).
#' 
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

merge_layout_attribs <- function(
    netlet, nodes_list, edges_list
    ){

	# check if netify object
	netify_check(netlet)	

	# pull out attrs
	obj_attrs <- attributes(netlet)

    # organize dv, dyadic, and nodal attributes in netlet
	net_dfs = decompose_netify( netlet ) 

	# cross_sec
	if(obj_attrs$netify_type == 'cross_sec'){

		# pull out nodes and edges from list format
		nodes = nodes_list[[1]]
		edges = edges_list[[1]]

		# in the nodal part of net_dfs add in the
		# xy pos of actors
		net_dfs$nodal_data = merge(
			net_dfs$nodal_data, nodes, 
			by.x='name', by.y='actor' )
		
		# now do the same for the edge data
		net_dfs$edge_data = merge(
			net_dfs$edge_data, edges, 
			by.x=c('from', 'to'), 
			by.y=c('from', 'to') ) }

	# longit
	if(obj_attrs$netify_type != 'cross_sec'){

		# if it is an ego longit list
		# then clean up time names since they
		# contain ego
        ego_netlet = obj_attrs$ego_netlet
        if(!is.null(ego_netlet)){
			# pull out time periods
			list_names = names(netlet)
			list_names = unlist(lapply(strsplit(
				list_names, '__'), function(x){x[2]}))
		} else { list_names = names(nodes_list) }

		# pull out nodes and edges from list format
		nodes = lapply(1:length(nodes_list), function(ii){
			nodes = nodes_list[[ii]]
			nodes$time = list_names[ii]
			return(nodes) })
		nodes = do.call('rbind', nodes)
		edges = lapply(1:length(edges_list), function(ii){
			edges = edges_list[[ii]]
			if(nrow(edges)!=0){
				edges$time = list_names[ii]	 }
			return(edges) })
		edges = do.call('rbind', edges) 

		# in the nodal part of net_dfs add in the
		# xy pos of actors
		net_dfs$nodal_data = merge(
			net_dfs$nodal_data, 
			nodes[,c("actor","time","x","y")], 
			by.x=c('name','time'), by.y=c('actor','time') )
		
		# now do the same for the edge data
		net_dfs$edge_data = merge(
			net_dfs$edge_data, edges, 
			by.x=c('from', 'to', 'time'), 
			by.y=c('from', 'to', 'time') ) }


    #
    return(net_dfs)
}
