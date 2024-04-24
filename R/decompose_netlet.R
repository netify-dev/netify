#' Decompose a netify object into edges and nodal data frames
#'
#' `decompose_netlet` separates a netify object into its constituent parts:
#' a data frame of edges and a data frame of nodal attributes. This function
#' is particularly useful for preparing network data for analyses that
#' require separate edge and node data sets.
#'
#' @param netlet A netify object to be decomposed.
#' @param remove_zeros Logical. If TRUE, remove edges with zero values.
#'
#' @return A list containing two elements: `edge_data` and `nodal_data`.
#'         `edge_data` is a data frame of edges with attributes, and
#'         `nodal_data` is a data frame containing node attributes.
#'
#' @examples
#' # load icews data
#' data(icews)
#' 
#' # choose attributes
#' nvars = c( 'i_polity2', 'i_log_gdp', 'i_log_pop' )
#' dvars = c( 'matlCoop', 'verbConf', 'matlConf' )
#'
#' # create a netify object
#' netlet = netify(
#'     dyad_data=icews, actor1='i', actor2='j',
#'     time = 'year',
#'     symmetric=FALSE, weight='verbCoop',
#'     mode='unipartite', sum_dyads=FALSE,
#'     actor_time_uniform=TRUE, actor_pds=NULL,
#'     diag_to_NA=TRUE, missing_to_zero=TRUE,
#'     nodal_vars = nvars, 
#'     dyad_vars = dvars
#' )
#' 
#' # decompose the netify object
#' decomposed = decompose_netlet( netlet )
#' 
#' lapply(decomposed, head)
#' 
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @export decompose_netlet
#' 

decompose_netlet <- function(
    netlet, remove_zeros=TRUE
){

	# check if netify object
	netify_check(netlet)	

	# pull out attrs
	obj_attrs <- attributes(netlet)

	# pull out msrmnts
	msrmnts <- netify_measurements(netlet)

    # build edge data from dv #####################
    # edge data
    edge_data = reshape2::melt( unnetify( netlet ) )
    edge_data$Var1 = char(edge_data$Var1)
    edge_data$Var2 = char(edge_data$Var2)
    edge_data = edge_data[
        edge_data$Var1 != edge_data$Var2, ]

    # remove zeros
    if(remove_zeros){
        edge_data = edge_data[edge_data$value != 0, ]
    }

    # add weight label
    if(!is.null(attr(netlet, 'weight'))){
        names(edge_data)[3] = attr(netlet, 'weight')
    } else { names(edge_data)[3] = 'net_value'}
    ######################

    # add other dyad attribs #####################
    # merge dyad attribs with dv edge data
    if( !is.null(attr(netlet, 'dyad_data')) ){

        # melt dyad data
        dyad_data = reshape2::melt( attr(netlet, 'dyad_data') )
        dyad_data = dyad_data[dyad_data$Var1 != dyad_data$Var2, ]

        # spread vars
        dyad_data = reshape2::dcast( 
            dyad_data, Var1 + Var2 + L1 ~ Var3, value.var='value' )

        # set ids based on netify_type
        if (attr(netlet, 'netify_type') == 'cross_sec') {
            merge_by_vars <- c('Var1', 'Var2')
        } else {
            merge_by_vars <- c('Var1', 'Var2', 'L1') }	

        # remove vars in dyad_data that are already in 
        # edge_data except id vars if necessary
        to_drop = setdiff(
            intersect(
                names(dyad_data), names(edge_data)),
            merge_by_vars)
        if(length(to_drop)>0){
            dyad_data = dyad_data[,-which(
                names(dyad_data) %in% to_drop)] }

        # merge to edge_data
        edge_data = merge(
            edge_data, dyad_data, by=merge_by_vars )

        # cleanup
        rm(dyad_data)
    }

    # id vars for cross-sec and longit case
    if(obj_attrs$netify_type == 'cross_sec'){
        edge_data$L1 = 1 }
    edge_id_vars = c('Var1', 'Var2', 'L1')
    
    # reorder vars
    edge_vars = c(
        edge_id_vars, 
        setdiff(names(edge_data), edge_id_vars))
    edge_data = edge_data[,edge_vars]

    # relabel id cols
    names(edge_data)[1:3] = c('from', 'to', 'time')
    ######################

    # org nodal attrib data #####################
    # other nodal data
    if( !is.null(attr(netlet, 'nodal_data'))){
        nodal_data = attr(netlet, 'nodal_data')
    } else {
        nodal_data = actor_pds_to_frame(
            obj_attrs$actor_pds
        )
    }

    # reorder vars
    node_id_vars = c('actor', 'time')
    node_vars = c(
        node_id_vars, 
        setdiff(names(nodal_data), node_id_vars))
    nodal_data = nodal_data[,node_vars]

    # relabel id cols
    names(nodal_data)[1:2] = c('name', 'time')
    
    # convert node time to char if not already
    nodal_data$time = char(nodal_data$time)
    ######################

    # 
    out = list(
        edge_data = edge_data,
        nodal_data = nodal_data)
    
    #
    return(out)

}