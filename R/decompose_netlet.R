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
    edge_data = reshape2::melt( get_raw( netlet ) )
    edge_data$Var1 = char(edge_data$Var1)
    edge_data$Var2 = char(edge_data$Var2)
    edge_data = edge_data[
        edge_data$Var1 != edge_data$Var2, ]

    # remove zeros
    if(remove_zeros){
        edge_data = edge_data[edge_data$value != 0, ]
    }

    # if starting with array change Var3 (time) to L1
    if(attr(netlet, 'netify_type')=='longit_array'){
        a_time_pos = which(names(edge_data)=='Var3')
        names(edge_data)[a_time_pos] = 'L1' }

    # add weight label
    value_pos = which(names(edge_data) == 'value')
    if(!is.null(attr(netlet, 'weight'))){
        names(edge_data)[value_pos] = attr(netlet, 'weight')
    } else { names(edge_data)[value_pos] = 'net_value'}

    # if ego netlet then we need to rename time column
    # since right now it is a concatenation of the ego
    # and the time point
    if(obj_attrs$netify_type != 'cross_sec'){
        ego_netlet = obj_attrs$ego_netlet
        if(!is.null(ego_netlet)){
            if(ego_netlet){
                edge_data$L1 = unlist( lapply( strsplit(
                    edge_data$L1, '__'), function(x){x[2]}))
    } } }
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
    if(obj_attrs$netify_type == 'cross_sec'){ edge_data$L1 = 1 }
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

    # if time variable not present in nodal data add
    if(!'time' %in% names(nodal_data)){ nodal_data$time = 1 }

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

    ######################
    # if cross-sectional make sure that time value
    # in edge_data is the same as time value
    # in nodal_data
    if(obj_attrs$netify_type == 'cross_sec'){
        edge_data$time = nodal_data$time[1]
    }
    ######################

    ######################
    # 
    out = list(
        edge_data = edge_data,
        nodal_data = nodal_data)
    ######################    

    ######################
    #
    return(out)
    ######################
}