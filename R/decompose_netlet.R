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

	# pull out attrs - cache for reuse
	obj_attrs <- attributes(netlet)
	netify_type <- obj_attrs$netify_type

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
    if(netify_type == 'longit_array'){
        names(edge_data)[names(edge_data) == 'Var3'] <- 'L1'
    }

    # add weight label
    weight_attr <- obj_attrs$weight
    if(!is.null(weight_attr)){
        names(edge_data)[names(edge_data) == 'value'] <- weight_attr
    } else { 
        names(edge_data)[names(edge_data) == 'value'] <- 'net_value'
    }

    # if ego netlet then we need to rename time column
    # since right now it is a concatenation of the ego
    # and the time point
    if(netify_type != 'cross_sec'){
        ego_netlet = obj_attrs$ego_netlet
        if(!is.null(ego_netlet) && ego_netlet){
            edge_data$L1 = vapply(strsplit(edge_data$L1, '__'), 
                                  function(x) x[2], character(1))
        }
    }
    ######################

    # add other dyad attribs #####################
    # merge dyad attribs with dv edge data
    dyad_data_attr <- obj_attrs$dyad_data
    if( !is.null(dyad_data_attr) ){

        # Convert new dyad_data structure to old format for melting
        # New structure: list(time_periods) -> list(variables) -> matrix
        # Convert to: list(time_periods) -> array(n_rows, n_cols, n_vars)
        time_periods <- names(dyad_data_attr)
        dyad_data_old_format <- vector("list", length(time_periods))
        names(dyad_data_old_format) <- time_periods
        
        for(time_period in time_periods) {
            var_matrices <- dyad_data_attr[[time_period]]
            
            if(length(var_matrices) > 0) {
                # Get dimensions from first matrix
                first_matrix <- var_matrices[[1]]
                n_rows <- nrow(first_matrix)
                n_cols <- ncol(first_matrix)
                n_vars <- length(var_matrices)
                var_names <- names(var_matrices)
                
                # Create array for this time period
                time_array <- array(
                    dim = c(n_rows, n_cols, n_vars),
                    dimnames = list(
                        rownames(first_matrix),
                        colnames(first_matrix),
                        var_names
                    )
                )
                
                # Fill array with data from individual matrices - optimized
                for(i in seq_along(var_names)) {
                    time_array[, , i] <- var_matrices[[i]]
                }
                
                dyad_data_old_format[[time_period]] <- time_array
            }
        }

        # melt dyad data
        dyad_data = reshape2::melt( dyad_data_old_format )
        dyad_data = dyad_data[dyad_data$Var1 != dyad_data$Var2, ]

        # spread vars
        dyad_data = reshape2::dcast( 
            dyad_data, Var1 + Var2 + L1 ~ Var3, value.var='value' )

        # set ids based on netify_type
        if (netify_type == 'cross_sec') {
            merge_by_vars <- c('Var1', 'Var2')
        } else {
            merge_by_vars <- c('Var1', 'Var2', 'L1') }	

        # remove vars in dyad_data that are already in 
        # edge_data except id vars if necessary - optimized
        dyad_names <- names(dyad_data)
        edge_names <- names(edge_data)
        to_drop = setdiff(intersect(dyad_names, edge_names), merge_by_vars)
        if(length(to_drop) > 0){
            drop_indices <- match(to_drop, dyad_names)
            dyad_data = dyad_data[, -drop_indices, drop=FALSE]
        }

        # merge to edge_data
        edge_data = merge(edge_data, dyad_data, by=merge_by_vars )

        # cleanup
        rm(dyad_data, dyad_data_old_format, time_periods, var_matrices, 
           first_matrix, time_array, dyad_names, edge_names)
    }

    # id vars for cross-sec and longit case
    if(netify_type == 'cross_sec'){ edge_data$L1 = 1 }
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
    nodal_data_attr <- obj_attrs$nodal_data
    if( !is.null(nodal_data_attr)){
        nodal_data = nodal_data_attr
    } else {
        nodal_data = actor_pds_to_frame(obj_attrs$actor_pds)
    }

    # if time variable not present in nodal data add
    nodal_names <- names(nodal_data)
    if(!'time' %in% nodal_names){ nodal_data$time = 1 }

    # reorder vars
    node_id_vars = c('actor', 'time')
    node_vars = c(
        node_id_vars, 
        setdiff(nodal_names, node_id_vars))
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
    if(netify_type == 'cross_sec'){
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