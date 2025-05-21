#' unnetify
#'
#' This function converts a netify object and
#' any associated attributes into a dyadic level
#' data.frame
#' 
#' @param netlet a netify object
#' 
#' @param remove_zeros Logical. If TRUE, remove edges with zero values.
#' 
#' @return a data.frame object
#' 
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @export unnetify
#' 

unnetify <- function(netlet, remove_zeros=FALSE){

	######################
    # check if netify object
    netify_check(netlet)  

    # get attributes
    obj_attrs = attributes(netlet)
	######################

	######################
    # get to df format
    net_dfs = decompose_netlet( netlet, remove_zeros=remove_zeros )
    edge_data = net_dfs$edge_data
    nodal_data = net_dfs$nodal_data
    rm(net_dfs)

    # create id vars
    edge_data$from_id = with(edge_data, paste(from, time, sep='_'))
    edge_data$to_id = with(edge_data, paste(to, time, sep='_'))
    nodal_data$id = with(nodal_data, paste(name, time, sep='_'))
	######################

	######################
    # merge nodal and dyad 
    nodal_vars = names(nodal_data)

    # remove id vars
    nodal_vars = setdiff(
        nodal_vars, 
        c('name', 'time', 'id') )

    # iterate through nodal_vars
    # and merge into edge_data
    for(nv in nodal_vars){

        # if not symmetric
        if(!obj_attrs$symmetric){

            # add from rel
            edge_data$tmp = nodal_data[match(edge_data$from_id, nodal_data$id),nv]
            names(edge_data)[ncol(edge_data)] = paste(nv, 'from', sep="_")

            # add to rel
            edge_data$tmp = nodal_data[match(edge_data$to_id, nodal_data$id),nv]
            names(edge_data)[ncol(edge_data)] = paste(nv, 'to', sep="_")
        }

        # if symmetric
        if(obj_attrs$symmetric){

            # add from rel
            edge_data$tmp = nodal_data[match(edge_data$from_id, nodal_data$id),nv]
            names(edge_data)[ncol(edge_data)] = paste(nv, 'dyad', sep="_")
        }
    }
	######################

	######################
    #
    return(edge_data) 
	######################        
}
