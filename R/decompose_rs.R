#' Decompose a netify object into edges and nodal data frames
#'
#' `decompose_netlet` (also available as `decompose_netify` and 
#' `netify_to_base`, `netlet_to_base`) separates a 
#' netify object into its constituent parts:
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
#' @aliases decompose_netify netify_to_base netlet_to_base
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

#' @rdname decompose_netlet
#' @export
decompose_netify <- decompose_netlet

#' @rdname decompose_netlet
#' @export
netify_to_base <- decompose_netlet

#' @rdname decompose_netlet
#' @export
netlet_to_base <- decompose_netlet

#' Break down an `igraph` object into base R components
#'
#' `decompose_igraph` (also available as `igraph_to_base`) processes an 
#' igraph object to extract its adjacency matrix along with any 
#' available vertex and edge attributes, returning them in a standardized list format.
#'
#' @param grph An igraph object.
#' @param weight An optional character string specifying the edge attribute to use as weights. Defaults to NULL.
#'
#' @return A list containing:
#' \describe{
#'   \item{adj_mat}{The adjacency matrix extracted from the igraph object.}
#'   \item{ndata}{A data frame of vertex attributes, or NULL if not available.}
#'   \item{ddata}{A data frame of edge attributes, or NULL if not available.}
#'   \item{weight}{The edge attribute name used, if provided.}
#' }
#'
#' @details
#' If the graph does not have vertex names, default names are assigned:
#' \itemize{
#'   \item For bipartite graphs: rows are labeled as "r1", "r2", etc., and columns as "c1", "c2", etc.
#'   \item For unipartite graphs: all vertices are labeled as "a1", "a2", etc.
#' }
#' Note: For longitudinal networks with changing actor composition, it is recommended 
#' to explicitly name vertices before conversion to ensure consistent actor identification across time periods.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export decompose_igraph
#' @aliases igraph_to_base

decompose_igraph <- function(grph, weight = NULL) {
    stopifnot(inherits(grph, "igraph"))

    # check if bipartite - but also verify the type attribute is actually logical
    is_bipartite <- FALSE
    if (igraph::is_bipartite(grph)) {
        vertex_types <- igraph::V(grph)$type
        
        # only treat as bipartite if type attribute exists and is logical
        if (!is.null(vertex_types) && is.logical(vertex_types)) {
            is_bipartite <- TRUE
        } else {
            # graph thinks it's bipartite but type attribute is invalid
            # treat as unipartite and warn the user
            cli::cli_warn(c(
                "!" = "Graph has a {.field type} vertex attribute but it's not logical.",
                "i" = "Treating graph as unipartite.",
                ">" = "For bipartite graphs, use logical values: {.code V(g)$type <- c(TRUE, FALSE, ...)}"
            ))
        }
    }
    
    # get existing vertex names if any
    vertex_names <- igraph::V(grph)$name
    
    if (is_bipartite) {
        # now we know vertex_types is logical
        n_type1 <- sum(!vertex_types)
        n_type2 <- sum(vertex_types)
        
        # for bipartite graphs, get the bipartite adjacency matrix
        # this returns a matrix of dimension n_type1 x n_type2
        adj_mat <- igraph::as_biadjacency_matrix(
            grph, 
            attr = weight, 
            sparse = FALSE
        )
        
        # create vertex names if they don't exist
        if (is.null(vertex_names)) {
            vertex_names <- character(length(vertex_types))
            vertex_names[!vertex_types] <- paste0("r", seq_len(n_type1))
            vertex_names[vertex_types] <- paste0("c", seq_len(n_type2))
            
            # set row/column names for the bipartite adjacency matrix
            rownames(adj_mat) <- paste0("r", seq_len(n_type1))
            colnames(adj_mat) <- paste0("c", seq_len(n_type2))
        } else {
            # use existing names
            rownames(adj_mat) <- vertex_names[!vertex_types]
            colnames(adj_mat) <- vertex_names[vertex_types]
        }
    } else {
        # for unipartite graphs, get the regular adjacency matrix
        adj_mat <- igraph::as_adjacency_matrix(
            grph, 
            attr = weight, 
            sparse = FALSE
        )
        
        # create vertex names if they don't exist
        if (is.null(vertex_names)) {
            n_vertices <- igraph::vcount(grph)
            vertex_names <- paste0("a", seq_len(n_vertices))
            rownames(adj_mat) <- vertex_names
            colnames(adj_mat) <- vertex_names
        } else {
            # use existing names
            rownames(adj_mat) <- vertex_names
            colnames(adj_mat) <- vertex_names
        }
    }

    # pull out vertex attributes as a data.frame, if any
    v_labs <- igraph::vertex_attr_names(grph)
    if (length(v_labs) > 0) {
        ndata <- igraph::as_data_frame(grph, what = "vertices")
        # always add vertex names as 'actor' column
        ndata$actor <- vertex_names
    } else {
        ndata <- NULL
    }

    # pull out edge attributes as a data.frame, if any
    e_labs <- igraph::edge_attr_names(grph)
    if (length(e_labs) > 0) {
        ddata <- igraph::as_data_frame(grph, what = "edges")
        # update from/to to use our naming scheme if they're numeric
        if (is.numeric(ddata$from) && is.numeric(ddata$to)) {
            ddata$from <- vertex_names[ddata$from]
            ddata$to <- vertex_names[ddata$to]
        }
    } else {
        ddata <- NULL
    }

    # return the decomposed components
    list(
        adj_mat = adj_mat,
        ndata = ndata,
        ddata = ddata,
        weight = weight
    )
}

#' @rdname decompose_igraph
#' @export
igraph_to_base <- decompose_igraph

#' Break down a `network` object into base R components
#'
#' `decompose_statnet` (also available as `decompose_network`, 
#' `network_to_base`, and `statnet_to_base`) processes an object 
#' from the `network` package to extract its adjacency matrix along with any available vertex and edge attributes, and returns them in a standardized list format.
#'
#' @param ntwk A `network` object.
#' @param weight An optional character string specifying the edge attribute to use as weights. Defaults to NULL.
#'
#' @return A list containing:
#' \describe{
#'   \item{adj_mat}{The adjacency matrix extracted from the network object.}
#'   \item{ndata}{A data frame of vertex attributes, or NULL if not available.}
#'   \item{ddata}{A data frame of edge attributes combined with vertex names, or NULL if not available.}
#'   \item{weight}{The edge attribute name used, if provided.}
#' }
#'
#' @details
#' If the network does not have vertex names, default names are assigned:
#' \itemize{
#'   \item For bipartite networks: rows are labeled as "r1", "r2", etc., and columns as "c1", "c2", etc.
#'   \item For unipartite networks: all vertices are labeled as "a1", "a2", etc.
#' }
#' Note: For longitudinal networks with changing actor composition, it is recommended 
#' to explicitly name vertices before conversion to ensure consistent actor identification across time periods.
#' 
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export decompose_statnet
#' @aliases decompose_network network_to_base statnet_to_base

decompose_statnet <- function(ntwk, weight = NULL) {
    stopifnot(inherits(ntwk, "network"))

    # check if bipartite
    is_bipartite <- network::is.bipartite(ntwk)
    
    # get or create vertex names
    vertex_names <- network::get.vertex.attribute(ntwk, "vertex.names")

    # check if vertex names are just the default numeric sequence
    # if so, treat as if they don't exist
    if (!is.null(vertex_names) && 
        is.numeric(vertex_names) && 
        identical(vertex_names, seq_len(network::network.size(ntwk)))) {
        vertex_names <- NULL
    }

    if (is_bipartite) {
        # get bipartite partition info
        bip_partition <- network::get.network.attribute(ntwk, "bipartite")
        n_type1 <- bip_partition
        n_type2 <- network::network.size(ntwk) - bip_partition
        
        # create vertex names if they don't exist
        if (is.null(vertex_names)) {
            vertex_names <- c(
                paste0("r", seq_len(n_type1)), 
                paste0("c", seq_len(n_type2))
            )
        }
        
        # get the full adjacency matrix first
        adj_mat <- network::as.matrix.network.adjacency(
            ntwk, 
            attrname = weight
        )
        
        # set row/column names
        rownames(adj_mat) <- vertex_names[1:n_type1]
        colnames(adj_mat) <- vertex_names[(n_type1 + 1):(n_type1 + n_type2)]
        
    } else {
        # for unipartite networks, get the regular adjacency matrix
        adj_mat <- network::as.matrix.network.adjacency(
            ntwk, 
            attrname = weight
        )
        
        # create vertex names if they don't exist
        if (is.null(vertex_names)) {
            n_vertices <- network::network.size(ntwk)
            vertex_names <- paste0("a", seq_len(n_vertices))
        }
        
        # set row/column names
        rownames(adj_mat) <- vertex_names
        colnames(adj_mat) <- vertex_names
    }

    # vertex attributes
    v_labs <- network::list.vertex.attributes(ntwk)
    # don't include system attributes as real vertex attributes
    system_attrs <- c("vertex.names", "na")
    v_labs <- setdiff(v_labs, system_attrs)
    
    if (length(v_labs) > 0) {
        # use lapply for better performance than sapply
        ndata_list <- lapply(v_labs, function(attr) {
            network::get.vertex.attribute(ntwk, attr)
        })
        names(ndata_list) <- v_labs
        ndata <- as.data.frame(ndata_list, stringsAsFactors = FALSE)
        
        # always add vertex names as 'actor' column
        ndata$actor <- vertex_names
    } else {
        ndata <- NULL
    }

    # edge attributes
    e_labs <- network::list.edge.attributes(ntwk)
    if (length(e_labs) > 0) {
        edgelist <- network::as.edgelist(ntwk)
        
        # use lapply for better performance
        attr_list <- lapply(e_labs, function(attr) {
            network::get.edge.attribute(ntwk, attr)
        })
        names(attr_list) <- e_labs
        attr_df <- as.data.frame(attr_list, stringsAsFactors = FALSE)
        
        ddata <- data.frame(
            from = vertex_names[edgelist[, 1]],
            to = vertex_names[edgelist[, 2]],
            attr_df,
            stringsAsFactors = FALSE
        )
    } else {
        ddata <- NULL
    }

    # return the decomposed components
    list(
        adj_mat = adj_mat,
        ndata = ndata,
        ddata = ddata,
        weight = weight
    )
}

#' @rdname decompose_statnet
#' @export
decompose_network <- decompose_statnet

#' @rdname decompose_statnet
#' @export
network_to_base <- decompose_statnet

#' @rdname decompose_statnet
#' @export
statnet_to_base <- decompose_statnet