#' Decompose a netify object into edge and node data frames
#'
#' `decompose_netify` (also available as `decompose`) separates a netify 
#' object into its constituent parts: a data frame of edges with attributes and 
#' a data frame of nodal attributes. 
#'
#' @param netlet A netify object (class "netify") to be decomposed.
#' @param remove_zeros Logical. If TRUE (default), edges with zero weight values 
#'   are removed from the edge data frame. If FALSE, zero-weight edges are retained.
#'
#' @return A list containing two data frames:
#'   \itemize{
#'     \item \strong{edge_data}: A data frame where each row represents an edge with columns:
#'       \itemize{
#'         \item \code{from}: Source node identifier
#'         \item \code{to}: Target node identifier  
#'         \item \code{time}: Time period (character; "1" for cross-sectional networks)
#'         \item \code{weight}: Edge weight values (using original weight variable name if specified)
#'         \item Additional columns for any dyadic variables stored in the netify object
#'       }
#'     \item \strong{nodal_data}: A data frame where each row represents a node-time combination with columns:
#'       \itemize{
#'         \item \code{name}: Node identifier
#'         \item \code{time}: Time period (character; "1" for cross-sectional networks)
#'         \item Additional columns for any nodal variables stored in the netify object
#'       }
#'   }
#'
#' @details
#' The function helpful for:
#' 
#' \strong{Edge data processing:}
#' \itemize{
#'   \item Extracts the adjacency matrix (or array for longitudinal networks) from the netify object
#'   \item Optionally removes zero-weight edges based on the remove_zeros parameter
#'   \item Merges any dyadic variables stored in the netify object
#'   \item Renames columns to standardized names (from, to, time)
#' }
#' 
#' \strong{Node data processing:}
#' \itemize{
#'   \item Extracts nodal attributes if present, or constructs from actor_pds information
#'   \item Ensures consistent time variable across node and edge data
#'   \item Renames columns to standardized names (name, time)
#' }
#' 
#' \strong{Time handling:}
#' \itemize{
#'   \item For cross-sectional networks: Sets time to "1" in both data frames
#'   \item For longitudinal networks: Preserves original time periods as character values
#'   \item For ego networks: Extracts time from ego-time concatenated identifiers
#' }
#' 
#' \strong{Variable preservation:}
#' 
#' All dyadic and nodal variables stored in the netify object are preserved in the 
#' output data frames. Dyadic variables are merged with the edge data, while nodal 
#' variables remain in the nodal data frame.
#'
#'
#' @examples
#' # Load example data
#' data(icews)
#' 
#' # Example 1: Cross-sectional network
#' icews_10 <- icews[icews$year == 2010,]
#' 
#' # Create netify object
#' net_cs <- netify(
#'   icews_10,
#'   actor1 = 'i', actor2 = 'j',
#'   symmetric = FALSE,
#'   weight = 'verbCoop'
#' )
#' 
#' # Decompose to data frames
#' decomposed_cs <- decompose_netify(net_cs)
#' 
#' # Examine structure
#' str(decomposed_cs)
#' head(decomposed_cs$edge_data)
#' head(decomposed_cs$nodal_data)
#' 
#' # Example 2: Longitudinal network with attributes
#' nvars <- c('i_polity2', 'i_log_gdp', 'i_log_pop')
#' dvars <- c('matlCoop', 'verbConf', 'matlConf')
#' 
#' net_longit <- netify(
#'   icews,
#'   actor1 = 'i', actor2 = 'j',
#'   time = 'year',
#'   symmetric = FALSE,
#'   weight = 'verbCoop',
#'   nodal_vars = nvars,
#'   dyad_vars = dvars
#' )
#' 
#' # Decompose with all attributes
#' decomposed_longit <- decompose_netify(net_longit)
#' 
#' # Check that variables are preserved
#' names(decomposed_longit$edge_data)  # Includes dyadic variables
#' names(decomposed_longit$nodal_data)  # Includes nodal variables
#' 
#' # Example 3: Keep zero-weight edges
#' decomposed_with_zeros <- decompose_netify(net_cs, remove_zeros = FALSE)
#' 
#' # Compare edge counts
#' nrow(decomposed_cs$edge_data)         # Without zeros
#' nrow(decomposed_with_zeros$edge_data)  # With zeros
#' 
#' # Example 4: Use for visualization prep
#' \dontrun{
#' # Decompose for use with ggplot2
#' plot_data <- decompose_netify(net_cs)
#' 
#' # Can now use edge_data and nodal_data separately
#' # for network visualization
#' }
#'
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @export decompose_netify

decompose_netify <- function(netlet, remove_zeros = TRUE) {
    
    # Input validation
    netify_check(netlet)
    if (!is.logical(remove_zeros) || length(remove_zeros) != 1) {
        stop("remove_zeros must be a single logical value")
    }
    
    # cache attributes for efficiency
    obj_attrs <- attributes(netlet)
    netify_type <- obj_attrs$netify_type
    weight_attr <- obj_attrs$weight
    ego_netlet <- obj_attrs$ego_netlet

    # get measurements
    msrmnts <- netify_measurements(netlet)

    # process edge data with
    edge_data <- process_edge_data(
        netlet = netlet,
        netify_type = netify_type,
        weight_attr = weight_attr,
        remove_zeros = remove_zeros,
        ego_netlet = ego_netlet
    )

    # process dyadic attributes if they exist
    if (!is.null(obj_attrs$dyad_data)) {
        edge_data <- merge_dyadic_attributes(
            edge_data = edge_data,
            dyad_data_attr = obj_attrs$dyad_data,
            netify_type = netify_type
        )
    }

    # finalize edge data structure
    edge_data <- finalize_edge_data(edge_data, netify_type)

    # process nodal data
    nodal_data <- process_nodal_data(obj_attrs, netify_type)

    # synchronize time variables
    if (netify_type == 'cross_sec' && nrow(nodal_data) > 0) {
        edge_data$time <- as.character(nodal_data$time[1])
    }

    list(
        edge_data = edge_data,
        nodal_data = nodal_data
    )
}

#' Process edge data from netify object
#' @keywords internal
#' @noRd
process_edge_data <- function(netlet, netify_type, weight_attr, remove_zeros, ego_netlet) {
    
    # extract raw adjacency data
    raw_data <- get_raw(netlet)

    # handle different data structures
    if (netify_type == 'cross_sec') {
            # for cross-sectional, raw_data is a matrix
            edge_data <- melt_matrix_sparse(
                    raw_data, 
                    remove_zeros = remove_zeros,
                    remove_diagonal = TRUE
            )
    } else if (netify_type == 'longit_array') {
            # for longitudinal array, melt the 3d array
            edge_data <- melt_array_sparse(
                    raw_data,
                    remove_zeros = remove_zeros,
                    remove_diagonal = TRUE
            )
    } else if (netify_type == 'longit_list') {
            # for longitudinal list, process each time period
            edge_data <- melt_list_sparse(
                    raw_data,
                    remove_zeros = remove_zeros,
                    remove_diagonal = TRUE
            )
    }

    # rename weight column
    if (!is.null(weight_attr)) {
            names(edge_data)[names(edge_data) == 'value'] <- weight_attr
    } else {
            names(edge_data)[names(edge_data) == 'value'] <- 'net_value'
    }

    # handle ego networks efficiently
    if (netify_type != 'cross_sec' && !is.null(ego_netlet) && ego_netlet) {
            if ('L1' %in% names(edge_data)) {
                    # vectorized extraction
                    edge_data$L1 <- sub("^[^_]+__", "", edge_data$L1)
            }
    }

    edge_data
}

#' Merge dyadic attributes into edge data
#' @keywords internal
#' @noRd
merge_dyadic_attributes <- function(edge_data, dyad_data_attr, netify_type) {
    
    # convert to meltable format
    dyad_data_melted <- melt_dyad_data(dyad_data_attr)

    if (nrow(dyad_data_melted) == 0) {
        return(edge_data)
    }

    # remove self-loops
    dyad_data_melted <- dyad_data_melted[
        dyad_data_melted$Var1 != dyad_data_melted$Var2, 
    ]

    # use tapply for efficient reshaping instead of dcast
    var_names <- unique(dyad_data_melted$Var3)

    # create keys for matching
    if (netify_type == 'cross_sec') {
        edge_key <- paste(edge_data$Var1, edge_data$Var2, sep = "__|__")
        dyad_key <- paste(dyad_data_melted$Var1, dyad_data_melted$Var2, sep = "__|__")
    } else {
        edge_key <- paste(edge_data$Var1, edge_data$Var2, edge_data$L1, sep = "__|__")
        dyad_key <- paste(dyad_data_melted$Var1, dyad_data_melted$Var2, 
                                            dyad_data_melted$L1, sep = "__|__")
    }

    # add each variable using match
    for (var in var_names) {
        var_data <- dyad_data_melted[dyad_data_melted$Var3 == var, ]
        var_key <- if (netify_type == 'cross_sec') {
            paste(var_data$Var1, var_data$Var2, sep = "__|__")
        } else {
            paste(var_data$Var1, var_data$Var2, var_data$L1, sep = "__|__")
        }
        
        match_idx <- match(edge_key, var_key)
        edge_data[[var]] <- var_data$value[match_idx]
    }

    #
    return(edge_data)
}

#' Finalize edge data structure
#' @keywords internal
#' @noRd
finalize_edge_data <- function(edge_data, netify_type) {
    
    # Add time column for cross-sectional
    if (netify_type == 'cross_sec' && !'L1' %in% names(edge_data)) {
        edge_data$L1 <- "1"
    }
    
    # Ensure L1 exists
    if (!'L1' %in% names(edge_data)) {
        edge_data$L1 <- "1"
    }
    
    # Reorder columns
    id_vars <- c('Var1', 'Var2', 'L1')
    other_vars <- setdiff(names(edge_data), id_vars)
    edge_data <- edge_data[, c(id_vars, other_vars), drop = FALSE]
    
    # Rename ID columns
    names(edge_data)[1:3] <- c('from', 'to', 'time')
    
    # Ensure time is character
    edge_data$time <- as.character(edge_data$time)
    
    edge_data
}

#' Process nodal data from netify object
#' @keywords internal
#' @noRd
process_nodal_data <- function(obj_attrs, netify_type) {
    
    # Get nodal data from attributes or construct from actor_pds
    if (!is.null(obj_attrs$nodal_data)) {
        nodal_data <- obj_attrs$nodal_data
    } else if (!is.null(obj_attrs$actor_pds)) {
        nodal_data <- actor_pds_to_frame(obj_attrs$actor_pds)
    } else {
        # Fallback: empty data frame
        nodal_data <- data.frame(
            actor = character(0),
            time = character(0),
            stringsAsFactors = FALSE
        )
    }
    
    # Ensure data frame
    if (!is.data.frame(nodal_data)) {
        nodal_data <- as.data.frame(nodal_data, stringsAsFactors = FALSE)
    }
    
    # Add time column if missing
    if (nrow(nodal_data) > 0 && !'time' %in% names(nodal_data)) {
        nodal_data$time <- "1"
    }
    
    # Standardize column order
    if (nrow(nodal_data) > 0) {
        id_vars <- c('actor', 'time')
        # Ensure actor column exists
        if (!'actor' %in% names(nodal_data)) {
            # Try to find actor column by position or name pattern
            possible_actor_cols <- c('name', 'node', 'vertex')
            actor_col <- intersect(possible_actor_cols, names(nodal_data))
            if (length(actor_col) > 0) {
                names(nodal_data)[names(nodal_data) == actor_col[1]] <- 'actor'
            } else if (ncol(nodal_data) > 0) {
                # Assume first column is actor
                names(nodal_data)[1] <- 'actor'
            }
        }
        
        existing_id_vars <- intersect(id_vars, names(nodal_data))
        other_vars <- setdiff(names(nodal_data), id_vars)
        nodal_data <- nodal_data[, c(existing_id_vars, other_vars), drop = FALSE]
        
        # Rename actor column to name
        if ('actor' %in% names(nodal_data)) {
            names(nodal_data)[names(nodal_data) == 'actor'] <- 'name'
        }
    }
    
    # Ensure time is character
    if ('time' %in% names(nodal_data)) {
        nodal_data$time <- as.character(nodal_data$time)
    }
    
    nodal_data
}

#' Decompose an igraph object into base R components
#'
#' `decompose_igraph` extracts the adjacency matrix and any vertex/edge attributes 
#' from an igraph object, returning them in a standardized list format. 
#'
#' @param grph An igraph object to be decomposed.
#' @param weight Character string specifying the edge attribute to use as weights 
#'   in the adjacency matrix. If NULL (default), the unweighted adjacency matrix 
#'   is returned with 1s for edges and 0s for non-edges.
#'
#' @return A list containing four elements:
#'   \itemize{
#'     \item \strong{adj_mat}: The adjacency matrix extracted from the igraph object
#'       \itemize{
#'         \item For unipartite graphs: Square matrix of dimension n×n
#'         \item For bipartite graphs: Rectangular matrix of dimension n₁×n₂
#'         \item Values are edge weights if specified, otherwise 0/1
#'       }
#'     \item \strong{ndata}: A data frame of vertex attributes, or NULL if none exist
#'       \itemize{
#'         \item Always includes an 'actor' column with vertex names
#'         \item Additional columns for each vertex attribute
#'       }
#'     \item \strong{ddata}: A data frame of edge attributes, or NULL if none exist
#'       \itemize{
#'         \item Columns 'from' and 'to' specify edge endpoints
#'         \item Additional columns for each edge attribute
#'       }
#'     \item \strong{weight}: The edge attribute name used for weights, if provided
#'   }
#'
#' @details
#' The function handles both unipartite and bipartite graphs appropriately:
#' 
#' \strong{Graph type detection:}
#' \itemize{
#'   \item Bipartite graphs must have a logical 'type' vertex attribute
#'   \item If the 'type' attribute exists but is not logical, the graph is treated 
#'     as unipartite with a warning
#' }
#' 
#' \strong{Vertex naming:}
#' 
#' If the graph lacks vertex names, default names are assigned:
#' \itemize{
#'   \item Unipartite graphs: "a1", "a2", ..., "an"
#'   \item Bipartite graphs: "r1", "r2", ... for type 1; "c1", "c2", ... for type 2
#' }
#' 
#' Existing vertex names are always preserved and used in the output.
#' 
#' \strong{Matrix extraction:}
#' \itemize{
#'   \item Unipartite: Uses `as_adjacency_matrix()` to get n×n matrix
#'   \item Bipartite: Uses `as_biadjacency_matrix()` to get n₁×n₂ matrix where 
#'     rows correspond to type=FALSE vertices and columns to type=TRUE vertices
#' }
#' 
#' \strong{Attribute handling:}
#' 
#' All vertex and edge attributes are preserved in the output data frames. System 
#' attributes (like 'name' and 'type') are included alongside user-defined attributes.
#'
#' @note 
#' For longitudinal networks with changing actor composition, explicitly set vertex 
#' names before decomposition to ensure consistent actor identification across time 
#' periods.
#' 
#' The adjacency matrix is always returned as a standard R matrix (not sparse), 
#' which may have memory implications for very large graphs.
#' 
#' When edge attributes are used as weights, ensure they contain numeric values. 
#' Non-numeric edge attributes will cause an error.
#'
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export decompose_igraph

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

#' Decompose a network object into base R components
#'
#' `decompose_statnet` (also available as `decompose_network`) extracts the 
#' adjacency matrix and any vertex/edge attributes 
#' from a network object (from the statnet suite), returning them in a standardized 
#' list format. 
#'
#' @param ntwk A network object (class "network") to be decomposed.
#' @param weight Character string specifying the edge attribute to use as weights 
#'   in the adjacency matrix. If NULL (default), the unweighted adjacency matrix 
#'   is returned with 1s for edges and 0s for non-edges.
#'
#' @return A list containing four elements:
#'   \itemize{
#'     \item \strong{adj_mat}: The adjacency matrix extracted from the network object
#'       \itemize{
#'         \item For unipartite networks: Square matrix of dimension n×n
#'         \item For bipartite networks: Full square matrix of dimension (n₁+n₂)×(n₁+n₂)
#'         \item Values are edge weights if specified, otherwise 0/1
#'       }
#'     \item \strong{ndata}: A data frame of vertex attributes, or NULL if none exist
#'       \itemize{
#'         \item Always includes an 'actor' column with vertex names
#'         \item Additional columns for each vertex attribute (excluding system attributes)
#'       }
#'     \item \strong{ddata}: A data frame of edge attributes, or NULL if none exist
#'       \itemize{
#'         \item Columns 'from' and 'to' specify edge endpoints using vertex names
#'         \item Additional columns for each edge attribute
#'       }
#'     \item \strong{weight}: The edge attribute name used for weights, if provided
#'   }
#'
#' @details
#' The function handles both unipartite and bipartite networks appropriately:
#' 
#' \strong{Network type detection:}
#' \itemize{
#'   \item Bipartite networks are identified using `is.bipartite()`
#'   \item The bipartite partition size is retrieved from the 'bipartite' network attribute
#' }
#' 
#' \strong{Vertex naming:}
#' 
#' The function checks for existing vertex names in the 'vertex.names' attribute. 
#' If names are just the default numeric sequence (1, 2, 3, ...), they are treated 
#' as missing. Default names are assigned when needed:
#' \itemize{
#'   \item Unipartite networks: "a1", "a2", ..., "an"
#'   \item Bipartite networks: "r1", "r2", ... for first partition; "c1", "c2", ... for second partition
#' }
#' 
#' \strong{Matrix extraction:}
#' 
#' Unlike igraph's bipartite handling, the network package returns the full 
#' adjacency matrix even for bipartite networks. The function:
#' \itemize{
#'   \item Extracts the full matrix using `as.matrix.network.adjacency()`
#'   \item For bipartite networks, the matrix has dimension (n₁+n₂)×(n₁+n₂) with 
#'     the first n₁ rows/columns for the first partition
#' }
#' 
#' \strong{Attribute handling:}
#' 
#' System attributes ('vertex.names' and 'na') are excluded from the vertex 
#' attribute data frame. All user-defined vertex and edge attributes are preserved.
#'
#' @note 
#' For longitudinal networks with changing actor composition, explicitly set vertex 
#' names before decomposition to ensure consistent actor identification across time 
#' periods.
#' 
#' The adjacency matrix format differs between this function and `decompose_igraph` 
#' for bipartite networks: this function returns the full square matrix while 
#' `decompose_igraph` returns only the rectangular bipartite portion.
#' 
#' Edge directions are preserved in the adjacency matrix according to the network's 
#' directed/undirected property.
#'
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export decompose_statnet
#' @aliases decompose_network

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

