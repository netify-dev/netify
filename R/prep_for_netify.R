#' Convert igraph, network, socio-matrices/arrays, and lists of each of those object to a netify object
#'
#' `prep_for_netify` takes in a network object (e.g., an igraph or network object) 
#' and outputs a netify object. If the input is already a matrix/array/list of matrices,
#' then it simply calls `new_netify`. Otherwise, it calls the appropriate internal 
#' helper (`process_igraph` or `process_network`) to extract adjacency matrix 
#' and any nodal/dyadic attributes, and then calls `new_netify`.
#' 
#' @aliases convert.to.netify
#' @param net_obj An R object (e.g., \code{igraph}, \code{network}, socio-matrix/array, or a list of these objects) that you want to convert to a netify object.
#' @param weight Optional. Name of the weight attribute in \code{net_obj} to be used 
#' as the main edge weight in the netify object. Default is \code{NULL}. Important to add  
#' for \code{igraph} and \code{network} objects because they do not have a default weight.
#' @param ... Additional arguments passed to the internal processing functions. 
#' @return a netify object
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords netify
#'
#' @export prep_for_netify

prep_for_netify <- function(
    net_obj,
    weight  = NULL, 
    ...
){

    # check type of net_obj
    class_net_obj <- class(net_obj)

    # capture any relevant user specified args
    # if user specified nodal_data or dyad_data,
    # we will use those instead of the ones extracted from net_obj
    other_args <- list(...)
    user_ndata <- other_args$nodal_data
    user_ddata <- other_args$dyad_data

    # placeholders for final adjacency data, plus nodal/dyadic
    adj_data <- NULL
    nodal_data <- NULL
    dyad_data <- NULL

    # if matrix/array/list-of-matrices, just organize
    if (class_net_obj %in% c("matrix", "array") || 
        (is.list(net_obj) && is.matrix(net_obj[[1]]) )) {
      adj_data <- net_obj
    }

    # if igraph
    if (is.null(adj_data) && inherits(net_obj, "igraph")) {
      processed <- process_igraph(net_obj, weight = weight)
      adj_data <- processed$adj_mat
      nodal_data <- processed$ndata
      dyad_data <- processed$ddata
      weight <- processed$weight
    }

    if (is.null(adj_data) && is.list(net_obj) && 
        all(sapply(net_obj, inherits, "igraph"))) {
      processed_list <- lapply(
        net_obj, process_igraph, weight = weight)
      adj_data <- lapply(processed_list, function(z) z$adj_mat)
      nodal_data <- processed_list[[1]]$ndata
      dyad_data <- processed_list[[1]]$ddata
      weight <- processed_list[[1]]$weight
    }

    # if network
    if (is.null(adj_data) && inherits(net_obj, "network")) {
      processed <- process_network(net_obj, weight = weight)
      adj_data <- processed$adj_mat
      nodal_data <- processed$ndata
      dyad_data <- processed$ddata
      weight <- processed$weight
    }

    if (is.null(adj_data) && is.list(net_obj) && 
        all(sapply(net_obj, inherits, "network"))) {
      processed_list <- lapply(
        net_obj, process_network, weight = weight)
      adj_data <- lapply(processed_list, function(z) z$adj_mat)
      nodal_data <- processed_list[[1]]$ndata
      dyad_data <- processed_list[[1]]$ddata
      weight <- processed_list[[1]]$weight
    }

    # if net_obj is not one of the supported types, stop
    if (!(is.matrix(net_obj) ||
          is.array(net_obj) ||
          inherits(net_obj, "igraph") ||
          inherits(net_obj, "network") ||
          (is.list(net_obj) && (all(sapply(net_obj, is.matrix)) ||
                                all(sapply(net_obj, inherits, "igraph")) ||
                                all(sapply(net_obj, inherits, "network"))))
         )) {
      cli::cli_alert_danger(
        "Error: Unsupported class for `net_obj`. Supported classes are: matrix, array, igraph, network, or a list of these objects.")
      stop()
    }

    # build final netify object
    out <- new_netify(
      data = adj_data,
      weight = weight,
      nodal_data = nodal_data,
    #   dyad_data = dyad_data,
      dyad_data = NULL,      
      ...
    )

    #
    return(out)
}

#' Break down an `igraph` object into base R components
#'
#' This function processes an igraph object to extract its adjacency matrix along with any available vertex and edge attributes, returning them in a standardized list format. It is intended for internal use.
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
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
process_igraph <- function(grph, weight = NULL) {
    stopifnot(inherits(grph, "igraph"))

    # pull out adjacency matrix (sparse=FALSE => standard matrix)
    adj_mat <- igraph::as_adjacency_matrix(grph, attr = weight, sparse = FALSE)

    # pull out vertex attributes as a data.frame, if any
    v_labs <- igraph::vertex_attr_names(grph)
    if (length(v_labs) > 0) {
        ndata <- igraph::as_data_frame(grph, what = "vertices")
    } else {
        ndata <- NULL
    }

    # pull out edge attributes as a data.frame, if any
    e_labs <- igraph::edge_attr_names(grph)
    if (length(e_labs) > 0) {
        ddata <- igraph::as_data_frame(grph, what = "edges")
    } else {
        ddata <- NULL
    }

    list(
        adj_mat = adj_mat,
        ndata   = ndata,
        ddata   = ddata,
        weight  = weight
    )
}

#' Break down a `network` object into base R components
#'
#' This function processes a network object from the network package to extract its adjacency matrix along with any available vertex and edge attributes, and returns them in a standardized list format. It is designed for internal use.
#'
#' @param ntwk A network object.
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
#' 
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
process_network <- function(ntwk, weight = NULL) {
    stopifnot(inherits(ntwk, "network"))

    # pull out adjacency matrix
    adj_mat <- network::as.matrix.network.adjacency(ntwk, attrname = weight)

    # vertex attributes
    v_labs <- network::list.vertex.attributes(ntwk)
    if (length(v_labs) > 0) {
        ndata <- as.data.frame(
            sapply(v_labs, function(attr) {
                network::get.vertex.attribute(ntwk, attr)
            }),
            stringsAsFactors = FALSE
        )
    } else {
        ndata <- NULL
    }

    # edge attributes
    vertex_names <- network::get.vertex.attribute(ntwk, "vertex.names")
    e_labs <- network::list.edge.attributes(ntwk)
    if (length(e_labs) > 0) {
        edgelist <- network::as.edgelist(ntwk)
        attr_df <- as.data.frame(
            sapply(e_labs, function(attr) {
                network::get.edge.attribute(ntwk, attr)
            }),
            stringsAsFactors = FALSE
        )
        ddata <- data.frame(
            from = vertex_names[edgelist[, 1]],
            to   = vertex_names[edgelist[, 2]],
            attr_df,
            stringsAsFactors = FALSE
        )
    } else {
        ddata <- NULL
    }

    list(
        adj_mat = adj_mat,
        ndata   = ndata,
        ddata   = ddata,
        weight  = weight
    )
}
