#' Convert netify objects to statnet network format
#'
#' Transforms netify network objects into statnet's network
#' objects (also available as `netify_to_network`,
#' `to_statnet`, and `to_network`),
#' providing access to the extensive statistical modeling capabilities
#' of the statnet suite, including ERGMs (Exponential Random Graph Models),
#' descriptive statistics, and network visualization tools.
#'
#' @param netlet A netify object containing network data. Currently supports
#'   single-layer networks only. For multilayer networks, use
#'   \code{\link{subset_netify}} to extract individual layers first.
#' @param add_nodal_attribs Logical. If TRUE (default), includes nodal attributes
#'   from the netify object as vertex attributes in the network object. Set to
#'   FALSE to create a network with structure only.
#' @param add_dyad_attribs Logical. If TRUE (default), includes dyadic attributes
#'   from the netify object as edge attributes in the network object. Set to
#'   FALSE to exclude edge covariates.
#'
#' @return A network object or list of network objects:
#'   \describe{
#'     \item{Cross-sectional networks}{Returns a single network object}
#'     \item{Longitudinal networks}{Returns a named list of network objects,
#'       with names corresponding to time periods}
#'   }
#'
#'   The resulting network object(s) will have:
#'   \itemize{
#'     \item Vertices named according to actors in the netify object
#'     \item Edge weights from the netify weight variable stored as "weight" attribute
#'     \item Vertex attributes for each nodal variable (if add_nodal_attribs = TRUE)
#'     \item Edge attributes for each dyadic variable (if add_dyad_attribs = TRUE)
#'     \item Proper directedness based on the symmetric parameter of the netify object
#'   }
#'
#' @details
#' The conversion process handles different netify structures:
#' \itemize{
#'   \item \strong{Cross-sectional}: Direct conversion to a single network object
#'   \item \strong{Longitudinal arrays}: Internally converted to list format, then
#'     each time slice becomes a separate network object
#'   \item \strong{Longitudinal lists}: Each time period converted to separate network object
#' }
#'
#' The statnet network format stores networks as an edgelist with attributes,
#' making it memory-efficient for sparse networks. All nodal and dyadic attributes
#' from the netify object are preserved and can be used in subsequent ERGM
#' modeling or network analysis.
#'
#' For longitudinal data, each time period results in an independent network
#' object. This format is suitable for discrete-time network analysis or
#' pooled ERGM estimation across time periods.
#'
#' @note
#' This function requires the network package (part of statnet) to be installed.
#'
#' For ERGM modeling, the ergm package (also part of statnet) should be loaded
#' after creating the network objects.
#'
#' @examples
#' # Load example data
#' data(icews)
#'
#' # Cross-sectional example
#' icews_10 <- icews[icews$year == 2010, ]
#'
#' # Create netify object with attributes
#' dvars <- c("matlCoop", "verbConf", "matlConf")
#' nvars <- c("i_polity2", "i_log_gdp", "i_log_pop")
#'
#' verbCoop_net <- netify(
#'     icews_10,
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop",
#'     dyad_vars = dvars,
#'     dyad_vars_symmetric = rep(FALSE, length(dvars)),
#'     nodal_vars = nvars
#' )
#'
#' # Convert to statnet network object
#' ntwk <- netify_to_statnet(verbCoop_net)
#'
#' # Examine the result
#' ntwk
#' network::network.size(ntwk) # number of vertices
#' network::network.edgecount(ntwk) # number of edges
#' network::list.vertex.attributes(ntwk) # nodal attributes
#' network::list.edge.attributes(ntwk) # edge attributes
#'
#' # Access specific attributes
#' network::get.vertex.attribute(ntwk, "i_polity2") # polity scores
#' network::get.edge.attribute(ntwk, "matlCoop") # material cooperation
#'
#' # Check network properties
#' network::is.directed(ntwk) # TRUE for this example
#' network::has.loops(ntwk) # FALSE (no self-ties)
#'
#' \donttest{
#' # Longitudinal example
#' verbCoop_longit <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE,
#'     weight = "verbCoop",
#'     dyad_vars = dvars,
#'     dyad_vars_symmetric = rep(FALSE, length(dvars)),
#'     nodal_vars = nvars
#' )
#'
#' # Convert to list of network objects
#' ntwk_list <- netify_to_statnet(verbCoop_longit)
#'
#' # Examine structure
#' length(ntwk_list) # number of time periods
#' names(ntwk_list) # time period labels
#'
#' # Access specific time period
#' ntwk_2002 <- ntwk_list[["2002"]]
#' ntwk_2002
#' }
#'
#' \dontrun{
#' # Use with ergm for modeling (requires ergm package)
#' library(ergm)
#' model <- ergm(ntwk ~ edges + mutual + nodematch("i_polity2"))
#' }
#'
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @export netify_to_statnet
#' @aliases netify_to_network to_statnet to_network

netify_to_statnet <- function(
    netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE) {
    # check if netify object
    netify_check(netlet)

    # if more than one layer tell user they must specify a single layer
    if (length(attributes(netlet)$layers) > 1) {
        cli::cli_alert_danger(
            "Error: This object has multiple layers.
      `netify_to_statnet` does not currently support multilayer `netify` inputs.
      Please use the `subset_netify` function to create a `netify` object with a single layer."
        )
        stop()
    }

    # assert dependencies for remapping data to network
    assert_dependency("network")

    ## three cases: cross-sec/matrix, longit list, longit array
    netlet_type <- attr(netlet, "netify_type")

    # if type array convert to list since network
    # doesnt support arrays anyhow
    if (netlet_type == "longit_array") {
        netlet <- array_to_list(netlet)
    }

    # check other attributes
    nodal_data_exist <- !is.null(
        attr(netlet, "nodal_data")[[1]]
    )
    dyad_data_exist <- !is.null(
        attr(netlet, "dyad_data")[[1]]
    )

    ## cross-sec case
    if (netlet_type == "cross_sec") {
        # convert to a statnet network object
        ntwk <- netify_net_to_statnet(netlet)

        # process nodal attributes if exist
        if (nodal_data_exist & add_nodal_attribs) {
            ntwk <- add_nodal_to_statnet(
                netlet, attr(netlet, "nodal_data"), ntwk
            )
        }

        # process dyadic attributes if exist
        if (dyad_data_exist & add_dyad_attribs) {
            ntwk <- add_dyad_to_statnet(
                netlet, attr(netlet, "dyad_data"), ntwk
            )
        }
    } # done with cross-sec case

    ## longit case
    if (netlet_type %in% c("longit_array", "longit_list")) {
        # iterate through netlet
        ntwk <- lapply(1:length(netlet), function(ii) {
            # get netlet slice
            netlet_slice <- netlet[[ii]]

            # get time listing
            time_val <- names(netlet)[ii]

            # convert to a statnet network object
            ntwk_slice <- netify_net_to_statnet(netlet_slice)

            # process nodal attributes if exist
            if (nodal_data_exist & add_nodal_attribs) {
                ntwk_slice <- add_nodal_to_statnet(
                    netlet_slice, attr(netlet, "nodal_data"),
                    ntwk_slice, time_val
                )
            }

            # process dyadic attributes if exist
            if (dyad_data_exist & add_dyad_attribs) {
                ntwk_slice <- add_dyad_to_statnet(
                    netlet_slice, attr(netlet, "dyad_data"),
                    ntwk_slice, time_val
                )
            }

            #
            return(ntwk_slice)
        })
        names(ntwk) <- names(netlet)
    } # done with longit case

    #
    return(ntwk)
}

#' @rdname netify_to_statnet
#' @export
netify_to_network <- netify_to_statnet

#' @rdname netify_to_statnet
#' @export
to_statnet <- netify_to_statnet

#' @rdname netify_to_statnet
#' @export
to_network <- netify_to_statnet

#' netify_net_to_statnet
#'
#' Convert netify object to a statnet network object
#'
#' @param netlet netify object
#' @return statnet network object
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd

netify_net_to_statnet <- function(netlet) {
    # check if bipartite
    bipartite_logical <- ifelse(attr(netlet, "mode") == "bipartite", TRUE, FALSE)

    # if weight is NULL then create logical to set value for ignore.eval
    if (is.null(attr(netlet, "weight", exact = TRUE))) {
        ignore_eval <- TRUE
    } else {
        ignore_eval <- FALSE
    }

    # convert to a statnet network object
    statnet_object <- network::network(
        get_raw(netlet),
        matrix.type = "adjacency",
        directed = !attr(netlet, "symmetric"),
        loops = !attr(netlet, "diag_to_NA"),
        bipartite = bipartite_logical,
        names.eval = attr(netlet, "weight", exact = TRUE),
        ignore.eval = ignore_eval
    )

    # set as network attribute as well if weight provided
    if (!is.null(attr(netlet, "weight", exact = TRUE))) {
        network::set.network.attribute(
            statnet_object, attr(netlet, "weight", exact = TRUE),
            get_raw(netlet)
        )
    }

    #
    return(statnet_object)
}

#' add_nodal_to_statnet
#'
#' Add nodal attributes to a network object from netify object
#'
#' @param netlet netify object
#' @param node_data node data from netlet object
#' @param statnet_object network object to modify
#' @param time time indicator for longit case
#' @return statnet network object with nodal attributes added
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd

add_nodal_to_statnet <- function(
    netlet, node_data, statnet_object, time = NULL) {
    # slice by time if relevant
    if (!is.null(time)) {
        node_data <- node_data[node_data[, 2] == time, ]
    }

    # loop through and add to a statnet network object
    node_var_start <- ifelse(is.null(time), 2, 3)
    for (ii in node_var_start:ncol(node_data)) {
        network::set.vertex.attribute(
            statnet_object, names(node_data)[ii], node_data[, ii]
        )
    }

    #
    return(statnet_object)
}

#' add_dyad_to_statnet
#'
#' Add dyad attributes to a network object from netify object
#'
#' @param netlet netify object
#' @param dyad_data_list dyad data from netlet object
#' @param statnet_object network object to modify
#' @param time time indicator for longit case
#' @return statnet network object with dyad attributes added
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd

add_dyad_to_statnet <- function(
    netlet, dyad_data_list, statnet_object, time = NULL) {
    # get dyadic data for specified time period
    if (is.null(time)) {
        var_matrices <- dyad_data_list[[1]]
    } else {
        var_matrices <- dyad_data_list[[time]]
    }

    # get var names from the list of matrices
    vars <- names(var_matrices)

    # cache netlet attributes for efficiency
    netlet_mode <- attr(netlet, "mode")
    netlet_diag_to_NA <- attr(netlet, "diag_to_NA")
    bipartite_logical <- netlet_mode == "bipartite"

    # iterate through dyadic vars and add into network object
    for (ii in seq_along(vars)) {
        var_name <- vars[ii]

        # get matrix for this variable
        dData <- var_matrices[[var_name]]

        # replace diagonal with 0s except if
        # netlet is bipartite or
        # diag_to_NA is set to FALSE
        if (!bipartite_logical && netlet_diag_to_NA) {
            diag(dData) <- 0
        }

        # set as edge attrib if not bipartite, weird sizing issue
        # when trying to add edge attribute to bipartite network
        # that is not clear to me
        if (!bipartite_logical) {
            network::set.edge.value(
                statnet_object, paste0(var_name, "_e"), dData
            )
        }

        # set as network attrib
        network::set.network.attribute(
            statnet_object, var_name, dData
        )
    }

    return(statnet_object)
}
