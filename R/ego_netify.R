#' Create ego network from a netify object
#'
#' `ego_netify` extracts an ego network from a
#' netify object. An ego network consists of a focal node (ego) and its immediate
#' neighbors (alters). For weighted networks, users can define neighborhoods using
#' edge weight thresholds. The function returns a netify object representing the
#' ego network.
#'
#' @param netlet A netify object (class "netify") from which to extract the ego network.
#' @param ego Character string specifying the name of the ego for whom to
#'   create the ego network. Must match an actor name in the netify object.
#' @param threshold Numeric value or vector specifying the threshold for including
#'   alters in the ego network based on edge weights. For longitudinal networks,
#'   can be a vector with length equal to the number of time periods to apply
#'   different thresholds over time. If NULL (default), uses 0 for unweighted
#'   networks and the mean edge weight for weighted networks.
#' @param ngbd_direction Character string specifying which neighbors to include
#'   for directed networks. Options are:
#'   \itemize{
#'     \item \code{"out"}: Include alters that ego has outgoing ties to
#'     \item \code{"in"}: Include alters that ego has incoming ties from
#'     \item \code{"any"}: Include alters with any tie to/from ego (default)
#'   }
#' @param include_ego Logical. If TRUE (default), the ego node is included in
#'   the ego network. If FALSE, only alters are included.
#'
#' @return A netify object representing the ego network. For longitudinal networks,
#'   returns a list of netify objects with one ego network per time period.
#'
#'   Each returned netify object includes additional attributes:
#'   \itemize{
#'     \item \code{ego_netify}: TRUE (indicator that this is an ego network)
#'     \item \code{ego_id}: Identifier of the ego
#'     \item \code{threshold}: Threshold value(s) used
#'     \item \code{ngbd_direction}: Direction specification used
#'     \item \code{include_ego}: Whether ego was included
#'   }
#'
#' @details
#' The function extracts an ego network by identifying all nodes connected to the
#' specified ego based on the given criteria:
#'
#' \strong{Neighborhood definition:}
#' \itemize{
#'   \item For unweighted networks: All nodes with edges to/from ego (threshold = 0)
#'   \item For weighted networks: All nodes with edge weights exceeding the threshold
#'   \item Direction matters only for directed networks (controlled by ngbd_direction)
#' }
#'
#' \strong{Threshold behavior:}
#' \itemize{
#'   \item If not specified, defaults to 0 for unweighted networks
#'   \item If not specified for weighted networks, uses the mean edge weight
#'   \item For longitudinal networks, can vary by time period if a vector is provided
#'   \item Edges with weights > threshold are included (not ≥)
#' }
#'
#' \strong{Output structure:}
#'
#' The function preserves all attributes from the original netify object, including
#' nodal and dyadic variables, but subsets them to include only ego and its neighbors.
#' For longitudinal networks, ego networks may vary in composition across time periods
#' as relationships change.
#'
#' \strong{Limitations:}
#' \itemize{
#'   \item Currently does not support multilayer networks
#'   \item Currently does not support bipartite networks
#' }
#'
#' @note
#' To create ego networks for multiple egos, use \code{lapply} or a loop to call
#' this function for each ego separately.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export ego_netify

ego_netify <- function(
    netlet, ego, threshold = NULL,
    ngbd_direction = "any", include_ego = TRUE) {
    ######################
    # check if netify object
    netify_check(netlet)

    # if longit array to longit list so we dont
    # need separate processes and we cant stay in
    # array format for ego nets anyhow
    # (i.e., ego+alters change over time)
    if (attr(netlet, "netify_type") == "longit_array") {
        netlet <- array_to_list(netlet, preserveAttr = TRUE)
    }

    # pull out attrs and msrmnts of original
    obj_attrs <- attributes(netlet)
    msrmnts <- netify_measurements(netlet)

    # determine if multilayer and/or longitudinal
    weighted <- !obj_attrs$weight_binary
    multilayer <- ifelse(length(obj_attrs$layers) > 1, TRUE, FALSE)
    longitudinal <- ifelse(obj_attrs$netify_type != "cross_sec", TRUE, FALSE)
    bipartite <- ifelse(obj_attrs$mode == "bipartite", TRUE, FALSE)

    # get netlet type
    netlet_type <- obj_attrs$netify_type
    ######################

    ######################
    # stop if multilayer and say we dont support
    if (multilayer) {
        cli::cli_alert_danger(
            "Error: This object has multiple layers.
            `ego_netify` does not currently support multilayer `netify` inputs.
            Please use the `subset_netify` function to create a `netify` object with a single layer."
        )
        stop()
    }

    # stop if bipartite and say we dont support
    if (bipartite) {
        cli::cli_alert_danger(
            "Error: This object is bipartite.
            `ego_netify` does not currently support bipartite `netify` inputs.
            Please use the `subset_netify` function to create a `netify` object with a single layer."
        )
        stop()
    }
    ######################

    ######################
    # Check if the ego is a single character value
    if (!is.character(ego) || length(ego) != 1) {
        cli::cli_alert_danger("`ego` must be a single character value.")
        stop()
    }

    # Check if the ego is in the network
    poss_actors <- unique(obj_attrs$actor_pds$actor)
    if (!ego %in% poss_actors) {
        cli::cli_alert_danger(
            paste0(
                'Error: The ego "', ego, '" is not in the data.'
            )
        )
        stop()
    }

    # add check for ngbd_direction
    if (!ngbd_direction %in% c("out", "in", "any")) {
        cli::cli_alert_danger(
            'Error: `ngbd_direction` must be one of "out", "in", or "any".'
        )
        stop()
    }
    ######################

    ######################
    # if supplied check that threshold is numeric
    if (!is.null(threshold) && !is.numeric(threshold)) {
        cli::cli_alert_danger("`threshold` argument must be a numeric input.")
        stop()
    }
    ######################

    ######################
    # get raw network
    raw_net <- get_raw(netlet)

    # if cross sec convert to a list object so that
    # we can use lapply
    if (netlet_type == "cross_sec") {
        raw_net <- list(raw_net)
    }
    ######################

    ######################
    # if unweighted then define neighborhood using threshold of zero
    if (!weighted) {
        threshold <- rep(0, length(raw_net))
    }

    # if weighted then define neighborhood using mean of edge weights
    # if threshold is not supplied
    if (weighted) {
        # if threshold provided and net is longit then check if
        # threshold is a vector of length equal to the number of time points
        # if not then rep the threshold to be the same for all time points
        if (!is.null(threshold) && longitudinal) {
            if (length(threshold) != length(raw_net)) {
                threshold <- rep(threshold, length(raw_net))
            }
        }

        # iterate through each list element and create new
        # adjacency matrix with only the ego and its neighbors
        if (is.null(threshold)) {
            threshold <- unlist(
                lapply(raw_net, function(net) {
                    mean(c(net), na.rm = TRUE)
                })
            )
        }
    }
    ######################

    ######################
    # define neighborhood for ego
    # based on thresh get vector of actors to keep by list element
    ego_nets <- get_ngbd_net_for_ego(
        raw_net, ego, threshold, include_ego, ngbd_direction
    )
    ######################

    ######################
    # add back in netify attributes

    # if cross sec then just work with the single network
    if (netlet_type == "cross_sec") {
        ego_net <- ego_nets[[1]]

        # create new attributes based on original
        new_attrs <- obj_attrs
        new_attrs$ego_netify <- TRUE
        new_attrs$threshold <- threshold
        new_attrs$ngbd_direction <- ngbd_direction
        new_attrs$include_ego <- include_ego
        new_attrs$ego_id <- ego
        new_attrs$ego_entry <- ego

        # update dim and dimnames
        new_attrs[c("dim", "dimnames")] <- attributes(ego_net)[c("dim", "dimnames")]

        # apply attributes
        attributes(ego_net) <- new_attrs

        return(ego_net)
    }

    # if longitudinal, process list of networks
    if (netlet_type == "longit_list") {
        # process each time period
        ego_nets_processed <- lapply(1:length(ego_nets), function(ii) {
            net <- ego_nets[[ii]]
            time_name <- names(ego_nets)[ii]

            # create attributes for this time period
            subobj_attrs <- attributes(netlet[[1]])
            subobj_attrs$ego_netify <- TRUE
            subobj_attrs$threshold <- threshold[ii]
            subobj_attrs$ngbd_direction <- ngbd_direction
            subobj_attrs$include_ego <- include_ego
            subobj_attrs$ego_id <- ego # Just the ego name, not ego: time
            subobj_attrs$ego_entry <- ego

            # update dim and dimnames
            subobj_attrs[c("dim", "dimnames")] <- attributes(net)[c("dim", "dimnames")]

            # apply attributes
            attributes(net) <- subobj_attrs
            return(net)
        })

        # add names - just time periods like regular netify objects
        names(ego_nets_processed) <- names(ego_nets)

        # add top-level attributes
        list_attrs <- obj_attrs
        list_attrs$ego_netify <- TRUE
        list_attrs$threshold <- threshold
        list_attrs$ngbd_direction <- ngbd_direction
        list_attrs$include_ego <- include_ego
        list_attrs$ego_longit <- TRUE
        list_attrs$ego_entry <- ego
        list_attrs$ego_id <- ego # Just the ego name
        list_attrs$names <- names(ego_nets)

        # apply attributes to list
        attributes(ego_nets_processed) <- list_attrs

        return(ego_nets_processed)
    }
    ######################
}
