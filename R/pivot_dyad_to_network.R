#' Pivot a dyadic variable to become the network
#'
#' `pivot_dyad_to_network` swaps a dyadic attribute with the main network in a netify object.
#' This is useful when you want to analyze a different relationship type that was stored
#' as a dyadic attribute. For example, if your network represents trade relationships
#' but you have FDI stored as a dyadic attribute, you can pivot to make FDI the main network.
#'
#' @param netlet A netify object containing the network and dyadic attributes
#' @param dyad_var Character string naming the dyadic variable to become the new network.
#'   Must exist in the netlet's dyad_data attribute.
#' @param make_network_dyad_var Logical. If TRUE (default), the current network
#'   will be preserved as a dyadic attribute.
#' @param network_var_name Character string specifying the name for the old network
#'   when converted to a dyadic attribute. If NULL (default), uses the current
#'   weight attribute name if available, otherwise "old_network".
#' @param symmetric Logical or NULL. Specifies whether the new network should be
#'   treated as symmetric. If NULL (default), attempts to detect from the dyadic
#'   variable's symmetry setting or data structure.
#' @param weight_type Character string describing the type of weight for the new
#'   network (e.g., "trade_volume", "fdi_amount"). If NULL (default), uses the
#'   dyad_var name.
#' @param diag_to_NA Logical. Whether to set diagonal values to NA in the new
#'   network. If NULL (default), inherits from the original netlet.
#' @param missing_to_zero Logical. Whether to treat missing values as zeros in
#'   the new network. If NULL (default), inherits from the original netlet.
#'
#' @return A netify object with the dyadic variable as the main network and
#'   (optionally) the old network preserved as a dyadic attribute. All other
#'   attributes and dyadic variables are preserved.
#'
#' @details
#' The function handles different netify types appropriately:
#' \itemize{
#'   \item For cross-sectional networks: performs a simple matrix swap
#'   \item For longitudinal arrays: swaps matrices across all time periods
#'   \item For longitudinal lists: swaps matrices for each time period
#'   \item For multilayer networks: swaps within each layer
#' }
#'
#' When the new network has different properties than the original (e.g., different
#' symmetry or weight type), the function updates the netify attributes accordingly.
#' For bipartite networks, the new network is always treated as asymmetric.
#'
#' If the dyadic variable was originally specified with symmetry information via
#' `add_dyad_vars()`, that information is used unless overridden by the symmetric
#' parameter.
#'
#' @examples
#' # Load example data
#' data(icews)
#'
#' # Create a netify object with verbal cooperation as the main network
#' # and material cooperation as a dyadic attribute
#' icews_10 <- icews[icews$year == 2010, ]
#'
#' net <- netify(
#'     icews_10,
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' net <- add_dyad_vars(
#'     net,
#'     icews_10,
#'     actor1 = "i", actor2 = "j",
#'     dyad_vars = "matlCoop",
#'     dyad_vars_symmetric = FALSE
#' )
#'
#' # Check the current network
#' print(net)
#'
#' # Pivot to make material cooperation the main network
#' net_pivoted <- pivot_dyad_to_network(
#'     net,
#'     dyad_var = "matlCoop",
#'     network_var_name = "verbCoop"
#' )
#'
#' # The main network is now material cooperation
#' print(net_pivoted)
#'
#' # The old network (verbal cooperation) is preserved as a dyadic attribute
#' attr(net_pivoted, "dyad_data")[["1"]][["verbCoop"]][1:5, 1:5]
#'
#' @author Shahryar Minhas
#'
#' @export pivot_dyad_to_network

pivot_dyad_to_network <- function(
    netlet,
    dyad_var,
    make_network_dyad_var = TRUE,
    network_var_name = NULL,
    symmetric = NULL,
    weight_type = NULL,
    diag_to_NA = NULL,
    missing_to_zero = NULL) {
    # validate inputs
    netify_check(netlet)
    checkmate::assert_string(dyad_var)
    checkmate::assert_logical(make_network_dyad_var, len = 1)
    checkmate::assert_string(network_var_name, null.ok = TRUE)
    checkmate::assert_logical(symmetric, len = 1, null.ok = TRUE)
    checkmate::assert_string(weight_type, null.ok = TRUE)
    checkmate::assert_logical(diag_to_NA, len = 1, null.ok = TRUE)
    checkmate::assert_logical(missing_to_zero, len = 1, null.ok = TRUE)

    # extract attributes from the netlet object
    netlet_attrs <- attributes(netlet)
    netlet_type <- netlet_attrs$netify_type

    # check if dyad_data exists in the netlet object
    if (is.null(netlet_attrs$dyad_data)) {
        cli::cli_abort("No dyadic variables found in the netify object. Use add_dyad_vars() first.")
    }

    # check if the specified dyad_var exists in the first time period
    first_time_data <- netlet_attrs$dyad_data[[1]]
    if (is.null(first_time_data[[dyad_var]])) {
        available_vars <- names(first_time_data)
        cli::cli_abort(c(
            "Dyadic variable '{dyad_var}' not found.",
            "i" = "Available dyadic variables: {paste(available_vars, collapse=', ')}"
        ))
    }

    # set default values for optional attributes if they are not provided
    if (is.null(diag_to_NA)) diag_to_NA <- netlet_attrs$diag_to_NA
    if (is.null(missing_to_zero)) missing_to_zero <- netlet_attrs$missing_to_zero
    if (is.null(weight_type)) weight_type <- dyad_var

    # determine the name for the network variable if needed
    if (make_network_dyad_var && is.null(network_var_name)) {
        if (!is.null(netlet_attrs$weight)) {
            network_var_name <- netlet_attrs$weight
        } else {
            network_var_name <- "old_network"
        }
    }

    # handle bipartite networks by forcing them to be asymmetric
    if (netlet_attrs$mode == "bipartite") {
        if (!is.null(symmetric) && symmetric) {
            cli::cli_warn("Bipartite networks must be asymmetric. Setting {.arg symmetric} = {.val FALSE}.")
        }
        symmetric <- FALSE
    } else {
        # for non-bipartite networks, detect symmetry if not explicitly provided
        if (is.null(symmetric)) {
            symmetric <- detect_dyad_symmetry(netlet, dyad_var)
            cli::cli_alert_info("Auto-detected symmetry for '{dyad_var}': {ifelse(symmetric, 'symmetric', 'asymmetric')}")
        }
    }

    # create a copy of the netlet object to modify
    new_netlet <- netlet
    new_attrs <- attributes(new_netlet)

    # perform the swap operation based on the type of netlet
    if (netlet_type == "cross_sec") {
        new_netlet <- swap_cross_sec(new_netlet, dyad_var, make_network_dyad_var, network_var_name)
    } else if (netlet_type == "longit_array") {
        new_netlet <- swap_longit_array(new_netlet, dyad_var, make_network_dyad_var, network_var_name)
    } else if (netlet_type == "longit_list") {
        new_netlet <- swap_longit_list(new_netlet, dyad_var, make_network_dyad_var, network_var_name)
    }

    # update attributes of the new netlet object
    attr(new_netlet, "weight") <- dyad_var
    attr(new_netlet, "symmetric") <- symmetric
    attr(new_netlet, "detail_weight") <- ifelse(
        weight_type == dyad_var,
        paste0("Weighted: ", weight_type),
        paste0("Weighted: ", weight_type, " (", dyad_var, ")")
    )
    attr(new_netlet, "diag_to_NA") <- diag_to_NA
    attr(new_netlet, "missing_to_zero") <- missing_to_zero

    # remove the dyad_var from dyad_data since it is now the main network
    dyad_data <- attr(new_netlet, "dyad_data")
    for (t in names(dyad_data)) {
        dyad_data[[t]][[dyad_var]] <- NULL
    }
    attr(new_netlet, "dyad_data") <- dyad_data

    # print success messages
    cli::cli_alert_success("Successfully pivoted '{dyad_var}' to be the main network")
    if (make_network_dyad_var) {
        cli::cli_alert_info("Previous network preserved as dyadic variable '{network_var_name}'")
    }

    #
    return(new_netlet)
}

#' Detect symmetry of a dyadic variable
#'
#' Internal function to detect whether a dyadic variable is symmetric
#' by checking if matrix[i,j] equals matrix[j,i] for all elements.
#'
#' @param netlet A netify object
#' @param dyad_var Name of the dyadic variable to check
#'
#' @return Logical indicating whether the variable is symmetric
#'
#' @keywords internal
#' @noRd
detect_dyad_symmetry <- function(netlet, dyad_var) {
    # get the first matrix for the dyadic variable
    first_matrix <- attr(netlet, "dyad_data")[[1]][[dyad_var]]

    # check if the matrix is square
    if (nrow(first_matrix) != ncol(first_matrix)) {
        return(FALSE)
    }

    # check if the matrix is symmetric
    is_symmetric <- isSymmetric(first_matrix, check.attributes = FALSE)

    return(is_symmetric)
}

#' Swap matrices for cross-sectional netify objects
#'
#' @param netlet Cross-sectional netify object
#' @param dyad_var Name of dyadic variable to become network
#' @param make_network_dyad_var Whether to preserve old network as dyadic variable
#' @param network_var_name Name for the old network when stored as dyadic variable
#'
#' @return Modified netify object
#' @keywords internal
#' @noRd
swap_cross_sec <- function(
    netlet, dyad_var,
    make_network_dyad_var, network_var_name) {
    # get the new network matrix from the dyadic variable
    new_network_matrix <- attr(netlet, "dyad_data")[["1"]][[dyad_var]]

    # if requested, save the old network as a dyadic variable
    if (make_network_dyad_var) {
        old_network_matrix <- netlet[, , drop = FALSE]
        attr(netlet, "dyad_data")[["1"]][[network_var_name]] <- old_network_matrix
    }

    # replace the network with the new matrix
    netlet[, ] <- new_network_matrix

    return(netlet)
}

#' Swap matrices for longitudinal array netify objects
#'
#' @param netlet Longitudinal array netify object
#' @param dyad_var Name of dyadic variable to become network
#' @param make_network_dyad_var Whether to preserve old network as dyadic variable
#' @param network_var_name Name for the old network when stored as dyadic variable
#'
#' @return Modified netify object
#' @keywords internal
#' @noRd
swap_longit_array <- function(
    netlet, dyad_var,
    make_network_dyad_var, network_var_name) {
    # check if the array has layers
    has_layers <- length(dim(netlet)) == 4

    if (has_layers) {
        # get the number of layers and time points
        n_layers <- dim(netlet)[3]
        n_time <- dim(netlet)[4]
        time_names <- dimnames(netlet)[[4]]

        # iterate over each time point
        for (t_idx in 1:n_time) {
            t_name <- time_names[t_idx]

            # get the new network matrix for the current time point
            new_network_matrix <- attr(netlet, "dyad_data")[[t_name]][[dyad_var]]

            # if requested, save the old network as a dyadic variable
            if (make_network_dyad_var) {
                old_network_array <- netlet[, , , t_idx, drop = FALSE]
                attr(netlet, "dyad_data")[[t_name]][[network_var_name]] <- old_network_array[, , 1, 1]
            }

            # replace the network for each layer with the new matrix
            for (l_idx in 1:n_layers) {
                netlet[, , l_idx, t_idx] <- new_network_matrix
            }
        }
    } else {
        # get the number of time points
        n_time <- dim(netlet)[3]
        time_names <- dimnames(netlet)[[3]]

        # iterate over each time point
        for (t_idx in 1:n_time) {
            t_name <- time_names[t_idx]

            # get the new network matrix for the current time point
            new_network_matrix <- attr(netlet, "dyad_data")[[t_name]][[dyad_var]]

            # if requested, save the old network as a dyadic variable
            if (make_network_dyad_var) {
                old_network_matrix <- netlet[, , t_idx]
                attr(netlet, "dyad_data")[[t_name]][[network_var_name]] <- old_network_matrix
            }

            # replace the network with the new matrix
            netlet[, , t_idx] <- new_network_matrix
        }
    }

    return(netlet)
}

#' Swap matrices for longitudinal list netify objects
#'
#' @param netlet Longitudinal list netify object
#' @param dyad_var Name of dyadic variable to become network
#' @param make_network_dyad_var Whether to preserve old network as dyadic variable
#' @param network_var_name Name for the old network when stored as dyadic variable
#'
#' @return Modified netify object
#' @keywords internal
#' @noRd
swap_longit_list <- function(
    netlet, dyad_var,
    make_network_dyad_var, network_var_name) {
    # get the names of the time periods
    time_periods <- names(netlet)

    # iterate over each time period
    for (t in time_periods) {
        # get the new network matrix for the current time period
        new_network_matrix <- attr(netlet, "dyad_data")[[t]][[dyad_var]]

        # if requested, save the old network as a dyadic variable
        if (make_network_dyad_var) {
            old_network_matrix <- netlet[[t]]
            attr(netlet, "dyad_data")[[t]][[network_var_name]] <- old_network_matrix
        }

        # preserve the attributes of the old network
        old_attrs <- attributes(netlet[[t]])

        # replace the network with the new matrix
        netlet[[t]][, ] <- new_network_matrix

        # restore the attributes to the updated network
        attributes(netlet[[t]]) <- old_attrs
    }

    return(netlet)
}
