#' Helper function for layer_netify to set layer labels
#'
#' @param netlet_list list of netlet objects
#' @param layer_labels character vector of layer labels
#' @return character vector of layer labels
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

set_layer_labels <- function(netlet_list, layer_labels) {
    # if provided make sure the number of labels
    # are the same as the length of the netlet list obj
    if (!is.null(layer_labels)) {
        if (length(layer_labels) != length(netlet_list)) {
            cli::cli_alert_danger(
                "Error: The number of layer labels must be
                the same as the number of netlets in `netlet_list`."
            )
            stop()
        }
    }

    # if layer labels not present see if we can pull them from
    # the list object
    if (is.null(layer_labels)) {
        layer_labels <- names(netlet_list) %||% paste0("layer", seq_along(netlet_list))
    }

    #
    return(layer_labels)
}

#' Cycle through elements of a netlet object and make sure
#' that they are identical before we try to merge into
#' a multilayer netify object
#'
#' @param a_list list object to check
#' @param elems character vector with names of elements to check
#' @param msg character vector of length two warning user
#' if elements are found to be not identical. First part of
#' vector is the preamble before the element name and
#' second part is what should come after the element name.
#' @return NULL
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

check_layer_compatible <- function(a_list, elems, msg) {
    # lets cycle through the elements one at a time so users
    # can be told, if relevant, which element is not identical
    for (elem in elems) {
        # check if that attribute is identical across layers
        elem_check <- identical_recursive(
            lapply(a_list, `[[`, elem)
        )

        # if not identical then throw error and stop
        if (!elem_check) {
            cli::cli_alert_danger(
                paste0(msg[1], elem, msg[2])
            )
            stop()
        }
    }
}

#' Helper function for layer_netify to extract attributes
#' from listed netlet objects
#'
#' @param a_list named list object
#' @param attrib character string of attribute to extract
#' @param list_format logical, if TRUE return list of attributes
#' @param get_unique logical, if TRUE return unique values of attribute
#' @return attribute values
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

get_attribs <- function(
    a_list, attrib,
    list_format = FALSE, get_unique = FALSE) {
    out <- lapply(a_list, `[[`, attrib)
    if (list_format) {
        return(out)
    }
    if (!list_format) {
        out <- unlist(out, use.names = FALSE)
        if (get_unique) {
            return(unique(out))
        } else {
            return(out)
        }
    }
}

#' Reduce and combine multiple nodal attributes of netify objects
#' into a single nodal attribute. Mainly for use within the
#' layer_netify function
#'
#' @param attribs_list list of attributes from each netlet
#' @param msrmnts_list list of msrmnts from each netlet
#' @param netlet_type character string of netlet type
#' @return nodal attribute data.frame
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

reduce_combine_nodal_attr <- function(
    attribs_list, msrmnts_list, netlet_type) {
    # extract nodal data from each netlet into list
    nodal_data_list <- lapply(attribs_list, `[[`, "nodal_data")

    # check for and then drop any null elements - vectorized
    nodal_data_list <- nodal_data_list[!vapply(nodal_data_list, is.null, logical(1))]

    # if only one element left then just
    # return that one nodal data element
    if (length(nodal_data_list) == 1) {
        return(nodal_data_list[[1]])
    }

    # if more than one element left then try and combine
    if (length(nodal_data_list) > 1) {
        # get a list of the vars across the netlets
        nvars <- get_attribs(msrmnts_list, "nvars", get_unique = TRUE)

        # check to make sure that the id column(s) are identical
        # if columns are not identical then return NULL with
        # warning that user needs to readd themselves
        n_id_check <- if (netlet_type == "cross_sec") {
            identical_recursive(lapply(nodal_data_list, function(x) x[, 1]))
        } else {
            identical_recursive(lapply(nodal_data_list, function(x) x[, 1:2]))
        }

        if (!n_id_check) {
            cli::cli_alert_warning(
                "Warning: Nodal data id columns are not identical across netlets,
                nodal data will not be merged from netlets, you can readd nodal
                attributes manually using the `add_nodal_data` function."
            )
            return(NULL)
        }

        # if id columns match then iteratively go through
        # nodal data and cbind together
        if (n_id_check) {
            # Pre-filter nvars for each slice to avoid repeated intersections
            ndata_list <- lapply(nodal_data_list, function(slice) {
                nvars_slice <- names(slice)[names(slice) %in% nvars]
                slice[, nvars_slice, drop = FALSE]
            })

            ndata <- do.call("cbind", ndata_list)

            if (netlet_type == "cross_sec") {
                ndata <- cbind(actor = nodal_data_list[[1]][, 1], ndata)
            } else {
                ndata <- cbind(nodal_data_list[[1]][, 1:2], ndata)
            }
            return(ndata)
        }
    }
}

#' Reduce and combine multiple dyadic attributes of netify objects
#' into a single dyadic attribute. Mainly for use within the
#' layer_netify function
#'
#' @param attribs_list list of attributes from each netlet
#' @param msrmnts_list list of msrmnts from each netlet
#' @param netlet_type character string of netlet type
#' @return dyad attribute data.frame
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

reduce_combine_dyad_attr <- function(
    attribs_list, msrmnts_list, netlet_type) {
    # extract dyad data from each netlet into list
    dyad_data_list <- lapply(attribs_list, `[[`, "dyad_data")

    # check for and then drop any null elements - already optimized
    null_check <- vapply(dyad_data_list, function(x) {
        is.null(x) || is.null(x[[1]])
    }, logical(1))
    dyad_data_list <- dyad_data_list[!null_check]

    # if only one element left then just
    # return that one dyad data element
    if (length(dyad_data_list) == 1) {
        return(dyad_data_list[[1]])
    }

    # if more than one element left then try and combine
    if (length(dyad_data_list) > 1) {
        # get a list of the vars across the netlets
        dvars <- get_attribs(msrmnts_list, "dvars", get_unique = TRUE)

        # check to make sure that the id row/col and time periods are
        # identical across dyad_data from netlets
        # For new structure: list(time) -> list(vars) -> matrix
        t_check <- identical_recursive(lapply(dyad_data_list, names))

        # Extract first time period's first variable matrix once
        first_elements <- lapply(dyad_data_list, function(x) {
            first_time <- x[[1]]
            if (length(first_time) > 0) {
                list(
                    rows = rownames(first_time[[1]]),
                    cols = colnames(first_time[[1]])
                )
            } else {
                list(rows = NULL, cols = NULL)
            }
        })

        r_check <- identical_recursive(lapply(first_elements, `[[`, "rows"))
        c_check <- identical_recursive(lapply(first_elements, `[[`, "cols"))

        if (!all(c(t_check, r_check, c_check))) {
            cli::cli_alert_warning(
                "Warning: Dyad data id columns are not identical across netlets,
                dyad data will not be merged from netlets, you can readd dyad
                attributes manually using the `add_dyad` function."
            )
            return(NULL)
        }

        # if id columns match then iteratively go through
        # dyad data and combine matrices
        if (all(c(t_check, r_check, c_check))) {
            # get time periods from first dyad_data element
            t_pds <- names(dyad_data_list[[1]])

            # Pre-allocate list
            ddata <- vector("list", length(t_pds))
            names(ddata) <- t_pds

            # process each time period more efficiently
            for (tt_idx in seq_along(t_pds)) {
                tt <- t_pds[tt_idx]

                # Pre-allocate combined_vars with known size
                combined_vars <- list()

                # iterate through each netlet's dyad data for this time period
                for (netlet_dyad_data in dyad_data_list) {
                    time_period_data <- netlet_dyad_data[[tt]]

                    if (!is.null(time_period_data) && length(time_period_data) > 0) {
                        # Direct assignment is faster than repeated list operations
                        for (var_name in names(time_period_data)) {
                            if (var_name %in% dvars) {
                                combined_vars[[var_name]] <- time_period_data[[var_name]]
                            }
                        }
                    }
                }

                # store combined variables for this time period
                ddata[[tt_idx]] <- combined_vars
            }

            #
            return(ddata)
        }
    }
}
