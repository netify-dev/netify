#' Create multilayer networks from multiple netify objects
#'
#' `layer_netify` combines multiple netify
#' objects into a single multilayer network structure. Each input network becomes
#' a layer in the resulting multilayer network, enabling analysis of multiple
#' relationship types or network views simultaneously.
#'
#' @param netlet_list A list of netify objects to layer together. All objects must
#'   have compatible dimensions and attributes (see Details).
#' @param layer_labels Character vector specifying names for each layer. If NULL
#'   (default), uses names from netlet_list or generates generic labels ("layer1",
#'   "layer2", etc.). Length must match the number of netify objects.
#'
#' @return A multilayer netify object with structure depending on input type:
#'   \itemize{
#'     \item \strong{Cross-sectional input}: 3D array \code{[actors × actors × layers]}
#'     \item \strong{Longitudinal array input}: 4D array \code{[actors × actors × layers × time]}
#'     \item \strong{Longitudinal list input}: List of 3D arrays, one per time period
#'   }
#'
#'   The returned object maintains the netify class and includes:
#'   \itemize{
#'     \item Combined nodal attributes (if compatible)
#'     \item Combined dyadic attributes (if compatible)
#'     \item Layer information accessible via \code{attr(obj, 'layers')}
#'     \item All standard netify attributes
#'   }
#'
#' @details
#' \strong{Compatibility requirements:}
#'
#' All netify objects in netlet_list must have identical:
#' \itemize{
#'   \item Network type (cross-sectional or longitudinal)
#'   \item Dimensions (same actors and time periods)
#'   \item Symmetry (all directed or all undirected)
#'   \item Mode (all unipartite or all bipartite)
#'   \item Actor composition (same actors in same order)
#' }
#'
#' \strong{Attribute handling:}
#'
#' Nodal and dyadic attributes are combined when possible:
#' \itemize{
#'   \item If attributes are identical across layers, the first layer's attributes are used
#'   \item If attributes differ but have compatible structure, they are merged
#'   \item If attributes are incompatible, a warning is issued and attributes must be
#'     added manually using \code{add_node_vars()} or \code{add_dyad_vars()}
#' }
#'
#'
#' @note
#' Memory usage increases with the number of layers. For large networks with many
#' layers, consider whether all layers are necessary for your analysis.
#'
#'
#' @examples
#' # Load example data
#' data(icews)
#'
#' # Example 1: Cross-sectional multilayer network
#' icews_10 <- icews[icews$year == 2010, ]
#'
#' # Create separate networks for different interaction types
#' verbal_coop <- netify(
#'     icews_10,
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop",
#'     nodal_vars = c("i_log_gdp", "i_log_pop"),
#'     dyad_vars = "verbConf"
#' )
#'
#' material_coop <- netify(
#'     icews_10,
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "matlCoop",
#'     nodal_vars = "i_polity2",
#'     dyad_vars = "matlConf"
#' )
#'
#' # Layer them together
#' coop_multilayer <- layer_netify(
#'     netlet_list = list(verbal_coop, material_coop),
#'     layer_labels = c("Verbal", "Material")
#' )
#'
#' # Check structure
#' dim(get_raw(coop_multilayer)) # [actors × actors × 2]
#' attr(coop_multilayer, "layers") # "Verbal" "Material"
#'
#' \donttest{
#' # Example 2: Longitudinal multilayer (array format)
#' verbal_longit <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE,
#'     weight = "verbCoop",
#'     output_format = "longit_array"
#' )
#'
#' material_longit <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE,
#'     weight = "matlCoop",
#'     output_format = "longit_array"
#' )
#'
#' # Create longitudinal multilayer
#' longit_multilayer <- layer_netify(
#'     list(verbal_longit, material_longit),
#'     layer_labels = c("Verbal", "Material")
#' )
#'
#' dim(get_raw(longit_multilayer)) # [actors × actors × 2 × years]
#' }
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export layer_netify

layer_netify <- function(netlet_list, layer_labels = NULL) {
    # user input checks - use lapply instead of vapply since netify_check doesn't return a value
    lapply(netlet_list, netify_check)

    # set layer labels
    layer_labels <- set_layer_labels(netlet_list, layer_labels)

    # store attributes from netlet objects
    attribs_list <- lapply(netlet_list, attributes)

    # define relevant attribute types that must be identical
    rel_attrs <- c(
        "netify_type", "actor_time_uniform",
        "actor_pds", "symmetric", "mode"
    )

    # check if attributes compatible
    check_layer_compatible(
        a_list = attribs_list,
        elems = rel_attrs,
        msg = c(
            "Error: The ",
            " attribute is not identical across the netlets in `netlet_list`."
        )
    )

    # Extract first set of attributes for reuse
    first_attribs <- attribs_list[[1]]

    # generate weights value for multilayer case - optimized
    weight_collapse <- vapply(attribs_list, function(x) {
        w <- x[["weight"]]
        if (is.null(w)) {
            "NULL"
        } else if (is.logical(w)) {
            if (w) "edge_value" else "NULL"
        } else {
            as.character(w)
        }
    }, character(1))
    weight_collapse <- paste(weight_collapse, collapse = ", ")

    # do the same for longer weight label descriptors - optimized
    weight_label_collapse <- paste(
        vapply(attribs_list, `[[`, character(1), "detail_weight"),
        collapse = " | "
    )

    # pull out logical for whether we have binary weights - already optimized
    is_binary_vec <- vapply(attribs_list, `[[`, logical(1), "is_binary")

    # check to make sure that the networks can be layered
    msrmnts_list <- lapply(netlet_list, netify_measurements)

    # define relevant measurements that must be identical
    rel_msrs <- c(
        "row_actors", "col_actors", "time",
        "n_row_actors", "n_col_actors", "n_time"
    )

    # check if dimensions compatible
    check_layer_compatible(
        a_list = msrmnts_list,
        elems = rel_msrs,
        msg = c(
            "Error: ",
            " are not identical across the netlets in `netlet_list`."
        )
    )

    # Extract measurements once for reuse
    first_msrmnts <- msrmnts_list[[1]]
    netlet_type <- first_attribs$netify_type
    n_layers <- length(netlet_list)
    n_time <- first_msrmnts$n_time
    n_row_actors <- first_msrmnts$n_row_actors
    n_col_actors <- first_msrmnts$n_col_actors
    time <- first_msrmnts$time
    row_actors <- first_msrmnts$row_actors
    col_actors <- first_msrmnts$col_actors

    # pull out raw versions of data
    netlet_raws <- lapply(netlet_list, get_raw)

    # Create base attributes list to avoid repetition
    base_attrs <- list(
        netify_type = netlet_type,
        actor_time_uniform = first_attribs$actor_time_uniform,
        actor_pds = first_attribs$actor_pds,
        weight = weight_collapse,
        detail_weight = weight_label_collapse,
        is_binary = is_binary_vec,
        symmetric = first_attribs$symmetric,
        mode = first_attribs$mode,
        layers = layer_labels,
        diag_to_NA = get_attribs(attribs_list, "diag_to_NA"),
        missing_to_zero = get_attribs(attribs_list, "missing_to_zero"),
        sum_dyads = get_attribs(attribs_list, "sum_dyads"),
        nodal_data = NULL,
        dyad_data = NULL
    )

    # cross-sec case
    if (netlet_type == "cross_sec") {
        # define and fill array efficiently
        netlet <- array(NA,
            dim = c(n_row_actors, n_col_actors, n_layers),
            dimnames = list(row_actors, col_actors, layer_labels)
        )

        for (ii in seq_len(n_layers)) {
            netlet[, , ii] <- netlet_raws[[ii]]
        }
    }

    # array case
    if (netlet_type == "longit_array") {
        # define and fill array efficiently
        netlet <- array(NA,
            dim = c(n_row_actors, n_col_actors, n_layers, n_time),
            dimnames = list(row_actors, col_actors, layer_labels, time)
        )

        for (ii in seq_len(n_layers)) {
            netlet[, , ii, ] <- netlet_raws[[ii]]
        }
    }

    # longit list case - optimized
    if (netlet_type == "longit_list") {
        # Pre-allocate list
        netlet <- vector("list", n_time)
        names(netlet) <- time

        # Create cross-sectional attributes once
        cs_attrs <- base_attrs
        cs_attrs$netify_type <- "cross_sec"
        cs_attrs$actor_time_uniform <- NULL
        cs_attrs$actor_pds <- NULL

        for (tt in seq_len(n_time)) {
            # define array
            arr <- array(NA,
                dim = c(n_row_actors[[tt]], n_col_actors[[tt]], n_layers),
                dimnames = list(row_actors[[tt]], col_actors[[tt]], layer_labels)
            )

            # fill in array
            for (ii in seq_len(n_layers)) {
                arr[, , ii] <- netlet_raws[[ii]][[tt]]
            }

            # add attributes efficiently
            class(arr) <- "netify"
            attributes(arr) <- c(attributes(arr), cs_attrs)

            netlet[[tt]] <- arr
        }
    }

    # add attributes to main object
    class(netlet) <- "netify"
    attributes(netlet) <- c(attributes(netlet), base_attrs)

    # Handle nodal data - optimized checks
    nodal_data_list <- lapply(attribs_list, `[[`, "nodal_data")
    if (identical_recursive(nodal_data_list)) {
        attr(netlet, "nodal_data") <- nodal_data_list[[1]]
    } else {
        attr(netlet, "nodal_data") <- reduce_combine_nodal_attr(
            attribs_list, msrmnts_list, netlet_type
        )
    }

    # Handle dyad data - optimized checks
    dyad_data_list <- lapply(attribs_list, `[[`, "dyad_data")
    if (identical_recursive(dyad_data_list)) {
        attr(netlet, "dyad_data") <- dyad_data_list[[1]]
    } else {
        attr(netlet, "dyad_data") <- reduce_combine_dyad_attr(
            attribs_list, msrmnts_list, netlet_type
        )
    }

    return(netlet)
}
