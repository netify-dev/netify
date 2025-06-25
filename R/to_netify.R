#' Convert igraph, network, or matrix objects to netify format
#'
#' Converts various network object types (igraph, network, matrices/arrays,
#' or lists of these) into netify objects (also available as `to_netify`).
#' Automatically extracts adjacency
#' matrices and any nodal/dyadic attributes from the input objects.
#'
#' @param net_obj An R object to convert: \code{igraph}, \code{network},
#'   matrix, array, or a list of these objects.
#' @param weight Optional. Name of the weight attribute in \code{net_obj} to be used
#'   as the main edge weight in the netify object. Default is \code{NULL}. Important to specify
#'   for \code{igraph} and \code{network} objects as they do not have a default weight.
#' @param ... Additional arguments passed to \code{new_netify}. Can include
#'   \code{nodal_data} or \code{dyad_data} to override extracted attributes.
#'
#' @return A netify object with:
#'   \itemize{
#'     \item Adjacency matrix or list of matrices
#'     \item Nodal attributes (if present in the input)
#'     \item Dyadic attributes (if present in the input)
#'     \item Weight specification (if provided)
#'   }
#'
#' @details
#' The function handles different input types:
#' \itemize{
#'   \item \strong{igraph}: Extracts adjacency matrix, vertex attributes as nodal data,
#'     and edge attributes as dyadic data
#'   \item \strong{network}: Extracts adjacency matrix, vertex attributes as nodal data,
#'     and edge attributes as dyadic data
#'   \item \strong{matrix}: Direct conversion, no attribute extraction
#'   \item \strong{array}: Assumes 3D arrays represent longitudinal networks
#'   \item \strong{list}: Must contain all objects of the same type (all igraph,
#'     all network, or all matrices)
#' }
#'
#' For longitudinal data (lists or 3D arrays), the function creates a netify object
#' with time-indexed components. Actor ordering is preserved from the input objects
#' and made consistent across all components (adjacency, nodal, and dyadic data).
#'
#' @note
#' When converting from igraph or network objects, specify the \code{weight} parameter
#' to designate which edge attribute should be used as the primary edge weight in
#' the netify object.
#'
#' @examples
#' \dontrun{
#' # From igraph
#' library(igraph)
#' g <- sample_gnp(10, 0.3)
#' E(g)$weight <- runif(ecount(g))
#' V(g)$type <- sample(c("A", "B"), vcount(g), replace = TRUE)
#'
#' net <- to_netify(g, weight = "weight")
#'
#' # From network
#' library(network)
#' n <- network(rgraph(10, tprob = 0.3))
#' set.vertex.attribute(n, "group", sample(1:2, 10, replace = TRUE))
#'
#' net <- to_netify(n)
#'
#' # From matrix
#' adj_mat <- matrix(rnorm(100), 10, 10)
#' net <- to_netify(adj_mat)
#'
#' # From list of matrices (longitudinal)
#' mat_list <- list(
#'     "2001" = matrix(rnorm(100), 10, 10),
#'     "2002" = matrix(rnorm(100), 10, 10)
#' )
#' net <- to_netify(mat_list)
#' }
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export to_netify

to_netify <- function(
    net_obj,
    weight = NULL,
    ...) {
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
    is_longitudinal <- FALSE
    is_symmetric <- NULL # track symmetry for dyad data conversion

    # single igraph object
    if (inherits(net_obj, "igraph")) {
        processed <- decompose_igraph(net_obj, weight = weight)
        adj_data <- processed$adj_mat
        nodal_data <- processed$ndata
        dyad_data <- processed$ddata
        weight <- processed$weight
        # get symmetry directly from igraph
        is_symmetric <- !igraph::is_directed(net_obj)

        # single network object
    } else if (inherits(net_obj, "network")) {
        processed <- decompose_statnet(net_obj, weight = weight)
        adj_data <- processed$adj_mat
        nodal_data <- processed$ndata
        dyad_data <- processed$ddata
        weight <- processed$weight
        # get symmetry directly from network
        is_symmetric <- !network::is.directed(net_obj)

        # single matrix
    } else if (is.matrix(net_obj)) {
        adj_data <- net_obj
        # for matrix, we'll check symmetry later if needed

        # array (3d for longitudinal)
    } else if (is.array(net_obj)) {
        adj_data <- net_obj
        is_longitudinal <- length(dim(net_obj)) == 3

        # list - need to check what type
    } else if (is.list(net_obj) && length(net_obj) > 0) {
        # determine list type and process accordingly
        list_result <- process_network_list(net_obj, weight)

        adj_data <- list_result$adj_data
        nodal_data <- list_result$nodal_data
        dyad_data <- list_result$dyad_data
        weight <- list_result$weight
        is_symmetric <- list_result$is_symmetric
        is_longitudinal <- list_result$is_longitudinal

        # unsupported type
    } else {
        cli::cli_alert_danger(
            "Error: Unsupported class for `net_obj`. Supported classes are: matrix, array, igraph, network, or a list of these objects."
        )
        stop()
    }

    # convert dyad_data to the new format if it exists
    if (!is.null(dyad_data)) {
        # for matrix/array cases where symmetry wasn't determined, check now
        if (is.null(is_symmetric)) {
            first_mat <- if (is.list(adj_data)) {
                adj_data[[1]]
            } else {
                adj_data
            }
            is_symmetric <- isSymmetric(first_mat)
        }

        dyad_data <- convert_dyad_data_to_new_format(
            dyad_data,
            adj_data,
            is_longitudinal,
            is_symmetric
        )
    }

    # validate user-provided nodal_data if given
    if (!is.null(user_ndata)) {
        validate_nodal_data(user_ndata, adj_data, is_longitudinal)
        nodal_data <- user_ndata
    }

    # validate user-provided dyad_data if given
    if (!is.null(user_ddata)) {
        validate_dyad_data(user_ddata, adj_data, is_longitudinal)
        dyad_data <- user_ddata
    }

    # ensure consistent actor ordering across all data components
    if (!is.null(nodal_data)) {
        nodal_data <- align_nodal_data_order(
            nodal_data,
            adj_data,
            is_longitudinal
        )
    }

    if (!is.null(dyad_data)) {
        dyad_data <- align_dyad_data_order(
            dyad_data,
            adj_data,
            is_longitudinal
        )
    }

    # build final netify object
    out <- new_netify(
        data = adj_data,
        weight = weight,
        nodal_data = nodal_data,
        dyad_data = dyad_data,
        ...
    )

    # return the netify object
    return(out)
}

#' Process a list of network objects
#'
#' Handles the processing of lists containing igraph, network,
#' or matrix objects.
#'
#' @param net_obj A list of network objects (igraph, network, or matrix)
#' @param weight Optional weight attribute name
#'
#' @return A list containing processed components
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
process_network_list <- function(net_obj, weight = NULL) {
    # initialize return values
    result <- list(
        adj_data = NULL,
        nodal_data = NULL,
        dyad_data = NULL,
        weight = weight,
        is_symmetric = NULL,
        is_longitudinal = TRUE
    )

    # check list type and process
    if (all(vapply(net_obj, inherits, logical(1), "igraph"))) {
        # process igraph list
        processed_list <- lapply(net_obj, decompose_igraph, weight = weight)
        result$is_symmetric <- !igraph::is_directed(net_obj[[1]])
    } else if (all(vapply(net_obj, inherits, logical(1), "network"))) {
        # process network list
        processed_list <- lapply(net_obj, decompose_statnet, weight = weight)
        result$is_symmetric <- !network::is.directed(net_obj[[1]])
    } else if (all(vapply(net_obj, is.matrix, logical(1)))) {
        # list of matrices - simple case
        result$adj_data <- net_obj
        return(result)
    } else {
        cli::cli_alert_danger(
            "Error: List must contain all matrices, all igraph objects, or all network objects."
        )
        stop()
    }

    # extract components from processed list
    result$adj_data <- lapply(processed_list, `[[`, "adj_mat")
    result$weight <- processed_list[[1]]$weight

    # process nodal data for longitudinal networks
    result$nodal_data <- process_longitudinal_nodal_data(
        processed_list,
        net_obj
    )

    # extract dyad data
    result$dyad_data <- lapply(processed_list, `[[`, "ddata")

    #
    return(result)
}

#' Process nodal data for longitudinal networks
#'
#' @param processed_list List of processed network decompositions
#' @param net_obj Original network object list (for time names)
#'
#' @return A combined data frame of nodal data or NULL
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
process_longitudinal_nodal_data <- function(processed_list, net_obj) {
    nodal_data_list <- lapply(seq_along(processed_list), function(i) {
        nd <- processed_list[[i]]$ndata
        if (!is.null(nd)) {
            # add time column
            nd$time <- names(net_obj)[i] %||% as.character(i)

            # rename 'name' to 'actor' for consistency if needed
            if ("name" %in% names(nd) && !"actor" %in% names(nd)) {
                names(nd)[names(nd) == "name"] <- "actor"
            }

            # if actor column doesn't exist, create it
            if (!"actor" %in% names(nd)) {
                nd$actor <- rownames(nd) %||% as.character(seq_len(nrow(nd)))
            }
        }
        return(nd)
    })

    # combine all non-null nodal data
    nodal_data_list <- nodal_data_list[!sapply(nodal_data_list, is.null)]
    if (length(nodal_data_list) > 0) {
        nodal_data <- do.call(rbind, nodal_data_list)
        rownames(nodal_data) <- NULL
        return(nodal_data)
    } else {
        return(NULL)
    }
}

#' Align nodal data actor order to match adjacency matrix order
#'
#' Ensure that actors in nodal_data appear in the same
#' order as they appear in the adjacency matrix row/column names.
#'
#' @param ndata A data frame containing nodal attributes with 'actor' column
#' @param adj_data An adjacency matrix or list of adjacency matrices
#' @param is_longitudinal Logical indicating whether the network is longitudinal
#'
#' @return A reordered data frame with actors matching adjacency matrix order
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
align_nodal_data_order <- function(ndata, adj_data, is_longitudinal) {
    # get reference actor order from adjacency matrix
    first_mat <- if (is.list(adj_data)) {
        adj_data[[1]]
    } else {
        adj_data
    }
    ref_actors <- rownames(first_mat)

    # should always have names now, but keep fallback
    if (is.null(ref_actors)) {
        ref_actors <- paste0("a", seq_len(nrow(first_mat)))
    }

    if (is_longitudinal) {
        # use split for better performance than repeated subsetting
        ndata_split <- split(ndata, ndata$time)

        # reorder within each time period
        ndata_ordered <- do.call(rbind, lapply(ndata_split, function(time_data) {
            time_data[match(ref_actors, time_data$actor), ]
        }))

        # reset row names
        rownames(ndata_ordered) <- NULL
    } else {
        # cross-sectional: simple reordering
        ndata_ordered <- ndata[match(ref_actors, ndata$actor), ]
        rownames(ndata_ordered) <- NULL
    }

    #
    return(ndata_ordered)
}

#' Align dyad data matrix order to match adjacency matrix order
#'
#' Ensures that row and column orders in dyad_data matrices
#' match the order of actors in the adjacency matrix.
#'
#' @param ddata A nested list structure containing dyadic attribute matrices
#' @param adj_data An adjacency matrix or list of adjacency matrices
#' @param is_longitudinal Logical indicating whether the network is longitudinal
#'
#' @return A reordered nested list with matrix dimensions matching adjacency matrix order
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
align_dyad_data_order <- function(ddata, adj_data, is_longitudinal) {
    # for longitudinal data, we need to align each time period separately
    if (is_longitudinal) {
        ddata_ordered <- vector("list", length(ddata))
        names(ddata_ordered) <- names(ddata)

        for (t_idx in seq_along(ddata)) {
            t <- names(ddata)[t_idx]

            # get the correct adjacency matrix for this time period
            current_mat <- adj_data[[t_idx]]

            # align this time period's data
            ddata_ordered[[t]] <- align_single_time_dyad_data(
                ddata[[t]],
                current_mat
            )
        }
    } else {
        # cross-sectional case
        ddata_ordered <- vector("list", length(ddata))
        names(ddata_ordered) <- names(ddata)

        # process each time period (should just be "1")
        for (t in names(ddata)) {
            ddata_ordered[[t]] <- align_single_time_dyad_data(
                ddata[[t]],
                adj_data
            )
        }
    }

    #
    return(ddata_ordered)
}

#' Align dyad data for a single time period
#'
#'
#' @param time_data List of matrices for a single time period
#' @param ref_mat Reference adjacency matrix for actor ordering
#'
#' @return Aligned list of matrices
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
align_single_time_dyad_data <- function(time_data, ref_mat) {
    ref_actors <- rownames(ref_mat)
    n_actors <- nrow(ref_mat)

    if (is.null(ref_actors)) {
        ref_actors <- paste0("a", seq_len(n_actors))
    }

    aligned_data <- vector("list", length(time_data))
    names(aligned_data) <- names(time_data)

    # process each variable
    for (var in names(time_data)) {
        mat <- time_data[[var]]

        # get current actor names
        curr_rows <- rownames(mat)
        curr_cols <- colnames(mat)

        # if no names, assume they're in order 1:n
        if (is.null(curr_rows)) {
            curr_rows <- as.character(seq_len(nrow(mat)))
        }
        if (is.null(curr_cols)) {
            curr_cols <- as.character(seq_len(ncol(mat)))
        }

        # reorder to match reference actors
        row_idx <- match(ref_actors, curr_rows)
        col_idx <- match(ref_actors, curr_cols)

        # create reordered matrix
        mat_ordered <- mat[row_idx, col_idx, drop = FALSE]
        dimnames(mat_ordered) <- list(ref_actors, ref_actors)

        aligned_data[[var]] <- mat_ordered
    }

    #
    return(aligned_data)
}

#' Convert edge data from igraph/network to new dyad_data format
#'
#' Convert edge data extracted from igraph or network objects into
#' standardized dyad_data format used by netify. The output format is a
#' nested list structure: time periods -> variables -> matrices.
#'
#' @param edge_data A data frame (cross-sectional) or list of data frames (longitudinal)
#' containing edge attributes with 'from' and 'to' columns
#' @param adj_data An adjacency matrix (cross-sectional) or list of adjacency matrices
#' (longitudinal) used to determine dimensions and actor names
#' @param is_longitudinal Logical indicating whether the network is longitudinal
#' @param is_symmetric Logical indicating whether the network is symmetric/undirected
#'
#' @return A list with the following structure:
#' \itemize{
#'   \item For cross-sectional: list("1" = list(var1 = matrix, var2 = matrix, ...))
#'   \item For longitudinal: list(time1 = list(var1 = matrix, ...), time2 = list(...), ...)
#' }
#' Returns NULL if edge_data is NULL.
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
convert_dyad_data_to_new_format <- function(
    edge_data,
    adj_data,
    is_longitudinal,
    is_symmetric = FALSE) {
    # handle null case
    if (is.null(edge_data)) {
        return(NULL)
    }

    # get time periods
    if (is_longitudinal) {
        time_periods <- names(adj_data) %||% as.character(seq_along(adj_data))
    } else {
        time_periods <- "1"
    }

    # pre-allocate the result structure
    dyad_data_new <- vector("list", length(time_periods))
    names(dyad_data_new) <- time_periods

    # process each time period
    for (t_idx in seq_along(time_periods)) {
        t <- time_periods[t_idx]

        # get the appropriate adjacency matrix for this time period
        if (is_longitudinal) {
            current_mat <- adj_data[[t_idx]]
            edge_df <- if (is.list(edge_data)) {
                edge_data[[t_idx]]
            } else {
                edge_data
            }
        } else {
            current_mat <- adj_data
            edge_df <- edge_data
        }

        # process this time period's edge data
        dyad_data_new[[t]] <- process_single_time_edge_data(
            edge_df,
            current_mat,
            is_symmetric
        )
    }

    #
    return(dyad_data_new)
}

#' Process edge data for a single time period
#'
#'
#' @param edge_df Edge data frame with 'from' and 'to' columns
#' @param current_mat Adjacency matrix for dimensions and actor names
#' @param is_symmetric Whether the network is symmetric/undirected
#'
#' @return List of matrices for each edge variable
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
process_single_time_edge_data <- function(edge_df, current_mat, is_symmetric) {
    # skip if no edge data
    if (is.null(edge_df) || nrow(edge_df) == 0) {
        return(list())
    }

    # get dimensions and actor names
    n_actors <- nrow(current_mat)
    actor_names <- rownames(current_mat)

    # if no actor names, create default ones
    if (is.null(actor_names)) {
        actor_names <- paste0("a", seq_len(n_actors))
    }

    # get variable names (excluding from/to columns)
    var_names <- setdiff(names(edge_df), c("from", "to"))

    # if no variables, return empty list
    if (length(var_names) == 0) {
        return(list())
    }

    # create actor lookup for fast indexing
    actor_lookup <- seq_len(n_actors)
    names(actor_lookup) <- actor_names

    # find valid edges
    from_indices <- actor_lookup[as.character(edge_df$from)]
    to_indices <- actor_lookup[as.character(edge_df$to)]
    valid_edges <- which(!is.na(from_indices) & !is.na(to_indices))

    # create result list
    result <- vector("list", length(var_names))
    names(result) <- var_names

    #
    if (length(valid_edges) > 0) {
        # extract valid indices
        valid_from <- from_indices[valid_edges]
        valid_to <- to_indices[valid_edges]

        # create matrices for each variable
        for (var in var_names) {
            result[[var]] <- create_edge_matrix(
                edge_df[[var]][valid_edges],
                valid_from,
                valid_to,
                n_actors,
                actor_names,
                is_symmetric
            )
        }
    } else {
        # no valid edges - create zero matrices
        for (var in var_names) {
            mat <- array(0, dim = c(n_actors, n_actors))
            dimnames(mat) <- list(actor_names, actor_names)
            result[[var]] <- mat
        }
    }

    #
    return(result)
}

#' Create edge attribute matrix
#'
#'
#' @param values Edge attribute values
#' @param from_idx From indices
#' @param to_idx To indices
#' @param n_actors Number of actors
#' @param actor_names Actor names for dimnames
#' @param is_symmetric Whether to symmetrize the matrix
#'
#' @return Matrix of edge attributes
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
create_edge_matrix <- function(
    values,
    from_idx,
    to_idx,
    n_actors,
    actor_names,
    is_symmetric) {
    # initialize matrix
    mat <- array(0, dim = c(n_actors, n_actors))

    # create index matrix
    if (is_symmetric) {
        # for symmetric networks, set both directions
        edge_idx <- cbind(
            c(from_idx, to_idx),
            c(to_idx, from_idx)
        )
        mat[edge_idx] <- rep(values, 2)
    } else {
        # for directed networks, set one direction only
        edge_idx <- cbind(from_idx, to_idx)
        mat[edge_idx] <- values
    }

    # add dimnames
    dimnames(mat) <- list(actor_names, actor_names)

    #
    return(mat)
}

#' Validate nodal data structure for netify objects
#'
#' Validates that user-provided nodal data meets requirements for netify objects.
#' Specifically, it checks for proper data frame structure,
#' required columns, and consistency with the network's actors.
#'
#' @param ndata A data frame containing nodal attributes
#' @param adj_data An adjacency matrix (cross-sectional) or list of adjacency
#' matrices (longitudinal) to check actor consistency
#' @param is_longitudinal Logical indicating whether the network is longitudinal
#'
#' @return NULL (invisibly). Function stops with error if validation fails.
#'
#' @details
#' The function performs the following checks:
#' \itemize{
#'   \item ndata must be a data.frame
#'   \item ndata must contain an 'actor' column
#'   \item For longitudinal networks, ndata must contain a 'time' column
#'   \item Warns if actors in the network are missing from nodal_data
#' }
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
validate_nodal_data <- function(ndata, adj_data, is_longitudinal) {
    if (!is.data.frame(ndata)) {
        cli::cli_alert_danger("nodal_data must be a data.frame")
        stop()
    }

    # check for required columns
    if (!("actor" %in% names(ndata))) {
        cli::cli_alert_danger("nodal_data must contain an 'actor' column")
        stop()
    }

    if (is_longitudinal && !("time" %in% names(ndata))) {
        cli::cli_alert_danger(
            "For longitudinal networks, nodal_data must contain a 'time' column"
        )
        stop()
    }

    # check actor names match
    first_mat <- if (is.list(adj_data)) {
        adj_data[[1]]
    } else {
        adj_data
    }
    actor_names <- rownames(first_mat)

    if (!is.null(actor_names)) {
        missing_actors <- setdiff(actor_names, ndata$actor)
        if (length(missing_actors) > 0) {
            cli::cli_alert_warning(
                "Some actors in the network are missing from nodal_data: {missing_actors}"
            )
        }
    }
}

#' Validate dyad data structure for netify objects
#'
#' Validate that user-provided dyad data meets the
#' requirements for netify objects. It checks for proper nested list structure,
#' matrix dimensions, and consistency with the network's time periods.
#'
#' @param ddata A nested list structure containing dyadic attributes
#' @param adj_data An adjacency matrix (cross-sectional) or list of adjacency
#' matrices (longitudinal) to check dimensions and time consistency
#' @param is_longitudinal Logical indicating whether the network is longitudinal
#'
#' @return NULL (invisibly). Function stops with error if validation fails.
#'
#' @details
#' The function validates the following structure:
#' \itemize{
#'   \item ddata must be a list
#'   \item Each time period must be a list of matrices
#'   \item Each matrix must have dimensions matching the number of actors
#'   \item Warns if expected time periods are missing from dyad_data
#' }
#'
#' Expected structure:
#' \itemize{
#'   \item Cross-sectional: list("1" = list(var1 = matrix, var2 = matrix, ...))
#'   \item Longitudinal: list(time1 = list(var1 = matrix, ...), time2 = list(...), ...)
#' }
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
validate_dyad_data <- function(ddata, adj_data, is_longitudinal) {
    if (!is.list(ddata)) {
        cli::cli_alert_danger("dyad_data must be a list")
        stop()
    }

    # get expected time periods
    if (is_longitudinal) {
        expected_times <- names(adj_data) %||% as.character(seq_along(adj_data))
    } else {
        expected_times <- "1"
    }

    # check time periods
    ddata_times <- names(ddata)
    missing_times <- setdiff(expected_times, ddata_times)
    if (length(missing_times) > 0) {
        cli::cli_alert_warning(
            "Some time periods are missing from dyad_data: {missing_times}"
        )
    }

    # validate each time period's structure
    for (t_idx in seq_along(ddata_times)) {
        t <- ddata_times[t_idx]
        time_data <- ddata[[t]]

        # get expected number of actors for this time period
        if (t %in% expected_times) {
            if (is_longitudinal) {
                t_idx_adj <- which(expected_times == t)
                n_actors <- nrow(adj_data[[t_idx_adj]])
            } else {
                n_actors <- nrow(adj_data)
            }
        } else {
            next # skip validation for unexpected time periods
        }

        # validate time period structure
        validate_single_time_dyad_data(time_data, t, n_actors)
    }
}

#' Validate dyad data for a single time period
#'
#' Validate the structure and dimensions of dyad data for a single time period.
#'
#' @param time_data List of matrices for a single time period
#' @param t Time period label
#' @param n_actors Expected number of actors
#'
#' @return NULL (invisibly). Stops with error if validation fails.
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
validate_single_time_dyad_data <- function(time_data, t, n_actors) {
    if (!is.list(time_data)) {
        cli::cli_alert_danger(
            "Each time period in dyad_data must be a list of matrices"
        )
        stop()
    }

    # check each variable
    for (var in names(time_data)) {
        mat <- time_data[[var]]

        if (!is.matrix(mat)) {
            cli::cli_alert_danger(
                "dyad_data[['{t}']][['{var}']] must be a matrix"
            )
            stop()
        }

        mat_dims <- dim(mat)
        if (mat_dims[1] != n_actors || mat_dims[2] != n_actors) {
            cli::cli_alert_danger(
                "dyad_data[['{t}']][['{var}']] has incorrect dimensions. Expected {n_actors}x{n_actors}, got {mat_dims[1]}x{mat_dims[2]}"
            )
            stop()
        }
    }
}
