#' Create a netify array from longitudinal dyadic data
#'
#' `get_adjacency_array` converts longitudinal dyadic data into a three-dimensional
#' netify array where the first two dimensions represent actors and the third
#' dimension represents time periods. This function creates an array of class
#' "netify" and should only be used when actor composition remains constant
#' across all time periods.
#'
#' @param dyad_data A data.frame containing longitudinal dyadic observations. Will
#'   be coerced to data.frame if a tibble or data.table is provided.
#' @param actor1 Character string specifying the column name for the first actor
#'   in each dyad.
#' @param actor2 Character string specifying the column name for the second actor
#'   in each dyad.
#' @param time Character string specifying the column name for time periods.
#' @param symmetric Logical. If TRUE (default), treats the network as undirected
#'   (i.e., edges have no direction). If FALSE, treats the network as directed.
#' @param mode Character string specifying network structure. Options are:
#'   \itemize{
#'     \item \code{"unipartite"}: One set of actors (default)
#'     \item \code{"bipartite"}: Two distinct sets of actors
#'   }
#' @param weight Character string specifying the column name containing edge weights.
#'   If NULL (default), edges are treated as unweighted (binary).
#' @param sum_dyads Logical. If TRUE, sums weight values when multiple edges exist
#'   between the same actor pair in the same time period. If FALSE (default), uses
#'   the last observed value.
#' @param diag_to_NA Logical. If TRUE (default), sets diagonal values (self-loops)
#'   to NA. Automatically set to FALSE for bipartite networks.
#' @param missing_to_zero Logical. If TRUE (default), treats missing edges as zeros.
#'   If FALSE, missing edges remain as NA.
#'
#' @return A three-dimensional array of class "netify" (a netify array) with:
#'   \itemize{
#'     \item \strong{Dimensions}: \code{[n_actors × n_actors × n_time]} for unipartite
#'       networks or \code{[n_actors1 × n_actors2 × n_time]} for bipartite networks
#'     \item \strong{Class}: "netify" - this is a full netify object compatible
#'       with all netify functions
#'     \item \strong{Attributes}: Extensive metadata including network properties,
#'       actor information, and processing parameters
#'   }
#'
#'   The returned object is a netify array that can be used with all netify
#'   functions such as `summary()`, `plot()`, `to_igraph()`, etc.
#'
#' @details
#' \strong{Note on usage:}
#'
#' While this function is exported and available for direct use, the primary and
#' recommended way to create netify arrays from longitudinal dyadic data is through
#' the `netify()` function. The `netify()` function:
#' \itemize{
#'   \item Automatically determines whether to create an array or list structure
#'   \item Handles time-varying actor composition
#'   \item Provides more comprehensive data validation
#'   \item Offers a unified interface for all types of network data
#' }
#'
#' Use `get_adjacency_array()` directly only when you specifically need low-level
#' control over array creation and are certain your actors remain constant across
#' time.
#'
#' @examples
#' # Load example data
#' data(icews)
#'
#' # Create a netify array (longitudinal directed network)
#' # with material conflict as edge weights
#' icews_array <- get_adjacency_array(
#'     dyad_data = icews,
#'     actor1 = "i",
#'     actor2 = "j",
#'     time = "year",
#'     symmetric = FALSE,
#'     weight = "matlConf"
#' )
#'
#' # Verify it's a netify object
#' class(icews_array) # "netify"
#'
#' # Check dimensions
#' dim(icews_array) # [n_actors, n_actors, n_years]
#'
#' # Access specific time period
#' icews_2010 <- icews_array[, , "2010"]
#'
#' @author Cassy Dorff, Ha Eun Choi, Shahryar Minhas
#'
#' @export get_adjacency_array

get_adjacency_array <- function(
    dyad_data,
    actor1 = NULL, actor2 = NULL, time = NULL,
    symmetric = TRUE, mode = "unipartite",
    weight = NULL, sum_dyads = FALSE,
    diag_to_NA = TRUE, missing_to_zero = TRUE
    ){
        
    # create weight string for storage as attribute in netify object
    weight_label <- weight_string_label(weight, sum_dyads)

    # if bipartite network then force diag_to_NA to be FALSE
    # and force asymmetric, create copy to preserve user choice
    user_symmetric <- symmetric
    if (mode == "bipartite") {
        diag_to_NA <- FALSE
        symmetric <- FALSE
    }

    # convert time to numeric and get labels
    time_info <- convert_time_to_numeric(dyad_data[[time]], time)
    dyad_data[[time]] <- time_info$numeric_time

    # time check
    if (!is.numeric(dyad_data[, time])) {
        cli::cli_alert_danger("Failed to convert time variable to numeric format.")
        stop()
    }

    # add weight if not supplied
    wOrig <- weight
    if (is.null(weight)) {
        dyad_data$weight_var <- 1
        weight <- "weight_var"
    }

    # subset to relevant vars once
    dyad_data <- dyad_data[, c(actor1, actor2, time, weight)]

    # get vector of time periods from conversion
    time_pds <- time_info$time_labels
    time_pds_num <- sort(unique(time_info$numeric_time))

    # get vector of actors - optimized extraction
    actors_rows <- unique_vector(dyad_data[, actor1])
    actors_cols <- unique_vector(dyad_data[, actor2])
    actors <- unique_vector(actors_rows, actors_cols)
    if (mode == "unipartite") {
        actors_rows <- actors_cols <- actors
    }

    # add info on actor time sample
    actor_pds <- data.frame(
        actor = actors,
        stringsAsFactors = FALSE
    )
    actor_pds$min_time <- time_pds[1]
    actor_pds$max_time <- time_pds[length(time_pds)]

    # check if there are repeating dyads
    num_repeat_dyads <- repeat_dyads_check(dyad_data, actor1, actor2, time)
    if (num_repeat_dyads > 0) {
        edge_value_check(wOrig, sum_dyads, TRUE)
    }

    # aggregate data if sum dyads selected
    if (sum_dyads) {
        dyad_data <- aggregate_dyad(dyad_data, actor1, actor2, time, weight, symmetric, missing_to_zero)
    }

    # remove zeros early if missing_to_zero is TRUE
    if (missing_to_zero) {
        dyad_data <- dyad_data[dyad_data[, weight] != 0, ]
    }

    # Pre-split data by time periods for faster subsetting
    time_indices <- split(seq_len(nrow(dyad_data)), dyad_data[, time])

    # Cache frequently accessed columns
    dyad_actor1 <- dyad_data[, actor1]
    dyad_actor2 <- dyad_data[, actor2]
    dyad_weight <- dyad_data[, weight]

    # organize array dimensions
    n_rows <- length(actors_rows)
    n_cols <- length(actors_cols)
    t <- length(time_pds)
    adj_out <- array(NA,
        dim = c(n_rows, n_cols, t),
        dimnames = list(actors_rows, actors_cols, time_pds)
    )

    # binary weight check vector
    bin_check <- logical(t)

    # iterate through third mode and fill in - optimized loop
    for (t_idx in seq_along(time_pds)) {
        time_pd <- time_pds[t_idx]
        time_pd_num <- time_pds_num[t_idx]

        # Get indices for this time period using pre-split data
        slice_indices <- time_indices[[as.character(time_pd_num)]]
        if (is.null(slice_indices)) slice_indices <- integer(0)

        # get values and indices for this time slice
        if (length(slice_indices) > 0) {
            slice_actor1 <- dyad_actor1[slice_indices]
            slice_actor2 <- dyad_actor2[slice_indices]
            value <- dyad_weight[slice_indices]

            # Pre-compute matrix indices to avoid repeated match() calls
            matRowIndices <- match(slice_actor1, actors_rows)
            matColIndices <- match(slice_actor2, actors_cols)
        } else {
            value <- numeric(0)
            matRowIndices <- integer(0)
            matColIndices <- integer(0)
        }

        # create logical value that is TRUE if weight is just 0/1
        weight_binary <- length(value) == 0 || all(value %in% c(0, 1))
        bin_check[t_idx] <- weight_binary

        # get adj mat filled in using optimized C++ function
        adj_mat <- get_matrix(
            n_rows = length(actors_rows),
            n_cols = length(actors_cols),
            actors_rows = actors_rows,
            actors_cols = actors_cols,
            matRowIndices = matRowIndices,
            matColIndices = matColIndices,
            value = value,
            symmetric = user_symmetric,
            missing_to_zero = missing_to_zero,
            diag_to_NA = diag_to_NA && mode == "unipartite"
        )

        # insert into array
        adj_out[, , as.character(time_pd)] <- adj_mat
    }

    # if user left weight NULL and set sum_dyads
    # to FALSE then record weight as NULL for
    # attribute purposes
    if (!sum_dyads && is.null(wOrig)) {
        weight <- NULL
    }

    # layer label
    if (is.null(weight)) {
        layer_label <- "weight1"
    } else {
        layer_label <- weight
    }

    # add attributes to array efficiently
    class(adj_out) <- "netify"
    attributes(adj_out) <- c(attributes(adj_out), list(
        netify_type = "longit_array",
        actor_time_uniform = TRUE,
        actor_pds = actor_pds,
        weight = weight,
        detail_weight = weight_label,
        weight_binary = all(bin_check),
        symmetric = user_symmetric,
        mode = mode,
        layers = layer_label,
        diag_to_NA = diag_to_NA,
        missing_to_zero = missing_to_zero,
        sum_dyads = sum_dyads,
        nodal_data = NULL,
        dyad_data = NULL
    ))

    return(adj_out)
}
