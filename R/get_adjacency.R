#' Create a netify matrix from cross-sectional dyadic data
#'
#' `get_adjacency` converts cross-sectional dyadic data into an adjacency matrix
#' of class "netify". This function creates a single network matrix representing
#' relationships at one point in time.
#'
#' @param dyad_data A data.frame containing dyadic observations. Will be coerced
#'   to data.frame if a tibble or data.table is provided.
#' @param actor1 Character string specifying the column name for the first actor
#'   in each dyad.
#' @param actor2 Character string specifying the column name for the second actor
#'   in each dyad.
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
#'   between the same actor pair. If FALSE (default), uses the last observed value.
#' @param diag_to_NA Logical. If TRUE (default), sets diagonal values (self-loops)
#'   to NA. Automatically set to FALSE for bipartite networks.
#' @param missing_to_zero Logical. If TRUE (default), treats missing edges as zeros.
#'   If FALSE, missing edges remain as NA.
#' @param nodelist Character vector of actor names to include in the network.
#'   If provided, ensures all listed actors appear in the network even if they
#'   have no edges (isolates). Useful when working with edgelists that only
#'   contain active dyads.
#'
#' @return A matrix of class "netify" (a netify matrix) with:
#'   \itemize{
#'   \item \strong{Dimensions}: \code{[n_actors x n_actors]} for unipartite networks
#'       or \code{[n_actors1 x n_actors2]} for bipartite networks
#'     \item \strong{Class}: "netify" - this is a full netify object compatible
#'       with all netify functions
#'     \item \strong{Attributes}: Metadata including network properties and
#'       processing parameters
#'   }
#'
#'   The returned object is a netify matrix that can be used with all netify
#'   functions such as `summary()`, `plot()`, `to_igraph()`, etc.
#'
#' @details
#' \strong{Note on usage:}
#'
#' While this function is exported and available for direct use, the primary and
#' recommended way to create netify objects from dyadic data is through the
#' `netify()` function. The `netify()` function:
#' \itemize{
#'   \item Provides a consistent interface for both cross-sectional and longitudinal data
#'   \item Includes additional data validation and preprocessing options
#'   \item Can incorporate nodal and dyadic attributes during creation
#'   \item Offers more comprehensive parameter checking
#' }
#'
#' Use `get_adjacency()` directly only when you need a simple adjacency matrix
#' creation without additional features.
#'
#' @examples
#' # Load example data
#' data(icews)
#'
#' # Subset to one year for cross-sectional analysis
#' icews_2010 <- icews[icews$year == 2010, ]
#'
#' # Create a directed network with verbal cooperation weights
#' verbCoop_net <- get_adjacency(
#'     dyad_data = icews_2010,
#'     actor1 = "i",
#'     actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # Create a directed network with material conflict weights
#' matlConf_net <- get_adjacency(
#'     dyad_data = icews_2010,
#'     actor1 = "i",
#'     actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "matlConf"
#' )
#'
#' # Verify class
#' class(verbCoop_net) # "netify"
#'
#' # Check dimensions
#' dim(verbCoop_net)
#'
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @export get_adjacency
#'

get_adjacency <- function(
    dyad_data,
    actor1 = NULL, actor2 = NULL,
    symmetric = TRUE, mode = "unipartite",
    weight = NULL, sum_dyads = FALSE,
    diag_to_NA = TRUE, missing_to_zero = TRUE,
    nodelist = NULL) {
    # if bipartite network then force diag_to_NA to be FALSE
    # and force asymmetric, create copy to preserve user choice
    user_symmetric <- symmetric
    if (mode == "bipartite") {
        diag_to_NA <- FALSE
        symmetric <- FALSE
    }

    # # if mode bipartite is specified make sure that
    # # actors in actor1 and actor2 columns are distinct
    # if(mode=='bipartite'){
    #   if(length(intersect(dyad_data[,actor1], dyad_data[,actor2])) > 0){
    #     cli::cli_alert_warning(
    #       "Warning: Mode has been inputted as bipartite but actors are not distinct across the modes."
    #     )
    #   }
    # }

    # create weight string for storage as attribute in netify object
    weight_label <- weight_string_label(weight, sum_dyads)

    # add weight if not supplied
    wOrig <- weight
    if (is.null(weight)) {
        dyad_data$weight_var <- 1
        weight <- "weight_var"
    }

    # subset to relevant vars once
    dyad_data <- dyad_data[, c(actor1, actor2, weight)]

    # get vector of actors - optimized extraction
    actors_rows <- unique_vector(dyad_data[, actor1])
    actors_cols <- unique_vector(dyad_data[, actor2])
    actors <- unique_vector(actors_rows, actors_cols)
    
    # Incorporate nodelist if provided
    if (!is.null(nodelist)) {
        # Convert to character to ensure consistency
        nodelist <- as.character(nodelist)
        
        # Add any missing actors from nodelist
        if (mode == "unipartite") {
            actors <- unique_vector(actors, nodelist)
            actors_rows <- actors_cols <- actors
        } else {
            # For bipartite, assume nodelist contains all actors
            # User should specify which are row/col actors
            cli::cli_alert_info("For bipartite networks, nodelist should contain all actors. Assigning to both row and column actors.")
            actors_rows <- unique_vector(actors_rows, nodelist)
            actors_cols <- unique_vector(actors_cols, nodelist)
            actors <- unique_vector(actors_rows, actors_cols)
        }
    } else if (mode == "unipartite") {
        actors_rows <- actors_cols <- actors
    }

    # actor year info
    actor_pds <- data.frame(
        actor = actors,
        stringsAsFactors = FALSE
    )
    actor_pds$min_time <- 1
    actor_pds$max_time <- 1

    # check if there are repeating dyads
    num_repeat_dyads <- repeat_dyads_check(dyad_data, actor1, actor2)
    if (num_repeat_dyads > 0) {
        edge_value_check(wOrig, sum_dyads, TRUE)
    }

    # aggregate data if sum dyads selected
    if (sum_dyads) {
        dyad_data <- aggregate_dyad(dyad_data, actor1, actor2, NULL, weight, symmetric, missing_to_zero)
    }

    # remove zeros early if missing_to_zero is TRUE
    if (missing_to_zero) {
        dyad_data <- dyad_data[dyad_data[, weight] != 0, ]
    }

    # Cache frequently accessed columns for efficiency
    dyad_actor1 <- dyad_data[, actor1]
    dyad_actor2 <- dyad_data[, actor2]
    dyad_weight <- dyad_data[, weight]

    # assign cross-section value for adjmat depending on user inputs
    value <- dyad_weight

    # create logical value that is TRUE if weight is just 0/1 - optimized check
    is_binary <- length(value) == 0 || all(value %in% c(0, 1))

    # Pre-compute matrix indices to avoid repeated match() calls
    matRowIndices <- match(dyad_actor1, actors_rows)
    matColIndices <- match(dyad_actor2, actors_cols)

    # convert to adjacency matrix using optimized C++ function
    adj_out <- get_matrix(
        n_rows = length(actors_rows),
        n_cols = length(actors_cols),
        actors_rows = actors_rows,
        actors_cols = actors_cols,
        matRowIndices = matRowIndices,
        matColIndices = matColIndices,
        value = value,
        symmetric = symmetric,
        missing_to_zero = missing_to_zero,
        diag_to_NA = diag_to_NA && mode == "unipartite"
    )

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

    # add class info and attributes efficiently
    class(adj_out) <- "netify"
    attributes(adj_out) <- c(attributes(adj_out), list(
        netify_type = "cross_sec",
        actor_time_uniform = TRUE,
        actor_pds = actor_pds,
        weight = weight,
        detail_weight = weight_label,
        is_binary = is_binary,
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
