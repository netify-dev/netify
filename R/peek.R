#' Preview subsets of network data from netify objects
#'
#' `peek` provides a
#' convenient way to inspect portions of network data stored in netify objects.
#' Rather than displaying entire networks (which can be overwhelming for large
#' datasets), this function allows you to examine specific actors, time periods,
#' or layers for data exploration, verification, and debugging.
#'
#' @param netlet A netify object to preview. Can be cross-sectional, longitudinal
#'   (array or list format), and/or multilayer.
#'
#' @param actors Character vector of actor names or numeric indices to subset.
#'   Selects these actors as both senders and receivers (extracts subgraph among
#'   these actors). Overridden by \code{from} and \code{to} if specified.
#'
#' @param from Specifies which actors to display as senders of ties (row actors
#'   in the adjacency matrix). Can be:
#'   \itemize{
#'     \item \strong{Single number}: Shows the first n actors (e.g., \code{from = 5}
#'       displays actors 1-5)
#'     \item \strong{Numeric vector}: Shows specific positions (e.g., \code{from = c(1,3,5)}
#'       displays the 1st, 3rd, and 5th actors)
#'     \item \strong{Character vector}: Shows named actors (e.g., \code{from = c("USA", "China")}
#'       displays these specific countries)
#'     \item \strong{NULL}: Shows all row actors (default is 3)
#'   }
#'   In bipartite networks, from represents actors in the first mode.
#'
#' @param to Specifies which actors to display as receivers of ties (column actors
#'   in the adjacency matrix). Accepts the same input types as \code{from}. In
#'   bipartite networks, columns represent actors in the second mode. Default is 3.
#'
#' @param time For longitudinal networks, specifies which time periods to display. Can be:
#'   \itemize{
#'     \item \strong{Single number}: Shows the nth time period (e.g., \code{time = 1}
#'       shows the first time period)
#'     \item \strong{Numeric vector}: Shows specific time indices (e.g., \code{time = c(1,5,10)})
#'     \item \strong{Character vector}: Shows named time periods (e.g., \code{time = c("2002", "2006")}
#'       displays these specific years)
#'     \item \strong{NULL}: Shows all time periods
#'   }
#'   Default is 1 (first time period only). Ignored for cross-sectional networks.
#'
#' @param layers For multilayer networks, specifies which layer(s) to display. Must be
#'   a character vector matching layer names in the netify object (e.g.,
#'   \code{layers = c("trade", "alliance")}). For single-layer networks, this
#'   parameter is ignored. Default is NULL (shows all layers).
#'
#' @return Returns a subset of the raw network data (without netify attributes):
#'
#'   \describe{
#'     \item{\strong{Cross-sectional networks}}{
#'       \itemize{
#'         \item Single layer: Returns a matrix with selected rows and columns
#'         \item Multilayer: Returns a 3D array (rows × columns × layers)
#'       }
#'     }
#'     \item{\strong{Longitudinal networks}}{
#'       \itemize{
#'         \item Array format: Returns an array with dimensions depending on selection
#'         \item List format: Returns a list of matrices, one per selected time period
#'       }
#'     }
#'   }
#'
#'   All returned objects preserve dimension names (actor names, time labels, layer
#'   names) for easy interpretation. Single dimensions are automatically dropped.
#'
#' @details
#' \strong{Purpose and Design}
#'
#' \code{peek} is designed as a lightweight data exploration tool. Unlike
#' \code{\link{subset_netify}}, which creates new netify objects with all attributes
#' preserved, \code{peek} returns only the raw network data for quick inspection.
#' This makes it ideal for:
#' \itemize{
#'   \item Verifying data structure and content
#'   \item Checking specific relationships
#'   \item Debugging data issues
#'   \item Quick visual inspection of network patterns
#' }
#'
#' \strong{Understanding Network Directions}
#'
#' In directed networks:
#' \itemize{
#'   \item \strong{From} represents actors sending ties (out-ties)
#'   \item \strong{To} represents actors receiving ties (in-ties)
#'   \item Cell \code{[i,j]} contains the tie from actor i to actor j
#' }
#'
#' For example, if cell \code{["USA", "China"]} = 5, this means USA sends a tie of
#' strength 5 to China.
#'
#' \strong{Smart Selection Behavior}
#'
#' The function includes several convenience features:
#' \itemize{
#'   \item Single numbers are expanded to ranges (e.g., \code{from = 5} becomes first 5 actors)
#'   \item Out-of-bounds indices are silently ignored (no errors during exploration)
#'   \item Character names are matched to actor labels
#'   \item Dimension reduction: if only one time period or layer is selected, that
#'     dimension is dropped from the output
#' }
#'
#'
#' @note
#' \strong{Important distinctions:}
#' \itemize{
#'   \item Use \code{peek} for quick data inspection (returns raw matrices)
#'   \item Use \code{\link{subset_netify}} to create new netify objects with attributes
#'   \item Use \code{\link{get_raw}} to extract all raw data from a netify object
#' }
#'
#' When multiple layers are present and no layer selection is specified, all layers
#' are returned with a warning message to remind you about the multilayer structure.
#'
#' @examples
#' # Load example data
#' data(icews)
#'
#' # Example 1: Basic usage with cross-sectional network
#' icews_10 <- icews[icews$year == 2010, ]
#' net <- netify(
#'     icews_10,
#'     actor1 = "i", actor2 = "j",
#'     weight = "verbCoop"
#' )
#'
#' # Default: see first 3 actors (both sending and receiving)
#' peek(net)
#'
#' # See first 5 senders and first 5 receivers
#' peek(net, from = 5, to = 5)
#'
#' # Example 2: Specific actors by name
#' # See ties from US and China to Russia, India, and Brazil
#' peek(net,
#'     from = c("United States", "China"),
#'     to = c("Russia", "India", "Brazil")
#' )
#'
#' # Use actors parameter to see subgraph
#' peek(net,
#'     actors = c("United States", "China", "Russia")
#' )
#'
#' # Example 3: Longitudinal network
#' net_longit <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     weight = "matlConf"
#' )
#'
#' # See first 5 actors in specific years
#' peek(net_longit,
#'     from = 5, to = 5,
#'     time = c("2002", "2006", "2010")
#' )
#'
#' # See all actors in year 2010
#' peek(net_longit,
#'     from = NULL, to = NULL,
#'     time = "2010"
#' )
#'
#' # Example 4: Using numeric indices
#' # See specific positions in the network
#' peek(net,
#'     from = c(1, 3, 5, 7), # 1st, 3rd, 5th, 7th senders
#'     to = 1:10
#' ) # first 10 receivers
#'
#' # Example 5: Quick inspection patterns
#' # See who USA interacts with
#' peek(net, from = "United States", to = 10) # USA's ties to first 10 countries
#' peek(net, from = 10, to = "United States") # First 10 countries' ties to USA
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export peek

peek <- function(netlet,
                 actors = NULL,
                 from = 3,
                 to = 3,
                 time = 1,
                 layers = NULL) {
    # user input checks
    netify_check(netlet)

    # Handle actors parameter - if specified, it sets both from and to
    if (!is.null(actors)) {
        from <- actors
        to <- actors
    }

    # Cache attributes for efficiency
    netify_type <- attr(netlet, "netify_type")
    n_layers <- length(attr(netlet, "layers"))
    is_multilayer <- n_layers > 1
    is_longit <- netify_type != "cross_sec"

    # Handle layer selection
    if (is_multilayer && is.null(layers)) {
        cli::cli_alert_warning("Multiple layers detected. Showing all layers. Specify 'layers' parameter to select specific layers.")
    } else if (!is_multilayer && !is.null(layers)) {
        cli::cli_alert_warning("Single layer network - ignoring 'layers' parameter.")
        layers <- NULL
    }

    # Validate layer selection if provided
    if (!is.null(layers)) {
        available_layers <- attr(netlet, "layers")
        invalid_layers <- setdiff(layers, available_layers)
        if (length(invalid_layers) > 0) {
            cli::cli_alert_danger(
                "Invalid layer(s): {paste(invalid_layers, collapse=', ')}. Available layers: {paste(available_layers, collapse=', ')}"
            )
            stop()
        }
    }

    # Get raw data for processing
    raw_data <- get_raw(netlet)

    # Process time selection for longitudinal data
    if (is_longit) {
        time_labels <- if (netify_type == "longit_list") {
            names(raw_data)
        } else {
            dimnames(raw_data)[[if (is_multilayer) 4 else 3]]
        }

        # Convert time parameter to indices
        if (is.null(time)) {
            time_idx <- seq_along(time_labels)
        } else if (is.character(time)) {
            time_idx <- match(time, time_labels)
            time_idx <- time_idx[!is.na(time_idx)]
        } else {
            time_idx <- time
        }

        # Validate time indices
        max_time <- length(time_labels)
        time_idx <- time_idx[time_idx >= 1 & time_idx <= max_time]

        if (length(time_idx) == 0) {
            cli::cli_alert_danger("No valid time periods selected")
            stop()
        }
    }

    # Process row/column selection based on netify type
    if (netify_type == "cross_sec") {
        return(peek_cross_sectional(raw_data, from, to, layers, is_multilayer))
    } else if (netify_type == "longit_array") {
        return(peek_longit_array(raw_data, from, to, time_idx, layers, is_multilayer))
    } else if (netify_type == "longit_list") {
        return(peek_longit_list(raw_data, from, to, time_idx, layers, is_multilayer))
    }
}

#' Extract subset from cross-sectional network data
#'
#' Internal helper function that processes cross-sectional network data
#' for the peek function. Handles both single-layer and multilayer networks.
#'
#' @param data Raw network data (matrix or 3D array)
#' @param rows Row selection (actors sending ties)
#' @param cols Column selection (actors receiving ties)
#' @param layers Layer names to select (NULL for all layers)
#' @param is_multilayer Logical indicating if network has multiple layers
#'
#' @return Subsetted matrix or array based on selection criteria
#'
#' @keywords internal
#' @noRd
peek_cross_sectional <- function(data, rows, cols, layers, is_multilayer) {
    # Get dimensions
    dims <- dim(data)
    actor_rows <- dimnames(data)[[1]]
    actor_cols <- dimnames(data)[[2]]

    # Process row selection
    row_idx <- process_actor_selection(rows, actor_rows, dims[1])
    col_idx <- process_actor_selection(cols, actor_cols, dims[2])

    # Subset data
    if (!is_multilayer) {
        return(data[row_idx, col_idx, drop = FALSE])
    } else {
        if (is.null(layers)) {
            out <- data[row_idx, col_idx, , drop = FALSE]
        } else {
            out <- data[row_idx, col_idx, layers, drop = FALSE]
        }
        return(drop(out))
    }
}

#' Extract subset from longitudinal array network data
#'
#' Internal helper function that processes longitudinal array-format network data
#' for the peek function. Handles time dimension along with actor and layer selection.
#'
#' @param data Raw network data in array format (3D or 4D array)
#' @param rows Row selection (actors sending ties)
#' @param cols Column selection (actors receiving ties)
#' @param time_idx Numeric indices of time periods to extract
#' @param layers Layer names to select (NULL for all layers)
#' @param is_multilayer Logical indicating if network has multiple layers
#'
#' @return Subsetted array with selected dimensions
#'
#' @keywords internal
#' @noRd
peek_longit_array <- function(data, rows, cols, time_idx, layers, is_multilayer) {
    # Get dimensions
    dims <- dim(data)
    actor_rows <- dimnames(data)[[1]]
    actor_cols <- dimnames(data)[[2]]

    # Process actor selection
    row_idx <- process_actor_selection(rows, actor_rows, dims[1])
    col_idx <- process_actor_selection(cols, actor_cols, dims[2])

    # Subset data
    if (!is_multilayer) {
        out <- data[row_idx, col_idx, time_idx, drop = FALSE]
    } else {
        if (is.null(layers)) {
            out <- data[row_idx, col_idx, , time_idx, drop = FALSE]
        } else {
            out <- data[row_idx, col_idx, layers, time_idx, drop = FALSE]
        }
    }
    return(drop(out))
}

#' Extract subset from longitudinal list network data
#'
#' Internal helper function that processes longitudinal list-format network data
#' for the peek function. Handles networks where actor composition may vary across
#' time periods.
#'
#' @param data List of network matrices, one per time period
#' @param rows Row selection (actors sending ties)
#' @param cols Column selection (actors receiving ties)
#' @param time_idx Numeric indices of time periods to extract
#' @param layers Layer names to select (NULL for all layers)
#' @param is_multilayer Logical indicating if network has multiple layers
#'
#' @return List of subsetted matrices for selected time periods
#'
#' @keywords internal
#' @noRd
peek_longit_list <- function(data, rows, cols, time_idx, layers, is_multilayer) {
    # Subset to requested time periods
    data_subset <- data[time_idx]

    # Process each time period
    result <- lapply(data_subset, function(slice) {
        dims <- dim(slice)
        actor_rows <- dimnames(slice)[[1]]
        actor_cols <- dimnames(slice)[[2]]

        # Process actor selection for this time period
        row_idx <- process_actor_selection(rows, actor_rows, dims[1])
        col_idx <- process_actor_selection(cols, actor_cols, dims[2])

        # Subset
        if (!is_multilayer) {
            return(slice[row_idx, col_idx, drop = FALSE])
        } else {
            if (is.null(layers)) {
                out <- slice[row_idx, col_idx, , drop = FALSE]
            } else {
                out <- slice[row_idx, col_idx, layers, drop = FALSE]
            }
            return(drop(out))
        }
    })

    return(result)
}

#' Process actor selection for peek operations
#'
#' Internal helper function that converts various actor selection formats into
#' numeric indices. Handles single numbers (first n), numeric vectors (specific
#' positions), character vectors (actor names), and NULL (all actors).
#'
#' @param selection User input for actor selection (numeric, character, or NULL)
#' @param actor_names Character vector of available actor names
#' @param max_dim Maximum dimension size (total number of actors)
#'
#' @return Numeric vector of valid actor indices
#'
#' @keywords internal
#' @noRd
process_actor_selection <- function(selection, actor_names, max_dim) {
    if (is.null(selection)) {
        # NULL means all actors
        return(seq_len(max_dim))
    } else if (is.numeric(selection) && length(selection) == 1) {
        # Single number means first n actors
        return(seq_len(min(selection, max_dim)))
    } else if (is.numeric(selection)) {
        # Numeric vector of indices
        return(selection[selection >= 1 & selection <= max_dim])
    } else if (is.character(selection) || is.factor(selection)) {
        # Character vector of actor names
        if (is.factor(selection)) selection <- as.character(selection)
        idx <- match(selection, actor_names)
        return(idx[!is.na(idx)])
    } else {
        cli::cli_alert_danger("Invalid selection type. Use numeric indices or character names.")
        stop()
    }
}
