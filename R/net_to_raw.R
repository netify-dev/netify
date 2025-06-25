#' raw_matrix
#'
#' This function takes in a netify object of type
#' cross_sec or longit_array and returns an R matrix/array
#' @param netlet a netify object of type
#' cross_sec' or 'longit_array'
#' @return an object of class matrix, array with netify
#' attributes stripped
#' @author Shahryar Minhas
#' @keywords internal
#' @noRd

raw_matrix <- function(netlet) {
    stopifnot("Not a netify object!" = is_netify(netlet))
    out <- netlet
    if (length(attributes(out)$dim) > 2) {
        class(out) <- c("array")
    } else {
        class(out) <- c("matrix", "array")
    }
    attributes(out)[3:length(attributes(out))] <- NULL
    return(out)
}

#' raw_list
#'
#' This function takes in a netify object of type
#' cross_sec and returns a regular matrix
#' @param netlet a netify object of type 'longit_list'
#' @return an object of class list with netify
#' attributes stripped
#' @author Shahryar Minhas
#' @keywords internal
#' @noRd

raw_list <- function(netlet) {
    stopifnot("Not a netify object!" = is_netify(netlet))
    netlet <- lapply(netlet, raw_matrix)
    class(netlet) <- c("list")
    # attributes(netlet)[2:length(attributes(netlet))] <- NULL
    return(netlet)
}

#' Get raw network data without netify attributes
#'
#' `get_raw` extracts the underlying network data structure (matrix or list)
#' from a netify object, removing all netify-specific attributes. This is useful
#' when you need to work with the base R data structures or pass the network
#' data to functions that don't recognize netify objects.
#'
#' @param netlet A netify object
#' @return A matrix or list object with netify attributes removed. The structure
#'   returned depends on the type of netify object:
#'   \itemize{
#'     \item Cross-sectional networks: returns a matrix
#'     \item Longitudinal array networks: returns a matrix
#'     \item Longitudinal list networks: returns a list
#'   }
#' @author Shahryar Minhas
#' @export get_raw

get_raw <- function(netlet) {
    # check
    netify_check(netlet)

    # get type
    obj_type <- attributes(netlet)$netify_type

    # strip netify attributes and return
    if (obj_type %in% c("cross_sec", "longit_array")) {
        return(raw_matrix(netlet))
    }
    if (obj_type %in% c("longit_list")) {
        return(raw_list(netlet))
    }
}
