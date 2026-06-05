#' raw_matrix
#'
#' this function takes in a netify object of type
#' cross_sec or longit_array and returns an r matrix/array
#' @param netlet a netify object of type
#' cross_sec' or 'longit_array'
#' @return an object of class matrix, array with netify
#' attributes stripped
#' @author shahryar minhas
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
#' this function takes in a netify object of type
#' cross_sec and returns a regular matrix
#' @param netlet a netify object of type 'longit_list'
#' @return an object of class list with netify
#' attributes stripped
#' @author shahryar minhas
#' @keywords internal
#' @noRd

raw_list <- function(netlet) {
	stopifnot("Not a netify object!" = is_netify(netlet))
	netlet <- lapply(netlet, function(x) {
		if (is_netify(x)) {
			return(raw_matrix(x))
		}
		out <- x
		if (is.null(dim(out))) {
			cli::cli_abort("Longitudinal netify list elements must be matrices or arrays.")
		}
		attributes(out) <- list(dim = dim(x), dimnames = dimnames(x))
		out
	})
	class(netlet) <- c("list")
	return(netlet)
}

#' get raw network data without netify attributes
#'
#' `get_raw` extracts the underlying network data structure (matrix or list)
#' from a netify object, removing all netify-specific attributes. this is useful
#' when you need to work with the base r data structures or pass the network
#' data to functions that don't recognize netify objects.
#'
#' @param netlet a netify object
#' @return a matrix or list object with netify attributes removed. the structure
#'   returned depends on the type of netify object:
#'   \itemize{
#'     \item cross-sectional networks: returns a matrix
#'     \item longitudinal array networks: returns a matrix
#'     \item longitudinal list networks: returns a list
#'   }
#' @author shahryar minhas
#' @export get_raw

get_raw <- function(netlet) {
	netify_check(netlet)

	obj_type <- attributes(netlet)$netify_type

	if (obj_type %in% c("cross_sec", "longit_array")) {
		return(raw_matrix(netlet))
	}
	if (obj_type %in% c("longit_list")) {
		return(raw_list(netlet))
	}
}
