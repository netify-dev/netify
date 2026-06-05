#' Extract measurements and dimensions from a netify object
#'
#' `netify_measurements` (also available as `measurements`)
#' extracts information about the structure, dimensions, and attributes
#' of a netify object. this function provides a standardized way to inspect network
#' properties across different netify types.
#'
#' @param netlet a netify object (class "netify") to analyze. can be cross-sectional,
#'   longitudinal array, or longitudinal list format.
#'
#' @return a list containing measurements of the netify object with the following
#'   components (availability depends on netify type):
#'
#'   \strong{actor information:}
#'   \itemize{
#'     \item \code{row_actors}: character vector (or list) of row actor names
#'     \item \code{col_actors}: character vector (or list) of column actor names
#'     \item \code{n_row_actors}: integer (or list) count of row actors
#'     \item \code{n_col_actors}: integer (or list) count of column actors
#'   }
#'
#'   \strong{temporal information:}
#'   \itemize{
#'     \item \code{time}: character vector of time period labels (NULL for cross-sectional)
#'     \item \code{n_time}: integer count of time periods (NULL for cross-sectional)
#'   }
#'
#'   \strong{layer information:}
#'   \itemize{
#'     \item \code{layers}: character vector of layer names (NULL if single layer)
#'     \item \code{n_layers}: integer count of layers (NULL if single layer)
#'   }
#'
#'   \strong{attribute information:}
#'   \itemize{
#'     \item \code{nvars}: character vector of nodal variable names
#'     \item \code{n_nvars}: integer count of nodal variables
#'     \item \code{dvars}: character vector of dyadic variable names
#'     \item \code{n_dvars}: integer count of dyadic variables
#'   }
#'
#' @details
#' the function will adapt its output based on the netify object type.
#'
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export netify_measurements
#' @aliases measurements

netify_measurements <- function(netlet) {
	# check if netify object
	netify_check(netlet)

	## three cases: cross-sec/matrix, longit list, longit array
	netlet_type <- attributes(netlet)$netify_type

	# check other attributes
	nodal_data_exist <- !is.null(
		attributes(netlet)$nodal_data[[1]]
	)
	dyad_data_exist <- !is.null(
		attributes(netlet)$dyad_data[[1]]
	)
	layers_exist <- ifelse(
		length(attributes(netlet)$layers) > 1, TRUE, FALSE
	)

	# output object
	msrmnts <- list(
		row_actors = NULL, col_actors = NULL, time = NULL, layers = NULL,
		n_row_actors = NULL, n_col_actors = NULL, n_time = NULL, n_layers = NULL,
		nvars = NULL, n_nvars = NULL, dvars = NULL, n_dvars = NULL
	)

	nodal_vars <- function(nd, netlet_type) {
		metadata <- "actor"
		if (netlet_type != "cross_sec") {
			metadata <- c(metadata, "time")
		}
		setdiff(colnames(nd), metadata)
	}
	dyad_vars <- function(dd) {
		unique(unlist(lapply(dd, names), use.names = FALSE))
	}

	# cross-sec
	if (netlet_type == "cross_sec") {
		# dv measurements
		msrmnts$row_actors <- rownames(netlet)
		msrmnts$col_actors <- colnames(netlet)
		msrmnts$n_row_actors <- nrow(netlet)
		msrmnts$n_col_actors <- ncol(netlet)

		# add layer info if more than one
		if (layers_exist) {
			msrmnts$layers <- attributes(netlet)$layers
			msrmnts$n_layers <- length(attributes(netlet)$layers)
		} else {
			msrmnts$layers <- NULL
			msrmnts$n_layers <- NULL
		}

		# check nodal data
		if (nodal_data_exist) {
			msrmnts$nvars <- nodal_vars(attributes(netlet)$nodal_data, netlet_type)
			msrmnts$n_nvars <- length(msrmnts$nvars)
		}

		# check dyad data
		if (dyad_data_exist) {
			# extract variable names from new structure: list(time) -> list(vars) -> matrix
			dyad_data_raw <- attributes(netlet)$dyad_data
			msrmnts$dvars <- dyad_vars(dyad_data_raw)
			msrmnts$n_dvars <- length(msrmnts$dvars)
		}
	}

	# longit array
	if (netlet_type == "longit_array") {
		# dv measurements
		msrmnts$row_actors <- rownames(netlet)
		msrmnts$col_actors <- colnames(netlet)
		msrmnts$n_row_actors <- nrow(netlet)
		msrmnts$n_col_actors <- ncol(netlet)

		# add layer info if more than one
		if (layers_exist) {
			msrmnts$layers <- attributes(netlet)$layers
			msrmnts$n_layers <- length(attributes(netlet)$layers)
			# time gets shifted to fourth dim if layers exist
			msrmnts$time <- dimnames(netlet)[[4]]
			msrmnts$n_time <- dim(netlet)[4]
		} else {
			msrmnts$layers <- NULL
			msrmnts$n_layers <- NULL
			# time gets shifted to third  dim if layers exist
			msrmnts$time <- dimnames(netlet)[[3]]
			msrmnts$n_time <- dim(netlet)[3]
		}

		# check nodal data
		if (nodal_data_exist) {
			msrmnts$nvars <- nodal_vars(attributes(netlet)$nodal_data, netlet_type)
			msrmnts$n_nvars <- length(msrmnts$nvars)
		}

		# check dyad data
		if (dyad_data_exist) {
			# extract variable names from new structure: list(time) -> list(vars) -> matrix
			dyad_data_raw <- attributes(netlet)$dyad_data
			msrmnts$dvars <- dyad_vars(dyad_data_raw)
			msrmnts$n_dvars <- length(msrmnts$dvars)
		}
	}

	# longit list
	if (netlet_type == "longit_list") {
		# dv measurements
		msrmnts$row_actors <- lapply(netlet, rownames)
		msrmnts$col_actors <- lapply(netlet, colnames)
		msrmnts$time <- names(netlet)
		msrmnts$n_row_actors <- lapply(netlet, nrow)
		msrmnts$n_col_actors <- lapply(netlet, ncol)
		msrmnts$n_time <- length(netlet)

		# add layer info if more than one
		if (layers_exist) {
			msrmnts$layers <- attributes(netlet)$layers
			msrmnts$n_layers <- length(attributes(netlet)$layers)
		} else {
			msrmnts$layers <- NULL
			msrmnts$n_layers <- NULL
		}

		# check nodal data
		if (nodal_data_exist) {
			msrmnts$nvars <- nodal_vars(attributes(netlet)$nodal_data, netlet_type)
			msrmnts$n_nvars <- length(msrmnts$nvars)
		}

		# check dyad data
		if (dyad_data_exist) {
			# extract variable names from new structure: list(time) -> list(vars) -> matrix
			dyad_data_raw <- attributes(netlet)$dyad_data
			msrmnts$dvars <- dyad_vars(dyad_data_raw)
			msrmnts$n_dvars <- length(msrmnts$dvars)
		}
	}

	#
	return(msrmnts)
}

#' @rdname netify_measurements
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
measurements <- netify_measurements
