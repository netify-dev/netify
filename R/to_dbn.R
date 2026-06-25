#' Convert netify objects to dbn format
#'
#' `netify_to_dbn` (also available as `to_dbn`)
#' transforms netify network objects into the array format required by the dbn
#' package for dynamic bilinear network models. this enables the use of
#' longitudinal latent space models for multilayer and single-layer networks.
#'
#' @param netlet a netify object (class "netify") containing longitudinal
#'   network data. must be of type \code{longit_array} or \code{longit_list}.
#'   supports both single-layer and multilayer networks.
#'
#' @return a list containing:
#'   \describe{
#'     \item{\strong{y}}{network adjacency data as a 4d array of dimensions
#'       \code{[n_actors, n_actors, n_layers, n_time]}. for single-layer
#'       networks, \code{n_layers = 1} and the third dimension is labeled
#'       with the weight variable name (or \code{"edge_value"} for binary
#'       networks). missing edges are preserved as na.
#'     }
#'     \item{\strong{xdyad}}{dyadic covariates as a 4d array of dimensions
#'       \code{[n_actors, n_actors, n_covariates, n_time]}, or NULL if none
#'       exist.
#'     }
#'     \item{\strong{xrow}}{sender/row actor attributes as a 3d array of dimensions
#'       \code{[n_actors, n_attributes, n_time]}, or NULL if none exist.
#'     }
#'     \item{\strong{xcol}}{receiver/column actor attributes as a 3d array of dimensions
#'       \code{[n_actors, n_attributes, n_time]}, or NULL if none exist.
#'       for symmetric networks, xcol is identical to xrow.
#'     }
#'   }
#'
#' @details
#' the dbn package expects a 4d array with dimensions
#' \code{[n, n, p, t]} where \code{n} is the number of actors, \code{p}
#' is the number of relation types (layers), and \code{t} is the number of
#' time periods.
#'
#' \strong{supported netify types:}
#' \itemize{
#'   \item \code{longit_array}: directly extracts or reshapes the underlying array
#'   \item \code{longit_list}: converts to array format first (actors are
#'     unioned across time, missing entries become na)
#' }
#'
#' cross-sectional networks are not supported since dbn is designed for
#' longitudinal data. bipartite networks are also not supported because dbn
#' expects square actor-by-actor arrays.
#'
#' \strong{variable requirements:}
#' \itemize{
#'   \item all nodal attributes must be numeric (integer or double)
#'   \item all dyadic attributes must be numeric or logical matrices
#'   \item character or factor variables must be converted before using this function
#' }
#'
#' @examples
#' # load example data
#' data(icews)
#'
#' # create two longitudinal networks
#' verbal_net <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE,
#'     weight = "verbCoop",
#'     output_format = "longit_array"
#' )
#'
#' material_net <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE,
#'     weight = "matlCoop",
#'     output_format = "longit_array"
#' )
#'
#' # create multilayer network
#' multi_net <- layer_netify(
#'     list(verbal_net, material_net),
#'     layer_labels = c("verbal", "material")
#' )
#'
#' # convert to dbn format
#' dbn_data <- netify_to_dbn(multi_net)
#' dim(dbn_data$y) # [n_actors, n_actors, 2, n_years]
#'
#' \donttest{
#' # single-layer also works
#' dbn_single <- netify_to_dbn(verbal_net)
#' dim(dbn_single$y) # [n_actors, n_actors, 1, n_years]
#' }
#'
#' @author shahryar minhas
#'
#' @export netify_to_dbn
#' @aliases to_dbn

netify_to_dbn <- function(netlet) {
	# check if netify object
	netify_check(netlet)

	# get type
	netlet_type <- attributes(netlet)$netify_type

	# dbn requires longitudinal data
	if (netlet_type == "cross_sec") {
		cli::cli_abort(
			"dbn requires longitudinal data.
				Please provide a `longit_array` or `longit_list` netify object."
		)
	}
	if (identical(attributes(netlet)$mode, "bipartite")) {
		cli::cli_abort(c(
			"x" = "{.fn netify_to_dbn} currently supports unipartite longitudinal netify objects only.",
			"i" = "Bipartite inputs produce rectangular arrays, but {.pkg dbn} expects square actor-by-actor arrays."
		))
	}

	# check attributes
	nodal_data_exists <- !is.null(attr(netlet, "nodal_data"))
	dyad_data_exists <- !is.null(attr(netlet, "dyad_data"))
	layers_exist <- length(attributes(netlet)$layers) > 1

	# get dimensions
	msrmnts <- netify_measurements(netlet)

	# make sure nodal attributes are numeric
	if (nodal_data_exists) {
		nvar_class_check <- apply(
			attr(netlet, "nodal_data")[, msrmnts$nvars, drop = FALSE],
			2, is.numeric
		)
		if (!all(nvar_class_check)) {
			cli::cli_abort(
				"All nodal attributes must be numeric."
			)
		}
	}
	if (dyad_data_exists) {
		validate_numeric_dyad_data(attr(netlet, "dyad_data"))
	}

	###########################
	# construct y array [n, n, p, t]
	###########################

	if (netlet_type == "longit_array") {
		raw_data <- get_raw(netlet)

		if (layers_exist) {
			# already [n, n, p, t]
			Y <- raw_data
		} else {
			# reshape [n, n, t] -> [n, n, 1, t]
			layer_label <- attributes(netlet)$weight
			if (is.null(layer_label) || is.logical(layer_label)) {
				layer_label <- "edge_value"
			}
			Y <- array(
				raw_data,
				dim = c(dim(raw_data)[1], dim(raw_data)[2], 1, dim(raw_data)[3]),
				dimnames = list(
					dimnames(raw_data)[[1]],
					dimnames(raw_data)[[2]],
					layer_label,
					dimnames(raw_data)[[3]]
				)
			)
		}
	}

	if (netlet_type == "longit_list") {
		# convert list to array first
		if (layers_exist) {
			# for multilayer longit_list, each element is a 3d array
			# [n_actors_t, n_actors_t, n_layers]
			# union all actors and build 4d array
			all_row_actors <- sort(unique(unlist(msrmnts$row_actors, use.names = FALSE)))
			all_col_actors <- sort(unique(unlist(msrmnts$col_actors, use.names = FALSE)))
			layer_labels <- msrmnts$layers
			time_periods <- msrmnts$time

			raw_data <- get_raw(netlet)

			Y <- array(NA,
				dim = c(
					length(all_row_actors), length(all_col_actors),
					msrmnts$n_layers, msrmnts$n_time
				),
				dimnames = list(
					all_row_actors, all_col_actors,
					layer_labels, time_periods
				)
			)

			for (tt in seq_along(time_periods)) {
				slice <- raw_data[[tt]]
				Y[rownames(slice), colnames(slice), , tt] <- slice
			}
		} else {
			# single-layer longit_list: convert to 3d then add layer dim
			arr_3d <- longit_dv_to_arr(netlet)
			layer_label <- attributes(netlet)$weight
			if (is.null(layer_label) || is.logical(layer_label)) {
				layer_label <- "edge_value"
			}
			Y <- array(
				arr_3d,
				dim = c(dim(arr_3d)[1], dim(arr_3d)[2], 1, dim(arr_3d)[3]),
				dimnames = list(
					dimnames(arr_3d)[[1]],
					dimnames(arr_3d)[[2]],
					layer_label,
					dimnames(arr_3d)[[3]]
				)
			)
		}
	}

	###########################
	# construct xdyad [n, n, p_dyad, t]
	###########################

	Xdyad <- NULL
	if (dyad_data_exists) {
		Xdyad <- longit_dyad_to_arr(netlet)
	}

	###########################
	# construct xrow and xcol [n, p_nodal, t]
	###########################

	Xrow <- NULL
	Xcol <- NULL
	if (nodal_data_exists) {
		nodal_arrs <- longit_nodal_to_arr(netlet)
		if (!is.null(nodal_arrs)) {
			Xrow <- nodal_arrs$"row"
			Xcol <- nodal_arrs$"col"
		}
	}

	# return
	out <- list(
		Y = Y,
		Xdyad = Xdyad,
		Xrow = Xrow,
		Xcol = Xcol
	)

	return(out)
}

#' @rdname netify_to_dbn
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
to_dbn <- netify_to_dbn
