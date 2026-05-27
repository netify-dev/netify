#' Convert netify objects to amen format
#'
#' `netify_to_amen` (also available as `to_amen`)
#' transforms netify network objects into the data structure required by the amen
#' package for advanced network modeling. This enables the use of Social Relations
#' Models (SRM), Additive and Multiplicative Effects (AME) models, and other
#' network regression approaches implemented in amen.
#'
#' @param netlet A netify object (class "netify") containing network data. Must be
#'   a single-layer network. For multilayer networks, first extract individual
#'   layers using \code{\link{subset_netify}}.
#' @param lame Logical. Controls the output format for longitudinal data:
#'   \itemize{
#'     \item \code{FALSE} (default): Formats output for compatibility with the
#'       standard version of the amen package, which uses array structures.
#'       Y is returned as a 3D array \code{[n_actors x n_actors x n_time]},
#'       Xdyad as a 4D array \code{[n_actors x n_actors x n_covariates x n_time]},
#'       and Xrow/Xcol as 3D arrays \code{[n_actors x n_attributes x n_time]}.
#'       This requires constant actor composition across time.
#'     \item \code{TRUE}: Formats output for compatibility with the netify-verse
#'       version called lame, which supports
#'       longitudinal network modeling with time-varying actor compositions.
#'       Y is returned as a list of T matrices (one per time period),
#'       Xdyad as a list of T 3D arrays, and Xrow/Xcol as lists of T matrices.
#'       Actor sets can vary across time periods, making this suitable for
#'       panels where countries enter/exit.
#'   }
#'   This parameter is ignored for cross-sectional data.
#'
#' @return The structure of the returned list depends on the data type and `lame` parameter:
#'
#'   \strong{For cross-sectional data or longitudinal with lame = FALSE (standard amen):}
#'   \describe{
#'     \item{\strong{Y}}{Network adjacency data as a numeric matrix or array:
#'       \itemize{
#'         \item Cross-sectional: Matrix of dimensions \code{[n_actors × n_actors]}
#'         \item Longitudinal: Array of dimensions \code{[n_actors × n_actors × n_time]}
#'       }
#'       Contains edge weights or binary indicators. Missing edges are preserved as NA.
#'     }
#'     \item{\strong{Xdyad}}{Dyadic covariates as an array, or NULL if none exist:
#'       \itemize{
#'         \item Cross-sectional: \code{[n_actors × n_actors × n_covariates]}
#'         \item Longitudinal: \code{[n_actors × n_actors × n_covariates × n_time]}
#'       }
#'       Each slice contains one dyadic covariate across all actor pairs.
#'     }
#'     \item{\strong{Xrow}}{Sender/row actor attributes as a matrix or array, or NULL if none exist:
#'       \itemize{
#'         \item Cross-sectional: \code{[n_actors × n_attributes]}
#'         \item Longitudinal: \code{[n_actors × n_attributes × n_time]}
#'       }
#'       Contains numeric attributes for actors when they act as senders.
#'     }
#'     \item{\strong{Xcol}}{Receiver/column actor attributes, structured identically to Xrow.
#'       For symmetric networks, Xcol is identical to Xrow. For bipartite networks,
#'       contains attributes for the second mode.
#'     }
#'   }
#'
#'   \strong{For longitudinal data with lame = TRUE (lame package):}
#'   \describe{
#'     \item{\strong{Y}}{A list of length T (time periods), where each element is an
#'       n × n relational matrix. Actor sets can vary across time periods.}
#'     \item{\strong{Xdyad}}{A list of length T, where each element is an n × n × pd
#'       array of dyadic covariates, or NULL if none exist}
#'     \item{\strong{Xrow}}{A list of length T, where each element is an n × pr matrix
#'       of nodal row covariates, or NULL if none exist}
#'     \item{\strong{Xcol}}{A list of length T, where each element is an n × pc matrix
#'       of nodal column covariates, or NULL if none exist}
#'   }
#'
#' @details
#' \strong{Variable requirements:}
#' \itemize{
#'   \item All nodal attributes must be numeric (integer or double)
#'   \item Character or factor variables must be converted before using this function
#'   \item Missing values (NA) are preserved and can be handled by amen's models
#' }
#'
#' \strong{When to use each format:}
#' \itemize{
#'   \item Use `lame = FALSE` when:
#'     \itemize{
#'       \item Actor composition is constant across time
#'     }
#'   \item Use `lame = TRUE` when:
#'     \itemize{
#'       \item Actors enter/exit the network over time
#'       \item Want access to other features in lame
#'     }
#' }
#'
#' @note
#' The function performs several validation checks:
#' \itemize{
#'   \item Ensures single-layer networks (multilayer not supported).
#'     For multilayer networks, first extract individual layers using
#'     \code{\link{subset_netify}} (e.g., \code{subset(net, layers = "trade")}).
#'   \item Verifies all nodal attributes are numeric
#'   \item Maintains actor ordering from the original netify object
#' }
#'
#' For multilayer longitudinal models that require a 4D array
#' \code{[n, n, p, T]}, see \code{\link{netify_to_dbn}} instead.
#'
#' \strong{Bipartite networks.} `amen::ame()` does not accept
#' rectangular Y matrices; passing the output of `to_amen()` on a
#' bipartite netify to `amen::ame()` will fail. Use
#' \code{\link{netify_to_lame}} (which sets `mode = "bipartite"` and
#' targets `lame::ame()`) for bipartite networks instead.
#'
#' @examples
#' # Load example data
#' data(icews)
#'
#' # Create a netify object
#' net <- netify(
#'     icews[icews$year == 2010, ],
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # Convert to amen format (standard)
#' amen_data <- netify_to_amen(net)
#' names(amen_data) # Y, Xdyad, Xrow, Xcol
#'
#' \dontrun{
#' # For longitudinal data with time-varying composition
#' longit_net <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # Convert to lame format
#' lame_data <- netify_to_amen(longit_net, lame = TRUE)
#' }
#'
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @export netify_to_amen
#' @aliases to_amen

netify_to_amen <- function(netlet, lame = FALSE) {
	# check if netify object
	netify_check(netlet)

	# if more than one layer tell user they must specify a single layer
	if (length(attributes(netlet)$layers) > 1) {
		cli::cli_abort(
			"This object has multiple layers.
	  `netify_to_amen` does not currently support multilayer `netify` inputs.
	  Please use the `subset_netify` function to create a `netify` object with a single layer."
		)
	}

	# check attributes
	nodal_data_exists <- !is.null(attr(netlet, "nodal_data"))
	dyad_data_exists <- !is.null(attr(netlet, "dyad_data"))

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

	## three cases: cross-sec/matrix, longit list, longit array
	netlet_type <- attributes(netlet)$netify_type

	# cross-sec case
	if (netlet_type == "cross_sec") {
		# if present, convert nodal_data attribute into a
		# data.matrix with rownames
		if (nodal_data_exists) {
			# organize nodal data separately for rows and cols
			n_list <- lapply(c("row", "col"), function(dlab) {
				# set up matrix to fill in for particular dim
				mat_dim <- c(
					msrmnts[[paste0("n_", dlab, "_actors")]],
					msrmnts$n_nvars
				)
				mat_actors <- msrmnts[[paste0(dlab, "_actors")]]
				mat_labs <- list(mat_actors, msrmnts$nvars)
				mat <- array(NA, mat_dim, dimnames = mat_labs)

				# pull nodal_data and restrict to actors in this mode
				to_add <- attr(netlet, "nodal_data")
				keep <- to_add$actor %in% mat_actors
				to_add <- to_add[keep, , drop = FALSE]
				if (nrow(to_add) == 0L) return(mat)
				to_add_rows <- to_add$actor
				to_add <- to_add[, msrmnts$nvars, drop = FALSE]
				to_add <- data.matrix(to_add)
				rownames(to_add) <- to_add_rows
				mat[to_add_rows, ] <- to_add
				return(mat)
			})
		} else {
			n_list <- list(NULL, NULL)
		}
		names(n_list) <- c("row", "col")

		if (dyad_data_exists) {
			# create dyad array
			var_matrices <- attr(netlet, "dyad_data")[[1]]
			first_matrix <- var_matrices[[1]]
			dyad_arr <- array(
				data = unlist(var_matrices, use.names = FALSE),
				dim = c(
					nrow(first_matrix), ncol(first_matrix),
					length(var_matrices)
				),
				dimnames = list(
					rownames(first_matrix), colnames(first_matrix),
					names(var_matrices)
				)
			)
		} else {
			dyad_arr <- NULL
		}

		# cross-sec output
		out <- list(
			Y = get_raw(netlet),
			Xdyad = dyad_arr,
			Xrow = n_list$"row",
			Xcol = n_list$"col"
		)
	}

	# longitudinal cases
	if (netlet_type %in% c("longit_array", "longit_list")) {
		# array format for standard amen
		if (!lame) {
			if (netlet_type == "longit_array") {
				out <- list(
					Y = get_raw(netlet),
					Xdyad = longit_dyad_to_arr(netlet),
					Xrow = longit_nodal_to_arr(netlet)$"row",
					Xcol = longit_nodal_to_arr(netlet)$"col"
				)
			}

			if (netlet_type == "longit_list") {
				out <- list(
					Y = longit_dv_to_arr(netlet),
					Xdyad = longit_dyad_to_arr(netlet),
					Xrow = longit_nodal_to_arr(netlet)$"row",
					Xcol = longit_nodal_to_arr(netlet)$"col"
				)
			}
		}

		# list format for lame
		if (lame) {
			# get raw data
			raw_data <- get_raw(netlet)

			# convert to list if it's an array
			if (netlet_type == "longit_array") {
				Y_list <- lapply(seq_len(dim(raw_data)[3]), function(t) raw_data[, , t])
				names(Y_list) <- dimnames(raw_data)[[3]]
			} else {
				Y_list <- raw_data
			}

			# convert dyadic data to list format
			Xdyad_list <- NULL
			if (dyad_data_exists) {
				dyad_data <- attr(netlet, "dyad_data")
				Xdyad_list <- lapply(names(dyad_data), function(t) {
					var_matrices <- dyad_data[[t]]
					if (length(var_matrices) > 0) {
						array(
							data = unlist(var_matrices, use.names = FALSE),
							dim = c(nrow(var_matrices[[1]]), ncol(var_matrices[[1]]), length(var_matrices)),
							dimnames = list(
								rownames(var_matrices[[1]]),
								colnames(var_matrices[[1]]),
								names(var_matrices)
							)
						)
					} else {
						NULL
					}
				})
				names(Xdyad_list) <- names(dyad_data)
			}

			# convert nodal data to list format
			Xrow_list <- NULL
			Xcol_list <- NULL
			if (nodal_data_exists) {
				nodal_df <- attr(netlet, "nodal_data")
				time_periods <- names(Y_list)
				is_bipartite <- identical(attr(netlet, "mode"), "bipartite")

				# pre-compute row/col actor sets for bipartite mode split
				row_actor_sets <- if (is_bipartite) {
					if (is.list(msrmnts$row_actors)) msrmnts$row_actors
					else rep(list(msrmnts$row_actors), length(time_periods))
				} else NULL
				col_actor_sets <- if (is_bipartite) {
					if (is.list(msrmnts$col_actors)) msrmnts$col_actors
					else rep(list(msrmnts$col_actors), length(time_periods))
				} else NULL
				if (!is.null(row_actor_sets)) names(row_actor_sets) <- time_periods
				if (!is.null(col_actor_sets)) names(col_actor_sets) <- time_periods

				build_slice <- function(t, keep_actors) {
					subset_df <- nodal_df[nodal_df$time == t, , drop = FALSE]
					if (!is.null(keep_actors)) {
						subset_df <- subset_df[
							as.character(subset_df$actor) %in% as.character(keep_actors),
							, drop = FALSE
						]
					}
					mat <- as.matrix(subset_df[, msrmnts$nvars, drop = FALSE])
					rownames(mat) <- subset_df$actor
					mat
				}

				Xrow_list <- lapply(time_periods, function(t) {
					build_slice(t, if (is_bipartite) row_actor_sets[[t]] else NULL)
				})
				names(Xrow_list) <- time_periods

				Xcol_list <- if (is_bipartite) {
					out <- lapply(time_periods, function(t) {
						build_slice(t, col_actor_sets[[t]])
					})
					names(out) <- time_periods
					out
				} else {
					Xrow_list
				}
			}

			out <- list(
				Y = Y_list,
				Xdyad = Xdyad_list,
				Xrow = Xrow_list,
				Xcol = Xcol_list
			)
		}
	}

	#
	return(out)
}

#' @rdname netify_to_amen
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
to_amen <- netify_to_amen
