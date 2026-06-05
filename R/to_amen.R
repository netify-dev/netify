#' Convert netify objects to amen format
#'
#' `netify_to_amen` (also available as `to_amen`)
#' transforms netify network objects into the data structure required by the amen
#' package for advanced network modeling. this enables the use of social relations
#' models (srm), additive and multiplicative effects (ame) models, and other
#' network regression approaches implemented in amen.
#'
#' @param netlet a netify object (class "netify") containing network data. must be
#'   a single-layer network. for multilayer networks, first extract individual
#'   layers using \code{\link{subset_netify}}.
#' @param lame logical. controls the output format for longitudinal data:
#'   \itemize{
#'     \item \code{FALSE} (default): formats output for compatibility with the
#'       standard version of the amen package, which uses array structures.
#'       y is returned as a 3d array \code{[n_actors x n_actors x n_time]},
#'       xdyad as a 4d array \code{[n_actors x n_actors x n_covariates x n_time]},
#'       and xrow/xcol as 3d arrays \code{[n_actors x n_attributes x n_time]}.
#'       this requires constant actor composition across time.
#'     \item \code{TRUE}: formats output for compatibility with the netify-verse
#'       version called lame, which supports
#'       longitudinal network modeling with time-varying actor compositions.
#'       y is returned as a list of t matrices (one per time period),
#'       xdyad as a list of t 3d arrays, and xrow/xcol as lists of t matrices.
#'       actor sets can vary across time periods, making this suitable for
#'       panels where countries enter/exit.
#'   }
#'   this parameter is ignored for cross-sectional data.
#'
#' @return the structure of the returned list depends on the data type and `lame` parameter:
#'
#'   \strong{for cross-sectional data or longitudinal with lame = FALSE (standard amen):}
#'   \describe{
#'     \item{\strong{y}}{network adjacency data as a numeric matrix or array:
#'       \itemize{
#'         \item cross-sectional: matrix of dimensions \code{[n_actors x n_actors]}
#'         \item longitudinal: array of dimensions \code{[n_actors x n_actors x n_time]}
#'       }
#'       contains edge weights or binary indicators. missing edges are preserved as na.
#'     }
#'     \item{\strong{xdyad}}{dyadic covariates as an array, or NULL if none exist:
#'       \itemize{
#'         \item cross-sectional: \code{[n_actors x n_actors x n_covariates]}
#'         \item longitudinal: \code{[n_actors x n_actors x n_covariates x n_time]}
#'       }
#'       each slice contains one dyadic covariate across all actor pairs.
#'     }
#'     \item{\strong{xrow}}{sender/row actor attributes as a matrix or array, or NULL if none exist:
#'       \itemize{
#'         \item cross-sectional: \code{[n_actors x n_attributes]}
#'         \item longitudinal: \code{[n_actors x n_attributes x n_time]}
#'       }
#'       contains numeric attributes for actors when they act as senders.
#'     }
#'     \item{\strong{xcol}}{receiver/column actor attributes, structured identically to xrow.
#'       for symmetric networks, xcol is identical to xrow. for bipartite networks,
#'       contains attributes for the second mode.
#'     }
#'   }
#'
#'   \strong{for longitudinal data with lame = TRUE (lame package):}
#'   \describe{
#'     \item{\strong{y}}{a list of length t (time periods), where each element is an
#'       n x n relational matrix. actor sets can vary across time periods.}
#'     \item{\strong{xdyad}}{a list of length t, where each element is an n x n x pd
#'       array of dyadic covariates, or NULL if none exist}
#'     \item{\strong{xrow}}{a list of length t, where each element is an n x pr matrix
#'       of nodal row covariates, or NULL if none exist}
#'     \item{\strong{xcol}}{a list of length t, where each element is an n x pc matrix
#'       of nodal column covariates, or NULL if none exist}
#'   }
#'
#' @details
#' \strong{variable requirements:}
#' \itemize{
#'   \item all nodal attributes must be numeric (integer or double)
#'   \item all dyadic attributes must be numeric or logical matrices
#'   \item character or factor variables must be converted before using this function
#'   \item missing values (na) are preserved and can be handled by amen's models
#' }
#'
#' \strong{when to use each format:}
#' \itemize{
#'   \item use `lame = FALSE` when:
#'     \itemize{
#'       \item actor composition is constant across time
#'     }
#'   \item use `lame = TRUE` when:
#'     \itemize{
#'       \item actors enter/exit the network over time
#'       \item want access to other features in lame
#'     }
#' }
#'
#' @note
#' the function performs several validation checks:
#' \itemize{
#'   \item ensures single-layer networks (multilayer not supported).
#'     for multilayer networks, first extract individual layers using
#'     \code{\link{subset_netify}} (e.g., \code{subset(net, layers = "trade")}).
#'   \item verifies all nodal and dyadic attributes are model-ready numeric inputs
#'   \item maintains actor ordering from the original netify object
#' }
#'
#' for multilayer longitudinal models that require a 4d array
#' \code{[n, n, p, t]}, see \code{\link{netify_to_dbn}} instead.
#'
#' \strong{bipartite networks.} `amen::ame()` does not accept
#' rectangular y matrices; passing the output of `to_amen()` on a
#' bipartite netify to `amen::ame()` will fail. use
#' \code{\link{netify_to_lame}} (which sets `mode = "bipartite"` and
#' targets `lame::ame()`) for bipartite networks instead.
#'
#' @examples
#' # load example data
#' data(icews)
#'
#' # create a netify object
#' net <- netify(
#'     icews[icews$year == 2010, ],
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # convert to amen format (standard)
#' amen_data <- netify_to_amen(net)
#' names(amen_data) # y, xdyad, xrow, xcol
#'
#' \dontrun{
#' # for longitudinal data with time-varying composition
#' longit_net <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # convert to lame format
#' lame_data <- netify_to_amen(longit_net, lame = TRUE)
#' }
#'
#' @author ha eun choi, cassy dorff, colin henry, shahryar minhas
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

	# make sure nodal and dyadic attributes are numeric
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

#' @keywords internal
#' @noRd
validate_numeric_dyad_data <- function(dyad_data) {
	bad <- character(0)
	for (period in names(dyad_data)) {
		vars <- dyad_data[[period]]
		for (var in names(vars)) {
			x <- vars[[var]]
			if (!(is.numeric(x) || is.logical(x))) {
				bad <- c(bad, paste0(var, " (", period, ")"))
			}
		}
	}
	if (length(bad) > 0L) {
		cli::cli_abort(c(
			"x" = "All dyadic attributes exported to modeling packages must be numeric or logical.",
			"i" = "Non-numeric dyadic attribute{?s}: {.val {bad}}."
		))
	}
	invisible(NULL)
}

#' @rdname netify_to_amen
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
to_amen <- netify_to_amen
