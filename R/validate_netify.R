#' Deep coherence check on a netify object
#'
#' `netify_check()` validates only class membership. `validate_netify()`
#' goes further: it verifies that the netify's internal pieces still
#' agree with each other after any user-side surgery (e.g., manually
#' edited `attr(., "nodal_data")`, `subset()` followed by an attribute
#' overwrite, etc.). Use this when you've hand-modified a netlet and
#' want to confirm it's still well-formed before passing to
#' `to_statnet()` / `to_amen()` / `plot()`.
#'
#' @param netlet A netify object.
#' @param verbose Logical. If `TRUE` (default), print a per-check
#' status banner; otherwise return silently.
#' @return Invisibly returns a list with one logical per check (`TRUE`
#' = passed). The function `cli::cli_abort()`s on any failure unless
#' `verbose = TRUE`, in which case failures are reported per-check
#' and the function returns invisibly with the full failure list.
#'
#' @examples
#' \dontrun{
#' data(icews)
#' net <- netify(icews[icews$year == 2010, ],
#' actor1 = "i", actor2 = "j", symmetric = FALSE, weight = "verbCoop")
#' validate_netify(net)
#' }
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export validate_netify
validate_netify <- function(netlet, verbose = TRUE) {
	netify_check(netlet)
	obj_attrs <- attributes(netlet)
	checks <- list()

	# netify_type is one of the three allowed
	checks$netify_type <- isTRUE(obj_attrs$netify_type %in%
		c("cross_sec", "longit_array", "longit_list"))

	# mode is a length-1 character matching the allowed values
	checks$mode <- is.character(obj_attrs$mode) &&
		length(obj_attrs$mode) == 1L &&
		isTRUE(obj_attrs$mode %in% c("unipartite", "bipartite"))

	# symmetric attribute is logical and non-NA
	sym_attr <- obj_attrs$symmetric
	checks$symmetric_type <- is.logical(sym_attr) &&
		length(sym_attr) >= 1L &&
		!anyNA(sym_attr)

	# layers count matches the raw data layout
	raw_err <- NULL
	raw <- tryCatch(get_raw(netlet),
		error = function(e) { raw_err <<- conditionMessage(e); NULL })
	checks$raw_extractable <- is.null(raw_err)
	nlayers <- length(obj_attrs$layers)
	if (!is.null(raw)) {
		ml_dim <- if (is.array(raw)) {
			# 3D cross-sec is [r,c,layer]; 4D longit is [r,c,layer,t]
			d <- dim(raw)
			if (length(d) == 2) 1L
			else if (length(d) == 3 && obj_attrs$netify_type == "cross_sec") d[3]
			else if (length(d) == 4) d[3]
			else 1L
		} else if (is.list(raw)) {
			first <- raw[[1]]
			if (is.array(first) && length(dim(first)) == 3L) dim(first)[3] else 1L
		} else 1L
		checks$layers_consistent <- (nlayers == ml_dim) ||
			(nlayers == 0 && ml_dim == 1)
	} else {
		checks$layers_consistent <- TRUE
	}

	# nodal_data actors must exist in the netlet
	nd <- obj_attrs$nodal_data
	if (!is.null(nd) && is.data.frame(nd) && "actor" %in% names(nd)) {
		known_actors <- if (!is.null(raw)) {
			if (is.matrix(raw)) rownames(raw)
			else if (is.array(raw)) dimnames(raw)[[1]]
			else if (is.list(raw)) unique(unlist(lapply(raw, rownames)))
			else NULL
		} else NULL
		if (!is.null(known_actors)) {
			# bipartite uses both row and col actors
			if (identical(obj_attrs$mode, "bipartite")) {
				col_actors <- if (is.matrix(raw)) colnames(raw)
					else if (is.array(raw)) dimnames(raw)[[2]]
					else if (is.list(raw)) unique(unlist(lapply(raw, colnames)))
					else NULL
				known_actors <- unique(c(known_actors, col_actors))
			}
			stray <- setdiff(unique(as.character(nd$actor)), known_actors)
			checks$nodal_actors_known <- length(stray) == 0
			if (!checks$nodal_actors_known && verbose) {
				cli::cli_alert_warning(c(
					"!" = "nodal_data references {length(stray)} actor{?s} not in the netlet: {.val {head(stray, 5)}}{if (length(stray) > 5) ' ...'}"
				))
			}
		} else {
			checks$nodal_actors_known <- TRUE
		}
	} else {
		checks$nodal_actors_known <- TRUE
	}

	# is_binary attribute matches the actual values
	ib_attr <- obj_attrs$is_binary
	if (!is.null(raw) && !is.null(ib_attr)) {
		vals <- if (is.list(raw)) unlist(lapply(raw, as.numeric), use.names = FALSE)
				else as.numeric(raw)
		vals <- vals[!is.na(vals)]
		actual_binary <- length(vals) == 0 || all(vals %in% c(0, 1))
		checks$is_binary_consistent <- isTRUE(all(ib_attr) == actual_binary)
	} else {
		checks$is_binary_consistent <- TRUE
	}

	# symmetric attribute matches matrix content for square cross-sec
	if (!is.null(raw) && is.matrix(raw) && nrow(raw) == ncol(raw)) {
		content_sym <- isSymmetric(unname(raw))
		stored_sym <- if (length(obj_attrs$symmetric) > 1) {
			obj_attrs$symmetric[1]
		} else obj_attrs$symmetric
		# directed storage of a symmetric matrix is fine
		checks$symmetric_consistent <- !isTRUE(stored_sym) || content_sym
	} else {
		checks$symmetric_consistent <- TRUE
	}

	# unipartite cross-sec must have matching row and col dimnames
	if (!is.null(raw) && is.matrix(raw) &&
		identical(obj_attrs$mode, "unipartite") && nrow(raw) == ncol(raw)) {
		checks$unipartite_dimnames <- identical(rownames(raw), colnames(raw))
	} else {
		checks$unipartite_dimnames <- TRUE
	}

	# longit_list slices must have row/col agreement when unipartite
	if (identical(obj_attrs$netify_type, "longit_list") &&
		is.list(raw) && length(raw) > 1L) {
		row_sets <- lapply(raw, rownames)
		col_sets <- lapply(raw, colnames)
		if (identical(obj_attrs$mode, "unipartite")) {
			ok <- vapply(seq_along(raw), function(i) {
				identical(row_sets[[i]], col_sets[[i]])
			}, logical(1))
			checks$slice_dimnames_consistent <- all(ok)
		} else {
			checks$slice_dimnames_consistent <- TRUE
		}
	} else {
		checks$slice_dimnames_consistent <- TRUE
	}

	# longitudinal nodal_data time values must be in the netlet period set
	is_longit <- identical(obj_attrs$netify_type, "longit_array") ||
		identical(obj_attrs$netify_type, "longit_list")
	if (is_longit && !is.null(nd) && is.data.frame(nd) && "time" %in% names(nd)) {
		net_times <- if (identical(obj_attrs$netify_type, "longit_array") &&
			is.array(raw)) {
			dimnames(raw)[[length(dim(raw))]]
		} else if (identical(obj_attrs$netify_type, "longit_list") &&
			is.list(raw)) {
			names(raw)
		} else NULL
		if (!is.null(net_times)) {
			stray_t <- setdiff(unique(as.character(nd$time)), as.character(net_times))
			checks$nodal_time_known <- length(stray_t) == 0
			if (!checks$nodal_time_known && verbose) {
				cli::cli_alert_warning(c(
					"!" = "nodal_data references {length(stray_t)} time label{?s} not in the netlet: {.val {head(stray_t, 5)}}{if (length(stray_t) > 5) ' ...'}"
				))
			}
		} else {
			checks$nodal_time_known <- TRUE
		}
	} else {
		checks$nodal_time_known <- TRUE
	}

	failed <- names(checks)[!vapply(checks, isTRUE, logical(1))]
	if (verbose) {
		if (length(failed) == 0) {
			cli::cli_alert_success("netify object passes all coherence checks ({length(checks)} checks).")
		} else {
			cli::cli_alert_danger("netify object failed {length(failed)} coherence check{?s}: {.val {failed}}.")
		}
	} else if (length(failed) > 0) {
		cli::cli_abort(c(
			"x" = "netify object failed coherence checks: {.val {failed}}.",
			"i" = "Pass {.code verbose = TRUE} for per-check diagnostics."
		))
	}
	invisible(checks)
}
