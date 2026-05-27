#' Combine multiple netify objects
#'
#' `bind_netifies()` concatenates two or more netify objects along
#' the time axis (for combining cross-sec → longit, or stacking
#' two longit panels into a longer one). All inputs must share the
#' same mode (`unipartite` / `bipartite`), symmetry, layers, and
#' (for cross-sec inputs) the same actor set. Actor sets across
#' periods may differ — the result is a `longit_list`.
#'
#' For combining different *layers* of the same time slice, use
#' `layer_netify()`.
#'
#' @param ... Two or more netify objects, or a single list of netify
#' objects.
#' @param names Optional character vector to name the resulting
#' periods. If `NULL`, periods are auto-named from the inputs'
#' existing period labels (with deduplication if collisions).
#' @param align_actors One of `"none"` (default), `"union"`, or
#' `"intersection"`. Controls how per-period actor sets are
#' reconciled when inputs differ:
#' - `"none"`: keep each period's actor set as supplied; resulting
#'   `longit_list` periods may have different dimensions (matches
#'   prior behavior).
#' - `"union"`: take the union of actor sets across all inputs and
#'   pad each period with NA rows/columns for actors not originally
#'   present.
#' - `"intersection"`: take the intersection of actor sets across
#'   all inputs and subset each period to only those actors.
#' @return A `longit_list` netify object.
#'
#' @details
#' This is **not** the same as `layer_netify()`:
#' - `bind_netifies()` joins along TIME.
#' - `layer_netify()` joins along RELATION (layer).
#'
#' Nodal and dyadic attributes are concatenated per-period; if two
#' inputs supply conflicting values for the same (actor, time), the
#' later input wins (with a one-shot inform).
#'
#' When downstream models (e.g., `tergm` CMLE) require uniform
#' actor composition across periods, use `align_actors = "union"`
#' or `"intersection"`.
#'
#' @examples
#' \dontrun{
#' data(icews)
#' n1 <- netify(icews[icews$year == 2010, ],
#' actor1 = "i", actor2 = "j", symmetric = FALSE,
#' weight = "verbCoop")
#' n2 <- netify(icews[icews$year == 2011, ],
#' actor1 = "i", actor2 = "j", symmetric = FALSE,
#' weight = "verbCoop")
#' combined <- bind_netifies(n1, n2, names = c("2010", "2011"))
#' summary(combined)
#' }
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export bind_netifies
bind_netifies <- function(
	...,
	names = NULL,
	align_actors = c("none", "union", "intersection")) {
	align_actors <- match.arg(align_actors)
	# deparsed arg labels for default cross-sec period names
	mc <- match.call(expand.dots = FALSE)
	dot_labels <- if (!is.null(mc$`...`)) {
		vapply(mc$`...`,
			function(a) paste(deparse(a), collapse = " "),
			character(1))
	} else character(0)

	dots <- list(...)
	# accept a single list of netifies
	if (length(dots) == 1L && is.list(dots[[1]]) && !is_netify(dots[[1]])) {
		list_in <- dots[[1]]
		dots <- list_in
		if (!is.null(base::names(list_in)) && all(nzchar(base::names(list_in)))) {
			dot_labels <- base::names(list_in)
		} else {
			dot_labels <- paste0("input_", seq_along(dots))
		}
	}
	if (length(dots) < 2L) {
		cli::cli_abort("{.fn bind_netifies} requires at least 2 netify objects.")
	}
	if (!all(vapply(dots, is_netify, logical(1)))) {
		cli::cli_abort("All inputs to {.fn bind_netifies} must be netify objects.")
	}
	# pad dot_labels to match dots length
	if (length(dot_labels) < length(dots)) {
		dot_labels <- c(dot_labels,
			paste0("input_", seq(length(dot_labels) + 1L, length(dots))))
	}
	# compatibility checks
	modes <- vapply(dots, function(n) attr(n, "mode"), character(1))
	if (length(unique(modes)) > 1L) {
		cli::cli_abort(c(
			"x" = "All inputs must share the same {.arg mode}.",
			"i" = "Got: {.val {modes}}."
		))
	}
	layers <- lapply(dots, function(n) attr(n, "layers"))
	if (length(unique(vapply(layers, length, integer(1)))) > 1L) {
		cli::cli_abort("All inputs must have the same number of layers.")
	}
	# build per-period matrix list from each input
	per_input <- lapply(dots, function(n) {
		nt <- attr(n, "netify_type")
		switch(nt,
			"cross_sec"    = list(get_raw(n)),
			"longit_array" = {
				arr <- get_raw(n)
				lapply(seq_len(dim(arr)[3]), function(t) arr[, , t])
			},
			"longit_list"  = lapply(n, get_raw)
		)
	})
	# default period names per input
	per_input_names <- lapply(seq_along(dots), function(i) {
		n <- dots[[i]]
		nt <- attr(n, "netify_type")
		nm <- switch(nt,
			"cross_sec"    = dot_labels[i],
			"longit_array" = dimnames(get_raw(n))[[3]] %||% paste0(dot_labels[i], "_", seq_len(dim(get_raw(n))[3])),
			"longit_list"  = names(n) %||% paste0(dot_labels[i], "_", seq_along(n))
		)
		nm
	})
	all_mats  <- do.call(c, per_input)
	all_names <- if (is.null(names)) do.call(c, per_input_names) else as.character(names)
	if (length(all_names) != length(all_mats)) {
		cli::cli_abort(c(
			"x" = "{.arg names} length ({length(all_names)}) does not match total periods ({length(all_mats)}).",
			"i" = "Pass exactly one name per resulting time slice."
		))
	}
	# disambiguate duplicate names
	if (any(duplicated(all_names))) {
		cli::cli_inform(c(
			"i" = "Duplicate period names detected; suffixing with `_2`, `_3`, ... to disambiguate."
		),
		.frequency = "once",
		.frequency_id = "bind_netifies_dedup")
		all_names <- make.unique(all_names, sep = "_")
	}
	names(all_mats) <- all_names

	# detect non-uniform actor sets across periods
	row_sets <- lapply(all_mats, rownames)
	col_sets <- lapply(all_mats, colnames)
	differs <- !all(vapply(row_sets, identical, logical(1), row_sets[[1]])) ||
		!all(vapply(col_sets, identical, logical(1), col_sets[[1]]))

	# realign per-period actor sets when requested
	if (differs && align_actors == "union") {
		row_union <- unique(unlist(row_sets, use.names = FALSE))
		col_union <- unique(unlist(col_sets, use.names = FALSE))
		all_mats <- lapply(all_mats, pad_matrix_to, row_union, col_union)
	} else if (differs && align_actors == "intersection") {
		row_inter <- Reduce(intersect, row_sets)
		col_inter <- Reduce(intersect, col_sets)
		if (length(row_inter) == 0L || length(col_inter) == 0L) {
			cli::cli_abort(c(
				"x" = "{.code align_actors = \"intersection\"} produced an empty actor set.",
				"i" = "At least one input has no actors in common with the others."
			))
		}
		all_mats <- lapply(all_mats, function(m) {
			m[row_inter, col_inter, drop = FALSE]
		})
	} else if (differs && align_actors == "none") {
		cli::cli_inform(c(
			"i" = "Actor sets differ across inputs; result is a {.cls longit_list} with non-uniform composition.",
			"i" = "Set {.code align_actors = \"union\"} to pad missing actors with NA, or {.code align_actors = \"intersection\"} to keep only shared actors."
		),
		.frequency = "once",
		.frequency_id = "bind_netifies_actor_diff")
	}

	# assemble longit_list netlet
	first <- dots[[1]]
	sym  <- if (length(attr(first, "symmetric")) > 1) attr(first, "symmetric")[1] else attr(first, "symmetric")
	out <- suppressMessages(suppressWarnings(
		new_netify(all_mats,
			mode = attr(first, "mode"),
			symmetric = sym,
			weight = attr(first, "weight"),
			diag_to_NA = isTRUE(attr(first, "diag_to_NA"))
		)
	))

	# attach combined nodal_data
	combined_nodal <- combine_input_nodal_data(dots, per_input_names)
	if (!is.null(combined_nodal) && nrow(combined_nodal) > 0L) {
		# reconcile nodal_data with realigned actor sets per period
		if (differs && align_actors %in% c("union", "intersection")) {
			combined_nodal <- reconcile_nodal_to_actors(
				combined_nodal, all_mats, align_actors
			)
		}
		attr(out, "nodal_data") <- combined_nodal
	}
	# attach combined dyad_data keyed by period
	combined_dyad <- combine_input_dyad_data(dots, per_input_names)
	if (!is.null(combined_dyad) && length(combined_dyad) > 0L) {
		attr(out, "dyad_data") <- combined_dyad
	}

	out
}

#' Reconcile combined nodal_data to per-period actor sets
#'
#' When `bind_netifies(align_actors = "union")` pads matrices with NA
#' rows for missing actors, the nodal_data is missing rows for those
#' new actors — downstream `to_statnet()` then sees NA vertex
#' attributes. Conversely, `align_actors = "intersection"` shrinks the
#' matrices but leaves the nodal_data unchanged, so it carries rows for
#' actors no longer present. This helper realigns the nodal_data per
#' period to exactly the actor set of the corresponding matrix.
#'
#' @keywords internal
#' @noRd
reconcile_nodal_to_actors <- function(nd, all_mats, align_actors) {
	if (!"time" %in% names(nd)) return(nd)
	out_frames <- list()
	for (k in seq_along(all_mats)) {
		period <- names(all_mats)[k]
		target_actors <- rownames(all_mats[[k]])
		if (is.null(target_actors)) next
		sub <- nd[as.character(nd$time) == as.character(period), , drop = FALSE]
		if (align_actors == "intersection") {
			sub <- sub[as.character(sub$actor) %in% target_actors, , drop = FALSE]
		} else if (align_actors == "union") {
			present <- as.character(sub$actor)
			missing_actors <- setdiff(target_actors, present)
			if (length(missing_actors) > 0L) {
				pad <- sub[rep(NA_integer_, length(missing_actors)), , drop = FALSE]
				pad$actor <- missing_actors
				pad$time <- period
				sub <- rbind(sub, pad)
			}
		}
		out_frames[[length(out_frames) + 1L]] <- sub
	}
	if (length(out_frames) == 0L) return(nd)
	do.call(rbind, c(out_frames, list(make.row.names = FALSE)))
}

#' Pad a matrix to a target row/column actor set with NAs
#'
#' Used by `bind_netifies(align_actors = "union")` to bring every
#' period to a common dimnames so longitudinal models (e.g., tergm
#' CMLE) see a uniform actor composition.
#'
#' @keywords internal
#' @noRd
pad_matrix_to <- function(m, row_target, col_target) {
	# fast path when already aligned
	if (identical(rownames(m), row_target) && identical(colnames(m), col_target)) {
		return(m)
	}
	out <- matrix(
		NA_real_,
		nrow = length(row_target),
		ncol = length(col_target),
		dimnames = list(row_target, col_target)
	)
	# preserve attributes other than dim/dimnames from source
	r_keep <- intersect(rownames(m), row_target)
	c_keep <- intersect(colnames(m), col_target)
	if (length(r_keep) > 0L && length(c_keep) > 0L) {
		out[r_keep, c_keep] <- m[r_keep, c_keep]
	}
	out
}

#' Combine per-input nodal_data frames into one (actor, time) frame
#'
#' Builds a single long-format nodal_data frame keyed by `actor` and
#' `time` from each input's existing nodal_data, stamping the per-input
#' period labels onto cross-sec inputs. Returns NULL if no input
#' carried nodal_data.
#'
#' @keywords internal
#' @noRd
combine_input_nodal_data <- function(dots, per_input_names) {
	frames <- list()
	for (i in seq_along(dots)) {
		nd <- attr(dots[[i]], "nodal_data")
		if (is.null(nd) || !is.data.frame(nd) || nrow(nd) == 0L) next
		nt <- attr(dots[[i]], "netify_type")
		if (nt == "cross_sec") {
			# stamp output period name onto cross-sec nodal_data
			nd$time <- per_input_names[[i]][1]
		} else {
			# longit inputs carry their own time column
			if (!"time" %in% names(nd)) {
				nd$time <- per_input_names[[i]][1]
			}
		}
		frames[[length(frames) + 1L]] <- nd
	}
	if (length(frames) == 0L) return(NULL)
	# rbind with union of columns, padding missing with NA
	all_cols <- unique(unlist(lapply(frames, names)))
	frames_padded <- lapply(frames, function(df) {
		missing_c <- setdiff(all_cols, names(df))
		for (m in missing_c) df[[m]] <- NA
		df[, all_cols, drop = FALSE]
	})
	do.call(rbind, c(frames_padded, list(make.row.names = FALSE)))
}

#' Combine per-input dyad_data lists into one period-keyed list
#'
#' Each input's `dyad_data` is a list keyed by period name; for
#' cross-sec inputs we stamp the output period name as the key.
#' Conflicts (same period, same var) are resolved later-wins.
#'
#' @keywords internal
#' @noRd
combine_input_dyad_data <- function(dots, per_input_names) {
	out <- list()
	for (i in seq_along(dots)) {
		dd <- attr(dots[[i]], "dyad_data")
		if (is.null(dd) || length(dd) == 0L) next
		nt <- attr(dots[[i]], "netify_type")
		if (nt == "cross_sec") {
			out[[per_input_names[[i]][1]]] <- if (is.list(dd) && length(dd) == 1L) dd[[1]] else dd
		} else {
			# keep per-period keys from longit inputs
			for (k in names(dd)) {
				out[[k]] <- dd[[k]]
			}
		}
	}
	if (length(out) == 0L) NULL else out
}

#' Merge method for netify objects (S3 alias for `bind_netifies`)
#'
#' Provides the base-R `merge()` generic dispatch for combining two
#' netify objects along the time axis. For more than two inputs or
#' programmatic use, prefer `bind_netifies()` directly.
#'
#' @param x,y netify objects.
#' @param ... Additional netify objects, or a `names =` argument.
#' @return A `longit_list` netify object.
#' @method merge netify
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
merge.netify <- function(x, y, ...) {
	bind_netifies(x, y, ...)
}

#' Convert a netify object to a tibble (long edge frame)
#'
#' S3 method for `tibble::as_tibble()`. Returns the same long-format
#' frame as [unnetify()] / [tidy.netify()], wrapped in a tibble for
#' tidyverse-pipe friendliness.
#'
#' Registered against the `tibble::as_tibble` generic via `.onLoad`,
#' so `tibble` is not a hard dependency. When `tibble` isn't
#' installed, a plain data.frame is returned.
#'
#' @param x A netify object.
#' @param ... Passed to [unnetify()] (e.g., `remove_zeros = TRUE`).
#' @return A tibble (or data.frame if tibble isn't installed) with
#' one row per dyad. Includes `from`, `to`, optional `time`/`layer`,
#' the edge weight, dyadic covariates, and nodal covariates merged
#' in with `_from` / `_to` suffixes.
#'
#' @seealso [tidy.netify()] for the broom-style sibling and
#' [unnetify()] for the underlying converter.
#'
#' @examples
#' \dontrun{
#' data(icews)
#' icews_10 <- icews[icews$year == 2010, ]
#' net <- netify(icews_10, actor1 = "i", actor2 = "j",
#'     symmetric = FALSE, weight = "verbCoop")
#' tibble::as_tibble(net)
#' }
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
as_tibble.netify <- function(x, ...) {
	netify_check(x)
	df <- unnetify(x, ...)
	df <- strip_tidy_internal_cols(df, x)
	if (requireNamespace("tibble", quietly = TRUE)) {
		tibble::as_tibble(df)
	} else {
		df
	}
}

# drop columns that only exist as internal join keys / placeholder time
# stamps in unnetify(): from_id/to_id are paste(from, time)/paste(to, time)
# join keys (useless once nodal vars are merged in), and `time` is a
# constant "1" placeholder for cross-sectional networks. keep the time
# column for longitudinal data since it carries real information.
strip_tidy_internal_cols <- function(df, netlet) {
	if (!is.data.frame(df) || nrow(df) == 0L && ncol(df) == 0L) return(df)
	nt <- attr(netlet, "netify_type")
	drop_cols <- c("from_id", "to_id")
	if (!is.null(nt) && identical(nt, "cross_sec")) {
		drop_cols <- c(drop_cols, "time")
	}
	keep <- setdiff(names(df), drop_cols)
	df[, keep, drop = FALSE]
}
