#' Combine multiple netify objects
#'
#' `bind_netifies()` concatenates two or more netify objects along
#' the time axis (for combining cross-sec -> longit, or stacking
#' two longit panels into a longer one). all inputs must share the
#' same mode (`unipartite` / `bipartite`), symmetry, layers, and
#' (for cross-sec inputs) the same actor set. actor sets across
#' periods may differ -- the result is a `longit_list`.
#'
#' for combining different *layers* of the same time slice, use
#' `layer_netify()`.
#'
#' @param ... two or more netify objects, or a single list of netify
#' objects.
#' @param names optional character vector to name the resulting
#' periods. if `NULL`, periods are auto-named from the inputs'
#' existing period labels (with deduplication if collisions).
#' @param align_actors one of `"none"` (default), `"union"`, or
#' `"intersection"`. controls how per-period actor sets are
#' reconciled when inputs differ:
#' - `"none"`: keep each period's actor set as supplied; resulting
#'   `longit_list` periods may have different dimensions (matches
#'   prior behavior).
#' - `"union"`: take the union of actor sets across all inputs and
#'   pad each period with na rows/columns for actors not originally
#'   present.
#' - `"intersection"`: take the intersection of actor sets across
#'   all inputs and subset each period to only those actors.
#' @return a `longit_list` netify object.
#'
#' @details
#' this is **not** the same as `layer_netify()`:
#' - `bind_netifies()` joins along time.
#' - `layer_netify()` joins along relation (layer).
#'
#' nodal and dyadic attributes are concatenated per-period; if two
#' inputs supply conflicting values for the same (actor, time), the
#' later input wins (with a one-shot inform).
#'
#' when downstream models (e.g., `tergm` cmle) require uniform
#' actor composition across periods, use `align_actors = "union"`
#' or `"intersection"`.
#'
#' multilayer inputs are supported when each input has the same layer labels.
#' the result is a longitudinal list whose per-period elements keep the layer
#' dimension, so \code{as.matrix(x, time = ..., layer = ...)},
#' \code{unnetify()}, and \code{decompose_netify()} can still address layers.
#'
#' @examples
#' \donttest{
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
#' @author cassy dorff, shahryar minhas
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
	if (!all(vapply(layers, identical, logical(1), layers[[1]]))) {
		cli::cli_abort(c(
			"x" = "All inputs must have identical layer labels.",
			"i" = "For combining different relations as layers, use {.fn layer_netify}."
		))
	}
	check_bind_attr <- function(attr_name, label = attr_name) {
		vals <- lapply(dots, attr, which = attr_name, exact = TRUE)
		if (!all(vapply(vals, identical, logical(1), vals[[1]]))) {
			cli::cli_abort(c(
				"x" = "All inputs must share the same {.arg {label}}.",
				"i" = "Input values: {.val {vapply(vals, function(x) paste(x %||% 'NULL', collapse = ', '), character(1))}}."
			))
		}
	}
	for (nm in c("symmetric", "weight", "is_binary", "diag_to_NA", "missing_to_zero")) {
		check_bind_attr(nm)
	}
	# build per-period matrix list from each input
		per_input <- lapply(dots, function(n) {
			nt <- attr(n, "netify_type")
			switch(nt,
				"cross_sec"    = list(get_raw(n)),
				"longit_array" = {
					arr <- get_raw(n)
					if (length(dim(arr)) == 4L) {
						lapply(seq_len(dim(arr)[4]), function(t) arr[, , , t, drop = FALSE][, , , 1])
					} else {
						lapply(seq_len(dim(arr)[3]), function(t) arr[, , t])
					}
				},
				"longit_list"  = lapply(n, get_raw)
			)
	})
	# default period names per input
	per_input_names <- lapply(seq_along(dots), function(i) {
		n <- dots[[i]]
			nt <- attr(n, "netify_type")
			raw <- get_raw(n)
			nm <- switch(nt,
				"cross_sec"    = dot_labels[i],
				"longit_array" = if (length(dim(raw)) == 4L) {
					dimnames(raw)[[4]] %||% paste0(dot_labels[i], "_", seq_len(dim(raw)[4]))
				} else {
					dimnames(raw)[[3]] %||% paste0(dot_labels[i], "_", seq_len(dim(raw)[3]))
				},
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
		per_output_names <- split(
			all_names,
			rep(seq_along(per_input_names), lengths(per_input_names))
		)

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
			subset_matrix_to(m, row_inter, col_inter)
		})
	} else if (differs && align_actors == "none") {
		cli::cli_inform(c(
			"i" = "Actor sets differ across inputs; result is a {.cls longit_list} with non-uniform composition.",
			"i" = "Set {.code align_actors = \"union\"} to pad missing actors with NA, or {.code align_actors = \"intersection\"} to keep only shared actors."
		),
		.frequency = "once",
			.frequency_id = "bind_netifies_actor_diff")
	}
	output_actor_time_uniform <- actor_sets_uniform(all_mats)

			# assemble longit_list netlet
		first <- dots[[1]]
		has_multilayer_periods <- any(vapply(all_mats, function(m) length(dim(m)) == 3L, logical(1)))
		sym  <- if (length(attr(first, "symmetric")) > 1) attr(first, "symmetric")[1] else attr(first, "symmetric")
		if (has_multilayer_periods) {
			actor_periods <- lapply(all_mats, function(m) unique(c(rownames(m), colnames(m))))
			all_actors <- unique(unlist(actor_periods, use.names = FALSE))
			actor_pds <- do.call(rbind, lapply(all_actors, function(a) {
				idx <- which(vapply(actor_periods, function(x) a %in% x, logical(1)))
				data.frame(
					actor = a,
					min_time = all_names[min(idx)],
					max_time = all_names[max(idx)],
					stringsAsFactors = FALSE
				)
			}))
			out <- all_mats
			class(out) <- "netify"
				attributes(out) <- c(attributes(out), list(
					netify_type = "longit_list",
					actor_time_uniform = output_actor_time_uniform,
					actor_pds = actor_pds,
				weight = attr(first, "weight"),
				detail_weight = attr(first, "detail_weight"),
				is_binary = attr(first, "is_binary"),
				symmetric = attr(first, "symmetric"),
				mode = attr(first, "mode"),
				layers = layers[[1]],
				diag_to_NA = attr(first, "diag_to_NA"),
				missing_to_zero = if (differs && align_actors == "union") {
					FALSE
				} else {
					attr(first, "missing_to_zero")
				},
				sum_dyads = attr(first, "sum_dyads"),
				nodal_data = NULL,
				dyad_data = NULL
			))
		} else {
			out <- suppressMessages(suppressWarnings(
				new_netify(all_mats,
					mode = attr(first, "mode"),
				symmetric = sym,
				weight = attr(first, "weight"),
				diag_to_NA = isTRUE(attr(first, "diag_to_NA")),
				missing_to_zero = if (differs && align_actors == "union") {
					FALSE
				} else {
					isTRUE(attr(first, "missing_to_zero"))
					}
				)
			))
		}

		# attach combined nodal_data
		combined_nodal <- combine_input_nodal_data(dots, per_input_names, per_output_names)
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
		combined_dyad <- combine_input_dyad_data(dots, per_input_names, per_output_names)
	if (!is.null(combined_dyad) && length(combined_dyad) > 0L) {
		if (differs && align_actors %in% c("union", "intersection")) {
			combined_dyad <- reconcile_dyad_to_actors(combined_dyad, all_mats)
		}
		attr(out, "dyad_data") <- combined_dyad
	}

	out
}

#' subset a matrix or layer array to selected row and column actors
#'
#' @keywords internal
#' @noRd
subset_matrix_to <- function(m, row_target, col_target) {
	if (length(dim(m)) == 3L) {
		return(m[row_target, col_target, , drop = FALSE])
	}
	m[row_target, col_target, drop = FALSE]
}

#' determine whether each period has the same row and column actor sets
#'
#' @keywords internal
#' @noRd
actor_sets_uniform <- function(all_mats) {
	row_sets <- lapply(all_mats, rownames)
	col_sets <- lapply(all_mats, colnames)
	all(vapply(row_sets, identical, logical(1), row_sets[[1]])) &&
		all(vapply(col_sets, identical, logical(1), col_sets[[1]]))
}

#' reconcile combined nodal_data to per-period actor sets
#'
#' when `bind_netifies(align_actors = "union")` pads matrices with na
#' rows for missing actors, the nodal_data is missing rows for those
#' new actors -- downstream `to_statnet()` then sees na vertex
#' attributes. conversely, `align_actors = "intersection"` shrinks the
#' matrices but leaves the nodal_data unchanged, so it carries rows for
#' actors no longer present. this helper realigns the nodal_data per
#' period to exactly the actor set of the corresponding matrix.
#'
#' @keywords internal
#' @noRd
reconcile_nodal_to_actors <- function(nd, all_mats, align_actors) {
	if (!"time" %in% names(nd)) return(nd)
	out_frames <- list()
	for (k in seq_along(all_mats)) {
		period <- names(all_mats)[k]
			target_actors <- unique(c(rownames(all_mats[[k]]), colnames(all_mats[[k]])))
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

#' reconcile combined dyad_data to per-period row and column actor sets
#'
#' @keywords internal
#' @noRd
reconcile_dyad_to_actors <- function(dd, all_mats) {
	out <- dd
	for (k in seq_along(all_mats)) {
		period <- names(all_mats)[k]
		vars <- out[[period]]
		if (is.null(vars) || length(vars) == 0L) next
		target <- all_mats[[k]]
		row_target <- rownames(target)
		col_target <- colnames(target)
		out[[period]] <- lapply(vars, function(m) {
			if (!is.matrix(m)) return(m)
			aligned <- matrix(
				NA,
				nrow = length(row_target),
				ncol = length(col_target),
				dimnames = list(row_target, col_target)
			)
			storage.mode(aligned) <- storage.mode(m)
			r_keep <- intersect(rownames(m), row_target)
			c_keep <- intersect(colnames(m), col_target)
			if (length(r_keep) > 0L && length(c_keep) > 0L) {
				aligned[r_keep, c_keep] <- m[r_keep, c_keep, drop = FALSE]
			}
			aligned
		})
	}
	out
}

#' pad a matrix to a target row/column actor set with nas
#'
#' used by `bind_netifies(align_actors = "union")` to bring every
#' period to a common dimnames so longitudinal models (e.g., tergm
#' cmle) see a uniform actor composition.
#'
#' @keywords internal
#' @noRd
pad_matrix_to <- function(m, row_target, col_target) {
	# fast path when already aligned
	if (identical(rownames(m), row_target) && identical(colnames(m), col_target)) {
		return(m)
	}
	if (length(dim(m)) == 3L) {
		out <- array(
			NA_real_,
			dim = c(length(row_target), length(col_target), dim(m)[3]),
			dimnames = list(row_target, col_target, dimnames(m)[[3]])
		)
		r_keep <- intersect(rownames(m), row_target)
		c_keep <- intersect(colnames(m), col_target)
		if (length(r_keep) > 0L && length(c_keep) > 0L) {
			out[r_keep, c_keep, ] <- m[r_keep, c_keep, , drop = FALSE]
		}
		return(out)
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

#' combine per-input nodal_data frames into one (actor, time) frame
#'
#' builds a single long-format nodal_data frame keyed by `actor` and
#' `time` from each input's existing nodal_data, stamping the per-input
#' period labels onto cross-sec inputs. returns NULL if no input
#' carried nodal_data.
#'
#' @keywords internal
#' @noRd
combine_input_nodal_data <- function(dots, per_input_names, per_output_names) {
	frames <- list()
	for (i in seq_along(dots)) {
		nd <- attr(dots[[i]], "nodal_data")
		if (is.null(nd) || !is.data.frame(nd) || nrow(nd) == 0L) next
		nt <- attr(dots[[i]], "netify_type")
		old_names <- as.character(per_input_names[[i]])
		new_names <- as.character(per_output_names[[i]])
		if (nt == "cross_sec") {
			# stamp output period name onto cross-sec nodal_data
			nd$time <- new_names[1]
		} else {
			# longit inputs carry their own time column; map old input
			# labels to final output labels after `names =` and deduplication.
			if (!"time" %in% names(nd)) {
				nd$time <- new_names[1]
			} else {
				idx <- match(as.character(nd$time), old_names)
				nd$time <- ifelse(is.na(idx), as.character(nd$time), new_names[idx])
			}
		}
		frames[[length(frames) + 1L]] <- nd
	}
	if (length(frames) == 0L) return(NULL)
	# rbind with union of columns, padding missing with na
	all_cols <- unique(unlist(lapply(frames, names)))
	frames_padded <- lapply(frames, function(df) {
		missing_c <- setdiff(all_cols, names(df))
		for (m in missing_c) df[[m]] <- NA
		df[, all_cols, drop = FALSE]
	})
	do.call(rbind, c(frames_padded, list(make.row.names = FALSE)))
}

#' combine per-input dyad_data lists into one period-keyed list
#'
#' each input's `dyad_data` is a list keyed by period name; for
#' cross-sec inputs we stamp the output period name as the key.
#' conflicts (same period, same var) are resolved later-wins.
#'
#' @keywords internal
#' @noRd
combine_input_dyad_data <- function(dots, per_input_names, per_output_names) {
	out <- list()
	for (i in seq_along(dots)) {
		dd <- attr(dots[[i]], "dyad_data")
		if (is.null(dd) || length(dd) == 0L) next
		nt <- attr(dots[[i]], "netify_type")
		old_names <- as.character(per_input_names[[i]])
		new_names <- as.character(per_output_names[[i]])
		if (nt == "cross_sec") {
			out[[new_names[1]]] <- if (is.list(dd) && length(dd) == 1L) dd[[1]] else dd
		} else {
			old_periods <- names(dd) %||% old_names
			for (k in seq_along(dd)) {
				idx <- match(as.character(old_periods[k]), old_names)
				key <- if (!is.na(idx)) new_names[idx] else new_names[k]
				out[[key]] <- dd[[k]]
			}
		}
	}
	if (length(out) == 0L) NULL else out
}

#' merge method for netify objects (s3 alias for `bind_netifies`)
#'
#' provides the base-r `merge()` generic dispatch for combining two
#' netify objects along the time axis. for more than two inputs or
#' programmatic use, prefer `bind_netifies()` directly.
#'
#' @param x,y netify objects.
#' @param ... additional netify objects, or a `names =` argument.
#' @return a `longit_list` netify object.
#' @method merge netify
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
merge.netify <- function(x, y, ...) {
	bind_netifies(x, y, ...)
}

#' convert a netify object to a tibble (long edge frame)
#'
#' s3 method for `tibble::as_tibble()`. returns the same long-format
#' frame as [unnetify()] / [tidy.netify()], wrapped in a tibble for
#' tidyverse-pipe friendliness.
#'
#' registered against the `tibble::as_tibble` generic via `.onload`,
#' so `tibble` is not a hard dependency. when `tibble` isn't
#' installed, a plain data.frame is returned.
#'
#' @param x a netify object.
#' @param ... passed to [unnetify()] (e.g., `remove_zeros = TRUE`).
#' @return a tibble (or data.frame if tibble isn't installed) with
#' one row per dyad. includes `from`, `to`, optional `time`/`layer`,
#' the edge weight, dyadic covariates, and nodal covariates merged
#' in with `_from` / `_to` suffixes.
#'
#' @seealso [tidy.netify()] for the broom-style sibling and
#' [unnetify()] for the underlying converter.
#'
#' @examples
#' \donttest{
#' data(icews)
#' icews_10 <- icews[icews$year == 2010, ]
#' net <- netify(icews_10, actor1 = "i", actor2 = "j",
#'     symmetric = FALSE, weight = "verbCoop")
#' tibble::as_tibble(net)
#' }
#'
#' @author cassy dorff, shahryar minhas
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
