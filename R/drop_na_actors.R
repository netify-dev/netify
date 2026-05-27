#' Drop actors with NA covariates from a netify object
#'
#' Removes actors whose \code{nodal_data} carries \code{NA} in one or more
#' covariate columns. ERGM terms like \code{nodecov()} and
#' \code{nodematch()} reject NA-bearing vertex attributes, so this helper
#' is handy upstream of \code{\link{netify_to_statnet}}. Works for
#' cross-sectional, longitudinal, and bipartite netlets.
#'
#' @param netlet A netify object with a \code{nodal_data} attribute.
#' @param cols Character vector of column names in \code{nodal_data} to
#'   check for \code{NA}. \code{NULL} (the default) checks every
#'   non-bookkeeping column (everything except \code{actor},
#'   \code{time}, and \code{layer}).
#'
#' @return A netify object equivalent to
#'   \code{subset_netify(netlet, actors = clean_actors)} after dropping
#'   any actor whose nodal rows contain \code{NA} in the inspected
#'   columns. If no NAs are found the input is returned unchanged.
#'
#' @details
#' For longitudinal netlets an actor is dropped from every period if any
#' of its rows in \code{nodal_data} carry \code{NA} in the inspected
#' columns. For bipartite netlets only actors in the mode that the
#' nodal table covers are filtered; the other mode passes through
#' untouched.
#'
#' Corner cases:
#' \itemize{
#'   \item If \code{cols} references a name that is not in
#'     \code{nodal_data}, the call aborts with a clear message listing
#'     the missing columns.
#'   \item If no actor carries \code{NA} in the inspected columns, the
#'     input netlet is returned unchanged (no inform).
#'   \item If \emph{every} actor carries \code{NA} (so the cleaned
#'     netlet would have zero actors), the call aborts rather than
#'     silently returning an empty netify, which would break downstream
#'     \code{to_statnet()} / \code{ergm()} pipelines.
#'   \item If the netlet has no \code{nodal_data} attribute attached,
#'     the input is returned unchanged.
#' }
#'
#' @examples
#' \donttest{
#' data(icews)
#' net <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE, weight = "verbCoop",
#'     nodal_vars = c("i_polity2", "i_log_gdp", "i_log_pop")
#' )
#' clean <- drop_na_actors(net, cols = c("i_polity2", "i_log_gdp"))
#' }
#'
#' @author Shahryar Minhas
#'
#' @export

drop_na_actors <- function(netlet, cols = NULL) {
	# check if netify object
	netify_check(netlet)

	# pull nodal_data; nothing to do if absent
	nd <- attr(netlet, "nodal_data")
	if (is.null(nd) || !is.data.frame(nd) || nrow(nd) == 0) {
		return(netlet)
	}

	# resolve actor column
	actor_col <- if ("actor" %in% names(nd)) "actor" else names(nd)[1]

	# pick covariate columns to inspect
	skip_cols <- intersect(c("actor", "time", "layer"), names(nd))
	default_cols <- setdiff(names(nd), skip_cols)
	if (is.null(cols)) {
		cols <- default_cols
	} else {
		missing_cols <- setdiff(cols, names(nd))
		if (length(missing_cols) > 0) {
			cli::cli_abort(
				"Column{?s} {.val {missing_cols}} not found in {.code nodal_data}."
			)
		}
	}

	# no columns to inspect
	if (length(cols) == 0) {
		return(netlet)
	}

	# find actors whose rows carry NA in any inspected column
	check_frame <- nd[, cols, drop = FALSE]
	bad_rows <- !stats::complete.cases(check_frame)
	if (!any(bad_rows)) {
		return(netlet)
	}
	bad_actors <- unique(as.character(nd[[actor_col]][bad_rows]))

	# restrict bad actors to the relevant bipartite mode
	bip <- identical(attr(netlet, "mode"), "bipartite")
	msrmnts <- netify_measurements(netlet)
	if (bip) {
		row_actors <- unique(unlist(msrmnts$row_actors, use.names = FALSE))
		col_actors <- unique(unlist(msrmnts$col_actors, use.names = FALSE))
		# a mode is genuinely "covered" by the nodal table only when at
		# least one of its actors has a non-NA value in any inspected col;
		# rows that are all-NA across `cols` are typically padding from
		# add_node_vars() and should not flag the other mode for removal.
		any_obs <- function(actors_subset) {
			rows <- as.character(nd[[actor_col]]) %in% actors_subset
			if (!any(rows)) return(FALSE)
			sub <- nd[rows, cols, drop = FALSE]
			any(!is.na(unlist(sub, use.names = FALSE)))
		}
		touches_row <- any_obs(row_actors)
		touches_col <- any_obs(col_actors)
		if (touches_row && !touches_col) {
			bad_actors <- intersect(bad_actors, row_actors)
		} else if (touches_col && !touches_row) {
			bad_actors <- intersect(bad_actors, col_actors)
		}
		# if neither side has any non-NA observation in `cols`, there is
		# nothing meaningful to filter on
		if (!touches_row && !touches_col) {
			return(netlet)
		}
	}

	if (length(bad_actors) == 0) {
		return(netlet)
	}

	# build the clean actor list per netlet structure
	if (bip) {
		row_actors <- unique(unlist(msrmnts$row_actors, use.names = FALSE))
		col_actors <- unique(unlist(msrmnts$col_actors, use.names = FALSE))
		clean_from <- setdiff(row_actors, bad_actors)
		clean_to <- setdiff(col_actors, bad_actors)
		total_actors <- length(row_actors) + length(col_actors)
		# refuse to return an empty netify (would crash downstream
		# to_statnet()/ergm() pipelines with a cryptic message)
		if (length(clean_from) == 0L || length(clean_to) == 0L) {
			cli::cli_abort(c(
				"x" = "Dropping NA actors would leave one bipartite mode empty ({length(clean_from)} row x {length(clean_to)} col actors).",
				"i" = "Every actor in at least one mode carries {.val NA} in column{?s} {.val {cols}}.",
				"i" = "Inspect with {.code attr(netlet, 'nodal_data')} and impute, or pick a different set of {.arg cols}."
			))
		}
		out <- subset_netify(
			netlet,
			from = clean_from,
			to = clean_to
		)
	} else {
		all_actors <- unique(unlist(
			if (!is.null(msrmnts$row_actors)) msrmnts$row_actors else msrmnts$actors,
			use.names = FALSE
		))
		clean_actors <- setdiff(all_actors, bad_actors)
		total_actors <- length(all_actors)
		# refuse to return an empty netify (would crash downstream
		# to_statnet()/ergm() pipelines with a cryptic message)
		if (length(clean_actors) == 0L) {
			cli::cli_abort(c(
				"x" = "Dropping NA actors would leave the netify empty (all {total_actors} actors carry {.val NA} in column{?s} {.val {cols}}).",
				"i" = "Inspect with {.code attr(netlet, 'nodal_data')} and impute, or pick a different set of {.arg cols}."
			))
		}
		out <- subset_netify(
			netlet,
			actors = clean_actors
		)
	}

	# one-shot inform
	cli::cli_inform(c(
		"i" = "Dropped {length(bad_actors)} of {total_actors} actor{?s} with NA covariates: {.val {sort(bad_actors)}}."
	),
	.frequency = "once",
	.frequency_id = paste0(
		"drop_na_actors_",
		paste(sort(cols), collapse = "_"), "_",
		paste(sort(bad_actors), collapse = "_")
	))

	return(out)
}
