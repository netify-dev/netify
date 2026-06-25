#' Create network object from various data types
#'
#' this function takes in various types of network data (dyadic datasets, matrices,
#' arrays, lists, igraph objects, or network objects) and outputs a netify object.
#'
#' @param input data object to netify. can be:
#'   \itemize{
#'     \item a data.frame (or tibble/data.table) with dyadic data
#'     \item a matrix representing an adjacency matrix
#'     \item a 3d array representing longitudinal networks
#'     \item a list of matrices representing longitudinal networks
#'     \item an igraph object
#'     \item a network object (from the network package)
#'     \item a list of igraph or network objects
#'   }
#' @param actor1 character: name of the actor 1 variable in the data (required
#'   for data.frame inputs)
#' @param actor2 character: name of the actor 2 variable in the data (required
#'   for data.frame inputs)
#' @param time character: name of the time variable in the data. can contain
#'   numeric, date, posixct/posixlt, or character values. non-numeric types
#'   will be converted to numeric indices while preserving original labels.
#'   if no time is provided then a cross-sectional network will be created.
#' @param weight character: name of the weighted edge variable in
#'   the data, default is NULL
#' @param symmetric logical: whether ties are symmetric, default is TRUE.
#'   for matrix, array, list, igraph, or network inputs this default is
#'   ignored unless explicitly set by the caller; symmetry is instead
#'   detected from the input itself. the default applies for data.frame
#'   inputs.
#' @param mode character: whether the network is unipartite or bipartite,
#'   default is unipartite. as with `symmetric`, this default applies to
#'   data.frame inputs and is auto-detected for matrix / array / list /
#'   igraph / network inputs unless explicitly set.
#' @param sum_dyads logical: whether to sum up the `weight` value when there exists repeating dyads
#' @param actor_time_uniform logical: whether to assume
#'   actors are the same across the full time series observed in the data
#'   TRUE means that actors are the same across the full time
#'   series observed in the data and the outputted netify object will
#'   be in an array format.
#'   FALSE means that actors come in and out of the observed data and
#'   their "existence" should be determined by the data, meaning that
#'   their first year of existence will be determined by the time point
#'   of their first event and their last year of existence by the
#'   time point of their last event. outputted netify object will be
#'   in a list format.
#' @param actor_pds a data.frame indicating start and end time point for every
#'   actor, this can be created using `get_actor_time_info`, unless provided this will
#'   estimated for the user based on their choice of `actor_time_uniform`
#' @param diag_to_NA logical: whether diagonals should be set to na,
#'   default is TRUE. for matrix / array / list inputs, the default is
#'   ignored unless explicitly set: instead, `diag_to_NA` is auto-detected
#'   by inspecting whether the diagonal of the supplied matrix is already
#'   `na`. pass an explicit value to override the auto-detection.
#' @param missing_to_zero logical: whether missing values should be set to
#'   zero, default is TRUE. as with `diag_to_NA`, this is auto-detected
#'   for matrix / array / list inputs based on whether the supplied data
#'   already contains off-diagonal `na`s.
#' @param output_format character: "cross_sec", "longit_array", or
#'   "longit_list". if not specified and time is NULL then output_format
#'   will be "cross_sec" and if time is specified then output_format
#'   will default to "longit_list". only applies to data.frame inputs.
#' @param nodal_vars character vector: names of the nodal variables in the input
#'   that should be added as attributes to the netify object (for data.frame inputs)
#' @param dyad_vars character vector: names of the dyadic variables in the input
#'   that should be added as attributes to the netify object (for data.frame inputs)
#' @param dyad_vars_symmetric logical vector: whether ties are symmetric, default is
#'   to use the same choice as the symmetric argument
#' @param input_type character: force specific input type interpretation.
#'   options are "auto" (default), "dyad_df", or "netify_obj". use "dyad_df"
#'   to force data.frame interpretation or "netify_obj" to force matrix/array/
#'   igraph/network interpretation.
#' @param nodelist optional list of all actors (nodes) that should be included
#'   in the network. for unipartite networks, pass a character vector. for
#'   bipartite networks, pass partition-aware input such as
#'   \code{list(row = row_actors, col = col_actors)} or a data frame with
#'   actor and mode columns. a flat vector can include only actors already
#'   observed in a bipartite row or column mode because new isolates cannot be
#'   assigned to a partition from their names alone.
#' @param force_dense logical: when a `matrix::sparsematrix` input would
#'   densify to a large allocation (n > 5000 and density < 1%), `netify()`
#'   aborts with a guidance message. set `force_dense = TRUE` to override
#'   the guard and proceed with densification.
#' @param ... additional arguments passed to `to_netify` when processing network objects
#'
#' @return a netify object
#'
#' @author ha eun choi, cassy dorff, colin henry, shahryar minhas
#'
#' @seealso \code{\link{netify_workflows}} for an overview of the
#'   create / explore / model workflow and how \code{netify()} fits
#'   into it; \code{\link{add_node_vars}}, \code{\link{add_dyad_vars}}
#'   for attaching attributes after construction; and
#'   \code{\link{classroom_edges}} / \code{\link{classroom_nodes}}
#'   for a small worked example.
#'
#' @examples
#'
#' # load example directed event data from icews
#' # this data comes in the form of a dyadic
#' # dataframe where all dyad pairs are listed
#' data(icews)
#'
#' # from a data.frame: generate a longitudinal, directed and weighted network
#' # where the weights are matlConf
#' icews_matlConf <- netify(
#'     input = icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE, weight = "matlConf"
#' )
#'
#' # from a matrix
#' adj_matrix <- matrix(rbinom(100, 1, 0.3), 10, 10)
#' net_from_matrix <- netify(adj_matrix)
#'
#' # from an igraph object
#' \donttest{
#' library(igraph)
#' g <- sample_gnp(10, 0.3)
#' net_from_igraph <- netify(g)
#' }
#'
#' @export netify
#'

netify <- function(
	input,
	actor1 = NULL, actor2 = NULL, time = NULL,
	symmetric = TRUE, mode = "unipartite",
	weight = NULL, sum_dyads = FALSE,
	actor_time_uniform = TRUE,
	actor_pds = NULL,
	diag_to_NA = TRUE,
	missing_to_zero = TRUE,
	output_format = NULL,
	nodal_vars = NULL,
	dyad_vars = NULL,
	dyad_vars_symmetric = rep(symmetric, length(dyad_vars)),
	input_type = c("auto", "dyad_df", "netify_obj"),
	nodelist = NULL,
	force_dense = FALSE,
	...) {
	input_type <- match.arg(input_type)

	# track which structural args the user set so matrix/array/list inputs
	# can have the rest auto-detected by new_netify
	user_set_symmetric <- !missing(symmetric)
	user_set_mode <- !missing(mode)
	user_set_diag_to_NA <- !missing(diag_to_NA)
	user_set_missing_to_zero <- !missing(missing_to_zero)

	valid_output_formats <- c("cross_sec", "longit_array", "longit_list")
	if (!is.null(output_format)) {
		if (!is.character(output_format) || length(output_format) != 1L ||
			is.na(output_format) || !output_format %in% valid_output_formats) {
			cli::cli_abort(
				"{.arg output_format} must be one of {.val {valid_output_formats}}."
			)
		}
	}

	if (missing(missing_to_zero) && missing_to_zero) {
		cli::cli_inform(
			c(
				"i" = "{.arg missing_to_zero} is set to {.val TRUE} (the default).",
				"!" = "Missing dyads will be filled with zeros. For latent space or other
				statistical network models, structural zeros and missing data have different
				meanings. Set {.code missing_to_zero = FALSE} to preserve NAs if this distinction
				matters for your analysis."
			),
			.frequency = "once",
			.frequency_id = "netify_missing_to_zero_warning"
		)
	}

	use_network_path <- FALSE

	# guard against densifying very large + very sparse matrices unless opted in
	if (isS4(input) && inherits(input, c("sparseMatrix", "Matrix"))) {
		input <- densify_sparse_input(input, force_dense = force_dense)
		use_network_path <- TRUE
	} else if (is.list(input) && length(input) > 0 &&
		isS4(input[[1]]) && inherits(input[[1]], c("sparseMatrix", "Matrix"))) {
		# longitudinal list of sparsematrix slices
		input <- lapply(input, densify_sparse_input, force_dense = force_dense)
		use_network_path <- TRUE
	}

	if (input_type == "auto") {
		if (inherits(input, c("matrix", "array", "igraph", "network"))) {
			use_network_path <- TRUE
		} else if (is.list(input) && length(input) > 0) {
			first_elem <- input[[1]]
			if (inherits(first_elem, c("matrix", "igraph", "network"))) {
				use_network_path <- TRUE
			}
		}
	} else if (input_type == "netify_obj") {
		use_network_path <- TRUE
	}

	if (use_network_path) {
		logical_check(sum_dyads, symmetric, diag_to_NA, missing_to_zero, actor_time_uniform)
		if (!is.character(mode) || length(mode) != 1L || is.na(mode) ||
			!mode %in% c("unipartite", "bipartite")) {
			cli::cli_abort(
				"{.arg mode} must be either {.val unipartite} or {.val bipartite}."
			)
		}

		# only forward structural args the user actually set
		to_netify_args <- list(
			net_obj = input,
			weight = weight,
			sum_dyads = sum_dyads,
			actor_time_uniform = actor_time_uniform,
			actor_pds = actor_pds
		)
		if (user_set_symmetric) to_netify_args$symmetric <- symmetric
		if (user_set_mode) to_netify_args$mode <- mode
		if (user_set_diag_to_NA) to_netify_args$diag_to_NA <- diag_to_NA
		if (user_set_missing_to_zero) to_netify_args$missing_to_zero <- missing_to_zero

		dots <- list(...)
		to_netify_args <- c(to_netify_args, dots)

		return(do.call(to_netify, to_netify_args))
	}

	# data.frame input path

	# accept aliases for an external one-row-per-actor frame
	nodal_alias_df <- NULL
	dot_args <- list(...)
	for (alias in c("nodal_data", "vertices", "vertex.attr")) {
		if (alias %in% names(dot_args)) {
			nodal_alias_df <- dot_args[[alias]]
			dot_args[[alias]] <- NULL
			break
		}
	}

	# catch beginner typos in ... and offer "did you mean" hints
	dot_names <- names(dot_args)
	if (length(dot_names) > 0L) {
		supported <- c(
			names(formals(netify)),
			"nodal_data", "vertices", "vertex.attr",
			"actor_col", "nodal_actor_col"
		)
		supported <- setdiff(unique(supported), c("", "..."))
		stray <- setdiff(dot_names, c(supported, ""))
		if (length(stray) > 0L) {
			hints <- vapply(stray, function(s) {
				d <- utils::adist(s, supported, ignore.case = TRUE)[1, ]
				keep <- which(d <= max(2L, ceiling(nchar(s) / 3L)))
				if (length(keep) == 0L) return("")
				# closest first, max 3 suggestions
				keep <- keep[order(d[keep])]
				keep <- keep[seq_len(min(3L, length(keep)))]
				sprintf("  %s -> %s", s, paste(supported[keep], collapse = " / "))
			}, character(1))
			hints <- hints[nzchar(hints)]
			msg <- c(
				"x" = "Unrecognized argument{?s} to {.fn netify}: {.arg {stray}}.",
				"i" = "See {.code ?netify} for the full argument list."
			)
			if (length(hints) > 0L) {
				msg <- c(msg, "i" = "Did you mean:",
					stats::setNames(hints, rep("*", length(hints))))
			}
			cli::cli_abort(msg)
		}
	}

	dyad_data <- df_check(input)
	logical_check(sum_dyads, symmetric, diag_to_NA, missing_to_zero)
	actor_check(actor1, actor2, dyad_data)
	weight_check(weight, dyad_data)
	if (!is.null(time)) {
		time_check(time, dyad_data)
	}

	# coerce actor columns to character
	if (!is.character(dyad_data[, actor1]) | !is.character(dyad_data[, actor2])) {
		cli::cli_alert_warning(
			"Converting `actor1` and/or `actor2` to character vector(s)."
		)
		dyad_data[, actor1] <- char(dyad_data[, actor1])
		dyad_data[, actor2] <- char(dyad_data[, actor2])
	}

	actor_mode_check(dyad_data, actor1, actor2, mode)

	# sum_dyads requires post-hoc nodal/dyad var attachment
	if (sum_dyads == TRUE) {
		if (!is.null(nodal_vars) | !is.null(dyad_vars)) {
			cli::cli_alert_warning(
				"When sum_dyads is set to TRUE nodal and dyadic attributes cannot automatically be created using `netify`. Instead users need to add them afterwards using the `add_dyad_vars` and `add_node_vars` functions."
			)
			nodal_vars <- dyad_vars <- NULL
		}
	}

	if (is.null(output_format)) {
		if (is.null(time)) {
			output_format <- "cross_sec"
		} else {
			n_time_periods <- length(unique(dyad_data[[time]]))

			if (n_time_periods == 1) {
				output_format <- "cross_sec"
				cli::cli_alert_info(
					"Time variable specified but only one time period found. Creating cross-sectional network."
				)
			} else {
				output_format <- "longit_list"
			}
		}
	}

	if (output_format %in% c("longit_array", "longit_list") && is.null(time)) {
		cli::cli_abort(
			"{.arg time} must be supplied when {.arg output_format} is {.val {output_format}}."
		)
	}

	# dispatch to the right adjacency builder
	if (output_format == "cross_sec") {
		netlet <- get_adjacency(
			dyad_data = dyad_data,
			actor1 = actor1, actor2 = actor2,
			symmetric = symmetric, mode = mode,
			weight = weight,
			sum_dyads = sum_dyads,
			diag_to_NA = diag_to_NA,
			missing_to_zero = missing_to_zero,
			nodelist = nodelist
		)
	}

	if (output_format == "longit_array") {
		netlet <- get_adjacency_array(
			dyad_data = dyad_data,
			actor1 = actor1, actor2 = actor2, time = time,
			symmetric = symmetric, mode = mode,
			weight = weight, sum_dyads = sum_dyads,
			diag_to_NA = diag_to_NA, missing_to_zero = missing_to_zero,
			nodelist = nodelist
		)
	}

	if (output_format == "longit_list") {
		netlet <- get_adjacency_list(
			dyad_data = dyad_data,
			actor1 = actor1, actor2 = actor2, time = time,
			symmetric = symmetric, mode = mode,
			weight = weight, sum_dyads = sum_dyads,
			actor_time_uniform = actor_time_uniform,
			actor_pds = actor_pds,
			diag_to_NA = diag_to_NA, missing_to_zero = missing_to_zero,
			nodelist = nodelist
		)
	}

	# attach user-supplied nodal vars
	if (!is.null(nodal_vars)) {
		node_data <- unique(dyad_data[, c(actor1, time, nodal_vars)])
		node_data <- aggregate_node_vars_from_dyads(
			node_data,
			key_cols = c(actor1, time),
			node_vars = nodal_vars,
			time = time
		)

		netlet <- add_node_vars(
			netlet = netlet, node_data = node_data,
			actor = actor1, time = time,
			node_vars = nodal_vars
		)
	}

	# attach user-supplied dyad vars
	if (!is.null(dyad_vars)) {
		if (is.null(dyad_vars_symmetric)) {
			dyad_vars_symmetric <- rep(symmetric, length(dyad_vars))
		}

		netlet <- add_dyad_vars(
			netlet = netlet, dyad_data = dyad_data,
			actor1 = actor1, actor2 = actor2, time = time,
			dyad_vars = dyad_vars,
			dyad_vars_symmetric = dyad_vars_symmetric
		)
	}

	# attach external one-row-per-actor frame supplied via nodal_data=
	if (!is.null(nodal_alias_df) && is.data.frame(nodal_alias_df)) {
		actor_col_candidates <- c("actor", "name", "id", "vertex", "pid",
			"student", "country", "actor_id", "node", "vertex_id")
		actor_col <- intersect(actor_col_candidates, names(nodal_alias_df))[1]
		if (is.na(actor_col)) actor_col <- names(nodal_alias_df)[1]
		netlet <- add_node_vars(
			netlet = netlet, node_data = nodal_alias_df,
			actor = actor_col, time = if (!is.null(time)) time else NULL
		)
	}

	return(netlet)
}
