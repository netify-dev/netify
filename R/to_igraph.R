#' Convert netify objects to igraph format
#'
#' transforms netify network objects into igraph objects (also available as `netify_to_igraph`),
#' preserving network structure and optionally including nodal and dyadic attributes as vertex
#' and edge attributes.
#'
#' @param netlet a netify object containing network data. single-layer
#'   networks return a single igraph (or a list of igraphs for longitudinal
#'   input); multilayer networks return a named list keyed by layer, with
#'   each element itself an igraph or list of igraphs.
#' @param add_nodal_attribs logical. if TRUE (default), includes nodal attributes
#'   from the netify object as vertex attributes in the igraph object. set to
#'   FALSE to create a network with structure only.
#' @param add_dyad_attribs logical. if TRUE (default), includes dyadic attributes
#'   from the netify object as edge attributes in the igraph object. set to
#'   FALSE to exclude edge covariates.
#'
#' @return an igraph object or list of igraph objects:
#'   \describe{
#'     \item{cross-sectional networks}{returns a single igraph object}
#'     \item{longitudinal networks}{returns a named list of igraph objects,
#'       with names corresponding to time periods}
#'     \item{multilayer networks}{returns a named list keyed by layer; each
#'       element is itself an igraph or (for longitudinal) a list of igraphs}
#'   }
#'
#'   the resulting igraph object(s) will have:
#'   \itemize{
#'     \item vertices named according to actors in the netify object
#'     \item edge weights from the netify weight variable (if present)
#'     \item vertex attributes for each nodal variable (if add_nodal_attribs = TRUE)
#'     \item edge attributes for each dyadic variable (if add_dyad_attribs = TRUE)
#'   }
#'
#' @details
#' the conversion process handles different netify structures:
#' \itemize{
#'   \item \strong{cross-sectional}: direct conversion to a single igraph object
#'   \item \strong{longitudinal arrays}: internally converted to list format, then
#'     each time slice becomes a separate igraph object
#'   \item \strong{longitudinal lists}: each time period converted to separate igraph object
#' }
#'
#' for directed networks, the resulting igraph object will be directed. for
#' undirected networks, the igraph object will be undirected. edge weights in
#' the netify object become edge weights in igraph.
#'
#' when longitudinal data includes actors that appear or disappear over time,
#' each time period's igraph object will contain only the actors present in
#' that period.
#'
#' @note
#' this function requires the igraph package to be installed.
#'
#' @examples
#' # load example data
#' data(icews)
#'
#' # example 1: cross-sectional network with attributes
#' icews_10 <- icews[icews$year == 2010, ]
#'
#' # create netify object with attributes
#' dvars <- c("matlCoop", "verbConf", "matlConf")
#' nvars <- c("i_polity2", "i_log_gdp", "i_log_pop")
#'
#' verbCoop_net <- netify(
#'     icews_10,
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop",
#'     dyad_vars = dvars,
#'     dyad_vars_symmetric = rep(FALSE, length(dvars)),
#'     nodal_vars = nvars
#' )
#'
#' # convert to igraph
#' ig <- netify_to_igraph(verbCoop_net)
#'
#' # examine the result
#' ig
#' igraph::vcount(ig) # number of vertices
#' igraph::ecount(ig) # number of edges
#' igraph::vertex_attr_names(ig) # nodal attributes
#' igraph::edge_attr_names(ig) # edge attributes
#'
#' # access specific attributes
#' igraph::V(ig)$i_polity2 # polity scores
#' igraph::E(ig)$matlCoop # material cooperation
#'
#' \donttest{
#' # example 2: longitudinal network
#' verbCoop_longit <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE,
#'     weight = "verbCoop",
#'     dyad_vars = dvars,
#'     dyad_vars_symmetric = rep(FALSE, length(dvars)),
#'     nodal_vars = nvars
#' )
#'
#' # convert to list of igraph objects
#' ig_list <- netify_to_igraph(verbCoop_longit)
#'
#' # examine structure
#' length(ig_list) # number of time periods
#' names(ig_list) # time period labels
#'
#' # access specific time period
#' ig_2002 <- ig_list[["2002"]]
#' ig_2002
#' }
#'
#' # example 3: convert without attributes
#' ig_structure_only <- netify_to_igraph(
#'     verbCoop_net,
#'     add_nodal_attribs = FALSE,
#'     add_dyad_attribs = FALSE
#' )
#'
#' # only network structure, no attributes
#' igraph::vertex_attr_names(ig_structure_only) # only "name"
#' igraph::edge_attr_names(ig_structure_only) # only "weight" (if present)
#'
#' @param .quiet_na logical. internal flag to suppress the
#'   one-shot na-to-zero cli alert when called from inside netify's
#'   own summary path. default `FALSE`; do not set in user code.
#'
#' @author ha eun choi, cassy dorff, colin henry, shahryar minhas
#'
#' @export netify_to_igraph
#' @aliases to_igraph

netify_to_igraph <- function(
	netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE,
	.quiet_na = FALSE) {
	# check if netify object
	netify_check(netlet)

	# assert dependencies for remapping data to igraph
	assert_dependency("igraph")

	# multilayer: dispatch one call per layer and return a named list
	layer_names <- attributes(netlet)$layers
	if (length(layer_names) > 1) {
		igrph_by_layer <- lapply(layer_names, function(lyr) {
			netify_to_igraph(
				subset_netify(netlet, layers = lyr),
				add_nodal_attribs = add_nodal_attribs,
				add_dyad_attribs = add_dyad_attribs,
				.quiet_na = .quiet_na
			)
		})
		names(igrph_by_layer) <- layer_names
		return(igrph_by_layer)
	}

	## three cases: cross-sec/matrix, longit list, longit array
	netlet_type <- attr(netlet, "netify_type")

	# if type array convert to list since igraph
	# doesnt support arrays anyhow
	if (netlet_type == "longit_array") {
		netlet <- array_to_list(netlet)
	}

	# check other attributes
	nodal_data_exist <- !is.null(
		attr(netlet, "nodal_data")[[1]]
	)
	dyad_data_exist <- !is.null(
		attr(netlet, "dyad_data")[[1]]
	)

	# cross-sec case
	if (netlet_type == "cross_sec") {
		# convert to igraph object
		igrph <- netify_net_to_igraph(netlet, .quiet_na = .quiet_na)

		# process nodal attributes if requested
		if (nodal_data_exist && add_nodal_attribs) {
			igrph <- add_nodal_to_igraph(
				netlet, attr(netlet, "nodal_data"), igrph
			)
		}

		# process dyadic attributes if requested
		if (dyad_data_exist && add_dyad_attribs) {
			igrph <- add_dyad_to_igraph(
				netlet, attr(netlet, "dyad_data"), igrph
			)
		}
	} # done with cross-sec case

	## longit case
	if (netlet_type %in% c("longit_array", "longit_list")) {
		# cache attributes once
		time_vals <- names(netlet)
		nodal_data_attr <- attr(netlet, "nodal_data")
		dyad_data_attr <- attr(netlet, "dyad_data")

		# if no attributes to add, simplify processing
		if ((!nodal_data_exist || !add_nodal_attribs) &&
			(!dyad_data_exist || !add_dyad_attribs)) {
			# just convert to igraph without attributes
			igrph <- lapply(netlet, netify_net_to_igraph,
				.quiet_na = .quiet_na)
			names(igrph) <- time_vals
		} else {
			# pre-process nodal data by time if needed
			nodal_data_by_time <- NULL
			if (nodal_data_exist && add_nodal_attribs && !is.null(nodal_data_attr)) {
				# create time-indexed lookup for faster access
				time_col <- nodal_data_attr[, 2]
				nodal_data_by_time <- split(nodal_data_attr, time_col)
			}

			# process all time periods
			igrph <- lapply(seq_along(netlet), function(ii) {
				netlet_slice <- netlet[[ii]]
				time_val <- time_vals[ii]

				# convert to igraph
				igrph_slice <- netify_net_to_igraph(netlet_slice,
					.quiet_na = .quiet_na)

				# add nodal attributes if needed
				if (nodal_data_exist && add_nodal_attribs && !is.null(nodal_data_by_time[[time_val]])) {
					node_data_t <- nodal_data_by_time[[time_val]]

					# match order efficiently
					igrph_nodes <- igraph_vertex_actor_names(igrph_slice)
					node_data_t <- node_data_t[match(igrph_nodes, node_data_t[, 1]), ]

					# vectorized attribute setting
					node_var_start <- 3 # skip actor and time columns
					if (ncol(node_data_t) >= node_var_start) {
						for (col_idx in node_var_start:ncol(node_data_t)) {
							igrph_slice <- igraph::set_vertex_attr(
								igrph_slice,
								name = names(node_data_t)[col_idx],
								value = node_data_t[, col_idx]
							)
						}
					}
				}

				# add dyadic attributes if needed
				if (dyad_data_exist && add_dyad_attribs) {
					igrph_slice <- add_dyad_to_igraph(
						netlet_slice,
						dyad_data_attr,
						igrph_slice,
						time_val
					)
				}

				return(igrph_slice)
			})

			names(igrph) <- time_vals
		}
	} # done with longit case

	#
	return(igrph)
}

#' @rdname netify_to_igraph
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
to_igraph <- netify_to_igraph

#' as.igraph method for netify objects
#'
#' s3 method that lets igraph's \code{\link[igraph]{as.igraph}} generic
#' dispatch on netify objects. equivalent to \code{netify_to_igraph(x, ...)}.
#' registered against the \pkg{igraph} namespace in \code{.onload}, so the
#' dispatch works regardless of whether \pkg{igraph} is attached before or
#' after \pkg{netify}.
#'
#' @param x a netify object.
#' @param ... extra arguments forwarded to \code{\link{netify_to_igraph}}
#'   (e.g. \code{add_nodal_attribs}, \code{add_dyad_attribs}).
#' @return an igraph object, or a list of igraph objects (longitudinal /
#'   multilayer), as produced by \code{netify_to_igraph}.
#'
#' @author shahryar minhas
#'
#' @rawNamespace if (requireNamespace("igraph", quietly = TRUE)) S3method(igraph::as.igraph, netify)

as.igraph.netify <- function(x, ...) {
	netify_to_igraph(x, ...)
}

#' netify_net_to_igraph
#'
#' convert netify object to igraph object
#'
#' @param netlet netify object
#' @return igraph object
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd

netify_net_to_igraph <- function(netlet, .quiet_na = FALSE) {
	# check if bipartite
	bipartite_logical <- ifelse(attr(netlet, "mode") == "bipartite", TRUE, FALSE)

	# weight logical
	if (!is.null(attr(netlet, "weight", exact = TRUE))) {
		weight_logical <- TRUE
	} else {
		weight_logical <- NULL
	}

	# strip netify attribs away
	raw_net <- get_raw(netlet)

	# replace nas with 0 for igraph compatibility, warn on non-diagonal nas
	diag_na <- if (!attr(netlet, "diag_to_NA")) FALSE else TRUE
	if (diag_na) {
		non_diag_na <- sum(is.na(raw_net)) - sum(is.na(diag(raw_net)))
	} else {
		non_diag_na <- sum(is.na(raw_net))
	}
	if (non_diag_na > 0 && !.quiet_na) {
		cli::cli_warn(
			"Replacing {non_diag_na} non-diagonal NA value{?s} with 0 for igraph compatibility."
		)
	}
	raw_net[is.na(raw_net)] <- 0

	# convert to igraph_object
	if (!bipartite_logical) {
		# symmetric: use "max" to merge upper and lower triangle values
		mode_val <- if(all(attr(netlet, "symmetric"))) "max" else "directed"
		
		igraph_object <- igraph::graph_from_adjacency_matrix(
			raw_net,
			mode = mode_val,
			weighted = weight_logical,
			diag = !attr(netlet, "diag_to_NA")
		)
	}

	# bipartite case
	if (bipartite_logical) {
		row_actors <- rownames(raw_net)
		col_actors <- colnames(raw_net)
		vertex_names <- bipartite_igraph_vertex_names(row_actors, col_actors)
		raw_net_igraph <- raw_net
		dimnames(raw_net_igraph) <- list(vertex_names$row, vertex_names$col)

		igraph_object <- igraph::graph_from_biadjacency_matrix(
			raw_net_igraph,
			directed = FALSE,
			weighted = weight_logical
		)
		igraph_object <- igraph::set_vertex_attr(
			igraph_object,
			name = "netify_actor",
			value = c(row_actors, col_actors)
		)
		igraph_object <- igraph::set_vertex_attr(
			igraph_object,
			name = "netify_mode",
			value = ifelse(igraph::V(igraph_object)$type, "col", "row")
		)
	}

	# add dv as edge attribute as well
	if (!is.null(attr(netlet, "weight", exact = TRUE))) {
		# match edge positions between raw data and igraph
		e_pos_igraph <- adj_igraph_positions(raw_net, igraph_object)

		# subset dyadic data matrix based on ids
		# and add edge attr
		igraph_object <- igraph::set_edge_attr(
			igraph_object,
			name = attr(netlet, "weight", exact = TRUE),
			value = raw_net[e_pos_igraph]
		)
	}

	#
	return(igraph_object)
}

#' add_nodal_to_igraph
#'
#' add nodal attributes to an igraph object from netify object
#'
#' @param netlet netify object
#' @param node_data node data from netlet object
#' @param igraph_object igraph object to modify
#' @param time time indicator for longit case
#' @return igraph object with nodal attributes added
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd

add_nodal_to_igraph <- function(
	netlet, node_data, igraph_object, time = NULL) {
	actor_col <- match("actor", names(node_data))
	if (is.na(actor_col)) {
		actor_col <- 1L
	}
	time_col <- match("time", names(node_data))
	if (is.na(time_col)) {
		time_col <- NULL
	}

	# slice by time if relevant
	if (!is.null(time) && !is.null(time_col)) {
		node_data <- node_data[as.character(node_data[[time_col]]) == as.character(time), , drop = FALSE]
	}

	# make sure order of nodes is the same
	igrph_nodes <- igraph_vertex_actor_names(igraph_object)
	node_data <- node_data[
		match(igrph_nodes, as.character(node_data[[actor_col]])),
		, drop = FALSE
	]

	# loop through and add to network object
	skip_cols <- c(actor_col, time_col)
	node_var_cols <- setdiff(seq_along(node_data), skip_cols)
	for (ii in node_var_cols) {
		igraph_object <- igraph::set_vertex_attr(
			igraph_object,
			name = names(node_data)[ii],
			value = node_data[, ii]
		)
	}

	#
	return(igraph_object)
}

#' add_dyad_to_igraph
#'
#' add dyad attributes to an igraph object from netify object
#'
#' @param netlet netify object
#' @param dyad_data_list dyad data from netlet object
#' @param igraph_object igraph object to modify
#' @param time time indicator for longit case
#' @return igraph object with dyad attributes added
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd

add_dyad_to_igraph <- function(netlet, dyad_data_list, igraph_object, time = NULL) {
	# get dyadic data for specified time period
	if (is.null(time)) {
		var_matrices <- dyad_data_list[[1]]
	} else {
		var_matrices <- dyad_data_list[[time]]
	}

	# get var names from the list of matrices
	vars <- names(var_matrices)
	if (length(vars) == 0) {
		return(igraph_object)
	}

	# get attrs
	netlet_mode <- attr(netlet, "mode")
	netlet_diag_to_NA <- attr(netlet, "diag_to_NA")
	bipartite_logical <- netlet_mode == "bipartite"

	# union of nonzero/non-na cells across dyad vars so the round-trip stays lossless
	first_mat <- var_matrices[[1]]
	ar <- rownames(first_mat); ac <- colnames(first_mat)

	v_names_now <- igraph::V(igraph_object)$name

	cell_mat <- matrix(FALSE, nrow = nrow(first_mat), ncol = ncol(first_mat))
	for (v in vars) {
		m <- var_matrices[[v]]
		if (!bipartite_logical && netlet_diag_to_NA) diag(m) <- 0
		cell_mat <- cell_mat | (!is.na(m) & m != 0)
	}

	# igraph can only store dyadic covariates as attributes on realized edges.
	# keep only realized edges in the graph
	existing_cell_mat <- matrix(FALSE, nrow = nrow(first_mat), ncol = ncol(first_mat))
	cur_pos <- adj_igraph_positions(first_mat, igraph_object)
	if (nrow(cur_pos) > 0L) {
		keep_pos <- !is.na(cur_pos[, "row"]) & !is.na(cur_pos[, "col"])
		cur_pos <- cur_pos[keep_pos, , drop = FALSE]
		if (nrow(cur_pos) > 0L) {
			existing_cell_mat[cur_pos] <- TRUE
			if (!igraph::is_directed(igraph_object) && nrow(first_mat) == ncol(first_mat)) {
				existing_cell_mat[cbind(cur_pos[, "col"], cur_pos[, "row"])] <- TRUE
			}
		}
	}
	n_non_edge_covars <- sum(cell_mat & !existing_cell_mat, na.rm = TRUE)
	if (n_non_edge_covars > 0L) {
		cli::cli_inform(
			c(
				"i" = "{.fn netify_to_igraph} kept the igraph edge set unchanged.",
				"*" = "{n_non_edge_covars} dyadic covariate cell{?s} on non-edge{?s} cannot be stored as igraph edge attributes."
			),
			.frequency = "once",
			.frequency_id = "netify_to_igraph_non_edge_dyad_covars"
		)
	}

	# get edge positions
	e_pos_igraph <- adj_igraph_positions(first_mat, igraph_object)

	# go through dyad vars
	edge_attrs <- lapply(vars, function(var_name) {
		d_data <- var_matrices[[var_name]]

		# replace diagonal with 0s if needed
		if (!bipartite_logical && netlet_diag_to_NA) {
			diag(d_data) <- 0
		}

		# extract values using pre-computed positions
		return(d_data[e_pos_igraph])
	})

	# set all edge attributes at once
	for (i in seq_along(vars)) {
		igraph_object <- igraph::set_edge_attr(
			igraph_object,
			name = vars[i],
			value = edge_attrs[[i]]
		)
	}

	#
	return(igraph_object)
}

#' adj_igraph_positions
#'
#' match igraph edge order with matrix data for the
#' purpose of setting edge attributes
#'
#' @param adj_mat adjacency matrix
#' @param igraph_object igraph object to modify
#' @return matrix object of how actor positions match
#' between the adj_mat and igraph_object
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd

adj_igraph_positions <- function(adj_mat, igraph_object) {
	# get row and column information from adj_mat
	ar <- rownames(adj_mat)
	nr <- length(ar)
	ac <- colnames(adj_mat)
	nc <- length(ac)
	vertex_names <- igraph::V(igraph_object)$name
	vertex_actor <- igraph::vertex_attr(igraph_object, "netify_actor")
	vertex_mode <- igraph::vertex_attr(igraph_object, "netify_mode")
	if (!is.null(vertex_actor) && !is.null(vertex_mode)) {
		row_idx <- which(vertex_mode == "row")
		col_idx <- which(vertex_mode == "col")
		ar_labs <- vertex_names[row_idx][match(ar, vertex_actor[row_idx])]
		ac_labs <- vertex_names[col_idx][match(ac, vertex_actor[col_idx])]
	} else {
		ar_labs <- ar
		ac_labs <- ac
	}
	ar_key <- data.frame(
		id = 1:nr, lab = ar_labs, stringsAsFactors = FALSE
	)
	ac_key <- data.frame(
		id = 1:nc, lab = ac_labs, stringsAsFactors = FALSE
	)

	# can simplify the commented above via igraph::as_data_frame
	e_lab_igraph <- igraph::as_data_frame(igraph_object, what = "edges")

	# get positions of each actor in e_lab_igraph
	# based on where they fall in akey
	e_pos_igraph <- cbind(
		row = ar_key$id[match(e_lab_igraph[, 1], ar_key$lab)],
		col = ac_key$id[match(e_lab_igraph[, 2], ac_key$lab)]
	)

	#
	return(e_pos_igraph)
}

bipartite_igraph_vertex_names <- function(row_actors, col_actors) {
	if (length(intersect(row_actors, col_actors)) == 0L) {
		return(list(row = row_actors, col = col_actors))
	}
	list(
		row = paste0("row:", row_actors),
		col = paste0("col:", col_actors)
	)
}

igraph_vertex_actor_names <- function(igraph_object) {
	actor <- igraph::vertex_attr(igraph_object, "netify_actor")
	if (!is.null(actor)) return(actor)
	igraph::V(igraph_object)$name
}

igraph_bipartite_endpoint_names <- function(igraph_object, row_actors, col_actors) {
	vertex_actor <- igraph::vertex_attr(igraph_object, "netify_actor")
	vertex_mode <- igraph::vertex_attr(igraph_object, "netify_mode")
	if (is.null(vertex_actor) || is.null(vertex_mode)) {
		return(list(from = row_actors, to = col_actors))
	}
	vertex_names <- igraph::V(igraph_object)$name
	row_idx <- which(vertex_mode == "row")
	col_idx <- which(vertex_mode == "col")
	list(
		from = vertex_names[row_idx][match(row_actors, vertex_actor[row_idx])],
		to = vertex_names[col_idx][match(col_actors, vertex_actor[col_idx])]
	)
}
