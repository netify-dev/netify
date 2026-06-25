#' Convert igraph, network, or matrix objects to netify format
#'
#' converts various network object types (igraph, network, matrices/arrays,
#' or lists of these) into netify objects (also available as `to_netify`).
#' automatically extracts adjacency
#' matrices and any nodal/dyadic attributes from the input objects.
#'
#' @param net_obj an r object to convert: \code{igraph}, \code{network},
#'   matrix, array, or a list of these objects.
#' @param weight optional. name of the weight attribute in \code{net_obj} to be used
#'   as the main edge weight in the netify object. default is \code{NULL}. important to specify
#'   for \code{igraph} and \code{network} objects as they do not have a default weight.
#' @param ... additional arguments passed to \code{new_netify}. can include
#'   \code{nodal_data} or \code{dyad_data} to override extracted attributes.
#'
#' @return a netify object with:
#'   \itemize{
#'     \item adjacency matrix or list of matrices
#'     \item nodal attributes (if present in the input)
#'     \item dyadic attributes (if present in the input)
#'     \item weight specification (if provided)
#'   }
#'
#' @details
#' the function handles different input types:
#' \itemize{
#'   \item \strong{igraph}: extracts adjacency matrix, vertex attributes as nodal data,
#'     and edge attributes as dyadic data
#'   \item \strong{network}: extracts adjacency matrix, vertex attributes as nodal data,
#'     and edge attributes as dyadic data
#'   \item \strong{matrix}: direct conversion, no attribute extraction
#'   \item \strong{array}: assumes 3d arrays represent longitudinal networks
#'   \item \strong{list}: must contain all objects of the same type (all igraph,
#'     all network, or all matrices)
#' }
#'
#' for longitudinal data (lists or 3d arrays), the function creates a netify object
#' with time-indexed components. actor ordering is preserved from the input objects
#' and made consistent across all components (adjacency, nodal, and dyadic data).
#'
#' @note
#' when converting from igraph or network objects, specify the \code{weight} parameter
#' to designate which edge attribute should be used as the primary edge weight in
#' the netify object.
#'
#' @examples
#' \donttest{
#' # from igraph
#' g <- igraph::sample_gnp(10, 0.3)
#' igraph::E(g)$weight <- runif(igraph::ecount(g))
#' igraph::V(g)$group <- sample(c("a", "b"), igraph::vcount(g), replace = TRUE)
#'
#' net <- to_netify(g, weight = "weight")
#'
#' # from network
#' adj <- matrix(rbinom(100, size = 1, prob = 0.3), 10, 10)
#' diag(adj) <- 0
#' n <- network::network(adj, directed = TRUE, matrix.type = "adjacency")
#' network::set.vertex.attribute(n, "group", sample(1:2, 10, replace = TRUE))
#'
#' net <- to_netify(n)
#'
#' # from matrix
#' adj_mat <- matrix(rnorm(100), 10, 10)
#' net <- to_netify(adj_mat)
#'
#' # from list of matrices (longitudinal)
#' mat_list <- list(
#'     "2001" = matrix(rnorm(100), 10, 10),
#'     "2002" = matrix(rnorm(100), 10, 10)
#' )
#' net <- to_netify(mat_list)
#' }
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export to_netify

to_netify <- function(
	net_obj,
	weight = NULL,
	...) {
	# capture user-provided attribute data
	other_args <- list(...)
	user_ndata <- other_args$nodal_data
	user_ddata <- other_args$dyad_data

	# placeholders for final adjacency data, plus nodal/dyadic
	adj_data <- NULL
	nodal_data <- NULL
	dyad_data <- NULL
	is_longitudinal <- FALSE
	is_symmetric <- NULL # track symmetry for dyad data conversion

	# single igraph object
	if (inherits(net_obj, "igraph")) {
		# guard zero-vertex graphs: netify's downstream construction
		# (actor_pds, dimnames) requires at least one actor, so fail fast
		# with an actionable message rather than a cryptic data.frame error
		if (igraph::vcount(net_obj) == 0L) {
			cli::cli_abort(c(
				"Cannot build a netify object from an empty igraph (0 vertices).",
				"i" = "Add at least one vertex before converting."
			))
		}
		if (is.null(weight)) {
			weight <- auto_select_igraph_weight(net_obj)
		}
		processed <- decompose_igraph(net_obj, weight = weight)
		adj_data <- processed$adj_mat
		nodal_data <- processed$ndata
		dyad_data <- processed$ddata
		weight <- processed$weight
	# read symmetry from igraph
	is_symmetric <- !igraph::is_directed(net_obj)
	# mark absent self-loops on unipartite graphs
		if (is.matrix(adj_data) && nrow(adj_data) == ncol(adj_data) &&
			!is_logical_bipartite_igraph(net_obj)) {
			any_loop <- igraph::ecount(net_obj) > 0L &&
				any(igraph::which_loop(net_obj))
			if (!any_loop) {
				diag(adj_data) <- NA
			}
		}

		# single network object
	} else if (inherits(net_obj, "network")) {
		if (is.null(weight)) {
			weight <- auto_select_network_weight(net_obj)
		}
		processed <- decompose_statnet(net_obj, weight = weight)
		adj_data <- processed$adj_mat
		nodal_data <- processed$ndata
		dyad_data <- processed$ddata
		weight <- processed$weight
		dyad_data <- drop_statnet_auxiliary_edge_attrs(dyad_data, weight)
		# get symmetry directly from network
		is_symmetric <- !network::is.directed(net_obj)

		# single matrix
	} else if (is.matrix(net_obj)) {
		adj_data <- net_obj
		# for matrix, we'll check symmetry later if needed

		# array (3d for longitudinal)
	} else if (is.array(net_obj)) {
		adj_data <- net_obj
		is_longitudinal <- length(dim(net_obj)) == 3

			# treat the third dimension of a bare 3d array as time
		if (is_longitudinal) {
			dn <- dimnames(net_obj)
			third_names <- if (is.null(dn)) NULL else dn[[3]]
			cli::cli_inform(
				c(
					"i" = "3D array detected: third dimension treated as {.strong time periods}{if (!is.null(third_names)) paste0(' (', paste(utils::head(third_names, 3), collapse = ', '), if (length(third_names) > 3) ', ...' else '', ')')}.",
					"!" = "If the third dimension instead represents distinct {.strong layers} (e.g. trade / alliances / conflict on the same actors), build separate cross-sectional netify objects and combine via {.fn layer_netify}."
				),
				.frequency = "once",
				.frequency_id = "netify_3d_array_axis_hint"
			)
		}

		# list of networks, type detected downstream
	} else if (is.list(net_obj) && length(net_obj) > 0) {
		# determine list type and process accordingly
		list_result <- process_network_list(net_obj, weight)

		adj_data <- list_result$adj_data
		nodal_data <- list_result$nodal_data
		dyad_data <- list_result$dyad_data
		weight <- list_result$weight
		is_symmetric <- list_result$is_symmetric
		is_longitudinal <- list_result$is_longitudinal

		# unsupported type
	} else {
		cli::cli_abort(
			"Unsupported class for {.arg net_obj}. Supported classes are: matrix, array, igraph, network, or a list of these objects."
		)
	}

	# convert dyad_data to the new format if it exists
	if (!is.null(dyad_data)) {
		# for matrix/array cases where symmetry wasn't determined, check now
		if (is.null(is_symmetric)) {
			first_mat <- if (is.list(adj_data)) {
				adj_data[[1]]
			} else {
				adj_data
			}
			is_symmetric <- isSymmetric(first_mat)
		}

		dyad_data <- convert_dyad_data_to_new_format(
			dyad_data,
			adj_data,
			is_longitudinal,
			is_symmetric
		)
	}

	# validate user-provided nodal_data if given
	if (!is.null(user_ndata)) {
		validate_nodal_data(user_ndata, adj_data, is_longitudinal)
		nodal_data <- user_ndata
	}

	# validate user-provided dyad_data if given
	if (!is.null(user_ddata)) {
		validate_dyad_data(user_ddata, adj_data, is_longitudinal)
		dyad_data <- user_ddata
	}

	# align actor order across data components
	if (!is.null(nodal_data)) {
		nodal_data <- align_nodal_data_order(
			nodal_data,
			adj_data,
			is_longitudinal
		)
	}

	if (!is.null(dyad_data)) {
		dyad_data <- align_dyad_data_order(
			dyad_data,
			adj_data,
			is_longitudinal
		)
	}

	# build final netify object
	out <- new_netify(
		data = adj_data,
		weight = weight,
		nodal_data = nodal_data,
		dyad_data = dyad_data,
		...
	)

	# return the netify object
	return(out)
}

#' process a list of network objects
#'
#' handles the processing of lists containing igraph, network,
#' or matrix objects.
#'
#' @param net_obj a list of network objects (igraph, network, or matrix)
#' @param weight optional weight attribute name
#'
#' @return a list containing processed components
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
process_network_list <- function(net_obj, weight = NULL) {
	# initialize return values
	result <- list(
		adj_data = NULL,
		nodal_data = NULL,
		dyad_data = NULL,
		weight = weight,
		is_symmetric = NULL,
		is_longitudinal = TRUE
	)

	# check list type and process
	if (all(vapply(net_obj, inherits, logical(1), "igraph"))) {
		if (is.null(weight)) {
			weight <- auto_select_igraph_weight(net_obj[[1]])
			result$weight <- weight
		}
		# process igraph inputs
		processed_list <- lapply(net_obj, decompose_igraph, weight = weight)
		result$is_symmetric <- !igraph::is_directed(net_obj[[1]])
		# mark absent self-loops on unipartite slices
		any_loop_per_slice <- vapply(net_obj, function(g) {
			igraph::ecount(g) > 0L && any(igraph::which_loop(g))
		}, logical(1))
		for (k in seq_along(processed_list)) {
			m <- processed_list[[k]]$adj_mat
			if (is.matrix(m) && nrow(m) == ncol(m) && !any_loop_per_slice[k] &&
				!is_logical_bipartite_igraph(net_obj[[k]])) {
				diag(m) <- NA
				processed_list[[k]]$adj_mat <- m
			}
		}
	} else if (all(vapply(net_obj, inherits, logical(1), "network"))) {
		if (is.null(weight)) {
			weight <- auto_select_network_weight(net_obj[[1]])
			result$weight <- weight
		}
		# process network inputs
		processed_list <- lapply(net_obj, decompose_statnet, weight = weight)
		result$is_symmetric <- !network::is.directed(net_obj[[1]])
	} else if (all(vapply(net_obj, is.matrix, logical(1)))) {
		# list of matrices - simple case
		result$adj_data <- net_obj
		return(result)
	} else {
		cli::cli_abort(
			"List must contain all matrices, all igraph objects, or all network objects."
		)
	}

	# extract components from processed list
	result$adj_data <- lapply(processed_list, `[[`, "adj_mat")
	result$weight <- processed_list[[1]]$weight

	# process nodal data for longitudinal networks
	result$nodal_data <- process_longitudinal_nodal_data(
		processed_list,
		net_obj
	)

	# extract dyad data
	result$dyad_data <- lapply(processed_list, `[[`, "ddata")
	if (all(vapply(net_obj, inherits, logical(1), "network"))) {
		result$dyad_data <- drop_statnet_auxiliary_edge_attrs(result$dyad_data, result$weight)
	}

	#
	return(result)
}

#' auto-select a numeric igraph edge weight attribute
#'
#' @keywords internal
#' @noRd
auto_select_igraph_weight <- function(grph) {
	ea <- igraph::edge_attr_names(grph)
	if ("weight" %in% ea) {
		vals <- igraph::edge_attr(grph, "weight")
		if (!is.null(vals) && is.numeric(vals)) {
			return("weight")
		}
	}

	for (nm in setdiff(ea, "weight")) {
		vals <- igraph::edge_attr(grph, nm)
		if (!is.null(vals) && is.numeric(vals)) {
			return(nm)
		}
	}

	NULL
}

#' auto-select a numeric network edge weight attribute
#'
#' @keywords internal
#' @noRd
auto_select_network_weight <- function(ntwk) {
	ea <- setdiff(network::list.edge.attributes(ntwk), c("na"))
	if ("weight" %in% ea) {
		vals <- network::get.edge.attribute(ntwk, "weight")
		if (!is.null(vals) && is.numeric(vals)) {
			return("weight")
		}
	}

	for (nm in setdiff(ea, "weight")) {
		vals <- network::get.edge.attribute(ntwk, nm)
		if (!is.null(vals) && is.numeric(vals)) {
			return(nm)
		}
	}

	NULL
}

#' detect igraph bipartite inputs that decompose to a biadjacency matrix
#'
#' @keywords internal
#' @noRd
is_logical_bipartite_igraph <- function(grph) {
	if (!igraph::is_bipartite(grph)) {
		return(FALSE)
	}
	vertex_types <- igraph::V(grph)$type
	!is.null(vertex_types) && is.logical(vertex_types)
}

#' drop statnet system attributes from auxiliary dyad variables
#'
#' @keywords internal
#' @noRd
drop_statnet_auxiliary_edge_attrs <- function(edge_data, weight = NULL) {
	drop_cols <- c("na", weight)
	if (is.null(edge_data) || length(drop_cols) == 0L) {
		return(edge_data)
	}
	drop_one <- function(df) {
		if (is.null(df) || !is.data.frame(df)) return(df)
		df[, setdiff(names(df), drop_cols), drop = FALSE]
	}
	if (is.data.frame(edge_data)) {
		return(drop_one(edge_data))
	}
	if (is.list(edge_data)) {
		return(lapply(edge_data, drop_one))
	}
	edge_data
}

#' process nodal data for longitudinal networks
#'
#' @param processed_list list of processed network decompositions
#' @param net_obj original network object list (for time names)
#'
#' @return a combined data frame of nodal data or NULL
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
process_longitudinal_nodal_data <- function(processed_list, net_obj) {
	nodal_data_list <- lapply(seq_along(processed_list), function(i) {
		nd <- processed_list[[i]]$ndata
		if (!is.null(nd)) {
			# add time column
			nd$time <- names(net_obj)[i] %||% as.character(i)

			# rename 'name' to 'actor' for consistency if needed
			if ("name" %in% names(nd) && !"actor" %in% names(nd)) {
				names(nd)[names(nd) == "name"] <- "actor"
			}

			# if actor column doesn't exist, create it
			if (!"actor" %in% names(nd)) {
				nd$actor <- rownames(nd) %||% as.character(seq_len(nrow(nd)))
			}
		}
		return(nd)
	})

	# combine all non-null nodal data
	nodal_data_list <- nodal_data_list[!sapply(nodal_data_list, is.null)]
	if (length(nodal_data_list) > 0) {
		nodal_data <- do.call(rbind, nodal_data_list)
		rownames(nodal_data) <- NULL
		# canonical column order: actor, time, then everything else
		lead_cols <- intersect(c("actor", "time"), names(nodal_data))
		other_cols <- setdiff(names(nodal_data), lead_cols)
		nodal_data <- nodal_data[, c(lead_cols, other_cols), drop = FALSE]
		return(nodal_data)
	} else {
		return(NULL)
	}
}

#' align nodal data actor order to match adjacency matrix order
#'
#' ensure that actors in nodal_data appear in the same
#' order as they appear in the adjacency matrix row/column names.
#'
#' @param ndata a data frame containing nodal attributes with 'actor' column
#' @param adj_data an adjacency matrix or list of adjacency matrices
#' @param is_longitudinal logical indicating whether the network is longitudinal
#'
#' @return a reordered data frame with actors matching adjacency matrix order
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
align_nodal_data_order <- function(ndata, adj_data, is_longitudinal) {
	if (is_longitudinal) {
		if (!is.list(adj_data)) {
			cli::cli_abort("Longitudinal nodal data alignment requires a list of adjacency matrices.")
		}
		time_labels <- names(adj_data) %||% as.character(seq_along(adj_data))
		ndata_split <- split(ndata, ndata$time)

		# reorder within each time period using that period's actor set
		ndata_ordered <- do.call(rbind, lapply(seq_along(adj_data), function(i) {
			ref_mat <- adj_data[[i]]
			ref_actors <- rownames(ref_mat)
			if (is.null(ref_actors)) {
				ref_actors <- paste0("a", seq_len(nrow(ref_mat)))
			}
			time_data <- ndata_split[[as.character(time_labels[i])]]
			if (is.null(time_data)) {
				return(ndata[0, , drop = FALSE])
			}
			actor_idx <- match(ref_actors, time_data$actor)
			actor_idx <- actor_idx[!is.na(actor_idx)]
			time_data[actor_idx, , drop = FALSE]
		}))
		rownames(ndata_ordered) <- NULL
	} else {
		ref_actors <- rownames(adj_data)
		if (is.null(ref_actors)) {
			ref_actors <- paste0("a", seq_len(nrow(adj_data)))
		}
		actor_idx <- match(ref_actors, ndata$actor)
		actor_idx <- actor_idx[!is.na(actor_idx)]
		ndata_ordered <- ndata[actor_idx, , drop = FALSE]
		rownames(ndata_ordered) <- NULL
	}

	#
	return(ndata_ordered)
}

#' align dyad data matrix order to match adjacency matrix order
#'
#' ensures that row and column orders in dyad_data matrices
#' match the order of actors in the adjacency matrix.
#'
#' @param ddata a nested list structure containing dyadic attribute matrices
#' @param adj_data an adjacency matrix or list of adjacency matrices
#' @param is_longitudinal logical indicating whether the network is longitudinal
#'
#' @return a reordered nested list with matrix dimensions matching adjacency matrix order
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
align_dyad_data_order <- function(ddata, adj_data, is_longitudinal) {
	# align each time period separately for longitudinal data
	if (is_longitudinal) {
		ddata_ordered <- vector("list", length(ddata))
		names(ddata_ordered) <- names(ddata)

		for (t_idx in seq_along(ddata)) {
			t <- names(ddata)[t_idx]

			# get the correct adjacency matrix for this time period
			current_mat <- adj_data[[t_idx]]

			# align this time period's data
			ddata_ordered[[t]] <- align_single_time_dyad_data(
				ddata[[t]],
				current_mat
			)
		}
	} else {
		# cross-sectional case
		ddata_ordered <- vector("list", length(ddata))
		names(ddata_ordered) <- names(ddata)

			# process the cross-sectional dyad data
		for (t in names(ddata)) {
			ddata_ordered[[t]] <- align_single_time_dyad_data(
				ddata[[t]],
				adj_data
			)
		}
	}

	#
	return(ddata_ordered)
}

#' align dyad data for a single time period
#'
#'
#' @param time_data list of matrices for a single time period
#' @param ref_mat reference adjacency matrix for actor ordering
#'
#' @return aligned list of matrices
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
align_single_time_dyad_data <- function(time_data, ref_mat) {
	ref_row_actors <- rownames(ref_mat)
	ref_col_actors <- colnames(ref_mat)
	n_rows <- nrow(ref_mat)
	n_cols <- ncol(ref_mat)

	if (is.null(ref_row_actors)) {
		ref_row_actors <- if (!is.null(ref_col_actors) && n_rows == n_cols) {
			ref_col_actors
		} else {
			paste0("a", seq_len(n_rows))
		}
	}
	if (is.null(ref_col_actors)) {
		ref_col_actors <- if (n_rows == n_cols) {
			ref_row_actors
		} else {
			paste0("c", seq_len(n_cols))
		}
	}

	aligned_data <- vector("list", length(time_data))
	names(aligned_data) <- names(time_data)

	# process each variable
	for (var in names(time_data)) {
		mat <- time_data[[var]]

		# get current actor names
		curr_rows <- rownames(mat)
		curr_cols <- colnames(mat)

		# if no names, assume they're in order 1:n
		if (is.null(curr_rows)) {
			curr_rows <- as.character(seq_len(nrow(mat)))
		}
		if (is.null(curr_cols)) {
			curr_cols <- as.character(seq_len(ncol(mat)))
		}

		# reorder to match reference row and column actors independently
		row_idx <- match(ref_row_actors, curr_rows)
		col_idx <- match(ref_col_actors, curr_cols)

		# create reordered matrix
		mat_ordered <- mat[row_idx, col_idx, drop = FALSE]
		dimnames(mat_ordered) <- list(ref_row_actors, ref_col_actors)

		aligned_data[[var]] <- mat_ordered
	}

	#
	return(aligned_data)
}

#' convert edge data from igraph/network to new dyad_data format
#'
#' convert edge data extracted from igraph or network objects into
#' standardized dyad_data format used by netify. the output format is a
#' nested list structure: time periods -> variables -> matrices.
#'
#' @param edge_data a data frame (cross-sectional) or list of data frames (longitudinal)
#' containing edge attributes with 'from' and 'to' columns
#' @param adj_data an adjacency matrix (cross-sectional) or list of adjacency matrices
#' (longitudinal) providing dimensions and actor names
#' @param is_longitudinal logical indicating whether the network is longitudinal
#' @param is_symmetric logical indicating whether the network is symmetric/undirected
#'
#' @return a list with the following structure:
#' \itemize{
#'   \item for cross-sectional: list("1" = list(var1 = matrix, var2 = matrix, ...))
#'   \item for longitudinal: list(time1 = list(var1 = matrix, ...), time2 = list(...), ...)
#' }
#' returns NULL if edge_data is NULL.
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
convert_dyad_data_to_new_format <- function(
	edge_data,
	adj_data,
	is_longitudinal,
	is_symmetric = FALSE) {
	# handle null case
	if (is.null(edge_data)) {
		return(NULL)
	}

	# get time periods
	if (is_longitudinal) {
		time_periods <- names(adj_data) %||% as.character(seq_along(adj_data))
	} else {
		time_periods <- "1"
	}

	# pre-allocate the result structure
	dyad_data_new <- vector("list", length(time_periods))
	names(dyad_data_new) <- time_periods

	# process each time period
	for (t_idx in seq_along(time_periods)) {
		t <- time_periods[t_idx]

		# get the appropriate adjacency matrix for this time period
		if (is_longitudinal) {
			current_mat <- adj_data[[t_idx]]
			edge_df <- if (is.list(edge_data)) {
				edge_data[[t_idx]]
			} else {
				edge_data
			}
		} else {
			current_mat <- adj_data
			edge_df <- edge_data
		}

		# process this time period's edge data
		dyad_data_new[[t]] <- process_single_time_edge_data(
			edge_df,
			current_mat,
			is_symmetric
		)
	}

	#
	return(dyad_data_new)
}

#' process edge data for a single time period
#'
#'
#' @param edge_df edge data frame with 'from' and 'to' columns
#' @param current_mat adjacency matrix for dimensions and actor names
#' @param is_symmetric whether the network is symmetric/undirected
#'
#' @return list of matrices for each edge variable
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
process_single_time_edge_data <- function(edge_df, current_mat, is_symmetric) {
	# skip if no edge data
	if (is.null(edge_df) || nrow(edge_df) == 0) {
		return(list())
	}

	# get dimensions and actor names
	n_rows <- nrow(current_mat)
	n_cols <- ncol(current_mat)
	row_actor_names <- rownames(current_mat)
	col_actor_names <- colnames(current_mat)

	# if no actor names, create default ones
	if (is.null(row_actor_names)) {
		row_actor_names <- if (!is.null(col_actor_names) && n_rows == n_cols) {
			col_actor_names
		} else {
			paste0("a", seq_len(n_rows))
		}
	}
	if (is.null(col_actor_names)) {
		col_actor_names <- if (n_rows == n_cols) {
			row_actor_names
		} else {
			paste0("c", seq_len(n_cols))
		}
	}

	# get variable names (excluding from/to columns)
	var_names <- setdiff(names(edge_df), c("from", "to"))

	# if no variables, return empty list
	if (length(var_names) == 0) {
		return(list())
	}

	# create actor lookups for fast indexing
	row_actor_lookup <- seq_len(n_rows)
	names(row_actor_lookup) <- row_actor_names
	col_actor_lookup <- seq_len(n_cols)
	names(col_actor_lookup) <- col_actor_names

	# find valid edges
	from_indices <- row_actor_lookup[as.character(edge_df$from)]
	to_indices <- col_actor_lookup[as.character(edge_df$to)]
	valid_edges <- which(!is.na(from_indices) & !is.na(to_indices))
	symmetrize_edges <- isTRUE(is_symmetric) &&
		n_rows == n_cols &&
		identical(row_actor_names, col_actor_names)

	# create result list
	result <- vector("list", length(var_names))
	names(result) <- var_names

	#
	if (length(valid_edges) > 0) {
		# extract valid indices
		valid_from <- from_indices[valid_edges]
		valid_to <- to_indices[valid_edges]

		# create matrices for each variable
		for (var in var_names) {
			result[[var]] <- create_edge_matrix(
					edge_df[[var]][valid_edges],
					valid_from,
					valid_to,
					n_rows,
					n_cols,
					row_actor_names,
					col_actor_names,
					symmetrize_edges
				)
			}
		} else {
			# no valid edges - create zero matrices
			for (var in var_names) {
				mat <- array(0, dim = c(n_rows, n_cols))
				dimnames(mat) <- list(row_actor_names, col_actor_names)
				result[[var]] <- mat
			}
		}

	#
	return(result)
}

#' create edge attribute matrix
#'
#'
#' @param values edge attribute values
#' @param from_idx from indices
#' @param to_idx to indices
#' @param n_rows number of row actors
#' @param n_cols number of column actors
#' @param row_actor_names row actor names for dimnames
#' @param col_actor_names column actor names for dimnames
#' @param is_symmetric whether to symmetrize the matrix
#'
#' @return matrix of edge attributes
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
create_edge_matrix <- function(
	values,
	from_idx,
	to_idx,
	n_rows,
	n_cols,
	row_actor_names,
	col_actor_names,
	is_symmetric) {
	# initialize matrix
	mat <- array(0, dim = c(n_rows, n_cols))

	# create index matrix
	if (is_symmetric) {
		# for symmetric networks, set both directions
		edge_idx <- cbind(
			c(from_idx, to_idx),
			c(to_idx, from_idx)
		)
		mat[edge_idx] <- rep(values, 2)
	} else {
		# for directed networks, set one direction only
		edge_idx <- cbind(from_idx, to_idx)
		mat[edge_idx] <- values
	}

	# add dimnames
	dimnames(mat) <- list(row_actor_names, col_actor_names)

	#
	return(mat)
}

#' validate nodal data structure for netify objects
#'
#' validates that user-provided nodal data meets requirements for netify objects.
#' specifically, it checks for proper data frame structure,
#' required columns, and consistency with the network's actors.
#'
#' @param ndata a data frame containing nodal attributes
#' @param adj_data an adjacency matrix (cross-sectional) or list of adjacency
#' matrices (longitudinal) to check actor consistency
#' @param is_longitudinal logical indicating whether the network is longitudinal
#'
#' @return NULL (invisibly). function stops with error if validation fails.
#'
#' @details
#' the function performs the following checks:
#' \itemize{
#'   \item ndata must be a data.frame
#'   \item ndata must contain an 'actor' column
#'   \item for longitudinal networks, ndata must contain a 'time' column
#'   \item warns if actors in the network are missing from nodal_data
#' }
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
validate_nodal_data <- function(ndata, adj_data, is_longitudinal) {
	if (!is.data.frame(ndata)) {
		cli::cli_abort("{.arg nodal_data} must be a data.frame.")
	}

	# check for required columns
	if (!("actor" %in% names(ndata))) {
		cli::cli_abort("{.arg nodal_data} must contain an {.val actor} column.")
	}

	if (is_longitudinal && !("time" %in% names(ndata))) {
		cli::cli_abort(
			"For longitudinal networks, {.arg nodal_data} must contain a {.val time} column."
		)
	}

	# check actor names match
	first_mat <- if (is.list(adj_data)) {
		adj_data[[1]]
	} else {
		adj_data
	}
	actor_names <- rownames(first_mat)

	if (!is.null(actor_names)) {
		missing_actors <- setdiff(actor_names, ndata$actor)
		if (length(missing_actors) > 0) {
			cli::cli_alert_warning(
				"Some actors in the network are missing from nodal_data: {missing_actors}"
			)
		}
	}
}

#' validate dyad data structure for netify objects
#'
#' validate that user-provided dyad data meets the
#' requirements for netify objects. it checks for proper nested list structure,
#' matrix dimensions, and consistency with the network's time periods.
#'
#' @param ddata a nested list structure containing dyadic attributes
#' @param adj_data an adjacency matrix (cross-sectional) or list of adjacency
#' matrices (longitudinal) to check dimensions and time consistency
#' @param is_longitudinal logical indicating whether the network is longitudinal
#'
#' @return NULL (invisibly). function stops with error if validation fails.
#'
#' @details
#' the function validates the following structure:
#' \itemize{
#'   \item ddata must be a list
#'   \item each time period must be a list of matrices
#'   \item each matrix must have dimensions matching the number of actors
#'   \item warns if expected time periods are missing from dyad_data
#' }
#'
#' expected structure:
#' \itemize{
#'   \item cross-sectional: list("1" = list(var1 = matrix, var2 = matrix, ...))
#'   \item longitudinal: list(time1 = list(var1 = matrix, ...), time2 = list(...), ...)
#' }
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
validate_dyad_data <- function(ddata, adj_data, is_longitudinal) {
	if (!is.list(ddata)) {
		cli::cli_abort("{.arg dyad_data} must be a list.")
	}

	# get expected time periods
	if (is_longitudinal) {
		expected_times <- names(adj_data) %||% as.character(seq_along(adj_data))
	} else {
		expected_times <- "1"
	}

	# check time periods
	ddata_times <- names(ddata)
	missing_times <- setdiff(expected_times, ddata_times)
	if (length(missing_times) > 0) {
		cli::cli_alert_warning(
			"Some time periods are missing from dyad_data: {missing_times}"
		)
	}

	# validate each time period's structure
	for (t_idx in seq_along(ddata_times)) {
		t <- ddata_times[t_idx]
		time_data <- ddata[[t]]

			# get expected number of actors for this time period
			if (t %in% expected_times) {
				if (is_longitudinal) {
					t_idx_adj <- which(expected_times == t)
					n_rows <- nrow(adj_data[[t_idx_adj]])
					n_cols <- ncol(adj_data[[t_idx_adj]])
				} else {
					n_rows <- nrow(adj_data)
					n_cols <- ncol(adj_data)
				}
			} else {
				next # skip validation for unexpected time periods
			}

			# validate time period structure
			validate_single_time_dyad_data(time_data, t, n_rows, n_cols)
		}
	}

#' validate dyad data for a single time period
#'
#' validate the structure and dimensions of dyad data for a single time period.
#'
#' @param time_data list of matrices for a single time period
#' @param t time period label
#' @param n_rows expected number of row actors
#' @param n_cols expected number of column actors
#'
#' @return NULL (invisibly). stops with error if validation fails.
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
validate_single_time_dyad_data <- function(time_data, t, n_rows, n_cols) {
	if (!is.list(time_data)) {
		cli::cli_abort(
			"Each time period in {.arg dyad_data} must be a list of matrices."
		)
	}

	# check each variable
	for (var in names(time_data)) {
		mat <- time_data[[var]]

		if (!is.matrix(mat)) {
			cli::cli_abort(
				"dyad_data[['{t}']][['{var}']] must be a matrix."
			)
		}

		mat_dims <- dim(mat)
		if (mat_dims[1] != n_rows || mat_dims[2] != n_cols) {
			cli::cli_abort(
				"dyad_data[['{t}']][['{var}']] has incorrect dimensions. Expected {n_rows}x{n_cols}, got {mat_dims[1]}x{mat_dims[2]}."
			)
		}
	}
}
