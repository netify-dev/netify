#' Convert netify objects to statnet network format
#'
#' transforms netify network objects into statnet's network
#' objects (also available as `netify_to_network`,
#' `to_statnet`, and `to_network`),
#' providing access to the extensive statistical modeling capabilities
#' of the statnet suite, including ergms (exponential random graph models),
#' descriptive statistics, and network visualization tools.
#'
#' @param netlet a netify object containing network data. cross-sectional,
#'   longitudinal, and multilayer netlets are all supported; multilayer
#'   inputs are dispatched layer-by-layer and the layer results are
#'   returned in a named list keyed by layer.
#' @param add_nodal_attribs logical. if TRUE (default), includes nodal attributes
#'   from the netify object as vertex attributes in the network object. set to
#'   FALSE to create a network with structure only.
#' @param add_dyad_attribs logical. if TRUE (default), includes dyadic attributes
#'   from the netify object as edge attributes in the network object. each
#'   dyad variable is attached in two places on the resulting network: as a
#'   network-level attribute under its original name (the full \eqn{n\times n}
#'   matrix) and as a per-edge attribute under \code{<var>_e}. the trailing
#'   \code{_e} disambiguates the per-edge edgelist from the network-level
#'   matrix. for \code{ergm::edgecov()} pass the \emph{original} (matrix)
#'   name, not the \code{_e} alias, since \code{edgecov()} reads a
#'   network-level matrix attribute. the \code{_e} per-edge attribute is
#'   exposed for descriptive use (e.g. coloring edges in
#'   \code{network::plot.network}).
#'
#' @return a network object or list of network objects:
#'   \describe{
#'     \item{cross-sectional networks}{returns a single network object}
#'     \item{longitudinal networks}{returns a named list of network objects
#'       with class \code{c("network.list", "list")}, names corresponding to
#'       time periods. the \code{network.list} class is the format that
#'       \pkg{tergm} cmle (\code{tergm(nl ~ form(.) + persist(.), estimate = "cmle",
#'       times = seq_along(nl))}) and \code{stergm} expect directly, so the
#'       output is plug-and-play for longitudinal ergms.}
#'     \item{multilayer networks}{returns a named list of (per-time) network
#'       objects keyed by layer name}
#'   }
#'
#'   the resulting network object(s) will have:
#'   \itemize{
#'     \item vertices named according to actors in the netify object
#'     \item edge weights from the netify weight variable stored as "weight" attribute
#'     \item vertex attributes for each nodal variable (if add_nodal_attribs = TRUE)
#'     \item edge attributes for each dyadic variable (if add_dyad_attribs = TRUE);
#'       per-edge attributes carry the \code{_e} suffix, network-level
#'       attributes keep the original name
#'     \item proper directedness based on the symmetric parameter of the netify object
#'     \item a \code{netify_na_cols} attribute (character vector) listing
#'       nodal columns that carry \code{na}s; pass this directly to
#'       \code{\link{drop_na_actors}()} to drop the offending actors before
#'       fitting ergm formulas that reference those columns
#'   }
#'
#' @details
#' the conversion process handles different netify structures:
#' \itemize{
#'   \item \strong{cross-sectional}: direct conversion to a single network object
#'   \item \strong{longitudinal arrays}: internally converted to list format, then
#'     each time slice becomes a separate network object
#'   \item \strong{longitudinal lists}: each time period converted to separate network object
#' }
#'
#' the statnet network format stores networks as an edgelist with attributes,
#' making it memory-efficient for sparse networks. all nodal and dyadic attributes
#' from the netify object are preserved and can be used in subsequent ergm
#' modeling or network analysis.
#'
#' for longitudinal data, each time period results in an independent network
#' object. this format is suitable for discrete-time network analysis or
#' pooled ergm estimation across time periods.
#'
#' @note
#' this function requires the network package (part of statnet) to be installed.
#'
#' for ergm modeling, the ergm package (also part of statnet) should be loaded
#' after creating the network objects.
#'
#' @examples
#' data(classroom_edges)
#' data(classroom_nodes)
#'
#' net <- netify(
#'     classroom_edges,
#'     actor1 = "from", actor2 = "to",
#'     symmetric = TRUE,
#'     nodal_data = classroom_nodes,
#'     missing_to_zero = TRUE
#' )
#'
#' ntwk <- netify_to_statnet(net)
#'
#' # examine the result
#' ntwk
#' network::network.size(ntwk) # number of vertices
#' network::network.edgecount(ntwk) # number of edges
#' network::list.vertex.attributes(ntwk) # nodal attributes
#' network::get.vertex.attribute(ntwk, "gender") # gender by student
#'
#' # check network properties
#' network::is.directed(ntwk)
#' network::has.loops(ntwk)
#'
#' # longitudinal example
#' classroom_panel <- rbind(
#'     transform(classroom_edges[1:12, ], wave = 1),
#'     transform(classroom_edges[13:24, ], wave = 2)
#' )
#'
#' longit_net <- netify(
#'     classroom_panel,
#'     actor1 = "from", actor2 = "to", time = "wave",
#'     symmetric = TRUE,
#'     missing_to_zero = TRUE
#' )
#'
#' ntwk_list <- netify_to_statnet(longit_net)
#'
#' # examine structure
#' length(ntwk_list) # number of time periods
#' names(ntwk_list) # time period labels
#'
#' \dontrun{
#' # use with ergm for modeling (requires ergm package)
#' library(ergm)
#' model <- ergm(ntwk ~ edges + nodematch("gender"))
#' }
#'
#' @author ha eun choi, cassy dorff, colin henry, shahryar minhas
#'
#' @export netify_to_statnet
#' @aliases netify_to_network to_statnet to_network

netify_to_statnet <- function(
	netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE) {
	# check if netify object
	netify_check(netlet)

	# assert dependencies for remapping data to network
	assert_dependency("network")

	# multilayer netlets: dispatch one call per layer and return a named
	# list keyed by layer; each element is itself a network or a
	# (longit) list of networks
	layer_names <- attributes(netlet)$layers
	if (length(layer_names) > 1) {
		ntwk_by_layer <- lapply(layer_names, function(lyr) {
			netify_to_statnet(
				subset_netify(netlet, layers = lyr),
				add_nodal_attribs = add_nodal_attribs,
				add_dyad_attribs = add_dyad_attribs
			)
		})
		names(ntwk_by_layer) <- layer_names
		return(ntwk_by_layer)
	}

	## three cases: cross-sec/matrix, longit list, longit array
	netlet_type <- attr(netlet, "netify_type")

	# if type array convert to list since network
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

	# detect nodal columns that carry nas; ergm terms like nodecov() and
	# nodematch() reject na-bearing vertex attributes, so stash the
	# offending columns on the resulting object(s) and inform the user
	# once how to clean them up via `drop_na_actors()`
	na_cols <- detect_na_cols(attr(netlet, "nodal_data"))
	if (length(na_cols) > 0L) {
		na_cols_call <- paste0(
			"c(",
			paste(paste0("'", na_cols, "'"), collapse = ", "),
			")"
		)
		cli::cli_inform(c(
			"!" = "Nodal columns with {.val NA} detected: {.val {na_cols}}. Ergm terms like {.fn nodecov}/{.fn nodematch} will refuse to fit.",
			"i" = "Use {.code drop_na_actors(net, cols = {na_cols_call})} (or impute) before refitting."
		),
		.frequency = "once",
		.frequency_id = paste0(
			"to_statnet_na_cols_", paste(sort(na_cols), collapse = "_")
		))
	}

	## cross-sec case
	if (netlet_type == "cross_sec") {
		# convert to a statnet network object
		ntwk <- netify_net_to_statnet(netlet)

		# process nodal attributes if exist
		if (nodal_data_exist & add_nodal_attribs) {
			ntwk <- add_nodal_to_statnet(
				netlet, attr(netlet, "nodal_data"), ntwk
			)
		}

		# process dyadic attributes if exist
		if (dyad_data_exist & add_dyad_attribs) {
			ntwk <- add_dyad_to_statnet(
				netlet, attr(netlet, "dyad_data"), ntwk
			)
		}

		# stash na-bearing nodal columns on the network for downstream
		# introspection (e.g. drop_na_actors(net, cols = attr(., "netify_na_cols")))
		attr(ntwk, "netify_na_cols") <- na_cols
	} # done with cross-sec case

	## longit case
	if (netlet_type %in% c("longit_array", "longit_list")) {
		# iterate through netlet
		ntwk <- lapply(1:length(netlet), function(ii) {
			# get netlet slice
			netlet_slice <- netlet[[ii]]

			# get time listing
			time_val <- names(netlet)[ii]

			# convert to a statnet network object
			ntwk_slice <- netify_net_to_statnet(netlet_slice)

			# process nodal attributes if exist
			if (nodal_data_exist & add_nodal_attribs) {
				ntwk_slice <- add_nodal_to_statnet(
					netlet_slice, attr(netlet, "nodal_data"),
					ntwk_slice, time_val
				)
			}

			# process dyadic attributes if exist
			if (dyad_data_exist & add_dyad_attribs) {
				ntwk_slice <- add_dyad_to_statnet(
					netlet_slice, attr(netlet, "dyad_data"),
					ntwk_slice, time_val
				)
			}

			# carry na-bearing nodal column list onto each slice
			attr(ntwk_slice, "netify_na_cols") <- na_cols

			#
			return(ntwk_slice)
		})
		names(ntwk) <- names(netlet)
		# stamp for tergm / stergm and print.network.list dispatch
		class(ntwk) <- c("network.list", "list")
		attr(ntwk, "netify_na_cols") <- na_cols
	} # done with longit case

	#
	return(ntwk)
}

#' detect_na_cols
#'
#' return the names of columns in a nodal_data frame (or list of frames,
#' for longit) that contain any na; skips bookkeeping columns (actor,
#' time, layer).
#'
#' @param nodal_data nodal_data attribute from a netify object
#' @return character vector of column names with nas (possibly empty)
#'
#' @keywords internal
#' @noRd

detect_na_cols <- function(nodal_data) {
	if (is.null(nodal_data)) return(character(0))

	# handle longit lists of nodal frames
	if (is.list(nodal_data) && !is.data.frame(nodal_data)) {
		cols <- unique(unlist(lapply(nodal_data, detect_na_cols), use.names = FALSE))
		return(if (is.null(cols)) character(0) else cols)
	}

	if (!is.data.frame(nodal_data) || nrow(nodal_data) == 0L) {
		return(character(0))
	}

	skip <- intersect(c("actor", "time", "layer"), names(nodal_data))
	candidate <- setdiff(names(nodal_data), skip)
	if (length(candidate) == 0L) return(character(0))

	has_na <- vapply(
		nodal_data[, candidate, drop = FALSE],
		function(x) any(is.na(x)),
		logical(1)
	)
	candidate[has_na]
}

#' @rdname netify_to_statnet
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
netify_to_network <- netify_to_statnet

#' @rdname netify_to_statnet
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
to_statnet <- netify_to_statnet

#' @rdname netify_to_statnet
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
to_network <- netify_to_statnet

#' netify_net_to_statnet
#'
#' convert netify object to a statnet network object
#'
#' @param netlet netify object
#' @return statnet network object
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd

netify_net_to_statnet <- function(netlet) {
	# check if bipartite
	bipartite_logical <- ifelse(attr(netlet, "mode") == "bipartite", TRUE, FALSE)

	# if weight is null then create logical to set value for ignore.eval
	if (is.null(attr(netlet, "weight", exact = TRUE))) {
		ignore_eval <- TRUE
	} else {
		ignore_eval <- FALSE
	}

	# bipartite networks have two disjoint vertex sets, so loops are
	# undefined; force loops = false in the bipartite branch even if
	# diag_to_NA was left as false in the netify (the default for
	# bipartite). otherwise statnet stamps `loops = true` on the network
	# object, which is conceptually wrong and trips up some ergm terms
	# that introspect on `has.loops()`.
	loops_flag <- if (bipartite_logical) FALSE else !attr(netlet, "diag_to_NA")

	# convert to a statnet network object
	statnet_object <- network::network(
		get_raw(netlet),
		matrix.type = "adjacency",
		directed = !all(attr(netlet, "symmetric")),
		loops = loops_flag,
		bipartite = bipartite_logical,
		names.eval = attr(netlet, "weight", exact = TRUE),
		ignore.eval = ignore_eval
	)

	# set as network attribute as well if weight provided
	if (!is.null(attr(netlet, "weight", exact = TRUE))) {
		network::set.network.attribute(
			statnet_object, attr(netlet, "weight", exact = TRUE),
			get_raw(netlet)
		)
	}

	#
	return(statnet_object)
}

#' add_nodal_to_statnet
#'
#' add nodal attributes to a network object from netify object
#'
#' @param netlet netify object
#' @param node_data node data from netlet object
#' @param statnet_object network object to modify
#' @param time time indicator for longit case
#' @return statnet network object with nodal attributes added
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd

add_nodal_to_statnet <- function(
	netlet, node_data, statnet_object, time = NULL) {
	# slice by time, finding the time column by name when possible
	if (!is.null(time)) {
		if ("time" %in% names(node_data)) {
			node_data <- node_data[
				as.character(node_data[["time"]]) == as.character(time), ,
				drop = FALSE
			]
		} else if (ncol(node_data) >= 2) {
			node_data <- node_data[
				as.character(node_data[, 2]) == as.character(time), ,
				drop = FALSE
			]
		}
	}

	# align nodal rows to the network's vertex.names so padded actors
	# (e.g., from bind_netifies(align_actors="union")) receive na, not
	# the wrong-actor values that positional indexing would assign
	vnames <- network::get.vertex.attribute(statnet_object, "vertex.names")
	actor_col <- if ("actor" %in% names(node_data)) "actor" else names(node_data)[1]
	row_idx <- match(vnames, as.character(node_data[[actor_col]]))

	# skip bookkeeping columns so they do not leak as vertex covariates
	skip_cols <- intersect(c("actor", "time", "layer"), names(node_data))
	cov_cols <- setdiff(names(node_data), skip_cols)
	for (cv in cov_cols) {
		vals <- node_data[[cv]][row_idx]
		network::set.vertex.attribute(statnet_object, cv, vals)
	}

	return(statnet_object)
}

#' add_dyad_to_statnet
#'
#' add dyad attributes to a network object from netify object
#'
#' @param netlet netify object
#' @param dyad_data_list dyad data from netlet object
#' @param statnet_object network object to modify
#' @param time time indicator for longit case
#' @return statnet network object with dyad attributes added
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd

add_dyad_to_statnet <- function(
	netlet, dyad_data_list, statnet_object, time = NULL) {
	# get dyadic data for specified time period
	if (is.null(time)) {
		var_matrices <- dyad_data_list[[1]]
	} else {
		var_matrices <- dyad_data_list[[time]]
	}

	# get var names from the list of matrices
	vars <- names(var_matrices)

	# cache netlet attributes for efficiency
	netlet_mode <- attr(netlet, "mode")
	netlet_diag_to_NA <- attr(netlet, "diag_to_NA")
	bipartite_logical <- netlet_mode == "bipartite"

	# one-shot inform on the `_e` per-edge attribute names so users
	# know what to plug into ergm::edgecov() without spelunking
	if (!bipartite_logical && length(vars) > 0L) {
		e_names <- paste0(vars, "_e")
		first_e <- e_names[1]
		first_var <- vars[1]
		cli::cli_inform(c(
			"i" = "Dyad covariates attached as per-edge attributes under {.val {e_names}} and as network-level matrices under their original names ({.val {vars}}).",
			"i" = "For {.fn ergm::edgecov} use the matrix name (e.g. {.code edgecov('{first_var}')}); the {.val _e} per-edge attribute is for descriptive use such as edge styling."
		),
		.frequency = "once",
		.frequency_id = paste0(
			"to_statnet_e_suffix_", paste(sort(vars), collapse = "_")
		))
	}

	# iterate through dyadic vars and add into network object
	for (ii in seq_along(vars)) {
		var_name <- vars[ii]

		# get matrix for this variable
		d_data <- var_matrices[[var_name]]

		# replace diagonal with 0s except if
		# netlet is bipartite or
		# diag_to_NA is set to false
		if (!bipartite_logical && netlet_diag_to_NA) {
			diag(d_data) <- 0
		}

		# set as edge attrib if not bipartite
		# note: we iterate over edges individually because
		# network::set.edge.value with a full matrix can mismap values
		if (!bipartite_logical) {
			vnames <- network::get.vertex.attribute(statnet_object, "vertex.names")
			el <- network::as.edgelist(statnet_object)
			n_edges <- nrow(el)
			edge_vals <- rep(0, n_edges)
			for (e_idx in seq_len(n_edges)) {
				from_name <- vnames[el[e_idx, 1]]
				to_name <- vnames[el[e_idx, 2]]
				if (from_name %in% rownames(d_data) && to_name %in% colnames(d_data)) {
					edge_vals[e_idx] <- d_data[from_name, to_name]
				}
			}
			network::set.edge.attribute(
				statnet_object, paste0(var_name, "_e"), edge_vals
			)
		}

		# set as network attrib
		network::set.network.attribute(
			statnet_object, var_name, d_data
		)
	}

	return(statnet_object)
}

#' as.network method for netify objects
#'
#' s3 method that lets statnet's \code{\link[network]{as.network}} generic
#' dispatch on netify objects. equivalent to \code{netify_to_statnet(x, ...)}.
#' registered against the \pkg{network} namespace in \code{.onload}, so the
#' dispatch works regardless of whether \pkg{network} is attached before or
#' after \pkg{netify}.
#'
#' @param x a netify object.
#' @param ... extra arguments forwarded to \code{\link{netify_to_statnet}}
#'   (e.g. \code{add_nodal_attribs}, \code{add_dyad_attribs}).
#' @return a statnet \code{network} object or list of network objects, as
#'   produced by \code{netify_to_statnet}.
#'
#' @author shahryar minhas
#'
#' @rawNamespace if (requireNamespace("network", quietly = TRUE)) S3method(network::as.network, netify)

as.network.netify <- function(x, ...) {
	netify_to_statnet(x, ...)
}
