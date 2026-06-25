#' Decompose a netify object into edge and node data frames
#'
#' `decompose_netify` separates a netify
#' object into its constituent parts: a data frame of edges with attributes and
#' a data frame of nodal attributes.
#'
#' @param netlet a netify object (class "netify") to be decomposed.
#' @param remove_zeros logical. if TRUE (default), edges with zero weight values
#'   are removed from the edge data frame. if FALSE, zero-weight edges are retained.
#'
#' @return a list containing two data frames:
#'   \itemize{
#'     \item \strong{edge_data}: a data frame where each row represents an edge with columns:
#'       \itemize{
#'         \item \code{from}: source node identifier
#'         \item \code{to}: target node identifier
#'         \item \code{time}: time period (character; "1" for cross-sectional networks)
#'         \item \code{weight}: edge weight values (using original weight variable name if specified)
#'         \item additional columns for any dyadic variables stored in the netify object
#'       }
#'     \item \strong{nodal_data}: a data frame where each row represents a node-time combination with columns:
#'       \itemize{
#'         \item \code{name}: node identifier
#'         \item \code{time}: time period (character; "1" for cross-sectional networks)
#'         \item additional columns for any nodal variables stored in the netify object
#'       }
#'   }
#'
#' @details
#' the function helpful for:
#'
#' \strong{edge data processing:}
#' \itemize{
#'   \item extracts the adjacency matrix (or array for longitudinal networks) from the netify object
#'   \item optionally removes zero-weight edges based on the remove_zeros parameter
#'   \item merges any dyadic variables stored in the netify object
#'   \item renames columns to standardized names (from, to, time)
#' }
#'
#' \strong{node data processing:}
#' \itemize{
#'   \item extracts nodal attributes if present, or constructs from actor_pds information
#'   \item ensures consistent time variable across node and edge data
#'   \item renames columns to standardized names (name, time)
#' }
#'
#' \strong{time handling:}
#' \itemize{
#'   \item for cross-sectional networks: sets time to "1" in both data frames
#'   \item for longitudinal networks: preserves original time periods as character values
#'   \item for ego networks: extracts time from ego-time concatenated identifiers
#' }
#'
#' \strong{variable preservation:}
#'
#' all dyadic and nodal variables stored in the netify object are preserved in the
#' output data frames. dyadic variables are merged with the edge data, while nodal
#' variables remain in the nodal data frame.
#'
#'
#' @examples
#' # load example data
#' data(icews)
#'
#' # example 1: cross-sectional network
#' icews_10 <- icews[icews$year == 2010, ]
#'
#' # create netify object
#' net_cs <- netify(
#'     icews_10,
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # decompose to data frames
#' decomposed_cs <- decompose_netify(net_cs)
#'
#' # examine structure
#' str(decomposed_cs)
#' head(decomposed_cs$edge_data)
#' head(decomposed_cs$nodal_data)
#'
#' # example 2: keep zero-weight edges
#' decomposed_with_zeros <- decompose_netify(net_cs, remove_zeros = FALSE)
#'
#' # compare edge counts
#' nrow(decomposed_cs$edge_data) # without zeros
#' nrow(decomposed_with_zeros$edge_data) # with zeros
#'
#' # example 4: use for visualization prep
#' \donttest{
#' # decompose for use with ggplot2
#' plot_data <- decompose_netify(net_cs)
#'
#' # can now use edge_data and nodal_data separately
#' # for network visualization
#' }
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export decompose_netify

decompose_netify <- function(netlet, remove_zeros = TRUE) {
	# input validation
	netify_check(netlet)
	if (!is.logical(remove_zeros) || length(remove_zeros) != 1) {
		stop("remove_zeros must be a single logical value")
	}

	# get attrs
	obj_attrs <- attributes(netlet)
	netify_type <- obj_attrs$netify_type
	weight_attr <- obj_attrs[["weight"]]
	ego_netlet <- obj_attrs$ego_netlet

	# get msrs
	msrmnts <- netify_measurements(netlet)

	# process edge data
	edge_data <- process_edge_data(
		netlet = netlet,
		netify_type = netify_type,
		weight_attr = weight_attr,
		remove_zeros = remove_zeros,
		ego_netlet = ego_netlet,
		is_bipartite = identical(obj_attrs$mode, "bipartite")
	)

	# process dyadic attributes if they exist
	if (!is.null(obj_attrs$dyad_data)) {
		edge_data <- merge_dyadic_attributes(
			edge_data = edge_data,
			dyad_data_attr = obj_attrs$dyad_data,
			netify_type = netify_type,
			is_bipartite = identical(obj_attrs$mode, "bipartite")
		)
	}

	# finalize edge data structure
	edge_data <- finalize_edge_data(edge_data, netify_type)

	# process nodal data
	# for longitudinal data, pass the resolved time labels
	if (netify_type != "cross_sec") {
		time_labels <- netify_measurements(netlet)$time
	} else {
		time_labels <- NULL
	}
	nodal_data <- process_nodal_data(obj_attrs, netify_type, time_labels)

	# synchronize time variables
	# guard zero-row edge_data: scalar $<- fails when nrow(edge_data) == 0
	if (netify_type == "cross_sec" && nrow(nodal_data) > 0 && nrow(edge_data) > 0) {
		edge_data$time <- as.character(nodal_data$time[1])
	}

	# prepare output
	out <- list(
		edge_data = edge_data,
		nodal_data = nodal_data
	)

	return(out)
}

#' decompose an igraph object into base r components
#'
#' `decompose_igraph` extracts the adjacency matrix and any vertex/edge attributes
#' from an igraph object, returning them in a standardized list format.
#'
#' @param grph an igraph object to be decomposed.
#' @param weight character string specifying the edge attribute to use as weights
#'   in the adjacency matrix. if NULL (default), the unweighted adjacency matrix
#'   is returned with 1s for edges and 0s for non-edges.
#'
#' @return a list containing four elements:
#'   \itemize{
#'     \item \strong{adj_mat}: the adjacency matrix extracted from the igraph object
#'       \itemize{
#'         \item for unipartite graphs: square matrix of dimension \eqn{n \times n}{n x n}
#'         \item for bipartite graphs: rectangular matrix of dimension \eqn{n_1 \times n_2}{n1 x n2}
#'         \item values are edge weights if specified, otherwise 0/1
#'       }
#'     \item \strong{ndata}: a data frame of vertex attributes, or NULL if none exist
#'       \itemize{
#'         \item always includes an 'actor' column with vertex names
#'         \item additional columns for each vertex attribute
#'       }
#'     \item \strong{ddata}: a data frame of edge attributes, or NULL if none exist
#'       \itemize{
#'         \item columns 'from' and 'to' specify edge endpoints
#'         \item additional columns for each edge attribute
#'       }
#'     \item \strong{weight}: the edge attribute name used for weights, if provided
#'   }
#'
#' @details
#' the function handles both unipartite and bipartite graphs appropriately:
#'
#' \strong{graph type detection:}
#' \itemize{
#'   \item bipartite graphs must have a logical 'type' vertex attribute
#'   \item if the 'type' attribute exists but is not logical, the graph is treated
#'     as unipartite with a warning
#' }
#'
#' \strong{vertex naming:}
#'
#' if the graph lacks vertex names, default names are assigned:
#' \itemize{
#'   \item unipartite graphs: "a1", "a2", ..., "an"
#'   \item bipartite graphs: "r1", "r2", ... for type 1; "c1", "c2", ... for type 2
#' }
#'
#' existing vertex names are always preserved and used in the output.
#'
#' \strong{matrix extraction:}
#' \itemize{
#'   \item unipartite: uses `as_adjacency_matrix()` to get \eqn{n \times n}{n x n} matrix
#'   \item bipartite: uses `as_biadjacency_matrix()` to get \eqn{n_1 \times n_2}{n1 x n2} matrix where
#'     rows correspond to type=FALSE vertices and columns to type=TRUE vertices
#' }
#'
#' \strong{attribute handling:}
#'
#' all vertex and edge attributes are preserved in the output data frames. system
#' attributes (like 'name' and 'type') are included alongside user-defined attributes.
#'
#' @note
#' for longitudinal networks with changing actor composition, explicitly set vertex
#' names before decomposition to ensure consistent actor identification across time
#' periods.
#'
#' the adjacency matrix is always returned as a standard r matrix (not sparse),
#' which may have memory implications for very large graphs.
#'
#' when edge attributes are used as weights, ensure they contain numeric values.
#' non-numeric edge attributes will cause an error.
#'
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export decompose_igraph

decompose_igraph <- function(grph, weight = NULL) {
	stopifnot(inherits(grph, "igraph"))

	# check if bipartite - but also verify the type attribute is actually logical
	is_bipartite <- FALSE
	if (igraph::is_bipartite(grph)) {
		vertex_types <- igraph::V(grph)$type

		# only treat as bipartite if type attribute exists and is logical
		if (!is.null(vertex_types) && is.logical(vertex_types)) {
			is_bipartite <- TRUE
		} else {
			# graph thinks it's bipartite but type attribute is invalid
			# treat as unipartite and warn the user
			cli::cli_warn(c(
				"!" = "Graph has a {.field type} vertex attribute but it's not logical.",
				"i" = "Treating graph as unipartite.",
				">" = "For bipartite graphs, use logical values: {.code V(g)$type <- c(TRUE, FALSE, ...)}"
			))
		}
	}

	# get existing vertex names if any
	vertex_names <- igraph::V(grph)$name

	if (is_bipartite) {
		n_type1 <- sum(!vertex_types)
		n_type2 <- sum(vertex_types)

		# for bipartite graphs, get the bipartite adjacency matrix
		# this returns a matrix of dimension n_type1 x n_type2
		adj_mat <- igraph::as_biadjacency_matrix(
			grph,
			attr = weight,
			sparse = FALSE
		)

		# create vertex names if they don't exist
		if (is.null(vertex_names)) {
			vertex_names <- character(length(vertex_types))
			vertex_names[!vertex_types] <- paste0("r", seq_len(n_type1))
			vertex_names[vertex_types] <- paste0("c", seq_len(n_type2))

			# set row/column names for the bipartite adjacency matrix
			rownames(adj_mat) <- paste0("r", seq_len(n_type1))
			colnames(adj_mat) <- paste0("c", seq_len(n_type2))
		} else {
			# use existing names
			rownames(adj_mat) <- vertex_names[!vertex_types]
			colnames(adj_mat) <- vertex_names[vertex_types]
		}
	} else {
		# for unipartite graphs, get the regular adjacency matrix
		adj_mat <- igraph::as_adjacency_matrix(
			grph,
			attr = weight,
			sparse = FALSE
		)

		# create vertex names if they don't exist
		if (is.null(vertex_names)) {
			n_vertices <- igraph::vcount(grph)
			# paste0("a", seq_len(0)) recycles to "a" (length 1), so guard the
			# handle zero-vertex graphs before building names
			# 0x0 adjacency
			vertex_names <- if (n_vertices == 0L) {
				character(0)
			} else {
				paste0("a", seq_len(n_vertices))
			}
			rownames(adj_mat) <- vertex_names
			colnames(adj_mat) <- vertex_names
		} else {
			# use existing names
			rownames(adj_mat) <- vertex_names
			colnames(adj_mat) <- vertex_names
		}
	}

	# pull out vertex attributes as a data.frame, if any
	v_labs <- igraph::vertex_attr_names(grph)
	if (length(v_labs) > 0) {
		ndata <- igraph::as_data_frame(grph, what = "vertices")
		# always add vertex names as 'actor' column
		ndata$actor <- vertex_names
		# drop a redundant igraph `name` column that duplicates `actor`
		if ("name" %in% names(ndata) &&
			identical(as.character(ndata$name), as.character(vertex_names))) {
			ndata$name <- NULL
		}
		# put 'actor' first so downstream column-position assumptions hold
		other_cols <- setdiff(names(ndata), "actor")
		ndata <- ndata[, c("actor", other_cols), drop = FALSE]
	} else {
		ndata <- NULL
	}

	# pull out edge attributes as a data.frame, if any
	e_labs <- igraph::edge_attr_names(grph)
	if (length(e_labs) > 0) {
		ddata <- igraph::as_data_frame(grph, what = "edges")
		# update from/to to use our naming scheme if they're numeric
		if (is.numeric(ddata$from) && is.numeric(ddata$to)) {
			ddata$from <- vertex_names[ddata$from]
			ddata$to <- vertex_names[ddata$to]
		}
		# drop duplicated `weight` column if it shadows a named weight
		if (!is.null(weight) && !identical(weight, "weight") &&
			"weight" %in% names(ddata) && weight %in% names(ddata)) {
			wcol <- ddata$weight
			named_col <- ddata[[weight]]
			if (is.numeric(wcol) && is.numeric(named_col) &&
				length(wcol) == length(named_col) &&
				isTRUE(all.equal(wcol, named_col, check.attributes = FALSE))) {
				ddata$weight <- NULL
			}
		}
		# the weight column already lives in the adjacency, so drop it from
		# dyad covariates to avoid double-storage on the round-trip
		if (!is.null(weight) && weight %in% names(ddata)) {
			ddata[[weight]] <- NULL
		}
		# if only from/to are left, there are no real edge covariates
		if (all(names(ddata) %in% c("from", "to"))) {
			ddata <- NULL
		}
	} else {
		ddata <- NULL
	}

	# return the decomposed components
	list(
		adj_mat = adj_mat,
		ndata = ndata,
		ddata = ddata,
		weight = weight
	)
}

#' decompose a network object into base r components
#'
#' `decompose_statnet` (also available as `decompose_network`) extracts the
#' adjacency matrix and any vertex/edge attributes
#' from a network object (from the statnet suite), returning them in a standardized
#' list format.
#'
#' @param ntwk a network object (class "network") to be decomposed.
#' @param weight character string specifying the edge attribute to use as weights
#'   in the adjacency matrix. if NULL (default), the unweighted adjacency matrix
#'   is returned with 1s for edges and 0s for non-edges.
#'
#' @return a list containing four elements:
#'   \itemize{
#'     \item \strong{adj_mat}: the adjacency matrix extracted from the network object
#'       \itemize{
#'         \item for unipartite networks: square matrix of dimension \eqn{n \times n}{n x n}
#'         \item for bipartite networks: full square matrix of dimension \eqn{(n_1 + n_2) \times (n_1 + n_2)}{(n1 + n2) x (n1 + n2)}
#'         \item values are edge weights if specified, otherwise 0/1
#'       }
#'     \item \strong{ndata}: a data frame of vertex attributes, or NULL if none exist
#'       \itemize{
#'         \item always includes an 'actor' column with vertex names
#'         \item additional columns for each vertex attribute (excluding system attributes)
#'       }
#'     \item \strong{ddata}: a data frame of edge attributes, or NULL if none exist
#'       \itemize{
#'         \item columns 'from' and 'to' specify edge endpoints using vertex names
#'         \item additional columns for each edge attribute
#'       }
#'     \item \strong{weight}: the edge attribute name used for weights, if provided
#'   }
#'
#' @details
#' the function handles both unipartite and bipartite networks appropriately:
#'
#' \strong{network type detection:}
#' \itemize{
#'   \item bipartite networks are identified using `is.bipartite()`
#'   \item the bipartite partition size is retrieved from the 'bipartite' network attribute
#' }
#'
#' \strong{vertex naming:}
#'
#' the function checks for existing vertex names in the 'vertex.names' attribute.
#' if names are just the default numeric sequence (1, 2, 3, ...), they are treated
#' as missing. default names are assigned when needed:
#' \itemize{
#'   \item unipartite networks: "a1", "a2", ..., "an"
#'   \item bipartite networks: "r1", "r2", ... for first partition; "c1", "c2", ... for second partition
#' }
#'
#' \strong{matrix extraction:}
#'
#' unlike igraph's bipartite handling, the network package returns the full
#' adjacency matrix even for bipartite networks. the function:
#' \itemize{
#'   \item extracts the full matrix using `as.matrix.network.adjacency()`
#'   \item for bipartite networks, the matrix has dimension \eqn{(n_1 + n_2) \times (n_1 + n_2)}{(n1 + n2) x (n1 + n2)} with
#'     the first \eqn{n_1}{n1} rows/columns for the first partition
#' }
#'
#' \strong{attribute handling:}
#'
#' system attributes ('vertex.names' and 'na') are excluded from the vertex
#' attribute data frame. all user-defined vertex and edge attributes are preserved.
#'
#' @note
#' for longitudinal networks with changing actor composition, explicitly set vertex
#' names before decomposition to ensure consistent actor identification across time
#' periods.
#'
#' the adjacency matrix format differs between this function and `decompose_igraph`
#' for bipartite networks: this function returns the full square matrix while
#' `decompose_igraph` returns only the rectangular bipartite portion.
#'
#' edge directions are preserved in the adjacency matrix according to the network's
#' directed/undirected property.
#'
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export decompose_statnet
#' @aliases decompose_network

decompose_statnet <- function(ntwk, weight = NULL) {
	stopifnot(inherits(ntwk, "network"))

	# check if bipartite
	is_bipartite <- network::is.bipartite(ntwk)

	# get or create vertex names
	vertex_names <- network::get.vertex.attribute(ntwk, "vertex.names")

	# check if vertex names are just the default numeric sequence
	# if so, treat as if they don't exist
	if (!is.null(vertex_names) &&
		is.numeric(vertex_names) &&
		identical(vertex_names, seq_len(network::network.size(ntwk)))) {
		vertex_names <- NULL
	}

	if (is_bipartite) {
		# get bipartite partition info
		bip_partition <- network::get.network.attribute(ntwk, "bipartite")
		n_type1 <- bip_partition
		n_type2 <- network::network.size(ntwk) - bip_partition

		# create vertex names if they don't exist
		if (is.null(vertex_names)) {
			vertex_names <- c(
				paste0("r", seq_len(n_type1)),
				paste0("c", seq_len(n_type2))
			)
		}

		# get the full adjacency matrix first
		adj_mat <- network::as.matrix.network.adjacency(
			ntwk,
			attrname = weight
		)

		# set row/column names
		rownames(adj_mat) <- vertex_names[1:n_type1]
		colnames(adj_mat) <- vertex_names[(n_type1 + 1):(n_type1 + n_type2)]
	} else {
		# for unipartite networks, get the regular adjacency matrix
		adj_mat <- network::as.matrix.network.adjacency(
			ntwk,
			attrname = weight
		)

		# create vertex names if they don't exist
		if (is.null(vertex_names)) {
			n_vertices <- network::network.size(ntwk)
			vertex_names <- paste0("a", seq_len(n_vertices))
		}

		# set row/column names
		rownames(adj_mat) <- vertex_names
		colnames(adj_mat) <- vertex_names
	}

	# vertex attributes
	v_labs <- network::list.vertex.attributes(ntwk)
	# don't include system attributes as real vertex attributes
	system_attrs <- c("vertex.names", "na")
	v_labs <- setdiff(v_labs, system_attrs)

	if (length(v_labs) > 0) {
		# use lapply for better performance than sapply
		ndata_list <- lapply(v_labs, function(attr) {
			network::get.vertex.attribute(ntwk, attr)
		})
		names(ndata_list) <- v_labs
		ndata <- as.data.frame(ndata_list, stringsAsFactors = FALSE)

		# always add vertex names as 'actor' column
		ndata$actor <- vertex_names
	} else {
		ndata <- NULL
	}

	# edge attributes
	e_labs <- network::list.edge.attributes(ntwk)
	if (length(e_labs) > 0) {
		edgelist <- network::as.edgelist(ntwk)

		# use lapply for better performance
		attr_list <- lapply(e_labs, function(attr) {
			network::get.edge.attribute(ntwk, attr)
		})
		names(attr_list) <- e_labs
		attr_df <- as.data.frame(attr_list, stringsAsFactors = FALSE)

		ddata <- data.frame(
			from = vertex_names[edgelist[, 1]],
			to = vertex_names[edgelist[, 2]],
			attr_df,
			stringsAsFactors = FALSE
		)
	} else {
		ddata <- NULL
	}

	# return the decomposed components
	list(
		adj_mat = adj_mat,
		ndata = ndata,
		ddata = ddata,
		weight = weight
	)
}

#' @rdname decompose_statnet
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
decompose_network <- decompose_statnet
