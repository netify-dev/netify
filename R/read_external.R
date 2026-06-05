#' Read a network from common file formats into a netify object
#'
#' thin wrappers around `igraph::read_graph()` for the formats users
#' coming from gephi / pajek / networkx most often hand over (graphml,
#' pajek `.net`, gml). each reader loads the file via igraph and
#' immediately runs `netify()` on the result so the user gets a
#' netify back in one call, with edge weights auto-detected and
#' directedness preserved.
#'
#' @param file path to the input file.
#' @param ... passed to `netify(igraph_obj, ...)` -- typically
#' `symmetric=`, `mode=`, or `weight=` overrides.
#' @return a netify object.
#' @name read_external
#' @rdname read_external
#'
#' @author cassy dorff, shahryar minhas
#'
NULL

#' @rdname read_external
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export read_graphml
read_graphml <- function(file, ...) {
	if (!requireNamespace("igraph", quietly = TRUE)) {
		cli::cli_abort("{.pkg igraph} required for {.fn read_graphml}.")
	}
	if (!file.exists(file)) {
		cli::cli_abort("File {.path {file}} does not exist.")
	}
	ig <- igraph::read_graph(file, format = "graphml")
	netify(ig, ...)
}

#' @rdname read_external
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export read_pajek
read_pajek <- function(file, ...) {
	if (!requireNamespace("igraph", quietly = TRUE)) {
		cli::cli_abort("{.pkg igraph} required for {.fn read_pajek}.")
	}
	if (!file.exists(file)) {
		cli::cli_abort("File {.path {file}} does not exist.")
	}
	ig <- igraph::read_graph(file, format = "pajek")
	netify(ig, ...)
}

#' @rdname read_external
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export read_gml
read_gml <- function(file, ...) {
	if (!requireNamespace("igraph", quietly = TRUE)) {
		cli::cli_abort("{.pkg igraph} required for {.fn read_gml}.")
	}
	if (!file.exists(file)) {
		cli::cli_abort("File {.path {file}} does not exist.")
	}
	ig <- igraph::read_graph(file, format = "gml")
	netify(ig, ...)
}
