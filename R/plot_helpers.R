#' Extract edges layer from netify plot components
#'
#' extracts the edge layer from a netify plot components object, allowing for
#' manual plot construction and customization. this function is part of the
#' modular plotting system that enables fine-grained control over network
#' visualization elements.
#'
#' @param comp a netify_plot_components object returned from
#'   \code{plot(..., return_components = TRUE)}
#'
#' @return a custom object of class "netify_edge" that can be added to a ggplot
#'   object using the + operator. the object contains the edge layer with all
#'   its aesthetic mappings and data.
#'
#' @examples
#' \donttest{
#' # create a netify object
#' mat <- matrix(c(NA, 1, 0, 0, NA, 1, 1, 0, NA), 3, 3,
#'     dimnames = list(c("alice", "bob", "carol"),
#'                     c("alice", "bob", "carol")))
#' net <- new_netify(mat, symmetric = FALSE)
#'
#' # get plot components
#' comp <- plot(net, return_components = TRUE)
#'
#' # build custom plot with edges
#' library(ggplot2)
#' ggplot() +
#'     netify_edge(comp)
#' }
#'
#' @seealso \code{\link{plot.netify}}, \code{\link{netify_node}},
#'   \code{\link{assemble_netify_plot}}
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
netify_edge <- function(comp) {
	if (!inherits(comp, "netify_plot_components")) {
		cli::cli_abort("Input must be netify_plot_components from {.code plot(..., return_components = TRUE)}")
	}
	if (is.null(comp$edges)) {
		cli::cli_abort("No edges in this plot")
	}

	# create the layer
	layer <- ggplot2::layer(
		geom = comp$edges$geom,
		data = comp$edges$data,
		mapping = comp$edges$mapping,
		stat = comp$edges$stat,
		position = comp$edges$position,
		params = comp$edges$params,
		inherit.aes = comp$edges$inherit.aes,
		show.legend = comp$edges$show.legend
	)

	# return it wrapped in a list with a custom class
	structure(list(layer), class = c("netify_edge", "list"))
}

#' add netify_edge to ggplot
#'
#' s3 method to add netify_edge objects to ggplot objects. this method is called
#' automatically when using the + operator with a netify_edge object.
#'
#' @param object a netify_edge object created by \code{\link{netify_edge}}
#' @param plot a ggplot object to which the edge layer will be added
#' @param ... additional arguments passed by ggplot2 (used internally)
#'
#' @return a ggplot object with the edge layer added
#'
#' @method ggplot_add netify_edge
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
ggplot_add.netify_edge <- function(object, plot, ...) {
	plot$layers <- append(plot$layers, object)
	plot
}

#' extract nodes layer from netify plot components
#'
#' extracts the node (point) layer from a netify plot components object, allowing
#' for manual plot construction and customization. nodes represent actors in the
#' network and can have various aesthetic mappings like size, color, and shape.
#'
#' @param comp a netify_plot_components object returned from
#'   \code{plot(..., return_components = TRUE)}
#'
#' @return a custom object of class "netify_node" that can be added to a ggplot
#'   object using the + operator. the object contains the node layer with all
#'   its aesthetic mappings and data.
#'
#' @examples
#' \donttest{
#' # create a netify object
#' mat <- matrix(c(NA, 1, 0, 0, NA, 1, 1, 0, NA), 3, 3,
#'     dimnames = list(c("alice", "bob", "carol"),
#'                     c("alice", "bob", "carol")))
#' net <- new_netify(mat, symmetric = FALSE)
#'
#' # get plot components
#' comp <- plot(net, return_components = TRUE)
#'
#' # build custom plot with nodes
#' library(ggplot2)
#' ggplot() +
#'     netify_node(comp)
#' }
#'
#' @seealso \code{\link{plot.netify}}, \code{\link{netify_edge}},
#'   \code{\link{assemble_netify_plot}}
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
netify_node <- function(comp) {
	if (!inherits(comp, "netify_plot_components")) {
		cli::cli_abort("Input must be netify_plot_components from {.code plot(..., return_components = TRUE)}")
	}
	if (is.null(comp$points)) {
		cli::cli_abort("No nodes in this plot")
	}

	# create the layer
	layer <- ggplot2::layer(
		geom = comp$points$geom,
		data = comp$points$data,
		mapping = comp$points$mapping,
		stat = comp$points$stat,
		position = comp$points$position,
		params = comp$points$params,
		inherit.aes = comp$points$inherit.aes,
		show.legend = comp$points$show.legend
	)

	# return it wrapped in a list with a custom class
	structure(list(layer), class = c("netify_node", "list"))
}

#' add netify_node to ggplot
#'
#' s3 method to add netify_node objects to ggplot objects. this method is called
#' automatically when using the + operator with a netify_node object.
#'
#' @param object a netify_node object created by \code{\link{netify_node}}
#' @param plot a ggplot object to which the node layer will be added
#' @param ... additional arguments passed by ggplot2 (used internally)
#'
#' @return a ggplot object with the node layer added
#'
#' @method ggplot_add netify_node
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
ggplot_add.netify_node <- function(object, plot, ...) {
	plot$layers <- append(plot$layers, object)
	plot
}

#' extract text layer from netify plot components
#'
#' extracts the text label layer from a netify plot components object. text labels
#' display actor names or other text annotations directly on the plot without
#' background boxes.
#'
#' @param comp a netify_plot_components object returned from
#'   \code{plot(..., return_components = TRUE)}
#'
#' @return a custom object of class "netify_text" that can be added to a ggplot
#'   object using the + operator. the object contains the text layer with all
#'   its aesthetic mappings and data.
#'
#' @examples
#' \donttest{
#' # create a netify object
#' mat <- matrix(c(NA, 1, 0, 0, NA, 1, 1, 0, NA), 3, 3,
#'     dimnames = list(c("alice", "bob", "carol"),
#'                     c("alice", "bob", "carol")))
#' net <- new_netify(mat, symmetric = FALSE)
#'
#' # get plot components with text labels
#' comp <- plot(net, add_text = TRUE, return_components = TRUE)
#'
#' # build custom plot with text
#' library(ggplot2)
#' ggplot() +
#'     netify_text(comp)
#' }
#'
#' @seealso \code{\link{plot.netify}}, \code{\link{netify_label}},
#'   \code{\link{assemble_netify_plot}}
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
netify_text <- function(comp) {
	if (!inherits(comp, "netify_plot_components")) {
		cli::cli_abort("Input must be netify_plot_components from {.code plot(..., return_components = TRUE)}")
	}
	if (is.null(comp$text)) {
		cli::cli_abort("No text in this plot")
	}

	layer <- ggplot2::layer(
		geom = comp$text$geom,
		data = comp$text$data,
		mapping = comp$text$mapping,
		stat = comp$text$stat,
		position = comp$text$position,
		params = comp$text$params,
		inherit.aes = comp$text$inherit.aes,
		show.legend = comp$text$show.legend
	)

	structure(list(layer), class = c("netify_text", "list"))
}

#' add netify_text to ggplot
#'
#' s3 method to add netify_text objects to ggplot objects. this method is called
#' automatically when using the + operator with a netify_text object.
#'
#' @param object a netify_text object created by \code{\link{netify_text}}
#' @param plot a ggplot object to which the text layer will be added
#' @param ... additional arguments passed by ggplot2 (used internally)
#'
#' @return a ggplot object with the text layer added
#'
#' @method ggplot_add netify_text
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
ggplot_add.netify_text <- function(object, plot, ...) {
	plot$layers <- append(plot$layers, object)
	plot
}

#' extract label layer from netify plot components
#'
#' extracts the label layer from a netify plot components object. labels display
#' actor names or other text annotations with background boxes, making them more
#' visible against complex network backgrounds.
#'
#' @param comp a netify_plot_components object returned from
#'   \code{plot(..., return_components = TRUE)}
#'
#' @return a custom object of class "netify_label" that can be added to a ggplot
#'   object using the + operator. the object contains the label layer with all
#'   its aesthetic mappings and data.
#'
#' @examples
#' \donttest{
#' # create a netify object
#' mat <- matrix(c(NA, 1, 0, 0, NA, 1, 1, 0, NA), 3, 3,
#'     dimnames = list(c("alice", "bob", "carol"),
#'                     c("alice", "bob", "carol")))
#' net <- new_netify(mat, symmetric = FALSE)
#'
#' # get plot components with labels
#' comp <- plot(net, add_label = TRUE, return_components = TRUE)
#'
#' # build custom plot with labels
#' library(ggplot2)
#' ggplot() +
#'     netify_label(comp)
#' }
#'
#' @seealso \code{\link{plot.netify}}, \code{\link{netify_text}},
#'   \code{\link{assemble_netify_plot}}
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
netify_label <- function(comp) {
	if (!inherits(comp, "netify_plot_components")) {
		cli::cli_abort("Input must be netify_plot_components from {.code plot(..., return_components = TRUE)}")
	}
	if (is.null(comp$label)) {
		cli::cli_abort("No labels in this plot")
	}

	layer <- ggplot2::layer(
		geom = comp$label$geom,
		data = comp$label$data,
		mapping = comp$label$mapping,
		stat = comp$label$stat,
		position = comp$label$position,
		params = comp$label$params,
		inherit.aes = comp$label$inherit.aes,
		show.legend = comp$label$show.legend
	)

	structure(list(layer), class = c("netify_label", "list"))
}

#' add netify_label to ggplot
#'
#' s3 method to add netify_label objects to ggplot objects. this method is called
#' automatically when using the + operator with a netify_label object.
#'
#' @param object a netify_label object created by \code{\link{netify_label}}
#' @param plot a ggplot object to which the label layer will be added
#' @param ... additional arguments passed by ggplot2 (used internally)
#'
#' @return a ggplot object with the label layer added
#'
#' @method ggplot_add netify_label
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
ggplot_add.netify_label <- function(object, plot, ...) {
	plot$layers <- append(plot$layers, object)
	plot
}

#' extract text_repel layer from netify plot components
#'
#' extracts the text_repel layer from a netify plot components object. text repel labels
#' display actor names with automatic repositioning to avoid overlaps, making them more
#' readable in dense networks.
#'
#' @param comp a netify_plot_components object returned from
#'   \code{plot(..., return_components = TRUE)}
#'
#' @return a custom object of class "netify_text_repel" that can be added to a ggplot
#'   object using the + operator. the object contains the text_repel layer with all
#'   its aesthetic mappings and data.
#'
#' @examples
#' \donttest{
#' # create a netify object
#' mat <- matrix(c(NA, 1, 0, 0, NA, 1, 1, 0, NA), 3, 3,
#'     dimnames = list(c("alice", "bob", "carol"),
#'                     c("alice", "bob", "carol")))
#' net <- new_netify(mat, symmetric = FALSE)
#'
#' # get plot components with text_repel
#' comp <- plot(net, add_text_repel = TRUE, return_components = TRUE)
#'
#' # build custom plot with repelled text
#' library(ggplot2)
#' ggplot() +
#'     netify_text_repel(comp)
#' }
#'
#' @seealso \code{\link{plot.netify}}, \code{\link{netify_text}},
#'   \code{\link{assemble_netify_plot}}
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
netify_text_repel <- function(comp) {
	if (!inherits(comp, "netify_plot_components")) {
		cli::cli_abort("Input must be netify_plot_components from {.code plot(..., return_components = TRUE)}")
	}
	if (is.null(comp$text_repel)) {
		cli::cli_abort("No text_repel in this plot")
	}

	layer <- ggplot2::layer(
		geom = comp$text_repel$geom,
		data = comp$text_repel$data,
		mapping = comp$text_repel$mapping,
		stat = comp$text_repel$stat,
		position = comp$text_repel$position,
		params = comp$text_repel$params,
		inherit.aes = comp$text_repel$inherit.aes,
		show.legend = comp$text_repel$show.legend
	)

	structure(list(layer), class = c("netify_text_repel", "list"))
}

#' add netify_text_repel to ggplot
#'
#' s3 method to add netify_text_repel objects to ggplot objects. this method is called
#' automatically when using the + operator with a netify_text_repel object.
#'
#' @param object a netify_text_repel object created by \code{\link{netify_text_repel}}
#' @param plot a ggplot object to which the text_repel layer will be added
#' @param ... additional arguments passed by ggplot2 (used internally)
#'
#' @return a ggplot object with the text_repel layer added
#'
#' @method ggplot_add netify_text_repel
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
ggplot_add.netify_text_repel <- function(object, plot, ...) {
	plot$layers <- append(plot$layers, object)
	plot
}

#' extract label_repel layer from netify plot components
#'
#' extracts the label_repel layer from a netify plot components object. label repel
#' annotations display actor names with background boxes and automatic repositioning
#' to avoid overlaps, providing optimal readability in dense networks.
#'
#' @param comp a netify_plot_components object returned from
#'   \code{plot(..., return_components = TRUE)}
#'
#' @return a custom object of class "netify_label_repel" that can be added to a ggplot
#'   object using the + operator. the object contains the label_repel layer with all
#'   its aesthetic mappings and data.
#'
#' @examples
#' \donttest{
#' # create a netify object
#' mat <- matrix(c(NA, 1, 0, 0, NA, 1, 1, 0, NA), 3, 3,
#'     dimnames = list(c("alice", "bob", "carol"),
#'                     c("alice", "bob", "carol")))
#' net <- new_netify(mat, symmetric = FALSE)
#'
#' # get plot components with label_repel
#' comp <- plot(net, add_label_repel = TRUE, return_components = TRUE)
#'
#' # build custom plot with repelled labels
#' library(ggplot2)
#' ggplot() +
#'     netify_label_repel(comp)
#' }
#'
#' @seealso \code{\link{plot.netify}}, \code{\link{netify_label}},
#'   \code{\link{assemble_netify_plot}}
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
netify_label_repel <- function(comp) {
	if (!inherits(comp, "netify_plot_components")) {
		cli::cli_abort("Input must be netify_plot_components from {.code plot(..., return_components = TRUE)}")
	}
	if (is.null(comp$label_repel)) {
		cli::cli_abort("No label_repel in this plot")
	}

	layer <- ggplot2::layer(
		geom = comp$label_repel$geom,
		data = comp$label_repel$data,
		mapping = comp$label_repel$mapping,
		stat = comp$label_repel$stat,
		position = comp$label_repel$position,
		params = comp$label_repel$params,
		inherit.aes = comp$label_repel$inherit.aes,
		show.legend = comp$label_repel$show.legend
	)

	structure(list(layer), class = c("netify_label_repel", "list"))
}

#' add netify_label_repel to ggplot
#'
#' s3 method to add netify_label_repel objects to ggplot objects. this method is called
#' automatically when using the + operator with a netify_label_repel object.
#'
#' @param object a netify_label_repel object created by \code{\link{netify_label_repel}}
#' @param plot a ggplot object to which the label_repel layer will be added
#' @param ... additional arguments passed by ggplot2 (used internally)
#'
#' @return a ggplot object with the label_repel layer added
#'
#' @method ggplot_add netify_label_repel
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
ggplot_add.netify_label_repel <- function(object, plot, ...) {
	plot$layers <- append(plot$layers, object)
	plot
}

#' reset aesthetic scales in ggplot
#'
#' creates a scale reset object that can be added to a ggplot to reset color,
#' fill, alpha, and size scales. this is necessary when using multiple layers
#' with different aesthetic mappings (e.g., different colors for edges vs nodes).
#'
#' @return a custom object of class "netify_scale_reset" that can be added to
#'   a ggplot object using the + operator
#'
#' @details
#' this function addresses the limitation in ggplot2 where each aesthetic can
#' only have one scale. by resetting scales between layers, you can have
#' different color mappings for edges and nodes, for example.
#'
#' @examples
#' \donttest{
#' # create a plot with different colors for edges and nodes
#' mat <- matrix(c(NA, 1, 0, 0, NA, 1, 1, 0, NA), 3, 3,
#'     dimnames = list(c("alice", "bob", "carol"),
#'                     c("alice", "bob", "carol")))
#' net <- new_netify(mat, symmetric = FALSE)
#' comp <- plot(net, return_components = TRUE)
#'
#' ggplot2::ggplot() +
#'     netify_edge(comp) +
#'     ggplot2::scale_color_manual(values = c("gray", "red")) +
#'     reset_scales() + # reset before adding nodes
#'     netify_node(comp) +
#'     ggplot2::scale_color_viridis_c()
#' }
#'
#' @seealso \code{\link[ggnewscale]{new_scale_color}}
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
reset_scales <- function() {
	structure(list(), class = c("netify_scale_reset", "list"))
}

#' add scale resets to ggplot
#'
#' s3 method to add scale reset objects to ggplot objects. this method resets
#' color, fill, alpha, and size scales using the ggnewscale package.
#'
#' @param object a netify_scale_reset object created by \code{\link{reset_scales}}
#' @param plot a ggplot object to which scale resets will be added
#' @param ... additional arguments passed by ggplot2 (used internally)
#'
#' @return a ggplot object with scale resets applied
#'
#' @method ggplot_add netify_scale_reset
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
ggplot_add.netify_scale_reset <- function(object, plot, ...) {
	plot <- plot + ggnewscale::new_scale_color()
	plot <- plot + ggnewscale::new_scale_fill()
	plot <- plot + ggnewscale::new_scale("alpha")
	plot <- plot + ggnewscale::new_scale("size")
	plot
}

#' assemble netify plot from components
#'
#' assembles a complete network plot from netify plot components. this function
#' automatically adds all available layers (edges, nodes, text, labels, and their
#' repel versions) in the correct order with appropriate scale resets between layers.
#'
#' @param comp a netify_plot_components object returned from
#'   \code{plot(..., return_components = TRUE)}
#'
#' @return a complete ggplot object ready for display or further customization
#'
#' @details
#' this function provides a convenient way to reassemble a plot from its
#' components after extracting them with \code{return_components = TRUE}.
#' it automatically:
#' \itemize{
#'   \item adds layers in the correct order (edges, nodes, text/text_repel, labels/label_repel)
#'   \item inserts scale resets between layers when necessary
#'   \item handles both standard and repel versions of text and label layers
#'   \item includes facets and themes if present
#' }
#'
#' @examples
#' \donttest{
#' # create a netify object
#' mat <- matrix(c(NA, 1, 0, 0, NA, 1, 1, 0, NA), 3, 3,
#'     dimnames = list(c("alice", "bob", "carol"),
#'                     c("alice", "bob", "carol")))
#' net <- new_netify(mat, symmetric = FALSE)
#'
#' # get plot components
#' comp <- plot(net, return_components = TRUE)
#'
#' # reassemble the plot
#' p <- assemble_netify_plot(comp)
#' print(p)
#' }
#'
#' @seealso \code{\link{plot.netify}}, \code{\link{netify_edge}},
#'   \code{\link{netify_node}}
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
assemble_netify_plot <- function(comp) {
	if (!inherits(comp, "netify_plot_components")) {
		cli::cli_abort("Input must be netify_plot_components from {.code plot(..., return_components = TRUE)}")
	}

	p <- comp$base

	# add edges
	if (!is.null(comp$edges)) {
		p <- p + netify_edge(comp)
	}

	# auto-add scale reset if both edges and nodes have mappings
	if (!is.null(comp$edges) && !is.null(comp$points)) {
		if (!is.null(comp$edge_scales$color) || !is.null(comp$point_scales$color) ||
			!is.null(comp$edge_scales$fill) || !is.null(comp$point_scales$fill)) {
			p <- p + reset_scales()
		}
	}

	# add nodes
	if (!is.null(comp$points)) {
		p <- p + netify_node(comp)
	}

	# add text if present
	if (!is.null(comp$text)) {
		if (!is.null(comp$points)) {
			p <- p + reset_scales()
		}
		p <- p + netify_text(comp)
	}

	# add text_repel if present
	if (!is.null(comp$text_repel)) {
		if (!is.null(comp$points)) {
			p <- p + reset_scales()
		}
		p <- p + netify_text_repel(comp)
	}

	# add labels if present
	if (!is.null(comp$label)) {
		if (!is.null(comp$text) || !is.null(comp$text_repel) || !is.null(comp$points)) {
			p <- p + reset_scales()
		}
		p <- p + netify_label(comp)
	}

	# add label_repel if present
	if (!is.null(comp$label_repel)) {
		if (!is.null(comp$label) || !is.null(comp$text) || !is.null(comp$text_repel) || !is.null(comp$points)) {
			p <- p + reset_scales()
		}
		p <- p + netify_label_repel(comp)
	}

	# add facets
	if (!is.null(comp$facets)) {
		p <- p + comp$facets
	}

	# add theme
	if (!is.null(comp$theme)) {
		p <- p + comp$theme
	}

	return(p)
}

#' print netify plot components
#'
#' prints a summary of the components available in a netify_plot_components object.
#' this helps users understand what layers and elements are available for manual
#' plot construction.
#'
#' @param x a netify_plot_components object returned from
#'   \code{plot(..., return_components = TRUE)}
#' @param ... additional arguments (currently unused)
#'
#' @return invisibly returns the input object
#'
#' @examples
#' \donttest{
#' # create plot components
#' mat <- matrix(c(NA, 1, 0, 0, NA, 1, 1, 0, NA), 3, 3,
#'     dimnames = list(c("alice", "bob", "carol"),
#'                     c("alice", "bob", "carol")))
#' net <- new_netify(mat, symmetric = FALSE)
#' comp <- plot(net, return_components = TRUE)
#'
#' # print summary
#' print(comp)
#' }
#'
#' @method print netify_plot_components
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
print.netify_plot_components <- function(x, ...) {
	cli::cli_h2("Netify plot components")
	cli::cli_ul(c("Base plot: ggplot object"))
	
	components <- c()
	if (!is.null(x$edges)) components <- c(components, "Edges: geom_segment/geom_curve layer")
	if (!is.null(x$points)) components <- c(components, "Points: geom_point layer")
	if (!is.null(x$text)) components <- c(components, "Text: geom_text layer")
	if (!is.null(x$text_repel)) components <- c(components, "Text Repel: geom_text_repel layer")
	if (!is.null(x$label)) components <- c(components, "Labels: geom_label layer")
	if (!is.null(x$label_repel)) components <- c(components, "Label Repel: geom_label_repel layer")
	if (!is.null(x$facets)) components <- c(components, "Facets: facet_wrap layer")
	if (!is.null(x$theme)) components <- c(components, "Theme: theme_netify")
	
	if (length(components) > 0) {
		cli::cli_ul(components)
	}
	
	cli::cli_inform(c(
		"i" = "Use {.fn assemble_netify_plot} to build or construct manually with {.fn netify_edge}, {.fn netify_node}, etc."
	))
	invisible(x)
}

#' set scale labels for netify plots
#'
#' provides a convenient way to set labels for aesthetic scales in netify plots.
#' this function simplifies the process of labeling scales that may be spread
#' across different layers (edges, nodes, text, labels).
#'
#' @param ... named arguments where the name is the aesthetic_component
#'   (e.g., "edge_alpha", "node_size", "edge_color") and the value is the
#'   label text to display in the legend
#'
#' @return a custom object of class "netify_labels" that can be added to a
#'   netify plot using the + operator
#'
#' @details
#' this function provides a user-friendly interface for setting scale labels
#' without needing to understand the complexity of ggnewscale. the naming
#' convention is:
#' \itemize{
#'   \item \code{edge_*} for edge aesthetics (e.g., edge_color, edge_alpha)
#'   \item \code{node_*} or \code{point_*} for node aesthetics (both work)
#'   \item \code{text_*} for text label aesthetics
#'   \item \code{label_*} for boxed label aesthetics
#' }
#'
#' @note this function only works with plots created using netify's plot method.
#' it will issue a warning if used with other ggplot objects.
#'
#' @examples
#' \donttest{
#' # set labels for different scales
#' mat <- matrix(c(NA, 2, 0, 0, NA, 1, 3, 0, NA), 3, 3,
#'     dimnames = list(c("alice", "bob", "carol"),
#'                     c("alice", "bob", "carol")))
#' net <- new_netify(mat, symmetric = FALSE, weight = "strength")
#'
#' plot(net, edge_alpha_var = "strength") +
#'     netify_scale_labels(
#'         edge_alpha = "connection strength",
#'         node_size = "node degree" # node_* is converted to point_*
#'     )
#' }
#'
#' @seealso \code{\link{plot.netify}}
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
netify_scale_labels <- function(...) {
	labels <- list(...)

	# convert any node_* to point_* for consistency with internal tracking
	names(labels) <- gsub("^node_", "point_", names(labels))

	structure(labels, class = c("netify_labels", "list"))
}

#' add netify scale labels to ggplot
#'
#' s3 method to add netify_labels objects to ggplot objects. this method updates
#' the labels of existing scales based on the specifications in the netify_labels
#' object.
#'
#' @param object a netify_labels object created by \code{\link{netify_scale_labels}}
#' @param plot a ggplot object to which the labels will be added
#' @param ... additional arguments passed by ggplot2 (used internally)
#'
#' @return a ggplot object with updated scale labels
#'
#' @method ggplot_add netify_labels
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
ggplot_add.netify_labels <- function(object, plot, ...) {
	# extract the netify plot data if available
	if (!is.null(plot$plot_env$last_netify_components)) {
		components <- plot$plot_env$last_netify_components
	} else {
		cli::cli_warn("netify_scale_labels only works with netify plots")
		return(plot)
	}

	# apply labels based on what scales exist
	for (aesthetic in names(object)) {
		parts <- strsplit(aesthetic, "_")[[1]]
		component <- parts[1]
		aes_type <- paste(parts[-1], collapse = "_")

		# determine which scale to modify
		if (component == "edge") {
			if (!is.null(components$edge_scales[[aes_type]])) {
				# find the right scale layer and update it
				plot <- update_scale_label(plot, aes_type, object[[aesthetic]], "edge")
			}
		} else if (component %in% c("node", "point")) {
			if (!is.null(components$point_scales[[aes_type]])) {
				plot <- update_scale_label(plot, aes_type, object[[aesthetic]], "node")
			}
		} else if (component == "text") {
			if (!is.null(components$text) && aes_type %in% names(components$text$mapping)) {
				plot <- update_scale_label(plot, aes_type, object[[aesthetic]], "text")
			}
		} else if (component == "label") {
			if (!is.null(components$label) && aes_type %in% names(components$label$mapping)) {
				plot <- update_scale_label(plot, aes_type, object[[aesthetic]], "label")
			}
		}
	}

	return(plot)
}

#' update scale labels in a ggplot
#'
#' internal helper function that updates the label of a specific aesthetic scale
#' in a ggplot object. this function adds the appropriate labs() call based on
#' the aesthetic type.
#'
#' @param plot the ggplot object to modify
#' @param aesthetic the aesthetic to update (e.g., "alpha", "color", "size")
#' @param label the new label text for the scale
#' @param component_type the type of component (e.g., "edge", "node", "text", "label")
#'
#' @return the updated ggplot object with the new scale label
#'
#' @keywords internal
#' @noRd
update_scale_label <- function(plot, aesthetic, label, component_type) {
	# build the labs() call for any supported aesthetic
	labs_list <- list()
	labs_list[[aesthetic]] <- label

	# apply the labs to the plot
	plot + do.call(ggplot2::labs, labs_list)
}
