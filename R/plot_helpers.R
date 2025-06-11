#' Extract edges layer from netify plot components
#'
#' @param comp A netify_plot_components object
#' @return A custom object that can be added to ggplot
#' @importFrom ggplot2 ggplot_add
#' @export
netify_edge <- function(comp) {
    if (!inherits(comp, "netify_plot_components")) {
        stop("Input must be netify_plot_components from plot(..., return_components = TRUE)")
    }
    if (is.null(comp$edges)) {
        stop("No edges in this plot")
    }
    
    # Create the layer
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
    
    # Return it wrapped in a list with a custom class
    structure(list(layer), class = c("netify_edge", "list"))
}

#' Method to add netify_edge to ggplot
#' @method ggplot_add netify_edge
#' @export
ggplot_add.netify_edge <- function(object, plot, object_name) {
    plot$layers <- append(plot$layers, object)
    plot
}

#' Extract nodes layer from netify plot components
#'
#' @param comp A netify_plot_components object
#' @return A custom object that can be added to ggplot
#' @export
netify_node <- function(comp) {
    if (!inherits(comp, "netify_plot_components")) {
        stop("Input must be netify_plot_components from plot(..., return_components = TRUE)")
    }
    if (is.null(comp$points)) {
        stop("No nodes in this plot")
    }
    
    # Create the layer
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
    
    # Return it wrapped in a list with a custom class
    structure(list(layer), class = c("netify_node", "list"))
}

#' Method to add netify_node to ggplot
#' @method ggplot_add netify_node
#' @export
ggplot_add.netify_node <- function(object, plot, object_name) {
    plot$layers <- append(plot$layers, object)
    plot
}

#' Extract text layer from netify plot components
#'
#' @param comp A netify_plot_components object
#' @return A custom object that can be added to ggplot
#' @export
netify_text <- function(comp) {
    if (!inherits(comp, "netify_plot_components")) {
        stop("Input must be netify_plot_components from plot(..., return_components = TRUE)")
    }
    if (is.null(comp$text)) {
        stop("No text in this plot")
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

#' Method to add netify_text to ggplot
#' @method ggplot_add netify_text
#' @export
ggplot_add.netify_text <- function(object, plot, object_name) {
    plot$layers <- append(plot$layers, object)
    plot
}

#' Extract label layer from netify plot components
#'
#' @param comp A netify_plot_components object
#' @return A custom object that can be added to ggplot
#' @export
netify_label <- function(comp) {
    if (!inherits(comp, "netify_plot_components")) {
        stop("Input must be netify_plot_components from plot(..., return_components = TRUE)")
    }
    if (is.null(comp$label)) {
        stop("No labels in this plot")
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

#' Method to add netify_label to ggplot
#' @method ggplot_add netify_label
#' @export
ggplot_add.netify_label <- function(object, plot, object_name) {
    plot$layers <- append(plot$layers, object)
    plot
}

#' Reset scales helper
#'
#' @return A custom object that resets scales
#' @export
reset_scales <- function() {
    structure(list(), class = c("netify_scale_reset", "list"))
}

#' Method to add scale resets to ggplot
#' @method ggplot_add netify_scale_reset
#' @export
ggplot_add.netify_scale_reset <- function(object, plot, object_name) {
    plot <- plot + ggnewscale::new_scale_color()
    plot <- plot + ggnewscale::new_scale_fill()
    plot <- plot + ggnewscale::new_scale('alpha')
    plot <- plot + ggnewscale::new_scale('size')
    plot
}

#' Assemble netify plot from components
#'
#' @param comp A netify_plot_components object
#' @return A ggplot object
#' @export
assemble_netify_plot <- function(comp) {
    if (!inherits(comp, "netify_plot_components")) {
        stop("Input must be netify_plot_components from plot(..., return_components = TRUE)")
    }
    
    p <- comp$base
    
    # Add edges
    if (!is.null(comp$edges)) {
        p <- p + edges(comp)
    }
    
    # Auto-add scale reset if both edges and nodes have mappings
    if (!is.null(comp$edges) && !is.null(comp$points)) {
        if (!is.null(comp$edge_scales$color) || !is.null(comp$point_scales$color) ||
            !is.null(comp$edge_scales$fill) || !is.null(comp$point_scales$fill)) {
            p <- p + reset_scales()
        }
    }
    
    # Add nodes
    if (!is.null(comp$points)) {
        p <- p + nodes(comp)
    }
    
    # Add text if present
    if (!is.null(comp$text)) {
        if (!is.null(comp$points)) {
            p <- p + reset_scales()
        }
        p <- p + text_layer(comp)
    }
    
    # Add labels if present
    if (!is.null(comp$label)) {
        if (!is.null(comp$text) || !is.null(comp$points)) {
            p <- p + reset_scales()
        }
        p <- p + label_layer(comp)
    }
    
    # Add facets
    if (!is.null(comp$facets)) {
        p <- p + comp$facets
    }
    
    # Add theme
    if (!is.null(comp$theme)) {
        p <- p + comp$theme
    }
    
    return(p)
}

#' Print method for netify_plot_components
#' @export
print.netify_plot_components <- function(x, ...) {
    cat("Netify plot components:\n")
    cat("  Base plot: ggplot object\n")
    if (!is.null(x$edges)) cat("  Edges: geom_segment/geom_curve layer\n")
    if (!is.null(x$points)) cat("  Points: geom_point layer\n")
    if (!is.null(x$text)) cat("  Text: geom_text layer\n")
    if (!is.null(x$label)) cat("  Labels: geom_label layer\n")
    if (!is.null(x$facets)) cat("  Facets: facet_wrap layer\n")
    if (!is.null(x$theme)) cat("  Theme: theme_netify\n")
    cat("\nUse assemble_netify_plot() to build or construct manually with edges(), nodes(), etc.\n")
}