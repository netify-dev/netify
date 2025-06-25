#' Prepare graphical parameters for plot.netify
#'
#' `gg_params` organizes and formats aesthetic parameters for network visualization
#' components (nodes, text labels, and edges) into a structure suitable for ggplot2.
#' The function separates static (fixed) parameters from dynamic (data-mapped)
#' parameters, enabling flexible network visualization.
#'
#' @param plot_args A list containing visualization specifications. Can include both
#'   static parameters (fixed values) and variable parameters (mapped to data columns)
#'   for different network components. See Details for the full list of supported
#'   parameters.
#'
#' @return A list with five components, each containing static and variable aesthetic mappings:
#'   \itemize{
#'     \item \strong{point}: Node visualization parameters
#'       \itemize{
#'         \item \code{static}: Named list of fixed aesthetic values
#'         \item \code{var}: Named list of aesthetic mappings (as formulas)
#'       }
#'     \item \strong{text}: Text label parameters
#'       \itemize{
#'         \item \code{static}: Named list of fixed aesthetic values
#'         \item \code{var}: Named list of aesthetic mappings (as formulas)
#'       }
#'     \item \strong{label}: Box label parameters
#'       \itemize{
#'         \item \code{static}: Named list of fixed aesthetic values
#'         \item \code{var}: Named list of aesthetic mappings (as formulas)
#'       }
#'     \item \strong{edge}: Straight edge parameters
#'       \itemize{
#'         \item \code{static}: Named list of fixed aesthetic values
#'         \item \code{var}: Named list of aesthetic mappings (as formulas)
#'       }
#'     \item \strong{curve}: Curved edge parameters
#'       \itemize{
#'         \item \code{static}: Named list of fixed aesthetic values including curvature
#'         \item \code{var}: Named list of aesthetic mappings (as formulas)
#'       }
#'   }
#'
#' @details
#' The function processes aesthetic parameters for network visualization by:
#'
#' \strong{Parameter organization:}
#'
#' For each visual component (nodes, text, labels, edges), the function:
#' \enumerate{
#'   \item Collects all relevant static parameters from plot_args
#'   \item Checks for corresponding variable parameters (ending in "_var")
#'   \item When a variable parameter exists, removes the static parameter to allow
#'     dynamic mapping
#'   \item Creates formula objects for variable mappings compatible with ggplot2's aes()
#' }
#'
#' \strong{Supported parameters:}
#'
#' \emph{Node (point) parameters:}
#' \itemize{
#'   \item Static: \code{point_alpha}, \code{point_color}, \code{point_fill},
#'     \code{point_shape}, \code{point_size}, \code{point_stroke}
#'   \item Variable: \code{point_alpha_var}, \code{point_color_var}, \code{point_fill_var},
#'     \code{point_shape_var}, \code{point_size_var}, \code{point_stroke_var}
#' }
#'
#' \emph{Text parameters:}
#' \itemize{
#'   \item Static: \code{text_alpha}, \code{text_color}, \code{text_size},
#'     \code{text_family}, \code{text_fontface}, \code{text_angle}, \code{check_overlap}
#'   \item Variable: \code{text_alpha_var}, \code{text_color_var}, \code{text_size_var}
#' }
#'
#' \emph{Label parameters:}
#' \itemize{
#'   \item Static: \code{label_alpha}, \code{label_color}, \code{label_fill},
#'     \code{label_size}, \code{label_family}, \code{label_fontface}, \code{label_angle},
#'     \code{label_hjust}, \code{label_vjust}, \code{label_lineheight}, \code{check_overlap}
#'   \item Variable: \code{label_alpha_var}, \code{label_color_var}, \code{label_fill_var},
#'     \code{label_size_var}
#' }
#'
#' \emph{Edge parameters:}
#' \itemize{
#'   \item Static: \code{edge_color}, \code{edge_linewidth}, \code{edge_linetype},
#'     \code{edge_alpha}, \code{edge_arrow}, \code{edge_arrow_fill}, \code{edge_lineend},
#'     \code{edge_linejoin}
#'   \item Variable: \code{edge_alpha_var}, \code{edge_color_var}, \code{edge_linetype_var},
#'     \code{edge_linewidth_var}
#'   \item Curve-specific: \code{edge_curvature}, \code{edge_angle}, \code{edge_ncp}
#' }
#'
#' \strong{Static vs. variable parameters:}
#'
#' Static parameters apply a fixed aesthetic value to all elements. Variable parameters
#' map aesthetics to data columns, allowing visual properties to vary based on data
#' values. When both are specified for the same aesthetic, the variable parameter
#' takes precedence.
#'
#' @note
#' This function is primarily intended for internal use by netify plotting functions.
#'
#' Variable parameters must reference column names that exist in the data frames
#' used for plotting (typically the output from `decompose_netify()`).
#'
#' The returned formula objects use the tilde notation (e.g., \code{~column_name})
#' required by ggplot2's aes() function.
#'
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

gg_params <- function( plot_args) {
    
    # node params #####################

    # node static param list
    point_static_params <- list(
        alpha = plot_args$point_alpha,
        color = plot_args$point_color,
        fill = plot_args$point_fill,
        shape = plot_args$point_shape,
        size = plot_args$point_size,
        stroke = plot_args$point_stroke
    )

    # Prepare a list to conditionally build the aes()
    point_aes_list <- list(x = ~x, y = ~y)

    # Add conditional aesthetics based on non-NULL entries
    if (!is.null(plot_args$point_alpha_var)) {
        point_aes_list$alpha <- stats::formula(
            paste0("~", plot_args$point_alpha_var)
        )
        if ("alpha" %in% names(point_static_params)) {
            point_static_params$alpha <- NULL
        }
    }
    if (!is.null(plot_args$point_color_var)) {
        point_aes_list$color <- stats::formula(
            paste0("~", plot_args$point_color_var)
        )
        if ("color" %in% names(point_static_params)) {
            point_static_params$color <- NULL
        }
    }
    if (!is.null(plot_args$point_fill_var)) {
        point_aes_list$fill <- stats::formula(
            paste0("~", plot_args$point_fill_var)
        )
        if ("fill" %in% names(point_static_params)) {
            point_static_params$fill <- NULL
        }
    }
    if (!is.null(plot_args$point_shape_var)) {
        point_aes_list$shape <- stats::formula(
            paste0("~", plot_args$point_shape_var)
        )
        if ("shape" %in% names(point_static_params)) {
            point_static_params$shape <- NULL
        }
    }
    if (!is.null(plot_args$point_size_var)) {
        point_aes_list$size <- stats::formula(
            paste0("~", plot_args$point_size_var)
        )
        if ("size" %in% names(point_static_params)) {
            point_static_params$size <- NULL
        }
    }
    if (!is.null(plot_args$point_stroke_var)) {
        point_aes_list$stroke <- stats::formula(
            paste0("~", plot_args$point_stroke_var)
        )
        if ("stroke" %in% names(point_static_params)) {
            point_static_params$stroke <- NULL
        }
    }
    ######################

    # text params #####################

    # text static param list
    text_static_params <- list(
        check_overlap = plot_args$check_overlap,
        alpha = plot_args$text_alpha,
        color = plot_args$text_color,
        size = plot_args$text_size,
        family = plot_args$text_family,
        fontface = plot_args$text_fontface,
        angle = plot_args$text_angle
    )

    # Prepare a list to conditionally build the aes()
    text_aes_list <- list(
        x = ~x, y = ~y,
        label = ~name_text
    )

    # Add conditional aesthetics based on non-NULL entries
    if (!is.null(plot_args$text_alpha_var)) {
        text_aes_list$alpha <- stats::formula(
            paste0("~", plot_args$text_alpha_var)
        )
        if ("alpha" %in% names(text_static_params)) {
            text_static_params$alpha <- NULL
        }
    }
    if (!is.null(plot_args$text_color_var)) {
        text_aes_list$color <- stats::formula(
            paste0("~", plot_args$text_color_var)
        )
        if ("color" %in% names(text_static_params)) {
            text_static_params$color <- NULL
        }
    }
    if (!is.null(plot_args$text_size_var)) {
        text_aes_list$size <- stats::formula(
            paste0("~", plot_args$text_size_var)
        )
        if ("size" %in% names(text_static_params)) {
            text_static_params$size <- NULL
        }
    }
    ######################

    # label params #####################

    # label static param list
    label_static_params <- list(
        alpha = plot_args$label_alpha,
        color = plot_args$label_color,
        fill = plot_args$label_fill,
        size = plot_args$label_size,
        family = plot_args$label_family,
        fontface = plot_args$label_fontface,
        angle = plot_args$label_angle,
        hjust = plot_args$label_hjust,
        vjust = plot_args$label_vjust,
        lineheight = plot_args$label_lineheight
    )

    # Prepare a list to conditionally build the aes()
    label_aes_list <- list(
        x = ~x, y = ~y,
        label = ~name_label
    )

    # Add conditional aesthetics based on non-NULL entries
    if (!is.null(plot_args$label_alpha_var)) {
        label_aes_list$alpha <- stats::formula(
            paste0("~", plot_args$label_alpha_var)
        )
        if ("alpha" %in% names(label_static_params)) {
            label_static_params$alpha <- NULL
        }
    }
    if (!is.null(plot_args$label_color_var)) {
        label_aes_list$color <- stats::formula(
            paste0("~", plot_args$label_color_var)
        )
        if ("color" %in% names(label_static_params)) {
            label_static_params$color <- NULL
        }
    }
    if (!is.null(plot_args$label_fill_var)) {
        label_aes_list$fill <- stats::formula(
            paste0("~", plot_args$label_fill_var)
        )
        if ("fill" %in% names(label_static_params)) {
            label_static_params$fill <- NULL
        }
    }
    if (!is.null(plot_args$label_size_var)) {
        label_aes_list$size <- stats::formula(
            paste0("~", plot_args$label_size_var)
        )
        if ("size" %in% names(label_static_params)) {
            label_static_params$size <- NULL
        }
    }
    ######################

    # edge seg/curve params #####################

    # edge static param list
    edge_static_params <- list(
        color = plot_args$edge_color,
        linewidth = plot_args$edge_linewidth,
        linetype = plot_args$edge_linetype,
        alpha = plot_args$edge_alpha,
        arrow = plot_args$edge_arrow,
        arrow.fill = plot_args$edge_arrow_fill,
        lineend = plot_args$edge_lineend,
        linejoin = plot_args$edge_linejoin
    )

    # curve static param list
    curve_static_params <- edge_static_params
    curve_static_params$curvature <- plot_args$edge_curvature
    curve_static_params$angle <- plot_args$edge_angle
    curve_static_params$ncp <- plot_args$edge_ncp
    curve_static_params$linejoin <- NULL

    # Prepare a list to conditionally build the aes() for edges
    edge_aes_list <- list(
        x = ~x1, y = ~y1,
        xend = ~x2, yend = ~y2
    )

    # Add conditional aesthetics based on non-NULL entries
    if (!is.null(plot_args$edge_alpha_var)) {
        edge_aes_list$alpha <- stats::formula(
            paste0("~", plot_args$edge_alpha_var)
        )
        if ("alpha" %in% names(edge_static_params)) {
            edge_static_params$alpha <- NULL
        }
        if ("alpha" %in% names(curve_static_params)) {
            curve_static_params$alpha <- NULL
        }
    }
    if (!is.null(plot_args$edge_color_var)) {
        edge_aes_list$color <- stats::formula(
            paste0("~", plot_args$edge_color_var)
        )
        if ("color" %in% names(edge_static_params)) {
            edge_static_params$color <- NULL
        }
        if ("color" %in% names(curve_static_params)) {
            curve_static_params$color <- NULL
        }
    }
    if (!is.null(plot_args$edge_linetype_var)) {
        edge_aes_list$linetype <- stats::formula(
            paste0("~", plot_args$edge_linetype_var)
        )
        if ("linetype" %in% names(edge_static_params)) {
            edge_static_params$linetype <- NULL
        }
        if ("linetype" %in% names(curve_static_params)) {
            curve_static_params$linetype <- NULL
        }
    }
    if (!is.null(plot_args$edge_linewidth_var)) {
        edge_aes_list$linewidth <- stats::formula(
            paste0("~", plot_args$edge_linewidth_var)
        )
        if ("linewidth" %in% names(edge_static_params)) {
            edge_static_params$linewidth <- NULL
        }
        if ("linewidth" %in% names(curve_static_params)) {
            curve_static_params$linewidth <- NULL
        }
    }
    ######################

    # org #####################
    out <- list(
        point = list(
            static = point_static_params,
            var = point_aes_list
        ),
        text = list(
            static = text_static_params,
            var = text_aes_list
        ),
        label = list(
            static = label_static_params,
            var = label_aes_list
        ),
        edge = list(
            static = edge_static_params,
            var = edge_aes_list
        ),
        curve = list(
            static = curve_static_params,
            var = edge_aes_list
        )
    )

    #
    return(out)
    ######################
}
