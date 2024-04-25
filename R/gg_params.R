#' Prepare graphical parameters for ggplot2 based on specified plot arguments.
#'
#' This function organizes and formats static and variable-dependent aesthetic parameters 
#' for nodes, text labels, and edges to be used in ggplot2 visualizations. It adjusts parameters
#' based on the existence and non-null status of the corresponding entries in `plot_args`.
#'
#' @param plot_args A list containing user-defined specifications for various graphical parameters
#'                  such as color, size, alpha, and others for different components of the plot.
#'
#' @return A list containing organized graphical parameters for nodes, text, labels, and edges.
#'         Each component has sub-lists for static parameters and variable parameters.
#' 
#' @author Cassy Dorff, Shahryar Minhas 
#'
#' @details
#' The function splits the graphical parameters into static and dynamic (variable-dependent) categories.
#' Static parameters are directly applied when the corresponding variable-dependent parameter is not set.
#' If a variable-dependent parameter is set, the static parameter is excluded to allow dynamic mapping in ggplot.
#'
#' Dynamic or variable-dependent parameters are set based on a mapping from data columns specified in `plot_args`.
#' The function checks for non-null entries in `plot_args` and assigns these to the appropriate aesthetic
#' if the entry exists. For example, if `plot_args$node_color_var` is not null, it will create a dynamic color 
#' mapping for nodes and remove any static color setting.
#'@export 

gg_params <- function(
    plot_args    
){

    # node params #####################

    # node static param list
    node_static_params = list(
        alpha = plot_args$node_alpha,
        color = plot_args$node_color,
        fill = plot_args$node_fill,
        shape = plot_args$node_shape,
        size = plot_args$node_size,
        stroke = plot_args$node_stroke )

    # Prepare a list to conditionally build the aes()
    node_aes_list <- list(x = ~x, y = ~y)

    # Add conditional aesthetics based on non-NULL entries
    if(!is.null(plot_args$node_alpha_var)){
        node_aes_list$alpha = formula(
            paste0('~', plot_args$node_alpha_var))
        node_static_params = node_static_params[
            -which(names(node_static_params)=='node_alpha')]
    }
    if(!is.null(plot_args$node_color_var)){
        node_aes_list$color = formula(
            paste0('~', plot_args$node_color_var))
        node_static_params = node_static_params[
            -which(names(node_static_params)=='node_color')]
    }
    if(!is.null(plot_args$node_fill_var)){
        node_aes_list$fill = formula(
            paste0('~', plot_args$node_fill_var))
        node_static_params = node_static_params[
            -which(names(node_static_params)=='node_fill')]
    }
    if(!is.null(plot_args$node_shape_var)){
        node_aes_list$shape = formula(
            paste0('~', plot_args$node_shape_var))
        node_static_params = node_static_params[
            -which(names(node_static_params)=='node_shape')]
    }
    if(!is.null(plot_args$node_size_var)){
        node_aes_list$size = formula(
            paste0('~', plot_args$node_size_var))
        node_static_params = node_static_params[
            -which(names(node_static_params)=='node_size')]
    }
    if(!is.null(plot_args$node_stroke_var)){
        node_aes_list$stroke = formula(
            paste0('~', plot_args$node_stroke_var))
        node_static_params = node_static_params[
            -which(names(node_static_params)=='node_stroke')]
    }
    ######################

    # text params #####################

    # text static param list
    text_static_params = list(
        check_overlap = plot_args$check_overlap,
        alpha = plot_args$text_alpha,
        color = plot_args$text_color,
        size = plot_args$text_size,
        family = plot_args$text_family,
        fontface = plot_args$text_fontface,
        angle = plot_args$text_angle )

    # Prepare a list to conditionally build the aes()
    text_aes_list <- list(
        x = ~x, y = ~y,
        label = ~name )

    # Add conditional aesthetics based on non-NULL entries
    if(!is.null(plot_args$text_alpha_var)){
        text_aes_list$alpha <- formula(
            paste0('~', plot_args$text_alpha_var))
        text_static_params = text_static_params[
            -which(names(text_static_params)=='text_alpha')]
    }
    if(!is.null(plot_args$text_color_var)){
        text_aes_list$color <- formula(
            paste0('~', plot_args$text_color_var))
        text_static_params = text_static_params[
            -which(names(text_static_params)=='text_color')]
    }
    if(!is.null(plot_args$text_size_var)){
        text_aes_list$size <- formula(
            paste0('~', plot_args$text_size_var))
        text_static_params = text_static_params[
            -which(names(text_static_params)=='text_size')]
    }
    ######################

    # label params #####################

    # label static param list
    label_static_params = list(
        check_overlap = plot_args$check_overlap,
        alpha = plot_args$label_alpha,
        color = plot_args$label_color,
        fill = plot_args$label_fill,
        size = plot_args$label_size,
        family = plot_args$label_family,
        fontface = plot_args$label_fontface,
        angle = plot_args$label_angle,
        hjust = plot_args$label_hjust,
        vjust = plot_args$label_vjust,
        lineheight = plot_args$label_lineheight )

    # Prepare a list to conditionally build the aes()
    label_aes_list <- list(
        x = ~x, y = ~y,
        label = ~name )

    # Add conditional aesthetics based on non-NULL entries
    if(!is.null(plot_args$label_alpha_var)){
        label_aes_list$alpha <- formula(
            paste0('~', plot_args$label_alpha_var) )
        label_static_params = label_static_params[
            -which(names(label_static_params)=='label_alpha')]
    }
    if(!is.null(plot_args$label_color_var)){
        label_aes_list$color <- formula(
            paste0('~', plot_args$label_color_var) )
        label_static_params = label_static_params[
            -which(names(label_static_params)=='label_color')]
    }
    if(!is.null(plot_args$label_fill_var)){
        label_aes_list$fill <- formula(
            paste0('~', plot_args$label_fill_var) )
        label_static_params = label_static_params[
            -which(names(label_static_params)=='label_fill')]
    }
    if(!is.null(plot_args$label_size_var)){
        label_aes_list$size <- formula(
            paste0('~', plot_args$label_size_var) )
        label_static_params = label_static_params[
            -which(names(label_static_params)=='label_size')]
    }
    ######################

    # edge seg/curve params #####################

    # edge static param list
    edge_static_params = list(
        color = plot_args$edge_color,
        linewidth = plot_args$edge_linewidth,
        linetype = plot_args$edge_linetype,
        alpha = plot_args$edge_alpha,
        arrow = plot_args$edge_arrow,
        arrow.fill = plot_args$edge_arrow_fill,
        lineend=plot_args$edge_lineend,
        linejoin=plot_args$edge_linejoin
    )

    # curve static param list
    curve_static_params = edge_static_params
    curve_static_params$curvature = plot_args$edge_curvature
    curve_static_params$angle = plot_args$edge_angle
    curve_static_params$ncp = plot_args$edge_ncp

    # Prepare a list to conditionally build the aes() for edges
    edge_aes_list <- list(
        x = ~x1, y = ~y1,
        xend = ~x2, yend = ~y2
        )

    # Add conditional aesthetics based on non-NULL entries
    if(!is.null(plot_args$edge_alpha_var)){
        edge_aes_list$alpha <- formula(
            paste0('~', plot_args$edge_alpha_var) )
        edge_static_params = edge_static_params[
            -which(names(edge_static_params)=='edge_alpha')]
    }
    if(!is.null(plot_args$edge_color_var)){
        edge_aes_list$color <- formula(
            paste0('~', plot_args$edge_color_var) )
        edge_static_params = edge_static_params[
            -which(names(edge_static_params)=='edge_color')]
    }
    if(!is.null(plot_args$edge_linetype_var)){
        edge_aes_list$linetype <- formula(
            paste0('~', plot_args$edge_linetype_var) )
        edge_static_params = edge_static_params[
            -which(names(edge_static_params)=='edge_linetype')]
    }
    if(!is.null(plot_args$edge_linewidth_var)){
        edge_aes_list$linewidth <- formula(
            paste0('~', plot_args$edge_linewidth_var) )
        edge_static_params = edge_static_params[
            -which(names(edge_static_params)=='edge_linewidth')]
    }
    ######################

    # org #####################
    out = list(
        node = list(
            static = node_static_params,
            var = node_aes_list
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