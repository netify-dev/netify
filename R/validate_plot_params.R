#' Validate plot parameters and warn about common mistakes
#'
#' This function checks the parameter names passed through ... in plot.netify
#' and warns users about potential typos or deprecated parameter names.
#'
#' @param plot_args List of all plot arguments
#' @param ... Additional arguments passed to plot.netify
#'
#' @return NULL (function is called for side effects - warnings)
#'
#' @importFrom utils adist
#' @keywords internal
validate_plot_params <- function(plot_args, ...) {
    # Get the names of parameters passed in ...
    dot_args <- list(...)
    dot_names <- names(dot_args)
    
    # Skip if no named arguments
    if (is.null(dot_names) || length(dot_names) == 0) {
        return(invisible(NULL))
    }
    
    # Define known valid parameters for plot.netify
    valid_params <- c(
        # Layout parameters
        "layout", "point_layout", "static_actor_positions", "which_static", "seed",
        "min_radius", "max_radius",
        # Display control
        "add_edges", "add_points", "add_text", "add_text_repel", "add_label", 
        "add_label_repel", "remove_isolates", "curve_edges", "use_theme_netify",
        "facet_type", "facet_ncol", "rescale_edge_weights", "check_overlap",
        "return_components", "style", "palette", "xlim", "ylim",
        # Filter parameters
        "node_filter", "edge_filter", "time_filter",
        # Edge aesthetics - fixed
        "edge_alpha", "edge_arrow", "edge_color", "edge_linewidth", "edge_linetype",
        "edge_angle", "edge_curvature", "edge_ncp", "edge_lineend", "edge_linejoin",
        "adjust_arrow_endpoints", "edge_arrow_gap", "edge_arrow_size_scale", "edge_arrow_fill",
        # Edge aesthetics - variable mappings (_by and legacy _var)
        "edge_alpha_by", "edge_alpha_var", "edge_color_by", "edge_color_var",
        "edge_linewidth_by", "edge_linewidth_var", "edge_linetype_by", "edge_linetype_var",
        # Edge aesthetics - manual scales and labels
        "edge_alpha_manual", "edge_alpha_label", "edge_color_manual", "edge_color_label",
        "edge_linewidth_manual", "edge_linewidth_label", "edge_linetype_label",
        "mutate_weight",
        # Node/Point aesthetics - fixed
        "node_alpha", "point_alpha", "node_color", "point_color", "node_fill", "point_fill",
        "node_shape", "point_shape", "node_size", "point_size", "node_stroke", "point_stroke",
        # Node/Point aesthetics - variable mappings (_by and legacy _var)
        "node_alpha_by", "point_alpha_by", "node_alpha_var", "point_alpha_var",
        "node_color_by", "point_color_by", "node_color_var", "point_color_var",
        "node_fill_by", "point_fill_by", "node_fill_var", "point_fill_var",
        "node_shape_by", "point_shape_by", "node_shape_var", "point_shape_var",
        "node_size_by", "point_size_by", "node_size_var", "point_size_var",
        "node_stroke_by", "point_stroke_by", "node_stroke_var", "point_stroke_var",
        # Node/Point aesthetics - manual scales and labels
        "node_color_manual", "point_color_manual", "node_color_label", "point_color_label",
        "node_fill_manual", "point_fill_manual", "node_fill_label", "point_fill_label",
        "node_shape_manual", "point_shape_manual", "node_shape_label", "point_shape_label",
        "node_size_manual", "point_size_manual", "node_size_label", "point_size_label",
        "node_alpha_label", "point_alpha_label", "node_stroke_label", "point_stroke_label",
        "point_size_guide",
        # Node/Point - grouping
        "node_group", "point_group", "node_group_by", "point_group_by",
        # Text aesthetics - fixed
        "text_alpha", "text_angle", "text_color", "text_family", "text_fill",
        "text_fontface", "text_hjust", "text_vjust", "text_lineheight", "text_size",
        # Text aesthetics - variable and labels
        "text_alpha_var", "text_alpha_label", "text_color_var", "text_color_label",
        "text_size_var", "text_size_label",
        # Text selection
        "select_text", "select_text_display",
        # Label aesthetics - fixed
        "label_alpha", "label_angle", "label_color", "label_family", "label_fill",
        "label_fontface", "label_hjust", "label_vjust", "label_lineheight", "label_size",
        # Label aesthetics - variable and labels
        "label_alpha_var", "label_alpha_label", "label_color_var", "label_color_label",
        "label_fill_var", "label_fill_label", "label_size_var", "label_size_label",
        # Label selection
        "select_label", "select_label_display",
        # Text repel parameters
        "text_repel_force", "text_repel_force_pull", "text_repel_max_overlaps",
        "text_repel_nudge_x", "text_repel_nudge_y", "text_repel_box_padding",
        "text_repel_point_padding", "text_repel_min_segment_length", "text_repel_arrow",
        "text_repel_max_time", "text_repel_max_iter", "text_repel_seed",
        "text_repel_xlim", "text_repel_ylim", "text_repel_direction",
        "text_repel_segment_color", "text_repel_segment_size", "text_repel_segment_alpha",
        "text_repel_segment_linetype", "text_repel_segment_curvature", "text_repel_segment_angle",
        "text_repel_segment_ncp", "text_repel_segment_square", "text_repel_segment_inflect",
        # Label repel parameters
        "label_repel_force", "label_repel_force_pull", "label_repel_max_overlaps",
        "label_repel_nudge_x", "label_repel_nudge_y", "label_repel_box_padding",
        "label_repel_point_padding", "label_repel_min_segment_length", "label_repel_arrow",
        "label_repel_max_time", "label_repel_max_iter", "label_repel_seed",
        "label_repel_xlim", "label_repel_ylim", "label_repel_direction",
        "label_repel_segment_color", "label_repel_segment_size", "label_repel_segment_alpha",
        "label_repel_segment_linetype", "label_repel_segment_curvature", "label_repel_segment_angle",
        "label_repel_segment_ncp", "label_repel_segment_square", "label_repel_segment_inflect",
        "label_repel_label_padding", "label_repel_label_r", "label_repel_label_size",
        # Highlighting
        "highlight", "highlight_color", "highlight_label", "highlight_size_increase",
        "show_other_in_legend",
        # Scale parameters
        "edge_arrow_saturation", "point_size_min", "point_size_max",
        # Color palettes and direction
        "node_color_palette", "node_fill_palette", "edge_color_palette",
        "node_color_direction", "node_fill_direction", "edge_color_direction",
        # Ego network parameters
        "ego_group_by", "ego_order_by", "ego_weight_to_distance",
        "ego_ring_gap", "ego_size", "buffer_radius", "transition_zone", "n_rings",
        "ego_name",
        # Miscellaneous 
        "arrow"
    )
    
    # Common typos and their corrections
    typo_map <- list(
        # Pluralization errors
        "highlight_colors" = "highlight_color",
        "node_colors" = "node_color",
        "point_colors" = "point_color",
        "edge_colors" = "edge_color",
        "node_sizes" = "node_size",
        "point_sizes" = "point_size",
        # Underscore vs no underscore
        "nodecolor" = "node_color",
        "nodesize" = "node_size",
        "edgecolor" = "edge_color",
        "edgewidth" = "edge_linewidth",
        "pointcolor" = "point_color",
        "pointsize" = "point_size",
        # Common alternatives
        "edge_width" = "edge_linewidth",
        "edge_size" = "edge_linewidth",
        "line_width" = "edge_linewidth",
        "line_size" = "edge_linewidth",
        "node_colour" = "node_color",
        "edge_colour" = "edge_color",
        "point_colour" = "point_color",
        # Highlight specific
        "highlight_colours" = "highlight_color",
        "highlight_increase" = "highlight_size_increase",
        "highlight_size" = "highlight_size_increase",
        # Filter typos
        "time" = "time_filter",
        "node_filters" = "node_filter",
        "edge_filters" = "edge_filter",
        # Text/label typos
        "text_repel" = "add_text_repel",
        "label_repel" = "add_label_repel",
        "repel_text" = "add_text_repel",
        "repel_label" = "add_label_repel"
    )
    
    # Check each parameter name
    for (param_name in dot_names) {
        # Skip empty names
        if (param_name == "") next
        
        # Check if it's a known typo
        if (param_name %in% names(typo_map)) {
            correct_param <- typo_map[[param_name]]
            cli::cli_warn(c(
                "!" = "Parameter '{param_name}' appears to be a typo.",
                "i" = "Did you mean '{correct_param}'?",
                "i" = "The parameter '{param_name}' is being ignored."
            ))
            next
        }
        
        # Check if it's not in the valid list
        if (!param_name %in% valid_params) {
            # Try fuzzy matching to suggest alternatives
            distances <- adist(param_name, valid_params)[1,]
            closest_match <- valid_params[which.min(distances)]
            min_distance <- min(distances)
            
            # If the distance is small, suggest the alternative
            if (min_distance <= 3) {  # Levenshtein distance threshold
                cli::cli_warn(c(
                    "!" = "Unknown parameter '{param_name}' in plot.netify().",
                    "i" = "Did you mean '{closest_match}'?",
                    "i" = "The parameter '{param_name}' is being ignored."
                ))
            } else {
                cli::cli_warn(c(
                    "!" = "Unknown parameter '{param_name}' in plot.netify().",
                    "i" = "This parameter is being ignored.",
                    "i" = "Use ?plot.netify to see valid parameters."
                ))
            }
        }
    }
    
    # Additional checks for specific parameter combinations
    if ("highlight_colors" %in% dot_names && "highlight_color" %in% dot_names) {
        cli::cli_warn(c(
            "!" = "Both 'highlight_colors' and 'highlight_color' specified.",
            "i" = "Use 'highlight_color' (singular). 'highlight_colors' is ignored."
        ))
    }
    
    if (("node_color_by" %in% dot_names || "point_color_by" %in% dot_names) && 
        "highlight" %in% dot_names) {
        cli::cli_inform(c(
            "i" = "Note: When using both color_by and highlight, highlighting takes precedence.",
            "i" = "To combine them, consider creating a custom variable before plotting."
        ))
    }
    
    return(invisible(NULL))
}