#' Adjust plotting parameters and data for netify visualization
#'
#' `adjust_plot_args` prepares plotting parameters and modifies network data 
#' from a netify object for visualization. This function sets default values 
#' for various plotting parameters and adjusts node and edge data frames based 
#' on the network's properties and user specifications.
#'
#' @param plot_args A list of user-defined plotting arguments. Can include parameters 
#'   for controlling visual elements such as text overlap checking, edge curvature, 
#'   node appearance, and selective labeling. See Details for the full list of 
#'   available parameters and their defaults.
#' @param net_dfs A list containing data frames as returned by `decompose_netify`. 
#'   Must include:
#'   \itemize{
#'     \item \strong{nodal_data}: A data.frame containing node information with 
#'       at minimum a 'name' column identifying each node
#'     \item \strong{edge_data}: A data.frame containing edge information with 
#'       columns for source, target, and optionally edge weights
#'   }
#' @param obj_attrs A list of attributes from the netify object. Must include:
#'   \itemize{
#'     \item \strong{symmetric}: Logical indicating whether the network is 
#'       undirected (TRUE) or directed (FALSE)
#'   }
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item \strong{plot_args}: The input plot_args list with all default values 
#'       set for any unspecified parameters
#'     \item \strong{net_dfs}: The modified net_dfs list with additional columns 
#'       for selective labeling (name_text and name_label)
#'   }
#'
#' @details
#' This function handles three main tasks:
#' 
#' \strong{1. Setting Default Values}
#' 
#' The function sets sensible defaults for all plotting parameters if not 
#' explicitly provided by the user:
#' 
#' \emph{General plot settings:}
#' \itemize{
#'   \item \code{use_theme_netify}: Use the netify theme (default: TRUE)
#'   \item \code{remove_isolates}: Remove isolated nodes (default: TRUE)
#'   \item \code{check_overlap}: Check for text/label overlap (default: TRUE)
#' }
#' 
#' \emph{Geometry visibility:}
#' \itemize{
#'   \item \code{add_points}: Show nodes as points (default: TRUE)
#'   \item \code{add_text}: Show node names as text (default: FALSE)
#'   \item \code{add_label}: Show node names as labels (default: FALSE)
#'   \item \code{add_edges}: Show edges (default: TRUE)
#'   \item \code{curve_edges}: Use curved edges (default: FALSE)
#' }
#' 
#' \emph{Point (node) appearance:}
#' \itemize{
#'   \item \code{point_alpha}: Transparency (default: NA)
#'   \item \code{point_color}: Border color (default: 'black')
#'   \item \code{point_fill}: Fill color (default: NA)
#'   \item \code{point_shape}: Shape code (default: 19)
#'   \item \code{point_size}: Size (default: 1.5)
#'   \item \code{point_stroke}: Border width (default: 0.5)
#' }
#' 
#' \emph{Text appearance:}
#' \itemize{
#'   \item \code{text_alpha}: Transparency (default: NA)
#'   \item \code{text_color}: Color (default: 'black')
#'   \item \code{text_fill}: Background fill (default: 'white')
#'   \item \code{text_size}: Size (default: 3.88)
#'   \item \code{text_family}: Font family (default: '')
#'   \item \code{text_fontface}: Font face (default: 1)
#'   \item \code{text_angle}: Rotation angle (default: 0)
#'   \item \code{text_hjust}: Horizontal justification (default: 0.5)
#'   \item \code{text_vjust}: Vertical justification (default: 0.5)
#'   \item \code{text_lineheight}: Line height (default: 1.2)
#' }
#' 
#' \emph{Label appearance:}
#' \itemize{
#'   \item \code{label_alpha}: Transparency (default: NA)
#'   \item \code{label_color}: Text color (default: 'black')
#'   \item \code{label_fill}: Background color (default: 'white')
#'   \item \code{label_size}: Size (default: 3.88)
#'   \item \code{label_family}: Font family (default: '')
#'   \item \code{label_fontface}: Font face (default: 1)
#'   \item \code{label_angle}: Rotation angle (default: 0)
#'   \item \code{label_hjust}: Horizontal justification (default: 0.5)
#'   \item \code{label_vjust}: Vertical justification (default: 0.5)
#'   \item \code{label_lineheight}: Line height (default: 1.2)
#' }
#' 
#' \emph{Edge appearance:}
#' \itemize{
#'   \item \code{edge_color}: Color (default: 'black')
#'   \item \code{edge_linewidth}: Width (default: 0.5)
#'   \item \code{edge_linetype}: Line type (default: 1)
#'   \item \code{edge_alpha}: Transparency (default: NA)
#'   \item \code{edge_curvature}: Curvature amount (default: 0.5)
#'   \item \code{edge_angle}: Angle in degrees (default: 90)
#'   \item \code{edge_ncp}: Number of control points (default: 5)
#'   \item \code{edge_lineend}: Line end style (default: 'butt')
#'   \item \code{edge_linejoin}: Line join style (default: 'round')
#'   \item \code{edge_arrow}: Arrow specification for directed networks (default: 
#'     arrow with 0.2cm length for directed networks, NULL for undirected)
#' }
#' 
#' \strong{2. Selective Node Labeling}
#' 
#' The function supports selective labeling of nodes through:
#' \itemize{
#'   \item \code{select_text}: Character vector of node names to show as text
#'   \item \code{select_text_display}: Optional character vector of alternative 
#'     text to display (must match length and order of select_text)
#'   \item \code{select_label}: Character vector of node names to show as labels
#'   \item \code{select_label_display}: Optional character vector of alternative 
#'     labels to display (must match length and order of select_label)
#' }
#' 
#' When selective labeling is used, the function creates new columns in the 
#' nodal_data data.frame (name_text and name_label) and automatically sets 
#' the corresponding add_text or add_label parameter to TRUE.
#' 
#' \strong{3. Edge Weight Handling}
#' 
#' For weighted networks, the function automatically detects edge weights and 
#' sets the edge_alpha_var parameter to map transparency to edge weights if no 
#' alpha variable is specified by the user.
#'
#' @note 
#' This function is primarily intended for internal use by netify plotting 
#' functions.
#' 
#' The function assumes that edge weights, if present, are stored in the fourth 
#' column of the edge_data data.frame as created by `decompose_netify`.
#' 
#' All color parameters accept standard R color specifications including named 
#' colors, hex codes, and RGB values.
#'
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @keywords internal
#' @noRd

adjust_plot_args <- function(plot_args, net_dfs, obj_attrs) {

	# by default specify to use theme_netify
	if(is.null(plot_args$use_theme_netify)){
		plot_args$use_theme_netify = TRUE }

	# by default remove isolates
	if(is.null(plot_args$remove_isolates)){
		plot_args$remove_isolates = TRUE }

	# by default set check overlap for text/label to TRUE
	if(is.null(plot_args$check_overlap)){
		plot_args$check_overlap = TRUE }

	# convert node_* to point_* internally for backward compatibility
	# allow users to use either node_* or point_* parameters
	node_to_point_mapping <- list(
		# Static parameters
		'node_alpha' = 'point_alpha',
		'node_color' = 'point_color', 
		'node_fill' = 'point_fill',
		'node_shape' = 'point_shape',
		'node_size' = 'point_size',
		'node_stroke' = 'point_stroke',
		# variable parameters - convert both _by and legacy _var
		'node_alpha_by' = 'point_alpha_var',
		'node_color_by' = 'point_color_var',
		'node_fill_by' = 'point_fill_var',
		'node_shape_by' = 'point_shape_var',
		'node_size_by' = 'point_size_var',
		'node_stroke_by' = 'point_stroke_var',
		# legacy _var support
		'node_alpha_var' = 'point_alpha_var',
		'node_color_var' = 'point_color_var',
		'node_fill_var' = 'point_fill_var',
		'node_shape_var' = 'point_shape_var',
		'node_size_var' = 'point_size_var',
		'node_stroke_var' = 'point_stroke_var' )
	
	# apply the mapping
	for(old_name in names(node_to_point_mapping)) {
		new_name <- node_to_point_mapping[[old_name]]
		if(!is.null(plot_args[[old_name]]) && is.null(plot_args[[new_name]])) {
			plot_args[[new_name]] <- plot_args[[old_name]]
			plot_args[[old_name]] <- NULL } }
	
	# convert _by to _var for all parameters (edge, point, text, label)
	by_to_var_mapping <- list(
		# edge parameters
		'edge_alpha_by' = 'edge_alpha_var',
		'edge_color_by' = 'edge_color_var',
		'edge_linewidth_by' = 'edge_linewidth_var',
		'edge_linetype_by' = 'edge_linetype_var',
		# point parameters (in case used directly)
		'point_alpha_by' = 'point_alpha_var',
		'point_color_by' = 'point_color_var',
		'point_fill_by' = 'point_fill_var',
		'point_shape_by' = 'point_shape_var',
		'point_size_by' = 'point_size_var',
		'point_stroke_by' = 'point_stroke_var',
		# text parameters
		'text_alpha_by' = 'text_alpha_var',
		'text_color_by' = 'text_color_var',
		'text_size_by' = 'text_size_var',
		# label parameters
		'label_alpha_by' = 'label_alpha_var',
		'label_color_by' = 'label_color_var',
		'label_fill_by' = 'label_fill_var',
		'label_size_by' = 'label_size_var' )
	
	# Apply the by to var mapping
	for(old_name in names(by_to_var_mapping)) {
		new_name <- by_to_var_mapping[[old_name]]
		if(!is.null(plot_args[[old_name]]) && is.null(plot_args[[new_name]])) {
			plot_args[[new_name]] <- plot_args[[old_name]]
			plot_args[[old_name]] <- NULL } }

	# process palette if provided #####################
	if(!is.null(plot_args$palette)) {
	palette_settings <- get_palette(plot_args$palette)
	
	if(!is.null(palette_settings)) {
		# Apply palette settings only if user hasn't explicitly set them
		if(is.null(plot_args$edge_color)) {
		plot_args$edge_color <- palette_settings$edge_color
		}
		if(is.null(plot_args$edge_alpha) && is.null(plot_args$edge_alpha_var)) {
		plot_args$edge_alpha <- palette_settings$edge_alpha
		}
		if(is.null(plot_args$point_fill) && is.null(plot_args$node_fill)) {
		plot_args$point_fill <- palette_settings$node_fill
		}
		if(is.null(plot_args$point_color) && is.null(plot_args$node_color)) {
		plot_args$point_color <- palette_settings$node_color
		}
		if(is.null(plot_args$curve_edges)) {
		plot_args$curve_edges <- palette_settings$curve_edges
		}
		# Set shape to 21 to use fill color
		if(is.null(plot_args$point_shape) && is.null(plot_args$node_shape)) {
		plot_args$point_shape <- 21
		}
	} else {
		cli::cli_alert_warning(
			"Unknown palette '{plot_args$palette}'. Use list_palettes() to see available options.")
	}
	}
	######################

	# process geom choices #####################
	if(is.null(plot_args$add_points)){ plot_args$add_points = TRUE }
	if(is.null(plot_args$add_text)){ plot_args$add_text = FALSE }
	if(is.null(plot_args$add_label)){ plot_args$add_label = FALSE }
	if(is.null(plot_args$add_edges)){ plot_args$add_edges = TRUE }
	if(is.null(plot_args$curve_edges)){ plot_args$curve_edges = FALSE }		
	######################		
	
	# label specific nodes via text or label #####################

	# add columns for text and label
	net_dfs$nodal_data$name_text = net_dfs$nodal_data$name
	net_dfs$nodal_data$name_label = net_dfs$nodal_data$name

	# if users chose to label specific nodes
	# then replace name column for text
	if(!is.null(plot_args$select_text)){

		# if user supplied alternative text in 
		# select_text_display then use that instead of 
		# the name in the df
		if(!is.null(plot_args$select_text_display)){
			# create a mapping from select_text to select_text_display
			text_mapping <- setNames(plot_args$select_text_display, plot_args$select_text)
			
			# apply the mapping, using NA for non-selected nodes
			net_dfs$nodal_data$name_text = ifelse(
				net_dfs$nodal_data$name %in% plot_args$select_text,
				text_mapping[net_dfs$nodal_data$name],
				NA)
		} else {
			# no display text provided, use the actual names for selected nodes
			net_dfs$nodal_data$name_text = ifelse(
				net_dfs$nodal_data$name %in% plot_args$select_text,
				net_dfs$nodal_data$name, 
				NA)  # Use NA instead of ''
		}

		# set add_text to TRUE
		plot_args$add_text = TRUE 	
	}

	# if users chose to label specific nodes
	# then replace name column for label
	if(!is.null(plot_args$select_label)){

		# if user supplied alternative label in 
		# select_label_display then use that instead of 
		# the name in the df
		if(!is.null(plot_args$select_label_display)){
			# create a mapping from select_label to select_label_display
			label_mapping <- setNames(plot_args$select_label_display, plot_args$select_label)
			
			# apply the mapping, using NA for non-selected nodes
			net_dfs$nodal_data$name_label = ifelse(
				net_dfs$nodal_data$name %in% plot_args$select_label,
				label_mapping[net_dfs$nodal_data$name],
				NA)
		} else {
			# no display label provided, use the actual names for selected nodes
			net_dfs$nodal_data$name_label = ifelse(
				net_dfs$nodal_data$name %in% plot_args$select_label,
				net_dfs$nodal_data$name,
				NA)  # Use NA instead of ''
		}

		# set add_label to TRUE
		plot_args$add_label = TRUE
	}
	######################	

	# set up static geom_point defaults #####################
	if(is.null(plot_args$point_alpha)){  plot_args$point_alpha = NA }
	if(is.null(plot_args$point_color)){ plot_args$point_color = 'black' }
	if(is.null(plot_args$point_fill)){ plot_args$point_fill = NA }
	if(is.null(plot_args$point_shape)){ plot_args$point_shape = 19 }		
	if(is.null(plot_args$point_size)){ plot_args$point_size = 1.5 }
	if(is.null(plot_args$point_stroke)){ plot_args$point_stroke = 0.5 }
	######################

	# set up static geom_text defaults #####################	
	if(is.null(plot_args$text_alpha)){ plot_args$text_alpha = NA }
	if(is.null(plot_args$text_color)){ plot_args$text_color = 'black' }
	if(is.null(plot_args$text_fill)){ plot_args$text_fill = 'white' }		
	if(is.null(plot_args$text_size)){ plot_args$text_size = 3.88 }
	if(is.null(plot_args$text_family)){ plot_args$text_family = '' }
	if(is.null(plot_args$text_fontface)){ plot_args$text_fontface = 1 }
	if(is.null(plot_args$text_angle)){ plot_args$text_angle = 0 }
	if(is.null(plot_args$text_hjust)){ plot_args$text_hjust = 0.5 }
	if(is.null(plot_args$text_vjust)){ plot_args$text_vjust = 0.5 }
	if(is.null(plot_args$text_lineheight)){ plot_args$text_lineheight = 1.2 }
	#####################

	# set up static geom_label defaults #####################	
	if(is.null(plot_args$label_alpha)){ plot_args$label_alpha = NA }
	if(is.null(plot_args$label_color)){ plot_args$label_color = 'black' }
	if(is.null(plot_args$label_fill)){ plot_args$label_fill = 'white' }		
	if(is.null(plot_args$label_size)){ plot_args$label_size = 3.88 }
	if(is.null(plot_args$label_family)){ plot_args$label_family = '' }
	if(is.null(plot_args$label_fontface)){ plot_args$label_fontface = 1 }
	if(is.null(plot_args$label_angle)){ plot_args$label_angle = 0 }
	if(is.null(plot_args$label_hjust)){ plot_args$label_hjust = 0.5 }
	if(is.null(plot_args$label_vjust)){ plot_args$label_vjust = 0.5 }
	if(is.null(plot_args$label_lineheight)){ plot_args$label_lineheight = 1.2 }
	#####################

	# set up geom_segment/curve/arrow defaults ####################

	# general segment
	if(is.null(plot_args$edge_color)) { plot_args$edge_color <- "black" }
	if(is.null(plot_args$edge_linewidth)) { plot_args$edge_linewidth <- 0.5 }
	if(is.null(plot_args$edge_linetype)) { plot_args$edge_linetype <- 1 }
	if(is.null(plot_args$edge_alpha)) { plot_args$edge_alpha <- NA }

	# general curve
	if(is.null(plot_args$edge_curvature)){ plot_args$edge_curvature = 0.5 }
	if(is.null(plot_args$edge_angle)){ plot_args$edge_angle = 90 }	
	if(is.null(plot_args$edge_ncp)){ plot_args$edge_ncp = 5 }			

	# line end/join type
	if(is.null(plot_args$edge_lineend)){ plot_args$edge_lineend = "butt" }
	if(is.null(plot_args$edge_linejoin)){ plot_args$edge_linejoin = "round" }

	# general arrow
	plot_args$edge_arrow_fill = NULL
	if(!obj_attrs$symmetric){
		if(is.null(plot_args$edge_arrow)){
			plot_args$edge_arrow = arrow(length = unit(0.2, "cm"))
		}
	} else {
		# For symmetric networks, no arrows
		plot_args$edge_arrow = NULL
	}
	######################

	######################
	# set up scale label parameters #####################
	# edge scale labels
	if(is.null(plot_args$edge_alpha_label)){ plot_args$edge_alpha_label = NULL }
	if(is.null(plot_args$edge_color_label)){ plot_args$edge_color_label = NULL }
	if(is.null(plot_args$edge_linewidth_label)){ plot_args$edge_linewidth_label = NULL }
	if(is.null(plot_args$edge_linetype_label)){ plot_args$edge_linetype_label = NULL }
	
	# node/point scale labels (supporting both node_ and point_ prefixes)
	if(is.null(plot_args$node_size_label) && is.null(plot_args$point_size_label)){ 
		plot_args$node_size_label = NULL 
	} else {
		plot_args$node_size_label = plot_args$node_size_label %||% plot_args$point_size_label
	}
	if(is.null(plot_args$node_color_label) && is.null(plot_args$point_color_label)){ 
		plot_args$node_color_label = NULL 
	} else {
		plot_args$node_color_label = plot_args$node_color_label %||% plot_args$point_color_label
	}
	if(is.null(plot_args$node_fill_label) && is.null(plot_args$point_fill_label)){ 
		plot_args$node_fill_label = NULL 
	} else {
		plot_args$node_fill_label = plot_args$node_fill_label %||% plot_args$point_fill_label
	}
	if(is.null(plot_args$node_shape_label) && is.null(plot_args$point_shape_label)){ 
		plot_args$node_shape_label = NULL 
	} else {
		plot_args$node_shape_label = plot_args$node_shape_label %||% plot_args$point_shape_label
	}
	if(is.null(plot_args$node_alpha_label) && is.null(plot_args$point_alpha_label)){ 
		plot_args$node_alpha_label = NULL 
	} else {
		plot_args$node_alpha_label = plot_args$node_alpha_label %||% plot_args$point_alpha_label
	}
	if(is.null(plot_args$node_stroke_label) && is.null(plot_args$point_stroke_label)){ 
		plot_args$node_stroke_label = NULL 
	} else {
		plot_args$node_stroke_label = plot_args$node_stroke_label %||% plot_args$point_stroke_label
	}
	
	# text scale labels
	if(is.null(plot_args$text_size_label)){ plot_args$text_size_label = NULL }
	if(is.null(plot_args$text_color_label)){ plot_args$text_color_label = NULL }
	if(is.null(plot_args$text_alpha_label)){ plot_args$text_alpha_label = NULL }
	
	# label (box) scale labels
	if(is.null(plot_args$label_size_label)){ plot_args$label_size_label = NULL }
	if(is.null(plot_args$label_color_label)){ plot_args$label_color_label = NULL }
	if(is.null(plot_args$label_fill_label)){ plot_args$label_fill_label = NULL }
	if(is.null(plot_args$label_alpha_label)){ plot_args$label_alpha_label = NULL }

    # Color palette parameters
    if(is.null(plot_args$node_color_palette)){ plot_args$node_color_palette = NULL }
    if(is.null(plot_args$node_fill_palette)){ plot_args$node_fill_palette = NULL }
    if(is.null(plot_args$edge_color_palette)){ plot_args$edge_color_palette = NULL }
    if(is.null(plot_args$node_color_direction)){ plot_args$node_color_direction = 1 }
    if(is.null(plot_args$node_fill_direction)){ plot_args$node_fill_direction = 1 }
    if(is.null(plot_args$edge_color_direction)){ plot_args$edge_color_direction = 1 }
    
    # Apply smart palette selection
    plot_args <- apply_smart_palettes(plot_args, net_dfs)	
	######################

	# choose weight var #####################
	# if network is weighted and users did not
	# specify alpha level choose for them
	# okay this is a hacky way to look at 
	# whether the edge has weight, ie
	# is not just a check for presence but
	# it should work ... main thing it depends on 
	# is just the position of the weight column
	# which should always be the third column because
	# of how the edge_data object inside of net_dfs
	# is created in decompose_netify
	if(length(unique(net_dfs$edge_data[,4]))>1){
		if(is.null(plot_args$edge_alpha_var)){
			plot_args$edge_alpha_var = names(
				net_dfs$edge_data)[4] }
	}
	######################

	######################
    #
    return(
        list(
            plot_args=plot_args,
            net_dfs=net_dfs
        )
    )
	######################
}