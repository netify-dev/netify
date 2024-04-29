#' Adjust Plotting Parameters and Data for Netify Visualization
#'
#' This function prepares the plotting parameters and data from a `netify` object for visualization.
#' It sets defaults for plotting parameters if not supplied by the user and modifies the node and
#' edge data frames based on the provided parameters and the attributes of the `netify` object.
#'
#' @param plot_args A list of user-defined plotting arguments that may include options for text overlap,
#'        edge curvature, edge angles, line segment parameters, and labeling specific actors.
#' @param net_dfs A list containing nodal and edge data frames as returned by `decompose_netlet`.
#'        `nodal_data` should contain node information and `edge_data` should contain edge information.
#' @param obj_attrs Attributes of the `netify` object, which should include details about whether the
#'        network is symmetric, its type (cross-sectional or longitudinal), and other relevant properties.
#'
#' @details
#' The function checks and sets default values for various plot parameters such as:
#' - `check_overlap`: Whether to check for overlap in text labels (default TRUE).
#' - `curve_edges`: Whether to curve the edges in the plot (default FALSE).
#' - `edge_curvature`: The curvature of the edges (default 0.5).
#' - `edge_angle`: The angle of line segments (default 90 degrees).
#' - `edge_ncp`: Number of control points for edge curvature (default 5).
#' - `edge_arrow`: Default arrow type for directed edges.
#' It also adjusts the edge and node data based on whether the network is directed or undirected,
#' and handles specific labeling of actors and transparency settings based on edge weights.
#'
#' @return A list containing the adjusted `plot_args` and modified `net_dfs` ready for plotting.
#' @author Cassy Dorff, Shahryar Minhas
#'
#'
#' @export

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

		# replace name label in net_dfs$nodal_data
		# with NA if not in to_label
		net_dfs$nodal_data$name_text = ifelse(
			net_dfs$nodal_data$name %in% plot_args$select_text,
			net_dfs$nodal_data$name, '' )

		# if user supplied alternative text in 
		# select_text_display then use that instead of 
		# the name in the df
		if(!is.null(plot_args$select_text_display)){
			net_dfs$nodal_data$name_text = plot_args$select_text_display[match(
				net_dfs$nodal_data$name_text, plot_args$select_text)] }

		# set add_text to TRUE
		plot_args$add_text = TRUE 	
	}

	# if users chose to label specific nodes
	# then replace name column for label
	if(!is.null(plot_args$select_label)){

		# replace name label in net_dfs$nodal_data
		# with NA if not in to_label
		net_dfs$nodal_data$name_label = ifelse(
			net_dfs$nodal_data$name %in% plot_args$select_label,
			net_dfs$nodal_data$name, '' )

		# if user supplied alternative label in 
		# select_label_display then use that instead of 
		# the name in the df
		if(!is.null(plot_args$select_label_display)){
		net_dfs$nodal_data$name_label = plot_args$select_label_display[match(
			net_dfs$nodal_data$name_label, plot_args$select_label)] }

		# set add_text to TRUE
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
	if(is.null(plot_args$edge_linejoin)){ plot_args$edge_lineend = "round" }		

	# general arrow
	plot_args$edge_arrow_fill = NULL
	if(!obj_attrs$symmetric){
		if(is.null(plot_args$edge_arrow)){
			plot_args$edge_arrow = arrow(length = unit(0.3, "cm"))
		} else {
			plot_args$edge_arrow = NULL
	} }
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
	# is created in decompose_netlet
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