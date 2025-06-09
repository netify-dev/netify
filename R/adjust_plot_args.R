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
#' @examples 
#' \dontrun{
#' # Basic usage with a netify object
#' data(icews)
#' icews_10 <- icews[icews$year == 2010,]
#' 
#' # Create netify object
#' net <- netify(
#'   icews_10,
#'   actor1 = 'i', actor2 = 'j',
#'   weight = 'verbCoop'
#' )
#' 
#' # Decompose to get data frames
#' net_dfs <- decompose_netify(net)
#' 
#' # Get object attributes
#' obj_attrs <- attributes(net)
#' 
#' # Prepare plotting arguments with custom settings
#' plot_args <- list(
#'   point_size = 3,
#'   edge_color = "gray50",
#'   select_text = c("USA", "China", "Russia"),
#'   curve_edges = TRUE,
#'   edge_curvature = 0.3
#' )
#' 
#' # Adjust plot arguments
#' adjusted <- adjust_plot_args(plot_args, net_dfs, obj_attrs)
#' 
#' # Access the prepared data
#' plot_args_final <- adjusted$plot_args
#' net_dfs_final <- adjusted$net_dfs
#' 
#' # Check that defaults were set
#' plot_args_final$point_color  # "black" (default)
#' plot_args_final$point_size   # 3 (user-specified)
#' 
#' # Check selective labeling columns
#' table(net_dfs_final$nodal_data$name_text != "")  # Only selected nodes
#' }
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
			plot_args$edge_arrow = arrow(length = unit(0.2, "cm"))
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