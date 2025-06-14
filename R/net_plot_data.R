#' Prepare netify data for network visualization
#'
#' `net_plot_data` processes a netify object and generates all necessary components 
#' for network visualization. This function handles layout computation, aesthetic 
#' parameter organization, and data structuring for subsequent plotting with ggplot2 
#' or other visualization tools.
#'
#' @param netlet A netify object (class "netify") containing the network to be 
#'   visualized. Must be a single-layer network (multilayer networks not currently 
#'   supported).
#' @param plot_args A list of plotting arguments controlling visualization appearance 
#'   and behavior. Can include:
#'   
#'   \strong{Layout parameters:}
#'   \itemize{
#'     \item \code{point_layout}: Pre-computed node positions as a data.frame or list 
#'       of data.frames (for longitudinal networks). If provided, overrides layout 
#'       algorithm selection
#'     \item \code{layout}: Character string specifying the igraph layout algorithm. 
#'       Options: "nicely" (default), "fr" (Fruchterman-Reingold), "kk" (Kamada-Kawai), 
#'       "circle", "star", "grid", "tree", "bipartite", and others. See 
#'       \code{\link{get_node_layout}} for full list
#'     \item \code{static_actor_positions}: Logical. If TRUE, maintains consistent 
#'       node positions across time periods in longitudinal networks
#'     \item \code{which_static}: Integer specifying which time period to use as 
#'       the template for static positions
#'     \item \code{seed}: Integer for reproducible random layouts
#'   }
#'   
#'   \strong{Display options:}
#'   \itemize{
#'     \item \code{remove_isolates}: Logical. Remove unconnected nodes (default: TRUE)
#'     \item \code{add_edges}: Logical. Include edges in visualization (default: TRUE)
#'     \item \code{curve_edges}: Logical. Use curved edges instead of straight (default: FALSE)
#'     \item \code{add_points}: Logical. Display nodes as points (default: TRUE)
#'     \item \code{add_text}: Logical. Add text labels to nodes (default: FALSE)
#'     \item \code{add_label}: Logical. Add boxed labels to nodes (default: FALSE)
#'   }
#'   
#'   \strong{Selective labeling:}
#'   \itemize{
#'     \item \code{select_text}: Character vector of node names to label with text
#'     \item \code{select_label}: Character vector of node names to label with boxes
#'   }
#'   
#'   Additional aesthetic parameters are processed by \code{adjust_plot_args} 
#'   and \code{gg_params}.
#'
#' @return A list with three components for creating network visualizations:
#'   \itemize{
#'     \item \strong{plot_args}: Processed plotting arguments with defaults applied 
#'       and parameters validated. Includes all layout and display settings
#'     \item \strong{ggnet_params}: Organized aesthetic parameters for ggplot2 mapping. 
#'       Contains separate specifications for nodes, edges, text, and labels with both 
#'       static and dynamic (data-mapped) aesthetics
#'     \item \strong{net_dfs}: Data frames ready for plotting:
#'       \itemize{
#'         \item \code{nodal_data}: Node information including positions (x, y), 
#'           attributes, and any additional variables
#'         \item \code{edge_data}: Edge information including endpoint coordinates 
#'           (x1, y1, x2, y2) and edge attributes
#'       }
#'   }
#'
#' @details
#' This function serves as the data preparation layer for netify visualization, 
#' performing several operations:
#' 
#' \strong{Data validation:}
#' \itemize{
#'   \item Ensures the input is a valid netify object
#'   \item Checks for single-layer networks (multilayer not supported)
#'   \item Validates ego networks contain only one ego
#' }
#' 
#' \strong{Layout computation:}
#' \itemize{
#'   \item Generates node positions using specified algorithm if not provided
#'   \item Calculates edge endpoint coordinates based on node positions
#'   \item Handles both cross-sectional and longitudinal layouts
#' }
#' 
#' \strong{Data organization:}
#' \itemize{
#'   \item Merges layout information with network attributes
#'   \item Processes plotting arguments and applies defaults
#'   \item Organizes aesthetic parameters for ggplot2 compatibility
#'   \item Removes isolates if requested
#' }
#' 
#' \strong{Output structure:}
#' 
#' The returned data is structured for direct use with ggplot2 or can be further 
#' customized. The separation of layout, aesthetics, and data allows for flexible 
#' visualization workflows.
#'
#' @note 
#' This function is primarily designed for use with netify's plot method but can 
#' be called directly for custom visualization workflows.
#' 
#' For multilayer networks, use \code{\link{subset_netify}} to extract individual 
#' layers before visualization.
#' 
#' For ego networks with multiple egos, create separate visualizations and combine 
#' them using packages like patchwork.
#' 
#'
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @export net_plot_data

net_plot_data <- function(netlet, plot_args=list()) {

	######################
    # check if netify object
    netify_check(netlet)  
	######################    

	######################    
    # if more than one layer tell user they must specify a single layer
    if(length(attributes(netlet)$layers) > 1){
    cli::cli_alert_danger(
        'Error: This object has multiple layers. 
        `plot` does not currently support multilayer `netify` inputs.
        Please use the `subset_netify` function to create a `netify` object with a single layer.' )
    stop() }
	######################    

	######################	
	# if ego net modify names and stop if more than one ego
    ego_netlet = attr(netlet, 'ego_netlet') ; ego_longit = FALSE
    if(!is.null(ego_netlet)){
        
        # get info on ego network
        ego_longit = attr(netlet, 'ego_longit')
        ego_entry = attr(netlet, 'ego_entry')

        # stop if more than one ego
        if(length(ego_entry) > 1){
            cli::cli_alert_danger(
                'Error: This object has multiple egos.
                `plot` does not currently support multiple ego inputs.
                For plotting purposes please create a `netify` object with a single ego,
                and consider just patching (via `patchwork`, for example) the plots
                together.' )
            stop() }
    }
	######################		

    ######################
    # pull out attrs
    obj_attrs <- attributes(netlet)

    # pull out msrmnts
    msrmnts <- netify_measurements(netlet)

    # try to set some smart defaults
    plot_args <- get_smart_defaults(netlet, msrmnts, plot_args)
    ######################

    ######################
    #
    g_list <- netify_to_igraph(netlet, 
        add_nodal_attribs = FALSE, 
        add_dyad_attribs = FALSE)

    # Modify get_node_layout to accept pre-converted igraph
    if(is.null(plot_args$point_layout)){
        nodes_list = get_node_layout(
            netlet=netlet, ig_netlet=g_list,
            layout = plot_args$layout,
            static_actor_positions = plot_args$static_actor_positions,
            which_static = plot_args$which_static,
            seed = plot_args$seed ) }

    # Modify get_edge_layout to use the same igraph
    edges_list = get_edge_layout(
        netlet=netlet, 
        nodes_layout=nodes_list, 
        ig_netlet=g_list )
    ######################

    ######################
    # org the netlet into a  and dyadic df with all the
    # relev attributes so that we can plot
    net_dfs <- merge_layout_attribs( netlet, nodes_list, edges_list)
    ######################  

    ######################  
    # adjust plot args
    out <- adjust_plot_args(plot_args, net_dfs, obj_attrs)
    plot_args <- out$plot_args
    net_dfs <- out$net_dfs
    rm(out)
    ######################  

    ######################  
    # get aesthetic parameters
    ggnet_params = gg_params( plot_args )
    ######################  

	######################	
	# remove isolates
	if(plot_args$remove_isolates){

		# get actor summary
		actor_summ <- summary_actor(netlet)

		# figure out what to keep and need to 
		# do separate for symmetric and asymmetric
		if(obj_attrs$symmetric){
			to_keep <- actor_summ[actor_summ$degree > 0,]
		} else {
			to_keep <- actor_summ[actor_summ$degree_total > 0 ,]}

		# add time var to to_keep if cross_sec
		if(obj_attrs$netify_type == 'cross_sec'){
			to_keep$time = net_dfs$nodal_data$time[1] }

		# create id vars
		to_keep$id = with(to_keep, paste(actor, time, sep='_'))
		net_dfs$nodal_data$id = with(net_dfs$nodal_data, paste(name, time, sep='_'))
		# net_dfs$edge_data$from_id = with(net_dfs$edge_data, paste(from, time, sep='_'))
		# net_dfs$edge_data$to_id = with(net_dfs$edge_data, paste(to, time, sep='_'))

		# subset
		net_dfs$nodal_data <- net_dfs$nodal_data[net_dfs$nodal_data$id %in% to_keep$id,]
	}
	######################		

    ######################  
    # Return all prepared data
    return(
        list(
            plot_args = plot_args, 
            ggnet_params = ggnet_params, 
            net_dfs = net_dfs
            )
        )
    }
    ######################  