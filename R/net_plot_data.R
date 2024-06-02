#' Data preparation for plotting 'netify' objects
#'
#' This function prepares the necessary data components for visualizing 'netify' objects. 
#' It processes network attributes and setups up layout and aesthetic parameters for subsequent plotting.
#'
#' @param netlet A 'netify' object, which contains the network data structured for analysis and visualization.
#' @param plot_args A list of arguments that influence the layout and presentation of the network visualization, including:
#'        - `point_layout`: Optional, user-provided node layout; if not provided, layout will be generated based on `layout` parameter.
#'        - `layout`: Specifies the layout algorithm from 'igraph' to position the nodes if `point_layout` is not provided. Available options include "nicely", "fruchterman.reingold", "kamada.kawai", "random", "circle", "star", "grid", "graphopt", "sugiyama", "drl", "lgl", "bipartite", "tree", "randomly", "dh", "fr", "kk", "gem", and "mds".
#'        - `remove_isolates`: Logical; if TRUE, isolates will be removed from the plot. Default is TRUE.
#'        - `static_actor_positions`: Logical indicating whether to use static positions for actors.
#'        - `add_edges`: Logical; if TRUE, edges will be added to the plot. Default is TRUE.
#'        - `curve_edges`: Logical; if TRUE, edges will be curved. Default is FALSE.
#'        - `add_points`: Logical; if TRUE, points (nodes) will be plotted. Default is TRUE.
#'        - `add_text`: Logical; if TRUE, text annotations will be added. Default is FALSE.
#'        - `add_label`: Logical; if TRUE, labels will be added. Default is FALSE.
#'        - `select_text`: A vector of node names to specifically add text to; others will not have text.
#'        - `select_label`: A vector of node names to specifically add labels to; others will not have labels.
#'
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @return A list containing `plot_args`, `ggnet_params`, and `net_dfs` which are used for setting up the plot:
#'         - `plot_args`: Adjusted plotting arguments including layout and graphical settings.
#'         - `ggnet_params`: Graphical parameters organized for nodes, edges, and labels.
#'         - `net_dfs`: Data frames for nodes and edges prepared for plotting.
#'
#' 
#' @importFrom cli cli_alert_danger
#' 
#' @export net_plot_data
#' @export

net_plot_data <- function(netlet, plot_args=list()) {

	######################
    # check if netify object
    netify_check(netlet)  

    # if more than one layer tell user they must specify a single layer
    if(length(attributes(netlet)$layers) > 1){
    cli::cli_alert_danger(
        'Error: This object has multiple layers. 
        `plot` does not currently support multilayer `netify` inputs.
        Please use the subset_netlet` function to create a `netify` object with a single layer.' )
    stop() }
	######################    

    ######################
    # pull out attrs
    obj_attrs <- attributes(netlet)

    # pull out msrmnts
    msrmnts <- netify_measurements(netlet)
    ######################

    ######################
    # get layouts of nodes after checking
    # whether user has supplied their own
    if(is.null(plot_args$point_layout)){
    nodes_list = get_node_layout(
        netlet, 
        layout=plot_args$layout, 
        static_actor_positions=plot_args$static_actor_positions,
        which_static=plot_args$which_static,
        seed=plot_args$seed
        )
    } else { 
    nodes_list = plot_args$point_layout
    if(!is.list(nodes_list)){
        nodes_list = list(nodes_list)
    }
    }

    # get info for drawing edge segments
    edges_list = get_edge_layout(
    netlet,  nodes_layout=nodes_list )
    ######################      

    ######################
    # org the netlet into a  and dyadic df with all the
    # relev attributes so that we can plot
    net_dfs <- merge_layout_attribs(
    netlet, nodes_list, edges_list)
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