#' Plotting method for 'netify' objects
#'
#' This function provides a comprehensive tool to visualize 'netify' objects through various graphical representations including points, edges, texts, and labels. It leverages the capabilities of the 'igraph' and 'ggplot' packages to create customizable network visualizations.
#'
#' @param x A 'netify' object, which contains the network data structured for analysis and visualization.
#' @param ... Additional arguments, which can include but are not limited to:
#'        - `point_layout`: Optional, user-provided node layout; if not provided, layout will be generated based on `layout` parameter.
#'        - `layout`: Specifies the layout algorithm from 'igraph' to position the nodes if `point_layout` is not provided.
#'        - `remove_isolates`: Logical; if TRUE, isolates will be removed from the plot. Default is TRUE.
#'        - `static_actor_positions` Logical indicating whether to use static positions for actors.
#'        Useful in longitudinal studies where node positions should remain consistent over time.
#'        If TRUE, the layout by default is calculated based on a collapsed adjacency matrix across 
#'        all time points. Users can also specify a specific time point to use as the static layout by 
#'        setting `which_static` to the desired time point. 
#'        - `add_edges`: Logical; if TRUE, edges will be added to the plot. Default is TRUE.
#'        - `curve_edges`: Logical; if TRUE, edges will be curved. Default is FALSE.
#'        - `add_points`: Logical; if TRUE, points (nodes) will be plotted. Default is TRUE.
#'        - `add_text`: Logical; if TRUE, text annotations will be added. Default is FALSE.
#'        - `add_label`: Logical; if TRUE, labels will be added. Default is FALSE.
#'        - `select_text`: A vector of node names to specifically add text to; others will not have text.
#'        - `select_label`: A vector of node names to specifically add labels to; others will not have labels.
#'        - `point_size`: A fixed size for all points, equivalent to `geom_point(size = point_size)` in `ggplot2`.
#'        - `point_size_var`: A variable from the node data to dynamically size points, equivalent to `geom_point(aes(size = point_size_var))` in `ggplot2`.
#'        These arguments control various aspects of the plot's appearance and functionality.
#'
#' @return A 'ggplot' plot object that can be further modified or printed.
#'
#' @author Cassy Dorff, Shahryar Minhas
#' 
#'
#' @examples
#' # load icews data
#' data(icews)
#' 
#' # choose attributes
#' nvars = c( 'i_polity2', 'i_log_gdp', 'i_log_pop' )
#' dvars = c( 'matlCoop', 'verbConf', 'matlConf' )
#'
#' # create a netify object
#' netlet = netify(
#'     dyad_data=icews, actor1='i', actor2='j',
#'     time = 'year',
#'     symmetric=FALSE, weight='verbCoop',
#'     mode='unipartite', sum_dyads=FALSE,
#'     actor_time_uniform=TRUE, actor_pds=NULL,
#'     diag_to_NA=TRUE, missing_to_zero=TRUE,
#'     nodal_vars = nvars, 
#'     dyad_vars = dvars
#' )
#'
#' # add basic summary network stats
#' netlet = add_nodal(
#'     netlet, 
#'     summary_actor(netlet),
#'     actor='actor', time='time')
#' 
#' # plot the network
#' plot(netlet, 
#'      edge_color='lightgrey', 
#'      point_size_var = 'degree_total',
#'      point_color_var = 'i_polity2'
#'      )
#' 
#' @import ggplot2
#' @importFrom ggnewscale new_scale_color new_scale_fill new_scale 
#' 
#' @export net_plot
#' @export



net_plot <- function(x, ...){

	######################			
    # get data for viz
    netify_data = net_plot_data(
        netlet=x, plot_args = list(...) )
    ggnet_params = netify_data$ggnet_params
    plot_args = netify_data$plot_args
    net_dfs = netify_data$net_dfs
    rm(netify_data)
	######################			    

	######################	
	# plot
	viz = ggplot()
	######################	

	# facet instructions #####################
	if(obj_attrs$netify_type!='cross_sec'){
		viz = viz + facet_wrap(~time, scales='free')
	}
	######################			

	# theme changes #####################
	if(plot_args$use_theme_netify){
		viz = viz + theme_netify()
	}
	######################			

	######################			
	return(viz)
	######################			    

}