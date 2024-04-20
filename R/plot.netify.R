#' Plot method for netify objects
#'
#' `plot.netify` takes in a netify object
#' and produces a network visualization
#' using the `tidygraph` and `ggraph` packages.
#'
#' @param x object of class netify, produced by netify()
#' @param ... Additional arguments passed to geom functions within 'ggraph'.
#'           This can include aesthetics mappings such as alpha, size, and color,
#'           or any other graphical parameters supported by 'geom_edge_link' and 'geom_node_point'.
#' @return ggraph object
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @import igraph
#' @import ggplot2
#' 
#' @export plot.netify

###notes to sm and cd int he documentation for this or vignette: 
### tel users that these are the igraph options
		# choices = c(
		# 	"nicely", "fruchterman.reingold", 
		# 	"kamada.kawai", "random", "circle", 
		# 	"star", "grid", "graphopt", 
		# 	"sugiyama", "drl", "lgl")

library(netify)
library(igraph)
library(ggplot2)

example(decompose_netlet)
x = netlet

x = subset_netlet(
	x, 
	when_to_subset=c('2008','2009') 
	# when_to_subset=c('2009')
)

plot_args = list(
	layout='kamada.kawai'
)

# layout: user has selected a layout algo from igraph 
# based on the choices in the documentatoin
# layout_matrix : user has made a matrix already inwhich 
# they describe the nodal positions of actors
# static_actors: default is TRUE

# plot.netify <- function(x, ...){

# 	# check if netify object
# 	netify_check(x)	

# 	# get plot args
# 	plot_args = list(...)	

	######################
	#
	netlet <- x ; rm(x)

	# pull out attrs
	obj_attrs <- attributes(netlet)

	# pull out msrmnts
	msrmnts <- netify_measurements(netlet)

	# define default behaviors in plot_args
	if(is.null(plot_args$static_actors)){
		plot_args$static_actors <- FALSE }

	# first step is convert to igraph and
	g = prep_for_igraph(netlet)
	######################

	# get node positions
    # define the layout function based on the user's choice	

	######################
	# first check to see if the user supplied their own
	# layout_matrix
	if(is.null(plot_args$layout_matrix)){

		# check to see if the user has a desired layout
		# algo from igraph, if not default to nicely
		if(is.null(plot_args$layout)){

			# if bipartite then we need to use 
			# bipartite layout otherwise use nicely
			if(obj_attrs$mode == 'bipartite'){
				layout_fun = 'bipartite'
			} else { layout_fun <- 'nicely' }

		# if the user has specified a layout
		# then match it using the code below
		} else {
		layout_fun <- match.arg(
			plot_args$layout, 
			choices = c(
				"nicely", "fruchterman.reingold", 
				"kamada.kawai", "random", "circle", 
				"star", "grid", "graphopt", 
				"sugiyama", "drl", "lgl", 'bipartite') )
		}

		# based on layout choice or default get the relevant
		# layout function from igraph and assign it to layout_fun
		layout_fun <- switch(
			layout_fun,
			nicely = layout_nicely,
			bipartite = layout_bipartite,
			fruchterman.reingold = layout_with_fr,
			kamada.kawai = layout_with_kk,
			random = layout_randomly,
			circle = layout_in_circle,
			star = layout_in_star,
			grid = layout_as_grid,
			graphopt = layout_with_graphopt,
			sugiyama = layout_with_sugiyama,
			drl = layout_with_drl,
			lgl = layout_with_lgl)

		# Calculate node positions using the specified layout
		# if this is a longit list and the user has 
		# specified that they want actor positions to change
		# then create a list of layout matrices
		if(obj_attrs$netify_type != 'cross_sec'){
			layout_matrix <- lapply(g, function(g_slice){
				l_matrix_slice = layout_fun(g_slice)
				rownames(l_matrix_slice) <- V(g_slice)$name
				return(l_matrix_slice) })
			names(layout_matrix) = names(g)
		} else {
			layout_matrix <- layout_fun(g)
			rownames(layout_matrix) <- msrmnts$row_actors }
	} # end of if block to assign layouts
	######################	

	######################
	# if static layout then choose first
	if(plot_args$static_actors){
		layout_matrix = layout_matrix[[1]] }
	######################		

	######################
	# if layout_matrix and g are not lists then
	# wrap them in one so we can have one
	# set of code for getting positions for 
	# longit and cross_sec
	if(!is.list(layout_matrix)){
		layout_matrix = list(layout_matrix)  }
	if(class(g)=='igraph'){
		g = list(g) }

	# set up nodes
	nodes_list = lapply(layout_matrix, function(layout_slice){

		# set up nodes df
		nodes = data.frame(
			index = 1:nrow(layout_slice),
			actor = rownames(layout_slice) )
		nodes = cbind(nodes, layout_slice)
		names(nodes)[3:4] = c("x", "y")

		#
		return(nodes) })

	# set up edges
	edges_list = lapply(1:length(g), function(ii){

		#
		g_slice = g[[ii]]

		# get edges
		edges = get.edgelist(g_slice, names=TRUE)
		edges = data.frame(edges)
		names(edges) = c("from", "to")

		#
		nodes = nodes_list[[ii]]

		# get info to make segments
		edges = cbind(
			edges, 
			nodes[edges$from,c("x", "y")],
			nodes[edges$to,c("x", "y")] )
		names(edges)[3:ncol(edges)] = c("x1", "y1", "x2", "y2")
		rownames(edges) = NULL

		#
		return(edges) })
	names(edges_list) = names(g)
	######################	

	# ######################
	# # set up nodes and edges
	# nodes = data.frame(
	# 	index = 1:nrow(layout_matrix),
	# 	actor = rownames(layout_matrix) )
	# nodes = cbind(nodes, layout_matrix)
	#  names(nodes)[3:4] = c("x", "y")

	# # pull out edgelist from igraph object created in previous section
	# # so edgelist tells us which actor (by index #) is connected to
	# # which other actor. NOTE that the index #s given in edgelist
	# # correspond to the ordering of the actors in the nodes object
	# edges = get.edgelist(g, names=FALSE)
	# edges = data.frame(edges)
	# names(edges) = c("from", "to")
	# edges$from = nodes$actor[match(edges$from, nodes$index)]
	# edges$to = nodes$actor[match(edges$to, nodes$index)]

	# # next, lets create a df called edges, in which we have all edge info and
	# # we pull in the node position info as well
	# edges = cbind(
	# 	edges, 
	# 	nodes[edgelist[,1],c("x", "y")],
	# 	nodes[edgelist[,2],c("x", "y")] )
	# names(edges)[3:ncol(edges)] = c("x1", "y1", "x2", "y2")
	# rownames(edges) = NULL
	# ######################

	######################
	# org the netlet into a  and dyadic df with all the
	# relev attributes so that we can plot
	net_dfs = decompose_netlet( netlet ) 

	# cross_sec
	if(obj_attrs$netify_type == 'cross_sec'){

		# pull out nodes and edges from list format
		nodes = nodes_list[[1]]
		edges = edges_list[[1]]

		# in the nodal part of net_dfs add in the
		# xy pos of actors
		net_dfs$nodal_data = merge(
			net_dfs$nodal_data, nodes, 
			by.x='name', by.y='actor' )
		
		# now do the same for the edge data
		net_dfs$edge_data = merge(
			net_dfs$edge_data, edges, 
			by.x=c('from', 'to'), 
			by.y=c('from', 'to') ) }

	# longit
	if(obj_attrs$netify_type != 'cross_sec'){

		# pull out nodes and edges from list format
		nodes = lapply(1:length(nodes_list), function(ii){
			nodes = nodes_list[[ii]]
			nodes$time = names(nodes_list)[ii]
			return(nodes) })
		nodes = do.call('rbind', nodes)
		edges = lapply(1:length(edges_list), function(ii){
			edges = edges_list[[ii]]
			edges$time = names(edges_list)[ii]
			return(edges) })
		edges = do.call('rbind', edges) 

		# in the nodal part of net_dfs add in the
		# xy pos of actors
		net_dfs$nodal_data = merge(
			net_dfs$nodal_data, nodes, 
			by.x=c('name','time'), by.y=c('actor','time') )
		
		# now do the same for the edge data
		net_dfs$edge_data = merge(
			net_dfs$edge_data, edges, 
			by.x=c('from', 'to', 'time'), 
			by.y=c('from', 'to', 'time') ) }
	######################

	######################
	# plot

# user parameters for plot

# nodes:
	# color
	# size
	# alpha

# edges:
	# alpha
	# color
	# linewidth

	ggplot() +
		geom_point(
			data=net_dfs$nodal_data,
			aes(
				x = x, 
				y = y,
				color = i_polity2,
				size = i_log_pop
			)
		) + 
		geom_text(
			data=net_dfs$nodal_data,
			aes(
				x = x, 
				y = y,
				label = name
			),
			check_overlap = TRUE
		) +
		geom_segment(
			data=net_dfs$edge_data,
			aes(
				x = x1, 
				y = y1,
				xend = x2,
				yend = y2,
				alpha = verbCoop
				# color = factor(matlConfBin)
			),
			size=1.5,
			arrow = arrow(length = unit(0.3, "cm"))
		) +
		facet_wrap(~time) +
		theme_netify()
	######################

}

# }

# 	# get node positions
# 	layout_positions <- create_layout(g, layout = "kk")

#     # decompose netlet
# 	net_dfs = decompose_netlet( netlet )

# 	# convert to tidygraph object
# 	gg = tbl_graph(
# 		nodes = net_dfs$nodal_data, 
# 		edges = net_dfs$edge_data, 
# 		directed = !obj_attrs$symmetric
# 	)

# 	# get plot args
# 	plot_args = list(...)	

# 	# assign layout if none provided
# 	if(!is.null(plot_args$layout)){
# 		plot_args$layout <- 'kk' }


# 	# create the ggraph plot
# 	viz <- ggraph(gg, layout = plot_args$layout)

# 	# initialize empty aesthetic lists
# 	edge_aes <- aes()
# 	node_aes <- aes()

# 	# dynamically add edge aesthetics
# 	if (!is.null(plot_args$edge_alpha_var)) {
# 		edge_aes <- modifyList(edge_aes, aes(alpha = !!sym(plot_args$edge_alpha_var))) }

# 	# dynamically add node aesthetics
# 	if (!is.null(plot_args$node_size_var)) {
# 		node_aes <- modifyList(node_aes, aes(size = !!sym(plot_args$node_size_var))) }
# 	if (!is.null(plot_args$node_color_var)) {
# 		node_aes <- modifyList(node_aes, aes(color = !!sym(plot_args$node_color_var))) }

# 	# Add edges to the plot using do.call
# 	viz <- viz + do.call(
# 		geom_edge_link, list(mapping = edge_aes))

# 	# Add nodes to the plot using do.call
# 	viz <- viz + do.call(
# 		geom_node_point, list(mapping = node_aes))

# 	# decide whether or not to facet
# 	# based on netify_type
# 	if( obj_attrs$netify_type != 'cross_sec' ){
# 		viz = viz + 
# 			facet_edges(~time) +
# 			facet_nodes(~time) }

# 	# finalize the plot
# 	viz = viz + theme_netify()

# 	#
# 	return(viz)
# }


#' theme_netify function
#'
#' This function returns a customized theme for netify plots.
#' It is based on the `theme_minimal` function from the `ggplot2` package.
#' It removes axis text and titles from the plot.
#'
#' @return A customized theme object for netify plots.
#' @author Cassy Dorff, Shahryar Minhas
#' @import ggplot2
#' @export theme_netify
#'
#'
theme_netify = function(){
	theme_minimal() + 
	theme(
		axis.text = element_blank(),
		axis.title = element_blank(),
		legend.position='top'
	)
}

# library(netify)

# example(decompose_netlet)

# netify_measurements(
# 	netlet
# )$row_actors

# cntries = c(
# 	'United States', 'China', 'Russian Federation', 'India', 'Germany', 'France', 'United Kingdom'
# )

# sub_net = subset_netlet(
# 	netlet, 
# 	what_to_subset=cntries,
# 	when_to_subset=c('2008')
# )

# plot.netify(
# 	sub_net,
# 	layout='kk',
# 	node_size_var='i_log_gdp',
# 	node_color_var='i_polity2',
# 	edge_alpha_var='verbCoop'
# 	)

# # netlet = sub_net

# plot_args = list(
# 	layout='kk',
# 	node_size_var='i_log_gdp',
# 	node_color_var='i_polity2',
# 	edge_alpha_var='verbCoop'
# )

# sub_08 = subset_netlet(
# 	netlet, 
# 	when_to_subset=c('2008')
# )

# sub_09 = subset_netlet(
# 	netlet, 
# 	when_to_subset=c('2009')
# )

# library(gridExtra)


# viz08 = plot.netify(
# 	sub_net,
# 	layout='kk',
# 	node_size_var='i_log_gdp',
# 	node_color_var='i_polity2',
# 	edge_alpha_var='verbCoop'
# 	)


# viz08 = plot.netify(
# 	sub_08,
# 	layout='kk',
# 	node_size_var='i_log_gdp',
# 	node_color_var='i_polity2',
# 	edge_alpha_var='verbCoop'
# 	)

# viz09 = plot.netify(
# 	sub_09,
# 	layout='kk',
# 	node_size_var='i_log_gdp',
# 	node_color_var='i_polity2',
# 	edge_alpha_var='verbCoop'
# 	)

# grid.arrange(viz08, viz09, ncol=1)

# library(tidygraph)
# library(ggraph)
# library(dplyr)

# # Sample data
# nodes <- tibble(
#   id = 1:4,
#   name = c("A", "B", "C", "D"),
#   group = c(1, 2, 1, 2),
#   time = c(1, 1, 2, 2)  # Time attribute for nodes
# )

# edges <- tibble(
#   from = c(1, 2, 3, 1),
#   to = c(2, 3, 4, 3),
#   type = c("X", "Y", "X", "Y"),
#   time = c(1, 2, 1, 2)  # Time attribute for edges
# )

# # Create tbl_graph
# graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

# # Use tidygraph to add node time to edges
# graph <- graph %>%
#   activate(edges) %>%
#   left_join(as_tibble(activate(graph, nodes)), by = c("from" = "id")) %>%
#   mutate(edge_facet = paste(time.x, time.y, sep = "-")) %>%
#   select(-time.x, -time.y, -group)

# # Plotting with ggraph
# ggraph(graph, layout = 'fr') +
#   geom_edge_link(aes(color = type), arrow = arrow(type = "closed", length = unit(4, "mm")), end_cap = circle(3, 'mm')) +
#   geom_node_point(aes(color = factor(group))) +
#   facet_wrap(~edge_facet) +
#   theme_minimal()






# # manual walk through
# # pull out attrs
# obj_attrs <- attributes(sub_net)
# # decompose netlet
# net_dfs = decompose_netlet( sub_net )

# net_dfs$nodal_data$time = as.numeric(net_dfs$nodal_data$time)
# net_dfs$edge_data$time = as.numeric(net_dfs$edge_data$time)

# # convert to tidygraph object
# library(tidygraph)
# library(ggraph)
# gg = tbl_graph(
# 	nodes = net_dfs$nodal_data, 
# 	edges = net_dfs$edge_data, 
# 	directed = !obj_attrs$symmetric
# )

# # First, ensure that node indices are properly set for joining
# gg <- gg %>%
#   activate(nodes) %>%
#   mutate(node_id = row_number())  # Ensure there's a column to join on that matches the edge indices

# # Now join node attributes to edges considering the time attribute
# gg <- gg %>%
#   activate(edges) %>%
#   left_join(as_tibble(activate(gg, nodes)), by = c("from" = "node_id", "time" = "time")) %>%
#   left_join(as_tibble(activate(gg, nodes)), by = c("to" = "node_id", "time" = "time"), suffix = c(".from", ".to"))


# # Plotting with ggraph
# ggraph(gg, layout = 'fr') +
# 	geom_edge_link(aes(alpha=verbCoop)) +
# 	geom_node_point(aes(size=i_log_gdp, color=i_polity2)) +
# 	geom_node_text(aes(label = name),  colour = 'black', show.legend = FALSE, repel=TRUE)	 +
#   facet_wrap(~time) +
#   scale_color_viridis_d() +
#   theme_minimal()

# gg <- gg %>%
#   activate(edges) %>%
#   left_join(as_tibble(activate(gg, nodes)), by = c("from" = "row_number", "time" = "time")) %>%
#   left_join(as_tibble(activate(gg, nodes)), by = c("to" = "row_number", "time" = "time"))

# # Use tidygraph to add node time to edges
# gg <- gg %>%
#   activate(edges) %>%
#   left_join(as_tibble(activate(graph, nodes)), by = c("from" = "name")) %>%
#   mutate(edge_facet = paste(time.x, time.y, sep = "-")) %>%
#   select(-time.x, -time.y, -group)

# ggraph(gg, layout = 'kk') +
# 	geom_edge_link(aes(alpha=verbCoop)) +
# 	geom_node_point(aes(size=i_log_gdp, color=i_polity2)) +
# 	geom_node_text(aes(label = name),  colour = 'black', show.legend = FALSE, repel=TRUE)	 +
# 	theme_netify() +
# 	# facet_graph(time~time)
# 	facet_wrap(~time) +
# 	facet_nodes(~time) +
# 	theme_netify()



############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
############################################
# library(dplyr)


# ggraph(gg, layout='kk') +
# 	geom_edge_link(aes(alpha=matlCoop)) + 
# 	geom_node_point(aes(size=i_log_gdp, color=i_polity2)) +
# 	geom_node_text(aes(label = name),  colour = 'black', show.legend = FALSE, repel=TRUE)	 +
# 	facet_edges(~time) +
# 	facet_nodes(~time) +
# 	theme_netify()

# ############# build data for graph
# library(netify)

# data(icews)

# nvars = c( 'i_polity2', 'i_log_gdp', 'i_log_pop' )
# dvars = c( 'matlCoop', 'verbConf', 'matlConf' )
# dvars_all = c( 'verbCoop', 'matlCoop', 'verbConf', 'matlConf' )

# netlet = netify(
# 	dyad_data=icews, actor1='i', actor2='j',
# 	time = 'year',
# 	symmetric=FALSE, weight='verbCoop',
# 	mode='unipartite', sum_dyads=FALSE,
# 	actor_time_uniform=TRUE, actor_pds=NULL,
# 	diag_to_NA=TRUE, missing_to_zero=TRUE,
# 	nodal_vars = nvars, 
# 	dyad_vars = dvars
# )

# # ################################
# # ################################
# # ################################
# # netlet = netify(
# # 	dyad_data=icews, actor1='i', actor2='j',
# # 	time = NULL, 
# # 	symmetric=FALSE, weight='verbCoop',
# # 	mode='unipartite', sum_dyads=TRUE,
# # 	actor_time_uniform=TRUE, actor_pds=NULL,
# # 	diag_to_NA=TRUE, missing_to_zero=TRUE,
# # 	nodal_vars = nvars, 
# # 	dyad_vars = dvars
# # )

# # # create average versions of nodal data to add
# # node_avg = icews %>%
# # 	select(i, i_polity2, i_log_gdp, i_log_pop) %>%
# # 	group_by(i) %>%
# # 	summarize_all(mean, na.rm=TRUE) %>%
# # 	data.frame()

# # # create average versions of dyad data to add
# # dyad_avg = icews %>%
# # 	select(
# # 		i, j, verbCoop, 
# # 		matlCoop, verbConf, matlConf) %>%
# # 	group_by(i, j) %>%
# # 	summarize_all(mean, na.rm=TRUE) %>%
# # 	data.frame()

# # # merge avg node and dyad to netlet
# # netlet = add_nodal(
# # 	netlet, node_avg, actor='i')
# # netlet = add_dyad(
# # 	netlet, dyad_avg, actor1='i', actor2='j',
# # 	dyad_vars_symmetric=rep(FALSE, ncol(dyad_avg)))
# # ################################
# # ################################
# # ################################

# # edge data
# edge_data = reshape2::melt( unnetify( netlet ) )
# edge_data = edge_data[edge_data$Var1 != edge_data$Var2, ]
# names(edge_data)[3] = attr(netlet, 'weight')

# # other dyad data
# if( !is.null(attr(netlet, 'dyad_data'))){

# 	# melt dyad data
# 	dyad_data = reshape2::melt( attr(netlet, 'dyad_data') )
# 	dyad_data = dyad_data[dyad_data$Var1 != dyad_data$Var2, ]

# 	# spread vars
# 	dyad_data = reshape2::dcast( 
# 		dyad_data, Var1 + Var2 + L1 ~ Var3, value.var='value' )
	
# 	# remove vars in dyad_data that are already in 
# 	# edge_data except id vars if necessary
# 	to_drop = setdiff(
# 		intersect(
# 			names(dyad_data), names(edge_data)),
# 		merge_by_vars)
# 	if(length(to_drop)>0){
# 		dyad_data = dyad_data[,-which(
# 			names(dyad_data) %in% to_drop)] }

# 	# merge to edge_data
# 	# choose how to attrib data to edge_data 
# 	# based on netify_type
# 	if (attr(netlet, 'netify_type') == 'cross_sec') {
# 		merge_by_vars <- c('Var1', 'Var2')
# 	} else {
# 		merge_by_vars <- c('Var1', 'Var2', 'L1') }	
# 	edge_data = merge(
# 		edge_data, dyad_data, by=merge_by_vars )
# }

# # other nodal data
# if( !is.null(attr(netlet, 'nodal_data'))){
# 	nodal_data = attr(netlet, 'nodal_data') }

# # reorder vars
# vars = c('Var1', 'Var2', 'L1')
# vars = c(vars, setdiff(
# 	names(edge_data), vars))
# edge_data = edge_data[,vars]

# # add time if missing in nodal_data
# if(!'time' %in% names(nodal_data)){
# 	nodal_data = cbind(
# 		nodal_data[,1], 
# 		time = rep(1, nrow(nodal_data)),
# 		nodal_data[,2:ncol(nodal_data)]
# 	)
# }

# # relabel id cols
# names(edge_data)[1:3] = c('from', 'to', 'time')
# names(nodal_data)[1:2] = c('name', 'time')

# # convert to tbl_graph
# gg = tbl_graph(
# 	nodes = nodal_data, 
# 	edges = edge_data, 
# 	directed = !attr(netlet, 'symmetric')
# )
# ############# build data for graph




# #
# g_full = tbl_graph(
# 	nodes = nodal_data, 
# 	edges = dv, 
# 	directed = !attr(netlet, 'symmetric')
# )

# g_full = g_full %>% 
# 	activate(nodes) %>% 
# 	filter(time %in% 2008:2009) %>% 
# 	activate(edges) %>% 
# 	filter(time %in% 2008:2009)

# gg = ggraph(g_full, layout='kk') +
# 	geom_edge_link(aes(alpha=matlCoop)) + 
# 	geom_node_point(aes(size=i_log_gdp, color=i_polity2)) + 
# 	facet_edges(~time) +
# 	theme_minimal()
# gg

# ##
# cntries = c(
# 	'United States', 'China', 'Russian Federation', 'India', 'Germany', 'France', 'United Kingdom' 
# )

# #
# dv = dv[dv$from %in% cntries & dv$to %in% cntries, ]
# nodal_data = nodal_data[nodal_data$name %in% cntries, ]

# #
# dv = dv[dv$time %in% 2008:2009, ]
# nodal_data = nodal_data[nodal_data$time %in% 2008:2009, ]

# g_full = tbl_graph(
# 	nodes = nodal_data, 
# 	edges = dv, 
# 	directed = !attr(netlet, 'symmetric')
# )


# gg = ggraph(g_full, layout='kk') +
# 	geom_edge_link(aes(alpha=matlCoop)) + 
# 	geom_node_point(aes(size=i_log_gdp, color=i_polity2)) +
# 	geom_node_text(aes(label = name),  colour = 'black', show.legend = FALSE, repel=TRUE)	 +
# 	facet_edges(~time) +
# 	facet_nodes(~time) +
# 	theme_minimal()
# gg

# ### static layout
# # Calculate node positions using Kamada-Kawai layout
# layout_positions <- create_layout(g_full, layout = "kk")

# # Add positions to the graph attributes
# g_full <- g_full %>%
#   activate(nodes) %>%
#   mutate(x = layout_positions$x, y = layout_positions$y)


# # first step in making plot is to take the info 
# # in the netlet and convert it into 
# # data for ggplot


# # library(netify)

# # # load example directed event data from ICEWS
# # # this data comes in the form of a dyadic
# # # dataframe where all dyad pairs are listed
# # data(icews)

# # # subset to a particular year
# # icews <- icews[icews$year=='2010', ]

# # # generate a cross sectional, directed, and weighted network
# # # where the weights are verbCoop
# # icews_verbCoop <- get_adjacency(
# #   dyad_data=icews, actor1='i', actor2='j',
# #   symmetric=FALSE, weight='verbCoop' )

# # #
# # library(tidygraph)
# # library(ggraph)fff

# # #
# # ig = prep_for_igraph(icews_verbCoop)

# # # 
# # g = as_tbl_graph(ig)

# # #
# # ggraph(g, layout='kk') +
# #     geom_edge_fan(aes(alpha=after_stat(index)), show.legend=FALSE)

# # ####







# # #############multilayer build data for graph
# # # from scratch.R in netify_test_files
# # netlet = icews_longit_list_layers

# # # dv data
# # dv = reshape2::melt( unnetify( netlet ) )
# # dv = dv[dv$Var1 != dv$Var2, ]
# # # names(dv)[3] = attr(netlet, 'weight')

# # # other dyad data
# # if( !is.null(attr(netlet, 'dyad_data'))){

# # 	# melt dyad data
# # 	dyad_data = reshape2::melt( attr(netlet, 'dyad_data') )
# # 	dyad_data = dyad_data[dyad_data$Var1 != dyad_data$Var2, ]

# # 	# spread vars
# # 	dyad_data = reshape2::dcast( 
# # 		dyad_data, Var1 + Var2 + L1 ~ Var3, value.var='value' )

# # 	# merge to dv
# # 	dv = merge(dv, dyad_data, by=c('Var1', 'Var2', 'L1') )

# # 	#
# # 	dv$L1 = num(dv$L1)
# # }

# # # other nodal data
# # if( !is.null(attr(netlet, 'nodal_data'))){
# # 	nodal_data = attr(netlet, 'nodal_data')
# # }

# # #
# # names(dv)[1:3] = c('from', 'to', 'time')
# # names(nodal_data)[1:2] = c('name', 'time')#############multilayer build data for graph