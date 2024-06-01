#' Print method for netify objects
#'
#' `tmpprint` takes in a netify object
#' and defines information to be displayed on print.
#'
#' @param x object of class netify, produced by get_adjacency
#' @param ... additional parameters not used
#' @return text information about the network
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'

tmpprint <- function(x, ...){

	#
	netlet <- x ; rm(x)

	# pull out attrs
	objAttrs <- attributes(netlet)

	# pull out msrmnts
	msrmnts <- netify_measurements(netlet)

####################
# needs new weight label

# needs bipartite info
	# when bipartite needs to give information on mode 1 and mode 2 actors separately

# stat information
# when binary needs to just count up number of edges and needs a correction for nas

# when not binary needs to calculate average score instead
# and needs a correction for nas

# needs multilayer info when relevant
	# need to show what variable is in each layer
	# separelty give variable names assigned to layers and then also give longer descriptors right below based on weight_Label attribute

# when longitudinal needs to give average density across time
# periods

# last should give a missing data counter when relevant as well
####################

	# info on netlet type
	obj_time <- ifelse( grepl('longit', objAttrs$netify_type),
		'Longitudinal', 'Cross-Sectional' )
	obj_mode <- ifelse( objAttrs$mode == 'unipartite', 
		'Unipartite', 'Bipartite' )
	obj_layer <- ifelse( length(objAttrs$layers)>1, 
		'Multilayer',  'Single Layer' )
	obj_symm <- ifelse( objAttrs$symmetric, 
		'Symmetric', 'Asymmetric' )

	# get information about cross-sections
	detail_weight_label <- objAttrs$detail_weight
	loops_label <- ifelse( objAttrs$diag_to_NA, 
		'No Loops Allowed', 'Loops Preserved' )

	# pull out msrmnt info about composition of network
	n_layers <- ifelse(is.null(msrmnts$n_layers), 1, msrmnts$n_layers)
	n_time <- ifelse(is.null(msrmnts$n_time), 1, msrmnts$n_time)
	n_row_actors <- msrmnts$n_row_actors
	n_col_actors <- msrmnts$n_col_actors

	# pull out total number of unique actors
	if(objAttrs$netify_type == 'longit_list'){
		n_row_actors <- length(unique(unlist(msrmnts$row_actors)))
		n_col_actors <- length(unique(unlist(msrmnts$col_actors))) }

	# gen labels from msrmnts
	time_label <- ifelse(n_time==1, 
		'Cross-Sectional', paste0('Longitudinal: ', n_time, ' Periods'))
	row_label <- paste0('# Unique Row Actors: ', n_row_actors)
	col_label <- paste0('# Unique Column Actors: ', n_col_actors)
	gen_row_col_label <- paste0('# Unique Actors: ', n_row_actors)

	# org info about ftrs
	nodal_ftrs_label <- ifelse(is.null(msrmnts$nvars), 'Nodal Features: None', paste(msrmnts$nvars, collapse=', '))
	dyad_ftrs_label <- ifelse(is.null(msrmnts$dvars), 'Dyadic Features: None', paste(msrmnts$dvars, collapse=', '))
	graph_ftrs_label <- ifelse(is.null(msrmnts$gvars), 'Graph Features: None', paste(msrmnts$gvars, collapse=', '))


# basic_stats <- 

# convert to raw object
raw_netlet <- get_raw(netlet)

# need this if i'm going to have one fn for all cases
iter_layers <- ifelse(is.null(n_layers), 1, n_layers)

# need some time period counter for iteration purposes
# even when we have cross-sectional data
if(is.null(msrmnts$time)){
	time_pds <- 1 } else { time_pds <- msrmnts$time}

# if cross-sectional then wrap in a list so that 
# we can reuse the code for longitudinal
if( !is.list(raw_netlet) ){
	if( objAttrs$netify_type == 'cross_sec'){
		raw_netlet <- list(raw_netlet) }
	if( objAttrs$netify_type == 'longit_array'){
		if( iter_layers > 1){
			raw_netlet <- lapply(1:n_time, function(tt){
				return(raw_netlet[,,,tt]) })
		}
		if( iter_layers == 1){
			raw_netlet <- lapply(1:n_time, function(tt){
				return(raw_netlet[,,tt]) })
		}
	} 
}
names(raw_netlet) <- time_pds

# convert netlet into a layer length list of 
# time length list of matrices
layer_time_mats <- lapply(1:iter_layers, function(ll){

	time_mats <- lapply(1:n_time, function(tt){

		#######################################
		### first subset every netify type by
		### a particular time period
		tt_raw_netlet <- raw_netlet[[tt]]
		#######################################

		#######################################
		### then subset by layer if they are present
		### in each netify type layers will be organized
		### into the third dim of the object

		# if no layers then there is no third dimension
		if( iter_layers == 1 ){ mat <- tt_raw_netlet }
		
		# if layers then get each layer separately
		if( iter_layers > 1){ mat <- tt_raw_netlet[,,ll] }

		#
		return(mat) })
		#######################################

	# name each time mat with corresponding time period
	names(time_mats) <- time_pds

	#
	return(time_mats) })

# add layer names
names(layer_time_mats) <- msrmnts$layers

# calculat basic stats for every mat, these 
# statistucs are dibe acoss time periods
stats <- lapply(1:length(layer_time_mats), function(ll){
	
	# pull out particular time series of matrices
	# or cross section in non-longit case
	tlmat <- layer_time_mats[[ll]]

	# if loops allowed because diag_to_NA is FALSE
	# then calculate number of loops by pulling out
	# diagonals from each matrix and summing across
	if(loops_label[ll]=='Loops Preserved'){
			nLoops <- Reduce('+', 
			lapply(tlmat, function(x){ sum(diag(x), na.rm=TRUE) }))
	} else { nLoops <- 'N/A'}

	# calculate number of edges (i.e., value greater than 0)
	# by summing across all matrices
	nEdges <- Reduce('+', 
		lapply(tlmat, function(x){ sum(as.vector(x)>0, na.rm=TRUE) }))

	# calculate density by vectorizing the entire list
	# of matrices and taking the mean
	density <- mean(unlist(
		lapply(tlmat, function(x){ as.vector(x) })), na.rm=TRUE)

	# calculate reciprocity for asymmetric case, by
	# vectorizing the list of matrices and the 
	# transposed list of matrices and using cor
	# to calculate correlation
	if(obj_symm=='Asymmetric'){
		recip <- cor(
			unlist(lapply(tlmat, function(x){ as.vector(x) })), 
			unlist(lapply(tlmat, function(x){ as.vector(t(x)) })), 
			use='pairwise.complete.obs')
	} else { recip <- 'N/A' }

	# if missing to zero is not TRUE then calculate
	# number of missing values and dividing by the total, 
	# with an adjustment made for diagonal values depending
	# on the value of diag_to_NA
	if(!attr(netlet, 'missing_to_zero')[ll]){
		if(attr(netlet, 'diag_to_NA')[ll]){
			missCount <- Reduce('+', lapply(tlmat, function(x){ 
				sum(is.na(as.vector(x))) - length(diag(x)) }))
			totCount <- Reduce('+', lapply(tlmat, length))
			propNA <- missCount/totCount			
		} else {
			missCount <- Reduce('+', lapply(tlmat, function(x){ 
				sum(is.na(as.vector(x))) }))
			totCount <- Reduce('+', lapply(tlmat, length))
			propNA <- missCount/totCount }
	} else { propNA <- 'N/A' }

	# organize into a data.frame so we can preserve
	# formatting
	out <- data.frame(nLoops, nEdges, density, recip, propNA)

	# 
	return(out) })

# organize into a layer x stats data.frame
stats <- do.call('rbind', stats)

# add layer labels
stats <- cbind(layers=attr(netlet, 'layers'), stats)

#
cli::cli_alert_success("Hello, you have created network data, yay!")

# org printing
ulid <- cli::cli_ul()

# print out network type info
# cli::cli_li( obj_time )
cli::cli_li(
	paste0(
		'Network Type: ', 
		paste(obj_symm, obj_mode, obj_layer, collapse=', '))
)
# cli::cli_li( obj_mode )
# cli::cli_li( obj_layer )
# cli::cli_li( obj_symm )
# cli::cli_li( detail_weight_label )
# cli::cli_li( loops_label )

# print outnetwork measurements
cli::cli_li(time_label)
cli::cli_li(row_label)
cli::cli_li(col_label)
cli::cli_li(gen_row_col_label)

# print stats
print(stats)

# print info about attributes
cli::cli_li(nodal_ftrs_label)
cli::cli_li(dyad_ftrs_label)
cli::cli_li(graph_ftrs_label)

# end printing
cli::cli_end(ulid)
cli::cli_end()

	# # this should be a seperate function in the end
	# print_netify_tmp <- function(
	# 	symmetric=symmetric, weighted=weight, cross_sec=TRUE,
	# 	nActors=nActors, nEdges=nEdges, nTime=NULL,
	# 	meanActors=meanActors, 
	# 	nodal=nodal_ftrs, dyad=dyad_ftrs, graph=graph_ftrs,
	# 	objAttrs = objAttrs 
	# 	){
	# 		# general message
	# 		cli::cli_alert_success("Hello, you have created network data, yay!")
	# 		ulid <- cli::cli_ul()
	# 		cli::cli_li(paste0("Symmetric: ", if(symmetric=="symmetric"){"Yes"}else{"No"} ))
	# 		cli::cli_li(paste0("Weighted: ", if(weighted=="weighted"){"Yes"}else{"No"} ))
	# 		cli::cli_li(paste0("Number of Unique Actors: ", nActors))
	# 		cli::cli_li(paste0("Number of Edges: ", nEdges))
	# 		cli::cli_li(paste0("Number of Time Points: ", 
	# 			if(!is.null(nTime)){nTime}else{"N/A"}))
	  
	# 		if(!is.na(meanActors)){
	# 			cli::cli_li(
	# 				paste0("Average Number of Actors: ", meanActors)) }
	# 		cli::cli_li(paste0("Nodal Attributes: ", nodal))
	# 		cli::cli_li(paste0("Dyad Attributes: ", dyad))
	# 		cli::cli_li(paste0("Graph Attributes: ", graph))	  	  
	# 		cli::cli_end(ulid)
	# 		cli::cli_end() }
	
	# print
	# cross-sec case
	# if( 
	# 	objAttrs$netify_type == 'cross_sec' 
	# 	){
	# 	print_netify_tmp(
	# 		symmetric=symmetric, weighted=weight,
	# 		nActors=nActors, nEdges=nEdges, nTime=NULL, 
	# 		nodal=nodal_ftrs, dyad=dyad_ftrs, graph=graph_ftrs, 
	# 		meanActors=meanActors
	# 		) }

	# # array or list and actors uniform case
	# if(
	# 	objAttrs$netify_type == 'longit_array' | 
	# 	(objAttrs$netify_type == 'longit_list' & objAttrs$actor_time_uniform)
	# 	){
	# 	print_netify_tmp(
	# 		symmetric=symmetric, weighted=weight,
	# 		nActors=nActors, nEdges=nEdges, nTime=nTime, 
	# 		nodal=nodal_ftrs, dyad=dyad_ftrs, graph=graph_ftrs, 
	# 		meanActors=meanActors
	# 		) }
	
	# # list and actors not uniform case
	# if(
	# 	objAttrs$netify_type == 'longit_list' & !objAttrs$actor_time_uniform
	# 	){
	# 	print_netify_tmp(
	# 		symmetric=symmetric, weighted=weight,
	# 		nActors=nActors, nEdges=nEdges, nTime=nTime, 
	# 		nodal=nodal_ftrs, dyad=dyad_ftrs, graph=graph_ftrs, 
	# 		meanActors=meanActors
	# 		) }

}

