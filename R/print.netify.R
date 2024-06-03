#' Print method for netify objects
#'
#' `print.netify` takes in a netify object
#' and defines information to be displayed on print.
#'
#' @param x object of class netify, produced by get_adjacency
#' @param ... additional parameters not used
#' @return text information about the network
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @importFrom utils capture.output
#' 
#' @export print.netify
#' @export

print.netify <- function(x, ...){

	######################
	netlet <- x ; rm(x)

	# pull out attrs
	objAttrs <- attributes(netlet)

	# pull out msrmnts
	msrmnts <- netify_measurements(netlet)
	######################

	######################
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
	nodal_ftrs_label <- paste0('Nodal Features: ', ifelse(
		is.null(msrmnts$nvars), 
		'None', paste(msrmnts$nvars, collapse=', ')))
	dyad_ftrs_label <- paste0('Dyad Features: ', ifelse(
		is.null(msrmnts$dvars), 
		'None', paste(msrmnts$dvars, collapse=', ')))
	graph_ftrs_label <- paste0('Graph Features: ', ifelse(
		is.null(msrmnts$gvars), 
		'None', paste(msrmnts$gvars, collapse=', ')))
	######################

	######################
	# get graph summary stats
	summ_stats = summary(netlet)

	# ids
	ids = intersect(c('net', 'layer'), names(summ_stats))

	# stats of interest
	to_keep_poss = c(
		'density', 'prop_edges_missing', 
		'mean_edge_weight', 
		# 'sd_edge_weight',
		# 'median_edge_weight',
		# 'min_edge_weight', 'max_edge_weight', 
		'reciprocity', 'transitivity' )
	to_keep_clean_poss = c(
		'dens', 'miss', 'mean', 
		# 'stdev', 
		# 'median',
		# 'min', 'max',
		'recip', 'trans' )
	to_keep = intersect(to_keep_poss, names(summ_stats))
	to_keep_clean = to_keep_clean_poss[match(to_keep, to_keep_poss)]

	# subset and organize
	summ_stats = summ_stats[,c(ids, to_keep)]

	# add label for net if layer not present
	if(!'layer' %in% names(summ_stats)){
		summ_stats = cbind(
			layer=attr(netlet, 'layers'), summ_stats ) }

	# if longitudinal then avg across time
	summ_stats = lapply(unique(summ_stats$layer), function(layer){
		slice = summ_stats[summ_stats$layer==layer, to_keep]
		names(slice) = to_keep_clean
		slice = data.frame( t(colMeans(slice, na.rm=TRUE)) )
		slice = round(slice, 3)
		slice = cbind(layer=layer, slice)
		return(slice) })
	summ_stats = do.call('rbind', summ_stats)

	# cleanup
	rownames(summ_stats) = summ_stats$layer
	summ_stats = summ_stats[,-1]		
	######################

	######################
	# print out network type info
	cli::cli({
		cli::cli_alert_success("Hello, you have created network data, yay!")

		# org printing
		ulid <- cli::cli_ul()

		# print out network type info
		cli::cli_li( obj_time )
		cli::cli_li( obj_mode )
		if(n_layers>1){ cli::cli_li( obj_layer ) }
		cli::cli_li( obj_symm )
		cli::cli_li( detail_weight_label )

		# print out network measurements
		cli::cli_li(time_label)
		cli::cli_li(row_label)
		cli::cli_li(col_label)
		cli::cli_li(gen_row_col_label)

		# print stats
		cli::cli_text("Network Summary Statistics:")
		# tbl <- knitr::kable(summ_stats, align = "r", format = "pandoc")
		tbl = capture.output(print(summ_stats, right = TRUE))
		cli::cli_verbatim(tbl)

		# print info about attributes
		cli::cli_li(nodal_ftrs_label)
		cli::cli_li(dyad_ftrs_label)
		# cli::cli_li(graph_ftrs_label)

		# end printing
		cli::cli_end(ulid)
	})
	######################
}
