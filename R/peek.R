#' peek method for netify objects
#'
#' peek takes in a netify object
#' and displays the some specified rows of information.
#'
#' @param netlet object of class netify
#' @param what_to_peek enter the name of specific nodes to peek in character vector form or provide a numeric range, default is to show the first three rows and columns of interactions
#' @param what_rows_to_peek similar as what_to_peek but specific to rows, default value is set to what_to_peek. If you want to peek at all rows then set this to NULL. 
#' @param what_cols_to_peek similar as what_to_peek but specific to columns, default value is set to what_to_peek. If you want to peek at all columns then set this to NULL.
#' @param when_to_peek choose time points to peek from, default is to show the first time point of data. 
#' If the entry is a numeric value or vector then it will be used as an index to the time dimension. 
#' If the entry is a character vector then it will be used to match the time dimension labels.
#' If you want to peek at all time points then set this to NULL.
#' @param what_layers_to_peek if the netlet object has multiple layers, then you must choose one layer to peek at.
#' @return slice of the network
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @examples
#'
#' # load data
#' data(icews)
#' 
#' # subset to a particular year
#' icews_10 <- icews[icews$year=='2010', ]
#' 
#' # gen netify object
#' icews_verbCoop <- netify(
#'   dyad_data=icews_10, actor1='i', actor2='j',
#'   symmetric=FALSE, weight='verbCoop' )
#' 
#' # peek at relations between a few countries
#' peek(icews_verbCoop,
#' 	what_to_peek = c('United Kingdom', 'United States','France') )
#' 
#' # specify rows and cols to peek at
#' peek(icews_verbCoop,
#' 	what_rows_to_peek = c('United Kingdom', 'United States','France'),
#' 	what_cols_to_peek = c('Russian Federation', 'Sri Lanka') )
#' 
#' # peek with longit array
#' icews_matlConf <- netify(
#' 	dyad_data=icews, 
#' 	actor1='i', actor2='j', time='year',
#' 	symmetric=FALSE, weight='matlConf',
#' 	output_format = 'longit_array' )
#' # peek at a few years for the first three rows/cols, 
#' # specify numeric index or character refs
#' peek(icews_matlConf, when_to_peek=c(1, 5, 10))
#' peek(icews_matlConf, when_to_peek=c('2002', '2006', '2011'))
#' 
#'
#' @export peek

peek <- function(
	netlet, 
	what_to_peek=3,
	what_rows_to_peek=what_to_peek,
	what_cols_to_peek=what_to_peek,
	when_to_peek=1,
	what_layers_to_peek=NULL
	){

    # user input checks
    netify_check(netlet)

	# pull out attrs
	objAttrs <- attributes(netlet)

	# get measurements
	msrmnts <- netify_measurements(netlet)

	# set n_layers in msrmnts to 1 if set to NULL
	if(is.null(msrmnts$n_layers)){ msrmnts$n_layers <- 1 }

	# get character vector of layers to keep or subset
	multilayer_logic_input <- msrmnts$n_layers > 1
	if(is.null(what_layers_to_peek)){

		# if no subsetting then type in terms of multilayer or not
		# is same in input and output
		multilayer_logic_out <- multilayer_logic_input

	} else {

		# if user specifies layers then check if resulting number of 
		# layers will constitute a multilayer object
		multilayer_logic_out <- length(what_layers_to_peek) > 1
		layer_labels <- what_layers_to_peek

		# stop if they supply an argument to what_layers_to_peek but netlet is not multilayer
		if(!multilayer_logic_input){
			cli::cli_alert_danger("You have supplied an argument to what_layers_to_peek but the netlet object is not multilayer. Please remove the argument to what_layers_to_peek or provide a multilayer netlet object." )
			stop() }

		# make sure that user specified layers exist
		layer_diffs = setdiff(layer_labels, msrmnts$layers)
		if(length(layer_diffs) > 0){
			cli::cli_alert_danger(
				paste0("The following specified layers do not exist in the object: ", paste(layer_diffs, collapse=", "), ". You can check what layers exist in your object using the following code: attr(netlet, 'layers'), where netlet should be substituted with the name of your netify object." ) )
			stop() }
	}

	# org when_to_peek info if longit data supplied
	if(objAttrs$netify_type %in% c('longit_list', 'longit_array')){

		# get time label
		if(objAttrs$netify_type == 'longit_list'){ time_labels <- msrmnts$time }
		if(objAttrs$netify_type == 'longit_array'){ time_labels <- msrmnts$time }

		# if null supplied to when_to_peek create 
		# numeric range corresponding to length of data
		if(is.null(when_to_peek) & objAttrs$netify_type == 'longit_list'){ 
			when_to_peek <- 1:length(netlet) }
		if(is.null(when_to_peek) & objAttrs$netify_type == 'longit_array' & !multilayer_logic_input){ 
			when_to_peek <- 1:(dim(netlet)[3]) }
		if(is.null(when_to_peek) & objAttrs$netify_type == 'longit_array' & multilayer_logic_input){ 
			when_to_peek <- 1:(dim(netlet)[4]) }

		# org info about when_to_peek
		# if character entry then match to numeric index of time dim
		if(is.character(when_to_peek)){
			when_to_peek <- match(when_to_peek, time_labels) }
	}

	# if null supplied to any of the what_ arguments then 
	# create numeric vectors corresponding to the size of the data
	if(is.null(what_rows_to_peek)){
		if(objAttrs$netify_type %in% c('cross_sec', 'longit_array')){
			what_rows_to_peek <- 1:nrow(netlet) }
		if(objAttrs$netify_type == 'longit_list'){
			what_rows_to_peek <- lapply(netlet, function(x){ 1:nrow(x) }) } }

	# similarly for cols
	if(is.null(what_cols_to_peek)){
		if(objAttrs$netify_type %in% c('cross_sec', 'longit_array')){
			what_cols_to_peek <- 1:ncol(netlet) }
		if(objAttrs$netify_type == 'longit_list'){
			what_cols_to_peek <- lapply(netlet, function(x){ 1:ncol(x) }) } }

	# org row and col specs into a list
	row_col <- list(
		rows=what_rows_to_peek, 
		cols=what_cols_to_peek)

	# more cleanup on user inputs to what_ arguments
	# if numeric entry and it's a single entry then 
	# adjust to provide a range
	row_col <- lapply(row_col, function(x){
		if(is.numeric(x) & length(x)==1){ x <- 1:x} else{ x } })

	# if character entry then we need to determine the row/col 
	# indices of where actors fall
	row_col <- lapply(1:2, function(ii){
		
		# index a row_col element
		x <- row_col[[ii]]

		# if user supplied a factor convert to character
		if(is.factor(x)){ x <- as.character(x) }

		# if character entry
		if(is.character(x)){

			# case of cross_sec and longit_array are straightforward
			# because we can just match the labels from the dimlabels
			if(objAttrs$netify_type %in% c('cross_sec', 'longit_array')){
				x <- match(x, dimnames(netlet)[[ii]]) }

			# case of longit_list requires us to iterate through
			# the list of mats and match by time period given actors 
			# can change
			if(objAttrs$netify_type == 'longit_list'){
				x <- lapply(when_to_peek, function(tt){
					match( x, dimnames(netlet[[tt]])[[ii]] ) } ) }
		} # end if character

		#
		return(x) } )

	# get raw version of netlet to help with subsetting
	netlet <- get_raw(netlet)

	# if cross sec print specified rows and cols
	if(objAttrs$netify_type == 'cross_sec'){

		# make sure rows and cols selected dont exceed dims of data
		row_col <- lapply(1:2, function(ii){
			return( intersect(row_col[[ii]], 1:dim(netlet)[ii]) ) })

		# so first check if the input is multilayer if not then just subset rows and cols
		if(!multilayer_logic_input){
			out = netlet[ row_col[[1]], row_col[[2]] , drop=FALSE ] }

		# if it is multilayer then we need to check
		# if user has supplied layers to subset to or not
		if(multilayer_logic_input) {

			# if what_layers_to_peek is NULL then just return with 
			# third mode accounted for
			if(is.null(what_layers_to_peek)){
				out = netlet[ row_col[[1]], row_col[[2]] , , drop=FALSE ] }

			# if what_layers_to_peek is supplied then subset by it
			if(!is.null(what_layers_to_peek)){
				out = netlet[ row_col[[1]], row_col[[2]] , layer_labels, drop=FALSE ] }
		}

		# if the input was multilayer but the output is not then drop the third mode
		if(multilayer_logic_input & !multilayer_logic_out){ out <- out[,,,drop=TRUE] }

		# 
		return( out )
	} # close the cross-sectional block
		
	# if array print specified rows, cols and time
	if(objAttrs$netify_type == 'longit_array'){

		# make sure time range specified doesnt exceed dims of data
		# remember that position of time depends on if the input is 
		# multilayer or not
		if(!multilayer_logic_input){
			when_to_peek <- intersect(when_to_peek, 1:dim(netlet)[3]) }
		if(multilayer_logic_input){
			when_to_peek <- intersect(when_to_peek, 1:dim(netlet)[4]) }

		# make sure rows and cols selected dont exceed dims of data
		row_col <- lapply(1:2, function(ii){
			return( intersect(row_col[[ii]], 1:dim(netlet)[ii]) ) })

		# so first check if the input is multilayer if not then just subset rows and cols
		if(!multilayer_logic_input){
			out = netlet[ row_col[[1]], row_col[[2]], when_to_peek , drop=FALSE ] }

		# if it is multilayer then we need to check
		# if user has supplied layers to subset to or not
		if(multilayer_logic_input) {

			# if what_layers_to_peek is NULL then just return with 
			# third mode accounted for
			if(is.null(what_layers_to_peek)){
				out = netlet[ row_col[[1]], row_col[[2]] , , when_to_peek, drop=FALSE ] }

			# if what_layers_to_peek is supplied then subset by it
			if(!is.null(what_layers_to_peek)){
				out = netlet[ row_col[[1]], row_col[[2]] , layer_labels, when_to_peek, drop=FALSE ] }
		}


		# drop unnecessary dimensions: 
		# time periods if user subsetted down to one period
		# layers if user subsetted down to one layer
		out = drop(out)

		# 
		return( out )
	} # close the longit array block

	# if list print specified rows, cols and time
	if(objAttrs$netify_type == 'longit_list'){

		# make sure time range specified doesnt exceed dims of data
		when_to_peek <- intersect(when_to_peek, 1:length(netlet))
		netlet <- netlet[when_to_peek]
		row_col <- lapply(row_col, function(x){ x[when_to_peek] })

		# iterate through time periods
		relev_netlet <- lapply(1:length(netlet), function(tt){
			
			# get slice of raw netlet
			slice <- netlet[[tt]]

			# pick relev actors but also make sure that 
			# range specified doesnt exceed dims in network
			row_col <- lapply(1:2, function(ii){

				# choose element from row_col
				ids <- row_col[[ii]]

				# list object if we have time varying ids
				# pull out relevant ids for time slice				
				if(is.list(ids)){ ids_slice <- ids[[tt]] }
				
				# not list, if just a general range specified
				# pull out relevant ids for time slice				
				if(!is.list(ids)){ ids_slice <- ids }

				# only keep if within range of data
				ids_slice <- intersect(ids_slice, 1:dim(slice)[ii])				

				#
				return(ids_slice) })

			# so first check if the input is multilayer if not then just subset rows and cols
			if(!multilayer_logic_input){
				relev_slice = slice[ row_col[[1]], row_col[[2]] , drop=FALSE ] }

			# if it is multilayer then we need to check
			# if user has supplied layers to subset to or not
			if(multilayer_logic_input) {

				# if what_layers_to_peek is NULL then just return with 
				# third mode accounted for
				if(is.null(what_layers_to_peek)){
					relev_slice = slice[ row_col[[1]], row_col[[2]] , , drop=FALSE ] }

				# if what_layers_to_peek is supplied then subset by it
				if(!is.null(what_layers_to_peek)){
					relev_slice = slice[ row_col[[1]], row_col[[2]] , layer_labels, drop=FALSE ] }
			}

			# if the input was multilayer but the output is not then drop the third mode
			if(multilayer_logic_input & !multilayer_logic_out){ relev_slice <- relev_slice[,,,drop=TRUE] }

			#
			return(relev_slice)
			})
		
		# add names back
		names(relev_netlet) <- names(netlet)

		#
		return(relev_netlet) } # close longit list block
}

