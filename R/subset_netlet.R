#' subset_netlet method for netify objects
#'
#' subset_netlet takes in a netify object
#' and allows for the extraction of smaller networks.
#'
#' @param netlet object of class netify
#' @param what_to_subset enter the name of specific nodes to subset in character vector form or provide a numeric range, default is to show the first three rows and columns of interactions
#' @param what_rows_to_subset similar as what_to_subset but specific to rows, default value is set to what_to_subset. If you want to subset at all rows then set this to NULL. 
#' @param what_cols_to_subset similar as what_to_subset but specific to columns, default value is set to what_to_subset. If you want to subset at all columns then set this to NULL.
#' @param when_to_subset choose time points to subset from, default is to show the first time point of data. 
#' If the entry is a numeric value or vector then it will be used as an index to the time dimension. 
#' If the entry is a character vector then it will be used to match the time dimension labels.
#' If you want to subset at all time points then set this to NULL.
#' @param what_layers_to_subset if the netlet object has multiple layers, then you must choose one layer to subset at.
#' @return a subset of the original network that has relevant netify attributes
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @examples
#'
#' # load example directed event data from ICEWS
#' data(icews)
#' 
#' # generate a longitudional netify object 
#' # with both dyadic and nodal attributes
#' icews_matlConf <- netify(
#'     dyad_data=icews,
#'     actor1='i', actor2='j', time='year',
#'     symmetric=FALSE, weight='matlConf',
#'     nodal_vars=c('i_polity2', 'i_log_gdp', 'i_log_pop'),
#'     dyad_vars=c('matlCoop', 'verbCoop', 'verbConf'),
#'     dyad_vars_symmetric=c(FALSE, FALSE, FALSE) )  
#' 
#' # subset to a few countries
#' icews_matlConf_subset <- subset_netlet(
#'    netlet=icews_matlConf,
#'   what_to_subset=c('United States', 'United Kingdom',
#'      'Russian Federation', 'China') )
#' 
#' # subset to a few countries and a few years
#' icews_matlConf_subset <- subset_netlet(
#'   netlet=icews_matlConf,
#'  what_to_subset=c('United States', 'United Kingdom',
#'     'Russian Federation', 'China'),
#' when_to_subset=c('2010', '2011') )
#' 
#' @export subset_netlet
#' 

subset_netlet <- function(
    netlet, 
	what_to_subset=20,
	what_rows_to_subset=what_to_subset,
	what_cols_to_subset=what_to_subset,
	when_to_subset=NULL,
	what_layers_to_subset=NULL
){

	# pull out attrs and msrmnts of original
	objAttrs <- attributes(netlet)
    msrmnts <- netify_measurements(netlet)
    nlayers <- length(objAttrs$layers)

    # check if output should be multilayer
    nlayers_subset <- ifelse(
        !is.null(what_layers_to_subset), 
        length(what_layers_to_subset), 
        nlayers )
    multilayer_logic <- ifelse(
        nlayers_subset==1, FALSE, TRUE )

    # when just one layer net or one layer
    # selected we can just use peek
    if( !multilayer_logic ){
        sub_net <- peek(
            netlet, 
            what_to_peek=what_to_subset,
            what_rows_to_peek=what_rows_to_subset,
            what_cols_to_peek=what_cols_to_subset,
            when_to_peek=when_to_subset,
            what_layer_to_peek=what_layers_to_subset ) }

    # longit check, if data is longitudinal and one time
    # period was subsetted then need to extract out of list
    # and also adjust objAttrs to reflect that the 
    # subsetted element is now a matrix instead of a list
    if( objAttrs$netify_type!='cross_sec' ){
        if( class(sub_net)=='list' & length(sub_net)==1 ){
            
            # extract out of list
            sub_net <- sub_net[[1]]

            # adjust objAttrs
            objAttrs <- objAttrs[-1]
            objAttrs <- append(
                objAttrs, 
                attributes(sub_net),
                after=0
                )
        }
    }

    # use peek function to generate subsetted 
    # version of the netlet object, since
    # peek doesnt support looking at multiple
    # layers we need to iterate peek through
    # layers and then combine back into
    # multidimensional array    
     if( multilayer_logic ){

        # iterate peek through layers
        sub_net <- lapply(
            what_layers_to_subset, 
            function(x){
                peek(
                    netlet, 
                    what_to_peek=what_to_subset,
                    what_rows_to_peek=what_rows_to_subset,
                    what_cols_to_peek=what_cols_to_subset,
                    when_to_peek=when_to_subset,
                    what_layer_to_peek=x ) } )

        # combine into multidimensional array
        sub_net <- do.call(
            'abind', 
            c(sub_net, along=3) )
    }

    # if user puts NULL for time and object
    # is longit then change when_to_subset to all
    # time points
    if( is.null(when_to_subset) & 
        objAttrs$netify_type!='cross_sec' ){
        when_to_subset <- msrmnts$time }

    # add back in netify attributes
    objAttrs2 <- objAttrs

    # new object: longit list
    if(is.list(sub_net)){

        # pull attributes from a cross-sec in the list
        crossSec_objAttrs <- attributes(netlet[[1]])
        sub_dims <- attributes(sub_net[[1]])

        # adjust actor composition
        crossSec_objAttrs[1:2] <- sub_dims[1:2]

        # apply change to each element in subsetted list
        sub_net <- lapply(sub_net, function(x){
            attributes(x) <- crossSec_objAttrs ; return(x) })

        # list level attributes
        # adjust years
        objAttrs2$names <- names(sub_net)
    }

    # new object: cross-sectional/longit array/multilayer
    if(!is.list(sub_net)){
    
        # adjust dimensions
        objAttrs2[1:2] <- attributes(sub_net)[1:2]

        # adjust netify_type
        if( length(dim(sub_net))==2 & !multilayer_logic ){
            objAttrs2$netify_type <- 'cross_sec' }

        # related mod for multilayer net
        if( length(dim(sub_net))==3 & multilayer_logic ){
            objAttrs2$netify_type <- 'cross_sec' }  
    }

    # deal with NULL entries for row/col selections
    # before moving towards adjusting actor
    # related attributes
    row_actors <- unlist(msrmnts$row_actors)
    col_actors <- unlist(msrmnts$col_actors)
    row_sel <- ifelse(
        is.null(what_rows_to_subset),
        row_actors, what_rows_to_subset )
    col_sel <- ifelse(
        is.null(what_cols_to_subset),
        col_actors, what_cols_to_subset )
    sub_actors <- unique(
        c(row_sel, col_sel))

    # adjust actor periods
    objAttrs2$actor_pds <- objAttrs$actor_pds[
        objAttrs$actor_pds$actor %in% sub_actors,]
    if(objAttrs2$netify_type != 'cross_sec'){
        objAttrs2$actor_pds$min_time <- apply(
            objAttrs2$actor_pds, 1, function(x){
                max( x['min_time'], min(when_to_subset)) } )
        objAttrs2$actor_pds$max_time <- apply(
            objAttrs2$actor_pds, 1, function(x){
                min( x['max_time'], max(when_to_subset)) } ) 
    }

    # adjust nodal_data
    if(!is.null(objAttrs$nodal_data)){
        objAttrs2$nodal_data <- objAttrs$nodal_data[
            objAttrs$nodal_data$actor %in% sub_actors &
            objAttrs$nodal_data$time %in% when_to_subset,] }

    # adjust dyad_data
    if(!is.null(objAttrs$dyad_data)){

        # first subset time
        objAttrs2$dyad_data <- objAttrs2$dyad_data[when_to_subset]
        
        # then iterate through and subset actors
        objAttrs2$dyad_data <- lapply(
            objAttrs2$dyad_data, function(dd){
                toKeep_rows <- intersect(rownames(dd), sub_actors)
                toKeep_cols <- intersect(colnames(dd), sub_actors)
                dd[toKeep_rows, toKeep_cols,] } ) }

    # add back in netify attributes
    attributes(sub_net) <- objAttrs2

    # return object
    return(sub_net)
}


# library(netify)
# data(icews)

# net_adj <- netify(
#     dyad_data=icews,
#     actor1='i', actor2='j', 
#     symmetric=FALSE, weight='matlConf',
#     nodal_vars=c('i_polity2', 'i_log_gdp', 'i_log_pop'),
#     dyad_vars=c('matlCoop', 'verbCoop', 'verbConf'),
#     dyad_vars_symmetric=c(FALSE, FALSE, FALSE),
#     sum_dyads=TRUE )  

# net_arr <- netify(
#     dyad_data=icews,
#     actor1='i', actor2='j', time='year',
#     symmetric=FALSE, weight='matlConf',
#     nodal_vars=c('i_polity2', 'i_log_gdp', 'i_log_pop'),
#     dyad_vars=c('matlCoop', 'verbCoop', 'verbConf'),
#     dyad_vars_symmetric=c(FALSE, FALSE, FALSE),
#     output_format='longit_array' )  

# net_list <- netify(
#     dyad_data=icews,
#     actor1='i', actor2='j', time='year',
#     symmetric=FALSE, weight='matlConf',
#     nodal_vars=c('i_polity2', 'i_log_gdp', 'i_log_pop'),
#     dyad_vars=c('matlCoop', 'verbCoop', 'verbConf'),
#     dyad_vars_symmetric=c(FALSE, FALSE, FALSE) )  

# # generate netify objects that will be layered together
# icews_10 <- icews[icews$year==2010,]
# icews_verbCoop <- netify(
#     dyad_data=icews_10, actor1='i', actor2='j',
#     symmetric=FALSE, weight='verbCoop',
#     nodal_vars=c('i_log_gdp', 'i_log_pop'),
#     dyad_vars=c('verbConf') )

# icews_matlCoop <- netify(
#     dyad_data=icews_10, actor1='i', actor2='j',
#     symmetric=FALSE, weight='matlCoop',
#     nodal_vars='i_polity2',
#     dyad_vars=c('matlConf') )
 
# # layer together cross-sec netify objects together
# multinet <- layer_netify(
#     netlet_list=list(icews_verbCoop, icews_matlCoop),
#     layer_labels=c('verbCoop', 'matlCoop') )

# head(attr(net_list, 'actor_pds'))

# netlet = net_arr
# what_to_subset = c(
#     'United States', 'United Kingdom', 
#     'Russian Federation', 'China' )
# # what_to_subset = NULL
# what_rows_to_subset=what_to_subset
# what_cols_to_subset=what_to_subset
# # when_to_subset = as.character(2010:2011)
# when_to_subset = NULL
# when_to_subset = '2013'
# what_layers_to_subset = NULL

# subset_netlet(
#     net_adj, 
#     what_to_subset=what_to_subset
# )

# subset_netlet(
#     net_arr, 
#     what_to_subset=what_to_subset,
#     when_to_subset=when_to_subset
# )

# subset_netlet(
#     net_list, 
#     what_to_subset=what_to_subset,
#     when_to_subset=when_to_subset
# )

# subset_netlet(
#     net_list, 
#     what_to_subset=NULL,
#     when_to_subset=when_to_subset
# )
