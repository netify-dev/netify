#' Layer netify objects together to create a multilayer network
#'
#' `layer_netlet` takes in two netify objects and 
#' layers them together to create a multilayer network
#' 
#' 
#' @param netlet_list a list of netifty objects that you want to layer together
#' @param layer_labels character: label of the layer for each netify object
#' @return an a multilayer network of class netify
#' 
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @examples
#' 
#' # cross-sectional example
#' data(icews)
#' icews_10 <- icews[icews$year==2010,]
#' 
#' # generate netify objects that will be layered together
#' icews_verbCoop <- netify(
#'     dyad_data=icews_10, actor1='i', actor2='j',
#'     symmetric=FALSE, weight='verbCoop',
#'     nodal_vars=c('i_log_gdp', 'i_log_pop'),
#'     dyad_vars=c('verbConf') )
#' 
#' icews_matlCoop <- netify(
#'     dyad_data=icews_10, actor1='i', actor2='j',
#'     symmetric=FALSE, weight='matlCoop',
#'     nodal_vars='i_polity2',
#'     dyad_vars=c('matlConf') )
#' 
#' # layer together cross-sec netify objects together
#' icews_verbCoop_matlCoop <- layer_netlet(
#'     netlet_list=list(icews_verbCoop, icews_matlCoop),
#'     layer_labels=c('verbCoop', 'matlCoop') )
#' 
#' # dimensions of the multilayer network from the
#' # cross-sectional case will be a 
#' # (number of actors) x (number of actors) x (number of layers)
#' dim(get_raw(icews_verbCoop_matlCoop))
#'
#' # longitudinal array example
#' icews_verbCoop_longit_a <- netify(
#'     dyad_data=icews, actor1='i', actor2='j', time='year',
#'     symmetric=FALSE, weight='verbCoop',
#'     nodal_vars=c('i_log_gdp', 'i_log_pop'),
#'     dyad_vars=c('verbConf'),
#'     output_format='longit_array' )
#' icews_matlCoop_longit_a <- netify(
#'     dyad_data=icews, actor1='i', actor2='j', time='year',
#'     symmetric=FALSE, weight='matlCoop',
#'     nodal_vars=c('i_polity2'),
#'     dyad_vars=c('matlConf'),
#'     output_format='longit_array' )
#' 
#' # layer together
#' icews_verbCoop_matlCoop_longit_a <- layer_netlet(
#'     netlet_list=list(icews_verbCoop_longit_a, icews_matlCoop_longit_a),
#'     layer_labels=c('verbCoop', 'matlCoop') )
#' 
#' # dimensions of the multilayer network from the
#' # longitudinal array case will be a 
#' # (number of actors) x (number of actors) x (number of layers) x 
#' # (number of time periods)
#' dim(get_raw(icews_verbCoop_matlCoop_longit_a)) 
#'
#' # longitudinal list example
#' # generate similar longitudinal list versions
#' icews_verbCoop_longit_l <- netify(
#'     dyad_data=icews, actor1='i', actor2='j', time='year',
#'     symmetric=FALSE, weight='verbCoop',
#'     nodal_vars=c('i_log_gdp', 'i_log_pop'),
#'     dyad_vars=c('verbConf') )
#' icews_matlCoop_longit_l <- netify(
#'     dyad_data=icews, actor1='i', actor2='j', time='year',
#'     symmetric=FALSE, weight='matlCoop',
#'     nodal_vars=c('i_polity2'),
#'     dyad_vars=c('matlConf') )
#' 
#' # layer together
#' icews_verbCoop_matlCoop_longit_l <- layer_netlet(
#'     netlet_list=list(icews_verbCoop_longit_l, icews_matlCoop_longit_l),
#'     layer_labels=c('verbCoop', 'matlCoop') )
#' 
#' # dimensions of the multilayer network from the 
#' # longitudinal list case will be a
#' # (number of time periods) list of 
#' # (number of actors) x (number of actors) x (number of layers) arrays
#' names(get_raw(icews_verbCoop_matlCoop_longit_l))
#' dim(get_raw(icews_verbCoop_matlCoop_longit_l)$'2010')
#' 
#' # information on layer labels can be accessed 
#' # from  the `layers` attribute 
#' attr(icews_verbCoop_matlCoop, 'layers')
#' attr(icews_verbCoop_matlCoop_longit_l, 'layers')
#'
#' @export

layer_netlet <- function(netlet_list, layer_labels=NULL){

    # user input checks
    invisible(lapply(netlet_list, netify_check))

    # set layer labels
    layer_labels <- set_layer_labels(netlet_list, layer_labels)

    # store attributes from netlet objects
    attribs_list <- lapply(netlet_list, attributes)

    # define relevant attribute types that must be identical
    rel_attrs <- c( 'netify_type', 'actor_time_uniform', 
        'actor_pds',  'symmetric', 'mode' )

    # check if attributes compatible
    check_layer_compatible(
        a_list=attribs_list, 
        elems=rel_attrs,
        msg=c( 'Error: The ', 
            ' attribute is not identical across the netlets in `netlet_list`.'
            ) )

    # generate weights value for multilayer case
    weight_collapse <- lapply(attribs_list, function(x){
        if(is.null(x$weight)){
            return('NULL') } else { return(x$weight) } })
    weight_collapse <- paste(
        unlist(weight_collapse), collapse=', ')

    # do the same for longer weight label descriptors
    # just dont need the NULL check
    weight_label_collapse <- lapply(attribs_list, function(x){
        return(x$detail_weight) })
    weight_label_collapse <- paste(
        unlist(weight_label_collapse), collapse=' | ')

    # pull out logical for whether we have binary weights
    weight_binary_vec = unlist( lapply(attribs_list, function(x){
        return(x$weight_binary) }) )

    # check to make sure that the networks can be layered
    # meaning that they share the same dimensions
    msrmnts_list <- lapply(netlet_list, netify_measurements)

    # define relevant  measurements that must be identical
    rel_msrs <- c( 'row_actors', 'col_actors', 'time', 
        'n_row_actors', 'n_col_actors', 'n_time' )

    # check if dimensions compatible
    check_layer_compatible(
        a_list=msrmnts_list, 
        elems=rel_msrs,
        msg=c( 'Error: ', 
            ' are not identical across the netlets in `netlet_list`.'
            ) )

    # now pull out relevant measurements so that we can construct
    # the array to store the multilayer network, can pull out just the
    # first element of the msrmnts_list since we know they are identical
    netlet_type <- attribs_list[[1]]$netify_type
    n_layers <- length(netlet_list)
    n_time <- msrmnts_list[[1]]$n_time
    n_row_actors <- msrmnts_list[[1]]$n_row_actors
    n_col_actors <- msrmnts_list[[1]]$n_col_actors
    time <- msrmnts_list[[1]]$time
    row_actors <- msrmnts_list[[1]]$row_actors
    col_actors <- msrmnts_list[[1]]$col_actors

    # pull out raw versions of data so that we can more 
    # efficiently fill in the array
    netlet_raws <- lapply(netlet_list, get_raw)

    # cross-sec case
    if( netlet_type=='cross_sec' ){

        # define array
        arr_dim <- c( n_row_actors, n_col_actors, n_layers)
        arr_labs <- list( row_actors, col_actors, layer_labels)
        netlet <- array(NA, dim=arr_dim, dimnames=arr_labs)

        # fill in array
        for(ii in 1:n_layers){ netlet[,,ii] <-  netlet_raws[[ii]] } }

    # array case
    if( netlet_type == 'longit_array' ){

        # define array
        arr_dim <- c( n_row_actors, n_col_actors, n_layers, n_time)
        arr_labs <- list( row_actors, col_actors, layer_labels, time)
        netlet <- array(NA, dim=arr_dim, dimnames=arr_labs)

        # fill in array
        for(ii in 1:n_layers){ netlet[,,ii,] <-  netlet_raws[[ii]] } }

    # longit list case
    if( netlet_type == 'longit_list' ){

        # here results are stored in a n_time list 
        # of n x n x n_layer arrays
        netlet <- lapply(1:n_time, function(tt){
            
            # define array
            arr_dim <- c( n_row_actors[[tt]], n_col_actors[[tt]], n_layers)
            arr_labs <- list( row_actors[[tt]], col_actors[[tt]], layer_labels)
            arr <- array(NA, dim=arr_dim, dimnames=arr_labs)

            # fill in array
            for(ii in 1:n_layers){ arr[,,ii] <-  netlet_raws[[ii]][[tt]] }

            # add attributes to each of these arrays
            class(arr) <- 'netify'
            attr(arr, 'netify_type') <- 'cross_sec'
            attr(arr, 'actor_time_uniform') <- NULL
            attr(arr, 'actor_pds') <- NULL
            attr(arr, 'weight') <- weight_collapse
            attr(arr, 'detail_weight') <- weight_label_collapse
            attr(arr, 'weight_binary') <- weight_binary_vec
            attr(arr, 'symmetric') <- attribs_list[[1]]$symmetric
            attr(arr, 'mode') <- attribs_list[[1]]$mode
            attr(arr, 'layers') <- layer_labels
            attr(arr, 'diag_to_NA') <- get_attribs(attribs_list, 'diag_to_NA')
            attr(arr, 'missing_to_zero') <- get_attribs(attribs_list, 'missing_to_zero')
            attr(arr, 'sum_dyads') <- get_attribs(attribs_list, 'sum_dyads')
            attr(arr, 'nodal_data') <- NULL
            attr(arr, 'dyad_data') <- NULL
            attr(arr, 'graph_data') <- NULL  
            return(arr) } )
        
        # assign names array elements of list object
        names(netlet) <- time }

    # add attributes
    class(netlet) <- 'netify'
    attr(netlet, 'netify_type') <- netlet_type
    attr(netlet, 'actor_time_uniform') <- attribs_list[[1]]$actor_time_uniform
    attr(netlet, 'actor_pds') <- attribs_list[[1]]$actor_pds
    attr(netlet, 'weight') <- weight_collapse
    attr(netlet, 'detail_weight') <- weight_label_collapse
    attr(netlet, 'weight_binary') <- weight_binary_vec
    attr(netlet, 'symmetric') <- attribs_list[[1]]$symmetric
    attr(netlet, 'mode') <- attribs_list[[1]]$mode
    attr(netlet, 'layers') <- layer_labels
    attr(netlet, 'diag_to_NA') <- get_attribs(attribs_list, 'diag_to_NA')
    attr(netlet, 'missing_to_zero') <- get_attribs(attribs_list, 'missing_to_zero')
    attr(netlet, 'sum_dyads') <- get_attribs(attribs_list, 'sum_dyads')
    attr(netlet, 'nodal_data') <- NULL
    attr(netlet, 'dyad_data') <- NULL
    attr(netlet, 'graph_data') <- NULL

    # combine nodal data attributes from each netlet
    # check if nodal data is identical across netlets
    nodal_data_ident_check <- identical_recursive(
        lapply(attribs_list, function(x){x$nodal_data}) )

    # if nodal data is identical then just pick first one
    if( nodal_data_ident_check ){
        attr(netlet, 'nodal_data') <- attribs_list[[1]]$nodal_data }

    # if nodal attributes not identical then combine manually
    if( !nodal_data_ident_check ){
        attr(netlet, 'nodal_data') <- reduce_combine_nodal_attr(
            attribs_list, msrmnts_list, netlet_type) }

    # combine nodal data attributes from each netlet
    # check if nodal data is identical across netlets
    dyad_data_ident_check <- identical_recursive(
        lapply(attribs_list, function(x){x$dyad_data}) )

    # if dyad data is identical then just pick first one
    if( dyad_data_ident_check ){
        attr(netlet, 'dyad_data') <- attribs_list[[1]]$dyad_data }

    # if dyad attributes not identical then combine manually
    if( !dyad_data_ident_check ){
        attr(netlet, 'dyad_data') <- reduce_combine_dyad_attr(
            attribs_list, msrmnts_list, netlet_type ) }

    # return with relev attributes added
    return(netlet)
}
