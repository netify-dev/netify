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
	what_to_subset=NULL,
	what_rows_to_subset=what_to_subset,
	what_cols_to_subset=what_to_subset,
	when_to_subset=NULL,
	what_layers_to_subset=NULL
){

    # check if netify object
    netify_check(netlet)    

    # pull out attrs and msrmnts of original
    obj_attrs <- attributes(netlet)
    msrmnts <- netify_measurements(netlet)
    nlayers <- length(obj_attrs$layers)

    # check if output should be multilayer
    nlayers_subset <- ifelse( 
        !is.null(what_layers_to_subset), 
        length(what_layers_to_subset), 
        nlayers )
    multilayer_logic <- ifelse( nlayers_subset==1, 
        FALSE, TRUE )

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
    # and also adjust obj_attrs to reflect that the 
    # subsetted element is now a matrix instead of a list
    if( obj_attrs$netify_type!='cross_sec' ){
        if( is.list(sub_net) & length(sub_net)==1 ){
            
            # extract out of list
            sub_net <- sub_net[[1]]
            
            # adjust obj_attrs
            obj_attrs <- obj_attrs[-1]
            obj_attrs <- append(
                obj_attrs,  attributes(sub_net),
                after=0 )
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
        obj_attrs$netify_type!='cross_sec' ){
        when_to_subset <- msrmnts$time }

    # add back in netify attributes
    obj_attrs2 <- obj_attrs

    # new object: longit list
    if(is.list(sub_net)){
    
        # pull attributes from a cross-sec in the list
        crossSec_obj_attrs <- attributes(netlet[[1]])
        # sub_dims <- attributes(sub_net[[1]])
        
        # # adjust actor composition
        # crossSec_obj_attrs[1:2] <- sub_dims[1:2]
        
        # apply change to each element in subsetted list
        sub_net <- lapply(sub_net, function(x){
            new_attribs <- append(attributes(x), crossSec_obj_attrs[-(1:2)])
            attributes(x) <- new_attribs
            return(x) })
        
        # list level attributes
        # adjust years
        obj_attrs2$names <- names(sub_net)
    }

    # new object: cross-sectional/longit array/multilayer
    if(!is.list(sub_net)){
    
        # adjust dimensions
        obj_attrs2[1:2] <- attributes(sub_net)[1:2]
        
        # adjust netify_type
        if( length(dim(sub_net))==2 & !multilayer_logic ){
            obj_attrs2$netify_type <- 'cross_sec' }
        
        # related mod for multilayer net
        if( length(dim(sub_net))==3 & multilayer_logic ){
            obj_attrs2$netify_type <- 'cross_sec' }  
    }

    # get actors in subsetted netlet
    if(is.list(sub_net)){
        sub_actors <- unlist(lapply(sub_net, rownames)) }
    if(!is.list(sub_net)){
        sub_actors <- rownames(sub_net) }
    names(sub_actors) <- NULL

    # adjust actor periods
    obj_attrs2$actor_pds <- obj_attrs$actor_pds[
    obj_attrs$actor_pds$actor %in% sub_actors,]
    if(obj_attrs2$netify_type != 'cross_sec'){
        obj_attrs2$actor_pds$min_time <- apply(
            obj_attrs2$actor_pds, 1, function(x){
            max( x['min_time'], min(when_to_subset)) } )
        obj_attrs2$actor_pds$max_time <- apply(
            obj_attrs2$actor_pds, 1, function(x){
            min( x['max_time'], max(when_to_subset)) } ) 
    }

    # adjust nodal_data
    if(!is.null(obj_attrs$nodal_data)){
    
        # longit case
        if(obj_attrs$netify_type!='cross_sec'){
            obj_attrs2$nodal_data <- obj_attrs$nodal_data[
            obj_attrs$nodal_data$actor %in% sub_actors &
                obj_attrs$nodal_data$time %in% when_to_subset,,drop=FALSE] }
        
        # cross_sec case
        if(obj_attrs$netify_type=='cross_sec'){
            obj_attrs2$nodal_data <- obj_attrs$nodal_data[
            obj_attrs$nodal_data$actor %in% sub_actors,,drop=FALSE] }
    }

    # adjust dyad_data
    if(!is.null(obj_attrs$dyad_data)){
    
        # first subset time if longit
        if(obj_attrs$netify_type!='cross_sec'){
            obj_attrs2$dyad_data <- obj_attrs2$dyad_data[when_to_subset] }
        
        # then iterate through and subset actors
        obj_attrs2$dyad_data <- lapply(
            obj_attrs2$dyad_data, function(dd){
            toKeep_rows <- intersect(rownames(dd), sub_actors)
            toKeep_cols <- intersect(colnames(dd), sub_actors)
            dd[toKeep_rows, toKeep_cols,,drop=FALSE] } )
    }

    # add back in netify attributes
    attributes(sub_net) <- obj_attrs2

    # return object
    return(sub_net)
}
