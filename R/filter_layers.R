#' Filters to specified layer(s) of a netify multilayer network object. 
#' 
#' @param netlet multilayer netlet object
#' @param layer_labels character: label of the layers to be filtered to
#' @return a netlet object subsetted to the chosen layers
#'
#' @examples
#' 
#' library(netify)
#' data(icews)
#' icews_10 <- icews[icews$year==2010,]
#' 
#' # generate netify objects that will be layered together
#' icews_verbCoop <- netify(
#'     dyad_data=icews_10, actor1='i', actor2='j',
#'     symmetric=FALSE, weight='verbCoop' )            
#' 
#' icews_verbConf <- netify(
#'     dyad_data=icews_10, actor1='i', actor2='j',
#'     symmetric=FALSE, weight='verbConf' )
#' 
#' icews_matlCoop <- netify(
#'     dyad_data=icews_10, actor1='i', actor2='j',
#'     symmetric=FALSE, weight='matlCoop' )
#' 
#' icews_matlConf <- netify(
#'     dyad_data=icews_10, actor1='i', actor2='j',
#'     symmetric=FALSE, weight='matlConf' )
#' 
#' icews_layers <- layer_netify(
#'     netlet_list=list(
#'         icews_verbCoop, icews_matlCoop,
#'         icews_verbConf, icews_matlConf ),
#'     layer_labels=c(
#'         'verbCoop', 'matlCoop',
#'         'verbConf', 'matlConf' ) )
#' 
#' # filter to verbCoop and verbConf layers
#' icews_verb <- filter_layers(
#'     netlet=icews_layers, 
#'     layer_labels=c('verbCoop', 'verbConf') )
#' 
#' # so you know what to filter, 
#' # information on layer labels can be accessed 
#' # from  the `layer_labels` attribute 
#' attr(icews_layers, 'layers')
#' 
#' @author Shahryar Minhas
#' @export

filter_layers <- function(netlet, layer_labels){

    # make sure it's a netlet object
    netify_check(netlet)

    # pull out original attributes
    orig_attr <- attributes(netlet)

	# make sure object actually has more than one layer
	if(length(orig_attr$layers) == 1){
		cli::cli_alert_danger(
			paste0(
                'Error: This object does not have multiple layers. 
                The only layer present is: ', orig_attr$layers, '.'))
		stop() }

    # if user supplied a numeric index then make sure that
    # layer exists in the range and convert to character
    if(is.numeric(layer_labels)){
        if( sum(layer_labels > length(orig_attr$layers))>0 ){
            cli::cli_alert_danger(
                paste0(
                    'Error: There are only ', length(orig_attr$layers),' layers present.'
                    )) ; stop() }
        layer_labels <- orig_attr$layers[layer_labels] }

    # pull out type
    netlet_type <- attr(netlet, 'netify_type')

    # make sure layer_labels are a character vector
    if(!is.character(layer_labels)){
        cli::cli_alert_danger(
            'Error: `layer_labels` must be a character vector' )
        stop() }

    # make sure layer_labels chosen exist in netlet object
    if(!all(layer_labels %in% attr(netlet, 'layers'))){
        cli::cli_alert_danger(
            'Error: `layer_labels` chosen do not exist in netlet object' )
        stop() }

    # if only one layer chosen, drop any extra dimensions
    drop_dim <- ifelse( length(layer_labels)==1, TRUE, FALSE )

    # get num index of which layers were kept
    layer_indices <- match(layer_labels, orig_attr$layers)
    
    # subset the layers
    # cross_sec case
    if(netlet_type == 'cross_sec'){
        subbed <- netlet[,,layer_labels,drop=drop_dim]
        new_attr <- c( attributes(subbed), orig_attr[-(1:2)] )
        new_attr$weight <- split_string(
            new_attr$weight, ', ', layer_indices )
        new_attr$layers <- layer_labels
        attributes(subbed) <- new_attr }

    # longit_array case
    if(netlet_type == 'longit_array'){
        subbed <- netlet[,,layer_labels,drop=drop_dim]
        new_attr <- c( attributes(subbed), orig_attr[-(1:2)] )
        new_attr$weight <- split_string(
            new_attr$weight, ', ', layer_indices )
        new_attr$layers <- layer_labels
        attributes(subbed) <- new_attr }

    # longit_list case
    if(netlet_type == 'longit_list'){
        # subset to chosen layers by time pd
        subbed <- lapply(netlet, function(slice){
            orig_attr_slice <- attributes(slice)
            subbed <- slice[,,layer_labels,drop=drop_dim]
            new_attr <- c( attributes(subbed), orig_attr_slice[-(1:2)] )
            new_attr$weight <- split_string(
                new_attr$weight, ', ', layer_indices )
            new_attr$layers <- layer_labels
            attributes(subbed) <- new_attr
            return(subbed) })
        new_attr <- orig_attr
        new_attr$weight <- split_string( new_attr$weight, ', ', layer_indices )
        new_attr$layers <- layer_labels
        attributes(subbed) <- new_attr }

    #
    return(subbed) }