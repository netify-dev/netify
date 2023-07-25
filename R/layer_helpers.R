#' Helper function for layer_netify to set layer labels
#' 
#' @param netlet_list list of netlet objects
#' @param layer_labels character vector of layer labels
#' @return character vector of layer labels

set_layer_labels <- function(netlet_list, layer_labels){
    # if provided maker sure the number of labels
    # are the same as the length of the netlet list obj
    if(!is.null(layer_labels)){
        if(length(layer_labels) != length(netlet_list)){
            cli::cli_alert_danger(
                'Error: The number of layer labels must be 
                the same as the number of netlets in `netlet_list`.')
            stop() } }

    # if layer labels not present see if we can pull them from
    # the list object
    if(is.null(layer_labels)){
        if(!is.null(names(netlet_list))){
            layer_labels <- names(netlet_list) 
        } else {
            layer_labels <- paste0("layer", 1:length(netlet_list)) } }

    #
    return(layer_labels) }

#' Cycle through elements of a netlet object and make sure
#' that they are identical before we try to merge into 
#' a multilayer netify object
#' 
#' @param a_list list object to check
#' @param elems character vector with names of elements to check
#' @param msg character vector of length two warning user 
#' if elements are found to be not identical. First part of 
#' vector is the preamble before the element name and
#' second part is what should come after the element name.
#' @return NULL

check_layer_compatible <- function(a_list, elems, msg){

    # lets cycle through the elements one at a time so users
    # can be told, if relevant, which element is not identical
    for(elem in elems){

        # check if that attribute is identical across layers
        elem_check <- identical_recursive(
            lapply(a_list, function(x){x[[elem]]}) )

        # if not identical then throw error and stop
        if(!elem_check){
            cli::cli_alert_danger(
                paste0( msg[1], elem, msg[2] ) )
            stop() } } }

#' Helper function for layer_netify to extract attributes
#' from listed netlet objects 
#' 
#' @param a_list named list object
#' @param attrib character string of attribute to extract
#' @param list_format logical, if TRUE return list of attributes
#' @param get_unique logical, if TRUE return unique values of attribute
#' @return attribute values

get_attribs <- function(
    a_list, attrib, 
    list_format=FALSE, get_unique=FALSE
    ){
    out <- lapply(a_list, function(x){x[[attrib]]})
    if(list_format){ return(out) }
    if(!list_format){
        out <- unlist(out)
        if(get_unique){ return(unique(out)) 
        } else { return(out) } } }

#' Reduce and combine multiple nodal attributes of netify objects
#' into a single nodal attribute. Mainly for use within the
#' layer_netify function
#' 
#' @param attribs_list list of attributes from each netlet
#' @param msrmnts_list list of msrmnts from each netlet
#' @param netlet_type character string of netlet type
#' @return nodal attribute data.frame

reduce_combine_nodal_attr <- function(
    attribs_list, msrmnts_list, netlet_type ){
    
    # extract nodal data from each netlet into list
    nodal_data_list <- lapply(
        attribs_list, function(x){x$nodal_data})

    # check for and then drop any null elements
    null_check <- unlist(
        lapply(nodal_data_list, function(x){is.null(x)}))
    nodal_data_list <- nodal_data_list[!null_check]

    # if only one element left then just
    # return that one nodal data element
    if(length(nodal_data_list) == 1){
        return( nodal_data_list[[1]] ) }

    # if more than one element left then try and combine
    if(length(nodal_data_list) > 1){

        # get a list of the vars across the netlets
        nvars <- get_attribs(msrmnts_list, 'nvars', get_unique=TRUE)

        # check to make sure that the id column(s) are identical
        # if columns are not identical then return NULL with
        # warning that user needs to readd themselves
        if(netlet_type == 'cross_sec'){
            n_id_check <- identical_recursive(
                lapply(nodal_data_list, function(x){x[,1]})) }
        if(netlet_type %in% c('longit_array', 'longit_list')){
            n_id_check <- identical_recursive(
                lapply(nodal_data_list, function(x){x[,1:2]})) }
        if(!n_id_check){
            cli::cli_alert_warning(
                'Warning: Nodal data id columns are not identical across netlets, 
                nodal data will not be merged from netlets, you can readd nodal 
                attributes manually using the `add_nodal_data` function.' )
            return( NULL ) }

        # if id columns match then iteratively go through 
        # nodal data and cbind together, procedure is different
        # depending on type
        if(n_id_check){
            ndata <- do.call('cbind', lapply(nodal_data_list, function(slice){
                nvars_slice <- intersect(names(slice), nvars)
                return(slice[,nvars_slice,drop=FALSE]) }))
            if(netlet_type == 'cross_sec'){
                ndata <- cbind(actor=nodal_data_list[[1]][,1], ndata[,nvars]) }
            if(netlet_type %in% c('longit_array', 'longit_list')){
                ndata <- cbind(nodal_data_list[[1]][,1:2], ndata[,nvars]) }
            return( ndata ) }
    } }

#' Reduce and combine multiple dyadic attributes of netify objects
#' into a single dyadic attribute. Mainly for use within the
#' layer_netify function
#' 
#' @param attribs_list list of attributes from each netlet
#' @param msrmnts_list list of msrmnts from each netlet
#' @param netlet_type character string of netlet type
#' @return dyad attribute data.frame

reduce_combine_dyad_attr <- function(
    attribs_list, msrmnts_list, netlet_type ){

    # extract dyad data from each netlet into list
    dyad_data_list <- lapply(attribs_list, function(x){x$dyad_data})

    # check for and then drop any null elements
    null_check <- unlist(lapply(dyad_data_list, function(x){is.null(x[[1]])}))
    dyad_data_list <- dyad_data_list[!null_check]

    # if only one element left then just
    # return that one dyad data element
    if(length(dyad_data_list) == 1){
        return( dyad_data_list[[1]] ) }

    # if more than one element left then try and combine
    if(length(dyad_data_list) > 1){

        # get a list of the vars across the netlets
        dvars <- get_attribs(msrmnts_list, 'dvars', get_unique=TRUE)

        # check to make sure that the id row/col and time periods are
        # identical across dyad_data from netlets
        t_check <- identical_recursive( lapply(dyad_data_list, names))
        r_check <- identical_recursive( lapply(dyad_data_list, rownames))
        c_check <- identical_recursive( lapply(dyad_data_list, colnames))
        if(sum(c(t_check, r_check, c_check)) != 3){
            cli::cli_alert_warning(
                'Warning: Dyad data id columns are not identical across netlets, 
                dyad data will not be merged from netlets, you can readd dyad 
                attributes manually using the `add_dyad` function.' )
            return(NULL) }

        # if id columns match then iteratively go through
        # dyad data and abind together
        if(sum(c(t_check, r_check, c_check)) == 3){

            # bind together dyadic arrays
            t_pds <- names(dyad_data_list[[1]])        
            abind3 <- function(x, y){ abind::abind(x, y, along=3) }
            ddata <- lapply(t_pds, function(tt){
                ddata_tt <- lapply(dyad_data_list, function(dd_ne){
                    out <- dd_ne[[tt]]
                    dvars_slice <- intersect(dvars, dimnames(out)[[3]])
                    return( out[,,dvars_slice,drop=FALSE] ) })
                return( Reduce('abind3', ddata_tt) ) })
            names(ddata) <- t_pds

            #
            return( ddata ) }
    }
}