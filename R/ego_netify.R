#' Create ego networks from a netify object
#'
#' `ego_netify` extracts ego networks from a 
#' netify object. An ego network consists of a focal node (ego) and its immediate 
#' neighbors (alters). For weighted networks, users can define neighborhoods using 
#' edge weight thresholds. The function returns netify object(s) representing the 
#' ego network(s).
#'
#' @param netlet A netify object (class "netify") from which to extract ego networks.
#' @param ego Character vector specifying the name(s) of the ego(s) for whom to 
#'   create ego networks. Must match actor names in the netify object.
#' @param threshold Numeric value or vector specifying the threshold for including 
#'   alters in the ego network based on edge weights. For longitudinal networks, 
#'   can be a vector with length equal to the number of time periods to apply 
#'   different thresholds over time. If NULL (default), uses 0 for unweighted 
#'   networks and the mean edge weight for weighted networks.
#' @param ngbd_direction Character string specifying which neighbors to include 
#'   for directed networks. Options are:
#'   \itemize{
#'     \item \code{"out"}: Include alters that ego has outgoing ties to
#'     \item \code{"in"}: Include alters that ego has incoming ties from
#'     \item \code{"any"}: Include alters with any tie to/from ego (default)
#'   }
#' @param include_ego Logical. If TRUE (default), the ego node is included in 
#'   the ego network. If FALSE, only alters are included.
#'
#' @return Depending on the input and number of egos specified:
#'   \itemize{
#'     \item \strong{Single ego, cross-sectional}: A netify object representing 
#'       the ego network
#'     \item \strong{Multiple egos, cross-sectional}: A list of netify objects, 
#'       one per ego
#'     \item \strong{Longitudinal (any number of egos)}: A list of netify objects 
#'       with ego-time combinations as elements
#'   }
#'   
#'   Each returned netify object includes additional attributes:
#'   \itemize{
#'     \item \code{ego_netify}: TRUE (indicator that this is an ego network)
#'     \item \code{ego_id}: Identifier of the ego (and time period if longitudinal)
#'     \item \code{threshold}: Threshold value(s) used
#'     \item \code{ngbd_direction}: Direction specification used
#'     \item \code{include_ego}: Whether ego was included
#'   }
#'
#' @details
#' The function extracts ego networks by identifying all nodes connected to the 
#' specified ego(s) based on the given criteria:
#' 
#' \strong{Neighborhood definition:}
#' \itemize{
#'   \item For unweighted networks: All nodes with edges to/from ego (threshold = 0)
#'   \item For weighted networks: All nodes with edge weights exceeding the threshold
#'   \item Direction matters only for directed networks (controlled by ngbd_direction)
#' }
#' 
#' \strong{Threshold behavior:}
#' \itemize{
#'   \item If not specified, defaults to 0 for unweighted networks
#'   \item If not specified for weighted networks, uses the mean edge weight
#'   \item For longitudinal networks, can vary by time period if a vector is provided
#'   \item Edges with weights > threshold are included (not ≥)
#' }
#' 
#' \strong{Output structure:}
#' 
#' The function preserves all attributes from the original netify object, including 
#' nodal and dyadic variables, but subsets them to include only ego and its neighbors. 
#' For longitudinal networks, ego networks may vary in composition across time periods 
#' as relationships change.
#' 
#' \strong{Limitations:}
#' \itemize{
#'   \item Currently does not support multilayer networks
#'   \item Currently does not support bipartite networks
#' }
#'
#'
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @export ego_netify

ego_netify = function(
    netlet, ego, threshold = NULL, 
    ngbd_direction='any', include_ego = TRUE ) {
  
    ######################
    # check if netify object
    netify_check(netlet)
  
    # if longit array to longit list so we dont
    # need separate processes and we cant stay in 
    # array format for ego nets anyhow
    # (i.e., ego+alters change over time)
    if(attr(netlet, 'netify_type') == 'longit_array'){ 
        netlet = array_to_list(netlet, preserveAttr=TRUE) }

    # pull out attrs and msrmnts of original
    obj_attrs <- attributes(netlet)
    msrmnts <- netify_measurements(netlet)

    # determine if multilayer and/or longitudinal
    weighted <- !obj_attrs$weight_binary
    multilayer <- ifelse( length(obj_attrs$layers)>1, TRUE, FALSE )
    longitudinal <- ifelse( obj_attrs$netify_type!='cross_sec', TRUE, FALSE )
    bipartite <- ifelse( obj_attrs$mode=='bipartite', TRUE, FALSE)

    # get netlet type
    netlet_type <- obj_attrs$netify_type
    ######################

    ######################
    # stop if multilayer and say we dont support
    if(multilayer){
		cli::cli_alert_danger(
            'Error: This object has multiple layers. 
            `ego_netify` does not currently support multilayer `netify` inputs. 
            Please use the `subset_netify` function to create a `netify` object with a single layer.' )
		stop() }

    # stop if bipartite and say we dont support
    if(bipartite){
		cli::cli_alert_danger(
            'Error: This object is bipartite. 
            `ego_netify` does not currently support bipartite `netify` inputs. 
            Please use the `subset_netify` function to create a `netify` object with a single layer.' )
		stop() }
    ######################

    ######################
    # Check if the ego is a character vector
    if (!is.character(ego)) {
        cli::cli_alert_danger("`ego` must be a character value or vector.")
        stop() }
  
    # Check if the ego is in the network
    poss_actors = unique(obj_attrs$actor_pds$actor)
    oops = ego[!ego %in% poss_actors]

    # if length oops not zero, then print error
    if(length(oops) > 0) {
      cli::cli_alert_danger(
        paste0(
          'Error: The following ego(s) are not in the data: ', 
          paste(oops, collapse = ', '), '.'))
      stop() }
    
    # add check for ngbd_direction
    if(!ngbd_direction %in% c('out', 'in', 'any')){
        cli::cli_alert_danger(
            'Error: `ngbd_direction` must be one of "out", "in", or "any".')
        stop() }
    ######################
  
    ######################
    # if supplied check that threshold is numeric
    if (!is.null(threshold) && !is.numeric(threshold)) {
        cli::cli_alert_danger("`threshold` argument must be a numeric input.")
        stop() }
    ######################

    ######################
    # get raw network
    raw_net = get_raw(netlet)

    # if cross sec convert to a list object so that
    # we can use lapply    
    if(netlet_type == 'cross_sec'){ raw_net <- list(raw_net) }
    ######################

    ######################
    # if unweighted then define neighborhood using threshold of zero
    if(!weighted){ threshold = rep(0, length(raw_net)) }
    
    # if weighted then define neighborhood using mean of edge weights
    # if threshold is not supplied
    if(weighted){

        # if threshold provided and net is longit then check if
        # threshold is a vector of length equal to the number of time points
        # if not then rep the threshold to be the same for all time points
        if(!is.null(threshold) && longitudinal){
            if(length(threshold) != length(raw_net)){
                threshold = rep(threshold, length(raw_net)) } }

        # iterate through each list element and create new 
        # adjacency matrix with only the ego and its neighbors
        if(is.null(threshold)){
            threshold = unlist(
                lapply(raw_net, function(net){ mean(c(net), na.rm=TRUE) } ) ) }
    }
    ######################

    ######################
    # define neighborhood for each ego
    # based on thresh get vector of actors to keep by list element
    ego_nets = lapply(ego, function(ego_ii){
        get_ngbd_net_for_ego(
            raw_net, ego_ii, threshold, include_ego, ngbd_direction) })
    ego_nets = do.call('c', ego_nets)
    ######################

    ######################
    # add back in netify attributes
    obj_attrs2 = obj_attrs
    obj_attrs2$ego_netify = TRUE
    obj_attrs2$threshold = threshold
    obj_attrs2$ngbd_direction = ngbd_direction    
    obj_attrs2$include_ego = include_ego
    obj_attrs2$ego_longit = longitudinal
    obj_attrs2$ego_vec = paste(ego, collapse = ', ')
    obj_attrs2$ego_entry = ego

    # get sub netlet attribs
    # if longit list then use first element
    if(netlet_type == 'longit_list'){
        subobj_attrs = attributes(netlet[[1]])
        subobj_attrs$ego_netify = TRUE
        subobj_attrs$threshold = threshold
        subobj_attrs$ngbd_direction = ngbd_direction    
        subobj_attrs$include_ego = include_ego
        subobj_attrs$ego_longit = FALSE
        subobj_attrs$ego_vec = paste(ego, collapse = ', ')
        subobj_attrs$ego_entry = ego
    }
    
    # if cross sec then use the entire thing
    if(netlet_type == 'cross_sec'){ subobj_attrs = obj_attrs2 }

    # add back to each list element replacing the
    # first two attribute elements with ego_nets
    ego_list_ids = names(ego_nets)
    ego_nets = lapply(1:length(ego_nets), function(ii){

        # get net
        net = ego_nets[[ii]]

        # add info about ego and pd if relev
        if(longitudinal){
            # add ego id to subobj_attrs
            subobj_attrs$ego_id  = gsub(
                '__', ': ', ego_list_ids[ii], fixed=TRUE)
            # add ego id to subobj_attrs
            subobj_attrs$ego_vec = subobj_attrs$ego_id
            subobj_attrs$ego_entry = subobj_attrs$ego_id }
                
        # non longit case
        if(!longitudinal){
            subobj_attrs$ego_id = ego_list_ids[ii]
            subobj_attrs$ego_vec = subobj_attrs$ego_id
            subobj_attrs$ego_entry = subobj_attrs$ego_id }
        
        # pull out new dim and dimnames for net
        new_dims = attributes(net)[c('dim', 'dimnames')]

        # replace first two elements of subobj_attrs
        subobj_attrs[c('dim','dimnames')] = new_dims
        attributes(net) = subobj_attrs
        return(net) })
    names(ego_nets) = ego_list_ids

    # if netify type is not cross sec then add
    # obj_attrs2 to top level list
    if(netlet_type != 'cross_sec'){

        # replace names column in obj_attrs2
        obj_attrs2$names = ego_list_ids

        # add attribs to top level
        attributes(ego_nets) = obj_attrs2

        # add attrib so this this is explicitly longit
        obj_attrs2$ego_longit = TRUE }

    # if cross sec and only one element then
    # pull out from list
    if(netlet_type == 'cross_sec' & length(ego_nets)==1){
        ego_nets = ego_nets[[1]] }
    
    # if origingally cross sec and more than one ego
    # was provided modify the top level appropriately
    if(netlet_type == 'cross_sec' & length(ego)>1){

        # drop dim and dimnames from obj_attrs2
        obj_attrs2 = obj_attrs2[-match(
            c('dim','dimnames'), names(obj_attrs2))]
        
        # modify obj_attrs2 to longit_list
        obj_attrs2$netify_type = 'longit_list'

        # add attrib so this doesnt get confused as longit
        obj_attrs2$ego_longit = FALSE

        # add back to ego_nets while keeping names
        attributes(ego_nets) = c(attributes(ego_nets), obj_attrs2) }
    ######################

    ######################
    return(ego_nets)
    ###################### 
}