#' Create Ego Networks from a 'netify' Object
#'
#' `ego_netlet` extracts ego networks from a given 'netify' object. An ego network is the subgraph consisting of a focal node (ego) and its immediate neighbors (alters) in the context of an unweighted network. For weighted networks, users can choose a threshold to define the neighborhood of the ego. The function returns a list of 'netify' objects, each representing an ego network. For oders of the neighborhood greater than 1, we recommend the `ego` function from the 'igraph' package.
#'
#' @param netlet A 'netify' object
#' @param ego A character vector specifying the name(s) of the ego(s) for whom to create the ego networks.
#' @param threshold A numeric value specifying the threshold for including alters in the ego network. The threshold is used to define the neighborhood of the ego in weighted networks. Default for unweighted networks is 0 and default for weighted networks is the average edge weight.
#' @param ngbd_direction For directed networks users can provide a character string specifying the type of relationship that the ego should have with alters to be considered neighbors. Options are that the neighborhood for an ego will be alters that it has an outgoing tie with ("out"), incoming tie from ("in"), or any tie ("any"). Default is "any".
#' @param include_ego Logical; if TRUE, the ego node will be included in the ego network. Default is TRUE.
#'
#' @return A list of 'netify' objects, each representing an ego network.
#'
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @export ego_netlet
#' @export
#' 

ego_netlet = function(
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
            `ego_netlet` does not currently support multilayer `netify` inputs. 
            Please use the `subset_netlet` function to create a `netify` object with a single layer.' )
		stop() }

    # stop if bipartite and say we dont support
    if(bipartite){
		cli::cli_alert_danger(
            'Error: This object is bipartite. 
            `ego_netlet` does not currently support bipartite `netify` inputs. 
            Please use the `subset_netlet` function to create a `netify` object with a single layer.' )
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
    obj_attrs2$ego_netlet = TRUE
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
        subobj_attrs$ego_netlet = TRUE
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