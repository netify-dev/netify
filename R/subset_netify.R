#' Subset netify objects
#'
#' Extracts a subset of a netify object based on specified actors, time periods, 
#' and/or layers while preserving all netify attributes and structure.
#'
#' @param x A netify object to subset
#' @param actors Character vector of actor names or numeric indices to subset. 
#'   Extracts the subgraph among these actors (includes ties both from and to 
#'   these actors). Default is NULL, which includes all actors.
#' @param time Time periods to subset. Can be:
#'   \itemize{
#'     \item Numeric vector: used as indices to the time dimension
#'     \item Character vector: matched against time dimension labels
#'     \item NULL: includes all time periods (default)
#'   }
#' @param layers Character vector of layer names to subset from multilayer networks. 
#'   For single-layer networks, this is ignored. For multilayer networks, at least 
#'   one layer must be specified.
#' @param from Character vector of actor names or numeric indices for actors sending 
#'   ties (row actors). Overrides \code{actors}. Set to NULL to include all 
#'   sending actors. In bipartite networks, this refers to actors in the first mode.
#' @param to Character vector of actor names or numeric indices for actors receiving 
#'   ties (column actors). Overrides \code{actors}. Set to NULL to include all 
#'   receiving actors. In bipartite networks, this refers to actors in the second mode.
#' @param ... Additional arguments (currently unused)
#'   
#' @return A netify object containing the requested subset with:
#'   \itemize{
#'     \item Subsetted adjacency matrix/matrices
#'     \item Corresponding nodal attributes (filtered to included actors/times)
#'     \item Corresponding dyadic attributes (filtered to included actor pairs/times)
#'     \item Updated netify attributes reflecting the new dimensions
#'   }
#'   
#'   The returned object's structure depends on the subset:
#'   \itemize{
#'     \item If one time period is selected from longitudinal data, returns cross-sectional
#'     \item If one layer is selected from multilayer data, returns single-layer
#'     \item Otherwise, maintains the original structure type
#'   }
#'
#' @details
#' This function is a netify-aware wrapper around the \code{\link{peek}} function,
#' which handles the raw data extraction. While \code{peek} returns raw matrices/arrays,
#' \code{subset} additionally:
#' \itemize{
#'   \item Preserves and updates all netify attributes
#'   \item Filters nodal and dyadic attribute data to match the subset
#'   \item Adjusts the netify type when dimensions change (e.g., longitudinal to cross-sectional)
#'   \item Maintains consistency between network data and attributes
#' }
#' 
#' The \code{from} and \code{to} parameters allow precise control over which ties 
#' to include:
#' \itemize{
#'   \item Use \code{actors} to get all ties among a set of actors (subgraph extraction)
#'   \item Use \code{from} to get all ties sent by specific actors
#'   \item Use \code{to} to get all ties received by specific actors
#'   \item Use both \code{from} and \code{to} to get ties between specific sets of actors
#' }
#' 
#' For bipartite networks, \code{from} refers to actors in the first mode (e.g., 
#' people) and \code{to} refers to actors in the second mode (e.g., organizations).
#'
#' @note 
#' When subsetting longitudinal data to a single time period, the function automatically
#' converts the result to a cross-sectional netify object. Similarly, subsetting 
#' multilayer data to a single layer produces a single-layer object.
#' 
#' @examples
#'
#' # load example directed event data from ICEWS
#' data(icews)
#' 
#' # generate a longitudional netify object 
#' # with both dyadic and nodal attributes
#' icews_matlConf <- netify(
#'     icews,
#'     actor1='i', actor2='j', time='year',
#'     symmetric=FALSE, weight='matlConf',
#'     nodal_vars=c('i_polity2', 'i_log_gdp', 'i_log_pop'),
#'     dyad_vars=c('matlCoop', 'verbCoop', 'verbConf'),
#'     dyad_vars_symmetric=c(FALSE, FALSE, FALSE) )  
#' 
#' # subset to a few countries using S3 method
#' icews_subset <- subset(
#'    icews_matlConf,
#'    actors=c('United States', 'United Kingdom',
#'             'Russian Federation', 'China') )
#' 
#' # subset to a few countries and a few years
#' icews_subset_2 <- subset(
#'   icews_matlConf,
#'   actors=c('United States', 'United Kingdom',
#'            'Russian Federation', 'China'),
#'   time=c('2010', '2011') )
#' 
#' # can also use subset_netify directly
#' icews_subset_3 <- subset_netify(
#'   netlet=icews_matlConf,
#'   actors=c('United States', 'United Kingdom',
#'            'Russian Federation', 'China'),
#'   time=c('2010', '2011') )
#' 
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @method subset netify
#' @export

subset.netify <- function(
    x, 
    actors = NULL,
    time = NULL,
    layers = NULL,
    from = NULL,
    to = NULL,
    ...
){
    # call subset_netify to handle nitty gritty
    subset_netify(
        netlet = x,
        actors = actors,
        time = time,
        layers = layers,
        from = from,
        to = to
    )
}

#' Internal subset function for netify objects
#'
#' This is the internal workhorse function called by the S3 method \code{subset.netify}.
#' Users should typically use \code{subset()} on netify objects rather than calling 
#' this function directly.
#'
#' @param netlet A netify object to subset
#' @param actors Character vector of actor names or numeric indices to subset
#' @param time Time periods to subset
#' @param layers Character vector of layer names to subset from multilayer networks
#' @param from Character vector of actor names or numeric indices for actors sending ties
#' @param to Character vector of actor names or numeric indices for actors receiving ties
#'
#' @return A netify object containing the requested subset
#'
#' @keywords internal
#' @export subset_netify

subset_netify <- function(
    netlet, 
    actors = NULL,
    time = NULL,
    layers = NULL,
    from = NULL,
    to = NULL
){

    # check if netify object
    netify_check(netlet)    

    # pull out attrs and msrmnts of original - cache for efficiency
    obj_attrs <- attributes(netlet)
    msrmnts <- netify_measurements(netlet)
    nlayers <- length(obj_attrs$layers)
    netify_type <- obj_attrs$netify_type
    
    # if longit_list with only one time period, flatten it
    if(netify_type == 'longit_list' && length(netlet) == 1){
        cli::cli_alert_warning(
            "Input is a longitudinal list with only one time period. Converting to cross-sectional."
        )
        # extract the single element
        netlet <- netlet[[1]]
        # update attributes
        obj_attrs <- attributes(netlet)
        netify_type <- obj_attrs$netify_type
        msrmnts <- netify_measurements(netlet)
    }
    
    # set from/to defaults based on actors parameter
    if(is.null(from) && !is.null(actors)) from <- actors
    if(is.null(to) && !is.null(actors)) to <- actors
    
    # cache logical checks
    is_multilayer_orig <- nlayers > 1
    is_longit <- netify_type != 'cross_sec'
    
    # check if output should be multilayer
    nlayers_subset <- if(!is.null(layers)) length(layers) else nlayers
    is_multilayer_out <- nlayers_subset > 1

    # check if output should be longitudinal
    if(is_longit){
        # if user puts null for time and object is longit, use all time points
        if(is.null(time)) {
            time <- msrmnts$time
        }
        n_time_out <- length(time)
        is_longit_out <- n_time_out > 1        
    } else { 
        is_longit_out <- FALSE 
    }

    # use peek to subset data with new parameter names
    sub_net <- peek(
        netlet, 
        from = from,
        to = to,
        time = time,
        layers = layers
    )

    # for longit_list, peek returns raw matrices
    # we need to rebuild the proper netify structure
    if(netify_type == 'longit_list' && is.list(sub_net)){
        # get a reference element to copy attributes from
        ref_elem <- netlet[[1]]
        ref_attrs <- attributes(ref_elem)
        
        # rebuild each element with proper netify attributes
        sub_net <- lapply(names(sub_net), function(time_name) {
            mat <- sub_net[[time_name]]
            
            # start with the matrix attributes (dim, dimnames)
            mat_attrs <- attributes(mat)
            
            # add all the netify attributes from reference, except dim/dimnames
            netify_attrs <- ref_attrs[!names(ref_attrs) %in% c('dim', 'dimnames')]
            
            # combine attributes
            all_attrs <- c(mat_attrs, netify_attrs)
            
            # apply attributes
            attributes(mat) <- all_attrs
            
            # ensure class is set
            class(mat) <- 'netify'
            
            return(mat)
        })
        names(sub_net) <- names(peek(netlet, from = from, to = to, time = time, layers = layers))
    }

    # handle structure changes when going from longit to cross-sectional
    extracted_single_time <- FALSE
    if(is_longit && !is_longit_out){
        if(is.list(sub_net) && length(sub_net) == 1){
            # extract from list
            sub_net <- sub_net[[1]]
            extracted_single_time <- TRUE
        } else if(netify_type == 'longit_array'){
            # update type for arrays
            obj_attrs$netify_type <- 'cross_sec'
        }
    }

    # create new attributes object
    if(extracted_single_time) {
        # use the extracted element's attributes as base
        new_attrs <- attributes(sub_net)
        
        # update actor_pds, nodal_data, dyad_data from parent if needed
        if(!is.null(obj_attrs$actor_pds)) {
            new_attrs$actor_pds <- obj_attrs$actor_pds
        }
        if(!is.null(obj_attrs$nodal_data)) {
            new_attrs$nodal_data <- obj_attrs$nodal_data
        }
        if(!is.null(obj_attrs$dyad_data)) {
            new_attrs$dyad_data <- obj_attrs$dyad_data
        }
    } else {
        # normal case - copy original attributes
        new_attrs <- obj_attrs
    }
    
    # update layer-related attributes if layers were subsetted
    if(!is.null(layers)){
        new_attrs$layers <- layers
        
        # find indices of selected layers efficiently
        layer_idx <- match(layers, obj_attrs$layers)
        
        # update all layer-specific attributes at once
        if(length(obj_attrs$weight) > 1){
            weight_vec <- strsplit(obj_attrs$weight, ', ')[[1]]
            new_attrs$weight <- paste(weight_vec[layer_idx], collapse=', ')
        }
        
        if(length(obj_attrs$detail_weight) > 1){
            detail_vec <- strsplit(obj_attrs$detail_weight, ' \\| ', perl=TRUE)[[1]]
            new_attrs$detail_weight <- paste(detail_vec[layer_idx], collapse=' | ')
        }
        
        # update logical vectors
        for(attr_name in c('weight_binary', 'diag_to_NA', 'missing_to_zero')){
            if(!is.null(obj_attrs[[attr_name]]) && length(obj_attrs[[attr_name]]) > 1){
                new_attrs[[attr_name]] <- obj_attrs[[attr_name]][layer_idx]
            }
        }
    }

    # get actors in subsetted netlet efficiently
    if(is.list(sub_net) && is_longit_out){
        # for lists, get unique actors across all time periods at once
        all_actors <- unique(unlist(lapply(sub_net, rownames), use.names = FALSE))
        
        # update list-specific attributes
        new_attrs$names <- names(sub_net)
        
        # update attributes for each list element if needed
        if(is_multilayer_orig || !is.null(layers)){
            sub_net <- lapply(sub_net, function(x){
                elem_attrs <- attributes(x)
                # update layer-related attributes in each element
                elem_attrs$layers <- new_attrs$layers
                elem_attrs$weight <- new_attrs$weight
                elem_attrs$detail_weight <- new_attrs$detail_weight
                elem_attrs$weight_binary <- new_attrs$weight_binary
                elem_attrs$diag_to_NA <- new_attrs$diag_to_NA
                elem_attrs$missing_to_zero <- new_attrs$missing_to_zero
                # reapply attributes
                attributes(x) <- elem_attrs
                return(x)
            })
        }
    } else {
        all_actors <- rownames(sub_net)
        
        # update dimensions if not already done
        if(!extracted_single_time) {
            new_attrs[1:2] <- attributes(sub_net)[1:2]
        }
        
        # update netify_type based on dimensions
        n_dims <- length(dim(sub_net))
        if(n_dims == 2 && !is_multilayer_out){
            new_attrs$netify_type <- 'cross_sec' 
        } else if(n_dims == 3 && is_multilayer_out && !is_longit_out){
            new_attrs$netify_type <- 'cross_sec' 
        }
    }

    # update actor_pds efficiently using vectorized operations
    if(!is.null(new_attrs$actor_pds)){
        actor_pds <- new_attrs$actor_pds
        actor_pds_subset <- actor_pds[actor_pds$actor %in% all_actors, , drop = FALSE]
        
        if(is_longit && !is_longit_out){
            # update time bounds
            if(is.numeric(time)){
                time_val <- time
            } else {
                time_val <- which(msrmnts$time %in% time)
            }
            actor_pds_subset$min_time <- pmax(actor_pds_subset$min_time, min(time_val))
            actor_pds_subset$max_time <- pmin(actor_pds_subset$max_time, max(time_val))
        }
        new_attrs$actor_pds <- actor_pds_subset
    }

    # update nodal_data efficiently
    if(!is.null(new_attrs$nodal_data)){
        nodal_data <- new_attrs$nodal_data
        
        # create filter once
        if(is_longit && !extracted_single_time){
            keep_rows <- nodal_data$actor %in% all_actors & nodal_data$time %in% time
        } else {
            keep_rows <- nodal_data$actor %in% all_actors
        }
        
        new_attrs$nodal_data <- nodal_data[keep_rows, , drop = FALSE]
    }

    # update dyad_data efficiently
    if(!is.null(new_attrs$dyad_data)){
        dyad_data <- new_attrs$dyad_data
        
        # subset time periods first if needed
        if(is_longit && !extracted_single_time){
            dyad_data <- dyad_data[as.character(time)]
        }
        
        # for single time extraction, get the right time period
        if(extracted_single_time && length(dyad_data) > 1){
            dyad_data <- dyad_data[as.character(time)]
        }
        
        # process each time period
        new_attrs$dyad_data <- lapply(dyad_data, function(time_data){
            # process each variable matrix
            lapply(time_data, function(var_matrix){
                # get actors that exist in this matrix
                toKeep_rows <- intersect(rownames(var_matrix), all_actors)
                toKeep_cols <- intersect(colnames(var_matrix), all_actors)
                # use direct indexing with actor names
                var_matrix[toKeep_rows, toKeep_cols, drop = FALSE]
            })
        })
    }

    # apply attributes based on object type
    if(is.list(sub_net) && is_longit_out){
        # for longitudinal lists, apply list-level attributes
        attributes(sub_net) <- new_attrs
        # ensure list has netify class
        class(sub_net) <- "netify"
    } else {
        # for single matrices/arrays, apply all attributes at once
        attributes(sub_net) <- new_attrs
        # ensure netify class
        if(!inherits(sub_net, "netify")) {
            class(sub_net) <- c("netify", class(sub_net))
        }
    }

    return(sub_net)
}