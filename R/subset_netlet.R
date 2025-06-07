#' subset method for netify objects
#'
#' This function takes in a netify object
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
#' @aliases subset_netify
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

    # pull out attrs and msrmnts of original - cache for efficiency
    obj_attrs <- attributes(netlet)
    msrmnts <- netify_measurements(netlet)
    nlayers <- length(obj_attrs$layers)
    netify_type <- obj_attrs$netify_type
    
    # cache logical checks
    multilayer_logic_orig <- nlayers > 1
    longit_logic_in <- netify_type != 'cross_sec'

    # check if output should be multilayer
    nlayers_subset <- if(!is.null(what_layers_to_subset)) {
        length(what_layers_to_subset)
    } else {
        nlayers
    }
    multilayer_logic_out <- nlayers_subset > 1

    # check if input was longitudinal and if it's still longit after user input
    if(longit_logic_in){
        new_pds <- if(is.null(when_to_subset)) 2 else length(when_to_subset)
        longit_logic_out <- new_pds > 1        
    } else { 
        longit_logic_out <- longit_logic_in 
    }

    # use peek to subset data
    sub_net <- peek(
        netlet, 
        what_to_peek=what_to_subset,
        what_rows_to_peek=what_rows_to_subset,
        what_cols_to_peek=what_cols_to_subset,
        when_to_peek=when_to_subset,
        what_layers_to_peek=what_layers_to_subset )

    # longit check for list, if data is longitudinal and one time
    # period was subsetted then need to extract out of list
    # and also adjust obj_attrs to reflect that the 
    # subsetted element is now a matrix instead of a list
    if(longit_logic_in){
        if(is.list(sub_net) && length(sub_net) == 1){
            
            # extract out of list
            sub_net <- sub_net[[1]]
            
            # adjust obj_attrs
            obj_attrs <- obj_attrs[-1]
            obj_attrs <- append(
                obj_attrs,  attributes(sub_net),
                after=0 )
        }
    }

    # longit check for array, if data is longitudional and one time
    # period was subsetted then make necessary changes to obj_attrs
    if(longit_logic_in){
        if(netify_type == 'longit_array' && !longit_logic_out){
            # adjust netify_type
            obj_attrs$netify_type <- 'cross_sec'
        }
    }

    # if user puts NULL for time and object
    # is longit then change when_to_subset to all time points
    if(is.null(when_to_subset) && netify_type != 'cross_sec'){
        when_to_subset <- msrmnts$time 
    }

    # add back in netify attributes
    obj_attrs2 <- obj_attrs

    # modify layers label if new layers
    # were provided, subsetting was 
    # already done beforehand 
    # additionally change weights label in attribs
    if(!is.null(what_layers_to_subset)){
        obj_attrs2$layers <- what_layers_to_subset

        # orig values - cached for efficiency
        orig_weight <- strsplit(obj_attrs2$weight, ', ')[[1]]
        orig_detail <- strsplit(obj_attrs2$detail_weight, ' | ', fixed=TRUE)[[1]]
        orig_weight_binary <- obj_attrs2$weight_binary        
        orig_diag_to_NA <- obj_attrs2$diag_to_NA
        orig_missing_to_zero <- obj_attrs2$missing_to_zero

        # figure out which to keep based on input in what_layers_to_subset and 
        # its index position in the original layers
        toKeep <- match(what_layers_to_subset, obj_attrs2$layers)

        # reconstruct based on what layers selected
        obj_attrs2$weight <- paste(orig_weight[toKeep], collapse=', ')
        obj_attrs2$detail_weight <- paste(orig_detail[toKeep], collapse=' | ')
        obj_attrs2$weight_binary <- orig_weight_binary[toKeep]
        obj_attrs2$diag_to_NA <- orig_diag_to_NA[toKeep]
        obj_attrs2$missing_to_zero <- orig_missing_to_zero[toKeep]    
    }

    # cache list check for efficiency
    is_list_sub_net <- is.list(sub_net)

    # new object: longit list
    if(is_list_sub_net){
    
        # pull attributes from a cross-sec in the list
        crossSec_obj_attrs <- attributes(netlet[[1]])
        # sub_dims <- attributes(sub_net[[1]])

        # modify crossSec_obj_attrs to account for new layers if relevant
        crossSec_obj_attrs$layers <- obj_attrs2$layers
        crossSec_obj_attrs$weight <- obj_attrs2$weight
        crossSec_obj_attrs$detail_weight <- obj_attrs2$detail_weight
        crossSec_obj_attrs$weight_binary <- obj_attrs2$weight_binary
        crossSec_obj_attrs$diag_to_NA <- obj_attrs2$diag_to_NA
        crossSec_obj_attrs$missing_to_zero <- obj_attrs2$missing_to_zero
    
        # # adjust actor composition
        # crossSec_obj_attrs[1:2] <- sub_dims[1:2]
        
        # apply change to each element in subsetted list - optimized
        crossSec_attrs_to_add <- crossSec_obj_attrs[-(1:2)]
        sub_net <- lapply(sub_net, function(x){
            new_attribs <- append(attributes(x), crossSec_attrs_to_add)
            attributes(x) <- new_attribs
            return(x) 
        })
        
        # list level attributes
        # adjust years
        obj_attrs2$names <- names(sub_net)
    }

    # new object: cross-sectional/longit array/multilayer
    if(!is_list_sub_net){
    
        # adjust dimensions
        obj_attrs2[1:2] <- attributes(sub_net)[1:2]
        
        # cache dimension length for efficiency
        sub_net_dims <- length(dim(sub_net))
        
        # adjust netify_type
        if(sub_net_dims == 2 && !multilayer_logic_out){
            obj_attrs2$netify_type <- 'cross_sec' 
        }
        
        # related mod for multilayer net
        if(sub_net_dims == 3 && multilayer_logic_out){
            obj_attrs2$netify_type <- 'cross_sec' 
        }  
    }

    # get actors in subsetted netlet - optimized
    if(is_list_sub_net){
        sub_actors <- unique(unlist(lapply(sub_net, rownames), use.names = FALSE))
    } else {
        sub_actors <- rownames(sub_net)
    }

    # adjust actor periods - vectorized operation
    actor_pds_subset <- obj_attrs$actor_pds[obj_attrs$actor_pds$actor %in% sub_actors, ]
    
    if(netify_type != 'cross_sec'){
        # vectorized min/max operations
        when_min <- min(when_to_subset)
        when_max <- max(when_to_subset)
        
        actor_pds_subset$min_time <- pmax(actor_pds_subset$min_time, when_min)
        actor_pds_subset$max_time <- pmin(actor_pds_subset$max_time, when_max)
    }
    obj_attrs2$actor_pds <- actor_pds_subset

    # adjust nodal_data - optimized logical operations
    nodal_data_orig <- obj_attrs$nodal_data
    if(!is.null(nodal_data_orig)){
        
        # create logical vectors for efficient subsetting
        actor_filter <- nodal_data_orig$actor %in% sub_actors
        
        if(netify_type != 'cross_sec'){
            time_filter <- nodal_data_orig$time %in% when_to_subset
            combined_filter <- actor_filter & time_filter
        } else {
            combined_filter <- actor_filter
        }
        
        obj_attrs2$nodal_data <- nodal_data_orig[combined_filter, , drop=FALSE]
    }

    # adjust dyad_data for new structure
    dyad_data_orig <- obj_attrs$dyad_data
    if(!is.null(dyad_data_orig)){
    
        # first subset time if longit
        if(netify_type != 'cross_sec'){
            obj_attrs2$dyad_data <- dyad_data_orig[when_to_subset] 
        } else {
            obj_attrs2$dyad_data <- dyad_data_orig
        }
        
        # then iterate through and subset actors for new structure
        # New structure: list(time) -> list(vars) -> matrix
        obj_attrs2$dyad_data <- lapply(
            obj_attrs2$dyad_data, function(time_period_data){
                
                # time_period_data is list(vars) -> matrix
                lapply(time_period_data, function(var_matrix){
                    # var_matrix is an individual matrix
                    toKeep_rows <- intersect(rownames(var_matrix), sub_actors)
                    toKeep_cols <- intersect(colnames(var_matrix), sub_actors)
                    var_matrix[toKeep_rows, toKeep_cols, drop=FALSE]
                })
            }
        )
    }

    # add back in netify attributes
    attributes(sub_net) <- obj_attrs2

    # return object
    return(sub_net)
}

#' @rdname subset_netlet
#' @export
subset_netify <- subset_netlet