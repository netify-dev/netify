#' Transform edge weights in a netify object
#'
#' @param netlet A netify object
#' @param transform_fn A function to apply to the weights (e.g., log, sqrt)
#' @param add_constant Numeric value to add before transformation (useful for log)
#' @param new_name Optional new name for the weight variable
#' @param keep_original Logical, whether to keep the original weight as a dyadic variable
#'
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @return A netify object with transformed weights
#' @export transform_weights

transform_weights <- function(
    netlet, transform_fn = NULL,
    add_constant = 0, new_name = NULL,
    keep_original = TRUE
){

    # check if the input is a valid netify object
    netify_check(netlet)
    weight_var <- attr(netlet, 'weight')
    
    # ensure the netify object has a weight variable
    if (is.null(weight_var)) {
        cli::cli_abort("no weight variable found in netify object")
    }
    
    # get the raw network data
    raw_net <- get_raw(netlet)
    
    # determine the type of netify object (e.g., cross-sectional, longitudinal)
    netify_type <- attr(netlet, 'netify_type')
    
    # store the original weight_binary status
    original_weight_binary <- attr(netlet, 'weight_binary')
    
    if (netify_type == 'cross_sec') {
        # if requested, save the original weights as a dyadic variable
        if (keep_original) {
            orig_data <- data.frame(
                actor1 = rep(rownames(raw_net), ncol(raw_net)),
                actor2 = rep(colnames(raw_net), each = nrow(raw_net)),
                original_weight = as.vector(raw_net)
            )
            
            netlet <- add_dyad_vars(
                netlet, 
                dyad_data = orig_data,
                actor1 = 'actor1', 
                actor2 = 'actor2',
                dyad_vars = 'original_weight',
                dyad_vars_symmetric = attr(netlet, 'symmetric')
            )
        }
        
        # apply the transformation function to the weights
        raw_net <- raw_net + add_constant
        if (!is.null(transform_fn)) {
            raw_net <- transform_fn(raw_net)
        }
        
        # check if the transformation resulted in binary values
        new_weight_binary <- all(as.vector(raw_net) %in% c(0, 1, NA))
        
        # update the raw data in the netify object
        netlet[,] <- raw_net
        
        # update the weight_binary attribute
        attr(netlet, 'weight_binary') <- new_weight_binary
        
    } else if (netify_type == 'longit_array') {
        # handle 3d array (longitudinal data)
        if (keep_original) {
            # note: storing original weights for longitudinal arrays is not implemented
            cli::cli_alert_info("keeping original weights for longitudinal arrays not yet implemented")
        }
        
        # track binary status for each time period
        bin_check <- logical(dim(raw_net)[3])
        
        # apply the transformation to each time slice
        for (t in 1:dim(raw_net)[3]) {
            slice <- raw_net[,,t] + add_constant
            if (!is.null(transform_fn)) {
                raw_net[,,t] <- transform_fn(slice)
            }
            # check if this time slice is binary
            bin_check[t] <- all(as.vector(raw_net[,,t]) %in% c(0, 1, NA))
        }
        netlet[,,] <- raw_net
        
        # update weight_binary attribute (true only if all time periods are binary)
        attr(netlet, 'weight_binary') <- all(bin_check)
        
    } else if (netify_type == 'longit_list') {
        # handle list of matrices (longitudinal data)
        bin_check <- logical(length(netlet))
        
        for (i in seq_along(netlet)) {
            if (keep_original) {
                # save original weights as a dyadic variable for this time period
                mat <- netlet[[i]]
                orig_data <- data.frame(
                    actor1 = rep(rownames(mat), ncol(mat)),
                    actor2 = rep(colnames(mat), each = nrow(mat)),
                    time = names(netlet)[i],
                    original_weight = as.vector(mat)
                )
                
                netlet <- add_dyad_vars(
                    netlet,
                    dyad_data = orig_data,
                    actor1 = 'actor1',
                    actor2 = 'actor2',
                    time = 'time',
                    dyad_vars = 'original_weight',
                    dyad_vars_symmetric = attr(netlet, 'symmetric')
                )
            }
            
            # apply the transformation to the matrix
            mat <- get_raw(netlet[[i]]) + add_constant
            if (!is.null(transform_fn)) {
                mat <- transform_fn(mat)
            }
            
            # check if this matrix is binary
            bin_check[i] <- all(as.vector(mat) %in% c(0, 1, NA))
            
            # preserve attributes when updating the matrix
            old_attrs <- attributes(netlet[[i]])
            netlet[[i]][,] <- mat
            
            # restore any attributes that might have been lost
            for (a in names(old_attrs)) {
                if (!(a %in% c("dim", "dimnames"))) {
                    attr(netlet[[i]], a) <- old_attrs[[a]]
                }
            }
            
            # update weight_binary for this specific matrix
            attr(netlet[[i]], 'weight_binary') <- bin_check[i]
        }
        
        # update overall weight_binary attribute (true only if all periods are binary)
        attr(netlet, 'weight_binary') <- all(bin_check)
    }
    
    # update the weight name if a new name is provided
    if (!is.null(new_name)) {
        attr(netlet, 'weight') <- new_name
        
        # update detail_weight based on whether network is now binary
        if (attr(netlet, 'weight_binary')) {
            if (original_weight_binary) {
                # was binary, still binary
                attr(netlet, 'detail_weight') <- paste0(new_name, ' (transformed binary)')
            } else {
                # was weighted, now binary
                attr(netlet, 'detail_weight') <- paste0(new_name, ' (binarized)')
                cli::cli_alert_info("network has been binarized through transformation")
            }
        } else {
            if (original_weight_binary) {
                # was binary, now weighted
                attr(netlet, 'detail_weight') <- paste0(new_name, ' (weighted from binary)')
                cli::cli_alert_info("binary network has been converted to weighted through transformation")
            } else {
                # was weighted, still weighted
                attr(netlet, 'detail_weight') <- paste0(new_name, ' (transformed)')
            }
        }
    } else {
        # no new name provided, but still update detail_weight if binary status changed
        if (attr(netlet, 'weight_binary') != original_weight_binary) {
            current_weight <- attr(netlet, 'weight') %||% 'weight'
            if (attr(netlet, 'weight_binary')) {
                attr(netlet, 'detail_weight') <- paste0(current_weight, ' (binarized)')
                cli::cli_alert_info("network has been binarized through transformation")
            } else {
                attr(netlet, 'detail_weight') <- paste0(current_weight, ' (weighted from binary)')
                cli::cli_alert_info("binary network has been converted to weighted through transformation")
            }
        }
    }
    
    # return the updated netify object
    return(netlet)
}