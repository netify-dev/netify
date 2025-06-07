#' Convert list of dependent variable(s) into an array
#' 
#' Mainly for use in going from a list netlet to an array netlet 
#' for bridging netlet objects to analysis with packages that
#' expect matrix/array inputs
#'
#' @param netlet netify object
#' @return An array object of dimensions nr x nc x t,
#' where nr is the number of row actors, nc is the number 
#' of column actors, and t is the number of time periods. 
#' #' @author Shahryar Minhas
#' 
#' @keywords internal
#' @noRd

longit_dv_to_arr <- function(netlet){

    # make sure it's a netify object
    netify_check(netlet)

    # pull out object to collapse into an array
    array_list <- get_raw(netlet)

    # get dimensions and type
    msrmnts <- netify_measurements(netlet)
    netlet_type <- attr(netlet, 'netify_type')

    # check if actor time is uniform
    actor_unif <- attr(netlet, 'actor_time_uniform')

    # longit_list + actor_unif case
    if(netlet_type == 'longit_list' & actor_unif){

        # set up array to fill in
        arr_dim <- c(
            msrmnts$n_row_actors[[1]],
            msrmnts$n_col_actors[[1]],
            msrmnts$n_time )
        arr_labs <- list(
            msrmnts$row_actors[[1]],
            msrmnts$col_actors[[1]],
            msrmnts$time )
        arr <- array(NA, arr_dim, dimnames=arr_labs)

        # fill in array
        for(tt in msrmnts$time){
            arr[,,tt] = array_list[[tt]] }
        return(arr) }

    # longit_list + !actor_unif case
    if(netlet_type == 'longit_list' & !actor_unif){

        # set up array to fill in            
        row_actors <- sort(unique(unlist(msrmnts$row_actors)))
        n_row_actors <- length(row_actors)
        col_actors <- sort(unique(unlist(msrmnts$col_actors)))
        n_col_actors <- length(col_actors)
        arr_dim <- c(
            n_row_actors,
            n_col_actors,
            msrmnts$n_time )
        arr_labs <- list(
            row_actors,
            col_actors,
            msrmnts$time )
        arr <- array(NA, arr_dim, dimnames=arr_labs)
        
        # fill in array
        for(tt in msrmnts$time){
            # pull out arr from list
            to_add <- array_list[[tt]]
            to_add_rows <- rownames(to_add)
            to_add_cols <- colnames(to_add)
            arr[to_add_rows, to_add_cols, tt] = to_add }
        return(arr) }
}

#' Convert list of dyadic arrays into an array
#' 
#' Mainly for use in going from a list netlet to an array netlet 
#' for bridging netlet objects to analysis with packages that
#' expect matrix/array inputs
#'
#' @param netlet netify object
#' @return An array object of dimensions nr x nc x pn x t,
#' where nr is the number of row actors, nc is the number 
#' of column actors, pn is the number of dyadic variables,
#' and t is the number of time periods.
#' #' @author Shahryar Minhas
#' 
#' @keywords internal
#' @noRd

longit_dyad_to_arr <- function(netlet){

    # make sure it's a netify object
    netify_check(netlet)

    # pull out object to collapse into an array
    array_list <- attr(netlet, 'dyad_data')

    # get dimensions and type - cache attributes
    msrmnts <- netify_measurements(netlet)
    netlet_attrs <- attributes(netlet)
    netlet_type <- netlet_attrs$netify_type
    actor_unif <- netlet_attrs$actor_time_uniform

    # Helper function to convert new structure to old array format
    convert_to_array <- function(time_period_data, target_rows, target_cols, dvars) {
        n_rows <- length(target_rows)
        n_cols <- length(target_cols)
        n_vars <- length(dvars)
        
        # Create array for this time period
        time_array <- array(
            NA,
            dim = c(n_rows, n_cols, n_vars),
            dimnames = list(target_rows, target_cols, dvars)
        )
        
        # Fill array with data from individual matrices
        for(i in seq_along(dvars)) {
            var_name <- dvars[i]
            if(!is.null(time_period_data[[var_name]])) {
                # Get the matrix for this variable
                var_matrix <- time_period_data[[var_name]]
                
                # Extract row/col names from matrix
                matrix_rows <- rownames(var_matrix)
                matrix_cols <- colnames(var_matrix)
                
                # Map matrix indices to target array indices
                row_indices <- match(matrix_rows, target_rows)
                col_indices <- match(matrix_cols, target_cols)
                
                # Fill in the array
                time_array[row_indices, col_indices, i] <- var_matrix
            }
        }
        
        return(time_array)
    }

    # longit_array + actor_unif case
    if(netlet_type == 'longit_array' && actor_unif){
        
        # set up array to fill in - cache dimensions
        n_row_actors <- msrmnts$n_row_actors
        n_col_actors <- msrmnts$n_col_actors
        n_dvars <- msrmnts$n_dvars
        n_time <- msrmnts$n_time
        time_periods <- msrmnts$time
        dvars <- msrmnts$dvars
        
        arr_dim <- c(n_row_actors, n_col_actors, n_dvars, n_time)
        arr_labs <- list(
            msrmnts$row_actors,
            msrmnts$col_actors,
            dvars,
            time_periods )
        arr <- array(NA, arr_dim, dimnames=arr_labs)
        
        # fill in array - optimized loop
        for(i in seq_along(time_periods)){
            tt <- time_periods[i]
            time_period_data <- array_list[[tt]]
            
            # Convert new structure to array format
            arr[,,,i] <- convert_to_array(
                time_period_data, 
                msrmnts$row_actors, 
                msrmnts$col_actors, 
                dvars
            )
        }
        return(arr) 
    }

    # longit_list + actor_unif case
    if(netlet_type == 'longit_list' && actor_unif){

        # set up array to fill in - cache dimensions
        n_row_actors <- msrmnts$n_row_actors[[1]]
        n_col_actors <- msrmnts$n_col_actors[[1]]
        n_dvars <- msrmnts$n_dvars
        n_time <- msrmnts$n_time
        time_periods <- msrmnts$time
        dvars <- msrmnts$dvars
        row_actors <- msrmnts$row_actors[[1]]
        col_actors <- msrmnts$col_actors[[1]]
        
        arr_dim <- c(n_row_actors, n_col_actors, n_dvars, n_time)
        arr_labs <- list(row_actors, col_actors, dvars, time_periods)
        arr <- array(NA, arr_dim, dimnames=arr_labs)

        # fill in array - optimized loop
        for(i in seq_along(time_periods)){
            tt <- time_periods[i]
            time_period_data <- array_list[[tt]]
            
            # Convert new structure to array format
            arr[,,,i] <- convert_to_array(
                time_period_data, 
                row_actors, 
                col_actors, 
                dvars
            )
        }
        return(arr) 
    }
    
    # longit_list + !actor_unif case
    if(netlet_type == 'longit_list' && !actor_unif){

        # set up array to fill in - optimized unique operations
        all_row_actors <- unlist(msrmnts$row_actors, use.names = FALSE)
        all_col_actors <- unlist(msrmnts$col_actors, use.names = FALSE)
        row_actors <- sort(unique(all_row_actors))
        col_actors <- sort(unique(all_col_actors))
        n_row_actors <- length(row_actors)
        n_col_actors <- length(col_actors)
        n_dvars <- msrmnts$n_dvars
        n_time <- msrmnts$n_time
        time_periods <- msrmnts$time
        dvars <- msrmnts$dvars
        
        arr_dim <- c(n_row_actors, n_col_actors, n_dvars, n_time)
        arr_labs <- list(row_actors, col_actors, dvars, time_periods)
        arr <- array(NA, arr_dim, dimnames=arr_labs)

        # fill in array - optimized with pre-computed matches
        for(i in seq_along(time_periods)){
            tt <- time_periods[i]
            time_period_data <- array_list[[tt]]
            
            # Get actors for this time period
            period_row_actors <- msrmnts$row_actors[[tt]]
            period_col_actors <- msrmnts$col_actors[[tt]]
            
            # Convert new structure to array format for this time period
            period_array <- convert_to_array(
                time_period_data, 
                period_row_actors, 
                period_col_actors, 
                dvars
            )
            
            # Map to full actor space
            row_indices <- match(period_row_actors, row_actors)
            col_indices <- match(period_col_actors, col_actors)
            
            # Fill in the main array
            arr[row_indices, col_indices, , i] <- period_array
        }
        return(arr) 
    } 
}

#' Convert nodal attribute of netlet into an array
#' 
#' Mainly for use in going from a list netlet to an array netlet
#' for bridging netlet objects to analysis with packages that
#' expect matrix/array inputs
#' 
#' @param netlet netify object
#' @return a list object of length two, one for the row actors and 
#' another for the column actors. Each element in the list is an 
#' array of dimensions n x pn x t, where n is the number of actors,
#' pn is the number of nodal variables, and t is the number of time
#' periods. The rownames of the array are the actors, and the
#' colnames are the nodal variables.
#' @author Shahryar Minhas
#' 
#' @keywords internal
#' @noRd

longit_nodal_to_arr <- function(netlet){

    # make sure it's a netify object
    netify_check(netlet)

    # if nodal data not present return NULL
    if(is.null(attr(netlet, 'nodal_data'))){
        return(NULL) }

    # pull out object to collapse into an array
    nodal_df <- attr(netlet, 'nodal_data')

    # get dimensions and type
    msrmnts <- netify_measurements(netlet)
    netlet_type <- attr(netlet, 'netify_type')

    # check if actor time is uniform
    actor_unif <- attr(netlet, 'actor_time_uniform')

    # longit_array + actor_unif case
    if(netlet_type == 'longit_array' & actor_unif){

        # set up array to fill in for both rows and cols
        out <- lapply(c('row', 'col'), function(dlab){
            
            # set up array to fill in for particular dim
            arr_dim <- c(
                msrmnts[[paste0('n_',dlab,'_actors')]],
                msrmnts$n_nvars, msrmnts$n_time )
            arr_labs <- list(
                msrmnts[[paste0(dlab, '_actors')]],
                msrmnts$nvars, msrmnts$time )
            arr <- array(NA, arr_dim, dimnames=arr_labs)

            # fill in array
            for(tt in msrmnts$time){
                to_add <- nodal_df[nodal_df$time==tt,]
                to_add_rows <- to_add$actor
                to_add <- to_add[,msrmnts$nvars,drop=FALSE]
                to_add <- data.matrix(to_add)
                rownames(to_add) <- to_add_rows
                arr[to_add_rows,,tt] = to_add }
            return(arr) })
        #
        names(out) = c('row', 'col')
        return(out) }

    # longit_array + actor_unif case
    if(netlet_type == 'longit_list' & actor_unif){

        # set up array to fill in for both rows and cols
        out <- lapply(c('row', 'col'), function(dlab){

            # set up array to fill in for particular dim
            arr_dim <- c(
                msrmnts[[paste0('n_', dlab, '_actors')]][[1]],
                msrmnts$n_nvars,
                msrmnts$n_time )
            arr_labs <- list(
                msrmnts[[paste0(dlab, '_actors')]][[1]],
                msrmnts$nvars,
                msrmnts$time )
            arr <- array(NA, arr_dim, dimnames=arr_labs)

            # fill in array
            for(tt in msrmnts$time){
                to_add <- nodal_df[nodal_df$time==tt,]
                to_add_rows <- to_add$actor
                to_add <- to_add[,msrmnts$nvars,drop=FALSE]
                to_add <- data.matrix(to_add)                
                rownames(to_add) <- to_add_rows
                arr[to_add_rows,,tt] = to_add }            
            return(arr) })

        #
        names(out) = c('row', 'col')
        return(out) }

    # longit_list + !actor_unif case
    if(netlet_type == 'longit_list' & !actor_unif){

        # set up array to fill in for both rows and cols
        out <- lapply(c('row', 'col'), function(dlab){

            # set up array to fill in for particular dim
            actors <- sort(unique(unlist(msrmnts[[paste0(dlab,'_actors')]])))
            n_actors <- length(actors)
            arr_dim <- c(
                n_actors,
                msrmnts$n_nvars,
                msrmnts$n_time )
            arr_labs <- list(
                actors,
                msrmnts$nvars,
                msrmnts$time )
            arr <- array(NA, arr_dim, dimnames=arr_labs)

            # fill in array
            for(tt in msrmnts$time){
                to_add <- nodal_df[nodal_df$time==tt,]
                to_add_rows <- to_add$actor
                to_add <- to_add[,msrmnts$nvars,drop=FALSE]
                to_add <- data.matrix(to_add)                
                rownames(to_add) <- to_add_rows
                arr[to_add_rows,,tt] = to_add }
            return(arr) })    
            #
            names(out) = c('row', 'col')
            return(out) }
}