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

longit_dv_to_arr <- function(netlet) {
    # make sure it's a netify object
    netify_check(netlet)

    # pull out object to collapse into an array
    array_list <- get_raw(netlet)

    # get dimensions and type - cache attributes
    netlet_attrs <- attributes(netlet)
    msrmnts <- netify_measurements(netlet)
    netlet_type <- netlet_attrs$netify_type
    actor_unif <- netlet_attrs$actor_time_uniform

    # longit_list + actor_unif case
    if (netlet_type == "longit_list" && actor_unif) {
        # Cache dimensions
        n_row <- msrmnts$n_row_actors[[1]]
        n_col <- msrmnts$n_col_actors[[1]]
        n_time <- msrmnts$n_time
        time_periods <- msrmnts$time

        # set up array to fill in
        arr <- array(NA,
            dim = c(n_row, n_col, n_time),
            dimnames = list(
                msrmnts$row_actors[[1]],
                msrmnts$col_actors[[1]],
                time_periods
            )
        )

        # fill in array - use seq_along for efficiency
        for (i in seq_along(time_periods)) {
            arr[, , i] <- array_list[[time_periods[i]]]
        }
        return(arr)
    }

    # longit_list + !actor_unif case
    if (netlet_type == "longit_list" && !actor_unif) {
        # Optimize unique operations
        all_row_actors <- unlist(msrmnts$row_actors, use.names = FALSE)
        all_col_actors <- unlist(msrmnts$col_actors, use.names = FALSE)
        row_actors <- sort(unique(all_row_actors))
        col_actors <- sort(unique(all_col_actors))

        # Cache dimensions
        n_row_actors <- length(row_actors)
        n_col_actors <- length(col_actors)
        n_time <- msrmnts$n_time
        time_periods <- msrmnts$time

        # set up array to fill in
        arr <- array(NA,
            dim = c(n_row_actors, n_col_actors, n_time),
            dimnames = list(row_actors, col_actors, time_periods)
        )

        # fill in array - optimized with direct indexing
        for (i in seq_along(time_periods)) {
            tt <- time_periods[i]
            to_add <- array_list[[tt]]
            arr[rownames(to_add), colnames(to_add), i] <- to_add
        }
        return(arr)
    }
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
#' @importFrom stats setNames
#'
#' @keywords internal
#' @noRd

longit_dyad_to_arr <- function(netlet) {
    # make sure it's a netify object
    netify_check(netlet)

    # pull out object to collapse into an array
    array_list <- attr(netlet, "dyad_data")

    # get dimensions and type - cache attributes
    netlet_attrs <- attributes(netlet)
    msrmnts <- netify_measurements(netlet)
    netlet_type <- netlet_attrs$netify_type
    actor_unif <- netlet_attrs$actor_time_uniform

    # Helper function to convert new structure to old array format - optimized
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

        # Fill array with data from individual matrices - vectorized where possible
        for (i in seq_along(dvars)) {
            var_matrix <- time_period_data[[dvars[i]]]
            if (!is.null(var_matrix)) {
                # Direct assignment when dimensions match
                if (identical(dim(var_matrix), c(n_rows, n_cols)) &&
                    identical(rownames(var_matrix), target_rows) &&
                    identical(colnames(var_matrix), target_cols)) {
                    time_array[, , i] <- var_matrix
                } else {
                    # Use direct indexing instead of match when possible
                    time_array[rownames(var_matrix), colnames(var_matrix), i] <- var_matrix
                }
            }
        }

        return(time_array)
    }

    # longit_array + actor_unif case
    if (netlet_type == "longit_array" && actor_unif) {
        # Cache all dimensions at once
        dims <- c(msrmnts$n_row_actors, msrmnts$n_col_actors, msrmnts$n_dvars, msrmnts$n_time)
        labs <- list(msrmnts$row_actors, msrmnts$col_actors, msrmnts$dvars, msrmnts$time)

        arr <- array(NA, dim = dims, dimnames = labs)

        # fill in array - cache common values
        row_actors <- msrmnts$row_actors
        col_actors <- msrmnts$col_actors
        dvars <- msrmnts$dvars

        for (i in seq_along(msrmnts$time)) {
            arr[, , , i] <- convert_to_array(
                array_list[[msrmnts$time[i]]],
                row_actors,
                col_actors,
                dvars
            )
        }
        return(arr)
    }

    # longit_list + actor_unif case
    if (netlet_type == "longit_list" && actor_unif) {
        # Cache dimensions and common values
        row_actors <- msrmnts$row_actors[[1]]
        col_actors <- msrmnts$col_actors[[1]]
        dvars <- msrmnts$dvars
        time_periods <- msrmnts$time

        dims <- c(length(row_actors), length(col_actors), length(dvars), length(time_periods))
        labs <- list(row_actors, col_actors, dvars, time_periods)

        arr <- array(NA, dim = dims, dimnames = labs)

        # fill in array
        for (i in seq_along(time_periods)) {
            arr[, , , i] <- convert_to_array(
                array_list[[time_periods[i]]],
                row_actors,
                col_actors,
                dvars
            )
        }
        return(arr)
    }

    # longit_list + !actor_unif case
    if (netlet_type == "longit_list" && !actor_unif) {
        # Optimize unique operations - do once
        all_row_actors <- unlist(msrmnts$row_actors, use.names = FALSE)
        all_col_actors <- unlist(msrmnts$col_actors, use.names = FALSE)
        row_actors <- sort(unique(all_row_actors))
        col_actors <- sort(unique(all_col_actors))

        # Cache dimensions
        dvars <- msrmnts$dvars
        time_periods <- msrmnts$time
        dims <- c(length(row_actors), length(col_actors), length(dvars), length(time_periods))
        labs <- list(row_actors, col_actors, dvars, time_periods)

        arr <- array(NA, dim = dims, dimnames = labs)

        # Pre-compute actor mappings for efficiency
        row_actor_indices <- setNames(seq_along(row_actors), row_actors)
        col_actor_indices <- setNames(seq_along(col_actors), col_actors)

        # fill in array
        for (i in seq_along(time_periods)) {
            tt <- time_periods[i]

            # Get actors for this time period
            period_row_actors <- msrmnts$row_actors[[tt]]
            period_col_actors <- msrmnts$col_actors[[tt]]

            # Convert new structure to array format for this time period
            period_array <- convert_to_array(
                array_list[[tt]],
                period_row_actors,
                period_col_actors,
                dvars
            )

            # Use pre-computed indices for faster lookup
            row_idx <- row_actor_indices[period_row_actors]
            col_idx <- col_actor_indices[period_col_actors]

            # Fill in the main array
            arr[row_idx, col_idx, , i] <- period_array
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

longit_nodal_to_arr <- function(netlet) {
    # make sure it's a netify object
    netify_check(netlet)

    # if nodal data not present return NULL
    nodal_df <- attr(netlet, "nodal_data")
    if (is.null(nodal_df)) {
        return(NULL)
    }

    # get dimensions and type - cache attributes
    netlet_attrs <- attributes(netlet)
    msrmnts <- netify_measurements(netlet)
    netlet_type <- netlet_attrs$netify_type
    actor_unif <- netlet_attrs$actor_time_uniform

    # Pre-compute common values
    nvars <- msrmnts$nvars
    n_nvars <- msrmnts$n_nvars
    time_periods <- msrmnts$time
    n_time <- msrmnts$n_time

    # Helper function to fill array - optimized
    fill_nodal_array <- function(actors, n_actors) {
        # Initialize array
        arr <- array(NA,
            dim = c(n_actors, n_nvars, n_time),
            dimnames = list(actors, nvars, time_periods)
        )

        # Convert to matrix once for efficiency
        nodal_matrix <- as.matrix(nodal_df[, nvars, drop = FALSE])

        # Group by time for efficiency
        time_indices <- split(seq_len(nrow(nodal_df)), nodal_df$time)

        # Fill array by time period
        for (i in seq_along(time_periods)) {
            tt <- time_periods[i]
            idx <- time_indices[[tt]]
            if (length(idx) > 0) {
                actors_tt <- nodal_df$actor[idx]
                arr[actors_tt, , i] <- nodal_matrix[idx, , drop = FALSE]
            }
        }
        return(arr)
    }

    # longit_array + actor_unif case
    if (netlet_type == "longit_array" && actor_unif) {
        # Create arrays for both row and col actors
        out <- list(
            row = fill_nodal_array(msrmnts$row_actors, msrmnts$n_row_actors),
            col = fill_nodal_array(msrmnts$col_actors, msrmnts$n_col_actors)
        )
        return(out)
    }

    # longit_list + actor_unif case
    if (netlet_type == "longit_list" && actor_unif) {
        # Use first time period's actors
        out <- list(
            row = fill_nodal_array(msrmnts$row_actors[[1]], msrmnts$n_row_actors[[1]]),
            col = fill_nodal_array(msrmnts$col_actors[[1]], msrmnts$n_col_actors[[1]])
        )
        return(out)
    }

    # longit_list + !actor_unif case
    if (netlet_type == "longit_list" && !actor_unif) {
        # Get unique actors efficiently
        all_row_actors <- unlist(msrmnts$row_actors, use.names = FALSE)
        all_col_actors <- unlist(msrmnts$col_actors, use.names = FALSE)
        row_actors <- sort(unique(all_row_actors))
        col_actors <- sort(unique(all_col_actors))

        out <- list(
            row = fill_nodal_array(row_actors, length(row_actors)),
            col = fill_nodal_array(col_actors, length(col_actors))
        )
        return(out)
    }
}
