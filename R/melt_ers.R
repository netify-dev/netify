#### these started off with me wanting to move away from reshape2
### then realizing that was hard because my function was slower
## started iterating with llms then and i learned a lot, anything
# clever below is from the llm, anything slow is from me

#' melt the face off of matrices
#' @keywords internal
#' @noRd
melt_matrix_base <- function(mat) {
    if (is.null(mat) || length(mat) == 0) {
        return(data.frame(
            Var1 = character(0),
            Var2 = character(0),
            value = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    # get dimensions
    nr <- nrow(mat)
    nc <- ncol(mat)

    # handle edge cases
    if (nr == 0 || nc == 0) {
        return(data.frame(
            Var1 = character(0),
            Var2 = character(0),
            value = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    # create indices efficiently
    row_idx <- rep(seq_len(nr), nc)
    col_idx <- rep(seq_len(nc), each = nr)

    # get names with fallback
    row_names <- rownames(mat)
    col_names <- colnames(mat)
    if (is.null(row_names)) row_names <- as.character(seq_len(nr))
    if (is.null(col_names)) col_names <- as.character(seq_len(nc))

    # create data.frame directly
    data.frame(
        Var1 = row_names[row_idx],
        Var2 = col_names[col_idx],
        value = as.vector(mat),
        stringsAsFactors = FALSE
    )
}

#' fast sparse matrix melt - only non-zero/non-na entries
#' @keywords internal
#' @noRd
melt_matrix_sparse <- function(mat, remove_zeros = TRUE, remove_diagonal = TRUE) {
    if (is.null(mat) || length(mat) == 0) {
        return(data.frame(
            Var1 = character(0),
            Var2 = character(0),
            value = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    # get indices of non-zero/non-na entries using which() with arr.ind
    # This is MUCH faster than iterating through the matrix
    if (remove_zeros) {
        idx <- which(mat != 0 & !is.na(mat), arr.ind = TRUE)
    } else {
        idx <- which(!is.na(mat), arr.ind = TRUE)
    }

    # remove diagonal if requested
    if (remove_diagonal && nrow(idx) > 0) {
        keep_idx <- idx[, 1] != idx[, 2]
        idx <- idx[keep_idx, , drop = FALSE]
    }

    # handle empty result
    if (nrow(idx) == 0) {
        return(data.frame(
            Var1 = character(0),
            Var2 = character(0),
            value = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    # get names
    row_names <- rownames(mat)
    col_names <- colnames(mat)
    if (is.null(row_names)) row_names <- as.character(seq_len(nrow(mat)))
    if (is.null(col_names)) col_names <- as.character(seq_len(ncol(mat)))

    # build data.frame directly using the indices
    data.frame(
        Var1 = row_names[idx[, 1]],
        Var2 = col_names[idx[, 2]],
        value = mat[idx],
        stringsAsFactors = FALSE
    )
}

#' fast sparse array melt for 3d arrays
#' @keywords internal
#' @noRd
melt_array_sparse <- function(arr, remove_zeros = TRUE, remove_diagonal = TRUE) {
    if (is.null(arr) || length(arr) == 0) {
        return(data.frame(
            Var1 = character(0),
            Var2 = character(0),
            Var3 = character(0),
            value = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    # get dimensions
    dims <- dim(arr)
    if (length(dims) != 3) {
        stop("input must be a 3d array")
    }

    # get dimnames
    dn <- dimnames(arr)
    if (is.null(dn[[1]])) dn[[1]] <- as.character(seq_len(dims[1]))
    if (is.null(dn[[2]])) dn[[2]] <- as.character(seq_len(dims[2]))
    if (is.null(dn[[3]])) dn[[3]] <- as.character(seq_len(dims[3]))

    # Use list to collect results from each time slice
    results_list <- vector("list", dims[3])

    # process each time slice
    for (k in seq_len(dims[3])) {
        mat <- arr[, , k]

        # Use the optimized sparse matrix melt
        sparse_result <- melt_matrix_sparse(mat, remove_zeros, remove_diagonal)

        if (nrow(sparse_result) > 0) {
            # Add time dimension
            sparse_result$L1 <- dn[[3]][k]
            results_list[[k]] <- sparse_result
        }
    }

    # Remove NULL entries and combine
    results_list <- results_list[!sapply(results_list, is.null)]

    if (length(results_list) == 0) {
        return(data.frame(
            Var1 = character(0),
            Var2 = character(0),
            L1 = character(0),
            value = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    # Combine all results
    result <- do.call(rbind, results_list)

    # Reorder columns to match expected output
    result <- result[, c("Var1", "Var2", "L1", "value")]

    return(result)
}

#' fast sparse list melt for lists of matrices
#' specifically, for sender, receiver, time
#' @keywords internal
#' @noRd
melt_list_sparse <- function(mat_list, remove_zeros = TRUE, remove_diagonal = TRUE) {
    if (is.null(mat_list) || length(mat_list) == 0) {
        return(data.frame(
            Var1 = character(0),
            Var2 = character(0),
            L1 = character(0),
            value = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    # get time period names
    time_names <- names(mat_list)
    if (is.null(time_names)) {
        time_names <- as.character(seq_along(mat_list))
    }

    # Use list to collect results
    results_list <- vector("list", length(mat_list))

    # process each matrix in the list
    for (i in seq_along(mat_list)) {
        mat <- mat_list[[i]]

        if (is.null(mat) || length(mat) == 0) next

        # Use the optimized sparse matrix melt
        sparse_result <- melt_matrix_sparse(mat, remove_zeros, remove_diagonal)

        if (nrow(sparse_result) > 0) {
            # Add time dimension
            sparse_result$L1 <- time_names[i]
            results_list[[i]] <- sparse_result
        }
    }

    # Remove NULL entries and combine
    results_list <- results_list[!sapply(results_list, is.null)]

    if (length(results_list) == 0) {
        return(data.frame(
            Var1 = character(0),
            Var2 = character(0),
            L1 = character(0),
            value = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    # Combine all results
    result <- do.call(rbind, results_list)

    # Reorder columns to match expected output
    result <- result[, c("Var1", "Var2", "L1", "value")]

    return(result)
}

#' Melt a data.frame from wide to long format
#'
#' @param data A data.frame to melt
#' @param id Character vector of column names to use as id variables.
#'   If NULL, all columns are melted.
#' @return A melted data.frame with columns for id variables (if any),
#'   'variable', and 'value'
#' @keywords internal
#' @noRd
melt_df <- function(data, id = NULL) {
    # ensure it's a data.frame
    if (!is.data.frame(data)) {
        stop("data must be a data.frame")
    }

    # get dimensions
    n <- nrow(data)

    # handle different id specifications
    if (is.null(id)) {
        # no id vars - melt everything
        if (n == 0 || ncol(data) == 0) {
            return(data.frame(
                variable = character(0),
                value = numeric(0),
                stringsAsFactors = FALSE
            ))
        }

        return(data.frame(
            variable = rep(names(data), each = n),
            value = unlist(data, use.names = FALSE),
            stringsAsFactors = FALSE
        ))
    }

    # with id vars
    measure_vars <- setdiff(names(data), id)

    # nothing to melt
    if (length(measure_vars) == 0) {
        return(data)
    }

    # create melted data.frame
    data.frame(
        data[rep(seq_len(n), length(measure_vars)), id, drop = FALSE],
        variable = factor(rep(measure_vars, each = n), levels = measure_vars),
        value = unlist(data[measure_vars], use.names = FALSE),
        row.names = NULL,
        stringsAsFactors = FALSE
    )
}

#' Convert dyad_data attribute to meltable format
#' specifically for sender, receiver, var, time
#' @keywords internal
#' @noRd
melt_var_time_list <- function(dyad_data_attr) {
    # check if dyad_data_attr is empty
    if (length(dyad_data_attr) == 0) {
        return(data.frame(
            Var1 = character(0),
            Var2 = character(0),
            Var3 = character(0),
            L1 = character(0),
            value = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    # figure out how many rows we need in total
    total_rows <- 0
    time_info <- vector("list", length(dyad_data_attr))
    idx <- 1

    # iterate over time periods and gather information
    for (time_period in names(dyad_data_attr)) {
        var_matrices <- dyad_data_attr[[time_period]]
        if (length(var_matrices) > 0) {
            first_mat <- var_matrices[[1]]
            nr <- nrow(first_mat)
            nc <- ncol(first_mat)
            n_vars <- length(var_matrices)
            n_cells <- nr * nc

            time_info[[idx]] <- list(
                time = time_period,
                n_cells = n_cells,
                n_vars = n_vars,
                nr = nr,
                nc = nc,
                var_names = names(var_matrices),
                row_names = rownames(first_mat) %||% as.character(seq_len(nr)),
                col_names = colnames(first_mat) %||% as.character(seq_len(nc))
            )

            total_rows <- total_rows + n_cells * n_vars
        }
        idx <- idx + 1
    }

    # if no valid time periods, return empty data.frame
    if (total_rows == 0) {
        return(data.frame(
            Var1 = character(0),
            Var2 = character(0),
            Var3 = character(0),
            L1 = character(0),
            value = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    # pre-allocate vectors for efficiency
    Var1 <- character(total_rows)
    Var2 <- character(total_rows)
    Var3 <- character(total_rows)
    L1 <- character(total_rows)

    # figure out the type of the values vector based on the first matrix
    first_mat <- dyad_data_attr[[1]][[1]]
    if (is.integer(first_mat)) {
        values <- integer(total_rows)
    } else if (is.logical(first_mat)) {
        values <- logical(total_rows)
    } else {
        values <- numeric(total_rows)
    }

    # fill the vectors in one pass
    current_row <- 1

    for (info in time_info) {
        if (is.null(info)) next

        time_period <- info$time
        var_matrices <- dyad_data_attr[[time_period]]

        # pre-compute row/column expansions for this time period
        n_cells <- info$n_cells
        nr <- info$nr
        nc <- info$nc

        # process each variable
        for (i in seq_along(info$var_names)) {
            var_name <- info$var_names[i]
            mat <- var_matrices[[var_name]]

            # calculate the range of rows to fill
            cell_start <- current_row
            cell_end <- current_row + n_cells - 1
            cell_range <- cell_start:cell_end

            # fill actor names (Var1 and Var2)
            Var1[cell_range] <- rep(info$row_names, nc)
            Var2[cell_range] <- rep(info$col_names, each = nr)

            # fill variable name and time period
            Var3[cell_range] <- var_name
            L1[cell_range] <- time_period

            # fill matrix values
            values[cell_range] <- as.vector(mat)

            # move to the next block of rows
            current_row <- current_row + n_cells
        }
    }

    # create the final data frame
    out <- data.frame(
        Var1 = Var1,
        Var2 = Var2,
        Var3 = Var3,
        L1 = L1,
        value = values,
        stringsAsFactors = FALSE
    )

    #
    return(out)
}
