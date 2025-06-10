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
    
    # get indices of non-zero/non-na entries
    if (remove_zeros) {
        idx <- which(mat != 0 & !is.na(mat), arr.ind = TRUE)
    } else {
        idx <- which(!is.na(mat), arr.ind = TRUE)
    }
    
    # remove diagonal if requested
    if (remove_diagonal && nrow(idx) > 0) {
        idx <- idx[idx[,1] != idx[,2], , drop = FALSE]
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
    
    # build data.frame directly
    data.frame(
        Var1 = row_names[idx[,1]],
        Var2 = col_names[idx[,2]],
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
    
    # pre-allocate maximum possible size
    max_size <- prod(dims)
    if (remove_diagonal) {
        max_size <- max_size - dims[1] * dims[3]  # subtract diagonal entries
    }
    
    Var1 <- character(max_size)
    Var2 <- character(max_size)
    Var3 <- character(max_size)
    values <- numeric(max_size)
    
    current_row <- 1
    
    # process each time slice
    for (k in seq_len(dims[3])) {
        mat <- arr[, , k]
        
        # get non-zero indices
        if (remove_zeros) {
            idx <- which(mat != 0 & !is.na(mat), arr.ind = TRUE)
        } else {
            idx <- which(!is.na(mat), arr.ind = TRUE)
        }
        
        # remove diagonal if requested
        if (remove_diagonal && nrow(idx) > 0) {
            idx <- idx[idx[,1] != idx[,2], , drop = FALSE]
        }
        
        if (nrow(idx) > 0) {
            n_entries <- nrow(idx)
            rows <- current_row:(current_row + n_entries - 1)
            
            Var1[rows] <- dn[[1]][idx[,1]]
            Var2[rows] <- dn[[2]][idx[,2]]
            Var3[rows] <- dn[[3]][k]
            values[rows] <- mat[idx]
            
            current_row <- current_row + n_entries
        }
    }
    
    # trim to actual size
    if (current_row > 1) {
        actual_rows <- seq_len(current_row - 1)
        result <- data.frame(
            Var1 = Var1[actual_rows],
            Var2 = Var2[actual_rows],
            L1 = Var3[actual_rows],  # note: renamed to L1 for consistency
            value = values[actual_rows],
            stringsAsFactors = FALSE
        )
    } else {
        result <- data.frame(
            Var1 = character(0),
            Var2 = character(0),
            L1 = character(0),
            value = numeric(0),
            stringsAsFactors = FALSE
        )
    }
    
    result
}

#' fast sparse list melt for lists of matrices
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
    
    # pre-calculate total size
    total_size <- 0
    for (i in seq_along(mat_list)) {
        mat <- mat_list[[i]]
        if (!is.null(mat)) {
            if (remove_diagonal) {
                # estimate non-zero entries (this is an upper bound)
                total_size <- total_size + length(mat) - nrow(mat)
            } else {
                total_size <- total_size + length(mat)
            }
        }
    }
    
    # pre-allocate
    Var1 <- character(total_size)
    Var2 <- character(total_size)
    L1 <- character(total_size)
    values <- numeric(total_size)
    
    current_row <- 1
    
    # process each matrix in the list
    for (i in seq_along(mat_list)) {
        mat <- mat_list[[i]]
        
        if (is.null(mat) || length(mat) == 0) next
        
        # get non-zero indices
        if (remove_zeros) {
            idx <- which(mat != 0 & !is.na(mat), arr.ind = TRUE)
        } else {
            idx <- which(!is.na(mat), arr.ind = TRUE)
        }
        
        # remove diagonal if requested
        if (remove_diagonal && nrow(idx) > 0) {
            idx <- idx[idx[,1] != idx[,2], , drop = FALSE]
        }
        
        if (nrow(idx) > 0) {
            n_entries <- nrow(idx)
            rows <- current_row:(current_row + n_entries - 1)
            
            # get row/col names
            row_names <- rownames(mat)
            col_names <- colnames(mat)
            if (is.null(row_names)) row_names <- as.character(seq_len(nrow(mat)))
            if (is.null(col_names)) col_names <- as.character(seq_len(ncol(mat)))
            
            Var1[rows] <- row_names[idx[,1]]
            Var2[rows] <- col_names[idx[,2]]
            L1[rows] <- time_names[i]
            values[rows] <- mat[idx]
            
            current_row <- current_row + n_entries
        }
    }
    
    # trim to actual size
    if (current_row > 1) {
        actual_rows <- seq_len(current_row - 1)
        result <- data.frame(
            Var1 = Var1[actual_rows],
            Var2 = Var2[actual_rows],
            L1 = L1[actual_rows],
            value = values[actual_rows],
            stringsAsFactors = FALSE
        )
    } else {
        result <- data.frame(
            Var1 = character(0),
            Var2 = character(0),
            L1 = character(0),
            value = numeric(0),
            stringsAsFactors = FALSE
        )
    }
    
    result
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