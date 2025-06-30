#' Matrix utility functions for netify
#'
#' This file contains shared matrix manipulation utilities used across
#' the netify package, eliminating redundancy and improving performance.
#'
#' @name matrix_utils
#' @keywords internal

#' Extract matrix from netify object
#'
#' Unified function to extract adjacency matrix from different types of netify objects
#'
#' @param net A netify object (cross-sectional, longitudinal array, or longitudinal list)
#' @param time_index For longitudinal data, which time period to extract (default: 1)
#' @return Numeric matrix
#'
#' @keywords internal
#' @noRd
extract_matrix <- function(net, time_index = 1) {
    attrs <- attributes(net)
    netify_type <- attrs$netify_type

    if (netify_type == "cross_sec") {
        return(as.matrix(net))
    } else if (netify_type == "longit_array") {
        return(net[, , time_index])
    } else if (netify_type == "longit_list") {
        return(net[[time_index]])
    } else {
        stop("Unknown netify type: ", netify_type)
    }
}

#' Get all unique actors from networks
#'
#' Extracts all unique actors from a list of networks or a single network
#'
#' @param nets Either a single netify object or a list of netify objects
#' @return Character vector of sorted unique actors
#'
#' @keywords internal
#' @noRd
get_all_actors <- function(nets) {
    if (!is.list(nets) || inherits(nets, "netify")) {
        # single net
        mat <- extract_matrix(nets)
        return(sort(unique(c(rownames(mat), colnames(mat)))))
    } else {
        # list of nets - use rcpp for efficiency
        mats <- lapply(nets, extract_matrix)
        return(get_all_actors_cpp(mats))
    }
}

#' Align matrices to common actors
#'
#' Ensures two or more matrices have the same dimensions and actor ordering
#'
#' @param ... Either two matrices or a list of matrices
#' @param all_actors Optional character vector of actors to align to
#' @param include_diagonal Whether to preserve diagonal values
#' @return For two matrices: list with mat1 and mat2. For multiple: list of aligned matrices
#'
#' @keywords internal
#' @noRd
align_matrices <- function(..., all_actors = NULL, include_diagonal = FALSE) {
    dots <- list(...)

    # handle different input types
    if (length(dots) == 1 && is.list(dots[[1]])) {
        # list of mats passed
        mats <- dots[[1]]
    } else if (length(dots) == 2) {
        # two mats passed directly
        mats <- dots
    } else {
        stop("align_matrices requires either 2 matrices or a list of matrices")
    }

    # Extract matrices if they're netify objects
    if (inherits(mats[[1]], "netify")) {
        mats <- lapply(mats, extract_matrix)
    }

    if (length(mats) == 2) {
        # use rcpp for two matrices
        return(align_matrices_cpp(mats[[1]], mats[[2]], all_actors))
    } else {
        # use rcpp for multiple matrices
        return(batch_align_matrices_cpp(mats, all_actors, include_diagonal))
    }
}

#' Melt matrix to long format
#'
#' Converts matrix to data frame with row, col, value columns.
#' Replaces multiple melt functions with single efficient implementation.
#'
#' @param mat Matrix to melt
#' @param remove_diagonal Remove diagonal elements (default: TRUE)
#' @param remove_zeros Remove zero values (default: TRUE)
#' @param na.rm Remove NA values (default: TRUE)
#' @param value.name Name for value column (default: "value")
#' @return Data frame with row, col, and value columns
#'
#' @keywords internal
#' @noRd
melt_matrix <- function(mat,
                        remove_diagonal = TRUE,
                        remove_zeros = TRUE,
                        na.rm = TRUE,
                        value.name = "value") {
    # use rcpp implementation
    result <- melt_matrix_cpp(mat, remove_diagonal, remove_zeros, na.rm)

    # rename columns to match expected format
    names(result)[names(result) == "row"] <- "Var1"
    names(result)[names(result) == "col"] <- "Var2"

    # rename value column if requested
    if (value.name != "value") {
        names(result)[names(result) == "value"] <- value.name
    }

    return(result)
}

#' Calculate correlation with proper handling of edge cases
#'
#' Wrapper around correlation_cpp that handles constant vectors and missing data
#'
#' @param x First numeric vector
#' @param y Second numeric vector
#' @param na.rm Remove NA values before calculation
#' @return Correlation coefficient or NA if cannot be calculated
#'
#' @importFrom stats var
#' @keywords internal
#' @noRd
safe_correlation <- function(x, y, na.rm = TRUE) {
    if (na.rm) {
        complete <- !is.na(x) & !is.na(y)
        x <- x[complete]
        y <- y[complete]
    }

    if (length(x) < 3) {
        return(NA_real_)
    }

    # check for zero variance
    if (var(x) == 0 || var(y) == 0) {
        if (var(x) == 0 && var(y) == 0 && all(x == y)) {
            # perfect correlation if both constant and equal
            return(1) 
        }
        # no correlation if one is constant
        return(0) 
    }

    return(correlation_cpp(x, y))
}

#' Ensure matrices have same dimensions
#'
#' Pads smaller matrix with zeros to match larger matrix dimensions
#'
#' @param mat1 First matrix
#' @param mat2 Second matrix
#' @return List with two matrices of same dimensions
#'
#' @keywords internal
#' @noRd
ensure_same_dimensions <- function(mat1, mat2) {
    n1 <- nrow(mat1)
    n2 <- nrow(mat2)

    if (n1 == n2 && ncol(mat1) == ncol(mat2)) {
        return(list(mat1 = mat1, mat2 = mat2))
    }

    # determine target dimensions
    n <- max(n1, n2, ncol(mat1), ncol(mat2))

    # pad matrices if needed
    if (n1 < n || ncol(mat1) < n) {
        new_mat1 <- matrix(0, n, n)
        new_mat1[1:n1, 1:ncol(mat1)] <- mat1
        mat1 <- new_mat1
    }

    if (n2 < n || ncol(mat2) < n) {
        new_mat2 <- matrix(0, n, n)
        new_mat2[1:n2, 1:ncol(mat2)] <- mat2
        mat2 <- new_mat2
    }

    return(list(mat1 = mat1, mat2 = mat2))
}
