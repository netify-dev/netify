# created with rcpp::compileattributes()
# generator token: 10be3573-1514-4c36-9d1c-5a225cd40393

#' determine number of duplicate dyad-time obs
#'
#' @param actor1 character vector for actor1
#' @param actor2 character vector for actor2
#' @param time numeric vector for time
#' @return an integer count of number of duplicate dyads
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
count_duplicate_dyads <- function(actor1, actor2, time) {
    .Call(`_netify_count_duplicate_dyads`, actor1, actor2, time)
}

#' alternative implementation for small actor sets
#' uses integer encoding for actors when possible
#'
#' @param actor1 character vector for actor1
#' @param actor2 character vector for actor2
#' @param time numeric vector for time
#' @return an integer count of number of duplicate dyads
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
count_duplicate_dyads_indexed <- function(actor1, actor2, time) {
    .Call(`_netify_count_duplicate_dyads_indexed`, actor1, actor2, time)
}

#' this function fills in an adjacency matrix based on actors and data
#'
#' @param n_rows integer specifying number of row actors
#' @param n_cols integer specifying number of column actors
#' @param actors_rows character vector of row actors
#' @param actors_cols character vector of column actors
#' @param matrowindices numeric vector indicating positions of senders in data
#' @param matcolindices numeric vector indicating positions of receivers in data
#' @param value numeric vector of values to fill in cross-sections of adjacency matrices
#' @param symmetric logical indicating whether ties are symmetric
#' @param missing_to_zero logical indicating whether missing values should be set to zero
#' @param diag_to_NA logical indicating whether diagonal should be set to na
#' @return an adjacency matrix
#' @author shahryar minhas, ha eun choi
#'
#' @keywords internal
#' @noRd
get_matrix <- function(n_rows, n_cols, actors_rows, actors_cols, matRowIndices, matColIndices, value, symmetric, missing_to_zero = TRUE, diag_to_NA = TRUE) {
    .Call(`_netify_get_matrix`, n_rows, n_cols, actors_rows, actors_cols, matRowIndices, matColIndices, value, symmetric, missing_to_zero, diag_to_NA)
}

#' this function fills in an adjacency matrix based on actors and data (integer version)
#'
#' @param n_rows integer specifying number of row actors
#' @param n_cols integer specifying number of column actors
#' @param actors_rows character vector of row actors
#' @param actors_cols character vector of column actors
#' @param matrowindices numeric vector indicating positions of senders in data
#' @param matcolindices numeric vector indicating positions of receivers in data
#' @param value integer vector of values to fill in cross-sections of adjacency matrices
#' @param symmetric logical indicating whether ties are symmetric
#' @param missing_to_zero logical indicating whether missing values should be set to zero
#' @param diag_to_NA logical indicating whether diagonal should be set to na
#' @return an integer adjacency matrix
#' @author shahryar minhas, ha eun choi
#'
#' @keywords internal
#' @noRd
get_matrix_integer <- function(n_rows, n_cols, actors_rows, actors_cols, matRowIndices, matColIndices, value, symmetric, missing_to_zero = TRUE, diag_to_NA = TRUE) {
    .Call(`_netify_get_matrix_integer`, n_rows, n_cols, actors_rows, actors_cols, matRowIndices, matColIndices, value, symmetric, missing_to_zero, diag_to_NA)
}

#' this function fills in an adjacency matrix based on actors and data (logical version)
#'
#' @param n_rows integer specifying number of row actors
#' @param n_cols integer specifying number of column actors
#' @param actors_rows character vector of row actors
#' @param actors_cols character vector of column actors
#' @param matrowindices numeric vector indicating positions of senders in data
#' @param matcolindices numeric vector indicating positions of receivers in data
#' @param value logical vector of values to fill in cross-sections of adjacency matrices
#' @param symmetric logical indicating whether ties are symmetric
#' @param missing_to_zero logical indicating whether missing values should be set to FALSE
#' @param diag_to_NA logical indicating whether diagonal should be set to na
#' @return a logical adjacency matrix
#' @author shahryar minhas, ha eun choi
#'
#' @keywords internal
#' @noRd
get_matrix_logical <- function(n_rows, n_cols, actors_rows, actors_cols, matRowIndices, matColIndices, value, symmetric, missing_to_zero = TRUE, diag_to_NA = TRUE) {
    .Call(`_netify_get_matrix_logical`, n_rows, n_cols, actors_rows, actors_cols, matRowIndices, matColIndices, value, symmetric, missing_to_zero, diag_to_NA)
}

#' this function fills in an adjacency matrix based on actors and data (character version)
#'
#' @param n_rows integer specifying number of row actors
#' @param n_cols integer specifying number of column actors
#' @param actors_rows character vector of row actors
#' @param actors_cols character vector of column actors
#' @param matrowindices numeric vector indicating positions of senders in data
#' @param matcolindices numeric vector indicating positions of receivers in data
#' @param value character vector of values to fill in cross-sections of adjacency matrices
#' @param symmetric logical indicating whether ties are symmetric
#' @param missing_to_zero logical indicating whether missing values should be set to empty string
#' @param diag_to_NA logical indicating whether diagonal should be set to na
#' @return a character adjacency matrix
#' @author shahryar minhas, ha eun choi
#'
#' @keywords internal
#' @noRd
get_matrix_character <- function(n_rows, n_cols, actors_rows, actors_cols, matRowIndices, matColIndices, value, symmetric, missing_to_zero = TRUE, diag_to_NA = TRUE) {
    .Call(`_netify_get_matrix_character`, n_rows, n_cols, actors_rows, actors_cols, matRowIndices, matColIndices, value, symmetric, missing_to_zero, diag_to_NA)
}

#' batch processing version for multiple time periods
#'
#' @param n_rows_vec integer vector of number of rows for each matrix
#' @param n_cols_vec integer vector of number of columns for each matrix
#' @param actors_rows_list list of character vectors for row actors
#' @param actors_cols_list list of character vectors for column actors
#' @param matrowindices_list list of integer vectors for row indices
#' @param matcolindices_list list of integer vectors for column indices
#' @param value_list list of numeric vectors for values
#' @param symmetric logical indicating whether ties are symmetric
#' @param missing_to_zero logical indicating whether missing values should be set to zero
#' @param diag_to_NA logical indicating whether diagonal should be set to na
#' @return a list of adjacency matrices
#' @author shahryar minhas, ha eun choi
#'
#' @keywords internal
#' @noRd
get_matrix_batch <- function(n_rows_vec, n_cols_vec, actors_rows_list, actors_cols_list, matRowIndices_list, matColIndices_list, value_list, symmetric, missing_to_zero = TRUE, diag_to_NA = TRUE) {
    .Call(`_netify_get_matrix_batch`, n_rows_vec, n_cols_vec, actors_rows_list, actors_cols_list, matRowIndices_list, matColIndices_list, value_list, symmetric, missing_to_zero, diag_to_NA)
}

#' fast matrix melting (wide to long format conversion)
#'
#' @param mat matrix to melt
#' @param remove_diagonal whether to remove diagonal elements
#' @param remove_zeros whether to remove zero values
#' @param na_rm whether to remove na values
#' @return dataframe with row, col, and value columns
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
melt_matrix_cpp <- function(mat, remove_diagonal = TRUE, remove_zeros = TRUE, na_rm = TRUE) {
    .Call(`_netify_melt_matrix_cpp`, mat, remove_diagonal, remove_zeros, na_rm)
}

#' extract all unique actors from a list of networks
#'
#' @param nets_list list of network matrices
#' @return character vector of unique sorted actors
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
get_all_actors_cpp <- function(nets_list) {
    .Call(`_netify_get_all_actors_cpp`, nets_list)
}

#' align two matrices to have the same actors
#'
#' @param mat1 first matrix
#' @param mat2 second matrix
#' @param all_actors character vector of all actors (optional)
#' @return list with two aligned matrices
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
align_matrices_cpp <- function(mat1, mat2, all_actors_arg = NULL) {
    .Call(`_netify_align_matrices_cpp`, mat1, mat2, all_actors_arg)
}

#' batch align multiple matrices
#'
#' @param nets_list list of network matrices
#' @param all_actors character vector of all actors (optional)
#' @param include_diagonal whether to include diagonal values
#' @return list of aligned matrices
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
batch_align_matrices_cpp <- function(nets_list, all_actors_arg = NULL, include_diagonal = FALSE) {
    .Call(`_netify_batch_align_matrices_cpp`, nets_list, all_actors_arg, include_diagonal)
}

#' calculate wasserstein-1 distance between two distributions
#'
#' @param x first vector of values
#' @param y second vector of values
#' @return wasserstein-1 distance between empirical distributions
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
calculate_wasserstein_cpp <- function(x, y) {
    .Call(`_netify_calculate_wasserstein_cpp`, x, y)
}

#' calculate jaccard similarity between two matrices
#'
#' @param mat1 first matrix
#' @param mat2 second matrix
#' @param threshold1 threshold for mat1 edges
#' @param threshold2 threshold for mat2 edges
#' @return jaccard similarity coefficient
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
calculate_jaccard_cpp <- function(mat1, mat2, threshold1, threshold2) {
    .Call(`_netify_calculate_jaccard_cpp`, mat1, mat2, threshold1, threshold2)
}

#' calculate hamming distance between two matrices
#'
#' @param mat1 first matrix
#' @param mat2 second matrix
#' @param threshold1 threshold for mat1 edges
#' @param threshold2 threshold for mat2 edges
#' @return hamming distance (proportion of differing edges)
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
calculate_hamming_cpp <- function(mat1, mat2, threshold1, threshold2) {
    .Call(`_netify_calculate_hamming_cpp`, mat1, mat2, threshold1, threshold2)
}

#' calculate edge changes between two matrices
#'
#' @param mat1 first matrix
#' @param mat2 second matrix
#' @param threshold1 threshold for mat1 edges
#' @param threshold2 threshold for mat2 edges
#' @return list with added, removed, maintained edges and weight correlation
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
calculate_edge_changes_cpp <- function(mat1, mat2, threshold1, threshold2) {
    .Call(`_netify_calculate_edge_changes_cpp`, mat1, mat2, threshold1, threshold2)
}

#' calculate spectral distance between two matrices
#'
#' @param mat1 first matrix
#' @param mat2 second matrix
#' @param spectral_rank number of eigenvalues to use (0 = all)
#' @return spectral distance based on laplacian eigenvalues
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
calculate_spectral_distance_cpp <- function(mat1, mat2, spectral_rank = 0L) {
    .Call(`_netify_calculate_spectral_distance_cpp`, mat1, mat2, spectral_rank)
}

#' double center a matrix
#'
#' @param a input matrix
#' @return double-centered matrix
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
double_center_cpp <- function(A) {
    .Call(`_netify_double_center_cpp`, A)
}

#' qap correlation test with node label permutation
#'
#' @param mat1 first matrix
#' @param mat2 second matrix
#' @param n_permutations number of permutations
#' @param seed random seed (-1 for random)
#' @return list with correlation and p-value
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
qap_correlation_cpp <- function(mat1, mat2, n_permutations, seed = -1L) {
    .Call(`_netify_qap_correlation_cpp`, mat1, mat2, n_permutations, seed)
}

#' qap correlation test with degree-preserving permutation
#'
#' @param mat1 first matrix (must be binary)
#' @param mat2 second matrix
#' @param n_permutations number of permutations
#' @param swaps_factor multiplier for number of edge swaps per permutation
#' @param seed random seed (-1 for random)
#' @return list with correlation and p-value
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
qap_degree_cpp <- function(mat1, mat2, n_permutations, swaps_factor = 10L, seed = -1L) {
    .Call(`_netify_qap_degree_cpp`, mat1, mat2, n_permutations, swaps_factor, seed)
}

#' calculate similarity matrix between node attributes
#'
#' @param attributes numeric vector of node attributes
#' @param method character string specifying similarity metric ("correlation", "euclidean", "categorical", "cosine", "jaccard", "manhattan", "hamming")
#' @return numeric matrix of pairwise similarities
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
calculate_similarity_matrix_cpp <- function(attributes, method) {
    .Call(`_netify_calculate_similarity_matrix_cpp`, attributes, method)
}

#' correlation calcs for homophily analysis
#'
#' @param x numeric vector of similarities
#' @param y numeric vector of ties (0/1)
#' @return correlation coefficient
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
correlation_cpp <- function(x, y) {
    .Call(`_netify_correlation_cpp`, x, y)
}

#' calculate homophily statistics with significance testing
#'
#' @param similarity_matrix numeric matrix of pairwise node similarities
#' @param net_matrix logical matrix of network ties
#' @param significance_test logical whether to perform permutation test
#' @param n_permutations integer number of permutations
#' @param alpha numeric significance level for confidence intervals
#' @return list containing homophily statistics
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd
calculate_homophily_stats_cpp <- function(similarity_matrix, net_matrix, significance_test, n_permutations, alpha, directed) {
    .Call(`_netify_calculate_homophily_stats_cpp`, similarity_matrix, net_matrix, significance_test, n_permutations, alpha, directed)
}
