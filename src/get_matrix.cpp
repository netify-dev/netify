#include <Rcpp.h>
using namespace Rcpp;

//' This function fills in an adjacency matrix based on actors and data
//'
//' @param n_rows integer specifying number of row actors
//' @param n_cols integer specifying number of column actors
//' @param actors_rows character vector of row actors
//' @param actors_cols character vector of column actors
//' @param matRowIndices numeric vector indicating positions of senders in data
//' @param matColIndices numeric vector indicating positions of receivers in data
//' @param value numeric vector of values to fill in cross-sections of adjacency matrices
//' @param symmetric logical indicating whether ties are symmetric
//' @param missing_to_zero logical indicating whether missing values should be set to zero
//' @param diag_to_NA logical indicating whether diagonal should be set to NA
//' @return an adjacency matrix
//' @author Shahryar Minhas, Ha Eun Choi
//'
//' @export get_matrix
// [[Rcpp::export]]

NumericMatrix get_matrix(
    int n_rows,
    int n_cols,
    CharacterVector actors_rows,
    CharacterVector actors_cols,
    IntegerVector matRowIndices,
    IntegerVector matColIndices,
    NumericVector value,
    bool symmetric,
    bool missing_to_zero = true,
    bool diag_to_NA = true
){
    // initialize matrix
    NumericMatrix m(n_rows, n_cols);

    // start off with NAs or zeros based on missing_to_zero
    if(missing_to_zero) {
        std::fill(m.begin(), m.end(), 0.0);
    } else {
        std::fill(m.begin(), m.end(), NumericVector::get_na());
    }

    // add row and column names only if we have actors
    if(actors_rows.size() == n_rows && actors_cols.size() == n_cols) {
        rownames(m) = actors_rows;
        colnames(m) = actors_cols;
    }

    // fill in based on data values
    int n_values = value.size();
    for(int i = 0; i < n_values; i++){
        // Check for valid indices first
        if(IntegerVector::is_na(matRowIndices[i]) || IntegerVector::is_na(matColIndices[i])) {
            continue;  // Skip NA indices
        }
        
        int a1i = matRowIndices[i] - 1;  // Convert to 0-based indexing
        int a2i = matColIndices[i] - 1;  // Convert to 0-based indexing
        double vi = value[i];

        // Bounds checking to prevent segfaults
        if(a1i >= 0 && a1i < n_rows && a2i >= 0 && a2i < n_cols) {
            m(a1i, a2i) = vi;
            
            // Handle symmetric case
            if(symmetric && a1i != a2i && a2i < n_rows && a1i < n_cols) {
                m(a2i, a1i) = vi;
            }
        }
    }

    // Handle diagonal elements
    if(diag_to_NA && n_rows == n_cols) {
        for(int i = 0; i < std::min(n_rows, n_cols); i++) {
            m(i, i) = NumericVector::get_na();
        }
    }

    return m;
}


#include <Rcpp.h>
using namespace Rcpp;

//' This function fills in an adjacency matrix based on actors and data (INTEGER VERSION)
//'
//' @param n_rows integer specifying number of row actors
//' @param n_cols integer specifying number of column actors
//' @param actors_rows character vector of row actors
//' @param actors_cols character vector of column actors
//' @param matRowIndices numeric vector indicating positions of senders in data
//' @param matColIndices numeric vector indicating positions of receivers in data
//' @param value integer vector of values to fill in cross-sections of adjacency matrices
//' @param symmetric logical indicating whether ties are symmetric
//' @param missing_to_zero logical indicating whether missing values should be set to zero
//' @param diag_to_NA logical indicating whether diagonal should be set to NA
//' @return an integer adjacency matrix
//' @author Shahryar Minhas, Ha Eun Choi
//'
//' @export get_matrix_integer
// [[Rcpp::export]]
IntegerMatrix get_matrix_integer(
    int n_rows,
    int n_cols,
    CharacterVector actors_rows,
    CharacterVector actors_cols,
    IntegerVector matRowIndices,
    IntegerVector matColIndices,
    IntegerVector value,
    bool symmetric,
    bool missing_to_zero = true,
    bool diag_to_NA = true
){
    // initialize matrix
    IntegerMatrix m(n_rows, n_cols);

    // start off with NAs or zeros based on missing_to_zero
    if(missing_to_zero) {
        std::fill(m.begin(), m.end(), 0);
    } else {
        std::fill(m.begin(), m.end(), IntegerVector::get_na());
    }

    // add row and column names only if we have actors
    if(actors_rows.size() == n_rows && actors_cols.size() == n_cols) {
        rownames(m) = actors_rows;
        colnames(m) = actors_cols;
    }

    // fill in based on data values
    int n_values = value.size();
    for(int i = 0; i < n_values; i++){
        // Check for valid indices first
        if(IntegerVector::is_na(matRowIndices[i]) || IntegerVector::is_na(matColIndices[i])) {
            continue;  // Skip NA indices
        }
        
        int a1i = matRowIndices[i] - 1;  // Convert to 0-based indexing
        int a2i = matColIndices[i] - 1;  // Convert to 0-based indexing
        int vi = value[i];

        // Bounds checking to prevent segfaults
        if(a1i >= 0 && a1i < n_rows && a2i >= 0 && a2i < n_cols) {
            m(a1i, a2i) = vi;
            
            // Handle symmetric case
            if(symmetric && a1i != a2i && a2i < n_rows && a1i < n_cols) {
                m(a2i, a1i) = vi;
            }
        }
    }

    // Handle diagonal elements
    if(diag_to_NA && n_rows == n_cols) {
        for(int i = 0; i < std::min(n_rows, n_cols); i++) {
            m(i, i) = IntegerVector::get_na();
        }
    }

    return m;
}

//' This function fills in an adjacency matrix based on actors and data (LOGICAL VERSION)
//'
//' @param n_rows integer specifying number of row actors
//' @param n_cols integer specifying number of column actors
//' @param actors_rows character vector of row actors
//' @param actors_cols character vector of column actors
//' @param matRowIndices numeric vector indicating positions of senders in data
//' @param matColIndices numeric vector indicating positions of receivers in data
//' @param value logical vector of values to fill in cross-sections of adjacency matrices
//' @param symmetric logical indicating whether ties are symmetric
//' @param missing_to_zero logical indicating whether missing values should be set to FALSE
//' @param diag_to_NA logical indicating whether diagonal should be set to NA
//' @return a logical adjacency matrix
//' @author Shahryar Minhas, Ha Eun Choi
//'
//' @export get_matrix_logical
// [[Rcpp::export]]
LogicalMatrix get_matrix_logical(
    int n_rows,
    int n_cols,
    CharacterVector actors_rows,
    CharacterVector actors_cols,
    IntegerVector matRowIndices,
    IntegerVector matColIndices,
    LogicalVector value,
    bool symmetric,
    bool missing_to_zero = true,
    bool diag_to_NA = true
){
    // initialize matrix
    LogicalMatrix m(n_rows, n_cols);

    // start off with NAs or FALSE based on missing_to_zero
    if(missing_to_zero) {
        std::fill(m.begin(), m.end(), false);
    } else {
        std::fill(m.begin(), m.end(), LogicalVector::get_na());
    }

    // add row and column names only if we have actors
    if(actors_rows.size() == n_rows && actors_cols.size() == n_cols) {
        rownames(m) = actors_rows;
        colnames(m) = actors_cols;
    }

    // fill in based on data values
    int n_values = value.size();
    for(int i = 0; i < n_values; i++){
        // Check for valid indices first
        if(IntegerVector::is_na(matRowIndices[i]) || IntegerVector::is_na(matColIndices[i])) {
            continue;  // Skip NA indices
        }
        
        int a1i = matRowIndices[i] - 1;  // Convert to 0-based indexing
        int a2i = matColIndices[i] - 1;  // Convert to 0-based indexing
        bool vi = value[i];

        // Bounds checking to prevent segfaults
        if(a1i >= 0 && a1i < n_rows && a2i >= 0 && a2i < n_cols) {
            m(a1i, a2i) = vi;
            
            // Handle symmetric case
            if(symmetric && a1i != a2i && a2i < n_rows && a1i < n_cols) {
                m(a2i, a1i) = vi;
            }
        }
    }

    // Handle diagonal elements
    if(diag_to_NA && n_rows == n_cols) {
        for(int i = 0; i < std::min(n_rows, n_cols); i++) {
            m(i, i) = LogicalVector::get_na();
        }
    }

    return m;
}

//' This function fills in an adjacency matrix based on actors and data (CHARACTER VERSION)
//'
//' @param n_rows integer specifying number of row actors
//' @param n_cols integer specifying number of column actors
//' @param actors_rows character vector of row actors
//' @param actors_cols character vector of column actors
//' @param matRowIndices numeric vector indicating positions of senders in data
//' @param matColIndices numeric vector indicating positions of receivers in data
//' @param value character vector of values to fill in cross-sections of adjacency matrices
//' @param symmetric logical indicating whether ties are symmetric
//' @param missing_to_zero logical indicating whether missing values should be set to empty string
//' @param diag_to_NA logical indicating whether diagonal should be set to NA
//' @return a character adjacency matrix
//' @author Shahryar Minhas, Ha Eun Choi
//'
//' @export get_matrix_character
// [[Rcpp::export]]
CharacterMatrix get_matrix_character(
    int n_rows,
    int n_cols,
    CharacterVector actors_rows,
    CharacterVector actors_cols,
    IntegerVector matRowIndices,
    IntegerVector matColIndices,
    CharacterVector value,
    bool symmetric,
    bool missing_to_zero = true,
    bool diag_to_NA = true
){
    // initialize matrix
    CharacterMatrix m(n_rows, n_cols);

    // start off with NAs or empty strings based on missing_to_zero
    if(missing_to_zero) {
        std::fill(m.begin(), m.end(), "");
    } else {
        std::fill(m.begin(), m.end(), CharacterVector::get_na());
    }

    // add row and column names only if we have actors
    if(actors_rows.size() == n_rows && actors_cols.size() == n_cols) {
        rownames(m) = actors_rows;
        colnames(m) = actors_cols;
    }

    // fill in based on data values
    int n_values = value.size();
    for(int i = 0; i < n_values; i++){
        // Check for valid indices first
        if(IntegerVector::is_na(matRowIndices[i]) || IntegerVector::is_na(matColIndices[i])) {
            continue;  // Skip NA indices
        }
        
        int a1i = matRowIndices[i] - 1;  // Convert to 0-based indexing
        int a2i = matColIndices[i] - 1;  // Convert to 0-based indexing
        String vi = value[i];

        // Bounds checking to prevent segfaults
        if(a1i >= 0 && a1i < n_rows && a2i >= 0 && a2i < n_cols) {
            m(a1i, a2i) = vi;
            
            // Handle symmetric case
            if(symmetric && a1i != a2i && a2i < n_rows && a1i < n_cols) {
                m(a2i, a1i) = vi;
            }
        }
    }

    // Handle diagonal elements
    if(diag_to_NA && n_rows == n_cols) {
        for(int i = 0; i < std::min(n_rows, n_cols); i++) {
            m(i, i) = CharacterVector::get_na();
        }
    }

    return m;
}


//' Batch processing version for multiple time periods
//' 
//' @param n_rows_vec integer vector of number of rows for each matrix
//' @param n_cols_vec integer vector of number of columns for each matrix
//' @param actors_rows_list list of character vectors for row actors
//' @param actors_cols_list list of character vectors for column actors  
//' @param matRowIndices_list list of integer vectors for row indices
//' @param matColIndices_list list of integer vectors for column indices
//' @param value_list list of numeric vectors for values
//' @param symmetric logical indicating whether ties are symmetric
//' @param missing_to_zero logical indicating whether missing values should be set to zero
//' @param diag_to_NA logical indicating whether diagonal should be set to NA
//' @return a list of adjacency matrices
//' @author Shahryar Minhas, Ha Eun Choi
//'
//' @export get_matrix_batch
// [[Rcpp::export]]

List get_matrix_batch(
    IntegerVector n_rows_vec,
    IntegerVector n_cols_vec,
    List actors_rows_list,
    List actors_cols_list,
    List matRowIndices_list,
    List matColIndices_list,
    List value_list,
    bool symmetric,
    bool missing_to_zero = true,
    bool diag_to_NA = true
){
    int n_matrices = n_rows_vec.size();
    List result(n_matrices);
    
    // Validate input sizes
    if(n_cols_vec.size() != n_matrices || 
       actors_rows_list.size() != n_matrices ||
       actors_cols_list.size() != n_matrices ||
       matRowIndices_list.size() != n_matrices ||
       matColIndices_list.size() != n_matrices ||
       value_list.size() != n_matrices) {
        stop("All input vectors/lists must have the same length");
    }
    
    for(int mat_idx = 0; mat_idx < n_matrices; mat_idx++) {
        int n_rows = n_rows_vec[mat_idx];
        int n_cols = n_cols_vec[mat_idx];
        
        // Handle empty matrices
        if(n_rows <= 0 || n_cols <= 0) {
            NumericMatrix m(std::max(1, n_rows), std::max(1, n_cols));
            if(missing_to_zero) {
                std::fill(m.begin(), m.end(), 0.0);
            } else {
                std::fill(m.begin(), m.end(), NumericVector::get_na());
            }
            result[mat_idx] = m;
            continue;
        }
        
        // Extract data for this matrix with error checking
        CharacterVector actors_rows;
        CharacterVector actors_cols;
        IntegerVector matRowIndices;
        IntegerVector matColIndices;
        NumericVector value;
        
        try {
            actors_rows = as<CharacterVector>(actors_rows_list[mat_idx]);
            actors_cols = as<CharacterVector>(actors_cols_list[mat_idx]);
            matRowIndices = as<IntegerVector>(matRowIndices_list[mat_idx]);
            matColIndices = as<IntegerVector>(matColIndices_list[mat_idx]);
            value = as<NumericVector>(value_list[mat_idx]);
        } catch(std::exception& e) {
            stop("Error extracting data for matrix %d: %s", mat_idx + 1, e.what());
        }
        
        // Validate data consistency
        if(matRowIndices.size() != matColIndices.size() || 
           matRowIndices.size() != value.size()) {
            stop("Inconsistent data sizes for matrix %d", mat_idx + 1);
        }
        
        // Use the optimized single matrix function
        NumericMatrix m = get_matrix(
            n_rows, n_cols, actors_rows, actors_cols,
            matRowIndices, matColIndices, value,
            symmetric, missing_to_zero, diag_to_NA
        );
        
        result[mat_idx] = m;
    }
    
    return result;
}