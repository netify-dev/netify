#include <Rcpp.h>
#include <algorithm>
#include <unordered_set>
#include <string>
#include <vector>

using namespace Rcpp;

//' Fast matrix melting (wide to long format conversion)
//'
//' @param mat Matrix to melt
//' @param remove_diagonal Whether to remove diagonal elements
//' @param remove_zeros Whether to remove zero values
//' @param na_rm Whether to remove NA values
//' @return DataFrame with row, col, and value columns
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
DataFrame melt_matrix_cpp(NumericMatrix mat, 
                         bool remove_diagonal = true,
                         bool remove_zeros = true,
                         bool na_rm = true) {
    
    CharacterVector row_names;
    CharacterVector col_names;
    
    // Get row and column names from the matrix
    List dimnames = mat.attr("dimnames");
    if (!Rf_isNull(dimnames) && dimnames.size() >= 2) {
        if (!Rf_isNull(dimnames[0])) {
            row_names = as<CharacterVector>(dimnames[0]);
        }
        if (!Rf_isNull(dimnames[1])) {
            col_names = as<CharacterVector>(dimnames[1]);
        }
    }
    
    // If no names, create default ones
    if (row_names.size() == 0) {
        row_names = CharacterVector(mat.nrow());
        for (int i = 0; i < mat.nrow(); i++) {
            row_names[i] = std::to_string(i + 1);
        }
    }
    if (col_names.size() == 0) {
        col_names = CharacterVector(mat.ncol());
        for (int i = 0; i < mat.ncol(); i++) {
            col_names[i] = std::to_string(i + 1);
        }
    }
    
    // Pre-allocate vectors (max possible size)
    int max_size = mat.nrow() * mat.ncol();
    std::vector<std::string> rows, cols;
    std::vector<double> values;
    rows.reserve(max_size);
    cols.reserve(max_size);
    values.reserve(max_size);
    
    // Melt the matrix
    for (int i = 0; i < mat.nrow(); i++) {
        for (int j = 0; j < mat.ncol(); j++) {
            // Skip diagonal if requested
            if (remove_diagonal && i == j) continue;
            
            double val = mat(i, j);
            
            // Skip NAs if requested
            if (na_rm && NumericMatrix::is_na(val)) continue;
            
            // Skip zeros if requested
            if (remove_zeros && val == 0.0) continue;
            
            // Add to result
            rows.push_back(std::string(row_names[i]));
            cols.push_back(std::string(col_names[j]));
            values.push_back(val);
        }
    }
    
    return DataFrame::create(
        Named("row") = rows,
        Named("col") = cols,
        Named("value") = values,
        _["stringsAsFactors"] = false
    );
}

//' Extract all unique actors from a list of networks
//'
//' @param nets_list List of network matrices
//' @return Character vector of unique sorted actors
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
CharacterVector get_all_actors_cpp(List nets_list) {
    std::unordered_set<std::string> all_actors;
    
    for (int i = 0; i < nets_list.size(); i++) {
        NumericMatrix mat = as<NumericMatrix>(nets_list[i]);
        
        // Get row and column names
        List dimnames = mat.attr("dimnames");
        if (!Rf_isNull(dimnames) && dimnames.size() >= 2) {
            if (!Rf_isNull(dimnames[0])) {
                CharacterVector rnames = as<CharacterVector>(dimnames[0]);
                for (int j = 0; j < rnames.size(); j++) {
                    all_actors.insert(std::string(rnames[j]));
                }
            }
            if (!Rf_isNull(dimnames[1])) {
                CharacterVector cnames = as<CharacterVector>(dimnames[1]);
                for (int j = 0; j < cnames.size(); j++) {
                    all_actors.insert(std::string(cnames[j]));
                }
            }
        }
    }
    
    // Convert to vector and sort
    std::vector<std::string> actors_vec(all_actors.begin(), all_actors.end());
    std::sort(actors_vec.begin(), actors_vec.end());
    
    // Convert back to CharacterVector
    CharacterVector result(actors_vec.size());
    for (size_t i = 0; i < actors_vec.size(); i++) {
        result[i] = actors_vec[i];
    }
    
    return result;
}

//' Align two matrices to have the same actors
//'
//' @param mat1 First matrix
//' @param mat2 Second matrix
//' @param all_actors Character vector of all actors (optional)
//' @return List with two aligned matrices
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List align_matrices_cpp(NumericMatrix mat1, NumericMatrix mat2,
                       Nullable<CharacterVector> all_actors_arg = R_NilValue) {
    
    CharacterVector all_actors;
    
    if (all_actors_arg.isNotNull()) {
        all_actors = as<CharacterVector>(all_actors_arg);
    } else {
        // Get all unique actors from both matrices
        std::unordered_set<std::string> actors_set;
        
        // From mat1
        List dimnames1 = mat1.attr("dimnames");
        if (!Rf_isNull(dimnames1) && dimnames1.size() >= 2) {
            if (!Rf_isNull(dimnames1[0])) {
                CharacterVector rnames1 = as<CharacterVector>(dimnames1[0]);
                for (int i = 0; i < rnames1.size(); i++) {
                    actors_set.insert(std::string(rnames1[i]));
                }
            }
            if (!Rf_isNull(dimnames1[1])) {
                CharacterVector cnames1 = as<CharacterVector>(dimnames1[1]);
                for (int i = 0; i < cnames1.size(); i++) {
                    actors_set.insert(std::string(cnames1[i]));
                }
            }
        }
        
        // From mat2
        List dimnames2 = mat2.attr("dimnames");
        if (!Rf_isNull(dimnames2) && dimnames2.size() >= 2) {
            if (!Rf_isNull(dimnames2[0])) {
                CharacterVector rnames2 = as<CharacterVector>(dimnames2[0]);
                for (int i = 0; i < rnames2.size(); i++) {
                    actors_set.insert(std::string(rnames2[i]));
                }
            }
            if (!Rf_isNull(dimnames2[1])) {
                CharacterVector cnames2 = as<CharacterVector>(dimnames2[1]);
                for (int i = 0; i < cnames2.size(); i++) {
                    actors_set.insert(std::string(cnames2[i]));
                }
            }
        }
        
        // Convert to sorted vector
        std::vector<std::string> actors_vec(actors_set.begin(), actors_set.end());
        std::sort(actors_vec.begin(), actors_vec.end());
        
        all_actors = CharacterVector(actors_vec.size());
        for (size_t i = 0; i < actors_vec.size(); i++) {
            all_actors[i] = actors_vec[i];
        }
    }
    
    int n = all_actors.size();
    
    // Create aligned matrices
    NumericMatrix aligned1(n, n);
    NumericMatrix aligned2(n, n);
    
    // Set row and column names
    rownames(aligned1) = all_actors;
    colnames(aligned1) = all_actors;
    rownames(aligned2) = all_actors;
    colnames(aligned2) = all_actors;
    
    // Create maps for quick lookup
    std::unordered_map<std::string, int> actor_to_idx;
    for (int i = 0; i < n; i++) {
        actor_to_idx[std::string(all_actors[i])] = i;
    }
    
    // Fill aligned1
    List dimnames1 = mat1.attr("dimnames");
    CharacterVector rnames1, cnames1;
    if (!Rf_isNull(dimnames1) && dimnames1.size() >= 2) {
        if (!Rf_isNull(dimnames1[0])) rnames1 = as<CharacterVector>(dimnames1[0]);
        if (!Rf_isNull(dimnames1[1])) cnames1 = as<CharacterVector>(dimnames1[1]);
    }
    
    for (int i = 0; i < mat1.nrow(); i++) {
        for (int j = 0; j < mat1.ncol(); j++) {
            if (!NumericMatrix::is_na(mat1(i,j)) && rnames1.size() > i && cnames1.size() > j) {
                std::string row_actor = std::string(rnames1[i]);
                std::string col_actor = std::string(cnames1[j]);
                
                if (actor_to_idx.find(row_actor) != actor_to_idx.end() &&
                    actor_to_idx.find(col_actor) != actor_to_idx.end()) {
                    int new_i = actor_to_idx[row_actor];
                    int new_j = actor_to_idx[col_actor];
                    aligned1(new_i, new_j) = mat1(i, j);
                }
            }
        }
    }
    
    // Fill aligned2
    List dimnames2 = mat2.attr("dimnames");
    CharacterVector rnames2, cnames2;
    if (!Rf_isNull(dimnames2) && dimnames2.size() >= 2) {
        if (!Rf_isNull(dimnames2[0])) rnames2 = as<CharacterVector>(dimnames2[0]);
        if (!Rf_isNull(dimnames2[1])) cnames2 = as<CharacterVector>(dimnames2[1]);
    }
    
    for (int i = 0; i < mat2.nrow(); i++) {
        for (int j = 0; j < mat2.ncol(); j++) {
            if (!NumericMatrix::is_na(mat2(i,j)) && rnames2.size() > i && cnames2.size() > j) {
                std::string row_actor = std::string(rnames2[i]);
                std::string col_actor = std::string(cnames2[j]);
                
                if (actor_to_idx.find(row_actor) != actor_to_idx.end() &&
                    actor_to_idx.find(col_actor) != actor_to_idx.end()) {
                    int new_i = actor_to_idx[row_actor];
                    int new_j = actor_to_idx[col_actor];
                    aligned2(new_i, new_j) = mat2(i, j);
                }
            }
        }
    }
    
    return List::create(
        Named("mat1") = aligned1,
        Named("mat2") = aligned2
    );
}

//' Batch align multiple matrices
//'
//' @param nets_list List of network matrices
//' @param all_actors Character vector of all actors (optional)
//' @param include_diagonal Whether to include diagonal values
//' @return List of aligned matrices
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List batch_align_matrices_cpp(List nets_list,
                             Nullable<CharacterVector> all_actors_arg = R_NilValue,
                             bool include_diagonal = false) {
    
    CharacterVector all_actors;
    
    if (all_actors_arg.isNotNull()) {
        all_actors = as<CharacterVector>(all_actors_arg);
    } else {
        all_actors = get_all_actors_cpp(nets_list);
    }
    
    int n_nets = nets_list.size();
    int n_actors = all_actors.size();
    
    List aligned_list(n_nets);
    
    // Create map for quick lookup
    std::unordered_map<std::string, int> actor_to_idx;
    for (int i = 0; i < n_actors; i++) {
        actor_to_idx[std::string(all_actors[i])] = i;
    }
    
    // Align each matrix
    for (int net_idx = 0; net_idx < n_nets; net_idx++) {
        NumericMatrix mat = as<NumericMatrix>(nets_list[net_idx]);
        NumericMatrix aligned(n_actors, n_actors);
        
        // Set names
        rownames(aligned) = all_actors;
        colnames(aligned) = all_actors;
        
        // Fill values
        List dimnames = mat.attr("dimnames");
        CharacterVector rnames, cnames;
        if (!Rf_isNull(dimnames) && dimnames.size() >= 2) {
            if (!Rf_isNull(dimnames[0])) rnames = as<CharacterVector>(dimnames[0]);
            if (!Rf_isNull(dimnames[1])) cnames = as<CharacterVector>(dimnames[1]);
        }
        
        for (int i = 0; i < mat.nrow(); i++) {
            for (int j = 0; j < mat.ncol(); j++) {
                if (!include_diagonal && i == j) continue;
                
                if (!NumericMatrix::is_na(mat(i,j)) && rnames.size() > i && cnames.size() > j) {
                    std::string row_actor = std::string(rnames[i]);
                    std::string col_actor = std::string(cnames[j]);
                    
                    if (actor_to_idx.find(row_actor) != actor_to_idx.end() &&
                        actor_to_idx.find(col_actor) != actor_to_idx.end()) {
                        int new_i = actor_to_idx[row_actor];
                        int new_j = actor_to_idx[col_actor];
                        aligned(new_i, new_j) = mat(i, j);
                    }
                }
            }
        }
        
        aligned_list[net_idx] = aligned;
    }
    
    // Preserve names from input list
    if (nets_list.hasAttribute("names")) {
        aligned_list.names() = nets_list.names();
    }
    
    return aligned_list;
}
