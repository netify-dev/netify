#include <Rcpp.h>
#include <algorithm>
#include <random>
using namespace Rcpp;

//' Calculate similarity matrix between node attributes
//'
//' @param attributes numeric vector of node attributes
//' @param method character string specifying similarity metric ("correlation", "euclidean", "categorical", "cosine", "jaccard", "manhattan", "hamming")
//' @return numeric matrix of pairwise similarities
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix calculate_similarity_matrix_cpp(NumericVector attributes, std::string method) {
    int n = attributes.size();
    NumericMatrix sim_matrix(n, n);
    
    if (method == "correlation" || method == "euclidean") {
        // for both methods with single attributes, it's just negative abs difference
        for (int i = 0; i < n; i++) {
            sim_matrix(i, i) = 0.0;  // diagonal
            for (int j = i + 1; j < n; j++) {
                double diff = attributes[i] - attributes[j];
                double sim_value = -std::abs(diff);
                sim_matrix(i, j) = sim_value;
                sim_matrix(j, i) = sim_value;  // symmetric
            }
        }
    } 
    else if (method == "categorical") {
        for (int i = 0; i < n; i++) {
            sim_matrix(i, i) = 0.0;
            for (int j = i + 1; j < n; j++) {
                double sim_value = (attributes[i] == attributes[j]) ? 1.0 : 0.0;
                sim_matrix(i, j) = sim_value;
                sim_matrix(j, i) = sim_value;
            }
        }
    }
    else if (method == "cosine") {
        // precompute norms
        NumericVector norms(n);
        for (int i = 0; i < n; i++) {
            norms[i] = std::sqrt(attributes[i] * attributes[i]);
            if (norms[i] == 0) norms[i] = 1.0;  // avoid division by zero
        }
        
        for (int i = 0; i < n; i++) {
            sim_matrix(i, i) = 0.0;
            for (int j = i + 1; j < n; j++) {
                double sim_value = (attributes[i] * attributes[j]) / (norms[i] * norms[j]);
                sim_matrix(i, j) = sim_value;
                sim_matrix(j, i) = sim_value;
            }
        }
    }
    else if (method == "jaccard") {
        // for single attributes, jaccard similarity for binary data
        // treats non-zero values as 1, zero as 0
        for (int i = 0; i < n; i++) {
            sim_matrix(i, i) = 0.0;
            for (int j = i + 1; j < n; j++) {
                // convert to binary
                int a_i = (attributes[i] != 0) ? 1 : 0;
                int a_j = (attributes[j] != 0) ? 1 : 0;
                
                // jaccard = intersection / union
                // for single binary attributes:
                // intersection = both are 1
                // union = at least one is 1
                double intersection = (a_i == 1 && a_j == 1) ? 1.0 : 0.0;
                double union_size = (a_i == 1 || a_j == 1) ? 1.0 : 0.0;
                
                double sim_value;
                if (union_size == 0) {
                    // both are 0, similarity is 0 (or could be 1, depending on interpretation)
                    sim_value = 0.0;
                } else {
                    sim_value = intersection / union_size;
                }
                
                sim_matrix(i, j) = sim_value;
                sim_matrix(j, i) = sim_value;
            }
        }
    }
    else if (method == "manhattan") {
        // manhattan distance (L1 norm), negated for similarity
        for (int i = 0; i < n; i++) {
            sim_matrix(i, i) = 0.0;
            for (int j = i + 1; j < n; j++) {
                double sim_value = -std::abs(attributes[i] - attributes[j]);
                sim_matrix(i, j) = sim_value;
                sim_matrix(j, i) = sim_value;
            }
        }
    }
    else if (method == "hamming") {
        // hamming distance for categorical data
        // for single attributes, it's 0 if equal, 1 if different
        // we negate to make it a similarity (0 if different, -1 if same)
        for (int i = 0; i < n; i++) {
            sim_matrix(i, i) = 0.0;
            for (int j = i + 1; j < n; j++) {
                double sim_value = (attributes[i] == attributes[j]) ? 0.0 : -1.0;
                sim_matrix(i, j) = sim_value;
                sim_matrix(j, i) = sim_value;
            }
        }
    }
    
    return sim_matrix;
}

//' Correlation calcs for homophily analysis
//'
//' @param x numeric vector of similarities
//' @param y numeric vector of ties (0/1)
//' @return correlation coefficient
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
double correlation_cpp(NumericVector x, NumericVector y) {
    int n = x.size();
    if (n != y.size()) {
        stop("Vectors must be same length");
    }
    
    double sum_x = 0.0, sum_y = 0.0, sum_xy = 0.0;
    double sum_x2 = 0.0, sum_y2 = 0.0;
    
    for (int i = 0; i < n; i++) {
        sum_x += x[i];
        sum_y += y[i];
        sum_xy += x[i] * y[i];
        sum_x2 += x[i] * x[i];
        sum_y2 += y[i] * y[i];
    }
    
    double mean_x = sum_x / n;
    double mean_y = sum_y / n;
    
    double cov = (sum_xy / n) - (mean_x * mean_y);
    double sd_x = std::sqrt((sum_x2 / n) - (mean_x * mean_x));
    double sd_y = std::sqrt((sum_y2 / n) - (mean_y * mean_y));
    
    if (sd_x == 0 || sd_y == 0) {
        return NA_REAL;
    }
    
    return cov / (sd_x * sd_y);
}

//' Calculate homophily statistics with significance testing
//'
//' @param similarity_matrix numeric matrix of pairwise node similarities
//' @param net_matrix logical matrix of network ties
//' @param significance_test logical whether to perform permutation test
//' @param n_permutations integer number of permutations
//' @param alpha numeric significance level for confidence intervals
//' @return list containing homophily statistics
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List calculate_homophily_stats_cpp(NumericMatrix similarity_matrix, 
                                  LogicalMatrix net_matrix,
                                  bool significance_test,
                                  int n_permutations,
                                  double alpha) {
    
    int n = similarity_matrix.nrow();
    
    // extract upper triangle
    std::vector<double> similarities;
    std::vector<int> ties;
    
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            if (!NumericMatrix::is_na(similarity_matrix(i, j)) && 
                !LogicalMatrix::is_na(net_matrix(i, j))) {
                similarities.push_back(similarity_matrix(i, j));
                ties.push_back(net_matrix(i, j) ? 1 : 0);
            }
        }
    }
    
    int n_pairs = similarities.size();
    if (n_pairs == 0) {
        return List::create(
            Named("homophily_correlation") = NA_REAL,
            Named("mean_similarity_connected") = NA_REAL,
            Named("mean_similarity_unconnected") = NA_REAL,
            Named("similarity_difference") = NA_REAL,
            Named("p_value") = NA_REAL,
            Named("ci_lower") = NA_REAL,
            Named("ci_upper") = NA_REAL,
            Named("n_connected_pairs") = 0,
            Named("n_unconnected_pairs") = 0
        );
    }
    
    // calculate means
    double sum_connected = 0.0, sum_unconnected = 0.0;
    int n_connected = 0, n_unconnected = 0;
    
    for (int i = 0; i < n_pairs; i++) {
        if (ties[i] == 1) {
            sum_connected += similarities[i];
            n_connected++;
        } else {
            sum_unconnected += similarities[i];
            n_unconnected++;
        }
    }
    
    double mean_connected = n_connected > 0 ? sum_connected / n_connected : NA_REAL;
    double mean_unconnected = n_unconnected > 0 ? sum_unconnected / n_unconnected : NA_REAL;
    double similarity_diff = (!NumericVector::is_na(mean_connected) && 
                             !NumericVector::is_na(mean_unconnected)) ? 
                             mean_connected - mean_unconnected : NA_REAL;
    
    // calculate correlation
    NumericVector sim_vec(similarities.begin(), similarities.end());
    NumericVector ties_vec(ties.begin(), ties.end());
    double homophily_cor = correlation_cpp(sim_vec, ties_vec);
    
    double p_value = NA_REAL;
    double ci_lower = NA_REAL;
    double ci_upper = NA_REAL;
    
    if (significance_test && !NumericVector::is_na(homophily_cor)) {
        // create random number generator
        std::random_device rd;
        std::mt19937 rng(rd());
        
        // permutation test
        NumericVector null_cors(n_permutations);
        
        for (int i = 0; i < n_permutations; i++) {
            // shuffle similarities
            NumericVector perm_sim = clone(sim_vec);
            // use std::shuffle instead of std::random_shuffle
            std::shuffle(perm_sim.begin(), perm_sim.end(), rng);
            null_cors[i] = correlation_cpp(perm_sim, ties_vec);
        }
        
        // calculate p-value
        int extreme_count = 0;
        int valid_count = 0;
        for (int i = 0; i < n_permutations; i++) {
            if (!NumericVector::is_na(null_cors[i])) {
                valid_count++;
                if (std::abs(null_cors[i]) >= std::abs(homophily_cor)) {
                    extreme_count++;
                }
            }
        }
        p_value = valid_count > 0 ? (double)extreme_count / valid_count : NA_REAL;
        
        // bootstrap CIs
        NumericVector boot_cors(n_permutations);
        // create uniform distribution for sampling
        std::uniform_int_distribution<int> uniform_dist(0, n_pairs - 1);
        
        for (int i = 0; i < n_permutations; i++) {
            // resample with replacement
            NumericVector boot_sim(n_pairs);
            NumericVector boot_ties(n_pairs);
            
            for (int j = 0; j < n_pairs; j++) {
                int idx = uniform_dist(rng);
                boot_sim[j] = sim_vec[idx];
                boot_ties[j] = ties_vec[idx];
            }
            
            boot_cors[i] = correlation_cpp(boot_sim, boot_ties);
        }
        
        // remove NAs and calculate quantiles
        LogicalVector not_na = !is_na(boot_cors);
        NumericVector valid_boots = boot_cors[not_na];
        
        if (valid_boots.size() > 0) {
            std::sort(valid_boots.begin(), valid_boots.end());
            int lower_idx = floor(alpha / 2 * valid_boots.size());
            int upper_idx = floor((1 - alpha / 2) * valid_boots.size());
            // make sure indices are within bounds
            lower_idx = std::max(0, std::min(lower_idx, (int)valid_boots.size() - 1));
            upper_idx = std::max(0, std::min(upper_idx, (int)valid_boots.size() - 1));
            ci_lower = valid_boots[lower_idx];
            ci_upper = valid_boots[upper_idx];
        }
    }
    
    return List::create(
        Named("homophily_correlation") = homophily_cor,
        Named("mean_similarity_connected") = mean_connected,
        Named("mean_similarity_unconnected") = mean_unconnected,
        Named("similarity_difference") = similarity_diff,
        Named("p_value") = p_value,
        Named("ci_lower") = ci_lower,
        Named("ci_upper") = ci_upper,
        Named("n_connected_pairs") = n_connected,
        Named("n_unconnected_pairs") = n_unconnected
    );
}