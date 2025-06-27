#include <Rcpp.h>
#include <RcppEigen.h>
#include <algorithm>
#include <random>
#include <unordered_map>

using namespace Rcpp;
// [[Rcpp::depends(RcppEigen)]]

//' Fast Jaccard similarity calculation
//'
//' @param mat1 First matrix
//' @param mat2 Second matrix  
//' @param threshold1 Threshold for first matrix
//' @param threshold2 Threshold for second matrix
//' @return Jaccard similarity coefficient
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
double calculate_jaccard_cpp(NumericMatrix mat1, NumericMatrix mat2,
                            double threshold1, double threshold2) {
    int nrow = mat1.nrow();
    int ncol = mat1.ncol();
    
    if (nrow != mat2.nrow() || ncol != mat2.ncol()) {
        stop("Matrices must have same dimensions");
    }
    
    int intersection = 0;
    int union_count = 0;
    
    for (int i = 0; i < nrow; i++) {
        for (int j = 0; j < ncol; j++) {
            bool edge1 = !NumericMatrix::is_na(mat1(i,j)) && mat1(i,j) > threshold1;
            bool edge2 = !NumericMatrix::is_na(mat2(i,j)) && mat2(i,j) > threshold2;
            
            if (edge1 && edge2) intersection++;
            if (edge1 || edge2) union_count++;
        }
    }
    
    if (union_count == 0) return 0.0;
    return (double)intersection / union_count;
}

//' Fast Hamming distance calculation
//'
//' @param mat1 First matrix
//' @param mat2 Second matrix
//' @param threshold1 Threshold for first matrix
//' @param threshold2 Threshold for second matrix
//' @return Normalized Hamming distance
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
double calculate_hamming_cpp(NumericMatrix mat1, NumericMatrix mat2,
                            double threshold1, double threshold2) {
    int nrow = mat1.nrow();
    int ncol = mat1.ncol();
    
    if (nrow != mat2.nrow() || ncol != mat2.ncol()) {
        stop("Matrices must have same dimensions");
    }
    
    int different = 0;
    int total = 0;
    
    for (int i = 0; i < nrow; i++) {
        for (int j = 0; j < ncol; j++) {
            bool na1 = NumericMatrix::is_na(mat1(i,j));
            bool na2 = NumericMatrix::is_na(mat2(i,j));
            
            if (!na1 || !na2) {
                total++;
                bool edge1 = !na1 && mat1(i,j) > threshold1;
                bool edge2 = !na2 && mat2(i,j) > threshold2;
                
                if (edge1 != edge2) different++;
            }
        }
    }
    
    if (total == 0) return 0.0;
    return (double)different / total;
}

//' Calculate edge changes between networks
//'
//' @param mat1 First matrix
//' @param mat2 Second matrix
//' @param threshold1 Threshold for first matrix
//' @param threshold2 Threshold for second matrix
//' @return List with added, removed, maintained counts and weight correlation
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List calculate_edge_changes_cpp(NumericMatrix mat1, NumericMatrix mat2,
                               double threshold1, double threshold2) {
    int nrow = mat1.nrow();
    int ncol = mat1.ncol();
    
    if (nrow != mat2.nrow() || ncol != mat2.ncol()) {
        stop("Matrices must have same dimensions");
    }
    
    int added = 0, removed = 0, maintained = 0;
    std::vector<double> weights1, weights2;
    
    for (int i = 0; i < nrow; i++) {
        for (int j = 0; j < ncol; j++) {
            bool na1 = NumericMatrix::is_na(mat1(i,j));
            bool na2 = NumericMatrix::is_na(mat2(i,j));
            
            bool edge1 = !na1 && mat1(i,j) > threshold1;
            bool edge2 = !na2 && mat2(i,j) > threshold2;
            
            if (edge1 && edge2) {
                maintained++;
                weights1.push_back(mat1(i,j));
                weights2.push_back(mat2(i,j));
            } else if (!edge1 && edge2) {
                added++;
            } else if (edge1 && !edge2) {
                removed++;
            }
        }
    }
    
    // Calculate weight correlation for maintained edges
    double weight_correlation = NA_REAL;
    if (weights1.size() >= 3) {
        // Calculate correlation manually to avoid dependency
        double n = weights1.size();
        double sum_x = 0, sum_y = 0, sum_xy = 0, sum_x2 = 0, sum_y2 = 0;
        
        for (size_t i = 0; i < weights1.size(); i++) {
            sum_x += weights1[i];
            sum_y += weights2[i];
            sum_xy += weights1[i] * weights2[i];
            sum_x2 += weights1[i] * weights1[i];
            sum_y2 += weights2[i] * weights2[i];
        }
        
        double cov = (sum_xy / n) - (sum_x / n) * (sum_y / n);
        double sd_x = sqrt((sum_x2 / n) - pow(sum_x / n, 2));
        double sd_y = sqrt((sum_y2 / n) - pow(sum_y / n, 2));
        
        if (sd_x > 0 && sd_y > 0) {
            weight_correlation = cov / (sd_x * sd_y);
        }
    }
    
    return List::create(
        Named("added") = added,
        Named("removed") = removed,
        Named("maintained") = maintained,
        Named("weight_correlation") = weight_correlation
    );
}

//' QAP correlation test with permutations
//'
//' @param mat1 First matrix
//' @param mat2 Second matrix
//' @param n_permutations Number of permutations
//' @return List with correlation and p-value
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List qap_correlation_cpp(NumericMatrix mat1, NumericMatrix mat2, int n_permutations) {
    int n = mat1.nrow();
    
    if (n != mat1.ncol() || n != mat2.nrow() || n != mat2.ncol()) {
        stop("Matrices must be square and same size");
    }
    
    // Vectorize matrices
    std::vector<double> vec1, vec2;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (!NumericMatrix::is_na(mat1(i,j)) && !NumericMatrix::is_na(mat2(i,j))) {
                vec1.push_back(mat1(i,j));
                vec2.push_back(mat2(i,j));
            }
        }
    }
    
    if (vec1.size() < 3) {
        return List::create(
            Named("correlation") = NA_REAL,
            Named("p_value") = NA_REAL
        );
    }
    
    // Calculate observed correlation
    double n_obs = vec1.size();
    double sum_x = 0, sum_y = 0, sum_xy = 0, sum_x2 = 0, sum_y2 = 0;
    
    for (size_t i = 0; i < vec1.size(); i++) {
        sum_x += vec1[i];
        sum_y += vec2[i];
        sum_xy += vec1[i] * vec2[i];
        sum_x2 += vec1[i] * vec1[i];
        sum_y2 += vec2[i] * vec2[i];
    }
    
    double mean_x = sum_x / n_obs;
    double mean_y = sum_y / n_obs;
    double cov = (sum_xy / n_obs) - mean_x * mean_y;
    double sd_x = sqrt((sum_x2 / n_obs) - mean_x * mean_x);
    double sd_y = sqrt((sum_y2 / n_obs) - mean_y * mean_y);
    
    double obs_cor = (sd_x > 0 && sd_y > 0) ? cov / (sd_x * sd_y) : 0.0;
    
    // Permutation test
    std::random_device rd;
    std::mt19937 rng(rd());
    std::vector<int> perm_indices(n);
    std::iota(perm_indices.begin(), perm_indices.end(), 0);
    
    int extreme_count = 0;
    
    for (int perm = 0; perm < n_permutations; perm++) {
        // Shuffle row and column indices
        std::shuffle(perm_indices.begin(), perm_indices.end(), rng);
        
        // Create permuted matrix and recalculate correlation
        vec1.clear();
        vec2.clear();
        
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                int pi = perm_indices[i];
                int pj = perm_indices[j];
                if (!NumericMatrix::is_na(mat1(pi,pj)) && !NumericMatrix::is_na(mat2(i,j))) {
                    vec1.push_back(mat1(pi,pj));
                    vec2.push_back(mat2(i,j));
                }
            }
        }
        
        if (vec1.size() >= 3) {
            // Recalculate correlation for permuted data
            double n_perm = vec1.size();
            sum_x = 0; sum_y = 0; sum_xy = 0; sum_x2 = 0; sum_y2 = 0;
            
            for (size_t i = 0; i < vec1.size(); i++) {
                sum_x += vec1[i];
                sum_y += vec2[i];
                sum_xy += vec1[i] * vec2[i];
                sum_x2 += vec1[i] * vec1[i];
                sum_y2 += vec2[i] * vec2[i];
            }
            
            mean_x = sum_x / n_perm;
            mean_y = sum_y / n_perm;
            cov = (sum_xy / n_perm) - mean_x * mean_y;
            sd_x = sqrt((sum_x2 / n_perm) - mean_x * mean_x);
            sd_y = sqrt((sum_y2 / n_perm) - mean_y * mean_y);
            
            double perm_cor = (sd_x > 0 && sd_y > 0) ? cov / (sd_x * sd_y) : 0.0;
            
            if (std::abs(perm_cor) >= std::abs(obs_cor)) {
                extreme_count++;
            }
        }
    }
    
    double p_value = (double)extreme_count / n_permutations;
    
    return List::create(
        Named("correlation") = obs_cor,
        Named("p_value") = p_value
    );
}

//' Calculate spectral distance between networks
//'
//' @param mat1 First matrix
//' @param mat2 Second matrix
//' @return Spectral distance
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
double calculate_spectral_distance_cpp(NumericMatrix mat1, NumericMatrix mat2) {
    int n = mat1.nrow();
    
    if (n != mat1.ncol() || n != mat2.nrow() || n != mat2.ncol()) {
        stop("Matrices must be square and same size");
    }
    
    // Convert to Eigen matrices
    Eigen::Map<Eigen::MatrixXd> eigen_mat1(mat1.begin(), n, n);
    Eigen::Map<Eigen::MatrixXd> eigen_mat2(mat2.begin(), n, n);
    
    // Create Laplacian matrices
    Eigen::MatrixXd L1 = Eigen::MatrixXd::Zero(n, n);
    Eigen::MatrixXd L2 = Eigen::MatrixXd::Zero(n, n);
    
    // Calculate degree matrices and Laplacians
    for (int i = 0; i < n; i++) {
        double deg1 = 0, deg2 = 0;
        for (int j = 0; j < n; j++) {
            if (!std::isnan(eigen_mat1(i,j))) {
                deg1 += std::abs(eigen_mat1(i,j));
                L1(i,j) = -eigen_mat1(i,j);
            }
            if (!std::isnan(eigen_mat2(i,j))) {
                deg2 += std::abs(eigen_mat2(i,j));
                L2(i,j) = -eigen_mat2(i,j);
            }
        }
        L1(i,i) = deg1;
        L2(i,i) = deg2;
    }
    
    // Compute eigenvalues
    Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es1(L1);
    Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es2(L2);
    
    Eigen::VectorXd eig1 = es1.eigenvalues();
    Eigen::VectorXd eig2 = es2.eigenvalues();
    
    // Sort eigenvalues (they should already be sorted by Eigen)
    std::sort(eig1.data(), eig1.data() + eig1.size());
    std::sort(eig2.data(), eig2.data() + eig2.size());
    
    // Calculate spectral distance
    double distance = 0.0;
    for (int i = 0; i < n; i++) {
        distance += std::pow(eig1(i) - eig2(i), 2);
    }
    
    return std::sqrt(distance);
}
