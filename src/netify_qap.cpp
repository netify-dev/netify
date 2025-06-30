// =====================================================================
//  netify_qap.cpp   –   self‑contained C++ back‑end for compare_networks
//      • calculate_wasserstein_cpp   (Wasserstein distance)
//      • calculate_jaccard_cpp
//      • calculate_hamming_cpp
//      • calculate_edge_changes_cpp
//      • calculate_spectral_distance_cpp
//      • double_center_cpp
//      • qap_correlation_cpp         (classic label permutation)
//      • qap_degree_cpp              (degree‑preserving, binary only)
//      • qap_freeman_lane_cpp        (Freeman–Lane MRQAP)
//      • qap_dsp_cpp                 (Double‑Semi‑Partial MRQAP)
// =====================================================================

#include <Rcpp.h>
#include <RcppEigen.h>
#include <algorithm>
#include <numeric>
#include <random>
#include <unordered_set>

using namespace Rcpp;

// [[Rcpp::depends(RcppEigen)]]

// ------------------------------------------------------------------
//   wasserstein distance calculation
// ------------------------------------------------------------------

//' Calculate Wasserstein-1 distance between two distributions
//'
//' @param x First vector of values
//' @param y Second vector of values
//' @return Wasserstein-1 distance between empirical distributions
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
double calculate_wasserstein_cpp(NumericVector x, NumericVector y) {
    // remove na values from both vectors
    std::vector<double> x_clean, y_clean;
    for (int i = 0; i < x.size(); ++i) {
        if (!NumericVector::is_na(x[i])) x_clean.push_back(x[i]);
    }
    for (int i = 0; i < y.size(); ++i) {
        if (!NumericVector::is_na(y[i])) y_clean.push_back(y[i]);
    }
    
    // check for empty vectors after na removal
    if (x_clean.empty() || y_clean.empty()) return NA_REAL;
    
    // sort both vectors for cdf calculation
    std::sort(x_clean.begin(), x_clean.end());
    std::sort(y_clean.begin(), y_clean.end());
    
    // compute empirical cdfs on the combined support
    // combine all unique values from both distributions
    std::vector<double> combined;
    combined.insert(combined.end(), x_clean.begin(), x_clean.end());
    combined.insert(combined.end(), y_clean.begin(), y_clean.end());
    std::sort(combined.begin(), combined.end());
    
    // remove duplicate values to get unique support points
    combined.erase(std::unique(combined.begin(), combined.end()), combined.end());
    
    // calculate wasserstein-1 distance as integral of |F_x - F_y|
    double wasserstein = 0.0;
    size_t i_x = 0, i_y = 0;
    double cdf_x = 0.0, cdf_y = 0.0;
    
    // iterate through all support points
    for (size_t i = 0; i < combined.size(); ++i) {
        // update cdf for x distribution
        while (i_x < x_clean.size() && x_clean[i_x] <= combined[i]) {
            cdf_x += 1.0 / x_clean.size();
            i_x++;
        }
        // update cdf for y distribution
        while (i_y < y_clean.size() && y_clean[i_y] <= combined[i]) {
            cdf_y += 1.0 / y_clean.size();
            i_y++;
        }
        
        // add area between cdfs (rectangle with height |cdf_x - cdf_y| and width to next point)
        if (i < combined.size() - 1) {
            wasserstein += std::abs(cdf_x - cdf_y) * (combined[i+1] - combined[i]);
        }
    }
    
    return wasserstein;
}

// ------------------------------------------------------------------
// helper functions
// ------------------------------------------------------------------

// speedy pearson correlation between two vectors
// avoids using r's cor() function for a need for speed
inline double fast_cor(
    const std::vector<double>& x,
    const std::vector<double>& y
    ){

    const size_t n = x.size();
    double sx=0, sy=0, sxx=0, syy=0, sxy=0;
    
    // calculate sums and sum of squares in one go
    for (size_t i=0;i<n;++i){
        sx  += x[i];  sy  += y[i];
        sxx += x[i]*x[i];
        syy += y[i]*y[i];
        sxy += x[i]*y[i];
    }
    
    // calculate means
    const double mx = sx/n, my = sy/n;
    
    // calculate covars and vars
    const double cov = sxy/n - mx*my;
    const double vx  = sxx/n - mx*mx;
    const double vy  = syy/n - my*my;
    
    // handle zero variance case
    if (vx<=0 || vy<=0) return 0.0;
    
    // return correlation coefficient
    return cov / std::sqrt(vx*vy);
}

// calculate ols residuals of y regressed on x (simple bivariate)
// modifies y in-place to contain residuals
inline void ols_residuals(
    std::vector<double>& y,
    const std::vector<double>& x
    ){
    double sxx=0, sxy=0;
    
    // calculate sum of squares and cross-product
    for (size_t i=0;i<y.size();++i){ 
        sxx += x[i]*x[i]; 
        sxy += x[i]*y[i]; 
    }
    
    // calculate beta coefficient (slope)
    double beta = (sxx==0) ? 0 : sxy/sxx;
    
    // subtract y hats from y to get resids
    for (size_t i=0;i<y.size();++i) y[i] -= beta*x[i];
}

// ------------------------------------------------------------------
//   1. jaccard similarity calculation
// ------------------------------------------------------------------

//' Calculate Jaccard similarity between two matrices
//'
//' @param mat1 First matrix
//' @param mat2 Second matrix  
//' @param threshold1 Threshold for mat1 edges
//' @param threshold2 Threshold for mat2 edges
//' @return Jaccard similarity coefficient
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
double calculate_jaccard_cpp(
    NumericMatrix mat1, NumericMatrix mat2,
    double threshold1, double threshold2
    ){

    const int n = mat1.nrow(), m = mat1.ncol();
    
    // ensure matrices have same dimensions
    if (n!=mat2.nrow() || m!=mat2.ncol())
        stop("Matrices must have same dimensions");

    int intersect = 0, uni = 0;
    
    // iterate through all matrix positions
    for (int i=0;i<n;++i) for (int j=0;j<m;++j) {
        // check if edge exists in mat1 (not na and above threshold)
        const bool e1 = !NumericMatrix::is_na(mat1(i,j)) && mat1(i,j)>threshold1;
        // check if edge exists in mat2 (not na and above threshold)
        const bool e2 = !NumericMatrix::is_na(mat2(i,j)) && mat2(i,j)>threshold2;
        
        // count union (edge in either matrix)
        if (e1 || e2) ++uni;
        // count intersection (edge in both matrices)
        if (e1 && e2) ++intersect;
    }
    
    // return jaccard coefficient (intersection/union)
    return (uni==0) ? 0.0 : double(intersect)/uni;
}

// ------------------------------------------------------------------
//   2. hamming distance calculation
// ------------------------------------------------------------------

//' Calculate Hamming distance between two matrices
//'
//' @param mat1 First matrix
//' @param mat2 Second matrix
//' @param threshold1 Threshold for mat1 edges
//' @param threshold2 Threshold for mat2 edges
//' @return Hamming distance (proportion of differing edges)
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
double calculate_hamming_cpp(
    NumericMatrix mat1, NumericMatrix mat2,
    double threshold1, double threshold2
    ){

    const int n = mat1.nrow(), m = mat1.ncol();
    
    // ensure matrices have same dimensions
    if (n!=mat2.nrow() || m!=mat2.ncol())
        stop("Matrices must have same dimensions");

    int diff = 0, tot = 0;
    
    // iterate through all matrix positions
    for (int i=0;i<n;++i) for (int j=0;j<m;++j) {
        // check for na values
        const bool na1 = NumericMatrix::is_na(mat1(i,j));
        const bool na2 = NumericMatrix::is_na(mat2(i,j));
        
        // skip if both are na
        if (na1 && na2) continue;
        
        // count total valid comparisons
        ++tot;
        
        // check edge existence based on thresholds
        const bool e1 = !na1 && mat1(i,j)>threshold1;
        const bool e2 = !na2 && mat2(i,j)>threshold2;
        
        // count differences
        if (e1 != e2) ++diff;
    }
    
    // return proportion of differences
    return (tot==0)?0.0:double(diff)/tot;
}

// ------------------------------------------------------------------
//   3. edge change analysis
// ------------------------------------------------------------------

//' Calculate edge changes between two matrices
//'
//' @param mat1 First matrix
//' @param mat2 Second matrix
//' @param threshold1 Threshold for mat1 edges
//' @param threshold2 Threshold for mat2 edges
//' @return List with added, removed, maintained edges and weight correlation
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List calculate_edge_changes_cpp(
    NumericMatrix mat1, NumericMatrix mat2,
    double threshold1, double threshold2
    ){

    const int n = mat1.nrow(), m = mat1.ncol();
    
    // ensure matrices have same dimensions
    if (n!=mat2.nrow() || m!=mat2.ncol())
        stop("Matrices must have same dimensions");

    int added=0, removed=0, kept=0;
    std::vector<double> w1, w2;  // weights of maintained edges

    // iterate through all matrix positions
    for (int i=0;i<n;++i) for (int j=0;j<m;++j) {
        // check edge existence in both matrices
        const bool e1 = !NumericMatrix::is_na(mat1(i,j)) && mat1(i,j)>threshold1;
        const bool e2 = !NumericMatrix::is_na(mat2(i,j)) && mat2(i,j)>threshold2;

        if (e1 && e2) {
            // edge exists in both - maintained
            ++kept; 
            // store weights for correlation calculation
            w1.push_back(mat1(i,j)); 
            w2.push_back(mat2(i,j));
        } else if (!e1 && e2) {
            // edge only in mat2 - added
            ++added;
        } else if (e1 && !e2) {
            // edge only in mat1 - removed
            ++removed;
        }
    }

    // calculate weight correlation if enough maintained edges
    double weight_cor = NA_REAL;
    if (w1.size()>=3) weight_cor = fast_cor(w1,w2);

    return List::create(_["added"]=added,
                        _["removed"]=removed,
                        _["maintained"]=kept,
                        _["weight_correlation"]=weight_cor);
}

// ------------------------------------------------------------------
//   4. spectral distance between networks
// ------------------------------------------------------------------

//' Calculate spectral distance between two matrices
//'
//' @param mat1 First matrix
//' @param mat2 Second matrix
//' @param spectral_rank Number of eigenvalues to use (0 = all)
//' @return Spectral distance based on Laplacian eigenvalues
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
double calculate_spectral_distance_cpp(
    NumericMatrix mat1, 
    NumericMatrix mat2, 
    int spectral_rank = 0
    ){

    const int n = mat1.nrow();
    
    // ensure square matrices of equal size
    if (n!=mat1.ncol() || n!=mat2.nrow() || n!=mat2.ncol())
        stop("Square matrices of equal size required");

    // map r matrices to eigen matrices
    Eigen::Map<Eigen::MatrixXd> A(mat1.begin(), n, n);
    Eigen::Map<Eigen::MatrixXd> B(mat2.begin(), n, n);

    // initialize laplacian matrices
    Eigen::MatrixXd L1 = Eigen::MatrixXd::Zero(n,n);
    Eigen::MatrixXd L2 = Eigen::MatrixXd::Zero(n,n);

    // construct laplacian matrices
    // laplacian = degree matrix - adjacency matrix
    for (int i=0;i<n;++i) {
        double deg1=0, deg2=0;
        
        // calculate degree and set off-diagonal elements
        for (int j=0;j<n;++j) {
            // for mat1
            if (!std::isnan(A(i,j))) { 
                deg1 += std::abs(A(i,j));  // sum absolute values for degree
                L1(i,j) = -A(i,j);          // negative adjacency for off-diagonal
            }
            // for mat2
            if (!std::isnan(B(i,j))) { 
                deg2 += std::abs(B(i,j));  // sum absolute values for degree
                L2(i,j) = -B(i,j);          // negative adjacency for off-diagonal
            }
        }
        // set diagonal to degree
        L1(i,i)=deg1; 
        L2(i,i)=deg2;
    }

    // compute eigenvalues of both laplacians
    Eigen::VectorXd e1 = Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd>(L1).eigenvalues();
    Eigen::VectorXd e2 = Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd>(L2).eigenvalues();

    // determine how many eigenvalues to use
    int k = n;  // default: use all eigenvalues
    if (spectral_rank > 0 && spectral_rank < n) {
        k = spectral_rank;  // use only top-k eigenvalues
    }
    
    // calculate euclidean distance between eigenvalue vectors
    // using only the largest k eigenvalues (eigenvalues are sorted in ascending order)
    double dist=0.0;
    for (int i=n-k; i<n; ++i) dist += std::pow(e1[i]-e2[i],2);
    
    return std::sqrt(dist);
}

// ------------------------------------------------------------------
//   5. double centering for distance matrices
// ------------------------------------------------------------------

//' Double center a matrix
//'
//' @param A Input matrix
//' @return Double-centered matrix
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericMatrix double_center_cpp(
    const NumericMatrix& A
    ){

    const int n = A.nrow();
    NumericVector rMeans(n), cMeans(n);
    double grand=0; 
    int cnt=0;

    // calculate row means, column means, and grand mean
    for (int i=0;i<n;++i) for (int j=0;j<n;++j)
        if (!NumericMatrix::is_na(A(i,j))) {
            rMeans[i]+=A(i,j);  // sum for row i
            cMeans[j]+=A(i,j);  // sum for column j
            grand+=A(i,j);      // sum for grand mean
            ++cnt;              // count non-na elements
        }
    
    // convert sums to means
    grand/=cnt;
    for (int i=0;i<n;++i){ 
        rMeans[i]/=n; 
        cMeans[i]/=n; 
    }

    // create output matrix as copy of input
    NumericMatrix out(clone(A));
    
    // apply double centering formula
    // centered_ij = a_ij - row_mean_i - col_mean_j + grand_mean
    for (int i=0;i<n;++i) for (int j=0;j<n;++j)
        if (!NumericMatrix::is_na(A(i,j)))
            out(i,j)=A(i,j)-rMeans[i]-cMeans[j]+grand;

    return out;
}

// ------------------------------------------------------------------
//   6. classic qap with label permutation
// ------------------------------------------------------------------

//' QAP correlation test with node label permutation
//'
//' @param mat1 First matrix
//' @param mat2 Second matrix
//' @param n_permutations Number of permutations
//' @param seed Random seed (-1 for random)
//' @return List with correlation and p-value
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List qap_correlation_cpp(
    const NumericMatrix& mat1,
    const NumericMatrix& mat2,
    int n_permutations,
    int seed = -1
    ){

    const int n = mat1.nrow();
    
    // ensure square matrices of equal size
    if (n!=mat1.ncol() || n!=mat2.nrow() || n!=mat2.ncol())
        stop("Square matrices of equal size required");

    // extract valid (non-na) pairs from both matrices
    std::vector<int> ii, jj;        // row and column indices
    std::vector<double> v1, v2;     // values from mat1 and mat2
    
    for (int i=0;i<n;++i) for (int j=0;j<n;++j)
        if (!NumericMatrix::is_na(mat1(i,j)) && !NumericMatrix::is_na(mat2(i,j))){
            ii.push_back(i); 
            jj.push_back(j);
            v1.push_back(mat1(i,j)); 
            v2.push_back(mat2(i,j));
        }
    
    // calculate observed correlation
    const double obs_cor = fast_cor(v1,v2);

    // setup random number generator
    std::mt19937 rng( (seed>=0)? seed : std::random_device{}() );
    
    // create permutation vector [0,1,2,...,n-1]
    std::vector<int> perm(n); 
    std::iota(perm.begin(),perm.end(),0);
    
    int ge=0;  // count permutations with |cor| >= |observed|

    // permutation test loop
    for (int r=0;r<n_permutations;++r) {
        // randomly shuffle node labels
        std::shuffle(perm.begin(), perm.end(), rng);
        
        // extract values from mat1 using permuted indices
        for (size_t k=0;k<v1.size();++k)
            v1[k] = mat1( perm[ii[k]], perm[jj[k]] );
        
        // check if absolute correlation is as extreme as observed
        if (std::abs(fast_cor(v1,v2)) >= std::abs(obs_cor)) ++ge;
    }

    return List::create(_["correlation"]=obs_cor,
                        _["p_value"]=double(ge)/n_permutations);
}

// ------------------------------------------------------------------
//   7. degree-preserving qap (for binary networks)
// ------------------------------------------------------------------

// perform one edge swap while preserving degrees
// swaps edges (a,b) and (c,d) to (a,d) and (c,b)
inline void one_edge_swap(
    std::vector<std::pair<int,int>>& edges,
    std::vector<char>& bit,
    std::mt19937& rng,
    int n
    ){

    std::uniform_int_distribution<> U(0, edges.size()-1);
    int tries=0;
    
    // try up to 10 times to find valid swap
    while (tries<10) {
        // randomly select two edges
        auto& e1 = edges[ U(rng) ];
        auto& e2 = edges[ U(rng) ];
        
        // skip if same edge selected twice
        if (e1==e2){ ++tries; continue; }

        // extract nodes
        int a=e1.first, b=e1.second, c=e2.first, d=e2.second;
        
        // skip if edges share a node (swap would create self-loop or multi-edge)
        if (a==c || b==d || a==d || c==b){ ++tries; continue; }

        // calculate indices for new edges
        long long ad = 1LL*a*n + d;
        long long cb = 1LL*c*n + b;
        
        // skip if new edges already exist
        if (bit[ad] || bit[cb]){ ++tries; continue; }

        // perform the swap
        bit[1LL*a*n + b]=0;  // remove (a,b)
        bit[1LL*c*n + d]=0;  // remove (c,d)
        bit[ad]=1;           // add (a,d)
        bit[cb]=1;           // add (c,b)
        
        // update edge list
        e1.second=d; 
        e2.second=b;
        return;
    }
}

//' QAP correlation test with degree-preserving permutation
//'
//' @param mat1 First matrix (must be binary)
//' @param mat2 Second matrix
//' @param n_permutations Number of permutations
//' @param swaps_factor Multiplier for number of edge swaps per permutation
//' @param seed Random seed (-1 for random)
//' @return List with correlation and p-value
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List qap_degree_cpp(
    const NumericMatrix& mat1,
    const NumericMatrix& mat2,
    int n_permutations,
    int swaps_factor = 10,
    int seed = -1
    ){

    const int n = mat1.nrow();
    
    // ensure square matrices of equal size
    if (n!=mat1.ncol() || n!=mat2.nrow() || n!=mat2.ncol())
        stop("Square matrices of equal size required");

    // verify mat1 is binary
    for (int i=0;i<n;++i) for (int j=0;j<n;++j)
        if (!NumericMatrix::is_na(mat1(i,j)) &&
            !(mat1(i,j)==0.0 || mat1(i,j)==1.0))
            stop("degree_preserving QAP expects binary mat1.");

    // build edge list and bitset representation
    std::vector<std::pair<int,int>> edges;
    edges.reserve(n*n/4);
    std::vector<char> bit(n*n, 0);  // flat array for fast access
    
    for (int i=0;i<n;++i) for (int j=0;j<n;++j)
        if (!NumericMatrix::is_na(mat1(i,j)) && mat1(i,j)==1.0){
            edges.emplace_back(i,j);
            bit[1LL*i*n+j]=1;
        }
    
    if (edges.empty()) stop("mat1 has no ones.");

    // determine number of swaps per permutation
    const int SWAPS = swaps_factor * edges.size();

    // find valid comparison positions
    std::vector<int> ii, jj;
    std::vector<double> v2;
    
    for (int i=0;i<n;++i) for (int j=0;j<n;++j)
        if (!NumericMatrix::is_na(mat1(i,j)) && !NumericMatrix::is_na(mat2(i,j))){
            ii.push_back(i); 
            jj.push_back(j);
            v2.push_back(mat2(i,j));
        }

    // extract mat1 values for observed correlation
    std::vector<double> v1(ii.size(), 0.0);
    for (size_t k=0;k<ii.size();++k)
        v1[k] = bit[ 1LL*ii[k]*n + jj[k] ];
    
    const double obs_cor = fast_cor(v1,v2);

    // setup random number generator
    std::mt19937 rng( (seed>=0)? seed : std::random_device{}() );
    int ge=0;

    // permutation test loop
    for (int r=0;r<n_permutations;++r) {
        // perform many edge swaps to randomize while preserving degrees
        for(int s=0;s<SWAPS;++s)
            one_edge_swap(edges, bit, rng, n);

        // extract permuted values
        for (size_t k=0;k<ii.size();++k)
            v1[k] = bit[ 1LL*ii[k]*n + jj[k] ];
        
        // check if correlation is as extreme
        if (std::abs(fast_cor(v1,v2)) >= std::abs(obs_cor)) ++ge;
    }

    return List::create(_["correlation"]=obs_cor,
                        _["p_value"]=double(ge)/n_permutations);
}

// ------------------------------------------------------------------
//   8. freeman-lane mrqap
// ------------------------------------------------------------------

//' Freeman-Lane MRQAP test
//'
//' @param mat1 Dependent matrix
//' @param mat2 Independent matrix
//' @param n_permutations Number of permutations
//' @param seed Random seed (-1 for random)
//' @return List with correlation and p-value
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List qap_freeman_lane_cpp(
    const NumericMatrix& mat1,
    const NumericMatrix& mat2,
    int n_permutations,
    int seed = -1
    ){

    const int n = mat1.nrow();
    std::vector<double> y,x;
    NumericMatrix Yres(n,n);  // residual matrix

    // extract valid pairs
    for (int i=0;i<n;++i) for (int j=0;j<n;++j)
        if (!NumericMatrix::is_na(mat1(i,j)) && !NumericMatrix::is_na(mat2(i,j))){
            y.push_back(mat1(i,j)); 
            x.push_back(mat2(i,j));
        }
    
    // calculate residuals of y regressed on x
    ols_residuals(y,x);

    // fill residual matrix
    size_t k=0;
    // iterate through matrix and fill with residuals or na
    for (int i=0;i<n;++i) for (int j=0;j<n;++j)
        Yres(i,j) = (!NumericMatrix::is_na(mat1(i,j)) && !NumericMatrix::is_na(mat2(i,j)))
                        ? y[k++] : NA_REAL;

    // perform qap on residuals vs original x
    return qap_correlation_cpp(Yres, mat2, n_permutations, seed);
}

// ------------------------------------------------------------------
//   9. double semi-partial mrqap
// ------------------------------------------------------------------

//' Double Semi-Partial MRQAP test
//'
//' @param mat1 First matrix
//' @param mat2 Second matrix
//' @param n_permutations Number of permutations
//' @param seed Random seed (-1 for random)
//' @return List with correlation and p-value
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List qap_dsp_cpp(
    const NumericMatrix& mat1,
    const NumericMatrix& mat2,
    int n_permutations,
    int seed = -1
    ){

    const int n = mat1.nrow();
    std::vector<double> y,x;

    // extract valid pairs
    for (int i=0;i<n;++i) for (int j=0;j<n;++j)
        if (!NumericMatrix::is_na(mat1(i,j)) && !NumericMatrix::is_na(mat2(i,j))){
            y.push_back(mat1(i,j)); 
            x.push_back(mat2(i,j));
        }
    
    // double semi-partialing process
    ols_residuals(y,x);       // remove x effect from y
    ols_residuals(x,y);       // remove y (now residual) effect from x

    // create residual matrices
    NumericMatrix Yres(n,n), Xres(n,n);
    size_t k=0;
    
    // fill both residual matrices
    for (int i=0;i<n;++i) for (int j=0;j<n;++j)
        if (!NumericMatrix::is_na(mat1(i,j)) && !NumericMatrix::is_na(mat2(i,j))){
            Yres(i,j)=y[k]; 
            Xres(i,j)=x[k]; 
            ++k;
        } else { 
            Yres(i,j)=NA_REAL; 
            Xres(i,j)=NA_REAL; 
        }

    // perform qap on both residual matrices
    return qap_correlation_cpp(Yres, Xres, n_permutations, seed);
}