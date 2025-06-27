#include <Rcpp.h>
#include <unordered_map>
#include <string>
using namespace Rcpp;

//' Determine number of duplicate dyad-time obs
//'
//' @param actor1 character vector for actor1
//' @param actor2 character vector for actor2
//' @param time numeric vector for time
//' @return an integer count of number of duplicate dyads
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
int count_duplicate_dyads(
  CharacterVector actor1, 
  CharacterVector actor2, 
  NumericVector time
  ){

    int n = actor1.size();
    
    // Use unordered_map for O(1) average lookup
    // For 100-200 actors, we might have up to 40k possible dyads
    // Reserve conservatively based on typical network density
    std::unordered_map<std::string, int> dyadCounts;
    dyadCounts.reserve(n / 2);  // Assume ~50% unique dyads as initial guess
    
    int repeats = 0;
    
    // Reuse string to avoid allocations
    std::string dyadKey;
    dyadKey.reserve(50);  // Most actor names + time won't exceed this
    
    for (int i = 0; i < n; ++i) {
        // Clear and build key efficiently
        dyadKey.clear();
        dyadKey.append(actor1[i]);
        dyadKey.push_back('_');
        dyadKey.append(actor2[i]);
        dyadKey.push_back('_');
        dyadKey.append(std::to_string(static_cast<int>(time[i])));
        
        // Increment and check in one operation
        if (++dyadCounts[dyadKey] == 2) {
            repeats++;
        }
    }
    
    return repeats;
}

//' Alternative implementation for small actor sets
//' Uses integer encoding for actors when possible
//'
//' @param actor1 character vector for actor1
//' @param actor2 character vector for actor2
//' @param time numeric vector for time
//' @return an integer count of number of duplicate dyads
//' @author Shahryar Minhas
//'
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
int count_duplicate_dyads_indexed(
  CharacterVector actor1, 
  CharacterVector actor2, 
  NumericVector time
  ){
    
    int n = actor1.size();
    
    // For small actor sets, create actor-to-index mapping
    std::unordered_map<std::string, int> actorIndex;
    int nextIndex = 0;
    
    // First pass: build actor index
    for (int i = 0; i < n; ++i) {
        std::string a1(actor1[i]);
        std::string a2(actor2[i]);
        
        if (actorIndex.find(a1) == actorIndex.end()) {
            actorIndex[a1] = nextIndex++;
        }
        if (actorIndex.find(a2) == actorIndex.end()) {
            actorIndex[a2] = nextIndex++;
        }
    }
    
    // If we have too many actors, fall back to string method
    if (nextIndex > 10000) {
        return count_duplicate_dyads(actor1, actor2, time);
    }
    
    // Use integer-based key for better performance
    // Key = actor1_idx * 100000 + actor2_idx * 100 + time_mod
    std::unordered_map<long long, int> dyadCounts;
    dyadCounts.reserve(n / 2);
    
    int repeats = 0;
    
    for (int i = 0; i < n; ++i) {
        int idx1 = actorIndex[std::string(actor1[i])];
        int idx2 = actorIndex[std::string(actor2[i])];
        int t = static_cast<int>(time[i]);
        
        // Create unique integer key
        // This works well for up to 10k actors and reasonable time values
        long long key = static_cast<long long>(idx1) * 100000000LL + 
                       static_cast<long long>(idx2) * 10000LL + 
                       (t % 10000);
        
        if (++dyadCounts[key] == 2) {
            repeats++;
        }
    }
    
    return repeats;
}
