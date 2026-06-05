#include <Rcpp.h>
#include <unordered_map>
#include <string>
#include <cmath>
#include <limits>
#include <sstream>
#include <iomanip>
using namespace Rcpp;

inline bool invalid_time_value(double value) {
    return NumericVector::is_na(value) ||
        !std::isfinite(value);
}

inline bool invalid_index_time_value(double value) {
    return invalid_time_value(value) ||
        value < std::numeric_limits<int>::min() ||
        value > std::numeric_limits<int>::max() ||
        std::floor(value) != value;
}

inline std::string encode_field(const std::string& value) {
    return std::to_string(value.size()) + ":" + value;
}

inline std::string encode_time(double value) {
    std::ostringstream out;
    out << std::setprecision(17) << value;
    return out.str();
}

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
    if (actor2.size() != n || time.size() != n) {
        stop("actor and time vectors must have equal length");
    }
    if (n == 0) return 0;
    
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
        if (invalid_time_value(time[i])) {
            stop("time values must be non-missing and finite");
        }

        // clear and build key efficiently. length-prefix fields so actor
        // names containing separators cannot collide.
        dyadKey.clear();
        dyadKey.append(encode_field(std::string(actor1[i])));
        dyadKey.push_back('|');
        dyadKey.append(encode_field(std::string(actor2[i])));
        dyadKey.push_back('|');
        dyadKey.append(encode_field(encode_time(time[i])));
        
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
    if (actor2.size() != n || time.size() != n) {
        stop("actor and time vectors must have equal length");
    }
    if (n == 0) return 0;
    
    // For small actor sets, create actor-to-index mapping
    std::unordered_map<std::string, int> actorIndex;
    int nextIndex = 0;
    
    // First pass: build actor index
    for (int i = 0; i < n; ++i) {
        if (invalid_time_value(time[i])) {
            stop("time values must be non-missing and finite");
        }
        if (invalid_index_time_value(time[i])) {
            return count_duplicate_dyads(actor1, actor2, time);
        }

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
    if (nextIndex > 100000) {
        return count_duplicate_dyads(actor1, actor2, time);
    }

    // Use integer-based key for better performance
    // Key = idx1 * (maxActors * maxTime) + idx2 * maxTime + t
    // where maxActors = nextIndex and maxTime is derived from data
    std::unordered_map<long long, int> dyadCounts;
    dyadCounts.reserve(n / 2);

    // Find time range for safe key encoding
    bool have_time = false;
    int t_min = 0;
    int t_max = 0;
    for (int i = 0; i < n; ++i) {
        if (invalid_index_time_value(time[i])) continue;

        int t = static_cast<int>(time[i]);
        if (!have_time) {
            t_min = t;
            t_max = t;
            have_time = true;
        }
        if (t < t_min) t_min = t;
        if (t > t_max) t_max = t;
    }
    if (!have_time) return 0;
    long long t_range = static_cast<long long>(t_max) - static_cast<long long>(t_min) + 1;
    long long n_actors = static_cast<long long>(nextIndex);

    // If key space would overflow, fall back to string method
    // long long max is ~9.2e18, so check n_actors * n_actors * t_range
    if (n_actors * n_actors > 9000000000000000LL / t_range) {
        return count_duplicate_dyads(actor1, actor2, time);
    }

    int repeats = 0;

    for (int i = 0; i < n; ++i) {
        if (invalid_index_time_value(time[i])) continue;

        int idx1 = actorIndex[std::string(actor1[i])];
        int idx2 = actorIndex[std::string(actor2[i])];
        long long t_offset = static_cast<long long>(static_cast<int>(time[i]) - t_min);

        // Create unique integer key with no collisions
        long long key = (static_cast<long long>(idx1) * n_actors + idx2) * t_range + t_offset;

        if (++dyadCounts[key] == 2) {
            repeats++;
        }
    }
    
    return repeats;
}
