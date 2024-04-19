#include <Rcpp.h>
using namespace Rcpp;

//' Determine number of repeating dyad-time obs
//'
//' @param actor1 character vector for actor1
//' @param actor2 character vector for actor2
//' @param time numeric vector for time
//' @return an integer count of number of repeating dyads
//' @author Shahryar Minhas
//'
//' @export repeat_dyads_check_cpp
// [[Rcpp::export]]
int repeat_dyads_check_cpp(
  CharacterVector actor1, 
  CharacterVector actor2, 
  NumericVector time

  ){

    // create a map to count occurrences of each unique dyad combination, map key is a string concatenating actor1, actor2, and time, ensuring unique identifiers for each dyad
    std::map<std::string, int> dyadCounts;

    // Assuming actor1 and actor2 vectors are the same length,
    // then iterate over all elements to create dyad-time ids
    int n = actor1.size(); 
    for (int i = 0; i < n; ++i) {

        // create unique identifier
        std::string dyadKey = std::string(actor1[i]) + "_" + std::string(actor2[i]) + "_" + std::to_string(time[i]);
        
        // increment count for this dyadKey in the map, adding the dyadKey to the map if it doesn't already exist.
        dyadCounts[dyadKey]++;
    }

    // counter for the number of dyads that repeat
    int repeats = 0;
    // iterate over pairs in the dyadCounts map
    for (const auto& pair : dyadCounts) {
        // if the count for this dyadKey is greater than 1, it means the dyad is repeating      
        if (pair.second > 1) {
            repeats++;
        }
    }

    // return total number of repeating
    return repeats;
}




