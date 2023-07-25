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
	bool symmetric
){

	// initialize matrix
	NumericMatrix m(n_rows,n_cols) ;

	// start off with NAs
	std::fill( m.begin(), m.end(), NumericVector::get_na() ) ;

	// add row and column names
	rownames(m) = actors_rows;
  	colnames(m) = actors_cols;

	// fill in based on data values
	for(int i = 0; i < value.size() ; i++){

		//
		int a1i = matRowIndices[i] - 1;
		int a2i = matColIndices[i] - 1;
		double vi = value[i];

		//
		m(a1i, a2i) = vi;

		//
		if(symmetric==TRUE){
			m(a2i,a1i) = vi;
		}

	}

	//
  return m;}
