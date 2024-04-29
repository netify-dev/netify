# miscellaneous functions for use in package

#' check if dependency is installed
#'
#' @noRd
check_dependency <- function(library_name) {
  flag <- requireNamespace(library_name, quietly = TRUE)
  if (!isTRUE(flag)) {
      msg <- sprintf("Please install the `%s` package.", library_name)
      return(msg)
  } else {
      return(TRUE)
  }
}
assert_dependency <- checkmate::makeAssertionFunction(check_dependency)

#' char
#'
#' Converts values into characters
#' @param x vector
#' @return character vector
#' @author Shahryar Minhas

char <- function(x){
	as.character(x)
}


#' num
#'
#' Converts values into character then numeric
#' @param x vector
#' @return numeric vector
#' @author Shahryar Minhas

num <- function(x){
	as.numeric(char(x))
}

#' unique_vector
#'
#' Get unique vector from 
#' multiple vector inputs
#' @param ... vector inputs
#' @return numeric vector
#' @author Shahryar Minhas

unique_vector <- function(...){
	u_vec <- unique(c(...))
	u_vec <- sort(u_vec)
	return(u_vec)
}

#' identical_recursive
#' 
#' Recursively check if two or more objects are identical
#' @param ... objects to check
#' @return logical indicating whether objects are identical
#' @author Shahryar Minhas

identical_recursive <- function(...){

    # org objects to compare
    to_compare <- c(...) 

    # if just two, then just compare
    if( length( to_compare) == 2){
        ident_logic <- identical( 
            to_compare[1] , to_compare[2] ) }
    
    # if more than two, then recursively call fn
    if( length( to_compare) > 2){
        ident_logic <- c(
            identical( to_compare[1], to_compare[2] ),
            identical_recursive( to_compare[-1] ) ) }

    # return TRUE if all are identical
    return( all(ident_logic) ) }

#' Break string into list of strings by some fixed character
#' and then extract the desired values around that fixed
#' character
#' 
#' @param string_to_split character: string to be split
#' @param break_by character: character to break string by
#' @param to_extract integer: index of the string to be extracted
#' @param fixed If `TRUE` match exactly, otherwise use regular expressions
#' @return a character vector of the extracted strings
#' 
#' @author Shahryar Minhas

split_string <- function(
	string_to_split, break_by, to_extract, fixed=TRUE){
    str_to_list <- strsplit(string_to_split, break_by, fixed=fixed)
    extract_relev <- lapply(str_to_list, function(x){x[to_extract]})
    return( unlist(extract_relev) ) }

#' array_to_list
#'
#' This function converts a three dimensional array
#' into a list of matrices
#' @param arr three dimensional array to list
#' @param preserveAttr logical indicating whether to preserve attributes
#' @return list object
#' @author Shahryar Minhas
#' @export

array_to_list <- function(arr, preserveAttr=TRUE){
	l <- lapply(1:dim(arr)[3], function(ii){ arr[,,ii] })
	names(l) = dimnames(arr)[[3]]
	if(preserveAttr){
		class(l) = 'netify'
		arrAttr = attributes(arr)
		for(ii in 3:length(arrAttr)){ attr(l, names(arrAttr)[ii]) = arrAttr[[ii]] }
	}
	return(l)
}

#' list_to_array
#' 
#' This function converts a list of matrices
#' into a three dimensional array
#' @param list_of_mats list object
#' @return three dimensional array
#' @author Shahryar Minhas
#' @export

list_to_array <- function(list_of_mats){

	# get dim info to create array
	row_actors = unique(unlist(lapply(list_of_mats, rownames)))
	col_actors = unique(unlist(lapply(list_of_mats, colnames)))
	time_points = names(list_of_mats)

	# create array
	arr = array(NA, 
		dim = c(length(row_actors), length(col_actors), length(time_points)),
		dimnames = list(row_actors, col_actors, time_points))

	# fill array
	for(ii in 1:length(list_of_mats)){
		list_of_mats_slice = list_of_mats[[ii]]
		rows_sl = rownames(list_of_mats_slice)
		cols_sl = colnames(list_of_mats_slice)
		arr[rows_sl, cols_sl, ii] = list_of_mats_slice
		}

	#
	return(arr)
}


#' gen_symm_id
#' 
#' This function creates a symmetric id 
#' for a given pair of actors
#' @param dyad_data dyadic data.frame object
#' @param actor1 name of actor1 column
#' @param actor2 name of actor2 column
#' @param time name of time column if relevant
#' @return a vector of symmetric ids that can be added back to the data.frame object
#' @author Shahryar Minhas
#' @export
#' 

gen_symm_id <- function(
	dyad_data, actor1, actor2, time=NULL
){
	symm_id <- apply(
		dyad_data, 1, 
		function(x){
			actors <- x[c(actor1, actor2)]
			actors <- sort(actors)
			out <- paste(actors, collapse='_')
			if(!is.null(time)){
				out <- paste(out, x[time], sep='_')
			}
			return(out) } )
	return(symm_id)
}
