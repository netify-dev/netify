#' raw_matrix
#'
#' This function takes in a netify object of type
#' cross_sec or longit_array and returns an R matrix/array
#' @param netlet a netify object of type
#' cross_sec' or 'longit_array'
#' @return an object of class matrix, array with netify
#' attributes stripped
#' @author Shahryar Minhas
#' @export

raw_matrix <- function(netlet){
    stopifnot("Not a netify object!" = is_netify(netlet))
    out <- netlet
    if(length(attributes(out)$dim) > 2){
        class(out) <- c('array') } 
    else {
        class(out) <- c('matrix','array')
    }
    attributes(out)[3:length(attributes(out))] <- NULL
    return(out)
}

#' raw_list
#'
#' This function takes in a netify object of type
#' cross_sec and returns a regular matrix
#' @param netlet a netify object of type 'longit_list'
#' @return an object of class list with netify
#' attributes stripped
#' @author Shahryar Minhas
#' @export

raw_list <- function(netlet){
    stopifnot("Not a netify object!" = is_netify(netlet))
    netlet <- lapply(netlet, raw_matrix)
    class(netlet) <- c('list')
    # attributes(netlet)[2:length(attributes(netlet))] <- NULL
    return(netlet)
}

#' get_raw
#'
#' This function serves as a wrapper for
#' raw_matrix and raw_list, which serve to
#' strip netify related attributes from lists
#' and matrix obejcts
#' @param netlet a netify object
#' @return a list or matrix object with netify attributes stripped
#' @author Shahryar Minhas
#' @export

get_raw <- function(netlet){

    # check
    netify_check(netlet)

	# get type
	obj_type <- attributes(netlet)$netify_type

	# strip netify attributes and return
    if( obj_type %in% c('cross_sec', 'longit_array') ){
        return( raw_matrix( netlet ) ) }
    if( obj_type %in% c('longit_list') ){
        return( raw_list( netlet ) ) }
}

#' unnetify
#'
#' This function serves as a wrapper for
#' get_raw and other conversion functions
#' that convert a netify object back into 
#' constituent parts
#' @param netlet a netify object
#' @return a list, matrix, or other object with netify attributes stripped
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#' @export

unnetify <- function(netlet){ get_raw(netlet) }
