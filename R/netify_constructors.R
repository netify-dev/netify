########################################
#' Is this object a netify object?
#' 
#' @aliases is.netify
#' @param x An R object
#' @return Logical constant, \code{TRUE} if argument \code{object} is a netify
#' object
#' @author Colin Henry
#' @keywords netify
#' @export is_netify

is_netify <- function(x) {
  "netify" %in% class(x)
}
########################################

########################################
#' netify_check
#'
#' Checks to make sure that object is of class netify
#' and stops process if not
#' @param netlet user inputted object to check
#' @return NULL object but stops the process if there 
#' is an error detected
#' @author Ha Eun Choi, Colin Henry, Shahryar Minhas
#' @export netify_check

netify_check <- function(netlet) {
  # check if `dyad_data` is df
  if (!is_netify(netlet)) {
    cli::cli_alert_danger("Error: check data type. Inputted object is not a `netify` object.")
    stop()
  }
  
  return(invisible(NULL))
}
########################################

########################################
#' Constructs a generic netify Object
#'
#' `new_netify` is a low-level constructor for creating new netify objects.
#' Mostly for internal use, but can be used to create netify objects from 
#' matrices, arrays, or lists of matrices.
#'
#' @param data A data object (matrix, array, or list of matrices).
#' @param ... Additional parameters to be stored as attributes on the netify object.
#'
#' @return A netify object with the appropriate attributes for the given data structure.
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @export new_netify

new_netify <- function(data, ...) {

  # merge user arguments with defaults
  default_params <- list(
    netify_type = NULL,
    actor_time_uniform = NULL,
    actor_pds = NULL,
    weight = NULL,
    detail_weight = NULL,
    weight_binary = NULL,
    symmetric = NULL,
    mode = NULL,
    layers = NULL,
    diag_to_NA = NULL,
    missing_to_zero = NULL,
    sum_dyads = FALSE,
    nodal_data = NULL,
    dyad_data = NULL,
    graph_data = NULL )

  # merge user params with defaults
  user_params  <- list(...)
  final_params <- utils::modifyList(default_params, user_params)

  # figure out class so we can specify netify_type
  data_class <- class(data)[1]
  if (!data_class %in% c("matrix", "array", "list")) {
    cli::cli_alert_danger("Error: `data` must be a matrix, array, or list.")
    stop() }

  # check for multilayer networks
  if ((data_class == "array" && length(dim(data)) > 3) ||
    (data_class == "list" && any(
      sapply(data, function(x) !is.matrix(x) || length(dim(x)) > 2)))) {
    cli::cli_alert_danger(
       "Error: `new_netify` doesn't support multilayer networks currently. Please create separate netlets with `new_netify` and then use the `layer_netlet` function to combine into a multilayer netify object.")
    stop() }

  # determine netify_type based on data class
  netify_type <- switch(
    data_class,
    "matrix" = "cross_sec",
    "array"  = "longit_array",
    "list"   = "longit_list" )

  # based on what hasnt been specified by user, 
  # determine what we netify attributes we need to figure out
  detect_symmetric <- is.null(final_params$symmetric)
  detect_mode <- is.null(final_params$mode)
  detect_diag_to_NA <- is.null(final_params$diag_to_NA)
  detect_missing_to_zero <- is.null(final_params$missing_to_zero)
  detect_weight <- is.null(final_params$weight)
  detect_actor_time_uniform <- is.null(final_params$actor_time_uniform)
  detect_actor_pds <- is.null(final_params$actor_pds)  

  # handle cross_sec
  if (netify_type == "cross_sec") {
    
    #
    mat <- data
    if (detect_symmetric){ final_params$symmetric <- check_symmetric(mat) }
    if (detect_mode){ final_params$mode <- guess_mode(mat) }
    if (detect_diag_to_NA){ final_params$diag_to_NA <- guess_diag_to_NA(mat) }
    if (detect_missing_to_zero){
      final_params$missing_to_zero <- guess_missing_to_zero(
        mat, final_params$diag_to_NA ) }
    if (detect_weight) {
      if (check_binary(mat)) {
        final_params$weight <- NULL
        final_params$detail_weight <- "Binary ties"
        final_params$weight_binary <- TRUE
      } else {
        final_params$weight <- "edge_value"
        final_params$detail_weight <- paste0("Edges weighted by ", final_params$weight)
        final_params$weight_binary <- FALSE
      }
    }

    # assign row/col names based on mode
    mat <- assign_dimnames(mat, final_params$mode)
    data <- mat

    # 
    if(detect_actor_time_uniform){
      final_params$actor_time_uniform <- TRUE }
    
    # create actor "pds" frame
    if(detect_actor_pds){
      final_params$actor_pds <- data.frame(
        actor=unique(c(rownames(mat), colnames(mat))),
        min_time=1, max_time=1,
        stringsAsFactors=FALSE ) }

  # handle longit_array
  } else if (netify_type == "longit_array") {

    #
    dims <- dim(data)

    # get mode assignment
    if(detect_mode){
      final_params$mode <- guess_mode(data[,,1, drop = TRUE]) }

    # clean up row/col names if necessary
    data <- assign_dimnames(data, final_params$mode)
    
    # name time dimension
    if (is.null(dimnames(data)[[3]])) {
      cdim <- dimnames(data)
      cdim[[3]] <- seq_len(dims[3])
      dimnames(data) <- cdim }

    # figure out if net is symmetric/weighted
    # add break here too?
    # should probably do all period
    # iteration in one go ...
    any_not_sym <- FALSE
    any_nonbinary <- FALSE
    for (ii in seq_len(dims[3])) {
      mat <- data[,,ii, drop=TRUE]
      if (detect_symmetric && !any_not_sym) {
        if (!check_symmetric(mat)) any_not_sym <- TRUE }
      if (detect_weight && !any_nonbinary) {
        if (!check_binary(mat)) any_nonbinary <- TRUE } }
    if(detect_mode){ final_params$mode <- guess_mode(data[,,1, drop=TRUE]) }
    if(detect_symmetric){ final_params$symmetric <- !any_not_sym }
    if(detect_diag_to_NA){
      final_params$diag_to_NA <- all(
        sapply(
          seq_len(dims[3]), function(ii) guess_diag_to_NA(data[,,ii, drop=TRUE]))) }
    if(detect_missing_to_zero){
      final_params$missing_to_zero <- all(
        sapply(
          seq_len(dims[3]), function(ii){
            guess_missing_to_zero(data[,,ii, drop=TRUE], final_params$diag_to_NA)
            } ) ) }
    if (detect_weight) {
      if (!any_nonbinary) {
        final_params$weight <- NULL
        final_params$detail_weight <- "Binary ties"
        final_params$weight_binary <- TRUE
      } else {
        final_params$weight <- "edge_value"
        final_params$detail_weight <- paste0("Edges weighted by ", final_params$weight)
        final_params$weight_binary <- FALSE
      } }

    #
    if(detect_actor_time_uniform){
      final_params$actor_time_uniform <- TRUE }
    
    # create actor pds frame
    if(detect_actor_pds){
      pds <- dimnames(data)[[3]]
      final_params$actor_pds <- data.frame(
        actor=unique(c(rownames(mat), colnames(mat))),
        min_time=pds[1], max_time=pds[length(pds)],
        stringsAsFactors=FALSE ) }

  # handle longit_list
  } else if (netify_type == "longit_list") {

    # if list has no names => t1,t2...
    if (is.null(names(data))) {
      names(data) <- seq_along(data) }

    # fix row/colnames in each slice if missing
    # can't finalize mode until we see the first slice or do detection
    # so let's do minimal approach:
    # if we can't detect mode yet, do it from first slice
    guess_md <- final_params$mode
    if(
      detect_mode && length(data) > 0 && is.matrix(data[[1]])
      ){ guess_md <- guess_mode(data[[1]]) }
    
    # figure out if net is symmetric/weighted
    any_not_sym <- FALSE
    any_nonbinary <- FALSE
    for (ii in seq_along(data)) {
      mat <- data[[ii]]
      if (!is.matrix(mat)) next
      if (is.null(guess_md)) { guess_md <- guess_mode(mat) }
      mat <- assign_dimnames(mat, guess_md)
      data[[ii]] <- mat

      if (detect_symmetric && !any_not_sym) {
        if (!check_symmetric(mat)) any_not_sym <- TRUE }
      if (detect_weight && !any_nonbinary) {
        if (!check_binary(mat)) any_nonbinary <- TRUE }
    }

    #
    if (detect_mode && !is.null(guess_md)) { final_params$mode <- guess_md }
    if (detect_symmetric){ final_params$symmetric <- !any_not_sym }
    if (detect_diag_to_NA){ 
      final_params$diag_to_NA <- all(sapply(data, guess_diag_to_NA)) }
    if (detect_missing_to_zero){ 
      final_params$missing_to_zero <- all(
        unlist(lapply(data, function(mat){
          guess_missing_to_zero(mat, final_params$diag_to_NA) } 
          ) )) }
    if (detect_weight) {
      if (!any_nonbinary) {
        final_params$weight <- NULL
        final_params$detail_weight <- "Binary ties"
        final_params$weight_binary <- TRUE
      } else {
        final_params$weight <- "edge_value"
        final_params$detail_weight <- paste0("Edges weighted by ", final_params$weight)
        final_params$weight_binary <- FALSE
      } }

    # if we never had a mode, default unipartite
    if (is.null(final_params$mode)) {
      final_params$mode <- "unipartite"
    }

    # figure out if actors change
    if(detect_actor_time_uniform){
      same_actors <- TRUE
      if (length(data) > 0 && is.matrix(data[[1]])) {
        row0 <- rownames(data[[1]])
        col0 <- colnames(data[[1]])
        for (i in seq_along(data)) {
          if (!identical(row0, rownames(data[[i]])) ||
              !identical(col0, colnames(data[[i]]))) {
            same_actors <- FALSE
            break # stop as soon as we find a mismatch
          }
        }
      }
      final_params$actor_time_uniform <- same_actors }

    # if no actor_pds provided then calculate based on actor_time_uniform
    if(detect_actor_pds){
      # if uniform and no actor_pds provided, then assume
      # actors exist for duration of data
      if(final_params$actor_time_uniform){
        pds <- names(data)
        actors <- unique_vector(
          unlist(lapply(data, function(mat) c(rownames(mat), colnames(mat)))))
        actor_pds <- data.frame(
          actor=actors, min_time=pds[1], max_time=pds[length(pds)],
          stringsAsFactors=FALSE) }
      
      # if not uniform and no actor_pds provided, then calculate
      # entry and exit based on min and max in data
      if(!final_params$actor_time_uniform){
        actor_pds <- get_actor_time_info(
          reshape2::melt(data), 'Var1', 'Var2', 'L1') }
    }

  } # finished list processing

  # guess a layer label if user hasn't supplied one
  # not currently supporting multilayer network
  if (is.null(final_params$layers)) {
    if (is.null(final_params$weight)) {
      final_params$layers <- "weight1"
    } else {
      final_params$layers <- as.character(final_params$weight)
    } }

  # if detail_weight wasn't set, do it now
  if (is.null(final_params$detail_weight)) {
    if (!is.null(final_params$weight)) {
      final_params$detail_weight <- paste0("Edges weighted by ", final_params$weight)
    } else {
      final_params$detail_weight <- "Binary ties"
    } }

  # clean up cross-secs in list
  if (netify_type == "longit_list") {
    for (ii in seq_along(data)) {
      mat_slice <- data[[ii]]
      if (!is.matrix(mat_slice)) {
        cli::cli_alert_danger(
          "Error: all elements of the 'list' must be matrices.")
        stop() }
      attributes(mat_slice) <- c(attributes(mat_slice), final_params)
      attr(mat_slice, "netify_type") <- "cross_sec"
      attr(mat_slice, "actor_time_uniform") <- NULL
      attr(mat_slice, 'actor_pds') <- NULL
      attr(mat_slice, 'nodal_data') <- NULL
      attr(mat_slice, 'dyad_data') <- NULL            
      class(mat_slice) <- "netify"
      data[[ii]] <- mat_slice } }

  # throw error about nodal data if it 
  # does not match data object and
  # expected structure


  # throw errir about dyad data if it
  # does not match data object and
  # expected structure


  # build the final object
  out <- structure(
    data,
    class = "netify",
    netify_type = netify_type )

  # attach all final_params
  for (nm in names(final_params)) {
    if (nm == "netify_type") next
    attr(out, nm) <- final_params[[nm]] }

  #
  return(out)
}
########################################

########################################
#' Assign Default Row/Column Names
#'
#' Internal helper function that assigns default row and column names to a matrix,
#' depending on whether the network is unipartite or bipartite. If the matrix
#' already has row/col names, they are preserved.
#'
#' @param mat A matrix (potentially without row or column names).
#' @param mode Character string: "unipartite" or "bipartite".
#'
#' @return The same \code{mat} with updated row and column names as needed.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
assign_dimnames <- function(mat, mode) {

  # get dims
  nr <- nrow(mat) ; nc <- ncol(mat)
  
  # check what to do with rownames
  if (is.null(rownames(mat))) {
    if (mode == "unipartite") {
      rownames(mat) <- paste0("a", seq_len(nr))
    } else {
      rownames(mat) <- paste0("r", seq_len(nr))
    } }
  
  # check what to do with colnames
  if (is.null(colnames(mat))) {
    if (mode == "unipartite") {
      colnames(mat) <- paste0("a", seq_len(nc))
    } else {
      colnames(mat) <- paste0("c", seq_len(nc))
    } }
  
  # return the matrix with updated names
  return( mat )
}
########################################

########################################
#' Check if a Matrix is Symmetric (Ignoring NA Diagonals)
#'
#' Internal helper that checks whether \code{mat} is equal to its transpose,
#' after temporarily setting any \code{NA} diagonal entries to \code{0}.
#'
#' @param mat A matrix to be checked.
#'
#' @return Logical. \code{TRUE} if \code{mat} is symmetric, otherwise \code{FALSE}.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
check_symmetric <- function(mat) {

  # set diagonal to zero
  diag(mat) <- 0
  
  # symm if mat is ident to its transpose
  return( identical(mat, t(mat)) )
}
########################################

########################################
#' Check if Matrix is Strictly Binary
#'
#' Internal helper that tests if all non-\code{NA} entries of \code{mat}
#' are in \code{\{0, 1\}}.
#'
#' @param mat A matrix to be tested.
#'
#' @return Logical. \code{TRUE} if all non-NA entries are 0/1, otherwise \code{FALSE}.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
check_binary <- function(mat) {

  # pull out non-na vals only
  non_na_vals <- mat[!is.na(mat)]
  
  # return FALSE as soon as a non-0/1 val is found
  return(!any(non_na_vals != 0 & non_na_vals != 1))
}
########################################

########################################
#' Check if Diagonal is Entirely NA
#'
#' Internal helper that checks whether the diagonal of \code{mat} is fully
#' \code{NA}. Used to guess whether \code{diag_to_NA} should be \code{TRUE}.
#'
#' @param mat A matrix to examine.
#'
#' @return Logical. \code{TRUE} if \code{diag(mat)} is all \code{NA}, otherwise \code{FALSE}.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
guess_diag_to_NA <- function(mat) {

  # assume TRUE if all diagonal entries are NA
  return( all(is.na(diag(mat))) )
}
########################################

########################################
#' Check if Missing Values Appear Off-Diagonal
#'
#' Internal helper that determines if any off-diagonal entries of \code{mat}
#' are \code{NA}. Sets diagonal to zero temporarily, then checks for \code{NA}.
#'
#' @param mat A matrix to examine.
#' @param diag_NA Logical. If \code{TRUE}, treat diagonal entries as \code{0} for the
#'  purpose of this check. If \code{FALSE}, diagonal entries are not considered.
#'
#' @return Logical. \code{TRUE} if no off-diagonal \code{NA}s are found,
#'   otherwise \code{FALSE}.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
guess_missing_to_zero <- function(mat, diag_NA=TRUE) {

  # set diags to zero if diag_NA is TRUE
  if(diag_NA){ diag(mat) <- 0 }

  # if any entries are NA, return FALSE
  return( !any(is.na(mat)) )
}
########################################

########################################
#' Guess Whether Matrix is Unipartite or Bipartite
#'
#' Internal helper that infers if \code{mat} should be treated as unipartite or
#' bipartite based on its dimensions and row/column name overlap.
#'
#' @param mat A matrix whose dimensions/names we examine.
#'
#' @return Character. Either \code{"unipartite"} or \code{"bipartite"}.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd
guess_mode <- function(mat) {

  # get dims
  nr <- nrow(mat) ; nc <- ncol(mat)
  
  # if matrix is not square, assume bipartite
  if (nr != nc) { return("bipartite") }
  
  # if row/col names are present, check for overlap
  rown <- rownames(mat) ; coln <- colnames(mat)
  
  # if both row and col names are present, check for overlap
  if (!is.null(rown) && !is.null(coln)) {
    if (length(intersect(rown, coln)) == length(rown)) {
      return("unipartite")
    } else {
      return("bipartite") } }
  
  # if no row/col names, assume unipartite
  return( "unipartite" )
}
########################################