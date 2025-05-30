#' Is this object a netify object?
#' 
#' @aliases is.netify
#' @param x An R object
#' @return Logical constant, \code{TRUE} if argument \code{object} is a netify
#' object
#' @author Colin Henry
#' 
#' @keywords netify
#' 
#' @export is_netify

is_netify <- function(x){
  "netify" %in% class(x)
}

#' netify_check
#'
#' Checks to make sure that object is of class netify
#' and stops process if not
#' @param netlet user inputted object to check
#' @return NULL object but stops the process if there 
#' is an error detected
#' @author Ha Eun Choi, Colin Henry, Shahryar Minhas
#' 
#' @export netify_check

netify_check <- function(netlet){

    # check if `dyad_data` is df
    if(!is_netify(netlet)) {
        cli::cli_alert_danger("Error: check data type. Inputted object is not a `netify` object.")
        stop() }

    # 
    return(invisible(NULL)) }

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
    actor_time_uniform = TRUE,
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
    stop()
  }

  # check for multilayer networks
  if ((data_class == "array" && length(dim(data)) > 3) ||
    (data_class == "list" && any(sapply(data, function(x) !is.matrix(x) || length(dim(x)) > 2)))) {
    cli::cli_alert_danger("Error: `new_netify` doesn't support multilayer networks currently. Please create separate netlets with `new_netify` and then use the `layer_netlet` function to combine into a multilayer netify object.")
    stop()
  }

  # determine netify_type based on data class
  netify_type <- switch(
    data_class,
    "matrix" = "cross_sec",
    "array"  = "longit_array",
    "list"   = "longit_list" )

  # helper: assign row/col names if missing
  # if mode=unipartite => a1,a2...
  # if mode=bipartite => r1,r2... / c1,c2...
  assign_dimnames <- function(mat, mode) {

    # get dims
    nr <- nrow(mat)
    nc <- ncol(mat)

    # if row/col names are missing, assign them
    if (is.null(rownames(mat))) {
      if (mode == "unipartite") {
        rownames(mat) <- paste0("a", seq_len(nr))
      } else {
        rownames(mat) <- paste0("r", seq_len(nr)) } }
    if (is.null(colnames(mat))) {
      if (mode == "unipartite") {
        colnames(mat) <- paste0("a", seq_len(nc))
      } else {
        colnames(mat) <- paste0("c", seq_len(nc)) } }

    #
    return(mat) }

  # helper: check symmetric ignoring diagonal if NA
  check_symmetric <- function(mat) {
    diag_na <- is.na(diag(mat))
    if (any(diag_na)){ diag(mat)[diag_na] <- 0 }
    return( identical(mat, t(mat)) ) }

  # helper: is strictly 0/1
  check_binary <- function(mat) {
    vals <- c(mat)
    vals <- vals[!is.na(vals)]
    return( all(vals %in% c(0,1)) ) }

  # helper: diag all NA => diag_to_NA=TRUE
  guess_diag_to_NA <- function(mat) {
    return( all(is.na(diag(mat))) ) }

  # helper: if there's any NA off diag => missing_to_zero=TRUE
  # return TRUE if NA found in off-diagonal elements
  # return FALSE if no NA found in off-diagonal elements
  guess_missing_to_zero <- function(mat) {
    d <- mat
    diag(d) <- 0
    return( !any(is.na(d)) ) }

  # guess mode => if nrow != ncol or row/col sets differ => bipartite else unipartite
  guess_mode <- function(mat) {

    # 
    nr <- nrow(mat)
    nc <- ncol(mat)
    if (nr != nc) return("bipartite")

    # 
    rown <- rownames(mat)
    coln <- colnames(mat)
    if (!is.null(rown) && !is.null(coln)) {
      if (length(intersect(rown, coln)) == length(rown)) {
        return("unipartite")
      } else {
        return("bipartite")
      } }
    return( "unipartite" ) }

  # if user hasn't specified actor_time_uniform, determine
  # based on input, if cross_sec/array then automaticallt TRUE
  # if longit_list, then TRUE if all slices have same actors
  if (is.null(final_params$actor_time_uniform)) {
    if (netify_type %in% c("cross_sec","longit_array")) {
      final_params$actor_time_uniform <- FALSE
    } else if (netify_type == "longit_list") {
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
      final_params$actor_time_uniform <- same_actors
    }
  }

  # based on what hasnt been specified by user, 
  # determine what we netify attributes we need to figure out
  detect_symmetric <- is.null(final_params$symmetric)
  detect_mode <- is.null(final_params$mode)
  detect_diag_to_NA <- is.null(final_params$diag_to_NA)
  detect_missing_to_zero <- is.null(final_params$missing_to_zero)
  detect_weight <- is.null(final_params$weight)

  # handle cross_sec
  if (netify_type == "cross_sec") {
    
    #
    mat <- data
    if (detect_symmetric){ final_params$symmetric <- check_symmetric(mat) }
    if (detect_mode){ final_params$mode <- guess_mode(mat) }
    if (detect_diag_to_NA){ final_params$diag_to_NA <- guess_diag_to_NA(mat) }
    if (detect_missing_to_zero){
      final_params$missing_to_zero <- guess_missing_to_zero(mat) }
    if (detect_weight) {
      if (check_binary(mat)) {
        final_params$weight        <- NULL
        final_params$detail_weight <- "Binary ties"
        final_params$weight_binary <- TRUE
      } else {
        final_params$weight        <- "edge_value"
        final_params$detail_weight <- paste0("Edges weighted by ", final_params$weight)
        final_params$weight_binary <- FALSE
      }
    }
    mat <- assign_dimnames(mat, final_params$mode)
    data <- mat

  # handle longit_array
  } else if (netify_type == "longit_array") {

    #
    dims <- dim(data)

    #
    # if dimnames missing => fix row/col names, time dimension names
    # row col
    if (is.null(dimnames(data)[[1]]) || is.null(dimnames(data)[[2]])) {
      # guess the mode from the first slice
      first_slice <- data[,,1, drop=TRUE]  
      guess_md <- if (!detect_mode) final_params$mode else guess_mode(first_slice)
      
      # Initialize dimnames for the array
      current_dimnames <- dimnames(data)
      if (is.null(current_dimnames)) {
        current_dimnames <- vector("list", length(dim(data))) }
      
      # Set row names
      if (is.null(current_dimnames[[1]])) {
        if (guess_md == "unipartite") {
          current_dimnames[[1]] <- paste0("a", seq_len(dims[1]))
        } else {
          current_dimnames[[1]] <- paste0("r", seq_len(dims[1]))
        } }
      
      # Set column names
      if (is.null(current_dimnames[[2]])) {
        if (guess_md == "unipartite") {
          current_dimnames[[2]] <- paste0("a", seq_len(dims[2]))
        } else {
          current_dimnames[[2]] <- paste0("c", seq_len(dims[2]))
        } }
      
      # Apply the dimnames to the array
      dimnames(data) <- current_dimnames
    }
    
    # third dim
    if (is.null(dimnames(data)[[3]])) {
      current_dimnames <- dimnames(data)
      if (is.null(current_dimnames)) {
        current_dimnames <- vector("list", length(dim(data)))
      }
      current_dimnames[[3]] <- paste0("t", seq_len(dims[3]))
      dimnames(data) <- current_dimnames
    }

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
          seq_len(dims[3]), function(ii) guess_missing_to_zero(data[,,ii, drop=TRUE]))) }
    if (detect_weight) {
      if (!any_nonbinary) {
        final_params$weight        <- NULL
        final_params$detail_weight <- "Binary ties"
        final_params$weight_binary <- TRUE
      } else {
        final_params$weight        <- "edge_value"
        final_params$detail_weight <- paste0("Edges weighted by ", final_params$weight)
        final_params$weight_binary <- FALSE
      }
    }

  # handle longit_list
  } else if (netify_type == "longit_list") {
    # if list has no names => t1,t2...
    if (is.null(names(data))) {
      names(data) <- paste0("t", seq_along(data)) }

    # fix row/colnames in each slice if missing
    # can't finalize mode until we see the first slice or do detection
    # so let's do minimal approach:
    # if we can't detect mode yet, do it from first slice
    guess_md <- final_params$mode
    if(
      detect_mode && length(data) > 0 && is.matrix(data[[1]])
      ){ guess_md <- guess_mode(data[[1]]) }
    
    # figure out if net is symmetric/weighted
    any_not_sym    <- FALSE
    any_nonbinary  <- FALSE
    for (i in seq_along(data)) {
      mat <- data[[i]]
      if (!is.matrix(mat)) next
      if (is.null(guess_md)) { guess_md <- guess_mode(mat) }
      mat <- assign_dimnames(mat, guess_md)
      data[[i]] <- mat

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
      final_params$missing_to_zero <- all(sapply(data, guess_missing_to_zero)) }
    if (detect_weight) {
      if (!any_nonbinary) {
        final_params$weight        <- NULL
        final_params$detail_weight <- "Binary ties"
        final_params$weight_binary <- TRUE
      } else {
        final_params$weight        <- "edge_value"
        final_params$detail_weight <- paste0("Edges weighted by ", final_params$weight)
        final_params$weight_binary <- FALSE
      }
    }
    # if we never had a mode, default unipartite
    if (is.null(final_params$mode)) {
      final_params$mode <- "unipartite"
    }
  }

  # guess a layer label if user hasn't supplied one
  # not currently supporting multilayer network
  if (is.null(final_params$layers)) {
    if (is.null(final_params$weight)) {
      final_params$layers <- "weight1"
    } else {
      final_params$layers <- as.character(final_params$weight)
    }
  }

  # if detail_weight wasn't set, do it now
  if (is.null(final_params$detail_weight)) {
    if (!is.null(final_params$weight)) {
      final_params$detail_weight <- paste0("Edges weighted by ", final_params$weight)
    } else {
      final_params$detail_weight <- "Binary ties"
    }
  }

  # if net_stats is empty, give minimal placeholders
  if (length(final_params$net_stats) == 0) {
    if (netify_type == "cross_sec") {
      if (final_params$mode == "bipartite") {
        final_params$net_stats$num_actors <- nrow(data) + ncol(data)
      } else {
        final_params$net_stats$num_actors <- nrow(data)
      }
    } else if (netify_type == "longit_array") {
      if (final_params$mode == "bipartite") {
        final_params$net_stats$num_actors <- nrow(data) + ncol(data)
      } else {
        final_params$net_stats$num_actors <- nrow(data)
      }
      final_params$net_stats$num_slices <- if (length(dim(data)) >= 3) {
        dim(data)[3] } else { 1 }
    } else if (netify_type == "longit_list") {
      final_params$net_stats$num_actors <- NA
    }
  }

  # if this is a list => each element is cross_sec
  if (netify_type == "longit_list") {
    for (i in seq_along(data)) {
      mat_slice <- data[[i]]
      if (!is.matrix(mat_slice)) {
        stop("all elements of a 'longit_list' must be matrices.")
      }
      attributes(mat_slice) <- c(attributes(mat_slice), final_params)
      attr(mat_slice, "netify_type")        <- "cross_sec"
      attr(mat_slice, "actor_time_uniform") <- NULL
      class(mat_slice) <- "netify"
      data[[i]] <- mat_slice
    }
  }

  # build the final object
  out <- structure(
    data,
    class = "netify",
    netify_type = netify_type )

  # attach all final_params
  for (nm in names(final_params)) {
    if (nm == "netify_type") next
    attr(out, nm) <- final_params[[nm]]
  }

  return(out)
}

