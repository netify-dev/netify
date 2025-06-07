#' Add dyad data to a netify object
#'
#' This function takes in a data.frame and outputs a netify object.
#'
#' @param netlet a netify object
#' @param dyad_data a data.frame object
#' @param actor1 character: actor 1 in the data
#' @param actor2 character: actor 2 in the data
#' @param time a character object indicating which variable in dyad_data tracks time
#' @param dyad_vars a vector of which variables from dyad_data should be merged
#' @param dyad_vars_symmetric logical vector: whether ties are symmetric, default is TRUE
#' @param replace_existing a logical indicating whether to replace existing nodal data
#'
#' @return a netify object
#'
#' @examples 
#'  
#' data(icews)
#' 
#' # cross-sectional case
#' icews_10 <- icews[icews$year==2010,]
#' 
#' verbCoop_net <- netify(
#'   dyad_data=icews_10,
#'   actor1 = 'i', actor2 = 'j', 
#'   symmetric=FALSE, weight='verbCoop' )
#' 
#' verbCoop_net <- add_dyad(
#'   netlet=verbCoop_net, 
#'   dyad_data=icews_10, 
#'   actor1='i', actor2='j', 
#'   dyad_vars=c('matlCoop', 'verbConf', 'matlConf'),
#'   dyad_vars_symmetric = rep(FALSE, 3) )
#' 
#' # dyadic data is now stored as a list of time periods,
#' # each containing a list of variable matrices:
#' dyad_data_structure <- attr(verbCoop_net, 'dyad_data')
#' 
#' # Access specific variable matrix:
#' matlCoop_matrix <- dyad_data_structure[["1"]][["matlCoop"]]
#' 
#' # Access all variables for a time period:
#' all_vars_t1 <- dyad_data_structure[["1"]]
#' 
#' # longitudinal case
#' verbCoop_longit_net <- netify(
#'     dyad_data=icews, 
#'     actor1='i', actor2='j', time='year',
#'     symmetric=FALSE,
#'     weight='verbCoop' )
#' 
#' verbCoop_longit_net <- add_dyad(
#'     netlet=verbCoop_longit_net,
#'     dyad_data=icews, 
#'     actor1='i', actor2='j', time='year',
#'     dyad_vars = c('matlCoop', 'verbConf', 'matlConf'),
#'     dyad_vars_symmetric=rep(FALSE, 3) )
#' 
#' # Access data for specific year and variable:
#' matlCoop_2002 <- attr(verbCoop_longit_net, 'dyad_data')[["2002"]][["matlCoop"]]
#' 
#' # Access all variables for year 2002:
#' all_vars_2002 <- attr(verbCoop_longit_net, 'dyad_data')[["2002"]]
#' 
#' @author Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @export add_dyad
#' 

add_dyad <- function(
  netlet, dyad_data, 
  actor1=NULL, actor2=NULL, time=NULL,
  dyad_vars=NULL, dyad_vars_symmetric=NULL,
  replace_existing=FALSE
  ){

  # user input checks
  netify_check(netlet)
  dyad_data <- df_check(dyad_data)
  actor_check(actor1, actor2, dyad_data)
  add_var_time_check(netlet, time)

  # some attribute info
  netlet_type <- attr(netlet, 'netify_type')
  netlet_mode <- attr(netlet, 'mode')
  actor_unif <- attr(netlet, 'actor_time_uniform')

  # determine variables to merge if specific ones are not provided
  if(is.null(dyad_vars)){ 
    dyad_vars = setdiff(names(dyad_data), c(actor1, actor2, time)) }

  # if user has not filled in dyadsVarsSymmetric logical then we need to choose for them
  # yell at user about supplying a choice for dyadsVarsSymmetric
  if( is.null( dyad_vars_symmetric ) & netlet_mode != 'bipartite' ){ 
    dyad_vars_symmetric = rep(attr(netlet, 'symmetric'), length(dyad_vars))
    cli::cli_alert_warning(
      "Warning: When adding dyadic variables it is best to specify whether the variable being added is symmetric or not") }
  if( netlet_mode == 'bipartite'  ){
    dyad_vars_symmetric = rep(FALSE, length(dyad_vars)) }

  # count up number of dyad_vars
  ndVars = length(dyad_vars)

  # check to see if there is already a dyad_data attribute in the netify object
  dyad_data_0 <- attributes(netlet)$dyad_data
  dyad_data_attrib_exists <- !is.null( dyad_data_0 )

  # if dyad_data attribute already exists and replace_existing is TRUE
  # then remove any vars that are already in the netlet dyad data attrib
  if( dyad_data_attrib_exists & replace_existing ){
    # Remove variables across all time periods
    for(time_period in names(dyad_data_0)) {
      for(var_name in dyad_vars) {
        dyad_data_0[[time_period]][[var_name]] <- NULL
      }
    }
  }

  # get netlet measurements
  msrmnts <- netify_measurements(netlet)

  # if cross_sec put in a 1 for time
  if(is.null(msrmnts$time)) { msrmnts$time <- 1 }

  # Pre-split dyad_data by time periods for efficiency
  if(!is.null(time) & attributes(netlet)$netify_type != 'cross_sec') {
    time_indices <- split(seq_len(nrow(dyad_data)), dyad_data[,time])
    dyad_actor1 <- dyad_data[,actor1]
    dyad_actor2 <- dyad_data[,actor2]
  }

  # construct new storage structure: list of time periods -> list of variable matrices
  dyad_structure <- lapply( msrmnts$time, function(timePd){

    # actors that should be in the matrices at this time point
    if( netlet_type == 'longit_list'){
      actors_rows <- msrmnts$row_actors[[timePd]]
      actors_cols <- msrmnts$col_actors[[timePd]]
      n_actors_rows <- msrmnts$n_row_actors[[timePd]]
      n_actors_cols <- msrmnts$n_col_actors[[timePd]] 
    }
    if( netlet_type %in% c('longit_array', 'cross_sec') ){
      actors_rows <- msrmnts$row_actors
      actors_cols <- msrmnts$col_actors
      n_actors_rows <- msrmnts$n_row_actors
      n_actors_cols <- msrmnts$n_col_actors 
    }

    # slice up dyad_data object for this time period
    if( !is.null(time) & attributes(netlet)$netify_type != 'cross_sec' ){
      slice_indices <- time_indices[[as.character(timePd)]]
      if(is.null(slice_indices)) slice_indices <- integer(0)
      
      if(length(slice_indices) > 0) {
        slice_actor1 <- dyad_actor1[slice_indices]
        slice_actor2 <- dyad_actor2[slice_indices]
        slice_data <- dyad_data[slice_indices, dyad_vars, drop=FALSE]
      } else {
        slice_actor1 <- character(0)
        slice_actor2 <- character(0)
        slice_data <- dyad_data[integer(0), dyad_vars, drop=FALSE]
      }
    } else {
      slice_actor1 <- dyad_data[,actor1]
      slice_actor2 <- dyad_data[,actor2]
      slice_data <- dyad_data[, dyad_vars, drop=FALSE]
    }

    # only keep rows that are in the netlet object
    valid_rows <- slice_actor1 %in% actors_rows & slice_actor2 %in% actors_cols
    slice_actor1 <- slice_actor1[valid_rows]
    slice_actor2 <- slice_actor2[valid_rows]
    slice_data <- slice_data[valid_rows, , drop=FALSE]

    # Pre-compute matrix indices for efficiency
    if(length(slice_actor1) > 0) {
      matRowIndices <- match(slice_actor1, actors_rows)
      matColIndices <- match(slice_actor2, actors_cols)
    } else {
      matRowIndices <- integer(0)
      matColIndices <- integer(0)
    }

    # create list of matrices for each dyadic variable
    var_matrices <- vector("list", ndVars)
    names(var_matrices) <- dyad_vars

    # iterate through variables and create individual matrices
    for(ii in 1:ndVars){
      var_name <- dyad_vars[ii]
      
      # determine appropriate storage mode for efficiency
      var_values <- if(length(slice_actor1) > 0) slice_data[, var_name] else numeric(0)
      storage_mode <- determine_storage_mode(var_values)
      
      # Use appropriate C++ function based on storage mode
      if(storage_mode == "double") {
        var_matrices[[var_name]] <- get_matrix(
          n_rows = n_actors_rows,
          n_cols = n_actors_cols,
          actors_rows = actors_rows,
          actors_cols = actors_cols,
          matRowIndices = matRowIndices,
          matColIndices = matColIndices,
          value = var_values,
          symmetric = dyad_vars_symmetric[ii],
          missing_to_zero = TRUE,
          diag_to_NA = FALSE
        )
      } else if(storage_mode == "integer") {
        var_matrices[[var_name]] <- get_matrix_integer(
          n_rows = n_actors_rows,
          n_cols = n_actors_cols,
          actors_rows = actors_rows,
          actors_cols = actors_cols,
          matRowIndices = matRowIndices,
          matColIndices = matColIndices,
          value = as.integer(var_values),
          symmetric = dyad_vars_symmetric[ii],
          missing_to_zero = TRUE,
          diag_to_NA = FALSE
        )
      } else if(storage_mode == "logical") {
        var_matrices[[var_name]] <- get_matrix_logical(
          n_rows = n_actors_rows,
          n_cols = n_actors_cols,
          actors_rows = actors_rows,
          actors_cols = actors_cols,
          matRowIndices = matRowIndices,
          matColIndices = matColIndices,
          value = as.logical(var_values),
          symmetric = dyad_vars_symmetric[ii],
          missing_to_zero = TRUE,
          diag_to_NA = FALSE
        )
      } else if(storage_mode == "character") {
        var_matrices[[var_name]] <- get_matrix_character(
          n_rows = n_actors_rows,
          n_cols = n_actors_cols,
          actors_rows = actors_rows,
          actors_cols = actors_cols,
          matRowIndices = matRowIndices,
          matColIndices = matColIndices,
          value = as.character(var_values),
          symmetric = dyad_vars_symmetric[ii],
          missing_to_zero = TRUE,
          diag_to_NA = FALSE
        )
      } else {
        # Fallback to original numeric function for unknown types
        var_matrices[[var_name]] <- get_matrix(
          n_rows = n_actors_rows,
          n_cols = n_actors_cols,
          actors_rows = actors_rows,
          actors_cols = actors_cols,
          matRowIndices = matRowIndices,
          matColIndices = matColIndices,
          value = as.numeric(var_values),
          symmetric = dyad_vars_symmetric[ii],
          missing_to_zero = TRUE,
          diag_to_NA = FALSE
        )
      }
    }

    # if dyad data attribute already existed, merge with existing data
    if( dyad_data_attrib_exists ){
      existing_vars <- dyad_data_0[[as.character(timePd)]]
      if(!is.null(existing_vars)) {
        # Merge existing variables with new ones
        var_matrices <- c(existing_vars, var_matrices)
      }
    }

    return(var_matrices)
  })

  # add time period labels to list
  names(dyad_structure) <- as.character(msrmnts$time)

  # add the new dyadic data structure as an attribute
  attr(netlet, 'dyad_data') <- dyad_structure

  # Return object
  return(netlet)
}

#' Determine optimal storage mode for matrix values
#' 
#' This internal function examines a vector of values and determines the most
#' efficient storage mode for creating matrices. It helps optimize memory usage
#' by selecting the appropriate data type-specific matrix creation function.
#'
#' @param values A vector of values that will be stored in a matrix. Can be
#'   logical, character, integer, or numeric.
#'
#' @return A character string indicating the optimal storage mode:
#'   \itemize{
#'     \item \code{"logical"} for logical vectors
#'     \item \code{"character"} for character vectors  
#'     \item \code{"integer"} for integer vectors or numeric vectors containing only integer values
#'     \item \code{"double"} for numeric vectors with decimal values or as default fallback
#'   }
#'
#' @details
#' The function performs type checking in a specific order to determine the most
#' appropriate storage mode. For numeric values, it additionally checks whether
#' all values are integers (even if stored as doubles) to potentially use more
#' memory-efficient integer storage. Empty vectors default to "double" storage.
#' 
#' This optimization is particularly important for large networks where using
#' the correct storage type can significantly reduce memory usage.
#'
#' @author Shahryar Minhas
#' 
#' @keywords internal
#' @noRd

determine_storage_mode <- function(values) {
  if(length(values) == 0) return("double")
  
  if(is.logical(values)) return("logical")
  if(is.character(values)) return("character")
  if(is.integer(values)) return("integer")
  if(is.numeric(values)) {
    # Check if values are actually integers
    if(all(values == as.integer(values), na.rm = TRUE)) {
      return("integer")
    } else {
      return("double")
    }
  }
  
  return("double")  # default fallback
}