#' Create a netify list from longitudinal dyadic data
#'
#' `get_adjacency_list` converts longitudinal dyadic data into a list of adjacency 
#' matrices of class "netify". This function creates a list structure where each 
#' element is a network matrix for a specific time period, allowing for 
#' time-varying actor composition.
#'
#' @param dyad_data A data.frame containing longitudinal dyadic observations. Will 
#'   be coerced to data.frame if a tibble or data.table is provided.
#' @param actor1 Character string specifying the column name for the first actor 
#'   in each dyad.
#' @param actor2 Character string specifying the column name for the second actor 
#'   in each dyad.
#' @param time Character string specifying the column name for time periods. Values 
#'   in this column must be numeric.
#' @param symmetric Logical. If TRUE (default), treats the network as undirected 
#'   (i.e., edges have no direction). If FALSE, treats the network as directed.
#' @param mode Character string specifying network structure. Options are:
#'   \itemize{
#'     \item \code{"unipartite"}: One set of actors (default)
#'     \item \code{"bipartite"}: Two distinct sets of actors
#'   }
#' @param weight Character string specifying the column name containing edge weights. 
#'   If NULL (default), edges are treated as unweighted (binary).
#' @param sum_dyads Logical. If TRUE, sums weight values when multiple edges exist 
#'   between the same actor pair in the same time period. If FALSE (default), uses 
#'   the last observed value.
#' @param actor_time_uniform Logical indicating how to handle actor composition:
#'   \itemize{
#'     \item \code{TRUE}: Assumes all actors exist across the entire time range
#'     \item \code{FALSE}: Determines actor existence from the data - actors exist 
#'       from their first observed interaction to their last
#'   }
#' @param actor_pds Optional data.frame specifying when actors enter and exit the 
#'   network. Must contain columns 'actor', 'min_time', and 'max_time'. Can be 
#'   created using `get_actor_time_info()`. If provided, overrides actor_time_uniform.
#' @param diag_to_NA Logical. If TRUE (default), sets diagonal values (self-loops) 
#'   to NA. Automatically set to FALSE for bipartite networks.
#' @param missing_to_zero Logical. If TRUE (default), treats missing edges as zeros. 
#'   If FALSE, missing edges remain as NA.
#'
#' @return A list of class "netify" (a netify list) with:
#'   \itemize{
#'     \item \strong{Elements}: Named list where each element is a netify matrix 
#'       for one time period
#'     \item \strong{Names}: Character representation of time periods
#'     \item \strong{Class}: "netify" - this is a full netify object compatible 
#'       with all netify functions
#'     \item \strong{Attributes}: Extensive metadata including network properties, 
#'       actor composition information, and processing parameters
#'   }
#'   
#'   Each matrix in the list may have different dimensions if actor composition 
#'   varies over time. The returned object can be used with all netify functions 
#'   such as `summary()`, `plot()`, `to_igraph()`, etc.
#'
#' @details
#' \strong{Note on usage:}
#' 
#' While this function is exported and available for direct use, the primary and 
#' recommended way to create netify objects from longitudinal dyadic data is through 
#' the `netify()` function. The `netify()` function:
#' \itemize{
#'   \item Automatically chooses between array and list representations based on 
#'     your data
#'   \item Provides more comprehensive data validation
#'   \item Can incorporate nodal and dyadic attributes during creation
#'   \item Offers a unified interface for all types of network data
#' }
#' 
#' Use `get_adjacency_list()` directly only when you specifically need a list 
#' structure or require low-level control over the creation process.
#' 
#' \strong{Actor composition handling:}
#' 
#' This function is particularly useful when actors enter and exit the network 
#' over time. Unlike `get_adjacency_array()`, which requires constant actor 
#' composition, this function can handle:
#' \itemize{
#'   \item New actors appearing in later time periods
#'   \item Actors exiting and no longer appearing in the data
#'   \item Different sets of actors active in each time period
#' }
#'
#' @examples
#' # Load example data
#' data(icews)
#' 
#' # Create a netify list with constant actor composition
#' icews_list <- get_adjacency_list(
#'   dyad_data = icews, 
#'   actor1 = 'i', 
#'   actor2 = 'j', 
#'   time = 'year',
#'   actor_time_uniform = TRUE,
#'   symmetric = FALSE, 
#'   weight = 'verbConf'
#' )
#' 
#' # Verify it's a netify object
#' class(icews_list)  # "netify"
#' 
#' # Check structure
#' length(icews_list)  # Number of time periods
#' names(icews_list)   # Time period labels
#' 
#' # Access specific time period
#' icews_2010 <- icews_list[["2010"]]
#' dim(icews_2010)
#'
#' @author Cassy Dorff, Ha Eun Choi, Shahryar Minhas
#' 
#' @export get_adjacency_list
#' 

get_adjacency_list <- function(
  dyad_data,
  actor1=NULL, actor2=NULL, time=NULL, 
  symmetric=TRUE, mode='unipartite',
  weight=NULL, sum_dyads=FALSE,
  actor_time_uniform=FALSE,
  actor_pds=NULL,
  diag_to_NA=TRUE, missing_to_zero=TRUE
){

  # create weight string for storage as attribute in netify object
  weight_label <- weight_string_label(weight, sum_dyads)

  # if bipartite network then force diag_to_NA to be FALSE
  # and force asymmetric, create copy to preserve user choice
  user_symmetric <- symmetric  
  if(mode=='bipartite'){
    diag_to_NA <- FALSE
    symmetric <- FALSE 
  }

  # # if mode bipartite is specified make sure that
  # # actors in actor1 and actor2 columns are distinct
  # if(mode=='bipartite'){
  #   if(length(intersect(dyad_data[,actor1], dyad_data[,actor2])) > 0){
  #     cli::cli_alert_warning(
  #       "Warning: Mode has been inputted as bipartite but actors are not distinct across the modes."
  #     )
  #   }
  # }

  # check if user supplied actor_pds
  user_actor_pds <- !is.null(actor_pds)

  # check to make sure time variable actually is numeric
  if(!is.numeric(dyad_data[,time])){
    cli::cli_alert_danger('Values in the time variable must be numeric.')
    stop()
  }

  # add weight if not supplied
  wOrig <- weight
  if(is.null(weight)){
    dyad_data$weight_var <- 1
    weight <- 'weight_var'
  }

  # subset to relevant vars once
  dyad_data <- dyad_data[,c(actor1, actor2, time, weight)]
  
  # get vector of time periods and convert to character
  time_pds <- char(unique_vector(dyad_data[,time]))
  time_pds_num <- as.numeric(time_pds)
  
  # Pre-compute all actors from data
  a1_all <- unique_vector(dyad_data[,actor1])
  a2_all <- unique_vector(dyad_data[,actor2])
  
  # if no actor_pds provided then calculate based on actor_time_uniform
  if(is.null(actor_pds)){
    
    if(actor_time_uniform){
      actors <- unique_vector(a1_all, a2_all)
      min_time <- min(dyad_data[,time], na.rm=TRUE)
      max_time <- max(dyad_data[,time], na.rm=TRUE)
      actor_pds <- data.frame(
        actor=actors, 
        min_time=min_time,
        max_time=max_time,
        stringsAsFactors=FALSE
      )
    } else {
      actor_pds <- get_actor_time_info(dyad_data, actor1, actor2, time)
    }
    
  } else {
    # make sure actor_pds is a data.frame
    actor_pds <- df_check(actor_pds)

    # make sure every actor has only been entered once
    if(length(unique(actor_pds$actor)) != nrow(actor_pds)){
      cli::cli_alert_danger(
        "Actors are repeating in `actor_pds`. Every actor must show up only once with a unique `min_time` and `max_time`."
      )
      stop()
    }

    # rename columns consistently
    names(actor_pds) <- c('actor', 'min_time', 'max_time')

    # update time periods based on actor_pds
    time_pds <- char(unique_vector(actor_pds$min_time, actor_pds$max_time))
    time_pds_num <- as.numeric(time_pds)

    # get actors present in data
    actors_in_data <- unique_vector(a1_all, a2_all)
    if(mode=='unipartite'){ 
      a1_all <- a2_all <- actors_in_data
    }

    # Vectorized filtering - more efficient than string operations
    # Create logical vectors for filtering
    actor1_valid <- dyad_data[,actor1] %in% actors_in_data
    actor2_valid <- dyad_data[,actor2] %in% actors_in_data  
    time_valid <- dyad_data[,time] %in% time_pds_num
    
    # Apply all filters at once
    valid_rows <- actor1_valid & actor2_valid & time_valid
    dyad_data <- dyad_data[valid_rows, ]

    # stop process if no dyads remain
    if(nrow(dyad_data) == 0){
      cli::cli_alert_danger(
        "No dyads remain after subsetting to the actors and years defined in `actor_pds`."
      )
      stop()
    }

    # More efficient actor-time filtering using vectorized operations
    # Create lookup table for valid actor-time combinations
    actor_time_lookup <- do.call(rbind, lapply(1:nrow(actor_pds), function(i){
      data.frame(
        actor = actor_pds$actor[i],
        time = actor_pds$min_time[i]:actor_pds$max_time[i],
        stringsAsFactors = FALSE
      )
    }))
    
    # Create keys for fast lookup
    lookup_key <- paste(actor_time_lookup$actor, actor_time_lookup$time, sep="_")
    data_key1 <- paste(dyad_data[,actor1], dyad_data[,time], sep="_")
    data_key2 <- paste(dyad_data[,actor2], dyad_data[,time], sep="_")
    
    # Filter using vectorized %in% operations
    valid_pairs <- data_key1 %in% lookup_key & data_key2 %in% lookup_key
    dyad_data <- dyad_data[valid_pairs, ]

    # Update actor lists based on filtered data
    a1_all <- unique_vector(dyad_data[,actor1])
    a2_all <- unique_vector(dyad_data[,actor2])
  }

  # check if there are repeating dyads
  num_repeat_dyads <- repeat_dyads_check(dyad_data, actor1, actor2, time)
  if(num_repeat_dyads > 0){ 
    edge_value_check(wOrig, sum_dyads, TRUE) 
  }

  # aggregate data if sum dyads selected
  if(sum_dyads){
    dyad_data <- aggregate_dyad(dyad_data, actor1, actor2, time, weight, symmetric, missing_to_zero)
  }

  # remove zeros early if missing_to_zero is TRUE
  if(missing_to_zero){
    dyad_data <- dyad_data[dyad_data[,weight] != 0, ]
  }

  # Pre-compute actor presence matrix for all time periods
  # This avoids recalculating for each time period
  actor_presence_matrix <- matrix(FALSE, nrow=nrow(actor_pds), ncol=length(time_pds_num))
  for(i in 1:nrow(actor_pds)){
    actor_presence_matrix[i, ] <- (time_pds_num >= actor_pds$min_time[i]) & 
                                  (time_pds_num <= actor_pds$max_time[i])
  }
  
  # Pre-split data by time periods for faster subsetting
  time_indices <- split(seq_len(nrow(dyad_data)), dyad_data[,time])
  
  # Cache frequently accessed columns
  dyad_actor1 <- dyad_data[,actor1]
  dyad_actor2 <- dyad_data[,actor2]
  dyad_weight <- dyad_data[,weight]

  # iterate through time periods with optimized operations
  adj_out <- vector("list", length(time_pds))
  names(adj_out) <- time_pds
  
  for(t_idx in seq_along(time_pds)){
    time_pd <- time_pds[t_idx]
    time_pd_num <- time_pds_num[t_idx]
    
    # Get indices for this time period
    slice_indices <- time_indices[[as.character(time_pd_num)]]
    if(is.null(slice_indices)) slice_indices <- integer(0)
    
    # determine actors present in this time period using pre-computed matrix
    actor_present <- actor_presence_matrix[, t_idx]
    actors <- sort(actor_pds$actor[actor_present])
    actors_rows <- actors_cols <- actors

    # break up into rows and cols for bipartite
    if(mode == 'bipartite'){
      actors_rows <- sort(actor_pds$actor[actor_present & actor_pds$actor %in% a1_all])
      actors_cols <- sort(actor_pds$actor[actor_present & actor_pds$actor %in% a2_all])
    }

    # get values and indices for this time slice
    if(length(slice_indices) > 0){
      slice_actor1 <- dyad_actor1[slice_indices]
      slice_actor2 <- dyad_actor2[slice_indices]
      value <- dyad_weight[slice_indices]
      
      # Pre-compute matrix indices to avoid repeated match() calls
      matRowIndices <- match(slice_actor1, actors_rows)
      matColIndices <- match(slice_actor2, actors_cols)
    } else {
      value <- numeric(0)
      matRowIndices <- integer(0)
      matColIndices <- integer(0)
    }

    # create logical value that is TRUE if weight is just 0/1
    weight_binary <- length(value) == 0 || all(value %in% c(0, 1))

    # get adj mat filled in using optimized C++ function
    adj_mat <- get_matrix(
      n_rows = length(actors_rows),
      n_cols = length(actors_cols),
      actors_rows = actors_rows,
      actors_cols = actors_cols,
      matRowIndices = matRowIndices,
      matColIndices = matColIndices,
      value = value,
      symmetric = symmetric,
      missing_to_zero = missing_to_zero,
      diag_to_NA = diag_to_NA && mode == 'unipartite'
    )

    # if user left weight NULL and set sum_dyads to FALSE
    weight_attr <- if(!sum_dyads && is.null(wOrig)) NULL else weight
    layer_label <- if(is.null(weight_attr)) 'weight1' else weight_attr

    # add class info and attributes efficiently
    class(adj_mat) <- 'netify'
    attributes(adj_mat) <- c(attributes(adj_mat), list(
      netify_type = 'cross_sec',
      actor_time_uniform = NULL,
      actor_pds = NULL,
      weight = weight_attr,
      detail_weight = weight_label,
      weight_binary = weight_binary,
      symmetric = user_symmetric,
      mode = mode,
      layers = layer_label,
      diag_to_NA = diag_to_NA,
      missing_to_zero = missing_to_zero,
      sum_dyads = sum_dyads,
      nodal_data = NULL,
      dyad_data = NULL
    ))
    
    adj_out[[t_idx]] <- adj_mat
  }
  
  # Final weight attribute handling
  weight_final <- if(!sum_dyads && is.null(wOrig)) NULL else weight
  layer_label_final <- if(is.null(weight_final)) 'weight1' else weight_final

  # if user supplied actor_pds then set actor_time_uniform to FALSE
  if(user_actor_pds) actor_time_uniform <- FALSE

  # get info on binary weights using vectorized operation
  bin_check <- vapply(adj_out, function(x) attr(x, 'weight_binary'), logical(1))

  # add attributes to list efficiently
  class(adj_out) <- 'netify'
  attributes(adj_out) <- c(attributes(adj_out), list(
    netify_type = 'longit_list',
    actor_time_uniform = actor_time_uniform,
    actor_pds = actor_pds,
    weight = weight_final,
    detail_weight = weight_label,
    weight_binary = all(bin_check),
    symmetric = user_symmetric,
    mode = mode,
    layers = layer_label_final,
    diag_to_NA = diag_to_NA,
    missing_to_zero = missing_to_zero,
    sum_dyads = sum_dyads,
    nodal_data = NULL,
    dyad_data = NULL
  ))
  
  return(adj_out)
}

