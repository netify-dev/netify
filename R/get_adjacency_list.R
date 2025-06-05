#' Longitudinal variants of `get_adjacency()` which returns a list of adjacency matrices
#'
#' This function takes in a dyadic dataset
#' and outputs a list of adjacency matrices in which the actor composition in the network can vary over time.
#'
#' @param dyad_data a dyadic dataframe (or a tibble)
#' @param actor1 character: name of the actor 1 variable in dyad_data
#' @param actor2 character: name of the actor 2 variable in dyad_data
#' @param time character: name of the time variable in dyad_data, the values of the time variable itself should be numeric
#' @param symmetric logical: whether ties are symmetric, default is TRUE
#' @param mode character: whether the network is unipartite or bipartite, default is unipartite
#' @param weight character: name of the weighted edges variable, default is NULL
#' @param sum_dyads logical: whether to sum up the `weight` value when there exists repeating dyads within time periods
#' @param actor_time_uniform logical: whether to assume
#' actors are the same across the full time series observed in the data
#' TRUE means that actors are the same across the full time
#' series observed in the data
#' FALSE means that actors come in and out of the observed data and
#' their "existence" should be determined by the data, meaning that
#' their first year of existence will be determined by the time point
#' of their first event and their last year of existence by the
#' time point of their last event
#' @param actor_pds a data.frame indicating start and end time point for every
#' actor, this can be created manually (see example) or using `get_actor_time_info.R`, 
#' if provided then choice of `actor_time_uniform` is irrelevant.
#' @param diag_to_NA logical: whether diagonals should be set to NA, default is TRUE
#' @param missing_to_zero logical: whether missing values should be set to zero, default is TRUE
#'
#' @return a list of adjacency matrices of class netify
#'
#' @author Cassy Dorff, Ha Eun Choi, Shahryar Minhas
#'
#' @examples
#' 
#' 
#' # load example directed event data from ICEWS
#' # this data comes in the form of a dyadic
#' # dataframe where all dyad pairs are listed
#' data(icews)
#' 
#' # generate a longitudinal, directed, and weighted network
#' # where the weights are verbConf
#' # note that in longitudinal networks we can have all the 
#' # same actors in every year and if that's the case
#' # we set actor_time_uniform to TRUE, in the next
#' # example we'll show how to adjust when actors
#' # change over time
#' icews_verbConf <- get_adjacency_list(
#'   dyad_data=icews, 
#'   actor1='i', actor2='j', time='year',
#'   actor_time_uniform=TRUE,
#'   symmetric=FALSE, weight='verbConf' )
#' icews_verbConf
#' 
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

  # if mode bipartite is specified make sure that
  # actors in actor1 and actor2 columns are distinct
  if(mode=='bipartite'){
    if(length(intersect(dyad_data[,actor1], dyad_data[,actor2])) > 0){
      cli::cli_alert_warning(
        "Warning: Mode has been inputted as bipartite but actors are not distinct across the modes."
      )
    }
  }

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
    dyad_data <- agg_across_units(dyad_data, actor1, actor2, time, weight, symmetric, missing_to_zero)
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

