#' Converts a dyadic data into a network in various formats
#'
#' `get_adjacency` takes in a dyadic dataset and outputs an adjacency matrix representation.
#'
#' @param dyad_data a dyadic dataframe (or a tibble)
#' @param actor1 character: name of the actor 1 in dyad_data
#' @param actor2 character: name of the actor 2 in dyad_data
#' @param symmetric logical: whether ties are symmetric, default is TRUE
#' @param mode character: whether the network is unipartite or bipartite, default is unipartite
#' @param weight character: name of the weighted edges variable, default is NULL
#' @param sum_dyads logical: whether to sum up the `weight` value when there exists repeating dyads within the dataset
#' @param diag_to_NA logical: whether diagonals should be set to NA, default is TRUE
#' @param missing_to_zero logical: whether missing values should be set to zero, default is TRUE
#'
#' @return an adjacency matrix of class netify
#'
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @examples
#'
#' # load example directed event data from ICEWS
#' # this data comes in the form of a dyadic
#' # dataframe where all dyad pairs are listed
#' data(icews)
#' 
#' # subset to a particular year
#' icews <- icews[icews$year=='2010', ]
#' 
#' # generate a cross sectional, directed, and weighted network
#' # where the weights are verbCoop
#' icews_verbCoop <- get_adjacency(
#'   dyad_data=icews, actor1='i', actor2='j',
#'   symmetric=FALSE, weight='verbCoop' )

#' # generate a cross sectional, directed and weighted network
#' # where the weights are matlConf
#' icews_matlConf <- get_adjacency(
#'   dyad_data=icews, actor1='i', actor2='j',
#'   symmetric=FALSE, weight='matlConf' )
#' 
#' 
#' @export get_adjacency
#' 

get_adjacency <- function(
  dyad_data,
  actor1=NULL, actor2=NULL, 
  symmetric=TRUE, mode='unipartite',
  weight=NULL, sum_dyads=FALSE, 
  diag_to_NA=TRUE, missing_to_zero=TRUE
){

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

  # create weight string for storage as attribute in netify object
  weight_label <- weight_string_label(weight, sum_dyads)

  # add weight if not supplied
  wOrig <- weight
  if(is.null(weight)){
    dyad_data$weight_var <- 1
    weight <- 'weight_var'
  }

  # subset to relevant vars once
  dyad_data <- dyad_data[,c(actor1, actor2, weight)]

  # get vector of actors - optimized extraction
  actors_rows <- unique_vector(dyad_data[,actor1])
  actors_cols <- unique_vector(dyad_data[,actor2])
  actors <- unique_vector(actors_rows, actors_cols)
  if(mode=='unipartite'){ 
    actors_rows <- actors_cols <- actors 
  }

  # actor year info
  actor_pds <- data.frame(
    actor=actors, 
    stringsAsFactors=FALSE
  )
  actor_pds$min_time <- 1
  actor_pds$max_time <- 1

  # check if there are repeating dyads
  num_repeat_dyads <- repeat_dyads_check(dyad_data, actor1, actor2)
  if(num_repeat_dyads > 0){ 
    edge_value_check(wOrig, sum_dyads, TRUE) 
  }
   
  # aggregate data if sum dyads selected
  if(sum_dyads){
    dyad_data <- agg_across_units(dyad_data, actor1, actor2, NULL, weight, symmetric, missing_to_zero)
  }
  
  # remove zeros early if missing_to_zero is TRUE
  if(missing_to_zero){
    dyad_data <- dyad_data[dyad_data[,weight] != 0, ]
  }

  # Cache frequently accessed columns for efficiency
  dyad_actor1 <- dyad_data[,actor1]
  dyad_actor2 <- dyad_data[,actor2]
  dyad_weight <- dyad_data[,weight]

  # assign cross-section value for adjmat depending on user inputs
  value <- dyad_weight

  # create logical value that is TRUE if weight is just 0/1 - optimized check
  weight_binary <- length(value) == 0 || all(value %in% c(0, 1))

  # Pre-compute matrix indices to avoid repeated match() calls
  matRowIndices <- match(dyad_actor1, actors_rows)
  matColIndices <- match(dyad_actor2, actors_cols)

  # convert to adjacency matrix using optimized C++ function
  adj_out <- get_matrix(
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

  # if user left weight NULL and set sum_dyads
  # to FALSE then record weight as NULL for
  # attribute purposes
  if(!sum_dyads && is.null(wOrig)){ 
    weight <- NULL 
  }

  # layer label
  if(is.null(weight)){ 
    layer_label <- 'weight1' 
  } else { 
    layer_label <- weight 
  }

  # add class info and attributes efficiently
  class(adj_out) <- 'netify'
  attributes(adj_out) <- c(attributes(adj_out), list(
    netify_type = 'cross_sec',
    actor_time_uniform = TRUE,
    actor_pds = actor_pds,
    weight = weight,
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

  return(adj_out)
}