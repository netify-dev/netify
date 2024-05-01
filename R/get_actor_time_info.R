#' Creates a dataframe indicating start and end time point
#'
#' `get_actor_time_info` takes in a longitudinal dyadic dataset and computes when
#' actors entered and exited the network. Entering will be determined by the
#' first period the actor had an interaction and exiting by the last period.
#' 
#' @param dyad_data a dyadic dataframe
#' @param actor1 character: actor 1 in the data
#' @param actor2 character: actor 2 in the data
#' @param time character: time in the data
#' 
#' @return a dataframe with three columns:
#' \describe{
#'   \item{actor}{a column indicating actors in the data}
#'   \item{min_time}{a column indicating the first time
#'     point in which the actor should be considered a part of the network}
#'   \item{max_time}{a column indicating the last time point in
#'     which the actor should be considered a part of the network}
#' }
#'
#' @author Shahryar Minhas, Ha Eun Choi
#' 
#' @examples
#' \dontrun{
#' library(peacesciencer)
#' library(dplyr)
#'
#' cow_dyads <- create_dyadyears(
#'   subset_years = c(1980:2001)
#'   ) %>%
#'   # add mids
#'   add_cow_mids()
#'
#' actor_time <- get_actor_time_info(
#'   dyad_data = cow_dyads, 
#'   actor1 = 'ccode1', 
#'   actor2 = 'ccode2', 
#'   time = 'year'
#' )
#' }
#'
#' @export get_actor_time_info
#' 

get_actor_time_info <- function(
  dyad_data,
  actor1, actor2, time
){

  # convert to dyadic data.frame
  dyad_data <- data.frame(dyad_data, stringsAsFactors=FALSE)

  # restructure data into a nodal format so that we can more easily
  # calculate min and max time points from the data
  a1 <- dyad_data[,c(actor1, time)]
  a2 <- dyad_data[,c(actor2, time)]
  names(a2) <- names(a1)
	nodal <- matrix(NA, nrow=(nrow(a1) + nrow(a2)), ncol=2,
		dimnames=list(NULL, c(actor1, time)) )
	nodal[1:nrow(a1),] <- as.matrix(a1)
	nodal[(nrow(a1)+1):nrow(nodal),] <- as.matrix(a2)
	rm(a1, a2)

	# get time stats by actor
  actor_year <- tapply(
    nodal[,time], nodal[,actor1], function(x){
      c(num(min(x, na.rm=TRUE)), num(max(x, na.rm=TRUE))) })
	rm(nodal)

  #
  actor_info <- do.call('rbind', actor_year)
  actor_info <- data.frame(actor_info, stringsAsFactors=FALSE)
  actor_info$actor <- rownames(actor_info)
  rownames(actor_info) <- NULL
  names(actor_info) <- c('min_time','max_time','actor')
  actor_info <- actor_info[,c('actor','min_time','max_time')]

  #
  return(actor_info)
}

#' actor_pds_to_frame
#'
#' This function converts the actor_pds attribute of netify
#' objects into a data.frame at the unit of observation level
#' @param netlet_actor_pds actor pds attributes from netify object
#' @return a data.frame object with actor pd info in a actor-time format
#' @author Colin Henry, Shahryar Minhas
#' @export

actor_pds_to_frame <- function(netlet_actor_pds){

	# iterate through actor rows
	frame <- lapply(1:nrow(netlet_actor_pds), function(ii){

		# get time range
		timeRange <- netlet_actor_pds$min_time[ii]:netlet_actor_pds$max_time[ii]

		# create frame for all pds actor existed
		iiFrame <- expand.grid(netlet_actor_pds$actor[ii], timeRange)
		iiFrame <- data.frame(iiFrame, stringsAsFactors=FALSE)
		names(iiFrame) <- c('actor', 'time')
		iiFrame$actor <- char(iiFrame$actor)
		return(iiFrame) })

	# combine into a single df and return
	frame <- do.call('rbind', frame)
	return(frame)
}

#' Derive actor periods attribute from netlet
#' 
#' @param netlet multilayer netlet object
#' @return data.frame formatted in the same way as the 
#' actor_pds attribute
#' 
#' @author Shahryar Minhas
#' 
#' @export

actor_pds_from_netlet <- function(netlet){

    # get type
    netlet_type <- attr(netlet, 'netify_type')

    # get measurements
    msrmnts <- netify_measurements(netlet)

    # cross_sec type is just the unique of 
    # row and col actors and time cols set to 1
    if(netlet_type == 'cross_sec'){

        actors <- unique(c(msrmnts$row_actors, msrmnts$col_actors))
        actor_pds <- data.frame(
            actor = actors, min_time = 1, max_time = 1, 
            stringsAsFactors = FALSE ) }

    # array type is the unique of row and col actors
    # expanded over the time range in the data
    if(netlet_type == 'longit_array'){

        frame <- expand.grid(
            actor1 = msrmnts$row_actors, 
            actor2 = msrmnts$col_actors,
            time = msrmnts$time )
        frame$actor1 <- char( frame$actor1 )
        frame$actor2 <- char( frame$actor2 )
        actor_pds <- get_actor_time_info(frame, 'actor1', 'actor2', 'time')
        actor_pds$min_time <- char( actor_pds$min_time )
        actor_pds$max_time <- char( actor_pds$max_time ) }

    # list type is the unique of row and col actors
    # expanded over the time range in the data
    if(netlet_type == 'longit_list'){

        # iterate through time ranges and get actors
        # by time period
        time_pds <- unique(msrmnts$time)
        frame <- lapply(time_pds, function(tt){

            # organize frame
            frame <- expand.grid(
                actor1 = msrmnts$row_actors[[tt]],
                actor2 = msrmnts$col_actors[[tt]],
                time = tt )
            frame$actor1 <- char( frame$actor1 )
            frame$actor2 <- char( frame$actor2 )
            frame$time <- char( frame$time )
            return(frame) })
        frame <- do.call('rbind', frame)
        actor_pds <- get_actor_time_info(
            frame, 'actor1', 'actor2', 'time') }

    #
    return(actor_pds) }