#' Extract actor time range information from dyadic data
#'
#' `get_actor_time_info` analyzes a longitudinal dyadic dataset to determine when 
#' each actor enters and exits the network. Entry is defined as the first time 
#' period in which an actor appears in any interaction (as either sender or 
#' receiver), and exit as the last time period.
#' 
#' @param dyad_data A data.frame containing longitudinal dyadic observations. Must 
#'   include columns for two actors and time periods. Will be coerced to data.frame 
#'   if a tibble or data.table is provided.
#' @param actor1 Character string specifying the column name for the first actor 
#'   in each dyad.
#' @param actor2 Character string specifying the column name for the second actor 
#'   in each dyad.
#' @param time Character string specifying the column name for time periods.
#' 
#' @return A data.frame with three columns containing actor-level time information:
#'   \itemize{
#'     \item \strong{actor}: Character vector of unique actor identifiers found 
#'       in either actor1 or actor2 columns
#'     \item \strong{min_time}: The earliest time period in which each actor 
#'       appears in the data (entry point)
#'     \item \strong{max_time}: The latest time period in which each actor 
#'       appears in the data (exit point)
#'   }
#'   
#'   Actors are ordered as they appear in the aggregation, not alphabetically or 
#'   by time.
#'
#' @details
#' The function performs the following operations:
#' 
#' \strong{Data processing:}
#' \enumerate{
#'   \item Combines actor1 and actor2 columns into a single nodal format
#'   \item Aggregates by actor to find minimum and maximum time periods
#'   \item Returns a clean data.frame with one row per unique actor
#' }
#' 
#' \strong{Use cases:}
#' 
#' Main usage in this package is to:
#' \itemize{
#'   \item Preparing actor existence information for the `actor_pds` parameter in 
#'     `netify()`
#' }
#' 
#' \strong{Assumptions:}
#' \itemize{
#'   \item An actor is considered "present" in any time period where they appear 
#'     in the data, regardless of their role (sender/receiver)
#'   \item Missing values in time periods are ignored when calculating min/max
#'   \item Actors must appear in at least one non-missing time period
#' }
#'
#' @note 
#' The function assumes that presence in the data indicates network participation. 
#' If actors can be temporarily absent from the network while still being considered 
#' members, this function will not capture such gaps.
#' 
#'
#' @author Shahryar Minhas, Ha Eun Choi
#' 
#' @export get_actor_time_info

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
#' @keywords internal
#' @noRd

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
#' @keywords internal
#' @noRd

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