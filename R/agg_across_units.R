#' agg_across_units
#'
#' Aggregate dyadic dataset
#' @param dyad_data user inputted data.frame object
#' @param actor1 character: actor 1 in the data
#' @param actor2 character: actor 2 in the data
#' @param time character: time in the data
#' @param weight character: weight variable in the data
#' @param symmetric logical: is the adjacency matrix symmetric
#' @param ignore_missing logical: should missing values be ignored
#' @return aggregated data.frame object
#' @author Shahryar Minhas
#'
#' @export agg_across_units

agg_across_units <- function(
	dyad_data,
	actor1,
	actor2,
	time=NULL,
	weight,
    symmetric, 
    ignore_missing=TRUE
){

    # add in logicals for how to treat NAs
    if(ignore_missing){
        remove_nas <- TRUE
        na_action_behav <- NULL
    } else {
        remove_nas <- FALSE
        na_action_behav <- na.omit }

    # if symmetric then flip the data on itself
    ### in the future we can think about whether
    ### flipping is necessary in every case, specifically,
    ### if both sides of the dyad show up in the symmetric
    ### data then flipping is unnecessary
    if(symmetric){
        tmp <- dyad_data
        tmp[,actor1] <- dyad_data[,actor2]
        tmp[,actor2] <- dyad_data[,actor1]
        dyad_data <- rbind(dyad_data, tmp)
        rm(tmp) }

    # check if time is specified
    if(is.null(time)){
        dyad_data <- aggregate(
            dyad_data[,weight],
            list(
                actor1=dyad_data[,actor1],
                actor2=dyad_data[,actor2]),
            simplify=TRUE,
            FUN=sum,
            na.rm=remove_nas, na.action=na_action_behav )
        names(dyad_data)[1:3] <- c(actor1, actor2, weight)
    } else {
	# aggregate
        dyad_data <- aggregate(
            dyad_data[,weight],
            list(
                actor1=dyad_data[,actor1],
                actor2=dyad_data[,actor2],
                time=dyad_data[,time]),
            simplify=TRUE,
            FUN=sum,
            na.rm=remove_nas, na.action=na_action_behav )
        names(dyad_data)[1:4] <- c(actor1, actor2, time, weight) }

    #
    return(dyad_data)
}
