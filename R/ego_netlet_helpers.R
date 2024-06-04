#' Get Neighborhood Network for Ego
#'
#' `get_ngbd_net_for_ego` extracts the neighborhood network for a specified ego from a given list of raw networks. It allows the user to define the neighborhood based on outgoing, incoming, or any relationship, and includes a threshold for weighted networks.
#'
#' @param raw_net A list of raw network matrices.
#' @param ego A character vector specifying the name of the ego for whom to create the neighborhood network.
#' @param threshold A numeric vector specifying the threshold for including alters in the neighborhood network. Default for unweighted networks is 0 and default for weighted networks is the average edge weight.
#' @param include_ego Logical; if TRUE, the ego node will be included in the neighborhood network. Default is TRUE.
#' @param ngbd_direction A character string specifying the type of relationship to consider for directed networks. Options are "out" for outgoing ties, "in" for incoming ties, and "any" for any relationship. Default is "any".
#' 
#' @return A list of neighborhood networks, each represented as a matrix.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export

get_ngbd_net_for_ego <- function(
    raw_net, ego, threshold, 
    include_ego = TRUE, ngbd_direction = "any") {
  
    ######################
    # iteratte through nets to construct ngbds
    ngbd_actors <- lapply(1:length(raw_net), function(ii) {

        # Get iith net and thresh
        net <- raw_net[[ii]]
        thresh <- threshold[ii]

        # For the ego, figure out what actors have connections
        ngbd_row <- rownames(net)[which(net[ego, ] > thresh)]
        ngbd_col <- colnames(net)[which(net[, ego] > thresh)]
        ngbd_any <- unique(c(ngbd_row, ngbd_col))

        # Add back ego if include_ego is TRUE
        if (include_ego) {
            ngbd_row <- c(ego, ngbd_row)
            ngbd_col <- c(ego, ngbd_col)
            ngbd_any <- c(ego, ngbd_any) }

        # Construct ngbd net based on ngbd_direction
        if(ngbd_direction == "out"){ ngbd_net <- net[ngbd_row, ngbd_row] }
        if(ngbd_direction == "in"){ ngbd_net <- net[ngbd_col, ngbd_col] }
        if(ngbd_direction == "any"){ ngbd_net <- net[ngbd_any, ngbd_any] }

        return(ngbd_net) })
    ######################

    ######################
    # add names
    if(length(raw_net)>1){
        names(ngbd_actors) <- paste(ego, names(raw_net), sep = "__")
    } else { names(ngbd_actors) <- ego }
    ######################

    #####################
    #
    return(ngbd_actors)
    #####################
}
