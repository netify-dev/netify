#' Adds graph level data to a netify object
#'
#' `add_graph` takes in a dataframe and outputs a netify object.
#'
#' @param netlet a netify object
#' @param graph_data a dataframe object
#' @param time a character object indicating which variable in graph_data tracks time
#' @param graph_vars a vector of which variables from graph_data should be merged
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
#' 
#' verbCoop_net <- netify(
#'   dyad_data=icews_10,
#'   actor1 = 'i', actor2 = 'j', 
#'   symmetric=FALSE, weight='verbCoop' )
#' 
#' # graph data should be a dataframe with one row 
#' # in the cross-sectional case and one row per  
#' # period in the longitudinal case, e.g.:
#' graphData <- summary(verbCoop_net)
#' 
#' 
#' @author Shahryar Minhas
#'
#' @export add_graph

add_graph <- function( 
  netlet, graph_data, 
  time=NULL, graph_vars=NULL 
  ){

}