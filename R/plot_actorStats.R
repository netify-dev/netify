#' Plotting function for actor level statistics for netify objects
#'
#' `plot_actorStats` takes in the output from `summary_actor` function
#' and outputs a graph with actor level statistics.
#'
#' @param summary_df dataframe produced by summary_actor
#' @param longitudinal logical: TRUE if longitudinal network, default is FALSE
#' @param net_stat character: actor-level network statistics, default is "average_degree_total"
#' @param actor_names character: actor name(s) in the network, default is NULL
#' @param toplist_n numeric: the number of actors to be listed as top n actors,
#' default is 20
#' @return a graph object summarizing actor level stats of the network(s)
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @export plot_actorStats
#' @export


plot_actorStats <- function(
  summary_df, longitudinal=FALSE, net_stat=NULL,
  actor_names=NULL, toplist_n=NULL) {

  # assert dependencies for plotting and reshaping data
  assert_dependency("ggplot2")
  assert_dependency("tidyr")  

  # organize 
  summary_df <- data.frame(do.call(cbind, summary_df), stringsAsFactors=FALSE)

  # cross-sec
  if(longitudinal==FALSE){
    
    # top 20 & no actor specified
    if(is.null(actor_names)){
      
      summary_df <- summary_df[, c("actor", net_stat)]
      summary_df <- data.frame(tidyr::pivot_longer(summary_df, !actor))
      splits <- split(summary_df, summary_df$name)
      order_topList <- function(x){ 
        head(x[order(x$value, decreasing = TRUE),], n=toplist_n )}
      heads <- lapply(splits, order_topList)
      summary_df <- do.call(rbind, heads)
      summary_df$value <- as.numeric(summary_df$value)
        
      p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = stats::reorder(actor, value), y = value)) +
        ggplot2::geom_bar(stat = "identity", position=ggplot2::position_dodge()) +
        ggplot2::labs(
          x = "",
          y = "",
          title = "",
          color = "")  +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.border = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.title = ggplot2::element_blank()) +
        ggplot2::facet_wrap(name~., scales = "free", ncol = 2) }
    
    
    if(!is.null(actor_names)){
      summary_df <- summary_df[summary_df[,"actor"] %in% c(actor_names), ] 
      summary_df <- summary_df[,c("actor", net_stat)]
      summary_df <- data.frame(tidyr::pivot_longer(summary_df, !actor))
      summary_df$value <- as.numeric(summary_df$value)
      
      
      p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = actor, y = value)) +
        ggplot2::geom_bar(stat="identity") +
        ggplot2::labs(
          x = "",
          y = "",
          title = "",
          color = "") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.border = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.title = ggplot2::element_blank()) +
        ggplot2::facet_wrap(name~., scales = "free", ncol = 2)
    }
  }
  
  else{

    
    if(!is.null(actor_names)){
      summary_df <- summary_df[summary_df[,"actor"] %in% c(actor_names), ]
    }
    summary_df <- data.frame(tidyr::pivot_longer(summary_df, !c(actor, time)))
    
    if(!is.null(net_stat)){
      summary_df <- summary_df[summary_df[,"name"] %in% c(net_stat), ]
    }
    
    summary_df$value <- as.numeric(summary_df$value)
    summary_df$time <- as.numeric(summary_df$time)
    
    p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = time, y = value, color=actor)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.border = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank()) +
      ggplot2::facet_wrap(name~., scales = "free", ncol = 2) }
  
  return(p)
  
}





