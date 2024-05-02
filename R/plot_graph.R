#' Plotting function for graph level statistics for netify objects
#'
#' `plot_graph` takes in the output from `summary.netify` function
#' and outputs a graph with graph level statistics.
#'
#' @param summary_df dataframe produced by summary
#' @param longitudinal logical: TRUE if longitudinal network, default is TRUE
#' @param net_stat character: network-level network statistics, default is "dens"
#' @return a graph object summarizing network level stats
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @export plot_graph
#' @export

plot_graph <- function(summary_df, longitudinal = TRUE, net_stat=net_stat) {
 
  # assert dependencies for plotting and reshaping data
  assert_dependency("ggplot2")
  assert_dependency("tidyr")    

  summary_df <- data.frame(do.call(cbind, summary_df), stringsAsFactors=FALSE)
  
  if(length(unique(summary_df[,"net"])) > 1) {
    
    df <- summary_df[,c("net", net_stat)]
    df <- data.frame(tidyr::pivot_longer(df, !net))
    df$value <- num(df$value)
    
    p <- ggplot2::ggplot(df,
                         ggplot2::aes(x=num(net), value)) +
      ggplot2::geom_line(colour="black") +
      ggplot2::geom_point() +
      ggplot2::ylab("") + ggplot2::xlab("") +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(name~., scales = "free", ncol = 2) 
    
    return(p)} else{
      
      cli::cli_alert_danger("Error: the input dataframe should be the network-level statistics of longditudinal network.")
      
    }
  
}
