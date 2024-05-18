#' Plotting function for graph level statistics for netify objects
#'
#' `plot_graph_stats` takes a data frame containing graph level statistics and 
#' generates a visual representation using either line or bar plots. This function
#' is designed to work with data frames that represent network statistics over 
#' multiple networks or time points.
#'
#' @param summary_df A data frame produced by the `summary.netify` function 
#'   or any other function that outputs network statistics in a similar format. 
#'   The data frame should have a column "net" which identifies the network 
#'   or time point for each row.
#' @param type A character string specifying the type of plot to generate: 
#'   either 'line' for line plots or 'bar' for bar plots. The default is 'line'.
#' @param specific_stats Optional vector of column names from `summary_df` 
#'   specifying which statistics to plot. If NULL (the default), all statistics 
#'   in `summary_df` will be plotted.
#'
#' @return A `ggplot` object displaying the specified statistics for each network.
#'   The plots are faceted by statistic type across the networks or time points.
#'
#'
#' @author Ha Eun Choi, Cassy Dorff, Shahryar Minhas
#'
#' @import ggplot2
#' @importFrom cli cli_alert_danger
#' @importFrom reshape2 melt
#' @import rlang
#'
#' @export plot_graph_stats
#' @export

plot_graph_stats <- function(
  summary_df, 
  type = 'line', 
  specific_stats = NULL
  ){

  # check to make sure type is bar or line
  if(type != 'line' & type != 'bar') {
    cli::cli_alert_danger(
      'Error: The `type` argument must be either "line" or "bar".')
    stop() }

  # check if data has more than one row
  if(nrow(summary_df) < 2) {
		cli::cli_alert_danger(
			'Error: The `summary_df` provided only has one row of data. 
			Consider an alternative way of depicting this information such as a table.' )
    print(summary_df)
		stop() }

  # subset data to specific stats
  # if specified by the user
  if(!is.null(specific_stats)) {

    # check if user specified stats are in the data
    oops = specific_stats[!specific_stats %in% names(summary_df)]

    # if length oops not zero, then print error
    if(length(oops) > 0) {
      cli::cli_alert_danger(
        paste0('Error: The following specified statistics are not in the data: ', paste(oops, collapse = ', '), '.'))
      stop() }

    # if present then subset
    summary_df <- summary_df[,
      c("net", specific_stats)]
  }

  # reshape data so we can create
  # grid of plots
  ggdata <- reshape2::melt(
    summary_df, id='net')
  
  # create plot
  viz = ggplot(
    data = ggdata, 
    aes(
      x = !!sym("net"), 
      y = !!sym("value"), 
      group = !!sym("variable")))

  # choose geom based on user input
  if(type == 'line') {
    viz = viz + geom_line() + geom_point() }
  if(type == 'bar') {
    viz = viz + geom_bar(stat='identity') }

  # add facets
  viz = viz +
    facet_wrap(~variable, scales='free_y') +
    labs( x='', y='' ) +
    theme_bw() +
    theme(
      panel.border =element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1),
      strip.text.x = element_text(color = "white"), 
      strip.background = element_rect(fill = "#525252", color = "#525252")
    )
  
  #
  return(viz)
}
