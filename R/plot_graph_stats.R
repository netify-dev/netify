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

  ######################
  # check to make sure type is bar or line
  if(type != 'line' & type != 'bar') {
    cli::cli_alert_danger(
      'Error: The `type` argument must be either "line" or "bar".')
    stop() }
  ######################

  ######################
  # determine if summary_df is summarizing multiple layers
  multilayer = FALSE
  layer_present = 'layer' %in% colnames(summary_df)  
  if(layer_present){
    if(length(unique(summary_df$layer)) == 1){
      summary_df = summary_df[,-which(colnames(summary_df) == 'layer')]
    } else { multilayer = TRUE } }
  ######################

  ######################
  # determine if summary_df is summarizing multiple time points
  if(length(unique(summary_df$net)) == 1){
    longitudinal = FALSE
    summary_df = summary_df[,-which(colnames(summary_df) == 'net')]
  } else { longitudinal = TRUE }
  ######################

  ######################
  # check if data has more than one row
  if( !multilayer & !longitudinal ) {
		cli::cli_alert_danger(
			'Error: The `summary_df` provided only has one row of data. 
			Consider an alternative way of depicting this information such as a table.' )
    print(summary_df)
		stop() }
  ######################

  ######################
  # note to user that if multilayer but not longit then function 
  # will default to bar plot
  if(multilayer & !longitudinal & type == 'line') {
    cli::cli_alert_warning(
      'Note: The data provided is summarizing multiple layers but not multiple time points. 
      The function will default to a bar plot.' )
    type = 'bar'
      }
  ######################    

  ######################
  # organize possible id variables
  ids = intersect( c('net', 'layer'), names(summary_df))
  ######################

  ######################
  # subset data to specific stats
  # if specified by the user
  if(!is.null(specific_stats)) {

    # check if user specified stats are in the data
    oops = specific_stats[!specific_stats %in% names(summary_df)]

    # if length oops not zero, then print error
    if(length(oops) > 0) {
      cli::cli_alert_danger(
        paste0(
          'Error: The following specified statistics are not in the data: ', 
          paste(oops, collapse = ', '), '.'))
      stop() }

    # if present then subset
    summary_df <- summary_df[, c(ids, specific_stats)]
  }
  ######################

  ######################
  # reshape data for ggplot
  ggdata <- reshape2::melt( summary_df, id=ids)
  ######################

  ######################  
  # define aesthetics for multilayer but not longit
  if(multilayer & !longitudinal){
    viz = ggplot( data=ggdata, 
      aes(
        x =!!sym("layer"),
        y =!!sym("value"),
        group =!!sym("variable") ) ) }

  # define aesthetics for longit but not multilayer
  if(!multilayer & longitudinal){
    viz = ggplot( data = ggdata, 
      aes(
        x = !!sym("net"), 
        y = !!sym("value"), 
        group = !!sym("variable") ) ) }

  # define aesthetics for multilayer and longit
  if(multilayer & longitudinal){
    viz = ggplot( data = ggdata, 
      aes(
        x = !!sym("net"), 
        y = !!sym("value"), 
        group = !!sym("layer"),
        color = !!sym("layer"),
        fill = !!sym("layer") ) ) }

  # choose geom based on user input
  if(type == 'line') { viz = viz + geom_line() + geom_point() }
  if(type == 'bar') { viz = viz + geom_bar(stat='identity', position=position_dodge()) }

  # add facets
  viz = viz +
    facet_wrap(~variable, scales='free_y') +
    labs( x='', y='' ) +
    theme_stat_netify()

  # if multilayer and not longit
  # then make x axis labels horizontal
  if(multilayer & !longitudinal){
    viz = viz + theme(axis.text.x = element_text(angle = 0, hjust = .5)) }
  ######################
  
  ######################  
  return(viz)
  ######################
}

# library(netify)
# example(layer_netlet)

# # icews_matlCoop # cross-sec single layer
# # icews_verbCoop_matlCoop # cross-sec multi layer
# # icews_matlCoop_longit_l # longit single layer
# # icews_verbCoop_matlCoop_longit_l # longit multi layer

# c_sing = summary(icews_matlCoop)
# c_mult = summary(icews_verbCoop_matlCoop)
# l_sing = summary(icews_matlCoop_longit_l)
# l_mult = summary(icews_verbCoop_matlCoop_longit_l)


# # cross-sec single layer variations
# plot_graph_stats(c_sing)

# # cross-sec multi layer variations
# plot_graph_stats(c_mult)

# # longit single layer variations
# plot_graph_stats(l_sing)

# # longit multi layer variations
# plot_graph_stats(l_mult)

