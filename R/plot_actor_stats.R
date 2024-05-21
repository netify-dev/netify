#' Plotting function for actor level statistics for netify objects
#'
#' `plot_actor_stats` takes in the output from `summary_actor` function
#' and creates visualizations for actor level statistics. This function supports both cross-sectional and longitudinal data, providing visual insights into the distribution of statistics either across all actors or focusing on specific actors over time.
#'
#' @param summary_df A dataframe produced by the `summary_actor` function containing actor-level or actor-time level statistics. The function expects the actor column to be labeled as "actor" and the time column to be labeled as "time" for longitudinal data.
#' @param across_actor Logical; if TRUE, visualizations will focus on the distribution of statistics across actors. If FALSE, visualizations will focus on specific actors. Default is TRUE. If setting across_actor to FALSE it is highly recommended to specify ten or fewer specific actors.
#' @param specific_actors Optional; a vector of specific actor names for which statistics will be plotted. When NULL, statistics for all actors are considered. This is only relevant if `across_actor` is set to FALSE.
#' @param specific_stats Optional; a vector of specific statistics to plot. If NULL, all available statistics in the dataframe are used. Given the number of statistics that are calculated by default for some network types, choosing a few will make the plot more readable.
#'
#' @return A `ggplot` object representing the requested visualization, which can be further customized or printed.
#'
#' @details This function is capable of generating different types of plots based on the structure of the input data:
#' - For cross-sectional data, it will show the distribution of statistics across actors or bar plots to compare specific actors.
#' - For longitudinal data, it will show how the distribution of statistics across change over time or line plots to track changes in statistics for specific actors over time.
#'
#'
#' @import ggplot2
#' @importFrom ggridges geom_density_ridges
#' @importFrom reshape2 melt
#' @importFrom cli cli_alert_danger cli_alert_warning
#' @import rlang
#' 
#' @export plot_actor_stats
#' 

plot_actor_stats <- function(
  summary_df,
  across_actor=TRUE,
  specific_actors=NULL,
  specific_stats=NULL
  ) {

  # check if summary object is longitudional
  if('time' %in% colnames(summary_df)) {
    longit = TRUE
  } else {
    longit = FALSE
  }

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

    # keep whichever ids are
    # already there
    ids = intersect(
      c('actor', 'time'),
      names(summary_df))

    # if present then subset
    summary_df <- summary_df[,
      c(ids, specific_stats)]
  }  

  # subset data to specific actors
  # if specified by the user
  if(!is.null(specific_actors)) {

    # check if user specified actors are in the data
    oops = specific_actors[!specific_actors %in% summary_df$actor]

    # if length oops not zero, then print error
    if(length(oops) > 0) {
      cli::cli_alert_danger(
        paste0(
          'Error: The following specified actors are not in the data: ', 
          paste(oops, collapse = ', '), '.'))
      stop() }

    # if present then subset
    summary_df <- summary_df[summary_df$actor %in% specific_actors,]
  }

  # cross-sectional case
  if(!longit){

    # prep data
    ggdata = reshape2::melt(summary_df, id='actor')

    # across actor density
    if(across_actor) {
      # density plots to showcase dist across actors by var
      viz = ggplot( ggdata, aes(x=!!sym("value") )) + 
        geom_density() +
        labs(x='', y='') +
        facet_wrap(~variable, scales='free') +
        theme_stat_netify()
    }

    # actor specific
    if(!across_actor){
      # barplots with actor on x axis and var on facet
      viz = ggplot( ggdata, aes(x=!!sym("actor"), y=!!sym("value")) ) + 
        geom_bar(stat='identity') +
        labs(x='', y='') +        
        facet_wrap(~variable, scales='free') +
        theme_stat_netify()
    }
  }

  # longitudinal case
  if(longit){

    # prep data
    ggdata = reshape2::melt(summary_df, id=c('actor', 'time'))

    # density across actors over time
    if(across_actor) {
      # factor time for ridges
      ggdata$time = factor(ggdata$time)
      # ridges to showcase dist across actors over time by var
      viz = ggplot( ggdata, aes( x=!!sym("value"), y=!!sym("time") ) ) +
        # geom_violin() +
        ggridges::geom_density_ridges() +
        labs(x='', y='') +        
        facet_wrap(~variable, scales='free_x') +
        theme_stat_netify() +
        theme(
          axis.text.x=element_text(size=8.8)
        )
    }

    # actor changes over time
    if(!across_actor){

      # write a warning that if they have more than 10 actors
      # the plot will quickly become unreadable
      if(length(unique(summary_df$actor)) > 8) {
        cli::cli_alert_warning(
          'Warning: You have more than 8 actors in your data. 
          The plot may become unreadable.')
      }

      # get actor count
      plus_eight_actors = ifelse(length(unique(summary_df$actor))>8, TRUE, FALSE)

      # line plots to showcase change over time by actor
      viz = ggplot( ggdata, aes(
          x=!!sym("time"), y=!!sym("value"), 
          group=!!sym("actor"), color=!!sym("actor")
          ) ) + 
        geom_line() +
        geom_point() +
        labs(x='', y='')

      # use colorbrewer to color if 8 or fewer actors
      if(!plus_eight_actors) { viz = viz + scale_color_brewer(type='qual', palette=2) } 
        
      # finish cleaning plot
      viz = viz +
        facet_wrap(~variable, scales='free') +
        theme_stat_netify()
    }
  }
  
  #
  return(viz)
}
