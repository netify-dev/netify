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
#' - For cross-sectional data, it can generate violin plots to show the distribution of statistics across actors or bar plots to compare specific actors.
#' - For longitudinal data, it can generate violin plots to show how statistics distribute over time across actors or line plots to track changes in statistics for specific actors over time.
#'
#'
#' @import ggplot2
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

    # across actor violin
    if(across_actor) {
      # violin plots to showcase dist across actors by var
      viz = ggplot( ggdata, aes(x=!!sym("variable"), y=!!sym("value")) ) + 
        geom_violin() +
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
      # factor time for violin
      ggdata$time = factor(ggdata$time)
      # violins to showcase dist across actors over time by var
      viz = ggplot( ggdata, aes(x=!!sym("time"), y=!!sym("value")) ) +
        geom_violin() +
        labs(x='', y='') +        
        facet_wrap(~variable, scales='free') +
        theme_stat_netify()
    }

    # actor changes over time
    if(!across_actor){

      # write a warning that if they have more than 10 actors
      # the plot will quickly become unreadable
      if(length(unique(summary_df$actor)) > 10) {
        cli::cli_alert_warning(
          'Warning: You have more than 10 actors in your data. 
          The plot may become unreadable.')
      }

      # line plots to showcase change over time by actor
      viz = ggplot( ggdata, aes(
          x=!!sym("time"), y=!!sym("value"), 
          group=!!sym("actor"), color=!!sym("actor")
          ) ) + 
        geom_line() +
        geom_point() +
        labs(x='', y='') +        
        scale_color_brewer(type='qual', palette=2) +
        facet_wrap(~variable, scales='free') +
        theme_stat_netify()
    }
  }
  
  #
  return(viz)
}


# library(netify)

# library(ggplot2)

# example(decompose_netlet)

# example(get_adjacency)

# example(get_adjacency_list)


# ####### cross sec case
# cross_asym_wgt = icews_matlConf

# s_mconf = summary_actor(cross_asym_wgt)

# ggdata = reshape2::melt(s_mconf, id='actor')


# #### violins across actors
# ggplot(
#   ggdata, aes(x=variable,y=value)
# ) + 
# geom_violin() +
# facet_wrap(~variable, scales='free')

# # top actor specific
# s_mconf_a = s_mconf
# s_mconf_a = s_mconf_a[order(s_mconf_a$degree_total,decreasing=TRUE),]
# top10 = s_mconf_a$actor[1:10]
# s_mconf_a = s_mconf_a[s_mconf_a$actor %in% top10,]
# ggdata_a = reshape2::melt(s_mconf_a, id='actor')
# ggplot(
#   ggdata_a, aes(x=actor,y=value)
# ) + 
# geom_bar(stat='identity') +
# facet_wrap(~variable, scales='free') +
# theme(
#   axis.text.x = element_text(angle = 90, hjust = 1, size=2)
# )
# ####### cross sec case

# ####### longit case
# longit_sym_bin = mid_network

# s_mid = summary_actor(longit_sym_bin)

# ggdata = reshape2::melt(s_mid, id=c('actor', 'time'))

# ### density across actors over time


# ### violins across actors over time
# ggplot(
#   ggdata, aes(x=factor(time),y=value)
# ) +
# geom_violin() +
# facet_wrap(~variable, scales='free')

# ### actor changes over time
# s_mid_a = s_mid
# s_mid_a = s_mid_a[order(s_mid_a$degree,decreasing=TRUE),]
# top5 = s_mid_a$actor[1:5]
# s_mid_a = s_mid_a[s_mid_a$actor %in% top5,]
# ggdata_a = reshape2::melt(s_mid_a, id=c('actor', 'time'))
# ggplot(
#   ggdata_a, aes(x=time,y=value, group=actor, color=actor)
# ) + 
# geom_line() +
# geom_point() +
# scale_color_brewer(type='qual', palette=2) +
# facet_wrap(~variable, scales='free') +
# theme_bw() +
# theme(
#   legend.position = 'top'
# )
# ####### longit case