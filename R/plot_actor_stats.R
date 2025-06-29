#' Visualize actor-level network statistics
#'
#' `plot_actor_stats` creates visualizations of actor-level statistics from
#' `summary_actor()` output. The function automatically adapts to the data
#' structure (cross-sectional/longitudinal, single/multilayer) and offers two
#' main visualization approaches: distribution across actors or tracking specific
#' actors.
#'
#' @param summary_df A data frame from `summary_actor()` containing actor-level
#'   statistics. Must include an "actor" column. May include "time" column for
#'   longitudinal data and "layer" column for multilayer networks.
#' @param longitudinal Logical indicating whether to treat data as longitudinal.
#'   If NULL (default), automatically detected based on presence of "time" column.
#'   Set to FALSE if only one unique time point exists.
#' @param multilayer Logical indicating whether to treat data as multilayer.
#'   If NULL (default), automatically detected based on presence of "layer" column.
#'   Set to FALSE if only one unique layer exists.
#' @param across_actor Logical. If TRUE (default), visualizes distribution of
#'   statistics across all actors. If FALSE, focuses on tracking specific actors.
#'   When TRUE with `specific_actors` provided, shows distribution for only those
#'   actors.
#' @param specific_stats Character vector of statistic names to plot. If NULL
#'   (default), plots all available statistics. Must match column names in
#'   `summary_df`.
#' @param specific_actors Character vector of actor names to highlight or focus on.
#'   If NULL (default) with `across_actor = FALSE`, includes all actors (with
#'   warning if > 25 actors). Must match values in the "actor" column.
#'
#' @return A ggplot object that can be further customized or saved. The plot type
#'   depends on the data structure and parameters:
#'   \itemize{
#'     \item \strong{Cross-sectional, across actors}: Density plots with rug plots
#'     \item \strong{Cross-sectional, specific actors}: Beeswarm plots
#'     \item \strong{Longitudinal, across actors}: Ridge density plots over time
#'     \item \strong{Longitudinal, specific actors}: Line plots over time
#'   }
#'
#'   All plots are faceted by statistic and, when applicable, by layer.
#'
#' @details
#' \strong{Visualization logic:}
#'
#' The function chooses appropriate visualizations based on data structure:
#' \itemize{
#'   \item \strong{Distribution plots} (`across_actor = TRUE`): Show how statistics
#'     are distributed across the actor population
#'   \item \strong{Actor-specific plots} (`across_actor = FALSE`): Track individual
#'     actors, with specified actors highlighted in color while others appear in gray
#' }
#'
#'
#' All plots use `theme_stat_netify()` for consistent styling across netify
#' visualizations.
#'
#' For multilayer longitudinal data with `across_actor = FALSE`, consider using
#' `specific_stats` to avoid overcrowded facets.
#'
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export plot_actor_stats

plot_actor_stats <- function(
    summary_df,
    longitudinal = ifelse("time" %in% colnames(summary_df), TRUE, FALSE),
    multilayer = ifelse("layer" %in% colnames(summary_df), TRUE, FALSE),
    across_actor = TRUE,
    specific_stats = NULL,
    specific_actors = NULL) {
    ######################
    # check which ids present
    time_present <- "time" %in% colnames(summary_df)
    layer_present <- "layer" %in% colnames(summary_df)

    # throw warning if there is only one unique
    # value in the time column
    if (time_present) {
        if (length(unique(summary_df$time)) == 1 & longitudinal) {
            cli::cli_alert_warning(
                "Note: The `summary_df` provided only has one unique time point, so longitudional will be set to FALSE."
            )
            longitudinal <- FALSE
            summary_df <- summary_df[, -which(colnames(summary_df) == "time")]
        }
    }

    # throw warning if there is only one unique value
    # in the layer column
    if (layer_present) {
        if (length(unique(summary_df$layer)) == 1 & multilayer) {
            cli::cli_alert_warning(
                "Note: The `summary_df` provided only has one unique layer, so layer will be set to FALSE."
            )
            multilayer <- FALSE
            summary_df <- summary_df[, -which(colnames(summary_df) == "layer")]
        }
    }
    ######################

    ######################
    # organize possible id variables
    ids <- intersect(c("actor", "time", "layer"), names(summary_df))
    ######################

    ######################
    # subset data to specific stats
    # if specified by the user
    if (!is.null(specific_stats)) {
        # check if user specified stats are in the data
        oops <- specific_stats[!specific_stats %in% names(summary_df)]

        # if length oops not zero, then print error
        if (length(oops) > 0) {
            cli::cli_alert_danger(
                paste0(
                    "Error: The following specified statistics are not in the data: ",
                    paste(oops, collapse = ", "), "."
                )
            )
            stop()
        }

        # if present then subset
        summary_df <- summary_df[, c(ids, specific_stats)]
    }
    ######################

    ######################
    # subset data to specific actors
    # if specified by the user
    if (!is.null(specific_actors)) {
        # check if user specified actors are in the data
        oops <- specific_actors[!specific_actors %in% summary_df$actor]

        # if length oops not zero, then print error
        if (length(oops) > 0) {
            cli::cli_alert_danger(
                paste0(
                    "Error: The following specified actors are not in the data: ",
                    paste(oops, collapse = ", "), "."
                )
            )
            stop()
        }
    }
    ######################

    ######################
    # org data
    ggdata <- melt_df(summary_df, id = ids)
    ######################

    ######################
    # across actor case
    if (across_actor) {
        ######################
        # if across actor and specific actors provided then
        # subset data by those actors
        if (!is.null(specific_actors)) {
            # warning about behavior when across_actor is TRUE and specific actors are provided
            cli::cli_alert_warning(
                "Note: When specific actors are provided and `across_actor` is set to TRUE, the data will be subsetted to only include the specified actors."
            )

            # subset
            summary_df <- summary_df[summary_df$actor %in% specific_actors, ]
        }
        ######################

        ######################
        # if not longit then construct simple density across stats
        if (!longitudinal) {
            # change aesthetic for multilayer nets
            if (multilayer) {
                viz <- ggplot(
                    ggdata,
                    aes(x = !!sym("value"), fill = !!sym("layer"), color = !!sym("layer"))
                )
            }
            if (!multilayer) {
                viz <- ggplot(ggdata, aes(x = !!sym("value")))
            }

            # add geom_density, rug, facet by variable, and thematic elements
            viz <- viz +
                geom_density(alpha = .4) +
                geom_rug(alpha = .3) +
                labs(x = "", y = "") +
                theme_stat_netify() +
                theme(axis.text.x = element_text(angle = 0)) +
                facet_wrap(~variable, scales = "free")
        } # end across actor non longit case
        ######################

        ######################
        # if longit then use ridge densities
        if (longitudinal) {
            # convert time to factor for plotting
            ggdata$time <- factor(ggdata$time)

            # change aesthetic for multilayer nets
            if (multilayer) {
                viz <- ggplot(
                    ggdata,
                    aes(x = !!sym("value"), y = !!sym("time"), fill = !!sym("layer"), color = !!sym("layer"))
                )
            }
            if (!multilayer) {
                viz <- ggplot(ggdata, aes(x = !!sym("value"), y = !!sym("time")))
            }

            # add geom_density_ridges, jittered points, facet by variable, and thematic elements
            viz <- viz +
                ggridges::geom_density_ridges(
                    rel_min_height = 0.01,
                    jittered_points = TRUE,
                    position = ggridges::position_points_jitter(
                        width = 0.000001, height = 0
                    ),
                    point_shape = "|", point_size = 1.5,
                    alpha = 0.4
                ) +
                labs(x = "", y = "") +
                facet_wrap(~variable, scales = "free_x") +
                theme_stat_netify() +
                theme(axis.text.x = element_text(angle = 0))
        } # end across actor longit case
        ######################
    } # end across actor case
    ######################

    ######################
    # specific actor case
    if (!across_actor) {
        ######################
        # get actor count
        if (is.null(specific_actors)) {
            n_actors <- length(unique(summary_df$actor))
        } else {
            n_actors <- length(unique(specific_actors))
        }

        # set up logic for whether there are more than 9 actors
        # if so then use random colors
        plus_actors <- ifelse(length(unique(specific_actors)) > 9, TRUE, FALSE)

        # write a warning that if they have "many" actors that
        # the plot will quickly become unreadable
        if (n_actors > 25) {
            cli::cli_alert_warning(
                "Warning: Consider providing some actors to the `specific_actors` argument so that actor patterns are more legible."
            )
        }

        # throw error if there are more than 657 actors because R only has 657 colors
        if (n_actors > 657) {
            cli::cli_alert_danger(
                "Error: The `summary_df` you have provided has more than 657 actors, please subset to fewer actors using the `specific_actors` argument of this function."
            )
            stop()
        }
        ######################

        ######################
        # get complete vector of actors
        acts <- unique(summary_df$actor)
        n_tot <- length(acts)

        # modify colors to highlight selected actors
        set.seed(6886)
        if (plus_actors) {
            cols <- grDevices::colors()[sample(n_actors, replace = FALSE)]
        } else {
            cols <- RColorBrewer::brewer.pal(9, "Set1")[sample(n_actors, replace = FALSE)]
        }

        # set up color key for actors
        colKey <- rep("grey", n_tot)
        names(colKey) <- acts
        colKey[match(specific_actors, names(colKey))] <- cols

        # set up border key for actors
        borKey <- rep("grey", n_tot)
        names(borKey) <- acts
        borKey[match(specific_actors, names(borKey))] <- "black"

        # add alpha scale
        alphaKey <- rep(.5, n_tot)
        names(alphaKey) <- acts
        alphaKey[match(specific_actors, names(alphaKey))] <- 1

        #
        alphaKey2 <- rep(.4, n_tot)
        names(alphaKey2) <- acts
        alphaKey2[match(specific_actors, names(alphaKey2))] <- 1

        # add size scale
        sizeKey <- rep(1, n_tot)
        names(sizeKey) <- acts
        sizeKey[match(specific_actors, names(sizeKey))] <- 2
        ######################

        ######################
        # construct plot for non longit case
        if (!longitudinal) {
            # change aesthetic for multilayer nets
            if (multilayer) {
                viz <- ggplot(ggdata, aes(
                    x = !!sym("layer"), y = !!sym("value"),
                    fill = !!sym("actor"),
                    color = !!sym("actor"),
                    alpha = !!sym("actor"),
                    size = !!sym("actor")
                ))
            }
            if (!multilayer) {
                viz <- ggplot(ggdata, aes(
                    x = !!sym("variable"), y = !!sym("value"),
                    fill = !!sym("actor"),
                    color = !!sym("actor"),
                    alpha = !!sym("actor"),
                    size = !!sym("actor")
                ))
            }

            # add geom_quasirandom, facet by variable, and thematic elements
            if (!multilayer) {
                viz <- viz +
                    ggbeeswarm::geom_quasirandom(shape = 21) +
                    scale_fill_manual(values = colKey, breaks = specific_actors) +
                    scale_color_manual(values = borKey) +
                    scale_alpha_manual(values = alphaKey) +
                    scale_size_manual(values = sizeKey) +
                    guides(color = "none", alpha = "none", size = "none") +
                    facet_wrap(~variable, scales = "free") +
                    theme_stat_netify() +
                    theme(axis.text.x = element_blank()) +
                    labs(x = "", y = "")
            }
            if (multilayer) {
                viz <- viz +
                    ggbeeswarm::geom_quasirandom(shape = 21) +
                    scale_fill_manual(values = colKey, breaks = specific_actors) +
                    scale_color_manual(values = borKey) +
                    scale_alpha_manual(values = alphaKey) +
                    scale_size_manual(values = sizeKey) +
                    guides(color = "none", alpha = "none", size = "none") +
                    facet_wrap(~variable, scales = "free_y") +
                    theme_stat_netify() +
                    labs(x = "", y = "") +
                    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
            }
        } # end non longit case
        ######################

        ######################
        # construct plot for longit case
        if (longitudinal) {
            # line plot by actor over time across stats
            viz <- ggplot(ggdata, aes(
                x = !!sym("time"), y = !!sym("value"), group = !!sym("actor"),
                color = !!sym("actor"), alpha = !!sym("actor")
            )) +
                geom_line() +
                geom_point() +
                scale_color_manual(values = colKey, breaks = specific_actors) +
                scale_alpha_manual(values = alphaKey2) +
                guides(alpha = "none", size = "none") +
                labs(x = "", y = "") +
                theme_stat_netify()

            # change facet based on multilayer or not
            if (multilayer) {
                # add a note suggesting to users that they may want to choose
                # specific statistics by passing some to the specific_stats argument
                if (is.null(specific_stats)) {
                    cli::cli_alert_warning(
                        "Note: When plotting longitudinal data with multiple layers, consider specifying specific statistics to plot using the `specific_stats` argument."
                    )
                }

                # add facet by layer and variable
                viz <- viz + facet_wrap(layer ~ variable, scales = "free_y", nrow = 2)
            }

            # if not multilayer then just facet by variable
            if (!multilayer) {
                viz <- viz + facet_wrap(~variable, scales = "free_y")
            }
        } # end longit case
        ######################
    } # end specific actor case
    ######################

    #
    return(viz)
}
