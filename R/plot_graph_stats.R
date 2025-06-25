#' Visualize network-level statistics
#'
#' `plot_graph_stats` creates line or bar plots to visualize network-level
#' statistics across multiple networks, time points, or layers. This function
#' is designed to work with output from `summary_net()` or similar functions
#' that produce network-level summary statistics.
#'
#' @param summary_df A data frame containing network-level statistics, typically
#'   from `summary_net()`. Must include a "net" column identifying each network
#'   or time point. May include a "layer" column for multilayer networks. All
#'   other columns should contain numeric statistics to plot.
#' @param type Character string specifying the plot type. Options are:
#'   \itemize{
#'     \item \code{"line"}: Line plot with points (default). Best for temporal data
#'     \item \code{"bar"}: Bar plot with grouped bars. Required for multilayer
#'       non-temporal data
#'   }
#' @param specific_stats Character vector of statistic names to plot. If NULL
#'   (default), plots all numeric columns in summary_df. Must match column names
#'   exactly.
#'
#' @return A ggplot object displaying the specified statistics. The plot structure
#'   depends on the data:
#'   \itemize{
#'     \item \strong{Single time/network}: Returns error (single row not plottable)
#'     \item \strong{Multiple times/networks}: Line or bar plot faceted by statistic
#'     \item \strong{Multilayer temporal}: Line plots colored by layer
#'     \item \strong{Multilayer non-temporal}: Grouped bar plots by layer
#'   }
#'
#'   All plots are faceted by statistic with free y-axis scales for better
#'   comparison across different value ranges.
#'
#' @details
#' \strong{Data structure detection:}
#'
#' The function automatically detects the structure of your data:
#' \itemize{
#'   \item \strong{Longitudinal}: Multiple unique values in "net" column
#'   \item \strong{Multilayer}: Multiple unique values in "layer" column
#'   \item \strong{Single network}: Only one row (returns error with suggestion)
#' }
#'
#' \strong{Plot type selection:}
#' \itemize{
#'   \item Line plots are preferred for temporal data to show trends
#'   \item Bar plots are automatically selected for multilayer non-temporal data
#'   \item Bar plots can be useful for comparing discrete time points
#' }
#'
#' \strong{Faceting behavior:}
#'
#' Each statistic gets its own facet panel with:
#' \itemize{
#'   \item Independent y-axis scales (scales = "free_y")
#'   \item Shared x-axis across all panels
#'   \item Automatic layout based on number of statistics
#' }
#'
#' @note
#' The function requires at least two networks/time points to create a meaningful
#' plot. For single network summaries, consider using a table format instead.
#'
#'
#' @author Ha Eun Choi, Cassy Dorff, Shahryar Minhas
#'
#' @export plot_graph_stats

plot_graph_stats <- function(
    summary_df,
    type = "line",
    specific_stats = NULL) {
    ######################
    # check to make sure type is bar or line
    if (type != "line" & type != "bar") {
        cli::cli_alert_danger(
            'Error: The `type` argument must be either "line" or "bar".'
        )
        stop()
    }
    ######################

    ######################
    # determine if summary_df is summarizing multiple layers
    multilayer <- FALSE
    layer_present <- "layer" %in% colnames(summary_df)
    if (layer_present) {
        if (length(unique(summary_df$layer)) == 1) {
            summary_df <- summary_df[, -which(colnames(summary_df) == "layer")]
        } else {
            multilayer <- TRUE
        }
    }
    ######################

    ######################
    # determine if summary_df is summarizing multiple time points
    if (length(unique(summary_df$net)) == 1) {
        longitudinal <- FALSE
        summary_df <- summary_df[, -which(colnames(summary_df) == "net")]
    } else {
        longitudinal <- TRUE
    }
    ######################

    ######################
    # check if data has more than one row
    if (!multilayer & !longitudinal) {
        cli::cli_alert_danger(
            "Error: The `summary_df` provided only has one row of data.
			Consider an alternative way of depicting this information such as a table."
        )
        print(summary_df)
        stop()
    }
    ######################

    ######################
    # note to user that if multilayer but not longit then function
    # will default to bar plot
    if (multilayer & !longitudinal & type == "line") {
        cli::cli_alert_warning(
            "Note: The data provided is summarizing multiple layers but not multiple time points.
      The function will default to a bar plot."
        )
        type <- "bar"
    }
    ######################

    ######################
    # organize possible id variables
    ids <- intersect(c("net", "layer"), names(summary_df))
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
    # reshape data for ggplot
    ggdata <- melt_df(summary_df, id = ids)
    ######################

    ######################
    # define aesthetics for multilayer but not longit
    if (multilayer & !longitudinal) {
        viz <- ggplot(
            data = ggdata,
            aes(
                x = !!sym("layer"),
                y = !!sym("value"),
                group = !!sym("variable")
            )
        )
    }

    # define aesthetics for longit but not multilayer
    if (!multilayer & longitudinal) {
        viz <- ggplot(
            data = ggdata,
            aes(
                x = !!sym("net"),
                y = !!sym("value"),
                group = !!sym("variable")
            )
        )
    }

    # define aesthetics for multilayer and longit
    if (multilayer & longitudinal) {
        viz <- ggplot(
            data = ggdata,
            aes(
                x = !!sym("net"),
                y = !!sym("value"),
                group = !!sym("layer"),
                color = !!sym("layer"),
                fill = !!sym("layer")
            )
        )
    }

    # choose geom based on user input
    if (type == "line") {
        viz <- viz + geom_line() + geom_point()
    }
    if (type == "bar") {
        viz <- viz + geom_bar(stat = "identity", position = position_dodge())
    }

    # add facets
    viz <- viz +
        facet_wrap(~variable, scales = "free_y") +
        labs(x = "", y = "") +
        theme_stat_netify()

    # if multilayer and not longit
    # then make x axis labels horizontal
    if (multilayer & !longitudinal) {
        viz <- viz + theme(axis.text.x = element_text(angle = 0, hjust = .5))
    }
    ######################

    ######################
    return(viz)
    ######################
}
