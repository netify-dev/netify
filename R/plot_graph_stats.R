#' Visualize network-level statistics
#'
#' `plot_graph_stats` creates line or bar plots to visualize network-level
#' statistics across multiple networks, time points, or layers. this function
#' is designed to work with output from `summary()` for netify objects or
#' similar functions that produce network-level summary statistics.
#'
#' @param summary_df a data frame containing network-level statistics, typically
#'   from `summary()` for a netify object. must include a "net" column
#'   identifying each network or time point. may include a "layer" column for
#'   multilayer networks. all other columns should contain numeric statistics to
#'   plot.
#' @param type character string specifying the plot type. options are:
#'   \itemize{
#'     \item \code{"line"}: line plot with points (default). best for temporal data
#'     \item \code{"bar"}: bar plot with grouped bars. required for multilayer
#'       non-temporal data
#'   }
#' @param specific_stats character vector of statistic names to plot. if NULL
#'   (default), plots all numeric columns in summary_df. must match column names
#'   exactly.
#'
#' @return a ggplot object displaying the specified statistics. the plot structure
#'   depends on the data:
#'   \itemize{
#'     \item \strong{single time/network}: returns error (single row not plottable)
#'     \item \strong{multiple times/networks}: line or bar plot faceted by statistic
#'     \item \strong{multilayer temporal}: line plots colored by layer
#'     \item \strong{multilayer non-temporal}: grouped bar plots by layer
#'   }
#'
#'   all plots are faceted by statistic with free y-axis scales for better
#'   comparison across different value ranges.
#'
#' @details
#' \strong{data structure detection:}
#'
#' the function automatically detects the structure of your data:
#' \itemize{
#'   \item \strong{longitudinal}: multiple unique values in "net" column
#'   \item \strong{multilayer}: multiple unique values in "layer" column
#'   \item \strong{single network}: only one row (returns error with suggestion)
#' }
#'
#' \strong{plot type selection:}
#' \itemize{
#'   \item line plots are preferred for temporal data to show trends
#'   \item bar plots are automatically selected for multilayer non-temporal data
#'   \item bar plots can be useful for comparing discrete time points
#' }
#'
#' \strong{faceting behavior:}
#'
#' each statistic gets its own facet panel with:
#' \itemize{
#'   \item independent y-axis scales (scales = "free_y")
#'   \item shared x-axis across all panels
#'   \item automatic layout based on number of statistics
#' }
#'
#' @note
#' the function requires at least two networks/time points to create a meaningful
#' plot. for single network summaries, consider using a table format instead.
#'
#'
#' @author ha eun choi, cassy dorff, shahryar minhas
#'
#' @export plot_graph_stats

plot_graph_stats <- function(
	summary_df,
	type = "line",
	specific_stats = NULL) {
	####
	# check to make sure type is bar or line
	if (type != "line" & type != "bar") {
		cli::cli_abort(
			'The `type` argument must be either "line" or "bar".'
		)
	}
	####

	####
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
	####

	####
	# determine if summary_df is summarizing multiple time points
	# check if 'net' column exists first
	if ("net" %in% colnames(summary_df)) {
		if (length(unique(summary_df$net)) == 1) {
			longitudinal <- FALSE
			summary_df <- summary_df[, -which(colnames(summary_df) == "net")]
		} else {
			longitudinal <- TRUE
		}
	} else {
		# if no 'net' column, it's not longitudinal
		longitudinal <- FALSE
	}
	####

	####
	# check if data has more than one row
	if (!multilayer & !longitudinal) {
		cli::cli_abort(c(
			"The {.arg summary_df} provided only has one row of data.",
			"i" = "Consider an alternative way of depicting this information such as a table."
		))
	}
	####

	####
	# note to user that if multilayer but not longit then function
	# will default to bar plot
	if (multilayer & !longitudinal & type == "line") {
		cli::cli_alert_warning(
			"Note: The data provided is summarizing multiple layers but not multiple time points.
	  The function will default to a bar plot."
		)
		type <- "bar"
	}
	####

	####
	# organize possible id variables
	ids <- intersect(c("net", "layer"), names(summary_df))
	####

	####
	# subset data to specific stats
	# if specified by the user
	if (!is.null(specific_stats)) {
		# check if user specified stats are in the data
		oops <- specific_stats[!specific_stats %in% names(summary_df)]

		# if length oops not zero, then print error
		if (length(oops) > 0) {
			cli::cli_abort(
				paste0(
					"The following specified statistics are not in the data: ",
					paste(oops, collapse = ", "), "."
				)
			)
		}

		# if present then subset
		summary_df <- summary_df[, c(ids, specific_stats)]
	}
	####

	####
	# reshape data for ggplot
	ggdata <- melt_df(summary_df, id = ids)
	####

	####
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

	# guard against unsupported plot structures
	if (!exists("viz")) {
		cli::cli_abort(
			"Unable to determine plot configuration for the provided data."
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
	####

	####
	return(viz)
	####
}

#' plot graph-level summary statistics
#'
#' @param x a graph-level summary returned by `summary()` for a netify object.
#' @param ... additional arguments passed to [plot_graph_stats()].
#'
#' @return a ggplot object displaying graph-level summary statistics.
#'
#' @export

plot.summary.netify <- function(x, ...) {
	plot_graph_stats(x, ...)
}
