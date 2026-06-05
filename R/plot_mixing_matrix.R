#' Visualize attribute mixing matrix results
#'
#' creates heatmap visualizations for attribute mixing matrices from \code{mixing_matrix()}.
#' the function creates a tile plot showing how different attribute categories interact in the network.
#'
#' @param mixing_results output from \code{mixing_matrix()} containing mixing matrices
#'   and summary statistics.
#' @param which_matrix integer or character. which matrix to plot if multiple are present.
#'   default is 1 (first matrix).
#' @param show_values logical. whether to display values in each tile. default TRUE.
#' @param value_digits integer. number of decimal places for displayed values. default 2.
#' @param color_scale character vector of three colors for low, mid, and high values.
#'   default uses a diverging color scale.
#' @param midpoint numeric. the midpoint for the diverging color scale. default NULL
#'   automatically calculates based on data range.
#' @param text_size numeric. size of value labels in tiles. default 4.
#' @param text_color character. color of text labels. default "black".
#' @param text_color_threshold numeric. if provided, values above this threshold (0-1 scale)
#'   will use white text, values below will use black text. default NULL uses text_color for all.
#' @param tile_border_color character. color of tile borders. default "white".
#' @param tile_border_size numeric. width of tile borders. default 0.5.
#' @param reorder_categories logical. whether to reorder categories by similarity. default FALSE.
#' @param diagonal_emphasis logical. whether to emphasize diagonal cells (within-group mixing).
#'   default TRUE.
#' @param ... additional arguments passed to ggplot2 functions.
#'
#' @return a ggplot2 object that can be further customized.
#'
#' @examples
#' \dontrun{
#' # create a network with categorical attributes
#' data(icews)
#' icews_10 <- icews[icews$year == 2010, ]
#' net <- netify(
#'     icews_10,
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # run mixing matrix analysis
#' mixing_result <- mixing_matrix(
#'     net,
#'     attribute = "i_polity2_cat"
#' )
#'
#' # create visualization
#' plot_mixing_matrix(mixing_result)
#' }
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom stats median hclust dist as.dist
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export plot_mixing_matrix

plot_mixing_matrix <- function(
	mixing_results,
	which_matrix = 1,
	show_values = TRUE,
	value_digits = 2,
	color_scale = c("#F18F01", "white", "#2E86AB"),
	midpoint = NULL,
	text_size = 4,
	text_color = "black",
	text_color_threshold = NULL,
	tile_border_color = "white",
	tile_border_size = 0.5,
	reorder_categories = FALSE,
	diagonal_emphasis = TRUE,
	...) {
	# validate mixing matrix input
	if (!is.list(mixing_results) || !"mixing_matrices" %in% names(mixing_results)) {
		cli::cli_abort(
			c(
				"x" = "mixing_results must be output from mixing_matrix().",
				"i" = "Ensure mixing_results is a list containing the 'mixing_matrices' element."
			)
		)
	}

	# extract the specified matrix based on the input `which_matrix`
	if (is.numeric(which_matrix)) {
		# check if the numeric index is within the range of available matrices
		if (which_matrix > length(mixing_results$mixing_matrices)) {
			cli::cli_abort(
				c(
					"x" = "Invalid matrix index.",
					"i" = "Only {length(mixing_results$mixing_matrices)} matrices are available."
				)
			)
		}
		matrix_data <- mixing_results$mixing_matrices[[which_matrix]]
	} else {
		# assume `which_matrix` is a valid name or key
		matrix_data <- mixing_results$mixing_matrices[[which_matrix]]
	}

	# stop execution if the specified matrix could not be extracted
	if (is.null(matrix_data)) {
		cli::cli_abort(
			c(
				"x" = "Could not extract the specified matrix.",
				"i" = "Check the value of `which_matrix` and ensure it matches an available matrix."
			)
		)
	}

	# convert the matrix to a long format
	df_long <- melt_matrix(matrix_data, remove_diagonal = FALSE, remove_zeros = FALSE, na.rm = FALSE)
	names(df_long) <- c("from", "to", "value")

	# reorder categories if requested and if the matrix is square
	if (reorder_categories && length(unique(df_long$from)) == length(unique(df_long$to))) {
		# use hierarchical clustering to determine the order of categories
		if (nrow(matrix_data) == ncol(matrix_data)) {
			dist_matrix <- as.dist(1 - matrix_data)
			hc <- hclust(dist_matrix)
			category_order <- rownames(matrix_data)[hc$order]
			df_long$from <- factor(df_long$from, levels = category_order)
			df_long$to <- factor(df_long$to, levels = category_order)
		}
	} else {
		# keep row and column order
		df_long$from <- factor(df_long$from, levels = rownames(matrix_data))
		df_long$to <- factor(df_long$to, levels = colnames(matrix_data))
	}

	# add a column to indicate whether each cell is on the diagonal
	df_long$is_diagonal <- as.character(df_long$from) == as.character(df_long$to)

	# calculate the midpoint for the color scale if it is not provided
	if (is.null(midpoint)) {
		# check if the results contain normalization information
		if (!is.null(mixing_results$summary_stats)) {
			# use the first row's stats if there are multiple time periods
			stats <- mixing_results$summary_stats[1, ]
			if ("normalized" %in% names(stats) && stats$normalized) {
				# for proportions, set the midpoint to 1 divided by the number of categories
				n_cats <- length(unique(df_long$from))
				midpoint <- 1 / n_cats
			} else {
				# for counts, use the median value as the midpoint
				midpoint <- median(df_long$value, na.rm = TRUE)
			}
		} else {
			# default to the median value if no summary stats are available
			midpoint <- median(df_long$value, na.rm = TRUE)
		}
	}

	# determine text colors for value labels
	if (!is.null(text_color_threshold)) {
		# if threshold is provided, use white text for values above the threshold
		# first normalize values to 0-1 scale for comparison
		value_range <- range(df_long$value, na.rm = TRUE)
		normalized_values <- (df_long$value - value_range[1]) / (value_range[2] - value_range[1])

		# apply threshold to determine text colors
		text_colors <- ifelse(normalized_values > text_color_threshold, "white", "black")
	} else {
		# default: use the provided text_color for all cells
		text_colors <- text_color
	}

	# create the base plot using ggplot2
	p <- ggplot(df_long, aes(x = .data$to, y = .data$from, fill = .data$value)) +
		geom_tile(color = tile_border_color, size = tile_border_size)

	# add emphasis to diagonal cells if requested
	if (diagonal_emphasis && any(df_long$is_diagonal)) {
		diag_data <- df_long[df_long$is_diagonal, ]
		p <- p +
			geom_tile(
				data = diag_data,
				aes(x = .data$to, y = .data$from),
				color = "black",
				size = tile_border_size * 2,
				fill = NA
			)
	}

	# add value labels to the plot if requested
	if (show_values) {
		p <- p +
			geom_text(
				aes(label = round(.data$value, value_digits)),
				size = text_size,
				color = text_colors
			)
	}

	# apply the color scale to the plot
	if (length(color_scale) == 3) {
		p <- p +
			scale_fill_gradient2(
				low = color_scale[1],
				mid = color_scale[2],
				high = color_scale[3],
				midpoint = midpoint,
				limits = c(
					min(df_long$value, na.rm = TRUE),
					max(df_long$value, na.rm = TRUE)
				)
			)
	} else {
		p <- p + scale_fill_gradient(low = color_scale[1], high = color_scale[2])
	}

	# determine the label for the fill legend based on the data
	fill_label <- "Count"
	if (!is.null(mixing_results$summary_stats)) {
		stats <- mixing_results$summary_stats[1, ]
		# check if the values are proportions
		max_val <- max(df_long$value, na.rm = TRUE)
		if (max_val <= 1.0 && min(df_long$value, na.rm = TRUE) >= 0) {
			fill_label <- "Proportion"
		}
	}

	# add axis labels, legend label, and theme settings to the plot
	p <- p +
		labs(
			x = "To",
			y = "From",
			fill = fill_label
		) +
		theme_minimal() +
		theme(
			axis.text.x = element_text(angle = 45, hjust = 1),
			axis.text.y = element_text(hjust = 1),
			panel.grid = element_blank(),
			aspect.ratio = 1 # keep the plot square
		)

	# add summary statistics as a subtitle if available
	if (!is.null(mixing_results$summary_stats)) {
		# use the appropriate row of stats if there are multiple time periods
		if (is.numeric(which_matrix) && nrow(mixing_results$summary_stats) >= which_matrix) {
			stats <- mixing_results$summary_stats[which_matrix, ]
		} else {
			stats <- mixing_results$summary_stats[1, ]
		}

		# construct the subtitle text from available statistics
		subtitle_parts <- c()
		if ("assortativity" %in% names(stats) && !is.na(stats$assortativity)) {
			subtitle_parts <- c(
				subtitle_parts,
				paste0("Assortativity: ", round(stats$assortativity, 3))
			)
		}
		if ("diagonal_proportion" %in% names(stats) && !is.na(stats$diagonal_proportion)) {
			subtitle_parts <- c(
				subtitle_parts,
				paste0("Diagonal proportion: ", round(stats$diagonal_proportion, 3))
			)
		}

		# add the subtitle to the plot if there are any statistics to display
		if (length(subtitle_parts) > 0) {
			subtitle_text <- paste(subtitle_parts, collapse = " | ")
			p <- p + labs(subtitle = subtitle_text)
		}
	}

	#
	return(p)
}

#' create a multi-panel mixing matrix visualization
#'
#' creates a faceted plot showing multiple mixing matrices, useful for comparing
#' patterns across time periods or network layers.
#'
#' @param mixing_results output from \code{mixing_matrix()} with multiple matrices
#' @param matrices_to_plot integer vector. which matrices to include. default NULL plots all.
#' @param ncol integer. number of columns in facet layout. default NULL auto-calculates.
#' @param shared_scale logical. whether to use the same color scale across panels. default TRUE.
#' @param ... additional arguments passed to plot_mixing_matrix for each panel
#'
#' @return a ggplot2 object with faceted mixing matrices
#'
#' @examples
#' \dontrun{
#' # create temporal network
#' data(icews)
#' net_temporal <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j",
#'     time = "year",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # run mixing matrix analysis across time
#' mixing_temporal <- mixing_matrix(
#'     net_temporal,
#'     attribute = "i_polity2_cat"
#' )
#'
#' # create faceted visualization
#' plot_mixing_matrix_facet(mixing_temporal, ncol = 2)
#' }
#'
#' @import ggplot2
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export plot_mixing_matrix_facet

plot_mixing_matrix_facet <- function(
	mixing_results,
	matrices_to_plot = NULL,
	ncol = NULL,
	shared_scale = TRUE,
	...) {
	# check if the patchwork package is available for combining plots
	# if not, stop execution and ask the user to install it
	if (!requireNamespace("patchwork", quietly = TRUE)) {
		cli::cli_abort(
			c(
				"x" = "Package 'patchwork' is required for faceted plots.",
				"!" = "Please install it using `install.packages('patchwork')`."
			)
		)
	}

	# determine which matrices to plot
	# if matrices_to_plot is not provided, default to all available matrices
	n_matrices <- length(mixing_results$mixing_matrices)
	if (is.null(matrices_to_plot)) {
		matrices_to_plot <- seq_len(n_matrices)
	}

	# validate that the indices in matrices_to_plot are within the range of available matrices
	if (any(matrices_to_plot > n_matrices)) {
		cli::cli_abort(
			c(
				"x" = "Invalid indices in `matrices_to_plot`.",
				"!" = "Indices exceed the number of available matrices ({n_matrices})."
			)
		)
	}

	# initialize variables for shared scale limits and midpoint
	# these will be used if shared_scale is enabled
	common_limits <- NULL
	common_midpoint <- NULL

	if (shared_scale) {
		# extract all values from the selected matrices
		# this is done to calculate common scale limits and midpoint
		all_values <- unlist(lapply(matrices_to_plot, function(i) {
			as.vector(mixing_results$mixing_matrices[[i]])
		}), use.names = FALSE)

		# remove any na values from the extracted values
		all_values <- all_values[!is.na(all_values)]

		# calculate the range and median of the values if there are any
		if (length(all_values) > 0) {
			common_limits <- range(all_values)
			common_midpoint <- median(all_values)
		} else {
			cli::cli_alert_warning(
				"No valid values found in the selected matrices for shared scale calculation."
			)
		}
	}

	# create an empty list to store individual plots
	plot_list <- vector("list", length(matrices_to_plot))

	# loop through each matrix index in matrices_to_plot
	for (idx in seq_along(matrices_to_plot)) {
		i <- matrices_to_plot[idx]

		# create a plot for the current matrix
		# if shared_scale is enabled and a common midpoint is available, use it
		if (shared_scale && !is.null(common_midpoint)) {
			p <- plot_mixing_matrix(
				mixing_results,
				which_matrix = i,
				midpoint = common_midpoint,
				...
			)
		} else {
			p <- plot_mixing_matrix(mixing_results, which_matrix = i, ...)
		}

		# add a title to the plot based on the matrix name or index
		matrix_names <- names(mixing_results$mixing_matrices)
		if (!is.null(matrix_names) && length(matrix_names) >= i) {
			p <- p + labs(title = matrix_names[i])
		} else {
			p <- p + labs(title = paste("Matrix", i))
		}

		# apply common scale limits if shared_scale is enabled
		if (shared_scale && !is.null(common_limits)) {
			# extract the current fill scale from the plot
			current_fill <- p$scales$scales[[which(sapply(p$scales$scales, function(x) "fill" %in% x$aesthetics))]]

			# if the current fill scale is continuous, reapply it with common limits
			if (!is.null(current_fill) && inherits(current_fill, "ScaleContinuous")) {
				# get the color scale from the original plot
				color_scale <- c(
					current_fill$palette(0),
					current_fill$palette(0.5),
					current_fill$palette(1)
				)

				# reapply the fill scale with the common limits and midpoint
				p <- p + scale_fill_gradient2(
					low = color_scale[1],
					mid = color_scale[2],
					high = color_scale[3],
					midpoint = common_midpoint,
					limits = common_limits
				)
			}
		}

		# store the plot in the plot list
		plot_list[[idx]] <- p
	}

	# determine the number of columns for the combined plot layout
	# if ncol is not provided, calculate it based on the number of plots
	if (is.null(ncol)) {
		ncol <- ceiling(sqrt(length(plot_list)))
		cli::cli_alert_info(
			"Number of columns for the combined plot layout set to {ncol}."
		)
	}

	# combine all individual plots into a single layout using patchwork
	assert_dependency("patchwork")
	combined_plot <- patchwork::wrap_plots(plot_list, ncol = ncol)

	#
	return(combined_plot)
}
