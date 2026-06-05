#' Plot method for `netify_comparison` objects
#'
#' renders a pairwise similarity heatmap from the matrix output of
#' `compare_networks()`. picks an appropriate matrix based on the
#' `method` the comparison was run with (correlation / qap / jaccard /
#' hamming / spectral). for qap comparisons, the plot renders the qap
#' correlation matrix. p-values remain available in
#' \code{x$significance_tests$qap_pvalues}.
#'
#' @param x a `netify_comparison` object returned by
#' `compare_networks()`.
#' @param ... additional arguments:
#' \describe{
#' \item{`metric`}{which matrix to render. default: auto-pick the
#' primary metric for the comparison's method. pass
#' \code{"correlation"} / \code{"jaccard"} / \code{"hamming"} /
#' \code{"qap"} / \code{"spectral"} to override.}
#' \item{`show_values`}{logical. overlay numeric values on each
#' tile. default `TRUE` when n <= 6, else `FALSE`.}
#' \item{`palette`}{two-color gradient endpoints. default
#' `c("#f0f0f0", "#1f78b4")` for correlation-like metrics.}
#' }
#'
#' @return a `ggplot` object.
#'
#' @method plot netify_comparison
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
plot.netify_comparison <- function(x, ...) {
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		cli::cli_abort("{.pkg ggplot2} is required for plot.netify_comparison.")
	}
	dots <- list(...)

	# locate a similarity matrix to render
	candidates <- list(
		correlation = x$correlations,
		jaccard     = x$jaccard,
		hamming     = x$hamming,
		spectral    = x$spectral,
		qap         = x$significance_tests$qap_correlations
	)
	# pick the first non-null one if user didn't specify
	chosen <- dots$metric %||% names(candidates)[
		vapply(candidates, function(m) !is.null(m) && is.matrix(m), logical(1))][1]
	if (is.null(chosen) || is.na(chosen)) {
		cli::cli_abort(c(
			"x" = "No similarity matrix found in the comparison object.",
			"i" = "Make sure {.fn compare_networks} was called with {.code return_details = TRUE}."
		))
	}
	mat <- candidates[[chosen]]
	if (is.null(mat) || !is.matrix(mat)) {
		cli::cli_abort(c(
			"x" = "{.arg metric} {.val {chosen}} not present in the comparison object.",
			"i" = "Available: {.val {names(candidates)[!vapply(candidates, is.null, logical(1))]}}."
		))
	}

	# melt to long format
	rn <- rownames(mat) %||% as.character(seq_len(nrow(mat)))
	cn <- colnames(mat) %||% as.character(seq_len(ncol(mat)))
	long_df <- data.frame(
		row = factor(rep(rn, times = ncol(mat)), levels = rn),
		col = factor(rep(cn, each = nrow(mat)), levels = cn),
		value = as.numeric(mat),
		stringsAsFactors = FALSE
	)

	show_values <- dots$show_values %||% (nrow(mat) <= 6)
	palette <- dots$palette %||% c("#f0f0f0", "#1f78b4")

	p <- ggplot2::ggplot(long_df, ggplot2::aes(x = .data$col, y = .data$row,
		fill = .data$value)) +
		ggplot2::geom_tile(color = "white", linewidth = 0.3) +
		ggplot2::scale_fill_gradient(
			low = palette[1], high = palette[2], na.value = "grey90"
		) +
		ggplot2::scale_y_discrete(limits = rev(rn)) +
		ggplot2::labs(x = NULL, y = NULL, fill = chosen) +
		ggplot2::coord_fixed()
	if (isTRUE(show_values)) {
		p <- p + ggplot2::geom_text(
			ggplot2::aes(label = sprintf("%.2f", .data$value)),
			size = 3, color = "black", na.rm = TRUE
		)
	}
	p + theme_publication_netify_ts(base_size = 10) +
		ggplot2::theme(
			axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
		)
}
