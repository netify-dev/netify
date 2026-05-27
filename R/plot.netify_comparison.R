#' Plot method for `netify_comparison` objects
#'
#' Renders a pairwise similarity heatmap from the matrix output of
#' `compare_networks()`. Picks an appropriate matrix based on the
#' `method` the comparison was run with (correlation / qap / jaccard /
#' hamming / spectral). For QAP comparisons, the lower triangle shows
#' QAP correlations and (optionally) the upper triangle shows
#' p-values, so a single plot communicates both effect and inference.
#'
#' @param x A `netify_comparison` object returned by
#' `compare_networks()`.
#' @param ... Additional arguments:
#' \describe{
#' \item{`metric`}{Which matrix to render. Default: auto-pick the
#' primary metric for the comparison's method. Pass
#' \code{"correlation"} / \code{"jaccard"} / \code{"hamming"} /
#' \code{"qap"} / \code{"spectral"} to override.}
#' \item{`show_values`}{Logical. Overlay numeric values on each
#' tile. Default `TRUE` when n <= 6, else `FALSE`.}
#' \item{`palette`}{Two-color gradient endpoints. Default
#' `c("#f0f0f0", "#1f78b4")` for correlation-like metrics.}
#' }
#'
#' @return A `ggplot` object.
#'
#' @method plot netify_comparison
#'
#' @author Cassy Dorff, Shahryar Minhas
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
