#' Animate a longitudinal netify object with gganimate
#'
#' Returns a ggplot built from the per-period `plot.netify()` output,
#' with `gganimate::transition_manual()` keyed on the time variable
#' so the animation steps through each period. Requires the
#' `gganimate` package.
#'
#' For static facet plots, just call `plot(net)` on a longit netlet
#' — that defaults to faceting by time. Use `animate_netify()` for
#' single-panel transitions instead of grid faceting (better for
#' presentations / videos where the eye can focus on one period at a
#' time).
#'
#' @param netlet A longitudinal netify object (`longit_array` /
#' `longit_list`).
#' @param ... Additional arguments passed to `plot.netify()`
#' (`node_color_by`, `node_size_by`, `style`, etc.).
#' @param static_actor_positions Logical. If `TRUE` (default for
#' animation since positions jumping around between periods is
#' visually confusing), pin node positions across time.
#' @return A `gganim` object. Render with `gganimate::animate(.)` or
#' `gganimate::anim_save("file.gif", .)`.
#'
#' @examples
#' \dontrun{
#' library(gganimate)
#' anim <- animate_netify(longit_net, node_color_by = "polity")
#' anim_save("trade_anim.gif", anim, fps = 4)
#' }
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export animate_netify
animate_netify <- function(netlet, ..., static_actor_positions = TRUE) {
	netify_check(netlet)
	# require a longitudinal netlet
	if (identical(attr(netlet, "netify_type"), "cross_sec")) {
		cli::cli_abort("{.fn animate_netify} requires a longitudinal netify; got cross-sectional.")
	}
	if (!requireNamespace("gganimate", quietly = TRUE)) {
		cli::cli_abort(c(
			"x" = "{.pkg gganimate} is required for {.fn animate_netify}.",
			"i" = "Install via {.code install.packages('gganimate')}."
		))
	}
	# build the layered plot
	p <- plot(netlet, static_actor_positions = static_actor_positions, ...)
	# step through periods via the time column on plot data
	p +
		gganimate::transition_manual(.data$time) +
		ggplot2::labs(title = "Time: {current_frame}")
}
