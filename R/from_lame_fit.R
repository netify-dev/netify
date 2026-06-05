#' Convert a fitted `lame`/`amen` AME object back into a netify
#'
#' takes the posterior-mean (or point-estimate) prediction matrix
#' from a `lame::ame()` / `lame::ame_als()` / `amen::ame()` fit and
#' wraps it as a netify so the predictions can be summarized, plotted,
#' or compared to the observed netlet via `compare_networks()`.
#'
#' useful for posterior-predictive checks: build the observed netlet,
#' run an ame fit, then `from_lame_fit(fit) |> plot(style = "heatmap")`
#' to visualize the fitted intensity matrix.
#'
#' @param fit a fitted object from `lame::ame()`, `lame::ame_als()`,
#' `lame::lame()`, or `amen::ame()`. must expose a fitted-value
#' matrix (e.g., `fit$ez`, `fit$zpostmean`, or `fitted(fit)`).
#' for `value` in `"prob_lower"` / `"prob_upper"` / `"fitted_lower"`
#' / `"fitted_upper"` the fit must additionally expose a per-draw
#' array of fitted values. the slots searched, in order, are:
#' `fit$boot$ez` and `fit$boot$y_hat` (lame als parametric / block
#' bootstrap), then `fit$ez_draws`, `fit$ezps`, and `fit$z_draws`
#' (gibbs posterior draws), with shape `[n, n, b]` or `[n, n, t, b]`.
#' @param value one of `"fitted"` (default -- posterior-mean linear
#' predictor `ez`/`zpostmean`), `"residual"` (observed - fitted),
#' `"prob"` (logistic / probit -> probability scale, when the
#' family supports it), or `"prob_lower"` / `"prob_upper"` /
#' `"fitted_lower"` / `"fitted_upper"` (per-cell `alpha`/`1-alpha`
#' quantiles across bootstrap or posterior draws). when `lame`
#' exposes a `fitted()` method for the object, that's preferred.
#' for `value = "prob"` with a binary family, link detection
#' follows this priority:
#' (1) any explicit `link` slot on the fit (or `fit$control$link`);
#' (2) als class (any token containing "als") or
#'   `fit$fit_method = "als"` -> probit, matching `lame::ame_als()`;
#' (3) `lame` class -> logit, matching `lame::lame()` gibbs default;
#' (4) `ame` / `amen` class (and no `fit_method = "logit"` override)
#'   -> probit, matching `amen::ame()` gibbs convention;
#' (5) otherwise logit fallback.
#' @param symmetric logical. override the inferred symmetry; default
#' reads from the fit's stored `mode`/`y`.
#' @param alpha numeric in (0, 0.5). tail probability for the
#' `*_lower` / `*_upper` quantiles. default `0.05` -> 90% interval
#' (lower = 0.025, upper = 0.975 when interpreted as a two-sided
#' ci; here we use lower = alpha/2, upper = 1 - alpha/2).
#' @return a cross-sectional netify object whose underlying matrix is
#' the fitted-value (or residual) matrix at the same actor ordering.
#'
#' @examples
#' \dontrun{
#' lm_in <- to_lame(net, fit_method = "als", bootstrap = 200)
#' fit <- lame::ame_als(y = lm_in$y, mode = "bipartite", family = "binary",
#' bootstrap = 200)
#' # round-trip predictions back into a netify for plotting
#' pred_net <- from_lame_fit(fit, value = "prob")
#' plot(pred_net, style = "heatmap")
#' }
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export from_lame_fit
from_lame_fit <- function(fit,
						  value = c("fitted", "residual", "prob",
									"prob_lower", "prob_upper",
									"fitted_lower", "fitted_upper"),
						  symmetric = NULL,
						  alpha = 0.05) {
	value <- match.arg(value)
	if (is.null(fit) || !is.list(fit)) {
		cli::cli_abort("{.arg fit} must be a list-like fit object from lame / amen.")
	}
	original_value <- value
	is_quantile <- value %in% c("prob_lower", "prob_upper",
								"fitted_lower", "fitted_upper")
	if (is_quantile) {
		if (!is.numeric(alpha) || length(alpha) != 1L ||
			is.na(alpha) || alpha <= 0 || alpha >= 0.5) {
			cli::cli_abort("{.arg alpha} must be a single number in (0, 0.5).")
		}
	}
	# locate the fitted-value matrix
	fitted_mat <- NULL
	fitted_time_slice <- NULL
	if (methods::existsMethod("fitted", class(fit)[1]) ||
		!is.null(utils::getS3method("fitted", class(fit)[1], optional = TRUE))) {
		fitted_mat <- tryCatch(stats::fitted(fit), error = function(e) NULL)
	}
	if (is.null(fitted_mat)) {
		for (slot in c("EZ", "ZpostMean", "Y_hat", "fitted", "mu")) {
			cand <- fit[[slot]]
			if (is.null(cand)) next
			if (is.matrix(cand)) {
				fitted_mat <- cand
				break
			}
			# longitudinal fits (lame::lame) store ez as [n, n, t];
			# collapse to the first slice and let the user know.
			if (is.array(cand) && length(dim(cand)) == 3L) {
				cli::cli_inform(c(
					"i" = "{.fn from_lame_fit}: {.code fit${slot}} is a 3D array; using the first time slice ({.val {dimnames(cand)[[3]][1] %||% '1'}}) for the cross-sectional netify."
				),
				.frequency = "once",
				.frequency_id = "from_lame_fit_longit_slice")
					fitted_mat <- cand[, , 1L, drop = TRUE]
					fitted_time_slice <- 1L
					break
				}
			}
	}
	if (is.null(fitted_mat)) {
		cli::cli_abort(c(
			"x" = "Could not locate a fitted-value matrix on {.arg fit}.",
			"i" = "Expected one of {.code fitted(fit)}, {.code fit$EZ}, {.code fit$ZpostMean}, {.code fit$Y_hat}.",
			"i" = "Available slots: {.val {names(fit)}}."
		))
	}
	# locate per-draw fitted-value array for quantile values
	draws_arr <- NULL
	if (is_quantile) {
		for (path in list(c("BOOT", "EZ"), c("BOOT", "Y_hat"),
						  c("EZ_draws"), c("EZps"), c("Z_draws"))) {
			cand <- fit
			for (k in path) cand <- if (is.null(cand)) NULL else cand[[k]]
			if (!is.null(cand) && is.array(cand) &&
				length(dim(cand)) >= 3L) {
				draws_arr <- cand
				break
			}
		}
		if (is.null(draws_arr)) {
			cli::cli_abort(c(
				"x" = "Could not locate a per-draw fitted-value array on {.arg fit}.",
				"i" = "Expected one of {.code fit$BOOT$EZ}, {.code fit$BOOT$Y_hat}, {.code fit$EZ_draws}, {.code fit$EZps}.",
				"i" = "For ALS fits, re-run with {.code bootstrap = 200} (or higher); for Gibbs, ensure draws are saved (e.g. {.code odens = 1})."
			))
		}
	}
	# per-cell quantile over the trailing draw dim
	per_cell_quantile <- function(arr, q) {
		d <- dim(arr)
		nd <- length(d)
		B <- d[nd]
		flat <- matrix(arr, ncol = B)
		qvec <- apply(flat, 1, function(x) {
			if (all(is.na(x))) NA_real_ else stats::quantile(x, probs = q,
				na.rm = TRUE, names = FALSE, type = 7)
		})
		array(qvec, dim = d[-nd], dimnames = dimnames(arr)[-nd])
	}
	# replace fitted_mat with quantile matrix
	if (is_quantile) {
		q <- if (grepl("_lower$", value)) alpha / 2 else 1 - alpha / 2
		q_arr <- per_cell_quantile(draws_arr, q)
		# pick first time slice for longitudinal draws
		if (length(dim(q_arr)) == 3L) {
			q_arr <- q_arr[, , 1L, drop = TRUE]
		}
		# carry dimnames from the point estimate
		if (is.null(rownames(q_arr)) && !is.null(rownames(fitted_mat))) {
			rownames(q_arr) <- rownames(fitted_mat)
		}
		if (is.null(colnames(q_arr)) && !is.null(colnames(fitted_mat))) {
			colnames(q_arr) <- colnames(fitted_mat)
		}
		fitted_mat <- q_arr
		# route through the appropriate downstream branch
		if (value %in% c("prob_lower", "prob_upper")) {
			value <- "prob"
		} else {
			value <- "fitted"
		}
	}
	# residual = y - fitted
	if (value == "residual") {
		Y <- fit$Y %||% fit$data$Y
		if (is.null(Y)) {
			cli::cli_abort("Cannot compute residuals: {.arg fit} doesn't store the observed Y.")
		}
		if (is.array(Y) && length(dim(Y)) == 3L && !is.null(fitted_time_slice)) {
			Y <- Y[, , fitted_time_slice, drop = TRUE]
		}
		if (!identical(dim(Y), dim(fitted_mat))) {
			cli::cli_abort("Y / fitted dimensions disagree: {dim(Y)} vs {dim(fitted_mat)}.")
		}
		fitted_mat <- Y - fitted_mat
	} else if (value == "prob") {
		family <- fit$family %||% fit$control$family %||% "normal"
		# pick link by explicit slot, then class, with logit fallback
		link <- fit$link %||% fit$control$link
		if (is.null(link)) {
			cls_vec <- as.character(class(fit))
			fm <- fit$fit_method %||% ""
			is_als <- any(grepl("als", cls_vec, ignore.case = TRUE)) ||
				identical(fm, "als")
			# treat both "ame" (amen) and explicit "amen" tokens as amen-style
			is_amen <- any(cls_vec %in% c("ame", "amen")) ||
				any(grepl("amen", cls_vec, ignore.case = TRUE))
			is_lame <- any(cls_vec %in% c("lame")) ||
				any(grepl("^lame", cls_vec, ignore.case = TRUE))
			link <- if (is_als) {
				"probit"
			} else if (is_lame) {
				"logit"
			} else if (is_amen && !identical(fm, "logit")) {
				"probit"
			} else {
				"logit"
			}
		}
		if (identical(family, "binary") || identical(family, "bin")) {
			fitted_mat <- if (identical(link, "probit")) {
				stats::pnorm(fitted_mat)
			} else {
				stats::plogis(fitted_mat)
			}
		} else {
			cli::cli_warn(c(
				"!" = "{.code value = 'prob'} only meaningful for binary family; got {.val {family}}. Returning fitted on the linear-predictor scale."
			))
		}
	}
	# infer mode and symmetry
	mode_attr <- fit$mode %||% "unipartite"
	if (is.null(symmetric)) {
		symmetric <- fit$symmetric %||% FALSE
	}
	if (is.null(rownames(fitted_mat))) {
		rownames(fitted_mat) <- paste0("a", seq_len(nrow(fitted_mat)))
	}
	if (is.null(colnames(fitted_mat))) {
		colnames(fitted_mat) <- if (identical(mode_attr, "bipartite")) {
			paste0("c", seq_len(ncol(fitted_mat)))
		} else {
			rownames(fitted_mat)
		}
	}
	# weight column name keyed on original value type
	weight_name <- switch(original_value,
		"prob"          = "predicted_prob",
		"residual"      = "residual",
		"fitted"        = "fitted_value",
		"prob_lower"    = "predicted_prob_lower",
		"prob_upper"    = "predicted_prob_upper",
		"fitted_lower"  = "fitted_value_lower",
		"fitted_upper"  = "fitted_value_upper"
	)
	suppressMessages(suppressWarnings(
			new_netify(fitted_mat,
				mode = mode_attr,
				symmetric = symmetric,
				weight = weight_name,
				diag_to_NA = !identical(mode_attr, "bipartite"))
		))
	}
