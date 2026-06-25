#' Test an observed network statistic against a NULL distribution
#'
#' simulates `n_sim` networks from a NULL model (default erdos-renyi
#' at the observed density), applies the user-supplied statistic to
#' each, and reports how the observed statistic compares -- point
#' estimate, percentile of the NULL distribution, and two-sided monte carlo
#' p-value against the selected NULL model.
#'
#' combines `simulate.netify()` + a vectorized `fn` loop into the
#' single-call entry users actually want: "is my observed transitivity
#' surprising vs. a random graph?"
#'
#' @param netlet the observed netify object.
#' @param fn function. takes a **netify object** (not an igraph or
#' matrix) and returns a single numeric value or a named numeric
#' vector. called once on the observed netlet and `n_sim` times on the
#' simulated draws. wrap igraph helpers with `to_igraph()` inside
#' `fn`, e.g. `fn = function(net) igraph::transitivity(to_igraph(net))`.
#' @param n_sim integer. number of NULL-model draws (default `200`).
#' @param model character. NULL-model family; passed to
#' `simulate.netify()`. one of `"erdos_renyi"` (default),
#' `"configuration"`, `"dyad_permutation"`.
#'
#' **picking a NULL model.** for ergm-trained users:
#' \describe{
#' \item{`erdos_renyi`}{random graph at observed density. use when
#' the NULL hypothesis is "no structure beyond density."}
#' \item{`configuration`}{random graph preserving the observed
#' degree sequence. use when you want to ask "is my observed
#' structure surprising *given* the degree distribution?" -- already
#' conditions on degree.}
#' \item{`dyad_permutation`}{vertex-label permutation. preserves the
#' full degree distribution and weight values; use when comparing
#' attribute-aware structure (homophily, mixing) against actor
#' relabelings.}
#' }
#' for weighted netlets, the simulator draws weights from the
#' empirical non-zero weight distribution so observed-vs-NULL stats
#' are computed on comparable scales (`dyad_permutation` is exact --
#' it preserves weights via relabel).
#' @param alpha numeric in (0, 1). two-sided alpha for the percentile
#' ci of the NULL distribution (default `0.05` -> 95%).
#' @param seed optional integer. if supplied, sets a local rng seed and
#' restores the user's global stream afterward. if `NULL`, NULL draws use
#' and advance the current rng stream normally.
#' @param verbose logical. progress ticker every 50 draws.
#'
#' @return a `data.frame` (also class `"netify_null_test"`) with one
#' row per stat and columns:
#' \describe{
#' \item{`metric`}{name of the statistic.}
#' \item{`observed`}{observed value on `netlet`.}
#' \item{`n_valid`}{number of non-`na` simulated draws used for that metric.}
#' \item{`null_mean`, `null_sd`}{moments of the NULL distribution.}
#' \item{`null_lower`, `null_upper`}{percentile ci bounds.}
#' \item{`p_value`}{two-sided monte carlo p-value using the simulated NULL draws.}
#' \item{`extreme`}{logical: is observed outside `[null_lower, null_upper]`?}
#' }
#'
#' @examples
#' \donttest{
#' data(icews)
#' net <- netify(icews[icews$year == 2010, ],
#' actor1 = "i", actor2 = "j", symmetric = FALSE, weight = "verbCoop")
#' my_stat <- function(net) {
#' s = suppressMessages(summary(net))
#' c(transitivity = s$transitivity, reciprocity = s$reciprocity)
#' }
#' compare_to_null(net, my_stat, n_sim = 20, seed = 1)
#' }
#'
#' @section parallel execution:
#' `compare_to_null` runs serially. at `n_sim > 50` with large n, the
#' per-draw `fn()` cost (igraph community detection, full
#' `summary_actor`) dominates wall-clock. there is no built-in
#' `parallel =` argument; drive the loop manually with
#' `future.apply::future_lapply()`:
#' \preformatted{
#' library(future); library(future.apply)
#' plan(multisession)
#' sims <- simulate(net, nsim = n_sim, model = "erdos_renyi", seed = 1)
#' null_draws <- future_lapply(sims, fn, future.seed = TRUE)
#' }
#' summarize `null_draws` as `compare_to_null` does internally
#' (`rowmeans`, quantiles, two-sided p). for 15k-node weekly
#' snapshots with `n_sim = 500`, a 4-core `multisession` plan cuts
#' wall-clock by roughly 3.5x.
#'
#' @seealso [simulate.netify()] for the NULL-model engine,
#' [bootstrap_netlet()] for sampling-variation cis around the
#' observed stat itself.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export compare_to_null
compare_to_null <- function(netlet, fn, n_sim = 200L,
							model = c("erdos_renyi", "configuration", "dyad_permutation"),
							alpha = 0.05, seed = NULL, verbose = TRUE) {
	netify_check(netlet)
	model <- match.arg(model)
	if (!is.function(fn)) {
		cli::cli_abort("{.arg fn} must be a function.")
	}
	if (!is.numeric(n_sim) || length(n_sim) != 1L ||
		is.na(n_sim) || !is.finite(n_sim) || n_sim < 2 || n_sim != floor(n_sim)) {
		cli::cli_abort("{.arg n_sim} must be a single integer >= 2.")
	}
	n_sim <- as.integer(n_sim)
	if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
		cli::cli_abort("{.arg alpha} must be in (0, 1).")
	}

	# observed stat
	obs <- fn(netlet)
	if (!is.numeric(obs)) {
		cli::cli_abort("{.arg fn} must return numeric on the observed netlet; got {.cls {class(obs)[1]}}.")
	}
	if (length(obs) == 0L) {
		cli::cli_abort("{.arg fn} must return at least one numeric value.")
	}
	if (any(!is.finite(obs))) {
		cli::cli_abort("{.arg fn} must return finite numeric values on the observed netlet.")
	}
	metric_names <- names(obs) %||% paste0("v", seq_along(obs))
	has_named_metrics <- !is.null(names(obs))
	if (has_named_metrics && (anyNA(metric_names) || any(metric_names == "") || anyDuplicated(metric_names))) {
		cli::cli_abort("{.arg fn} must return unique, non-empty names when returning a named vector.")
	}
	align_stat_vector <- function(val) {
		if (!is.numeric(val) || length(val) != length(metric_names) || any(!is.finite(val))) return(NULL)
		if (has_named_metrics) {
			val_names <- names(val)
			if (is.null(val_names) || anyNA(val_names) || any(val_names == "") ||
				anyDuplicated(val_names) || !setequal(val_names, metric_names)) {
				return(NULL)
			}
			return(as.numeric(val[metric_names]))
		}
		as.numeric(val)
	}

	# null model draws
	sims <- simulate(netlet, nsim = n_sim, seed = seed, model = model)

	null_draws <- matrix(NA_real_, nrow = n_sim, ncol = length(obs))
	colnames(null_draws) <- metric_names
	first_err <- NULL
	n_fail <- 0L
	for (b in seq_len(n_sim)) {
		val_result <- tryCatch(
			list(value = fn(sims[[b]]), error = NULL),
			error = function(e) list(value = NULL, error = e)
		)
		if (is.null(first_err) && !is.null(val_result$error)) {
			first_err <- val_result$error
		}
		val <- val_result$value
		if (is.null(val) || length(val) != length(obs) || !is.numeric(val)) {
			n_fail <- n_fail + 1L
		} else {
			aligned_val <- align_stat_vector(val)
			if (is.null(aligned_val)) {
				n_fail <- n_fail + 1L
			} else {
			if (any(!is.finite(aligned_val))) {
				n_fail <- n_fail + 1L
			} else {
				null_draws[b, ] <- aligned_val
			}
		}
		}
		if (verbose && b %% 50L == 0L) {
			cli::cli_alert_info("null draw {b}/{n_sim}")
		}
	}
	if (n_fail > 0L) {
		msg <- c(
			"!" = "{n_fail} of {n_sim} null draw{?s} produced unusable {.arg fn} output (dropped from null distribution)."
		)
		if (!is.null(first_err)) {
			msg <- c(msg, "i" = "First error: {conditionMessage(first_err)}")
		}
		cli::cli_warn(msg)
	}
	valid_counts <- colSums(is.finite(null_draws))
	if (any(valid_counts == 0L)) {
		empty_metrics <- metric_names[valid_counts == 0L]
		cli::cli_abort(c(
			"x" = "No valid null draws were available for metric{?s} {.val {empty_metrics}}.",
			"i" = "Check that {.arg fn} works on simulated netlets for model {.val {model}}."
		))
	}
	if (any(valid_counts < n_sim)) {
		low_metrics <- metric_names[valid_counts < n_sim]
		cli::cli_warn(c(
			"!" = "Some null draws returned {.val NA} for metric{?s} {.val {low_metrics}}.",
			"i" = "Use the {.field n_valid} column to see the effective null distribution size for each metric."
		))
	}

	low_q  <- alpha / 2
	high_q <- 1 - alpha / 2
	out <- data.frame(
		metric     = metric_names,
		observed   = as.numeric(obs),
		n_valid    = as.integer(valid_counts),
		null_mean  = vapply(seq_along(obs), function(j) mean(null_draws[, j], na.rm = TRUE), numeric(1)),
		null_sd    = vapply(seq_along(obs), function(j) stats::sd(null_draws[, j], na.rm = TRUE), numeric(1)),
		null_lower = vapply(seq_along(obs), function(j) stats::quantile(null_draws[, j], low_q,  na.rm = TRUE), numeric(1)),
		null_upper = vapply(seq_along(obs), function(j) stats::quantile(null_draws[, j], high_q, na.rm = TRUE), numeric(1)),
		stringsAsFactors = FALSE
	)
	# two-sided permutation p-value with plus-one finite-simulation correction
	out$p_value <- vapply(seq_along(obs), function(j) {
		v <- null_draws[, j]
		v <- v[is.finite(v)]
		if (length(v) == 0) return(NA_real_)
		upper <- (sum(v >= obs[j]) + 1) / (length(v) + 1)
		lower <- (sum(v <= obs[j]) + 1) / (length(v) + 1)
		min(1, 2 * min(upper, lower))
	}, numeric(1))
	out$extreme <- !is.na(out$null_lower) &
		(out$observed < out$null_lower | out$observed > out$null_upper)
	class(out) <- c("netify_null_test", class(out))
	attr(out, "n_sim") <- n_sim
	attr(out, "model") <- model
	attr(out, "alpha") <- alpha
	attr(out, "null_draws") <- null_draws
	out
}
