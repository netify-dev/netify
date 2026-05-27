#' Test an observed network statistic against a null distribution
#'
#' Simulates `n_sim` networks from a null model (default Erdős-Rényi
#' at the observed density), applies the user-supplied statistic to
#' each, and reports how the observed statistic compares — point
#' estimate, percentile of the null distribution, and two-sided
#' permutation p-value.
#'
#' Combines `simulate.netify()` + a vectorized `fn` loop into the
#' single-call entry users actually want: "is my observed transitivity
#' surprising vs. a random graph?"
#'
#' @param netlet The observed netify object.
#' @param fn Function. Takes a **netify object** (not an igraph or
#' matrix) and returns a single numeric value or a named numeric
#' vector. Called once on the observed netlet and `n_sim` times on the
#' simulated draws. Wrap igraph helpers with `to_igraph()` inside
#' `fn`, e.g. `fn = function(net) igraph::transitivity(to_igraph(net))`.
#' @param n_sim Integer. Number of null-model draws (default `200`).
#' @param model Character. Null-model family; passed to
#' `simulate.netify()`. One of `"erdos_renyi"` (default),
#' `"configuration"`, `"dyad_permutation"`.
#'
#' **Picking a null model.** For ERGM-trained users:
#' \describe{
#' \item{`erdos_renyi`}{Random graph at observed density. Use when
#' the null hypothesis is "no structure beyond density."}
#' \item{`configuration`}{Random graph preserving the observed
#' degree sequence. Use when you want to ask "is my observed
#' structure surprising *given* the degree distribution?" — already
#' conditions on degree.}
#' \item{`dyad_permutation`}{Vertex-label permutation. Preserves the
#' full degree distribution and weight values; use when comparing
#' attribute-aware structure (homophily, mixing) against actor
#' relabelings.}
#' }
#' For weighted netlets, the simulator draws weights from the
#' empirical non-zero weight distribution so observed-vs-null stats
#' are computed on comparable scales (`dyad_permutation` is exact —
#' it preserves weights via relabel).
#' @param alpha Numeric in (0, 1). Two-sided alpha for the percentile
#' CI of the null distribution (default `0.05` → 95%).
#' @param seed Optional integer. Local RNG seed (does not leak into
#' the user's global stream).
#' @param verbose Logical. Progress ticker every 50 draws.
#'
#' @return A `data.frame` (also class `"netify_null_test"`) with one
#' row per stat and columns:
#' \describe{
#' \item{`metric`}{Name of the statistic.}
#' \item{`observed`}{Observed value on `netlet`.}
#' \item{`null_mean`, `null_sd`}{Moments of the null distribution.}
#' \item{`null_lower`, `null_upper`}{Percentile CI bounds.}
#' \item{`p_value`}{Two-sided permutation p-value.}
#' \item{`extreme`}{Logical: is observed outside `[null_lower, null_upper]`?}
#' }
#'
#' @examples
#' \dontrun{
#' data(icews)
#' net <- netify(icews[icews$year == 2010, ],
#' actor1 = "i", actor2 = "j", symmetric = FALSE, weight = "verbCoop")
#' my_stat <- function(net) {
#' s = suppressMessages(summary(net))
#' c(transitivity = s$transitivity, reciprocity = s$reciprocity)
#' }
#' compare_to_null(net, my_stat, n_sim = 200, seed = 1)
#' }
#'
#' @section Parallel execution:
#' `compare_to_null` runs serially. At `n_sim > 50` with large N, the
#' per-draw `fn()` cost (igraph community detection, full
#' `summary_actor`) dominates wall-clock. There is no built-in
#' `parallel =` argument; drive the loop manually with
#' `future.apply::future_lapply()`:
#' \preformatted{
#' library(future); library(future.apply)
#' plan(multisession)
#' sims <- simulate(net, nsim = n_sim, model = "erdos_renyi", seed = 1)
#' null_draws <- future_lapply(sims, fn, future.seed = TRUE)
#' }
#' Summarize `null_draws` as `compare_to_null` does internally
#' (`rowMeans`, quantiles, two-sided p). For 15K-node weekly
#' snapshots with `n_sim = 500`, a 4-core `multisession` plan cuts
#' wall-clock by roughly 3.5x.
#'
#' @seealso [simulate.netify()] for the null-model engine,
#' [bootstrap_netlet()] for sampling-variation CIs around the
#' observed stat itself.
#'
#' @author Cassy Dorff, Shahryar Minhas
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
	if (!is.numeric(n_sim) || length(n_sim) != 1L || n_sim < 2) {
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
	metric_names <- names(obs) %||% paste0("v", seq_along(obs))

	# null model draws
	sims <- simulate(netlet, nsim = n_sim, seed = seed, model = model)

	null_draws <- matrix(NA_real_, nrow = n_sim, ncol = length(obs))
	colnames(null_draws) <- metric_names
	first_err <- NULL
	n_fail <- 0L
	for (b in seq_len(n_sim)) {
		val <- tryCatch(fn(sims[[b]]),
			error = function(e) { if (is.null(first_err)) first_err <<- e; NULL })
		if (is.null(val) || length(val) != length(obs) || !is.numeric(val)) {
			n_fail <- n_fail + 1L
		} else {
			null_draws[b, ] <- val
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
		cli::cli_inform(msg)
	}

	low_q  <- alpha / 2
	high_q <- 1 - alpha / 2
	out <- data.frame(
		metric     = metric_names,
		observed   = as.numeric(obs),
		null_mean  = vapply(seq_along(obs), function(j) mean(null_draws[, j], na.rm = TRUE), numeric(1)),
		null_sd    = vapply(seq_along(obs), function(j) stats::sd(null_draws[, j], na.rm = TRUE), numeric(1)),
		null_lower = vapply(seq_along(obs), function(j) stats::quantile(null_draws[, j], low_q,  na.rm = TRUE), numeric(1)),
		null_upper = vapply(seq_along(obs), function(j) stats::quantile(null_draws[, j], high_q, na.rm = TRUE), numeric(1)),
		stringsAsFactors = FALSE
	)
	# two-sided permutation p-value clamped to [0, 1]
	out$p_value <- vapply(seq_along(obs), function(j) {
		v <- null_draws[, j]
		v <- v[!is.na(v)]
		if (length(v) == 0) return(NA_real_)
		min(1, 2 * min(mean(v >= obs[j]), mean(v <= obs[j])))
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
