#' Bootstrap any user-supplied function of a netify object
#'
#' resamples actors with replacement (snijders & borgatti 1999 vertex
#' bootstrap) per panel, rebuilds the netlet on each draw, applies
#' the user-supplied `fn`, and returns the per-draw values plus
#' percentile confidence intervals.
#'
#' this is the general bootstrap interface for any scalar or named numeric
#' vector summary of a netify. it is useful for homophily, mixing-matrix,
#' centrality, and downstream fit summaries (e.g., the coefficient of a
#' `to_igraph` -> `igraph::cluster_*` -> modularity pipeline).
#'
#' @param netlet a `netify` object.
#' @param fn function. takes a netlet, returns a single numeric value
#' or a named numeric vector. called once per bootstrap draw.
#' @param n_boot integer. number of bootstrap replicates (default
#' `200`).
#' @param alpha numeric in (0, 1). two-sided percentile-ci alpha
#' (default `0.05` -> 95% intervals).
#' @param seed optional integer. if supplied, sets a local rng seed and
#' restores the user's global stream afterward. if `NULL`, bootstrap draws
#' use and advance the current rng stream normally.
#' @param verbose logical. if `TRUE` (default), print a progress
#' ticker every 50 draws.
#'
#' @return a `data.frame` with one row per element of `fn(netlet)`
#' and columns:
#' \describe{
#' \item{`metric`}{name of the output element (or `"value"` for
#' scalar fn).}
#' \item{`point`}{point estimate from `fn(netlet)` on the
#' original (un-resampled) netlet.}
#' \item{`n_valid`}{number of non-`na` bootstrap draws used for that metric.}
#' \item{`mean`}{bootstrap mean.}
#' \item{`sd`}{bootstrap standard deviation.}
#' \item{`lower`, `upper`}{lower / upper percentile ci bounds.}
#' }
#' the full per-draw matrix is stashed as
#' `attr(out, "bootstrap_draws")` for callers who want the empirical
#' distribution (e.g., for kernel-density plots).
#'
#' @details
#' resampling is **vertex-level**, not edge-level: actors are
#' sampled with replacement and the netlet's adjacency is sliced
#' accordingly. for longitudinal netlets, the same resampled index
#' set is applied to every period (preserves within-actor
#' dependence). multilayer netlets are unsupported -- bootstrap each
#' layer independently via `subset_netify(layers = ...)` first.
#' the resampled netlets contain the resampled adjacency only. statistics
#' that require attached nodal or dyadic attributes should recreate those
#' attributes inside `fn`, or use a NULL-model workflow such as
#' `compare_to_null(..., model = "dyad_permutation")` when the goal is an
#' attribute-aware randomization.
#'
#' @section parallel execution:
#' `bootstrap_netlet` runs serially. when `n_boot > 50` and the netlet
#' is large enough that each draw is non-trivial (rule of thumb:
#' n > ~1000 with the default centrality-heavy `fn`), parallelism
#' helps. there is no built-in `parallel =` argument; instead drive
#' the loop yourself with `future.apply::future_lapply()`, which
#' respects whatever `future::plan()` the caller set:
#' \preformatted{
#' library(future); library(future.apply)
#' plan(multisession)        # or multicore on linux/macos
#' draws <- future_lapply(1:n_boot, function(b) {
#'   idx <- sample(actors, length(actors), replace = TRUE)
#'   fn(subset_netify(net, actors = idx))
#' }, future.seed = TRUE)
#' }
#' then summarize `draws` exactly as `bootstrap_netlet` does
#' internally. wrapping the inner body of `bootstrap_netlet` in a
#' future-aware loop is a small refactor and is the recommended path
#' for ~15k-node weekly snapshots where the serial pass would tie up
#' a single core for hours.
#'
#' @examples
#' \donttest{
#' data(icews)
#' net <- netify(icews[icews$year == 2010, ],
#' actor1 = "i", actor2 = "j", symmetric = FALSE, weight = "verbCoop")
#' # bootstrap a custom scalar: mean(closeness)
#' my_stat <- function(net) {
#' sa <- summary_actor(net, stats = "all")
#' c(mean_closeness = mean(sa$closeness_all, na.rm = TRUE))
#' }
#' boot_out <- bootstrap_netlet(net, fn = my_stat, n_boot = 100, seed = 1)
#' boot_out
#' }
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export bootstrap_netlet
bootstrap_netlet <- function(netlet, fn, n_boot = 200L, alpha = 0.05,
							 seed = NULL, verbose = TRUE) {
	netify_check(netlet)
	if (!is.function(fn)) {
		cli::cli_abort("{.arg fn} must be a function taking a netify and returning a scalar or named numeric vector.")
	}
	if (!is.numeric(n_boot) || length(n_boot) != 1L ||
		is.na(n_boot) || !is.finite(n_boot) || n_boot < 2 || n_boot != floor(n_boot)) {
		cli::cli_abort("{.arg n_boot} must be a single integer >= 2.")
	}
	n_boot <- as.integer(n_boot)
	if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
		cli::cli_abort("{.arg alpha} must be in (0, 1).")
	}

	# multilayer is unsupported
	if (length(attr(netlet, "layers")) > 1) {
		cli::cli_abort(c(
			"x" = "{.fn bootstrap_netlet} doesn't yet support multilayer netlets.",
			"i" = "Subset to one layer first: {.code subset_netify(net, layers = '<lyr>') |> bootstrap_netlet(fn)}."
		))
	}

	# isolate rng from the user's global stream only for explicit seeds
	with_local_seed(seed, {

	# point estimate on the original netlet
	point_vec <- tryCatch(fn(netlet),
		error = function(e) {
			cli::cli_abort(c(
				"x" = "{.arg fn} failed on the original netlet: {conditionMessage(e)}",
				"i" = "Bootstrap aborted before the resample loop began. Fix {.arg fn} on the input first."
			))
		})
	if (!is.numeric(point_vec)) {
		cli::cli_abort("{.arg fn} must return numeric; got {.cls {class(point_vec)[1]}}.")
	}
	if (length(point_vec) == 0L) {
		cli::cli_abort("{.arg fn} must return at least one numeric value.")
	}
	if (any(!is.finite(point_vec))) {
		cli::cli_abort("{.arg fn} must return finite numeric values on the original netlet.")
	}
	metric_names <- names(point_vec) %||% paste0("v", seq_along(point_vec))
	has_named_metrics <- !is.null(names(point_vec))
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

	# pull raw matrices for index-based resampling
	obj_attrs <- attributes(netlet)
	netlet_type <- obj_attrs$netify_type
	sym  <- if (length(obj_attrs$symmetric) > 1) obj_attrs$symmetric[1] else obj_attrs$symmetric
	ibin <- if (length(obj_attrs$is_binary) > 1) obj_attrs$is_binary[1] else obj_attrs$is_binary
	wlab <- obj_attrs$weight

	raw_list <- switch(netlet_type,
		"cross_sec" = list("1" = get_raw(netlet)),
		"longit_array" = {
			arr <- get_raw(netlet)
			setNames(
				lapply(seq_len(dim(arr)[3]), function(t) arr[, , t]),
				dimnames(arr)[[3]] %||% as.character(seq_len(dim(arr)[3]))
			)
		},
		"longit_list" = setNames(lapply(netlet, get_raw), names(netlet))
	)
	first <- raw_list[[1]]
	if (!is.matrix(first)) {
		cli::cli_abort("Bootstrap currently supports square cross-sec / longit netlets only (got {class(first)[1]}).")
	}
	if (obj_attrs$mode == "bipartite" || nrow(first) != ncol(first)) {
		cli::cli_abort(c(
			"x" = "{.fn bootstrap_netlet} currently supports square unipartite netlets only.",
			"i" = "Bipartite vertex bootstrap requires separate row/column-mode resampling and is not implemented."
		))
	}
	n <- nrow(first)
	actors <- rownames(first) %||% paste0("a", seq_len(n))

	# resample and rebuild loop
	draws <- matrix(NA_real_, nrow = n_boot, ncol = length(point_vec))
	colnames(draws) <- metric_names
	first_err <- NULL
	n_err <- 0L
	n_ragged <- 0L
	for (b in seq_len(n_boot)) {
		idx <- sample.int(n, n, replace = TRUE)
		synth <- paste0("b", seq_len(n))
		# resampled matrices keyed like raw_list
		resamp_list <- lapply(raw_list, function(mat) {
			m <- mat[idx, idx, drop = FALSE]
			rownames(m) <- colnames(m) <- synth
			m
		})
		# rebuild netlet with original topology
		if (netlet_type == "cross_sec") {
			boot_net <- suppressMessages(suppressWarnings(
				new_netify(resamp_list[[1]], mode = obj_attrs$mode,
					symmetric = sym, is_binary = ibin, weight = wlab,
					diag_to_NA = isTRUE(obj_attrs$diag_to_NA),
					missing_to_zero = isTRUE(obj_attrs$missing_to_zero))
			))
		} else {
			boot_net <- suppressMessages(suppressWarnings(
				new_netify(resamp_list, mode = obj_attrs$mode,
					symmetric = sym, is_binary = ibin, weight = wlab,
					diag_to_NA = isTRUE(obj_attrs$diag_to_NA),
					missing_to_zero = isTRUE(obj_attrs$missing_to_zero))
			))
		}
		val_result <- tryCatch(
			list(value = fn(boot_net), error = NULL),
			error = function(e) list(value = NULL, error = e)
		)
		if (is.null(first_err) && !is.null(val_result$error)) {
			first_err <- val_result$error
		}
		val <- val_result$value
		if (is.null(val)) {
			n_err <- n_err + 1L
		} else {
			aligned_val <- align_stat_vector(val)
			if (is.null(aligned_val)) {
				n_ragged <- n_ragged + 1L
			} else {
				if (any(!is.finite(aligned_val))) {
					n_ragged <- n_ragged + 1L
				} else {
					draws[b, ] <- aligned_val
				}
			}
		}
		if (verbose && b %% 50L == 0L) {
			cli::cli_alert_info("bootstrap draw {b}/{n_boot}")
		}
	}
	n_dropped <- n_err + n_ragged
	if (n_dropped > 0L) {
		msg <- c(
			"!" = "{n_dropped} of {n_boot} bootstrap draw{?s} dropped ({n_err} errored, {n_ragged} returned non-conforming length/type)."
		)
		if (!is.null(first_err)) {
			msg <- c(msg, "i" = "First error: {conditionMessage(first_err)}")
		}
		if (n_dropped > n_boot / 2L) {
			msg <- c(msg, "x" = "More than half of draws failed; CIs are unreliable.")
		}
		cli::cli_warn(msg)
	}
	valid_counts <- colSums(is.finite(draws))
	if (any(valid_counts == 0L)) {
		empty_metrics <- metric_names[valid_counts == 0L]
		cli::cli_abort(c(
			"x" = "No valid bootstrap draws were available for metric{?s} {.val {empty_metrics}}.",
			"i" = "Check that {.arg fn} works on resampled netlets, or use a statistic that does not require attributes removed by actor resampling."
		))
	}
	if (any(valid_counts < n_boot)) {
		low_metrics <- metric_names[valid_counts < n_boot]
		cli::cli_warn(c(
			"!" = "Some bootstrap draws returned {.val NA} for metric{?s} {.val {low_metrics}}.",
			"i" = "Use the {.field n_valid} column to see the effective bootstrap size for each metric."
		))
	}

	low_q  <- alpha / 2
	high_q <- 1 - alpha / 2
	out <- data.frame(
		metric = metric_names,
		point  = as.numeric(point_vec),
		n_valid = as.integer(valid_counts),
		mean   = vapply(seq_along(point_vec), function(j) mean(draws[, j], na.rm = TRUE), numeric(1)),
		sd     = vapply(seq_along(point_vec), function(j) stats::sd(draws[, j], na.rm = TRUE), numeric(1)),
		lower  = vapply(seq_along(point_vec), function(j) stats::quantile(draws[, j], low_q, na.rm = TRUE), numeric(1)),
		upper  = vapply(seq_along(point_vec), function(j) stats::quantile(draws[, j], high_q, na.rm = TRUE), numeric(1)),
		stringsAsFactors = FALSE
	)
	attr(out, "bootstrap_draws") <- draws
	attr(out, "n_boot") <- n_boot
	attr(out, "alpha") <- alpha
	out
	})
}
