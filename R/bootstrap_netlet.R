#' Bootstrap any user-supplied function of a netify object
#'
#' Resamples actors with replacement (Snijders & Borgatti 1999 vertex
#' bootstrap) per panel, rebuilds the netlet on each draw, applies
#' the user-supplied `fn`, and returns the per-draw values plus
#' percentile confidence intervals.
#'
#' This generalizes the per-graph-stat bootstrap that
#' `summary(net, bootstrap = TRUE)` runs internally: any scalar / named
#' numeric vector summary of a netify can be wrapped. Useful for
#' homophily / mixing-matrix / centrality stats that don't expose
#' their own bootstrap, and for downstream fit summaries (e.g., the
#' coefficient of a `to_igraph` -> `igraph::cluster_*` -> modularity
#' pipeline).
#'
#' @param netlet A `netify` object.
#' @param fn Function. Takes a netlet, returns a single numeric value
#' or a named numeric vector. Called once per bootstrap draw.
#' @param n_boot Integer. Number of bootstrap replicates (default
#' `200`).
#' @param alpha Numeric in (0, 1). Two-sided percentile-CI alpha
#' (default `0.05` → 95% intervals).
#' @param seed Optional integer. If supplied, sets a local RNG seed
#' without leaking into the user's global stream.
#' @param verbose Logical. If `TRUE` (default), print a progress
#' ticker every 50 draws.
#'
#' @return A `data.frame` with one row per element of `fn(netlet)`
#' and columns:
#' \describe{
#' \item{`metric`}{Name of the output element (or `"value"` for
#' scalar fn).}
#' \item{`point`}{Point estimate from `fn(netlet)` on the
#' original (un-resampled) netlet.}
#' \item{`mean`}{Bootstrap mean.}
#' \item{`sd`}{Bootstrap standard deviation.}
#' \item{`lower`, `upper`}{Lower / upper percentile CI bounds.}
#' }
#' The full per-draw matrix is stashed as
#' `attr(out, "bootstrap_draws")` for callers who want the empirical
#' distribution (e.g., for kernel-density plots).
#'
#' @details
#' Resampling is **vertex-level**, not edge-level: actors are
#' sampled with replacement and the netlet's adjacency is sliced
#' accordingly. For longitudinal netlets, the same resampled index
#' set is applied to every period (preserves within-actor
#' dependence). Multilayer netlets are unsupported — bootstrap each
#' layer independently via `subset_netify(layers = ...)` first.
#'
#' @section Parallel execution:
#' `bootstrap_netlet` runs serially. When `n_boot > 50` and the netlet
#' is large enough that each draw is non-trivial (rule of thumb:
#' N > ~1000 with the default centrality-heavy `fn`), parallelism
#' helps. There is no built-in `parallel =` argument; instead drive
#' the loop yourself with `future.apply::future_lapply()`, which
#' respects whatever `future::plan()` the caller set:
#' \preformatted{
#' library(future); library(future.apply)
#' plan(multisession)        # or multicore on Linux/macOS
#' draws <- future_lapply(1:n_boot, function(b) {
#'   idx <- sample(actors, length(actors), replace = TRUE)
#'   fn(subset_netify(net, actors = idx))
#' }, future.seed = TRUE)
#' }
#' Then summarize `draws` exactly as `bootstrap_netlet` does
#' internally. Wrapping the inner body of `bootstrap_netlet` in a
#' future-aware loop is a small refactor and is the recommended path
#' for ~15K-node weekly snapshots where the serial pass would tie up
#' a single core for hours.
#'
#' @examples
#' \dontrun{
#' data(icews)
#' net <- netify(icews[icews$year == 2010, ],
#' actor1 = "i", actor2 = "j", symmetric = FALSE, weight = "verbCoop")
#' # bootstrap a custom scalar: mean(closeness)
#' my_stat <- function(net) {
#' sa <- summary_actor(net, stats = "closeness")
#' c(mean_closeness = mean(sa$closeness_all, na.rm = TRUE))
#' }
#' boot_out <- bootstrap_netlet(net, fn = my_stat, n_boot = 100, seed = 1)
#' boot_out
#' }
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export bootstrap_netlet
bootstrap_netlet <- function(netlet, fn, n_boot = 200L, alpha = 0.05,
							 seed = NULL, verbose = TRUE) {
	netify_check(netlet)
	if (!is.function(fn)) {
		cli::cli_abort("{.arg fn} must be a function taking a netify and returning a scalar or named numeric vector.")
	}
	if (!is.numeric(n_boot) || length(n_boot) != 1L || n_boot < 2) {
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

	# isolate RNG from the user's global stream
	restore_rng <- save_rng_state()
	on.exit(restore_rng(), add = TRUE)
	if (!is.null(seed)) set.seed(seed)

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
	metric_names <- names(point_vec) %||% paste0("v", seq_along(point_vec))

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
			lapply(seq_len(dim(arr)[3]), function(t) arr[, , t]) |>
				setNames(dimnames(arr)[[3]] %||% as.character(seq_len(dim(arr)[3])))
		},
		"longit_list" = lapply(netlet, get_raw) |> setNames(names(netlet))
	)
	first <- raw_list[[1]]
	if (!is.matrix(first)) {
		cli::cli_abort("Bootstrap currently supports square cross-sec / longit netlets only (got {class(first)[1]}).")
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
					symmetric = sym, is_binary = ibin, weight = wlab)
			))
		} else {
			boot_net <- suppressMessages(suppressWarnings(
				new_netify(resamp_list, mode = obj_attrs$mode,
					symmetric = sym, is_binary = ibin, weight = wlab)
			))
		}
		val <- tryCatch(fn(boot_net),
			error = function(e) { if (is.null(first_err)) first_err <<- e; NULL })
		if (is.null(val)) {
			n_err <- n_err + 1L
		} else if (!is.numeric(val) || length(val) != length(point_vec)) {
			n_ragged <- n_ragged + 1L
		} else {
			draws[b, ] <- val
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
			cli::cli_warn(msg)
		} else {
			cli::cli_inform(msg)
		}
	}

	low_q  <- alpha / 2
	high_q <- 1 - alpha / 2
	out <- data.frame(
		metric = metric_names,
		point  = as.numeric(point_vec),
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
}
