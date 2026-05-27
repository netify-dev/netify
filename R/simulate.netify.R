#' Simulate null-model networks from a netify object
#'
#' Generates `nsim` new netify objects from one of three standard
#' null models, holding the actor set fixed (and, where applicable,
#' the observed density / degree sequence). Useful for sanity-checking
#' whether an observed network statistic (transitivity, modularity,
#' etc.) is surprising relative to a chance benchmark, without
#' reaching for `statnet::ergm` for a simple null.
#'
#' @param object A netify object (cross-sectional or per-period; for
#' longitudinal input each period is simulated independently).
#' @param nsim Integer. Number of simulated draws to return.
#' @param seed Optional integer. Local RNG seed; the user's global
#' `set.seed()` stream is left untouched.
#' @param model Character. One of:
#' \describe{
#' \item{`"erdos_renyi"`}{Independent Bernoulli edges matched
#' to the observed density (and directedness).}
#' \item{`"configuration"`}{Configuration-model rewire that
#' preserves the observed degree sequence (in/out for directed
#' inputs). Uses `igraph::sample_degseq()`.}
#' \item{`"dyad_permutation"`}{Permute dyads (Snijders-Borgatti
#' vertex relabel + symmetric reshuffle). Preserves density and,
#' conditional on permutation symmetry, degree distribution
#' shape.}
#' }
#' @param ... Passed to the underlying model implementation.
#'
#' @return A list of length `nsim` of netify objects with the same
#' class / mode / symmetry as the input.
#'
#' @importFrom stats simulate
#' @method simulate netify
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
simulate.netify <- function(object, nsim = 1L, seed = NULL,
							model = c("erdos_renyi", "configuration", "dyad_permutation"),
							...) {
	netify_check(object)
	model <- match.arg(model)
	if (!is.numeric(nsim) || length(nsim) != 1L || nsim < 1) {
		cli::cli_abort("{.arg nsim} must be a positive integer.")
	}
	nsim <- as.integer(nsim)

	restore_rng <- save_rng_state()
	on.exit(restore_rng(), add = TRUE)
	if (!is.null(seed)) set.seed(seed)

	obj_attrs <- attributes(object)
	netlet_type <- obj_attrs$netify_type
	sym  <- if (length(obj_attrs$symmetric) > 1) obj_attrs$symmetric[1] else obj_attrs$symmetric
	ibin <- if (length(obj_attrs$is_binary) > 1) obj_attrs$is_binary[1] else obj_attrs$is_binary
	wlab <- obj_attrs$weight
	sim_weighted <- !isTRUE(ibin)

	if (isTRUE(obj_attrs$mode == "bipartite")) {
		cli::cli_abort(c(
			"x" = "{.fn simulate.netify} does not support bipartite netlets.",
			"i" = "Build a unipartite projection (or use {.fn netify} with mode={.val unipartite}) before simulating."
		))
	}

	# pull per-period raw matrices (cross-sec wraps as length-1 list)
	raw_list <- switch(netlet_type,
		"cross_sec" = list("1" = get_raw(object)),
		"longit_array" = {
			arr <- get_raw(object)
			lapply(seq_len(dim(arr)[3]), function(t) arr[, , t]) |>
				setNames(dimnames(arr)[[3]] %||% as.character(seq_len(dim(arr)[3])))
		},
		"longit_list" = lapply(object, get_raw) |> setNames(names(object))
	)

	# sample_degseq wrapper that falls through alternate methods
	sample_degseq_safe <- function(...) {
		args <- list(...)
		methods_to_try <- c("fast.heur.simple", "vl", "configuration")
		last_err <- NULL
		for (m in methods_to_try) {
			res <- tryCatch(
				do.call(igraph::sample_degseq, c(args, list(method = m))),
				error = function(e) { last_err <<- e; NULL }
			)
			if (!is.null(res)) return(res)
		}
		cli::cli_abort(c(
			"x" = "Configuration model failed: {conditionMessage(last_err)}",
			"i" = "Try {.code model = 'erdos_renyi'} or {.code 'dyad_permutation'} instead."
		), call = NULL)
	}

	# simulator for a single period
	simulate_one <- function(mat) {
		actors <- rownames(mat) %||% paste0("a", seq_len(nrow(mat)))
		n <- nrow(mat)
		# density over off-diagonal non-NA cells
		off_mask <- row(mat) != col(mat)
		off_vals <- mat[off_mask]
		finite_off <- off_vals[!is.na(off_vals)]
		dens <- if (length(finite_off) > 0) mean(finite_off != 0) else 0
		# empirical weight pool
		weight_pool <- if (sim_weighted) {
			finite_off[finite_off != 0 & is.finite(finite_off)]
		} else NULL

		out <- matrix(0, n, n, dimnames = list(actors, actors))

		if (model == "erdos_renyi") {
			# bernoulli at observed density
			if (isTRUE(sym)) {
				ut <- which(upper.tri(out))
				draws <- stats::rbinom(length(ut), 1, dens)
				out[ut] <- draws
				out <- out + t(out)
			} else {
				off <- which(row(out) != col(out))
				out[off] <- stats::rbinom(length(off), 1, dens)
			}
		} else if (model == "configuration") {
			if (!requireNamespace("igraph", quietly = TRUE)) {
				cli::cli_abort("{.pkg igraph} required for configuration model.")
			}
			# binarize for degree sequence calculation
			adj <- (mat != 0) & !is.na(mat)
			if (isTRUE(sym)) {
				# undirected degree from rowSums alone
				deg <- rowSums(adj)
				g <- sample_degseq_safe(deg)
				out <- as.matrix(igraph::as_adjacency_matrix(g))
			} else {
				in_d  <- colSums(adj)
				out_d <- rowSums(adj)
				g <- sample_degseq_safe(out.deg = out_d, in.deg = in_d)
				out <- as.matrix(igraph::as_adjacency_matrix(g))
			}
			dimnames(out) <- list(actors, actors)
		} else if (model == "dyad_permutation") {
			# relabel actors to permute the dyad structure
			perm <- sample.int(n)
			out <- mat[perm, perm, drop = FALSE]
			dimnames(out) <- list(actors, actors)
		}

		# resample edge weights from the empirical pool
		if (sim_weighted && length(weight_pool) > 0 &&
			model %in% c("erdos_renyi", "configuration")) {
			if (isTRUE(sym)) {
				ut <- which(out != 0 & upper.tri(out))
				if (length(ut) > 0) {
					out[ut] <- sample(weight_pool, length(ut), replace = TRUE)
				}
				# mirror upper into lower to keep symmetry
				lt <- lower.tri(out)
				out[lt] <- 0
				out <- out + t(out) - diag(diag(out))
			} else {
				ones <- which(out != 0 & off_mask)
				if (length(ones) > 0) {
					out[ones] <- sample(weight_pool, length(ones), replace = TRUE)
				}
			}
		}

		diag(out) <- if (isTRUE(attr(object, "diag_to_NA"))) NA else 0
		out
	}

	# draw nsim independent replicates
	sim_is_binary <- if (sim_weighted) FALSE else TRUE
	sim_weight   <- if (sim_weighted) wlab else NULL
	out <- lapply(seq_len(nsim), function(k) {
		sim_list <- lapply(raw_list, simulate_one)
		# rebuild netlet matching input topology
		if (netlet_type == "cross_sec") {
			suppressMessages(suppressWarnings(
				new_netify(sim_list[[1]], mode = obj_attrs$mode,
					symmetric = sym, is_binary = sim_is_binary, weight = sim_weight)
			))
		} else {
			suppressMessages(suppressWarnings(
				new_netify(sim_list, mode = obj_attrs$mode,
					symmetric = sym, is_binary = sim_is_binary, weight = sim_weight)
			))
		}
	})
	# stamp class + metadata
	class(out) <- c("netify_sim_list", "list")
	attr(out, "model") <- model
	attr(out, "nsim") <- nsim
	attr(out, "seed") <- seed
	out
}
