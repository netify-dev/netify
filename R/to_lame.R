#' Convert a netify object to the format expected by `lame::ame()`
#'
#' `netify_to_lame()` (also available as `to_lame()`) is a thin
#' specialization of `\link{netify_to_amen}` for the sibling `lame`
#' package, which is not on CRAN. Install from GitHub:
#' \code{remotes::install_github("netify-dev/lame")}.
#' `lame::ame()` accepts the same
#' `Y` / `Xdyad` / `Xrow` / `Xcol` skeleton as `amen::ame()` but adds
#' two things netify users care about:
#'
#' \itemize{
#' \item rectangular `Y` for **bipartite** networks via
#' `mode = "bipartite"` — the standard `amen::ame()` rejects this.
#' \item ragged longitudinal panels via `lame::list_to_array()` →
#' `lame::ame_rep_*` — `amen` requires constant actor composition.
#' }
#'
#' This function wraps `\link{netify_to_amen}` and:
#' \enumerate{
#' \item picks a `family` default appropriate to the netify
#' (`"binary"` for binary nets, `"normal"` for weighted),
#' \item suggests the correct `mode` argument for `lame::ame()`
#' (`"unipartite"` / `"bipartite"`),
#' \item for `lame = TRUE` longitudinal output, emits a copy-paste
#' `lame::list_to_array()` + `lame::lame()` snippet in the
#' returned `ame_call` slot. The function itself does **not**
#' pad the list — run the snippet (or the literal call) to do
#' that step yourself.
#' }
#'
#' @param netlet A netify object.
#' @param lame Logical. As in \code{\link{netify_to_amen}}: pass
#' `TRUE` when actor composition varies over time. Default `FALSE`.
#' @param family Optional character. AME family to use. If `NULL`
#' (default), inferred from the netify: `"binary"` for binary
#' networks, `"normal"` for weighted, with a once-per-session info
#' message naming the choice.
#' @param pad Logical. When `lame = TRUE` and the per-period list
#' is returned (the function never actually pads in place; it
#' always returns a list), controls whether the once-per-session
#' info message points users at the `lame::list_to_array()` +
#' `lame::lame()` snippet baked into `nl$ame_call`. Default
#' `TRUE` (emit the message); set `FALSE` to silence it.
#' @param fit_method One of `"gibbs"` (default — MCMC posterior via
#' `lame::ame()` / `lame::lame()`) or `"als"` (fast alternating-
#' least-squares point estimate via `lame::ame_als()`). For
#' bipartite + binary networks where MCMC is slow, ALS with
#' `bootstrap > 0` gives a fast point estimate plus parametric/block
#' bootstrap uncertainty intervals in a single call. The choice
#' only affects the generated `ame_call` snippet — the `Y`/`Xdyad`/
#' `Xrow`/`Xcol` payload is identical.
#' @param bootstrap Integer. Number of bootstrap replicates for ALS
#' uncertainty intervals. Ignored when `fit_method = "gibbs"` (MCMC
#' draws ARE the uncertainty representation there). Default `0`
#' (no bootstrap). Pass `200` for a reasonable interval estimate.
#'
#' @return A list with the same shape as \code{\link{netify_to_amen}}
#' output, plus two helpful extras:
#' \describe{
#' \item{`mode`}{Character: `"unipartite"` or `"bipartite"`. Pass
#' this directly to `lame::ame(mode = .)`.}
#' \item{`family`}{Character: the suggested family (`"binary"` or
#' `"normal"`). Pass to `lame::ame(family = .)`.}
#' \item{`ame_call`}{Character: a literal `lame::ame()` call
#' string the user can copy-paste.}
#' }
#'
#' @details
#' \strong{Bipartite + binary: the case `to_amen()` cannot fit.}
#' For a bipartite weighted-binary netify (e.g., person × event
#' attendance, 0/1), this is the exact pipeline:
#' \preformatted{
#' bp <- netify(df, actor1 = "person", actor2 = "event",
#' mode = "bipartite", weight = "attended")
#' nl <- to_lame(bp) # auto-detects binary
#' fit <- lame::ame(
#' Y = nl$Y, Xrow = nl$Xrow, Xcol = nl$Xcol,
#' mode = nl$mode, family = nl$family,
#' nscan = 1000, burn = 500
#' )
#' # uncertainty: posterior intervals are in fit$BETA / fit$VC
#' }
#'
#' \strong{Ragged longitudinal panels.} When `lame = TRUE` and
#' actors enter / exit the network, this function:
#' \enumerate{
#' \item builds the per-period list via `netify_to_amen(lame=TRUE)`,
#' \item bakes a `lame::list_to_array(actors = U, Y = nl$Y, ...)`
#' pad-then-fit snippet into `nl$ame_call` that the user runs
#' to materialize the 3D `[n, n, T]` array and fit `lame::lame()`.
#' }
#' The returned `Y` / `Xdyad` / `Xrow` / `Xcol` are always the
#' per-period list — `pad` only controls whether the once-per-
#' session info message reminding the user of the snippet fires.
#'
#' @seealso \code{\link{netify_to_amen}} (the underlying converter),
#' \code{\link{netify_to_statnet}}, \code{\link{netify_to_dbn}},
#' \code{\link{netify_to_igraph}}.
#'
#' @author Shahryar Minhas
#'
#' @export netify_to_lame
#' @aliases to_lame
netify_to_lame <- function(netlet, lame = FALSE, family = NULL,
						   pad = TRUE,
						   fit_method = c("gibbs", "als"),
						   bootstrap = 0L) {
	netify_check(netlet)
	fit_method <- match.arg(fit_method)
	if (!is.numeric(bootstrap) || length(bootstrap) != 1L || bootstrap < 0) {
		cli::cli_abort("{.arg bootstrap} must be a non-negative integer (number of bootstrap replicates).")
	}
	bootstrap <- as.integer(bootstrap)

	obj_attrs <- attributes(netlet)
	is_bipartite <- isTRUE(obj_attrs$mode == "bipartite")
	# derive is_binary lazily for matrix-built objects
	ib <- obj_attrs$is_binary
	if (is.null(ib) || length(ib) == 0) {
		raw <- get_raw(netlet)
		v <- if (is.list(raw)) {
			unlist(lapply(raw, as.numeric), use.names = FALSE)
		} else {
			as.numeric(raw)
		}
		v <- v[!is.na(v)]
		ib <- length(v) == 0 || all(v %in% c(0, 1))
	}
	if (length(ib) > 1) ib <- ib[1]
	is_binary <- isTRUE(ib)

	# auto-pick family from binary vs weighted
	if (is.null(family)) {
		family <- if (is_binary) "binary" else "normal"
		cli::cli_inform(c(
			"i" = "{.fn to_lame}: using {.code family = {.val {family}}} ({if (is_binary) 'binary network' else 'weighted network'}). Pass {.arg family} explicitly to override (e.g., {.val ord}, {.val tobit}, {.val rrl})."
		),
		.frequency = "once",
		.frequency_id = "netify_to_lame_family_choice")
	}

	# build amen-shape structure
	am <- netify_to_amen(netlet, lame = lame)

	# recurse per layer for multilayer longitudinal
	if (length(obj_attrs$layers) > 1 && is.list(am) && is.null(am$Y)) {
		out <- lapply(names(am), function(lyr) {
			a <- am[[lyr]]
			enrich_lame(a, mode = if (is_bipartite) "bipartite" else "unipartite",
				family = family, lame_arg = lame, pad = pad,
				fit_method = fit_method, bootstrap = bootstrap)
		})
		names(out) <- names(am)
		return(out)
	}

	enrich_lame(am,
		mode = if (is_bipartite) "bipartite" else "unipartite",
		family = family, lame_arg = lame, pad = pad,
		fit_method = fit_method, bootstrap = bootstrap)
}

#' @keywords internal
#' @noRd
enrich_lame <- function(am, mode, family, lame_arg, pad,
						fit_method = "gibbs", bootstrap = 0L) {
	out <- am
	out$mode <- mode
	out$family <- family

	# point users to lame::list_to_array for ragged longitudinal output
	if (isTRUE(lame_arg) && is.list(am$Y) && !is.array(am$Y) && isTRUE(pad)) {
		cli::cli_inform(c(
			"i" = "{.fn to_lame}: returning {length(am$Y)} per-period matrices. A ready-to-run {.code lame::list_to_array()} + {.code lame::lame()} snippet is in {.code nl$ame_call} -- {.code cat(nl$ame_call)} to print it.",
			"i" = "Install {.pkg lame} from {.url https://github.com/netify-dev/lame} if you don't have it."
		),
		.frequency = "once",
		.frequency_id = "netify_to_lame_no_pad_recipe")
	}

	# build a copy-pasteable ame_call string
	has_xrow <- !is.null(out$Xrow)
	has_xcol <- !is.null(out$Xcol)
	has_xdyad <- !is.null(out$Xdyad)
	arg_block <- function(include_dyad = TRUE) {
		bits <- "Y = nl$Y"
		if (include_dyad && has_xdyad) bits <- c(bits, "Xdyad = nl$Xdyad")
		if (has_xrow) bits <- c(bits, "Xrow = nl$Xrow")
		if (has_xcol) bits <- c(bits, "Xcol = nl$Xcol")
		paste(bits, collapse = ", ")
	}
	# pick fit function and bootstrap block per fit_method
	use_als <- identical(fit_method, "als")
	fit_fn <- if (use_als) "lame::ame_als" else "lame::ame"
	boot_block <- if (use_als && bootstrap > 0L) {
		sprintf(", bootstrap = %dL", bootstrap)
	} else ""
	# mcmc settings vs als settings
	tail_block <- if (use_als) {
		sprintf("%s, max_iter = 200", boot_block)
	} else {
		ifelse(bootstrap > 0L,
			", nscan = 1000, burn = 500",
			", nscan = 1000, burn = 500")
	}
	if (mode == "bipartite") {
		out$ame_call <- sprintf(
			"%s(%s, mode = \"bipartite\", family = \"%s\"%s)",
			fit_fn, arg_block(include_dyad = FALSE), family, tail_block
		)
	} else if (isTRUE(lame_arg) && is.list(am$Y)) {
		# emit a single list_to_array pad followed by a lame() fit
		lta_args <- c("actors = U", "Y = nl$Y")
		if (has_xdyad) lta_args <- c(lta_args, "Xdyad = nl$Xdyad")
		if (has_xrow)  lta_args <- c(lta_args, "Xrow = nl$Xrow")
		if (has_xcol)  lta_args <- c(lta_args, "Xcol = nl$Xcol")
		pad_lines <- c(
			"U <- unique(unlist(lapply(nl$Y, rownames)))",
			sprintf("padded <- lame::list_to_array(%s)", paste(lta_args, collapse = ", "))
		)
		fit_args <- "Y = padded$Y"
		if (has_xdyad) fit_args <- paste0(fit_args, ", Xdyad = padded$Xdyad")
		if (has_xrow)  fit_args <- paste0(fit_args, ", Xrow = padded$Xrow")
		if (has_xcol)  fit_args <- paste0(fit_args, ", Xcol = padded$Xcol")
		# lame::lame() is the longitudinal unified entry
		method_arg <- if (use_als) ", method = \"als\"" else ", method = \"mcmc\""
		long_boot <- if (use_als && bootstrap > 0L) {
			sprintf(", bootstrap = %dL", bootstrap)
		} else ""
		out$ame_call <- paste(
			c(pad_lines,
				sprintf("lame::lame(%s, family = \"%s\"%s%s, nscan = 1000, burn = 500)",
					fit_args, family, method_arg, long_boot)
			),
			collapse = "\n"
		)
	} else {
		out$ame_call <- sprintf(
			"%s(%s, family = \"%s\"%s)",
			fit_fn, arg_block(), family, tail_block
		)
	}
	out$fit_method <- fit_method
	out$bootstrap <- bootstrap
	out
}

#' @rdname netify_to_lame
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
to_lame <- netify_to_lame
