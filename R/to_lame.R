#' Convert a netify object to the format expected by `lame::ame()`
#'
#' `netify_to_lame()` (also available as `to_lame()`) is a thin
#' specialization of `\link{netify_to_amen}` for the optional `lame`
#' workflow. `lame::ame()` accepts the same
#' `y` / `xdyad` / `xrow` / `xcol` skeleton as `amen::ame()` but adds
#' two things netify users care about:
#'
#' \itemize{
#' \item rectangular `y` for **bipartite** networks via
#' `mode = "bipartite"` -- the standard `amen::ame()` rejects this.
#' \item ragged longitudinal panels via `lame::lame()`, which aligns
#' per-period actor sets by name -- `amen` requires constant actor
#' composition.
#' }
#'
#' this function wraps `\link{netify_to_amen}` and:
#' \enumerate{
#' \item picks a `family` default appropriate to the netify
#' (`"binary"` for binary nets, `"normal"` for weighted),
#' \item suggests the correct `mode` argument for `lame::ame()`
#' (`"unipartite"` / `"bipartite"`),
#' \item for longitudinal output, emits a copy-paste `lame::lame()`
#' call in the returned `ame_call` slot. with `lame = TRUE` the
#' per-period lists are passed directly; with `lame = FALSE` the
#' snippet first splits the `[n, n, t]` arrays into the per-period
#' lists that `lame::lame()` expects.
#' }
#'
#' @param netlet a netify object.
#' @param lame logical. as in \code{\link{netify_to_amen}}: pass
#' `TRUE` when actor composition varies over time. default `FALSE`.
#' @param family optional character. ame family to use. if `NULL`
#' (default), inferred from the netify: `"binary"` for binary
#' networks, `"normal"` for weighted, with a once-per-session info
#' message naming the choice.
#' @param pad logical. when `lame = TRUE` and the per-period list
#' is returned (the function never actually pads in place; it
#' always returns a list), controls whether the once-per-session
#' info message points users at the `lame::lame()` call
#' baked into `nl$ame_call`. default
#' `TRUE` (emit the message); set `FALSE` to silence it.
#' @param fit_method one of `"gibbs"` (default -- mcmc posterior via
#' `lame::ame()` / `lame::lame()`) or `"als"` (fast alternating-
#' least-squares point estimate via `lame::ame_als()`). for
#' bipartite + binary networks where mcmc is slow, als with
#' `bootstrap > 0` gives a fast point estimate plus parametric/block
#' bootstrap uncertainty intervals in a single call. the choice
#' only affects the generated `ame_call` snippet -- the `y`/`xdyad`/
#' `xrow`/`xcol` payload is identical.
#' @param bootstrap integer. number of bootstrap replicates for als
#' uncertainty intervals. ignored when `fit_method = "gibbs"` (mcmc
#' draws are the uncertainty representation there). default `0`
#' (no bootstrap). pass `200` for a reasonable interval estimate.
#'
#' @return a list with the same shape as \code{\link{netify_to_amen}}
#' output, plus two helpful extras:
#' \describe{
#' \item{`mode`}{character: `"unipartite"` or `"bipartite"`. pass
#' this directly to `lame::ame(mode = .)`.}
#' \item{`family`}{character: the suggested family (`"binary"` or
#' `"normal"`). pass to `lame::ame(family = .)`.}
#' \item{`ame_call`}{character: a literal `lame::ame()` call
#' (cross-sectional) or `lame::lame()` snippet (longitudinal)
#' the user can copy-paste.}
#' }
#'
#' @details
#' \strong{bipartite + binary: the case `to_amen()` cannot fit.}
#' for a bipartite weighted-binary netify (e.g., person x event
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
#' \strong{ragged longitudinal panels.} when `lame = TRUE` and
#' actors enter / exit the network, this function:
#' \enumerate{
#' \item builds the per-period list via `netify_to_amen(lame=TRUE)`,
#' \item bakes a `lame::lame()` call into `nl$ame_call` that passes
#' the per-period lists directly; `lame::lame()` aligns actors
#' across periods by name, for unipartite and bipartite networks.
#' }
#' the returned `Y` / `Xdyad` / `Xrow` / `Xcol` are always the
#' per-period list -- `pad` only controls whether the once-per-
#' session info message reminding the user of the snippet fires.
#'
#' @seealso \code{\link{netify_to_amen}} (the underlying converter),
#' \code{\link{netify_to_statnet}}, \code{\link{netify_to_dbn}},
#' \code{\link{netify_to_igraph}}.
#'
#' @author shahryar minhas
#'
#' @export netify_to_lame
#' @aliases to_lame
netify_to_lame <- function(
	netlet,
	lame = FALSE,
	family = NULL,
	pad = TRUE,
	fit_method = c("gibbs", "als"),
	bootstrap = 0L
) {
	netify_check(netlet)
	fit_method <- match.arg(fit_method)
	family_supplied <- !is.null(family)
	if (!is.numeric(bootstrap) || length(bootstrap) != 1L ||
		is.na(bootstrap) || !is.finite(bootstrap) || bootstrap < 0 || bootstrap != floor(bootstrap)) {
		cli::cli_abort("{.arg bootstrap} must be a non-negative integer (number of bootstrap replicates).")
	}
	bootstrap <- as.integer(bootstrap)

	obj_attrs <- attributes(netlet)
	is_bipartite <- isTRUE(obj_attrs$mode == "bipartite")
	# recurse per layer before netify_to_amen() rejects multilayer inputs
	if (length(obj_attrs$layers) > 1) {
		out <- lapply(obj_attrs$layers, function(lyr) {
			netify_to_lame(
				subset_netify(netlet, layers = lyr),
				lame = lame,
				family = if (family_supplied) family else NULL,
				pad = pad,
				fit_method = fit_method, bootstrap = bootstrap
			)
		})
		names(out) <- obj_attrs$layers
		return(out)
	}

	# detect binary matrices when the attribute is absent
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
			"i" = "{.fn to_lame}: using {.code family = {.val {family}}} ({if (is_binary) 'binary network' else 'weighted network'}). Pass {.arg family} explicitly to override (e.g., {.val ordinal}, {.val poisson})."
		),
		.frequency = "once",
		.frequency_id = "netify_to_lame_family_choice")
	}

	# build amen-shape structure
	am <- netify_to_amen(netlet, lame = lame)

	enrich_lame(am,
		mode = if (is_bipartite) "bipartite" else "unipartite",
		family = family, lame_arg = lame, pad = pad,
		fit_method = fit_method, bootstrap = bootstrap)
}

#' @keywords internal
#' @noRd
enrich_lame <- function(
	am,
	mode,
	family,
	lame_arg,
	pad,
	fit_method = "gibbs",
	bootstrap = 0L
) {
	out <- am
	out$mode <- mode
	out$family <- family

	# describe ragged longitudinal output
	if (isTRUE(lame_arg) && is.list(am$Y) && !is.array(am$Y) && isTRUE(pad)) {
		cli::cli_inform(c(
			"i" = "{.fn to_lame}: returning {length(am$Y)} per-period matrices.",
			"i" = "A ready-to-run {.code lame::lame()} call is in {.code nl$ame_call}; use {.code cat(nl$ame_call)} to print it."
		),
		.frequency = "once",
		.frequency_id = "netify_to_lame_no_pad_recipe")
	}

	# build the ame call template
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
	is_list_longit <- isTRUE(lame_arg) && is.list(am$Y) && !is.array(am$Y)
	is_array_longit <- is.array(am$Y) && length(dim(am$Y)) == 3L
	if (is_list_longit || is_array_longit) {
		# lame::lame() is the longitudinal unified entry
		method_arg <- if (use_als) ", method = \"als\"" else ", method = \"mcmc\""
		long_boot <- if (use_als && bootstrap > 0L) {
			sprintf(", bootstrap = %dL", bootstrap)
		} else ""
		mode_arg <- if (mode == "bipartite") ", mode = \"bipartite\"" else ""
		if (is_list_longit) {
			# lame::lame() aligns per-period actor sets by name
			pre_lines <- character(0)
			fit_args <- arg_block()
		} else {
			# split [n, n, t] arrays into the per-period lists lame::lame() expects
			pre_lines <- c(
				"periods <- dimnames(nl$Y)[[3]]",
				"Y <- sapply(periods, function(tt) nl$Y[, , tt], simplify = FALSE)"
			)
			fit_args <- "Y = Y"
			if (has_xdyad) {
				pre_lines <- c(pre_lines,
					"Xdyad <- sapply(periods, function(tt) array(nl$Xdyad[, , , tt], dim(nl$Xdyad)[1:3], dimnames(nl$Xdyad)[1:3]), simplify = FALSE)"
				)
				fit_args <- paste0(fit_args, ", Xdyad = Xdyad")
			}
			if (has_xrow) {
				pre_lines <- c(pre_lines,
					"Xrow <- sapply(periods, function(tt) matrix(nl$Xrow[, , tt], dim(nl$Xrow)[1], dimnames = dimnames(nl$Xrow)[1:2]), simplify = FALSE)"
				)
				fit_args <- paste0(fit_args, ", Xrow = Xrow")
			}
			if (has_xcol) {
				pre_lines <- c(pre_lines,
					"Xcol <- sapply(periods, function(tt) matrix(nl$Xcol[, , tt], dim(nl$Xcol)[1], dimnames = dimnames(nl$Xcol)[1:2]), simplify = FALSE)"
				)
				fit_args <- paste0(fit_args, ", Xcol = Xcol")
			}
		}
		out$ame_call <- paste(
			c(pre_lines,
				sprintf("lame::lame(%s%s, family = \"%s\"%s%s, nscan = 1000, burn = 500)",
					fit_args, mode_arg, family, method_arg, long_boot)
			),
			collapse = "\n"
		)
	} else if (mode == "bipartite") {
		out$ame_call <- paste(
			sprintf(
					"%s(%s, mode = \"bipartite\", family = \"%s\"%s)",
					fit_fn, arg_block(include_dyad = TRUE), family, tail_block
				)
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
#' @author cassy dorff, shahryar minhas
#'
#' @export
to_lame <- netify_to_lame
