#' Binarize a netify object at a threshold
#'
#' thin wrapper around `mutate_weights()` for the very common case of
#' "dichotomize the weighted network at a cut-point." returns a new
#' netify with edge values coerced to 0/1 based on the supplied
#' threshold (or threshold function).
#'
#' @param netlet a weighted netify object.
#' @param threshold numeric scalar (default `0` -- any nonzero edge
#' becomes 1) or a function `f(x)` that takes the vector of edge
#' weights and returns a single numeric threshold (e.g.,
#' `function(x) median(x, na.rm = TRUE)` or
#' `function(x) quantile(x, 0.75, na.rm = TRUE)`).
#' @param strict logical. if `TRUE`, edges with weight strictly
#' greater than the threshold become 1; if `FALSE` (default), the
#' threshold itself is included (>=). for `threshold = 0`, the
#' default gives the "any nonzero edge counts" semantics that
#' matches the rest of the package (signed-weight density,
#' homophily-default-threshold, etc.).
#' @param abs logical. if `TRUE`, compare `|x|` to the threshold so
#' that negative-magnitude ties also count toward the binarization.
#' defaults to `FALSE`. when the network contains both positive and
#' negative weights and `abs = FALSE`, `binarize()` informs once
#' that negative ties will be dropped.
#' @param new_name optional character. new name for the binarized
#' weight column (default keeps the original name).
#' @return a binarized netify object with `is_binary = TRUE`.
#'
#' @details na cells (e.g., the diagonal under `diag_to_NA = TRUE`)
#' propagate as na in the output rather than becoming 0. use
#' `na.rm = TRUE` when summing edges if you want them treated as 0.
#' structural zeros stay zero in every branch -- a negative
#' `threshold` will not promote empty cells to 1, regardless of
#' `strict` or `abs`.
#'
#' @examples
#' \donttest{
#' data(icews)
#' net <- netify(icews[icews$year == 2010, ],
#' actor1 = "i", actor2 = "j", symmetric = FALSE, weight = "verbCoop")
#' # any-nonzero-edge dichotomization
#' bin0 <- binarize(net)
#' # 75th-percentile of nonzero weights
#' bin75 <- binarize(net, threshold = function(x) {
#' nz <- x[x > 0]
#' quantile(nz, 0.75, na.rm = TRUE)
#' })
#' }
#'
#' @seealso [mutate_weights()] for arbitrary transformations.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export binarize
binarize <- function(netlet, threshold = 0, strict = FALSE, abs = FALSE,
					 new_name = NULL) {
	netify_check(netlet)
	if (!is.numeric(threshold) && !is.function(threshold)) {
		cli::cli_abort("{.arg threshold} must be a numeric scalar or a function returning one.")
	}
	raw_obj <- get_raw(netlet)
	raw_vec <- if (is.list(raw_obj)) {
		unlist(lapply(raw_obj, as.numeric), use.names = FALSE)
	} else {
		as.numeric(raw_obj)
	}
	raw_vec <- raw_vec[!is.na(raw_vec)]
	has_neg <- length(raw_vec) > 0 && any(raw_vec < 0)
	# resolve threshold value, allowing a function or a scalar
	if (is.function(threshold)) {
		thr_val <- threshold(raw_vec)
		if (!is.numeric(thr_val) || length(thr_val) != 1L) {
			cli::cli_abort("{.arg threshold} function must return a single numeric value.")
		}
	} else {
		if (length(threshold) != 1L) {
			cli::cli_abort("{.arg threshold} must be length 1.")
		}
		thr_val <- threshold
	}
	# warn on signed data when abs is off, since negative ties collapse to 0
	if (has_neg && !isTRUE(abs) && is.finite(thr_val)) {
		cli::cli_inform(c(
			"!" = "Network has negative weights but {.arg abs} is {.code FALSE} (threshold {.val {thr_val}}).",
			"i" = "All negative ties will become 0. Pass {.code abs = TRUE} to compare against magnitudes."
		),
		.frequency = "once",
		.frequency_id = "netify_binarize_signed_threshold")
	}
	# guard on `x != 0` in every branch so structural zeros stay zero
	cmp_fn <- if (isTRUE(abs)) {
		if (isTRUE(strict)) {
			function(x) ((x != 0) & (abs(x) > thr_val)) * 1L
		} else {
			function(x) ((x != 0) & (abs(x) >= thr_val)) * 1L
		}
	} else if (isTRUE(strict)) {
		function(x) ((x != 0) & (x > thr_val)) * 1L
	} else {
		function(x) ((x != 0) & (x >= thr_val)) * 1L
	}
	out <- mutate_weights(netlet,
		transform_fn = cmp_fn,
		new_name = new_name,
		keep_original = FALSE)
	# stamp is_binary attribute
	attr(out, "is_binary") <- TRUE
	out
}
