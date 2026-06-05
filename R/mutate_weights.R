#' Mutate edge weights in a netify object
#'
#' `mutate_weights` applies mathematical transformations to edge weights in a netify object.
#' this is useful for normalizing data, handling skewed distributions, creating binary networks,
#' or applying any custom mathematical transformation to network weights.
#'
#' @param netlet a netify object with edge weights to transform
#' @param transform_fn a function to apply to the weights. can be any function that takes
#'   a matrix and returns a matrix (e.g., `log`, `sqrt`, `function(x) x^2`). if NULL,
#'   only the `add_constant` operation is performed.
#' @param add_constant numeric value to add to weights before applying `transform_fn`.
#'   useful for log transformations (e.g., `add_constant = 1` for `log(x + 1)`) or
#'   shifting distributions.
#' @param new_name optional new name for the weight variable. if provided, updates
#'   the weight attribute and descriptive labels. if NULL, keeps the original name.
#' @param keep_original logical. if TRUE (default), preserves the original weights
#'   as a dyadic variable. if FALSE, discards original weights to save memory.
#'
#' @return a netify object with transformed weights. the original weights are optionally
#'   preserved as a dyadic variable named "original_weight".
#'
#' @details
#' the function handles all netify object types:
#' \itemize{
#'   \item \strong{cross-sectional}: transforms the single network matrix
#'   \item \strong{longitudinal arrays}: transforms each time slice
#'   \item \strong{longitudinal lists}: transforms each time period matrix
#' }
#'
#' the function automatically updates network attributes:
#' \itemize{
#'   \item updates `is_binary` if transformation results in 0/1 values
#'   \item updates `detail_weight` with transformation description
#'   \item preserves all other network and nodal attributes
#' }
#'
#' for longitudinal arrays, original weight preservation is not yet implemented
#' and will show an informational message.
#'
#' @examples
#' # load example data
#' data(icews)
#' icews_2010 <- icews[icews$year == 2010, ]
#'
#' # create a weighted network
#' net <- netify(
#'     icews_2010,
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # example 1: log transformation (common for skewed data)
#' net_log <- mutate_weights(
#'     net,
#'     transform_fn = log,
#'     add_constant = 1, # log(x + 1) to handle zeros
#'     new_name = "log_verbCoop"
#' )
#' print(net_log)
#'
#' # example 2: square root transformation (moderate skewness)
#' net_sqrt <- mutate_weights(
#'     net,
#'     transform_fn = sqrt,
#'     new_name = "sqrt_verbCoop"
#' )
#'
#' # example 3: binarization (convert to presence/absence)
#' net_binary <- mutate_weights(
#'     net,
#'     transform_fn = function(x) ifelse(x > 0, 1, 0),
#'     new_name = "verbCoop_binary"
#' )
#'
#' # example 4: standardization (z-scores)
#' net_std <- mutate_weights(
#'     net,
#'     transform_fn = function(x) {
#'         mean_x <- mean(x, na.rm = TRUE)
#'         sd_x <- sd(x, na.rm = TRUE)
#'         return((x - mean_x) / sd_x)
#'     },
#'     new_name = "verbCoop_standardized"
#' )
#'
#' # example 5: rank transformation
#' net_rank <- mutate_weights(
#'     net,
#'     transform_fn = function(x) rank(x, na.last = "keep"),
#'     new_name = "verbCoop_ranked"
#' )
#'
#' # example 6: power transformation
#' net_power <- mutate_weights(
#'     net,
#'     transform_fn = function(x) x^0.5, # square root as power
#'     new_name = "verbCoop_power"
#' )
#'
#' # example 7: min-max normalization (scale to 0-1)
#' net_norm <- mutate_weights(
#'     net,
#'     transform_fn = function(x) {
#'         min_x <- min(x, na.rm = TRUE)
#'         max_x <- max(x, na.rm = TRUE)
#'         return((x - min_x) / (max_x - min_x))
#'     },
#'     new_name = "verbCoop_normalized"
#' )
#'
#' # example 8: winsorization (cap extreme values)
#' net_winsor <- mutate_weights(
#'     net,
#'     transform_fn = function(x) {
#'         q95 <- quantile(x, 0.95, na.rm = TRUE)
#'         return(pmin(x, q95)) # cap at 95th percentile
#'     },
#'     new_name = "verbCoop_winsorized"
#' )
#'
#' # example 9: only add constant (no transformation function)
#' net_shifted <- mutate_weights(
#'     net,
#'     add_constant = 10,
#'     new_name = "verbCoop_shifted"
#' )
#'
#' # example 10: don't keep original weights to save memory
#' net_log_compact <- mutate_weights(
#'     net,
#'     transform_fn = log1p, # log(1 + x), handles zeros automatically
#'     new_name = "log1p_verbCoop",
#'     keep_original = FALSE
#' )
#'
#' # example 11: longitudinal network transformation
#' \donttest{
#' # create longitudinal network
#' net_longit <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE,
#'     weight = "verbCoop",
#'     actor_time_uniform = FALSE
#' )
#'
#' # transform across all time periods
#' net_longit_log <- mutate_weights(
#'     net_longit,
#'     transform_fn = log1p,
#'     new_name = "log_verbCoop"
#' )
#' }
#'
#' # example 12: custom transformation with multiple operations
#' net_custom <- mutate_weights(
#'     net,
#'     transform_fn = function(x) {
#'         # complex transformation: log, then standardize
#'         x_log <- log(x + 1)
#'         x_std <- (x_log - mean(x_log, na.rm = TRUE)) / sd(x_log, na.rm = TRUE)
#'         return(x_std)
#'     },
#'     new_name = "verbCoop_log_std"
#' )
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export mutate_weights

mutate_weights <- function(
	netlet, transform_fn = NULL,
	add_constant = 0, new_name = NULL,
	keep_original = TRUE) {
	netify_check(netlet)
	weight_var <- attr(netlet, "weight", exact = TRUE)

	# default weight name for binary networks
	if (is.null(weight_var)) {
		weight_var <- "edge_value"
		attr(netlet, "weight") <- weight_var
	}

	raw_net <- get_raw(netlet)
	netify_type <- attr(netlet, "netify_type")
	original_is_binary <- attr(netlet, "is_binary")

	if (netify_type == "cross_sec") {
		if (keep_original) {
			orig_data <- data.frame(
				actor1 = rep(rownames(raw_net), ncol(raw_net)),
				actor2 = rep(colnames(raw_net), each = nrow(raw_net)),
				original_weight = as.vector(raw_net)
			)

			netlet <- add_dyad_vars(
				netlet,
				dyad_data = orig_data,
				actor1 = "actor1",
				actor2 = "actor2",
				dyad_vars = "original_weight",
				dyad_vars_symmetric = all(attr(netlet, "symmetric"))
			)
		}

		raw_net <- raw_net + add_constant
		if (!is.null(transform_fn)) {
			raw_net <- transform_fn(raw_net)
		}

		new_is_binary <- all(as.vector(raw_net) %in% c(0, 1, NA))

		netlet[, ] <- raw_net
		attr(netlet, "is_binary") <- new_is_binary
	} else if (netify_type == "longit_array") {
		if (keep_original) {
			cli::cli_alert_info("keeping original weights for longitudinal arrays not yet implemented")
		}

		bin_check <- logical(dim(raw_net)[3])

		for (t in 1:dim(raw_net)[3]) {
			slice <- raw_net[, , t] + add_constant
			if (!is.null(transform_fn)) {
				raw_net[, , t] <- transform_fn(slice)
			}
			bin_check[t] <- all(as.vector(raw_net[, , t]) %in% c(0, 1, NA))
		}
		netlet[, , ] <- raw_net

		attr(netlet, "is_binary") <- all(bin_check)
	} else if (netify_type == "longit_list") {
		bin_check <- logical(length(netlet))

		for (i in seq_along(netlet)) {
			if (keep_original) {
				mat <- netlet[[i]]
				orig_data <- data.frame(
					actor1 = rep(rownames(mat), ncol(mat)),
					actor2 = rep(colnames(mat), each = nrow(mat)),
					time = names(netlet)[i],
					original_weight = as.vector(mat)
				)

				netlet <- add_dyad_vars(
					netlet,
					dyad_data = orig_data,
					actor1 = "actor1",
					actor2 = "actor2",
					time = "time",
					dyad_vars = "original_weight",
					dyad_vars_symmetric = all(attr(netlet, "symmetric"))
				)
			}

			mat <- get_raw(netlet[[i]]) + add_constant
			if (!is.null(transform_fn)) {
				mat <- transform_fn(mat)
			}

			bin_check[i] <- all(as.vector(mat) %in% c(0, 1, NA))

			# preserve matrix attributes across assignment
			old_attrs <- attributes(netlet[[i]])
			netlet[[i]][, ] <- mat

			for (a in names(old_attrs)) {
				if (!(a %in% c("dim", "dimnames"))) {
					attr(netlet[[i]], a) <- old_attrs[[a]]
				}
			}

			attr(netlet[[i]], "is_binary") <- bin_check[i]
		}

		attr(netlet, "is_binary") <- all(bin_check)
	}

	if (!is.null(new_name)) {
		attr(netlet, "weight") <- new_name

		if (attr(netlet, "is_binary")) {
			if (original_is_binary) {
				attr(netlet, "detail_weight") <- paste0(new_name, " (transformed binary)")
			} else {
				attr(netlet, "detail_weight") <- paste0(new_name, " (binarized)")
				cli::cli_alert_info("network has been binarized through transformation")
			}
		} else {
			if (original_is_binary) {
				attr(netlet, "detail_weight") <- paste0(new_name, " (weighted from binary)")
				cli::cli_alert_info("binary network has been converted to weighted through transformation")
			} else {
				attr(netlet, "detail_weight") <- paste0(new_name, " (transformed)")
			}
		}
	} else {
		if (attr(netlet, "is_binary") != original_is_binary) {
			current_weight <- attr(netlet, "weight", exact = TRUE) %||% "weight"
			if (attr(netlet, "is_binary")) {
				attr(netlet, "detail_weight") <- paste0(current_weight, " (binarized)")
				cli::cli_alert_info("network has been binarized through transformation")
			} else {
				attr(netlet, "detail_weight") <- paste0(current_weight, " (weighted from binary)")
				cli::cli_alert_info("binary network has been converted to weighted through transformation")
			}
		}
	}

	return(netlet)
}
