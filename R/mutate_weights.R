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
#'   for multilayer networks, supply one name per layer.
#' @param keep_original logical. if TRUE (default), preserves the original weights
#'   as a dyadic variable. if FALSE, discards original weights to save memory.
#'
#' @return a netify object with transformed weights. the original weights are optionally
#'   preserved as a dyadic variable named `original_weight` (or
#'   `original_weight_<layer>` for each layer of a multilayer network).
#'
#' @details
#' the function handles all netify object types:
#' \itemize{
#'   \item \strong{cross-sectional}: transforms the single network matrix
#'   \item \strong{longitudinal arrays}: transforms each time slice
#'   \item \strong{longitudinal lists}: transforms each time period matrix
#'   \item \strong{multilayer networks}: transforms each layer separately
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
	layer_names <- attr(netlet, "layers")
	n_layers <- max(length(layer_names), 1L)
	is_multilayer <- n_layers > 1
	original_is_binary <- rep_len(attr(netlet, "is_binary") %||% FALSE, n_layers)

	if (!is.null(new_name) && (!is.character(new_name) || length(new_name) != n_layers)) {
		cli::cli_abort(c(
			"x" = "{.arg new_name} must be a character vector with one name per layer ({n_layers}).",
			"i" = "This network has layer{?s} {.val {layer_names}}."
		))
	}

	# per-layer weight names and labels
	weight_vec <- strsplit(as.character(weight_var), ", ", fixed = TRUE)[[1]]
	weight_vec <- rep_len(weight_vec, n_layers)
	detail_vec <- strsplit(
		as.character(attr(netlet, "detail_weight") %||% ""),
		" | ",
		fixed = TRUE
	)[[1]]
	detail_vec <- rep_len(c(detail_vec, ""), n_layers)

	mutate_slice <- function(mat) {
		mat <- mat + add_constant
		if (!is.null(transform_fn)) {
			mat <- transform_fn(mat)
		}
		mat
	}
	is_binary_slice <- function(mat) {
		all(as.vector(mat) %in% c(0, 1, NA))
	}

	# store original weights as dyadic variables
	if (keep_original && netify_type == "longit_array") {
		cli::cli_alert_info("keeping original weights for longitudinal arrays not yet implemented")
	} else if (keep_original) {
		orig_vars <- if (is_multilayer) {
			paste0("original_weight_", layer_names)
		} else {
			"original_weight"
		}
		orig_list <- if (netify_type == "cross_sec") list(raw_net) else raw_net
		orig_data <- do.call(rbind, lapply(seq_along(orig_list), function(tt) {
			mat <- orig_list[[tt]]
			pd_data <- data.frame(
				actor1 = rep(rownames(mat), ncol(mat)),
				actor2 = rep(colnames(mat), each = nrow(mat))
			)
			if (netify_type == "longit_list") {
				pd_data$time <- names(orig_list)[tt]
			}
			n_cells <- nrow(mat) * ncol(mat)
			for (ll in seq_len(n_layers)) {
				cells <- seq_len(n_cells) + (ll - 1L) * n_cells
				pd_data[[orig_vars[ll]]] <- as.vector(mat)[cells]
			}
			pd_data
		}))

		netlet <- add_dyad_vars(
			netlet,
			dyad_data = orig_data,
			actor1 = "actor1",
			actor2 = "actor2",
			time = if (netify_type == "longit_list") "time" else NULL,
			dyad_vars = orig_vars,
			dyad_vars_symmetric = rep_len(unname(as.logical(attr(netlet, "symmetric"))), n_layers)
		)
	}

	# transform each layer slice and track which layers are binary
	layer_binary <- rep(TRUE, n_layers)
	if (netify_type == "cross_sec") {
		if (is_multilayer) {
			for (ll in seq_len(n_layers)) {
				raw_net[, , ll] <- mutate_slice(raw_net[, , ll])
				layer_binary[ll] <- is_binary_slice(raw_net[, , ll])
			}
		} else {
			raw_net <- mutate_slice(raw_net)
			layer_binary <- is_binary_slice(raw_net)
		}
		netlet[] <- raw_net
	} else if (netify_type == "longit_array") {
		n_time <- dim(raw_net)[length(dim(raw_net))]
		for (tt in seq_len(n_time)) {
			if (is_multilayer) {
				for (ll in seq_len(n_layers)) {
					raw_net[, , ll, tt] <- mutate_slice(raw_net[, , ll, tt])
					layer_binary[ll] <- layer_binary[ll] && is_binary_slice(raw_net[, , ll, tt])
				}
			} else {
				raw_net[, , tt] <- mutate_slice(raw_net[, , tt])
				layer_binary <- layer_binary && is_binary_slice(raw_net[, , tt])
			}
		}
		netlet[] <- raw_net
	} else if (netify_type == "longit_list") {
		for (tt in seq_along(netlet)) {
			elem <- netlet[[tt]]
			mat <- get_raw(elem)
			if (is_multilayer) {
				for (ll in seq_len(n_layers)) {
					mat[, , ll] <- mutate_slice(mat[, , ll])
					layer_binary[ll] <- layer_binary[ll] && is_binary_slice(mat[, , ll])
				}
			} else {
				mat <- mutate_slice(mat)
				layer_binary <- layer_binary && is_binary_slice(mat)
			}
			elem[] <- mat
			netlet[[tt]] <- elem
		}
	}

	# set weight names and labels for each layer
	new_weight_vec <- new_name %||% weight_vec
	binarized <- layer_binary & !original_is_binary
	weighted <- !layer_binary & original_is_binary
	for (ll in seq_len(n_layers)) {
		if (!is.null(new_name)) {
			label <- if (layer_binary[ll] && original_is_binary[ll]) {
				"transformed binary"
			} else if (binarized[ll]) {
				"binarized"
			} else if (weighted[ll]) {
				"weighted from binary"
			} else {
				"transformed"
			}
			detail_vec[ll] <- paste0(new_name[ll], " (", label, ")")
		} else if (binarized[ll]) {
			detail_vec[ll] <- paste0(weight_vec[ll], " (binarized)")
		} else if (weighted[ll]) {
			detail_vec[ll] <- paste0(weight_vec[ll], " (weighted from binary)")
		}
	}
	if (any(binarized)) {
		cli::cli_alert_info("network has been binarized through transformation")
	}
	if (any(weighted)) {
		cli::cli_alert_info("binary network has been converted to weighted through transformation")
	}

	new_attrs <- list(
		weight = paste(new_weight_vec, collapse = ", "),
		detail_weight = paste(detail_vec, collapse = " | "),
		is_binary = layer_binary
	)
	if (!is_multilayer && !is.null(new_name)) {
		new_attrs$layers <- new_name
	}

	# apply weight attributes to the netlet and each period
	for (attr_name in names(new_attrs)) {
		attr(netlet, attr_name) <- new_attrs[[attr_name]]
	}
	if (netify_type == "longit_list") {
		for (tt in seq_along(netlet)) {
			elem <- netlet[[tt]]
			for (attr_name in names(new_attrs)) {
				attr(elem, attr_name) <- new_attrs[[attr_name]]
			}
			netlet[[tt]] <- elem
		}
	}

	return(netlet)
}
