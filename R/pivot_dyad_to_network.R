#' Pivot a dyadic variable to become the network
#'
#' `pivot_dyad_to_network` swaps a dyadic attribute with the main network in a netify object.
#' this is useful when you want to analyze a different relationship type that was stored
#' as a dyadic attribute. for example, if your network represents trade relationships
#' but you have fdi stored as a dyadic attribute, you can pivot to make fdi the main network.
#'
#' @param netlet a netify object containing the network and dyadic attributes
#' @param dyad_var character string naming the dyadic variable to become the new network.
#'   must exist in the netlet's dyad_data attribute.
#' @param make_network_dyad_var logical. if TRUE (default), the current network
#'   will be preserved as a dyadic attribute.
#' @param network_var_name character string specifying the name for the old network
#'   when converted to a dyadic attribute. if NULL (default), uses the current
#'   weight attribute name if available, otherwise "old_network".
#' @param symmetric logical or NULL. specifies whether the new network should be
#'   treated as symmetric. if NULL (default), attempts to detect from the dyadic
#'   variable's symmetry setting or data structure.
#' @param weight_type character string describing the type of weight for the new
#'   network (e.g., "trade_volume", "fdi_amount"). if NULL (default), uses the
#'   dyad_var name.
#' @param diag_to_NA logical. whether to set diagonal values to na in the new
#'   network. if NULL (default), inherits from the original netlet.
#' @param missing_to_zero logical. whether to treat missing values as zeros in
#'   the new network. if NULL (default), inherits from the original netlet.
#'
#' @return a netify object with the dyadic variable as the main network and
#'   (optionally) the old network preserved as a dyadic attribute. all other
#'   attributes and dyadic variables are preserved.
#'
#' @details
#' the function handles different netify types appropriately:
#' \itemize{
#'   \item for cross-sectional networks: performs a simple matrix swap
#'   \item for longitudinal arrays: swaps matrices across all time periods
#'   \item for longitudinal lists: swaps matrices for each time period
#'   \item for multilayer networks: swaps within each layer
#' }
#'
#' when the new network has different properties than the original (e.g., different
#' symmetry or weight type), the function updates the netify attributes accordingly.
#' for bipartite networks, the new network is always treated as asymmetric.
#'
#' if the dyadic variable was originally specified with symmetry information via
#' `add_dyad_vars()`, that information is used unless overridden by the symmetric
#' parameter.
#'
#' @examples
#' # load example data
#' data(icews)
#'
#' # create a netify object with verbal cooperation as the main network
#' # and material cooperation as a dyadic attribute
#' icews_10 <- icews[icews$year == 2010, ]
#'
#' net <- netify(
#'     icews_10,
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' net <- add_dyad_vars(
#'     net,
#'     icews_10,
#'     actor1 = "i", actor2 = "j",
#'     dyad_vars = "matlCoop",
#'     dyad_vars_symmetric = FALSE
#' )
#'
#' # check the current network
#' print(net)
#'
#' # pivot to make material cooperation the main network
#' net_pivoted <- pivot_dyad_to_network(
#'     net,
#'     dyad_var = "matlCoop",
#'     network_var_name = "verbCoop"
#' )
#'
#' # the main network is now material cooperation
#' print(net_pivoted)
#'
#' # the old network (verbal cooperation) is preserved as a dyadic attribute
#' attr(net_pivoted, "dyad_data")[["1"]][["verbCoop"]][1:5, 1:5]
#'
#' @author shahryar minhas
#'
#' @export pivot_dyad_to_network

pivot_dyad_to_network <- function(
	netlet,
	dyad_var,
	make_network_dyad_var = TRUE,
	network_var_name = NULL,
	symmetric = NULL,
	weight_type = NULL,
	diag_to_NA = NULL,
	missing_to_zero = NULL) {
	# validate inputs
	netify_check(netlet)
	checkmate::assert_string(dyad_var)
	checkmate::assert_logical(make_network_dyad_var, len = 1)
	checkmate::assert_string(network_var_name, null.ok = TRUE)
	checkmate::assert_logical(symmetric, len = 1, null.ok = TRUE)
	checkmate::assert_string(weight_type, null.ok = TRUE)
	checkmate::assert_logical(diag_to_NA, len = 1, null.ok = TRUE)
	checkmate::assert_logical(missing_to_zero, len = 1, null.ok = TRUE)

	# extract attributes from the netlet object
	netlet_attrs <- attributes(netlet)
	netlet_type <- netlet_attrs$netify_type

	# check if dyad_data exists in the netlet object
	if (is.null(netlet_attrs$dyad_data)) {
		cli::cli_abort("No dyadic variables found in the netify object. Use add_dyad_vars() first.")
	}

	# check if the specified dyad_var exists in the first time period
	first_time_data <- netlet_attrs$dyad_data[[1]]
	if (is.null(first_time_data[[dyad_var]])) {
		available_vars <- names(first_time_data)
		cli::cli_abort(c(
			"Dyadic variable '{dyad_var}' not found.",
			"i" = "Available dyadic variables: {paste(available_vars, collapse=', ')}"
		))
	}

	# set default values for optional attributes if they are not provided
	if (is.null(diag_to_NA)) diag_to_NA <- netlet_attrs$diag_to_NA
	if (is.null(missing_to_zero)) missing_to_zero <- netlet_attrs$missing_to_zero
	if (is.null(weight_type)) weight_type <- dyad_var

	# determine the name for the network variable if needed
	if (make_network_dyad_var && is.null(network_var_name)) {
		if (!is.null(netlet_attrs[["weight"]])) {
			network_var_name <- netlet_attrs[["weight"]]
		} else {
			network_var_name <- "old_network"
		}
	}

	# handle bipartite networks by forcing them to be asymmetric
	if (netlet_attrs$mode == "bipartite") {
		if (!is.null(symmetric) && symmetric) {
			cli::cli_warn("Bipartite networks must be asymmetric. Setting {.arg symmetric} = {.val FALSE}.")
		}
		symmetric <- FALSE
		} else {
			# detect symmetry for non-bipartite networks
			if (is.null(symmetric)) {
				symmetric <- detect_dyad_symmetry(netlet, dyad_var)
				symmetry_label <- if (length(symmetric) > 1L) {
					paste0(names(symmetric), "=", ifelse(symmetric, "symmetric", "asymmetric"), collapse = ", ")
				} else {
					ifelse(symmetric, "symmetric", "asymmetric")
				}
				cli::cli_alert_info("Auto-detected symmetry for '{dyad_var}': {symmetry_label}")
			}
		}

	# create a copy of the netlet object to modify
	new_netlet <- netlet
	new_attrs <- attributes(new_netlet)

	# perform the swap operation based on the type of netlet
	if (netlet_type == "cross_sec") {
		new_netlet <- swap_cross_sec(new_netlet, dyad_var, make_network_dyad_var, network_var_name)
	} else if (netlet_type == "longit_array") {
		new_netlet <- swap_longit_array(new_netlet, dyad_var, make_network_dyad_var, network_var_name)
	} else if (netlet_type == "longit_list") {
		new_netlet <- swap_longit_list(new_netlet, dyad_var, make_network_dyad_var, network_var_name)
	}

	# update attributes of the new netlet object
	layer_count <- length(new_attrs$layers %||% character(0))
	attr(new_netlet, "weight") <- if (layer_count > 1L) {
		paste(rep(dyad_var, layer_count), collapse = ", ")
	} else {
		dyad_var
	}
	attr(new_netlet, "symmetric") <- symmetric
	detail_weight <- ifelse(
		weight_type == dyad_var,
		paste0("Weighted: ", weight_type),
		paste0("Weighted: ", weight_type, " (", dyad_var, ")")
	)
	attr(new_netlet, "detail_weight") <- if (layer_count > 1L) {
		paste(rep(detail_weight, layer_count), collapse = " | ")
	} else {
		detail_weight
	}
	attr(new_netlet, "diag_to_NA") <- diag_to_NA
	attr(new_netlet, "missing_to_zero") <- missing_to_zero

	# remove the dyad_var from dyad_data since it is now the main network
	dyad_data <- attr(new_netlet, "dyad_data")
	for (t in names(dyad_data)) {
		dyad_data[[t]][[dyad_var]] <- NULL
	}
	attr(new_netlet, "dyad_data") <- dyad_data

	# print success messages
	cli::cli_alert_success("Successfully pivoted '{dyad_var}' to be the main network")
	if (make_network_dyad_var) {
		cli::cli_alert_info("Previous network preserved as dyadic variable '{network_var_name}'")
	}

	#
	return(new_netlet)
}

#' detect symmetry of a dyadic variable
#'
#' internal function to detect whether a dyadic variable is symmetric
#' by checking if matrix\[i,j\] equals matrix\[j,i\] for all elements.
#'
#' @param netlet a netify object
#' @param dyad_var name of the dyadic variable to check
#'
#' @return logical indicating whether the variable is symmetric
#'
#' @keywords internal
#' @noRd
detect_dyad_symmetry <- function(netlet, dyad_var) {
	# get the first matrix for the dyadic variable
	first_matrix <- attr(netlet, "dyad_data")[[1]][[dyad_var]]

	if (length(dim(first_matrix)) == 3L) {
		out <- vapply(seq_len(dim(first_matrix)[3]), function(i) {
			layer_matrix <- first_matrix[, , i]
			nrow(layer_matrix) == ncol(layer_matrix) &&
				isSymmetric(layer_matrix, check.attributes = FALSE)
		}, logical(1))
		names(out) <- dimnames(first_matrix)[[3]] %||% paste0("layer", seq_along(out))
		if (length(unique(out)) == 1L) return(unname(out[1]))
		return(out)
	}

	# check if the matrix is square
	if (nrow(first_matrix) != ncol(first_matrix)) {
		return(FALSE)
	}

	# check if the matrix is symmetric
	is_symmetric <- isSymmetric(first_matrix, check.attributes = FALSE)

	return(is_symmetric)
}

#' swap matrices for cross-sectional netify objects
#'
#' @param netlet cross-sectional netify object
#' @param dyad_var name of dyadic variable to become network
#' @param make_network_dyad_var whether to preserve old network as dyadic variable
#' @param network_var_name name for the old network when stored as dyadic variable
#'
#' @return modified netify object
#' @keywords internal
#' @noRd
swap_cross_sec <- function(
	netlet, dyad_var,
	make_network_dyad_var, network_var_name) {
	# get the new network matrix from the dyadic variable
	new_network_matrix <- attr(netlet, "dyad_data")[["1"]][[dyad_var]]

	# if requested, save the old network as a dyadic variable
	if (make_network_dyad_var) {
		old_network_matrix <- if (length(dim(netlet)) == 3L) netlet[, , , drop = FALSE] else netlet[, , drop = FALSE]
		attr(netlet, "dyad_data")[["1"]][[network_var_name]] <- old_network_matrix
	}

	# replace the network with the new matrix
	if (length(dim(netlet)) == 3L) {
		netlet[, , ] <- pivot_replacement_array(new_network_matrix, dim(netlet), dimnames(netlet), dyad_var)
	} else {
		netlet[, ] <- new_network_matrix
	}

	return(netlet)
}

#' swap matrices for longitudinal array netify objects
#'
#' @param netlet longitudinal array netify object
#' @param dyad_var name of dyadic variable to become network
#' @param make_network_dyad_var whether to preserve old network as dyadic variable
#' @param network_var_name name for the old network when stored as dyadic variable
#'
#' @return modified netify object
#' @keywords internal
#' @noRd
swap_longit_array <- function(
	netlet, dyad_var,
	make_network_dyad_var, network_var_name) {
	# check if the array has layers
	has_layers <- length(dim(netlet)) == 4

	if (has_layers) {
		# get the number of layers and time points
		n_layers <- dim(netlet)[3]
		n_time <- dim(netlet)[4]
		time_names <- dimnames(netlet)[[4]]

		# iterate over each time point
		for (t_idx in 1:n_time) {
			t_name <- time_names[t_idx]

				# get the new network matrix for the current time point
				new_network_matrix <- attr(netlet, "dyad_data")[[t_name]][[dyad_var]]

			# if requested, save the old network as a dyadic variable
			if (make_network_dyad_var) {
					old_network_array <- netlet[, , , t_idx, drop = FALSE]
					attr(netlet, "dyad_data")[[t_name]][[network_var_name]] <- old_network_array[, , , 1, drop = FALSE]
				}

				netlet[, , , t_idx] <- pivot_replacement_array(
					new_network_matrix, dim(netlet)[1:3], dimnames(netlet)[1:3], dyad_var)
			}
	} else {
		# get the number of time points
		n_time <- dim(netlet)[3]
		time_names <- dimnames(netlet)[[3]]

		# iterate over each time point
		for (t_idx in 1:n_time) {
			t_name <- time_names[t_idx]

			# get the new network matrix for the current time point
			new_network_matrix <- attr(netlet, "dyad_data")[[t_name]][[dyad_var]]

			# if requested, save the old network as a dyadic variable
			if (make_network_dyad_var) {
				old_network_matrix <- netlet[, , t_idx]
				attr(netlet, "dyad_data")[[t_name]][[network_var_name]] <- old_network_matrix
			}

			# replace the network with the new matrix
			netlet[, , t_idx] <- new_network_matrix
		}
	}

	return(netlet)
}

#' swap matrices for longitudinal list netify objects
#'
#' @param netlet longitudinal list netify object
#' @param dyad_var name of dyadic variable to become network
#' @param make_network_dyad_var whether to preserve old network as dyadic variable
#' @param network_var_name name for the old network when stored as dyadic variable
#'
#' @return modified netify object
#' @keywords internal
#' @noRd
swap_longit_list <- function(
	netlet, dyad_var,
	make_network_dyad_var, network_var_name) {
	# get the names of the time periods
	time_periods <- names(netlet)

	# iterate over each time period
	for (t in time_periods) {
		# get the new network matrix for the current time period
		new_network_matrix <- attr(netlet, "dyad_data")[[t]][[dyad_var]]

		# if requested, save the old network as a dyadic variable
		if (make_network_dyad_var) {
			old_network_matrix <- netlet[[t]]
			attr(netlet, "dyad_data")[[t]][[network_var_name]] <- old_network_matrix
		}

			# preserve the attributes of the old network
			old_attrs <- attributes(netlet[[t]])

			# replace the network with the new matrix
			if (length(dim(netlet[[t]])) == 3L) {
				netlet[[t]][, , ] <- pivot_replacement_array(
					new_network_matrix, dim(netlet[[t]]), dimnames(netlet[[t]]), dyad_var)
			} else {
				netlet[[t]][, ] <- new_network_matrix
			}

		# restore network attributes
		attributes(netlet[[t]]) <- old_attrs
	}

	return(netlet)
}

#' normalize a pivot replacement matrix for single- or multi-layer arrays
#'
#' @keywords internal
#' @noRd
pivot_replacement_array <- function(value, target_dim, target_dimnames, dyad_var) {
	if (length(dim(value)) == 2L) {
		cli::cli_warn(c(
			"!" = "Dyadic variable {.val {dyad_var}} is stored as one matrix; using it for every layer.",
			"i" = "Use a layer-specific dyadic array when each layer should receive a different replacement network."
		), .frequency = "once", .frequency_id = paste0("pivot_replicate_", dyad_var))
		out <- array(value,
			dim = target_dim,
			dimnames = target_dimnames)
		return(out)
	}
	if (length(dim(value)) == 3L && identical(dim(value), target_dim)) {
		return(value)
	}
	cli::cli_abort(c(
		"x" = "Dyadic variable {.val {dyad_var}} has dimensions incompatible with the target multilayer network.",
		"i" = "Expected a matrix with dimensions {.val {target_dim[1:2]}} or an array with dimensions {.val {target_dim}}."
	))
}
