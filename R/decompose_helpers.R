#' Process edge data from netify object
#' @keywords internal
#' @noRd
process_edge_data <- function(netlet, netify_type, weight_attr, remove_zeros, ego_netlet) {
    # extract raw adjacency data
    raw_data <- get_raw(netlet)

    # handle different data structures
    if (netify_type == "cross_sec") {
        # for cross-sectional, raw_data is a matrix
        edge_data <- melt_matrix(
            raw_data,
            remove_zeros = remove_zeros,
            remove_diagonal = TRUE
        )
    } else if (netify_type == "longit_array") {
        # for longitudinal array, melt the 3d array
        edge_data <- melt_array(
            raw_data,
            remove_zeros = remove_zeros,
            remove_diagonal = TRUE
        )
    } else if (netify_type == "longit_list") {
        # for longitudinal list, process each time period
        edge_data <- melt_list_sparse(
            raw_data,
            remove_zeros = remove_zeros,
            remove_diagonal = TRUE
        )
    }

    # rename weight column
    if (!is.null(weight_attr)) {
        names(edge_data)[names(edge_data) == "value"] <- weight_attr
    } else {
        names(edge_data)[names(edge_data) == "value"] <- "net_value"
    }

    # handle ego networks efficiently
    if (netify_type != "cross_sec" && !is.null(ego_netlet) && ego_netlet) {
        if ("L1" %in% names(edge_data)) {
            # vectorized extraction
            edge_data$L1 <- sub("^[^_]+__", "", edge_data$L1)
        }
    }

    edge_data
}

#' Merge dyadic attributes into edge data
#' @keywords internal
#' @noRd
merge_dyadic_attributes <- function(edge_data, dyad_data_attr, netify_type) {
    # melt dyad data once
    dyad_data_melted <- melt_var_time_list(dyad_data_attr)

    if (nrow(dyad_data_melted) == 0) {
        return(edge_data)
    }

    # remove self-loops
    self_loop_idx <- dyad_data_melted$Var1 != dyad_data_melted$Var2
    dyad_data_melted <- dyad_data_melted[self_loop_idx, ]

    # get unique variables
    var_names <- unique(dyad_data_melted$Var3)

    # for cross-sectional networks
    if (netify_type == "cross_sec") {
        # get all unique actors
        all_actors <- unique(c(
            edge_data$Var1, edge_data$Var2,
            dyad_data_melted$Var1, dyad_data_melted$Var2
        ))
        n_actors <- length(all_actors)

        # create actor to index mapping
        actor_to_idx <- setNames(seq_len(n_actors), all_actors)

        # convert edge data actors to indices once
        edge_idx1 <- actor_to_idx[edge_data$Var1]
        edge_idx2 <- actor_to_idx[edge_data$Var2]

        # process each variable
        for (var in var_names) {
            # get data for this variable
            var_data <- dyad_data_melted[dyad_data_melted$Var3 == var, ]

            # create lookup matrix
            lookup_mat <- matrix(NA, n_actors, n_actors)

            # fill lookup matrix using indices
            var_idx1 <- actor_to_idx[var_data$Var1]
            var_idx2 <- actor_to_idx[var_data$Var2]
            mat_idx <- cbind(var_idx1, var_idx2)
            lookup_mat[mat_idx] <- var_data$value

            # extract values for edges using matrix indexing
            edge_idx <- cbind(edge_idx1, edge_idx2)
            edge_data[[var]] <- lookup_mat[edge_idx]
        }
    } else {
        # for longitudinal networks

        # get unique time periods from edge data
        time_periods <- unique(edge_data$L1)

        # process each time period
        for (t in time_periods) {
            # get edges for this time period
            time_mask <- edge_data$L1 == t
            edge_subset_idx <- which(time_mask)

            if (length(edge_subset_idx) == 0) next

            # get dyad data for this time period
            dyad_time_mask <- dyad_data_melted$L1 == t
            dyad_subset <- dyad_data_melted[dyad_time_mask, ]

            if (nrow(dyad_subset) == 0) next

            # get actors for this time period
            actors_t <- unique(c(
                edge_data$Var1[edge_subset_idx],
                edge_data$Var2[edge_subset_idx],
                dyad_subset$Var1,
                dyad_subset$Var2
            ))
            n_actors_t <- length(actors_t)

            # create mapping for this time period
            actor_to_idx_t <- setNames(seq_len(n_actors_t), actors_t)

            # convert to indices
            edge_idx1_t <- actor_to_idx_t[edge_data$Var1[edge_subset_idx]]
            edge_idx2_t <- actor_to_idx_t[edge_data$Var2[edge_subset_idx]]

            # process each variable for this time period
            for (var in var_names) {
                # initialize column if it doesn't exist
                if (!(var %in% names(edge_data))) {
                    edge_data[[var]] <- NA
                }

                # get variable data for this time
                var_data_t <- dyad_subset[dyad_subset$Var3 == var, ]

                if (nrow(var_data_t) == 0) next

                # create lookup matrix for this time period
                lookup_mat_t <- matrix(NA, n_actors_t, n_actors_t)

                # fill lookup matrix
                var_idx1_t <- actor_to_idx_t[var_data_t$Var1]
                var_idx2_t <- actor_to_idx_t[var_data_t$Var2]
                mat_idx_t <- cbind(var_idx1_t, var_idx2_t)
                lookup_mat_t[mat_idx_t] <- var_data_t$value

                # extract values for edges
                edge_idx_t <- cbind(edge_idx1_t, edge_idx2_t)
                edge_data[[var]][edge_subset_idx] <- lookup_mat_t[edge_idx_t]
            }
        }
    }

    return(edge_data)
}


#' Finalize edge data structure
#' @keywords internal
#' @noRd
finalize_edge_data <- function(edge_data, netify_type) {
    # add time column for cross-sec if missing
    if (netify_type == "cross_sec" && !"L1" %in% names(edge_data)) {
        edge_data$L1 <- "1"
    }

    # ensure L1 exists
    if (!"L1" %in% names(edge_data)) {
        edge_data$L1 <- "1"
    }

    # reorder columns efficiently using direct indexing
    id_cols <- c("Var1", "Var2", "L1")
    other_cols <- setdiff(names(edge_data), id_cols)
    col_order <- c(id_cols, other_cols)

    # subset and rename in one operation
    edge_data <- edge_data[, col_order, drop = FALSE]
    names(edge_data)[1:3] <- c("from", "to", "time")

    # ensure time is character (vectorized)
    edge_data$time <- as.character(edge_data$time)

    #
    return(edge_data)
}

#' Process nodal data from netify object
#' @keywords internal
#' @noRd
process_nodal_data <- function(obj_attrs, netify_type) {
    # check if nodal data exists in obj_attrs
    # if not, try to build it from actor_pds
    if (!is.null(obj_attrs$nodal_data)) {
        nodal_data <- obj_attrs$nodal_data
    } else if (!is.null(obj_attrs$actor_pds)) {
        nodal_data <- actor_pds_to_frame(obj_attrs$actor_pds)
    } else {
        # fallback to an empty data frame if no data is found
        nodal_data <- data.frame(
            actor = character(0),
            time = character(0),
            stringsAsFactors = FALSE
        )
    }

    # make sure nodal_data is a data frame
    if (!is.data.frame(nodal_data)) {
        nodal_data <- as.data.frame(nodal_data, stringsAsFactors = FALSE)
    }

    # add a "time" column if it's missing
    if (nrow(nodal_data) > 0 && !"time" %in% names(nodal_data)) {
        nodal_data$time <- "1"
    }

    # standardize column order for consistency
    if (nrow(nodal_data) > 0) {
        id_vars <- c("actor", "time")

        # check if "actor" column exists
        # if not, try to guess it based on common names or position
        if (!"actor" %in% names(nodal_data)) {
            possible_actor_cols <- c("name", "node", "vertex")
            actor_col <- intersect(possible_actor_cols, names(nodal_data))
            if (length(actor_col) > 0) {
                names(nodal_data)[names(nodal_data) == actor_col[1]] <- "actor"
            } else if (ncol(nodal_data) > 0) {
                # assume first column is "actor" if no better guess
                names(nodal_data)[1] <- "actor"
            }
        }

        # reorder columns so id_vars come first
        existing_id_vars <- intersect(id_vars, names(nodal_data))
        other_vars <- setdiff(names(nodal_data), id_vars)
        nodal_data <- nodal_data[, c(existing_id_vars, other_vars), drop = FALSE]

        # rename "actor" column to "name" for consistency
        if ("actor" %in% names(nodal_data)) {
            names(nodal_data)[names(nodal_data) == "actor"] <- "name"
        }
    }

    # make sure "time" column is character type
    if ("time" %in% names(nodal_data)) {
        nodal_data$time <- as.character(nodal_data$time)
    }

    #
    return(nodal_data)
}
