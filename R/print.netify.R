#' Print method for netify objects
#'
#' Displays a formatted summary of a netify object, including network type,
#' dimensions, summary statistics, and available attributes.
#'
#' @param x A netify object
#' @param ... Additional parameters (not used)
#'
#' @return Invisibly returns the input netify object. Called for its side effect
#'   of printing network information to the console.
#'
#' @details
#' The print method displays:
#' \itemize{
#'   \item Network type (unipartite/bipartite, symmetric/asymmetric)
#'   \item Edge weight specification
#'   \item Temporal structure (cross-sectional or number of time periods)
#'   \item Actor counts (total unique actors, or separate row/column counts for bipartite)
#'   \item Summary statistics (density, reciprocity, transitivity, etc.)
#'   \item Available nodal and dyadic attributes
#' }
#'
#' For longitudinal networks, summary statistics are averaged across time periods.
#' For multilayer networks, statistics are shown separately for each layer.
#'
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @importFrom utils capture.output
#'
#' @export print.netify
#' @export

print.netify <- function(x, ...) {
    ######################
    netlet <- x
    rm(x)

    # pull out attrs
    obj_attrs <- attributes(netlet)

    # pull out msrmnts
    msrmnts <- netify_measurements(netlet)

    # pull out some ego info if it's there,
    # assume FALSE
    ego_netlet <- FALSE
    ego_longit <- FALSE
    include_ego <- FALSE
    if (!is.null(obj_attrs$ego_netlet)) {
        if (obj_attrs$ego_netlet) {
            ego_netlet <- TRUE
            ego_vec <- obj_attrs$ego_vec
            ego_entry <- obj_attrs$ego_entry
            threshold <- obj_attrs$threshold
            ngbd_direction <- obj_attrs$ngbd_direction
            include_ego <- obj_attrs$include_ego
            ego_longit <- obj_attrs$ego_longit
        }
    }

    # ALSO check the newer attribute name (ego_netify)
    if (!is.null(obj_attrs$ego_netify)) {
        if (obj_attrs$ego_netify) {
            ego_netlet <- TRUE
            ego_vec <- obj_attrs$ego_vec %||% obj_attrs$ego_entry # fallback
            ego_entry <- obj_attrs$ego_entry
            threshold <- obj_attrs$threshold
            ngbd_direction <- obj_attrs$ngbd_direction
            include_ego <- obj_attrs$include_ego
            ego_longit <- obj_attrs$ego_longit %||% FALSE
        }
    }
    ######################

    ######################
    # info on netlet type
    obj_time <- ifelse(grepl("longit", obj_attrs$netify_type),
        "Longitudinal", "Cross-Sectional"
    )
    obj_mode <- ifelse(obj_attrs$mode == "unipartite",
        "Unipartite", "Bipartite"
    )
    obj_layer <- ifelse(length(obj_attrs$layers) > 1,
        "Multilayer", "Single Layer"
    )
    obj_symm <- ifelse(obj_attrs$symmetric,
        "Symmetric", "Asymmetric"
    )

    # get information about cross-sections
    detail_weight_label <- obj_attrs$detail_weight
    loops_label <- ifelse(obj_attrs$diag_to_NA,
        "No Loops Allowed", "Loops Preserved"
    )

    # pull out msrmnt info about composition of network
    n_layers <- ifelse(is.null(msrmnts$n_layers), 1, msrmnts$n_layers)
    n_time <- ifelse(is.null(msrmnts$n_time), 1, msrmnts$n_time)
    n_row_actors <- msrmnts$n_row_actors
    n_col_actors <- msrmnts$n_col_actors

    # pull out total number of unique actors
    if (obj_attrs$netify_type == "longit_list") {
        n_row_actors <- length(unique(unlist(msrmnts$row_actors)))
        n_col_actors <- length(unique(unlist(msrmnts$col_actors)))
    }

    # gen labels from msrmnts
    time_label <- ifelse(n_time == 1,
        "Cross-Sectional", paste0("Longitudinal: ", n_time, " Periods")
    )
    row_label <- paste0("# Unique Row Actors: ", n_row_actors)
    col_label <- paste0("# Unique Column Actors: ", n_col_actors)
    gen_row_col_label <- paste0("# Unique Actors: ", n_row_actors)

    # org info about ftrs
    nodal_ftrs_label <- paste0("Nodal Features: ", ifelse(
        is.null(msrmnts$nvars),
        "None", paste(msrmnts$nvars, collapse = ", ")
    ))
    dyad_ftrs_label <- paste0("Dyad Features: ", ifelse(
        is.null(msrmnts$dvars),
        "None", paste(msrmnts$dvars, collapse = ", ")
    ))
    graph_ftrs_label <- paste0("Graph Features: ", ifelse(
        is.null(msrmnts$gvars),
        "None", paste(msrmnts$gvars, collapse = ", ")
    ))
    ######################

    ######################
    # get graph summary stats
    summ_stats <- summary(netlet)

    # ids
    ids <- intersect(c("net", "layer"), names(summ_stats))

    # stats of interest
    to_keep_poss <- c(
        "density", "prop_edges_missing",
        "mean_edge_weight",
        # 'sd_edge_weight',
        # 'median_edge_weight',
        # 'min_edge_weight', 'max_edge_weight',
        "reciprocity", "transitivity"
    )
    to_keep_clean_poss <- c(
        "dens", "miss", "mean",
        # 'stdev',
        # 'median',
        # 'min', 'max',
        "recip", "trans"
    )
    to_keep <- intersect(to_keep_poss, names(summ_stats))
    to_keep_clean <- to_keep_clean_poss[match(to_keep, to_keep_poss)]

    # subset and organize
    summ_stats <- summ_stats[, c(ids, to_keep)]

    # add label for net if layer not present
    if (!"layer" %in% names(summ_stats)) {
        summ_stats <- cbind(
            layer = attr(netlet, "layers"), summ_stats
        )
    }

    # # if ego netlet then move net to layer column
    # if(ego_netlet){
    # 	summ_stats$layer = summ_stats$net
    # 	if(ego_longit){
    # 		summ_stats$layer = unlist(
    # 			lapply( strsplit(
    # 				summ_stats$net, '__'), function(x){x[1]}))
    # 	}
    # }

    # if longitudinal then avg across time
    summ_stats <- lapply(unique(summ_stats$layer), function(layer) {
        slice <- summ_stats[summ_stats$layer == layer, to_keep]
        names(slice) <- to_keep_clean
        slice <- data.frame(t(colMeans(slice, na.rm = TRUE)))
        slice <- round(slice, 3)
        slice <- cbind(layer = layer, slice)
        return(slice)
    })
    summ_stats <- do.call("rbind", summ_stats)

    # cleanup
    rownames(summ_stats) <- summ_stats$layer
    summ_stats <- summ_stats[, -1]
    ######################

    ######################
    # adjustments for info on ego_networks
    if (ego_netlet) {
        # pull out ego net ids
        ego_ids <- names(netlet)

        # pull out units
        ego_units <- ego_ids

        # if longit separate year from unit
        if (ego_longit) {
            ego_units <- unique(extract_ego_name(ego_ids))
            ego_pds <- unique(extract_ego_time(ego_ids))
        }

        # count up number of egos
        n_ego_units <- length(ego_entry)

        # modify items being printed
        time_label <- ifelse(ego_longit,
            paste0("Longitudinal: ", length(ego_pds), " Periods"),
            "Cross-Sectional"
        )

        # modify gen_row_col_label
        gen_row_col_label <- paste0(
            "# Unique Egos: ", n_ego_units, " | ",
            "# Unique Alters: ", n_row_actors - n_ego_units
        )
    }
    ######################

    ######################
    # intro message
    if (ego_netlet) {
        intro <- paste0(
            "Hello, you have created a neighborhood network for ego(s) (",
            ego_vec, "), yay!"
        )
        stat_msg <- "Neighborhood Network Summary Statistics:"
        if (obj_attrs$netify_type == "longit_list" & ego_longit) {
            stat_msg <- paste0(
                "Neighborhood Network Summary Statistics (averaged across time):"
            )
        }
    } else {
        intro <- "Hello, you have created network data, yay!"
        stat_msg <- "Network Summary Statistics:"
        if (n_time > 1) {
            stat_msg <- paste0(
                "Network Summary Statistics (averaged across time):"
            )
        }
    }

    # print out network type info
    cli::cli({
        cli::cli_alert_success(intro)

        # org printing
        ulid <- cli::cli_ul()

        # ego net specific info
        if (ego_netlet) {
            cli::cli_li(cli::col_magenta("Type: Ego Network"))
            cli::cli_li(paste0("Ego: ", ego_entry))
            cli::cli_li(paste0(
                "Direction: ",
                switch(ngbd_direction,
                    "out" = "Outgoing ties only",
                    "in" = "Incoming ties only",
                    "any" = "Any ties (in or out)"
                )
            ))
            cli::cli_li(paste0("Ego included: ", ifelse(include_ego, "Yes", "No")))
            if (!is.null(threshold) && any(threshold > 0)) {
                # smart rounding: use signif for very small/large numbers, round for normal range
                threshold_display_vals <- sapply(threshold, function(x) {
                    if (x == 0) {
                        "0"
                    } else if (abs(x) < 0.01 || abs(x) > 10000) {
                        # scientific notation for very small or very large numbers
                        format(x, scientific = TRUE, digits = 3)
                    } else if (abs(x) < 1) {
                        # small numbers, use significant figures
                        as.character(signif(x, 3))
                    } else {
                        # normal range numbers, round to 2 decimal places
                        as.character(round(x, 2))
                    }
                })

                if (length(threshold_display_vals) == 1) {
                    threshold_display <- threshold_display_vals
                } else {
                    # for multiple thresholds, show first 2 and last 2 if more than 4
                    if (length(threshold_display_vals) <= 4) {
                        threshold_display <- paste0(
                            "varies by time (",
                            paste(threshold_display_vals, collapse = ", "), ")"
                        )
                    } else {
                        first_two <- threshold_display_vals[1:2]
                        last_two <- threshold_display_vals[(length(threshold_display_vals) - 1):length(threshold_display_vals)]
                        threshold_display <- paste0(
                            "varies by time (",
                            paste(first_two, collapse = ", "),
                            ", ... , ",
                            paste(last_two, collapse = ", "), ")"
                        )
                    }
                }
                cli::cli_li(paste0("Threshold: ", threshold_display))
            }
            # cli::cli_text("")  # blank line for separation
        }

        # print out network type info
        # cli::cli_li( obj_time )
        cli::cli_li(obj_mode)
        if (n_layers > 1) {
            cli::cli_li(obj_layer)
        }
        cli::cli_li(obj_symm)
        cli::cli_li(detail_weight_label)

        # print out network measurements
        cli::cli_li(time_label)
        if (obj_attrs$mode == "bipartite") {
            cli::cli_li(row_label)
            cli::cli_li(col_label)
        }
        cli::cli_li(gen_row_col_label)

        # print stats
        cli::cli_text(stat_msg)
        # tbl <- knitr::kable(summ_stats, align = "r", format = "pandoc")
        tbl <- capture.output(print(summ_stats, right = TRUE))
        cli::cli_verbatim(tbl)

        # print info about attributes
        cli::cli_li(nodal_ftrs_label)
        cli::cli_li(dyad_ftrs_label)
        # cli::cli_li(graph_ftrs_label)

        # end printing
        cli::cli_end(ulid)
    })
    ######################

    #
    invisible(netlet)
}
