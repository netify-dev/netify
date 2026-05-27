#' Calculate actor-level network statistics
#'
#' Computes comprehensive actor-level statistics for netify objects, including
#' degree, strength, centrality measures, and custom metrics. Handles different
#' network types (directed/undirected, weighted/unweighted) appropriately.
#'
#' @param netlet A netify object containing network data
#' @param invert_weights_for_igraph Logical. If TRUE (default), inverts edge weights
#'   when calculating closeness and betweenness centrality, as igraph interprets
#'   weights as distances. Set to FALSE if your weights already represent distances.
#' @param other_stats Optional named list of custom functions to calculate additional
#'   actor-level statistics. Each function should accept a matrix and return a
#'   vector with one value per actor.
#' @param stats One of `"all"` (default) or `"fast"`. The `"fast"` path
#'   returns only degree- and strength-style columns and skips closeness,
#'   betweenness, eigenvector, and HITS. When the user does not pass
#'   `stats` explicitly, the default auto-promotes to `"fast"` once the
#'   number of actors reaches
#'   `getOption("netify.fast_threshold", 1500L)`; passing
#'   `stats = "all"` explicitly always honors that request.
#'
#' @return A data frame with actor-level statistics. Columns always include:
#'   \describe{
#'     \item{\code{actor}}{Actor name/identifier}
#'     \item{\code{time}}{Time period (for longitudinal networks)}
#'     \item{\code{layer}}{Layer name (for multilayer networks)}
#'   }
#'
#'   Additional columns depend on network type:
#'
#'   \strong{For undirected networks:}
#'   \describe{
#'     \item{\code{degree}}{Number of connections. Calculated as \eqn{d_i = \sum_{j=1}^{n} a_{ij}}, where \eqn{a_{ij}} is the adjacency matrix element.}
#'     \item{\code{prop_ties}}{Proportion of possible ties realized. Calculated as \eqn{p_i = \frac{d_i}{n-1}}, where \eqn{d_i} is the degree and \eqn{n} is the total number of actors.}
#'     \item{\code{net_share}}{Actor's share of total network connections. Calculated as \eqn{s_i = \frac{d_i}{\sum_{j=1}^{n} d_j}}.}
#'     \item{\code{closeness}}{Closeness centrality. Calculated as \eqn{C_i = \frac{1}{\sum_{j} d(i, j)}}, where \eqn{d(i, j)} is the shortest path distance to every other actor \eqn{j}.}
#'     \item{\code{betweenness}}{Betweenness centrality. Calculated as \eqn{B_i = \sum_{s \neq i \neq t} \frac{\sigma_{st}(i)}{\sigma_{st}}}, where \eqn{\sigma_{st}} is the total number of shortest paths from node \eqn{s} to node \eqn{t} and \eqn{\sigma_{st}(i)} is the number of those paths that pass through \eqn{i}.}
#'     \item{\code{eigen_centrality}}{Eigenvector centrality, based on the principal eigenvector of the adjacency matrix.}
#'   }
#'
#'   \strong{For directed networks:}
#'   \describe{
#'     \item{\code{in_degree}, \code{out_degree}}{Incoming and outgoing connections. \eqn{d_i^{in} = \sum_{j=1}^{n} a_{ji}} and \eqn{d_i^{out} = \sum_{j=1}^{n} a_{ij}}.}
#'     \item{\code{in_prop_ties}, \code{out_prop_ties}}{Proportion of possible in/out ties}
#'     \item{\code{total_degree}}{Sum of in and out degree}
#'     \item{\code{in_closeness}, \code{out_closeness}}{Directed closeness centrality}
#'     \item{\code{betweenness}}{Betweenness centrality}
#'     \item{\code{authority}, \code{hub}}{Authority and hub scores (asymmetric only)}
#'   }
#'
#'   \strong{For weighted networks, additional columns:}
#'   \describe{
#'     \item{\code{strength_sum}}{Sum of edge weights. For undirected: \eqn{s_i^{sum} = \sum_{j=1}^{n} w_{ij}}. For directed: separate in/out sums.}
#'     \item{\code{strength_avg}}{Average edge weight. Calculated as \eqn{s_i^{avg} = \frac{s_i^{sum}}{d_i}}.}
#'     \item{\code{strength_sd}}{Standard deviation of edge weights. Calculated as \eqn{s_i^{sd} = \sqrt{\frac{1}{d_i} \sum_{j=1}^{n} (w_{ij} - s_i^{avg})^2}}.}
#'     \item{\code{strength_median}}{Median edge weight}
#'   }
#'
#' @details
#' The function automatically adapts calculations based on network properties:
#'
#' \strong{Centrality Measures:}
#' \itemize{
#'   \item \strong{Degree}: Count of direct connections. For directed networks,
#'     calculated separately for incoming (in-degree) and outgoing (out-degree) ties.
#'   \item \strong{Closeness}: Measures how quickly an actor can reach all others.
#'     Based on the inverse of the sum of shortest path distances.
#'   \item \strong{Betweenness}: Measures how often an actor lies on shortest paths
#'     between other actors, indicating brokerage potential.
#'   \item \strong{Eigenvector}: Measures importance based on connections to other
#'     important actors. Computed using the principal eigenvector of the adjacency matrix.
#'   \item \strong{Authority/Hub}: For directed networks only. Authority scores measure
#'     importance as targets of ties from important sources. Hub scores measure
#'     importance as sources of ties to important targets.
#' }
#'
#' \strong{Weight Handling:}
#'
#' By default, the function assumes larger weights indicate stronger relationships.
#' When calculating closeness and betweenness centrality, weights are inverted
#' (1/weight) because igraph treats edge weights as distances. For distance-based
#' networks where larger values already represent distances or weaker relationships,
#' set \code{invert_weights_for_igraph = FALSE}.
#'
#' \strong{Custom Statistics:}
#'
#' Add custom metrics using the \code{other_stats} parameter. Each function receives
#' the adjacency matrix and should return a vector with one value per actor:
#'
#' \preformatted{
#' # Example: Maximum tie weight for each actor
#' max_out <- function(mat) apply(mat, 1, max, na.rm = TRUE)
#' max_in <- function(mat) apply(mat, 2, max, na.rm = TRUE)
#'
#' stats <- summary_actor(net,
#'   other_stats = list(max_out = max_out, max_in = max_in))
#' }
#'
#' \strong{Mathematical Formulations:}
#'
#' For symmetric unweighted networks:
#' \itemize{
#'   \item Degree: \eqn{d_i = \sum_{j=1}^{n} a_{ij}}
#'   \item Proportion of ties: \eqn{p_i = \frac{d_i}{n-1}}
#'   \item Network share: \eqn{s_i = \frac{d_i}{\sum_{j=1}^{n} d_j}}
#'   \item Closeness: \eqn{C_i = \frac{1}{\sum_{j} d(i, j)}}
#'   \item Betweenness: \eqn{B_i = \sum_{s \neq i \neq t} \frac{\sigma_{st}(i)}{\sigma_{st}}}
#' }
#'
#' For symmetric weighted networks, additional measures:
#' \itemize{
#'   \item Strength sum: \eqn{s_i^{sum} = \sum_{j=1}^{n} w_{ij}}
#'   \item Strength average: \eqn{s_i^{avg} = \frac{s_i^{sum}}{d_i}}
#'   \item Strength standard deviation: \eqn{s_i^{sd} = \sqrt{\frac{1}{d_i} \sum_{j=1}^{n} (w_{ij} - s_i^{avg})^2}}
#' }
#'
#' For asymmetric networks, statistics are calculated separately for rows (out)
#' and columns (in), with totals where applicable.
#'
#' @note
#' For longitudinal networks, statistics are calculated separately for each time
#' period. For multilayer networks, statistics are calculated separately for each
#' layer unless layers have been aggregated beforehand.
#'
#' Missing values (NA) in the network are excluded from calculations. Isolates
#' (actors with no connections) receive appropriate values (0 for degree, NA for
#' some centrality measures).
#'
#' The function handles both cross-sectional and longitudinal data structures,
#' as well as single-layer and multilayer networks. For ego networks created
#' with netify, the function appropriately handles the ego-alter structure.
#'
#' @examples
#' \donttest{
#' # Load example data
#' data(icews)
#'
#' # Basic usage with directed network
#' net <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # Get actor statistics
#' actor_stats <- summary_actor(net)
#' head(actor_stats)
#'
#' # Add custom statistics
#' max_out <- function(mat) apply(mat, 1, max, na.rm = TRUE)
#' max_in <- function(mat) apply(mat, 2, max, na.rm = TRUE)
#'
#' actor_stats_custom <- summary_actor(
#'     net,
#'     other_stats = list(
#'         max_out = max_out,
#'         max_in = max_in
#'     )
#' )
#' head(actor_stats_custom)
#' }
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @importFrom igraph closeness betweenness eigen_centrality hits_scores
#'
#' @export summary_actor
#'

summary_actor <- function(netlet, invert_weights_for_igraph = TRUE,
	other_stats = NULL, stats = c("all", "fast")) {
	####
	# check if netify object
	netify_check(netlet)

	# track whether caller supplied stats= explicitly
	user_set_stats <- !missing(stats)

	# "fast" skips closeness / betweenness / eigen / HITS
	stats <- match.arg(stats)

	# pull out attrs
	obj_attrs <- attributes(netlet)

	# auto-promote to fast at large N unless user set stats= explicitly
	fast_threshold <- as.integer(getOption("netify.fast_threshold", 1500L))
	n_actors_probe <- if (obj_attrs$netify_type == "longit_list") {
		nrow(netlet[[1]])
	} else if (obj_attrs$netify_type == "longit_array") {
		dim(netlet)[1]
	} else {
		nrow(netlet)
	}
	if (!user_set_stats && stats == "all" && is.finite(n_actors_probe) &&
		n_actors_probe >= fast_threshold) {
		cli::cli_inform(
			c("i" = "Auto-promoting to {.code stats = \"fast\"} at N = {n_actors_probe} (>= {.code getOption(\"netify.fast_threshold\", 1500L)}).",
			  "*" = "Set {.code stats = \"all\"} explicitly to force centralities, or raise the threshold via {.code options(netify.fast_threshold = ...)}."),
			.frequency = "once",
			.frequency_id = "summary_actor_auto_fast"
		)
		stats <- "fast"
	}
	is_fast <- stats == "fast"

	# pull out number of layers
	layers <- obj_attrs$layers
	n_layers <- length(layers)

	# if bipartite then set symmetric parameter
	# in obj_attrs to FALSE
	if (obj_attrs$mode == "bipartite") {
		obj_attrs$symmetric <- FALSE
	}

	# get type
	netify_type <- obj_attrs$netify_type
	is_longit <- netify_type != "cross_sec"
	####

	####
	# save original
	netlet_base <- netlet

	# pre-allocate list for efficiency
	net_stats_l_mutli <- vector("list", n_layers)

	# iterate through each layer
	for (i in seq_along(layers)) {
		layer <- layers[i]

		####
		# if just one layer then just set netlet to user input,
		# if more than one then subset
		if (n_layers == 1) {
			netlet <- netlet_base
		} else {
			netlet <- subset_netify(
				netlet_base,
				layers = layer
			)
			obj_attrs <- attributes(netlet)
		}

		# convert to list so we can have one
		# set of code for each type
		object <- switch(netify_type,
			"cross_sec" = list(netlet),
			"longit_array" = array_to_list(netlet),
			"longit_list" = netlet
		)
		####

		####
		# calc stats across netlet(s) - more efficient with lapply
		net_stats_actor_list <- lapply(object, function(mat) {
			actor_stats_for_netlet(
				mat, obj_attrs,
				invert_weights_for_igraph = invert_weights_for_igraph,
				other_stats = other_stats,
				fast = is_fast
			)
		})
		####

		####
		# more efficient data frame creation
		# combine all matrices at once, then convert to data frame
		net_stats_actor <- do.call("rbind", net_stats_actor_list)

		# convert to data frame with actor names efficiently
		actor_names <- rownames(net_stats_actor)
		net_stats_actor <- as.data.frame(net_stats_actor, stringsAsFactors = FALSE)
		net_stats_actor$actor <- actor_names
		net_stats_actor$layer <- layer

		# store in pre-allocated list
		net_stats_l_mutli[[i]] <- net_stats_actor
	}
	####

	####
	# bind into one data frame with rbind.fill logic for mixed columns
	all_cols <- unique(unlist(lapply(net_stats_l_mutli, names)))
	net_stats_l_mutli <- lapply(net_stats_l_mutli, function(df) {
		missing_cols <- setdiff(all_cols, names(df))
		if (length(missing_cols) > 0) {
			df[missing_cols] <- NA
		}
		df[all_cols]
	})
	net_stats_actor <- do.call("rbind", net_stats_l_mutli)
	rownames(net_stats_actor) <- NULL

	# drop layer column if only one layer
	if (n_layers == 1) {
		net_stats_actor$layer <- NULL
	}

	# if longitudinal, actor contains both year and name information
	if (is_longit) {
		actor_split <- strsplit(net_stats_actor$actor, ".", fixed = TRUE)
		net_stats_actor$time <- vapply(actor_split, `[`, character(1), 1)
		net_stats_actor$actor <- vapply(actor_split, `[`, character(1), 2)
	}

	# reorder columns with id vars first
	id_vars <- c("actor", "layer", "time")
	existing_id_vars <- intersect(id_vars, names(net_stats_actor))
	stat_vars <- setdiff(names(net_stats_actor), id_vars)
	net_stats_actor <- net_stats_actor[, c(existing_id_vars, stat_vars)]
	####

	####
	# pull out some ego info if it's there
	ego_netlet <- !is.null(obj_attrs$ego_netlet) && obj_attrs$ego_netlet

	if (ego_netlet) {
		ego_vec <- obj_attrs$ego_vec
		ego_longit <- obj_attrs$ego_longit

		# if ego netlet then make some changes
		# layer will be added and used for ego
		# net will stand in for time if ego data is longit

		# non-longit ego: layer holds ego name
		if (!ego_longit) {
			if (obj_attrs$netify_type == "cross_sec") {
				net_stats_actor$layer <- ego_vec
			} else {
				net_stats_actor$layer <- net_stats_actor$time
			}

			net_stats_actor$time <- NULL
			existing_id_vars <- c("actor", "layer")
		}

		# longit ego: split ego__time into layer + time
		if (ego_longit) {
			time_split <- strsplit(net_stats_actor$time, "__", fixed = TRUE)
			net_stats_actor$layer <- vapply(time_split, `[`, character(1), 1)
			net_stats_actor$time <- vapply(time_split, `[`, character(1), 2)

			existing_id_vars <- c("actor", "layer", "time")
		}

		stat_vars <- setdiff(names(net_stats_actor), existing_id_vars)
		net_stats_actor <- net_stats_actor[, c(existing_id_vars, stat_vars)]
	}
	####

	# tag stats mode so downstream helpers can adapt
	attr(net_stats_actor, "stats") <- stats

	# tag class so plot() dispatches to plot_actor_stats
	class(net_stats_actor) <- c("summary_actor", "data.frame")

	####
	return(net_stats_actor)
	####
}

#' Plot method for summary_actor output
#'
#' S3 method that dispatches `plot()` on a `summary_actor` data frame to
#' [plot_actor_stats()] so the `summary_actor(net) |> plot()` idiom works
#' without the user having to remember the helper name. Pass any
#' [plot_actor_stats()] argument through `...`.
#'
#' @param x A `summary_actor` data frame from [summary_actor()].
#' @param ... Additional arguments passed to [plot_actor_stats()] (e.g.
#'   `across_actor`, `specific_stats`, `specific_actors`).
#'
#' @return A `ggplot` object.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
plot.summary_actor <- function(x, ...) {
	plot_actor_stats(x, ...)
}
