#' Calculate actor-level network statistics
#'
#' computes actor-level statistics for netify objects, including degree,
#' strength, centrality measures, and custom metrics. handles different
#' network types (directed/undirected, weighted/unweighted) appropriately.
#'
#' @param netlet a netify object containing network data
#' @param invert_weights_for_igraph logical. if TRUE (default), inverts edge weights
#'   when calculating closeness and betweenness centrality, as igraph interprets
#'   weights as distances. set to FALSE if your weights already represent distances.
#' @param other_stats optional named list of custom functions to calculate additional
#'   actor-level statistics. each function should accept a matrix and return a
#'   vector with one value per actor.
#' @param stats one of `"all"` (default) or `"fast"`. the `"fast"` path
#'   returns only degree- and strength-style columns and skips closeness,
#'   betweenness, eigenvector, and hits. when the user does not pass
#'   `stats` explicitly, the default auto-promotes to `"fast"` once the
#'   number of actors reaches
#'   `getoption("netify.fast_threshold", 1500l)`; passing
#'   `stats = "all"` explicitly always honors that request.
#'
#' @return a data frame with actor-level statistics. columns always include:
#'   \describe{
#'     \item{\code{actor}}{actor name/identifier}
#'     \item{\code{time}}{time period (for longitudinal networks)}
#'     \item{\code{layer}}{layer name (for multilayer networks)}
#'   }
#'
#'   additional columns depend on network type:
#'
#'   \strong{for undirected networks:}
#'   \describe{
#'     \item{\code{degree}}{number of realized non-zero connections. for weighted
#'       networks, degree remains a count; use strength columns for sums and moments
#'       of edge weights.}
#'     \item{\code{prop_ties}}{proportion of possible ties realized. calculated as \eqn{p_i = \frac{d_i}{n-1}}, where \eqn{d_i} is the degree and \eqn{n} is the total number of actors.}
#'     \item{\code{network_share}}{actor's share of total network connections.
#'       for unweighted networks this is based on degree; for weighted networks
#'       this is based on absolute realized tie mass, so signed networks do not
#'       produce negative shares.}
#'     \item{\code{closeness}}{closeness centrality. calculated as \eqn{c_i = \frac{1}{\sum_{j} d(i, j)}}, where \eqn{d(i, j)} is the shortest path distance to every other actor \eqn{j}.}
#'     \item{\code{betweenness}}{betweenness centrality. calculated as \eqn{b_i = \sum_{s \neq i \neq t} \frac{\sigma_{st}(i)}{\sigma_{st}}}, where \eqn{\sigma_{st}} is the total number of shortest paths from node \eqn{s} to node \eqn{t} and \eqn{\sigma_{st}(i)} is the number of those paths that pass through \eqn{i}.}
#'     \item{\code{eigen_vector}}{eigenvector centrality, based on the principal eigenvector of the adjacency matrix.}
#'   }
#'
#'   \strong{for directed networks:}
#'   \describe{
#'     \item{\code{degree_in}, \code{degree_out}}{incoming and outgoing realized
#'       non-zero connection counts. for weighted networks, degree columns remain
#'       counts and strength columns summarize edge weights.}
#'     \item{\code{prop_ties_in}, \code{prop_ties_out}, \code{prop_ties_total}}{proportion of possible in, out, and total ties.}
#'     \item{\code{network_share_in}, \code{network_share_out}, \code{network_share_total}}{actor's share of incoming, outgoing, and total network connections. for weighted networks these shares use absolute realized tie mass.}
#'     \item{\code{degree_total}}{sum of in and out degree}
#'     \item{\code{closeness_in}, \code{closeness_out}, \code{closeness_all}}{directed closeness centrality}
#'     \item{\code{betweenness}}{betweenness centrality}
#'     \item{\code{authority_score}, \code{hub_score}}{authority and hub scores (asymmetric only)}
#'   }
#'
#'   \strong{for weighted networks, additional columns:}
#'   \describe{
#'     \item{\code{strength_sum}}{sum of edge weights. directed networks return \code{strength_sum_in}, \code{strength_sum_out}, and \code{strength_sum_total}.}
#'     \item{\code{strength_avg}}{average weight among realized non-zero ties. directed networks return \code{strength_avg_in}, \code{strength_avg_out}, and \code{strength_avg_total}.}
#'     \item{\code{strength_std}}{standard deviation of weights among realized non-zero ties. directed networks return \code{strength_std_in}, \code{strength_std_out}, and \code{strength_std_total}.}
#'     \item{\code{strength_median}}{median weight among realized non-zero ties. directed networks return \code{strength_median_in}, \code{strength_median_out}, and \code{strength_median_total}.}
#'   }
#'
#' @details
#' the function automatically adapts calculations based on network properties:
#'
#' \strong{centrality measures:}
#' \itemize{
#'   \item \strong{degree}: count of direct connections. for directed networks,
#'     calculated separately for incoming (in-degree) and outgoing (out-degree) ties.
#'   \item \strong{closeness}: measures how quickly an actor can reach all others.
#'     based on the inverse of the sum of shortest path distances.
#'   \item \strong{betweenness}: measures how often an actor lies on shortest paths
#'     between other actors, indicating brokerage potential.
#'   \item \strong{eigenvector}: measures importance based on connections to other
#'     important actors. computed using the principal eigenvector of the adjacency matrix.
#'   \item \strong{authority/hub}: for directed networks only. authority scores measure
#'     importance as targets of ties from important sources. hub scores measure
#'     importance as sources of ties to important targets.
#' }
#'
#' \strong{weight handling:}
#'
#' by default, the function assumes larger weights indicate stronger relationships.
#' when calculating closeness and betweenness centrality, weights are shifted to a
#' positive scale when needed and inverted (1/weight) because igraph treats edge
#' weights as distances. for distance-based networks where larger values already
#' represent distances or weaker relationships, set
#' \code{invert_weights_for_igraph = FALSE}. eigenvector, authority, and hub scores
#' use the positive shifted strength scale without distance inversion.
#' strength summaries are calculated over realized non-zero ties. observed zero
#' non-ties and missing dyads are excluded from strength averages, standard
#' deviations, and medians; negative signed ties remain part of the realized-tie
#' distribution.
#'
#' \strong{custom statistics:}
#'
#' add custom metrics using the \code{other_stats} parameter. each function receives
#' the adjacency matrix and should return a vector with one value per actor:
#'
#' \preformatted{
#' # example: maximum tie weight for each actor
#' max_out <- function(mat) apply(mat, 1, max, na.rm = TRUE)
#' max_in <- function(mat) apply(mat, 2, max, na.rm = TRUE)
#'
#' stats <- summary_actor(net,
#'   other_stats = list(max_out = max_out, max_in = max_in))
#' }
#'
#' \strong{mathematical formulations:}
#'
#' for symmetric unweighted networks:
#' \itemize{
#'   \item degree: \eqn{d_i = \sum_{j=1}^{n} a_{ij}}
#'   \item proportion of ties: \eqn{p_i = \frac{d_i}{n-1}}
#'   \item network share: \eqn{s_i = \frac{d_i}{\sum_{j=1}^{n} d_j}}
#'   \item closeness: \eqn{c_i = \frac{1}{\sum_{j} d(i, j)}}
#'   \item betweenness: \eqn{b_i = \sum_{s \neq i \neq t} \frac{\sigma_{st}(i)}{\sigma_{st}}}
#' }
#'
#' for symmetric weighted networks, additional measures:
#' \itemize{
#'   \item strength sum: \eqn{s_i^{sum} = \sum_{j=1}^{n} w_{ij}}
#'   \item strength average: mean of the actor's realized non-zero tie weights
#'   \item strength standard deviation: standard deviation of the actor's realized
#'     non-zero tie weights
#' }
#'
#' for asymmetric networks, statistics are calculated separately for rows (out)
#' and columns (in), with totals where applicable.
#'
#' @note
#' for longitudinal networks, statistics are calculated separately for each time
#' period. for multilayer networks, statistics are calculated separately for each
#' layer unless layers have been aggregated beforehand.
#'
#' missing values (na) in the network are excluded from calculations. isolates
#' (actors with no connections) receive appropriate values (0 for degree, na for
#' some centrality measures).
#'
#' the function handles both cross-sectional and longitudinal data structures,
#' as well as single-layer and multilayer networks. for ego networks created
#' with netify, the function appropriately handles the ego-alter structure.
#'
#' @examples
#' \donttest{
#' # load example data
#' data(icews)
#'
#' # basic usage with directed network
#' net <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j", time = "year",
#'     symmetric = FALSE,
#'     weight = "verbCoop"
#' )
#'
#' # get actor statistics
#' actor_stats <- summary_actor(net)
#' head(actor_stats)
#'
#' # add custom statistics
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
#' @author cassy dorff, shahryar minhas
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

	# track whether the caller supplied stats
	user_set_stats <- !missing(stats)

	# "fast" skips closeness / betweenness / eigen / hits
	stats <- match.arg(stats)

	# pull out attrs
	obj_attrs <- attributes(netlet)

	# use the fast path for large networks by default
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
	# in obj_attrs to false
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
		# calc stats across netlet(s) and keep actor/time identifiers explicit
		object_names <- names(object)
		if (is.null(object_names)) {
			object_names <- as.character(seq_along(object))
		}
		object_names[is.na(object_names) | object_names == ""] <- as.character(which(is.na(object_names) | object_names == ""))
		net_stats_actor_list <- lapply(seq_along(object), function(k) {
			stats_mat <- actor_stats_for_netlet(
				object[[k]], obj_attrs,
				invert_weights_for_igraph = invert_weights_for_igraph,
				other_stats = other_stats,
				fast = is_fast
			)
			stats_df <- as.data.frame(stats_mat, stringsAsFactors = FALSE)
			stats_df$actor <- rownames(stats_mat)
			if (is_longit) {
				stats_df$time <- object_names[k]
			}
			stats_df
		})
		####

		####
		# more efficient data frame creation
		# combine all matrices at once, then convert to data frame
		net_stats_actor <- do.call("rbind", net_stats_actor_list)
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

	# reorder columns with id vars first
	id_vars <- c("actor", "layer", "time")
	existing_id_vars <- intersect(id_vars, names(net_stats_actor))
	stat_vars <- setdiff(names(net_stats_actor), id_vars)
	net_stats_actor <- net_stats_actor[, c(existing_id_vars, stat_vars)]
	####

		# pull ego metadata into the actor summary
	ego_netlet <- isTRUE(obj_attrs$ego_netlet) || isTRUE(obj_attrs$ego_netify)

	if (ego_netlet) {
		ego_vec <- obj_attrs$ego_vec %||% obj_attrs$ego_id %||% obj_attrs$ego_entry
		ego_longit <- isTRUE(obj_attrs$ego_longit)

			# map ego ids onto the summary columns

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
			has_compound_names <- vapply(time_split, length, integer(1)) >= 2L
			if (all(has_compound_names)) {
				net_stats_actor$layer <- vapply(time_split, `[`, character(1), 1)
				net_stats_actor$time <- vapply(time_split, `[`, character(1), 2)
			} else {
				net_stats_actor$layer <- ego_vec
			}

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

#' plot method for summary_actor output
#'
#' s3 method that dispatches `plot()` on a `summary_actor` data frame to
#' [plot_actor_stats()] so the `summary_actor(net) |> plot()` idiom works
#' without the user having to remember the helper name. pass any
#' [plot_actor_stats()] argument through `...`.
#'
#' @param x a `summary_actor` data frame from [summary_actor()].
#' @param ... additional arguments passed to [plot_actor_stats()] (e.g.
#'   `across_actor`, `specific_stats`, `specific_actors`).
#'
#' @return a `ggplot` object.
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
plot.summary_actor <- function(x, ...) {
	plot_actor_stats(x, ...)
}
