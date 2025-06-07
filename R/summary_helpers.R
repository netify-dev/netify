#' Calculate graph-level statistics for a single netlet
#'
#' `graph_stats_for_netlet` calculates various graph-level statistics for a single netlet (network slice) within a netify object. This function is designed to be used internally by the `summary.netify` function to process each netlet separately.
#'
#' @param mat A matrix representing a single netlet (network slice) from a netify object.
#' @param obj_attrs A list of attributes associated with the netify object, including information about the network type, mode, and other properties.
#' @param summary_args A list of additional arguments passed from the `summary.netify` function, which can include user-defined statistical functions.
#'
#' @return A named vector containing the calculated graph-level statistics for the given netlet. The specific statistics included depend on the network type and attributes.
#'
#' @details This function is not meant to be called directly by users but is instead used internally by the `summary.netify` function to process each netlet separately. It calculates a wide range of graph-level statistics based on the input matrix and the attributes of the netify object.
#'
#' @importFrom stats median
#' 
#' @keywords internal
#' @noRd

graph_stats_for_netlet <- function(mat, obj_attrs, summary_args) {
  # prep some objects
  g <- prep_for_igraph(mat)
  bin_mat <- +(mat > 0)
  vec_mat <- c(mat)
  vec_bin_mat <- c(bin_mat)

  # start calcs
  row_means <- rowMeans(mat, na.rm = TRUE)
  col_means <- colMeans(mat, na.rm = TRUE)
  num_row_actors <- nrow(mat)
  num_col_actors <- ncol(mat)

  # get proportion missing
  if (obj_attrs$diag_to_NA) {
    prop_edges_missing <- (sum(is.na(vec_mat)) - num_row_actors) / length(vec_mat)
  } else {
    prop_edges_missing <- sum(is.na(vec_mat)) / length(vec_mat)
  }

  # min/max scores
  min_edge_weight <- min(vec_mat, na.rm = TRUE)
  max_edge_weight <- max(vec_mat, na.rm = TRUE)
  median_edge_weight <- median(vec_mat, na.rm = TRUE)  

  # graph level summary measures
  density <- mean(vec_bin_mat, na.rm = TRUE)
  mean_edge_weight <- mean(vec_mat, na.rm = TRUE)
  sd_edge_weight <- sd(vec_mat, na.rm = TRUE)
  num_edges <- sum(vec_bin_mat, na.rm = TRUE)

  # competition measures
  row_sums <- row_means * num_col_actors
  col_sums <- col_means * num_row_actors
  competition_row <- sum((row_sums / sum(row_sums))^2)
  competition_col <- sum((col_sums / sum(col_sums))^2)

  # symmetric case
  competition <- row_sums

  # getting at actor variability
  sd_of_row_means <- sd(row_means, na.rm = TRUE)
  sd_of_col_means <- sd(col_means, na.rm = TRUE)

  # covar and recip calcs for one mode nets
  if (obj_attrs$mode == 'unipartite' & !obj_attrs$symmetric) {
    covar_of_row_col_means <- cor(row_means, col_means, use = 'pairwise.complete.obs')
    reciprocity <- cor(vec_mat, c(t(mat)), use = 'pairwise.complete.obs')
  }

  # place holder val for bipartite
  if (obj_attrs$mode == 'bipartite' | obj_attrs$symmetric) {
    covar_of_row_col_means <- NA
    reciprocity <- NA
  }

  # get igraph clustering coef
  transitivity <- igraph::transitivity(g)

  # organize
  out <- c(
    num_row_actors = num_row_actors,
    num_col_actors = num_col_actors,
    density = density,
    num_edges = num_edges,
    prop_edges_missing = prop_edges_missing,    
    mean_edge_weight = mean_edge_weight,
    sd_edge_weight = sd_edge_weight,
    median_edge_weight = median_edge_weight,
    min_edge_weight = min_edge_weight,
    max_edge_weight = max_edge_weight,
    competition_row = competition_row,
    competition_col = competition_col,
    sd_of_row_means = sd_of_row_means,
    sd_of_col_means = sd_of_col_means,
    covar_of_row_col_means = covar_of_row_col_means,
    reciprocity = reciprocity,
    transitivity = transitivity
  )

  # calculate any user-supplied stats
  if ('other_stats' %in% names(summary_args)) {
    other_out <- lapply(summary_args$other_stats, function(stat) {
      stat(mat)
    })
    out <- append(out, unlist(other_out))
  }

  return(out)
}

#' Calculate actor-level statistics for a single netlet
#'
#' `actor_stats_for_netlet` calculates various actor-level statistics for a single netlet (network slice) within a netify object. This function is designed to be used internally by the `summary_actor` function to process each netlet separately.
#'
#' @param mat A matrix representing a single netlet (network slice) from a netify object.
#' @param obj_attrs A list of attributes associated with the netify object, including information about the network type, mode, and other properties.
#' @param invert_weights_for_igraph Logical; if TRUE, the weights of the edges are inverted before being used in the calculation of closeness or betweenness centrality. This is because igraph treats edge weights as distances. Inverting weights can be crucial when higher weights should imply stronger (or more valuable) connections rather than longer distances. Default is TRUE.
#' @param other_stats A named list of functions that take a matrix and return additional actor-level statistics to be included in the output. Each function should accept a matrix as input and return a vector or single value per actor. This allows for the inclusion of custom metrics in the summary output.
#'
#' @return A data frame containing the calculated actor-level statistics for the given netlet. The specific statistics included depend on the network type and attributes.
#'
#' @details This function is not meant to be called directly by users but is instead used internally by the `summary_actor` function to process each netlet separately. It calculates a wide range of actor-level statistics based on the input matrix and the attributes of the netify object.
#'
#' @keywords internal
#' @noRd
actor_stats_for_netlet <- function(mat, obj_attrs, invert_weights_for_igraph = TRUE, other_stats = NULL) {
  # get igraph version for igraph stats
  g <- prep_for_igraph(mat)

  # get weights, force positive, and invert if necessary
  g_wgts <- igraph::E(g)$weight
  if (any(g_wgts < 0)) {
    g_wgts <- g_wgts - min(g_wgts) + 1
  }
  if (invert_weights_for_igraph) {
    g_wgts <- 1 / g_wgts
    g_wgts <- ifelse(is.infinite(g_wgts), 0, g_wgts)
  }

  # symmetric / bin edge case start
  if (obj_attrs$symmetric & obj_attrs$weight_binary) {
    # simple mat stats
    degree <- rowSums(mat > 0, na.rm = TRUE)
    prop_ties <- rowMeans(mat > 0, na.rm = TRUE)

    # get network share
    network_share <- degree / sum(degree, na.rm = TRUE)

    # igraph stats
    closeness <- igraph::closeness(g, normalized = TRUE)
    betweenness <- igraph::betweenness(g, normalized = TRUE)
    eigen_vector <- igraph::eigen_centrality(g)$vector

    # organize output
    out <- data.frame(
      degree = degree,
      prop_ties = prop_ties,
      network_share = network_share,
      closeness = closeness,
      betweenness = betweenness,
      eigen_vector = eigen_vector
    )
  } # symmetric / bin edge case done

  # symmetric / weighted case start
  if (obj_attrs$symmetric & !obj_attrs$weight_binary) {
    # simple mat stats
    degree <- rowSums(mat > 0, na.rm = TRUE)
    prop_ties <- rowMeans(mat > 0, na.rm = TRUE)
    strength_sum <- rowSums(mat, na.rm = TRUE)
    strength_avg <- rowMeans(mat, na.rm = TRUE)
    strength_std <- apply(mat, 1, sd, na.rm = TRUE)
    strength_median <- apply(mat, 1, median, na.rm = TRUE)

    # network share
    network_share <- strength_sum / sum(strength_sum, na.rm = TRUE)

    # igraph stats
    closeness <- igraph::closeness(g, normalized = TRUE, weights = g_wgts)
    betweenness <- igraph::betweenness(g, normalized = TRUE, weights = g_wgts)
    eigen_vector <- igraph::eigen_centrality(g)$vector

    # organize output
    out <- data.frame(
      degree = degree,
      prop_ties = prop_ties,
      strength_sum = strength_sum,
      strength_avg = strength_avg,
      strength_std = strength_std,
      strength_median = strength_median,
      network_share = network_share,
      closeness = closeness,
      betweenness = betweenness,
      eigen_vector = eigen_vector
    )
  } # symmetric / weighted case end

  # not symmetric / bin edge case start
  if (!obj_attrs$symmetric & obj_attrs$weight_binary) {
    # simple mat stats
    degree_out <- rowSums(mat > 0, na.rm = TRUE)
    degree_in <- colSums(mat > 0, na.rm = TRUE)
    if (obj_attrs$mode != 'bipartite') {
      degree_total <- degree_in + degree_out
    } else {
      degree_total <- c(degree_in, degree_out)
    }
    prop_ties_out <- rowMeans(mat > 0, na.rm = TRUE)
    prop_ties_in <- colMeans(mat > 0, na.rm = TRUE)
    if (obj_attrs$mode != 'bipartite') {
      prop_ties_total <- sum(degree_total > 0, na.rm = TRUE) / length(degree_total)
    } else {
      prop_ties_total <- c(prop_ties_in, prop_ties_out)
    }

    # network share
    network_share_in <- degree_in / sum(degree_in, na.rm = TRUE)
    network_share_out <- degree_out / sum(degree_out, na.rm = TRUE)
    network_share_total <- degree_total / sum(degree_total, na.rm = TRUE)

    # igraph stats
    closeness_in <- igraph::closeness(g, mode = 'in', normalized = TRUE)
    closeness_out <- igraph::closeness(g, mode = 'out', normalized = TRUE)
    closeness_all <- igraph::closeness(g, mode = 'all', normalized = TRUE)
    betweenness <- igraph::betweenness(g, normalized = TRUE)
    authority_score <- igraph::authority_score(g)$vector
    hub_score <- igraph::hub_score(g)$vector

    # organize output for bipartite case
    if(obj_attrs$mode=='bipartite'){
      out <- data.frame(
        degree_total = degree_total,
        prop_ties_total = prop_ties_total,
        network_share_total = network_share_total,
        closeness_all = closeness_all,
        betweenness = betweenness,
        authority_score = authority_score,
        hub_score = hub_score
      )
    } # bipartite

    # organize output for non bipartite case
    if(obj_attrs$mode!='bipartite'){
      out <- data.frame(
        degree_in = degree_in,
        degree_out = degree_out,
        degree_total = degree_total,
        prop_ties_in = prop_ties_in,
        prop_ties_out = prop_ties_out,
        prop_ties_total = prop_ties_total,
        network_share_in = network_share_in,
        network_share_out = network_share_out,
        network_share_total = network_share_total,
        closeness_in = closeness_in,
        closeness_out = closeness_out,
        closeness_all = closeness_all,
        betweenness = betweenness,
        authority_score = authority_score,
        hub_score = hub_score
      )
    } # not bipartite
  } # not symmetric / bin edge case done

  # not symmetric / weighted case start
  if (!obj_attrs$symmetric & !obj_attrs$weight_binary) {
    # simple mat stats
    degree_out <- rowSums(mat > 0, na.rm = TRUE)
    degree_in <- colSums(mat > 0, na.rm = TRUE)
    if (obj_attrs$mode != 'bipartite') {
      degree_total <- degree_in + degree_out
    } else {
      degree_total <- c(degree_in, degree_out)
    }
    prop_ties_out <- rowMeans(mat > 0, na.rm = TRUE)
    prop_ties_in <- colMeans(mat > 0, na.rm = TRUE)
    if (obj_attrs$mode != 'bipartite') {
      prop_ties_total <- sum(degree_total > 0, na.rm = TRUE) / length(degree_total)
    } else {
      prop_ties_total <- c(prop_ties_in, prop_ties_out)
    }
    strength_sum_out <- rowSums(mat, na.rm = TRUE)
    strength_sum_in <- colSums(mat, na.rm = TRUE)
    if (obj_attrs$mode != 'bipartite') {
      strength_sum_total <- strength_sum_in + strength_sum_out
    } else {
      strength_sum_total <- c(strength_sum_in, strength_sum_out)
    }
    strength_avg_out <- rowMeans(mat, na.rm = TRUE)
    strength_avg_in <- colMeans(mat, na.rm = TRUE)
    if (obj_attrs$mode != 'bipartite') {
      strength_avg_total <- (strength_avg_in + strength_avg_out) / 2
    } else {
      strength_avg_total <- c(strength_avg_in, strength_avg_out)
    }
    strength_median_out <- apply(mat, 1, median, na.rm = TRUE)
    strength_median_in <- apply(mat, 2, median, na.rm = TRUE)
    if (obj_attrs$mode != 'bipartite') {
      strength_median_total <- (strength_median_in + strength_median_out) / 2
    } else {
      strength_median_total <- c(strength_median_in, strength_median_out)
    }
    strength_std_out <- apply(mat, 1, sd, na.rm = TRUE)
    strength_std_in <- apply(mat, 2, sd, na.rm = TRUE)
    if (obj_attrs$mode != 'bipartite') {
      strength_std_total <- (strength_std_in + strength_std_out) / 2
    } else {
      strength_std_total <- c(strength_std_in, strength_std_out)
    }

    # network share
    network_share_in <- strength_sum_in / sum(strength_sum_in, na.rm = TRUE)
    network_share_out <- strength_sum_out / sum(strength_sum_out, na.rm = TRUE)
    network_share_total <- strength_sum_total / sum(strength_sum_total, na.rm = TRUE)

    # igraph stats
    closeness_in <- igraph::closeness(g, mode = 'in', normalized = TRUE, weights = g_wgts)
    closeness_out <- igraph::closeness(g, mode = 'out', normalized = TRUE, weights = g_wgts)
    closeness_all <- igraph::closeness(g, mode = 'all', normalized = TRUE, weights = g_wgts)
    betweenness <- igraph::betweenness(g, normalized = TRUE, weights = g_wgts)
    authority_score <- igraph::authority_score(g)$vector
    hub_score <- igraph::hub_score(g)$vector

    # organize output for bipartite case
    if(obj_attrs$mode=='bipartite'){
      out <- data.frame(
        degree_total = degree_total,
        prop_ties_total = prop_ties_total,
        strength_sum_total = strength_sum_total,
        strength_avg_total = strength_avg_total,
        strength_std_total = strength_std_total,
        strength_median_total = strength_median_total,
        network_share_total = network_share_total,
        closeness_all = closeness_all,
        betweenness = betweenness,
        authority_score = authority_score,
        hub_score = hub_score
      )
    } # bipartite

    # organize output for non bipartite case
    if(obj_attrs$mode!='bipartite'){
      out <- data.frame(
        degree_in = degree_in,
        degree_out = degree_out,
        degree_total = degree_total,
        prop_ties_in = prop_ties_in,
        prop_ties_out = prop_ties_out,
        prop_ties_total = prop_ties_total,
        strength_sum_in = strength_sum_in,
        strength_sum_out = strength_sum_out,
        strength_sum_total = strength_sum_total,
        strength_avg_in = strength_avg_in,
        strength_avg_out = strength_avg_out,
        strength_avg_total = strength_avg_total,
        strength_std_in = strength_std_in,
        strength_std_out = strength_std_out,
        strength_std_total = strength_std_total,
        strength_median_in = strength_median_in,
        strength_median_out = strength_median_out,
        strength_median_total = strength_median_total,
        network_share_in = network_share_in,
        network_share_out = network_share_out,
        network_share_total = network_share_total,
        closeness_in = closeness_in,
        closeness_out = closeness_out,
        closeness_all = closeness_all,
        betweenness = betweenness,
        authority_score = authority_score,
        hub_score = hub_score
      )
    } # not bipartite

  } # not symmetric / weighted case done

  # calculate any user-supplied stats
  if (!is.null(other_stats)) {
    other_out <- lapply(other_stats, function(stat) {
      stat(mat)
    })
    out <- cbind(out, do.call('cbind', other_out))
  }

  return(out)
}