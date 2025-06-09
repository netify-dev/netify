set.seed(6886)

# library(testthat)
# library(netify)
# devtools::load_all("~/Research/netify_dev/netify")

test_that("summary_actor works for cross-sectional networks", {
  # Create a simple cross-sectional network
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  net <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    symmetric = FALSE,
    weight = 'verbCoop'
  )
  
  # Test basic summary
  actor_stats <- summary_actor(net)
  
  expect_s3_class(actor_stats, "data.frame")
  expect_true("actor" %in% names(actor_stats))
  expect_false("time" %in% names(actor_stats))  # No time for cross-sectional
  expect_false("layer" %in% names(actor_stats))  # No layer column for single layer
  
  # Check for directed network columns
  expect_true(all(c("degree_in", "degree_out", "degree_total") %in% names(actor_stats)))
  expect_true(all(c("closeness_in", "closeness_out", "closeness_all") %in% names(actor_stats)))
  expect_true("betweenness" %in% names(actor_stats))
  expect_true("authority_score" %in% names(actor_stats))
  expect_true("hub_score" %in% names(actor_stats))
  
  # Check that each actor appears once
  expect_equal(nrow(actor_stats), length(unique(actor_stats$actor)))
})

test_that("summary_actor works for longitudinal networks", {
  # Create longitudinal network
  data(icews)
  net_longit <- netify(
    icews,
    actor1 = 'i', actor2 = 'j', time = 'year',
    symmetric = FALSE,
    weight = 'matlConf'
  )
  
  # Test summary
  actor_stats <- summary_actor(net_longit)
  
  expect_s3_class(actor_stats, "data.frame")
  expect_true("actor" %in% names(actor_stats))
  expect_true("time" %in% names(actor_stats))
  
  # Check that we have entries for each actor-time combination
  n_times <- length(unique(icews$year))
  expect_true(nrow(actor_stats) > n_times)  # Multiple actors per time
  
  # Verify time periods are correctly extracted
  expect_true(all(unique(actor_stats$time) %in% as.character(unique(icews$year))))
})

test_that("summary_actor works for symmetric networks", {
  # Create symmetric network
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  net_sym <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    symmetric = TRUE,
    weight = 'verbCoop'
  )
  
  actor_stats <- summary_actor(net_sym)
  
  # Check that undirected-only columns are present
  expect_true("degree" %in% names(actor_stats))
  expect_true("prop_ties" %in% names(actor_stats))
  expect_true("network_share" %in% names(actor_stats))
  expect_true("closeness" %in% names(actor_stats))
  expect_true("betweenness" %in% names(actor_stats))
  expect_true("eigen_vector" %in% names(actor_stats))
  
  # Check that directed-only columns are absent
  expect_false("degree_in" %in% names(actor_stats))
  expect_false("degree_out" %in% names(actor_stats))
  expect_false("authority_score" %in% names(actor_stats))
  expect_false("hub_score" %in% names(actor_stats))
})

test_that("summary_actor works for binary networks", {
  # Create binary network
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  net_bin <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    symmetric = FALSE
  )
  
  actor_stats <- summary_actor(net_bin)
  
  # Check that binary network has degree columns
  expect_true(all(c("degree_in", "degree_out", "degree_total") %in% names(actor_stats)))
  
  # Check that weighted statistics are absent
  expect_false("strength_sum_in" %in% names(actor_stats))
  expect_false("strength_sum_out" %in% names(actor_stats))
  expect_false("strength_avg_in" %in% names(actor_stats))
  expect_false("strength_std_in" %in% names(actor_stats))
})

test_that("summary_actor works for weighted networks", {
  # Create weighted network
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  net_weighted <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    symmetric = FALSE,
    weight = 'verbCoop'
  )
  
  actor_stats <- summary_actor(net_weighted)
  
  # Check that weighted statistics are present
  expect_true(all(c("strength_sum_in", "strength_sum_out", "strength_sum_total") %in% names(actor_stats)))
  expect_true(all(c("strength_avg_in", "strength_avg_out", "strength_avg_total") %in% names(actor_stats)))
  expect_true(all(c("strength_std_in", "strength_std_out", "strength_std_total") %in% names(actor_stats)))
  expect_true(all(c("strength_median_in", "strength_median_out", "strength_median_total") %in% names(actor_stats)))
})

test_that("summary_actor works for multilayer networks", {
  # Create multilayer network
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  
  net1 <- netify(icews_10, actor1='i', actor2='j', weight='verbCoop')
  net2 <- netify(icews_10, actor1='i', actor2='j', weight='matlCoop')
  
  multi_net <- layer_netify(
    list(net1, net2),
    layer_labels = c("verbal", "material")
  )
  
  actor_stats <- summary_actor(multi_net)
  
  expect_true("layer" %in% names(actor_stats))
  expect_equal(sort(unique(actor_stats$layer)), c("material", "verbal"))
  
  # Each actor should appear in each layer
  n_actors <- length(unique(actor_stats$actor))
  expect_equal(nrow(actor_stats), n_actors * 2)  # 2 layers
})

test_that("summary_actor handles custom statistics correctly", {
  # Create test network
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  net <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    weight = 'verbCoop'
  )
  
  # Test single custom function
  max_out <- function(mat) {
    apply(mat, 1, max, na.rm = TRUE)
  }
  
  actor_stats1 <- summary_actor(net, other_stats = list(max_out = max_out))
  expect_true("max_out" %in% names(actor_stats1))
  expect_equal(length(actor_stats1$max_out), nrow(actor_stats1))
  
  # Test multiple custom functions
  max_in <- function(mat) {
    apply(mat, 2, max, na.rm = TRUE)
  }
  
  actor_stats2 <- summary_actor(net, other_stats = list(
    max_out = max_out,
    max_in = max_in
  ))
  expect_true(all(c("max_out", "max_in") %in% names(actor_stats2)))
})

test_that("summary_actor handles bipartite networks", {
  # Create bipartite network data
  bip_data <- data.frame(
    actor1 = rep(letters[1:5], each = 3),
    actor2 = rep(LETTERS[1:3], 5),
    weight = runif(15, 0, 10)
  )
  
  net_bip <- netify(
    bip_data,
    actor1 = 'actor1', actor2 = 'actor2',
    mode = 'bipartite',
    weight = 'weight'
  )
  
  actor_stats <- summary_actor(net_bip)
  
  # Check that all actors from both modes are included
  all_actors <- c(letters[1:5], LETTERS[1:3])
  expect_true(all(all_actors %in% actor_stats$actor))
  expect_equal(nrow(actor_stats), 8)  # 5 + 3 actors
  
  # Bipartite networks should be treated as directed
  expect_true("degree_total" %in% names(actor_stats))
})

test_that("summary_actor handles ego networks", {
  # First create a netify object
  data(icews)
  net <- netify(
    icews,
    actor1 = 'i', actor2 = 'j', time = 'year',
    weight = 'verbCoop'
  )
  
  # Then create ego network
  ego_net <- ego_netify(
    net,
    ego = "United States"
  )
  
  actor_stats <- summary_actor(ego_net)
    
  # Check that it returns something :)
  expect_true(nrow(actor_stats) > 0)
})

test_that("summary_actor produces consistent output structure", {
  # Create different types of networks
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  
  # Test directed network
  net_directed <- netify(icews_10, 'i', 'j', symmetric = FALSE, weight = 'verbCoop')
  stats_directed <- summary_actor(net_directed)
  
  expect_s3_class(stats_directed, "data.frame")
  expect_true("actor" %in% names(stats_directed))
  expect_true(nrow(stats_directed) > 0)
  
  # Test undirected network
  net_undirected <- netify(icews_10, 'i', 'j', symmetric = TRUE, weight = 'verbCoop')
  stats_undirected <- summary_actor(net_undirected)
  
  expect_s3_class(stats_undirected, "data.frame")
  expect_true("actor" %in% names(stats_undirected))
  expect_equal(nrow(stats_undirected), nrow(stats_directed))  # Same actors
  
  # Test binary network
  net_binary <- netify(icews_10, 'i', 'j', symmetric = FALSE)
  stats_binary <- summary_actor(net_binary)
  
  expect_s3_class(stats_binary, "data.frame")
  expect_true("actor" %in% names(stats_binary))
  expect_equal(nrow(stats_binary), nrow(stats_directed))  # Same actors
})

test_that("summary_actor weight inversion parameter works", {
  # Create a network with multiple paths where weights matter
  # Diamond-shaped network: A -> B -> D and A -> C -> D
  # Different weights on each path will affect shortest paths
  test_data <- data.frame(
    actor1 = c("A", "A", "B", "C", "B", "C"),
    actor2 = c("B", "C", "D", "D", "C", "B"),
    weight = c(1,   10,  1,   1,   5,   5)  # Path through B is shorter by weight
  )
  
  net <- netify(
    test_data,
    actor1 = 'actor1', 
    actor2 = 'actor2',
    weight = 'weight',
    symmetric = FALSE
  )
  
  # Get stats with default inversion (TRUE)
  stats_inverted <- summary_actor(net, invert_weights_for_igraph = TRUE)
  
  # Get stats without inversion  
  stats_not_inverted <- summary_actor(net, invert_weights_for_igraph = FALSE)
  
  # Both should return data frames with same structure
  expect_identical(names(stats_inverted), names(stats_not_inverted))
  expect_equal(nrow(stats_inverted), nrow(stats_not_inverted))
  
  # verify the parameter works without error
  expect_s3_class(stats_inverted, "data.frame")
  expect_s3_class(stats_not_inverted, "data.frame")
  
  expect_equal(stats_inverted$strength_sum_out, stats_not_inverted$strength_sum_out)
})

test_that("summary_actor weight inversion produces different centrality values", {
  # Create a network where shortest paths differ based on weight interpretation
  # Path 1: A -> B -> E (weights: 1 + 1 = 2)
  # Path 2: A -> C -> D -> E (weights: 10 + 10 + 10 = 30)
  # When weights are distances: Path 1 is shorter (2 < 30)
  # When weights are inverted (strengths): Path 2 is "shorter" (1/10 + 1/10 + 1/10 = 0.3 < 1/1 + 1/1 = 2)
  
  test_data <- data.frame(
    actor1 = c("A", "A", "B", "C", "D"),
    actor2 = c("B", "C", "E", "D", "E"),
    weight = c(1,  10,  1,  10,  10)
  )
  
  net <- netify(
    test_data,
    actor1 = 'actor1', 
    actor2 = 'actor2',
    weight = 'weight',
    symmetric = FALSE
  )
  
  # Get stats with inversion (weights as strengths)
  stats_inverted <- summary_actor(net, invert_weights_for_igraph = TRUE)
  
  # Get stats without inversion (weights as distances) 
  stats_not_inverted <- summary_actor(net, invert_weights_for_igraph = FALSE)
  
  # Extract values for node B (which is on the short direct path)
  b_inverted <- stats_inverted[stats_inverted$actor == "B",]
  b_not_inverted <- stats_not_inverted[stats_not_inverted$actor == "B",]
  
  # Extract values for node C (which is on the long indirect path)
  c_inverted <- stats_inverted[stats_inverted$actor == "C",]
  c_not_inverted <- stats_not_inverted[stats_not_inverted$actor == "C",]
  
  # Betweenness centrality should differ
  # When weights are distances (not inverted): B should have higher betweenness (on shortest path)
  # When weights are strengths (inverted): C should have higher betweenness
  expect_true(
    b_not_inverted$betweenness != b_inverted$betweenness ||
    c_not_inverted$betweenness != c_inverted$betweenness,
    info = "Betweenness centrality should differ with weight inversion"
  )
  
  # #
  # cat("\nBetweenness without inversion (weights as distances):\n")
  # cat("Node B:", b_not_inverted$betweenness, "\n")
  # cat("Node C:", c_not_inverted$betweenness, "\n")
  
  # cat("\nBetweenness with inversion (weights as strengths):\n")
  # cat("Node B:", b_inverted$betweenness, "\n")
  # cat("Node C:", c_inverted$betweenness, "\n")
})

# Alternative test with a guaranteed difference
test_that("summary_actor weight inversion affects closeness centrality", {
  # Create a star network with different weight patterns
  # Center node A connects to all others
  # Weights vary significantly
  test_data <- data.frame(
    actor1 = c("A", "A", "A", "A"),
    actor2 = c("B", "C", "D", "E"),
    weight = c(1, 2, 10, 20)  # Very different weights
  )
  
  net <- netify(
    test_data,
    actor1 = 'actor1', 
    actor2 = 'actor2',
    weight = 'weight',
    symmetric = TRUE  # Use symmetric to simplify
  )
  
  # Get stats with and without inversion
  stats_inv <- summary_actor(net, invert_weights_for_igraph = TRUE)
  stats_no_inv <- summary_actor(net, invert_weights_for_igraph = FALSE)
  
  # Look at closeness for the center node A
  a_inv <- stats_inv[stats_inv$actor == "A",]
  a_no_inv <- stats_no_inv[stats_no_inv$actor == "A",]
  
  # Closeness should definitely differ for node A
  expect_false(
    isTRUE(all.equal(a_inv$closeness, a_no_inv$closeness)),
    info = "Closeness centrality should differ for central node with weight inversion"
  )
  
  # Also check peripheral nodes
  b_inv <- stats_inv[stats_inv$actor == "B",]
  b_no_inv <- stats_no_inv[stats_no_inv$actor == "B",]
  
  expect_false(
    isTRUE(all.equal(b_inv$closeness, b_no_inv$closeness)),
    info = "Closeness centrality should differ for peripheral nodes with weight inversion"
  )
  
  # #
  # cat("\nCloseness centrality differences:\n")
  # cat("Node A - No inversion:", a_no_inv$closeness, "With inversion:", a_inv$closeness, "\n")
  # cat("Node B - No inversion:", b_no_inv$closeness, "With inversion:", b_inv$closeness, "\n")
})