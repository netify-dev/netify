set.seed(6886)

# library(testthat)
# library(netify)
# devtools::load_all("~/Research/netify_dev/netify")

test_that("summary.netify works for cross-sectional networks", {
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
  summ <- summary(net)
  
  expect_s3_class(summ, "data.frame")
  expect_equal(nrow(summ), 1)  # One row for cross-sectional
  expect_true("density" %in% names(summ))
  expect_true("num_actors" %in% names(summ))
  expect_true("transitivity" %in% names(summ))
  expect_true("reciprocity" %in% names(summ))
  expect_false("layer" %in% names(summ))  # No layer column for single layer
})

test_that("summary.netify works for longitudinal networks", {
  # Create longitudinal network
  data(icews)
  net_longit <- netify(
    icews,
    actor1 = 'i', actor2 = 'j', time = 'year',
    symmetric = FALSE,
    weight = 'matlConf'
  )
  
  # Test summary
  summ <- summary(net_longit)
  
  expect_s3_class(summ, "data.frame")
  expect_equal(nrow(summ), length(unique(icews$year)))
  expect_true("net" %in% names(summ))
  expect_equal(summ$net, as.character(unique(icews$year)))
})

test_that("summary.netify works for symmetric networks", {
  # Create symmetric network
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  net_sym <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    symmetric = TRUE,
    weight = 'verbCoop'
  )
  
  summ <- summary(net_sym)
  
  # Check that directed-only columns are absent
  expect_false("reciprocity" %in% names(summ))
  expect_false("covar_of_row_col_means" %in% names(summ))
  expect_false("competition_col" %in% names(summ))
  
  # Check that renamed columns exist
  expect_true("competition" %in% names(summ))
  expect_true("sd_of_actor_means" %in% names(summ))
})

test_that("summary.netify works for binary networks", {
  # Create binary network
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  net_bin <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    symmetric = FALSE
  )
  
  summ <- suppressWarnings(summary(net_bin))
  
  # Check that weight statistics are absent
  expect_false("mean_edge_weight" %in% names(summ))
  expect_false("sd_edge_weight" %in% names(summ))
  expect_false("median_edge_weight" %in% names(summ))
})

test_that("summary.netify works for multilayer networks", {
  # Create multilayer network
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  
  net1 <- netify(icews_10, actor1='i', actor2='j', weight='verbCoop')
  net2 <- netify(icews_10, actor1='i', actor2='j', weight='matlCoop')
  
  multi_net <- layer_netify(
    list(net1, net2),
    layer_labels = c("verbal", "material")
  )
  
  summ <- summary(multi_net)
  
  expect_equal(nrow(summ), 2)  # Two layers
  expect_true("layer" %in% names(summ))
  expect_equal(summ$layer, c("verbal", "material"))
})

test_that("summary.netify handles custom statistics correctly", {
  # Create test network
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  net <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    weight = 'verbCoop'
  )
  
  # Test single-value custom function
  mean_stat <- function(mat) {
    mean(mat, na.rm = TRUE)
  }
  
  summ1 <- summary(net, other_stats = list(avg = mean_stat))
  expect_true("avg" %in% names(summ1))
  expect_type(summ1$avg, "double")
  
  # Test multi-value custom function
  multi_stat <- function(mat) {
    c(total = sum(mat, na.rm = TRUE),
      nonzero = sum(mat > 0, na.rm = TRUE))
  }
  
  summ2 <- summary(net, other_stats = list(custom = multi_stat))
  expect_true("total" %in% names(summ2))
  expect_true("nonzero" %in% names(summ2))
  
  # Test multiple custom functions
  summ3 <- summary(net, other_stats = list(
    avg = mean_stat,
    custom = multi_stat
  ))
  expect_true(all(c("avg", "custom.total", "custom.nonzero") %in% names(summ3)))
})

test_that("summary.netify handles bipartite networks", {
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
  
  summ <- summary(net_bip)
  
  # Check bipartite-specific columns
  expect_true("num_row_actors" %in% names(summ))
  expect_true("num_col_actors" %in% names(summ))
  expect_false("num_actors" %in% names(summ))
})

test_that("summary.netify handles missing data correctly", {
  # Create network with missing values
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  # Introduce some NAs
  icews_10$verbCoop[sample(nrow(icews_10), 100)] <- NA
  
  net_missing <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    weight = 'verbCoop',
    missing_to_zero = FALSE
  )
  
  summ <- summary(net_missing)
  
  expect_true("prop_edges_missing" %in% names(summ))
  expect_true(summ$prop_edges_missing > 0)
  expect_false(is.na(summ$density))  # Should handle NAs gracefully
})

test_that("summary.netify handles ego networks", {
  # First create a netify object
  data(icews)
  net <- netify(
    icews,
    actor1 = 'i', actor2 = 'j', time = 'year',
    weight = 'verbCoop'
  )
  
  # Then create ego network from the netify object
  ego_net <- ego_netify(
    net,
    ego = "United States"
  )
  
  summ <- summary(ego_net)
  
  # Check ego network structure
  expect_true("net" %in% names(summ))
  expect_true(all(grepl("United States", summ$layer)))
})

test_that("summary.netify produces consistent output structure", {

  # Create different types of networks
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  
  # Test directed network
  net_directed <- netify(icews_10, 'i', 'j', symmetric = FALSE, weight = 'verbCoop')
  summ_directed <- summary(net_directed)
  
  expect_s3_class(summ_directed, "data.frame")
  expect_equal(nrow(summ_directed), 1)  # Cross-sectional has 1 row
  expect_true(all(c("density", "num_edges", "transitivity") %in% names(summ_directed)))
  expect_true("reciprocity" %in% names(summ_directed))  # Directed networks have reciprocity
  
  # Test undirected network
  net_undirected <- netify(icews_10, 'i', 'j', symmetric = TRUE, weight = 'verbCoop')
  summ_undirected <- summary(net_undirected)
  
  expect_s3_class(summ_undirected, "data.frame")
  expect_equal(nrow(summ_undirected), 1)
  expect_true(all(c("density", "num_edges", "transitivity") %in% names(summ_undirected)))
  expect_false("reciprocity" %in% names(summ_undirected))  # Undirected networks don't have reciprocity
  
  # Test binary network
  icews_10_bin = icews_10[icews_10$verbCoop > mean(icews_10$verbCoop, na.rm=TRUE), ]
  net_binary <- netify(icews_10_bin, 'i', 'j', symmetric = FALSE)
  summ_binary <- summary(net_binary)
  
  expect_s3_class(summ_binary, "data.frame")
  expect_equal(nrow(summ_binary), 1)
  expect_true(all(c("density", "num_edges", "transitivity") %in% names(summ_binary)))
  expect_false("mean_edge_weight" %in% names(summ_binary))  # Binary networks don't have weight stats
})
