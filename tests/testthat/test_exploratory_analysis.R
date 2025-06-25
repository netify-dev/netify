set.seed(6886)

# library(testthat)
# library(netify)

test_that("homophily works for cross-sectional networks", {
  # Create a simple cross-sectional network with attributes
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  
  # Use existing nodal and dyadic variables from icews data
  net <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    symmetric = FALSE,
    weight = 'verbCoop',
    nodal_vars = c('i_polity2', 'i_log_gdp'),
    dyad_vars = c('matlCoop', 'verbConf')
  )
  
  # Test categorical homophily with polity2
  result_cat <- homophily(net, attribute = "i_polity2", method = "correlation")
  
  expect_s3_class(result_cat, "data.frame")
  expect_true("homophily_correlation" %in% names(result_cat))
  expect_true("attribute" %in% names(result_cat))
  expect_equal(result_cat$attribute[1], "i_polity2")
  expect_equal(result_cat$method[1], "correlation")
  
  # Test continuous homophily with GDP
  result_cont <- homophily(net, attribute = "i_log_gdp", method = "correlation")
  
  expect_s3_class(result_cont, "data.frame")
  expect_equal(result_cont$attribute[1], "i_log_gdp")
  expect_equal(result_cont$method[1], "correlation")
  
  # Test without significance testing
  result_no_sig <- homophily(net, attribute = "i_polity2", method = "correlation", significance_test = FALSE)
  
  expect_true(is.na(result_no_sig$p_value[1]))
})

test_that("mixing_matrix works for cross-sectional networks", {
  # Create a simple cross-sectional network with attributes
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  
  # Create a categorical variable from polity2 for mixing matrix
  icews_10$polity_cat <- cut(icews_10$i_polity2, 
                              breaks = c(-Inf, -5, 5, Inf), 
                              labels = c("Autocracy", "Anocracy", "Democracy"))
  
  net <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    symmetric = FALSE,
    weight = 'verbCoop',
    nodal_vars = c('polity_cat', 'i_log_gdp')
  )
  
  # Test mixing matrix
  result <- mixing_matrix(net, attribute = "polity_cat")
  
  expect_type(result, "list")
  expect_true("mixing_matrices" %in% names(result))
  expect_true("summary_stats" %in% names(result))
  expect_s3_class(result$summary_stats, "data.frame")
  expect_true("assortativity" %in% names(result$summary_stats))
  expect_true("diagonal_proportion" %in% names(result$summary_stats))
  
  # Check mixing matrix structure
  mixing_matrix <- result$mixing_matrices[[1]]
  expect_true(is.matrix(mixing_matrix))
  expect_equal(rownames(mixing_matrix), colnames(mixing_matrix))
})

test_that("dyad_correlation works with dyadic attributes", {
  # Create a simple cross-sectional network
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  
  net <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    symmetric = FALSE,
    weight = 'verbCoop',
    dyad_vars = c('matlCoop', 'verbConf', 'matlConf')
  )
  
  # Test dyadic correlation analysis
  result <- dyad_correlation(net, dyad_vars = "matlCoop")
  
  expect_s3_class(result, "data.frame")
  expect_true("correlation" %in% names(result))
  expect_true("dyad_var" %in% names(result))
  expect_equal(result$dyad_var[1], "matlCoop")
  expect_true("n_pairs" %in% names(result))
  expect_true(result$n_pairs[1] > 0)
})

test_that("attribute_report works with comprehensive analysis", {
  # Create a simple cross-sectional network with both nodal and dyadic attributes
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  
  net <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    symmetric = FALSE,
    weight = 'verbCoop',
    nodal_vars = c('i_polity2', 'i_log_gdp', 'i_log_pop'),
    dyad_vars = c('matlCoop', 'verbConf')
  )
  
  # Test comprehensive analysis
  result <- attribute_report(net, 
                                    node_vars = c("i_polity2", "i_log_gdp"),
                                    dyad_vars = "matlCoop",
                                    significance_test = FALSE)  # Faster for testing
  
  expect_type(result, "list")
  expect_true("homophily_analysis" %in% names(result))
  expect_true("mixing_analysis" %in% names(result))
  expect_true("dyadic_correlations" %in% names(result))
  expect_true("centrality_correlations" %in% names(result))
  expect_true("attribute_summaries" %in% names(result))
  expect_true("overall_summary" %in% names(result))
  
  # Check individual components if they exist
  if (!is.null(result$homophily_analysis)) {
    expect_s3_class(result$homophily_analysis, "data.frame")
  }
  if (!is.null(result$mixing_analysis)) {
    expect_type(result$mixing_analysis, "list")
  }
  if (!is.null(result$dyadic_correlations)) {
    expect_s3_class(result$dyadic_correlations, "data.frame")
  }
  if (!is.null(result$centrality_correlations)) {
    expect_s3_class(result$centrality_correlations, "data.frame")
  }
  if (!is.null(result$attribute_summaries)) {
    expect_type(result$attribute_summaries, "list")
  }
  expect_type(result$overall_summary, "list")
})

test_that("functions handle missing data gracefully", {
  # Create network without attributes
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  net <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    symmetric = FALSE,
    weight = 'verbCoop'
  )
  
  # Test homophily analysis with missing attribute
  expect_error(homophily(net, attribute = "nonexistent"))
  
  # Test mixing matrix with missing attribute
  expect_error(mixing_matrix(net, attribute = "nonexistent"))
  
  # Test dyadic correlation with no dyadic data
  expect_error(dyad_correlation(net, dyad_vars = "distance"))
  
  # Test comprehensive analysis with no attributes
  result <- attribute_report(net, include_homophily = FALSE, 
                                    include_mixing = FALSE, 
                                    include_dyadic_correlations = FALSE,
                                    include_centrality = FALSE)
  
  expect_type(result, "list")
  expect_true("overall_summary" %in% names(result))
})

test_that("functions work with different similarity methods", {
  # Create a simple cross-sectional network with attributes
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  
  net <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    symmetric = FALSE,
    weight = 'verbCoop',
    nodal_vars = c('i_log_gdp', 'i_log_pop')
  )
  
  # Test different similarity methods
  methods <- c("correlation", "euclidean", "cosine")
  for (method in methods) {
    result <- homophily(net, attribute = "i_log_gdp", method = method, 
                              significance_test = FALSE)
    expect_s3_class(result, "data.frame")
    expect_equal(result$method[1], method)
  }
})

test_that("functions work with different correlation methods", {
  # Create a simple cross-sectional network with dyadic attributes
  data(icews)
  icews_10 <- icews[icews$year == 2010,]
  
  net <- netify(
    icews_10,
    actor1 = 'i', actor2 = 'j',
    symmetric = FALSE,
    weight = 'verbCoop',
    dyad_vars = c('matlCoop', 'verbConf', 'matlConf')
  )
  
  # Test different correlation methods
  methods <- c("pearson", "spearman", "kendall")
  for (method in methods) {
    result <- suppressWarnings(dyad_correlation(
      net, dyad_vars = "matlCoop", 
      method = method, significance_test = FALSE))
    expect_s3_class(result, "data.frame")
    expect_equal(result$method[1], method)
  }
})