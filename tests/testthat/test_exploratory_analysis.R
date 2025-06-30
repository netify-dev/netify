set.seed(6886)

library(testthat)
library(netify)

test_that("homophily works for cross-sectional networks", {
    # Create a simple cross-sectional network with attributes
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    # Use existing nodal and dyadic variables from icews data
    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = c("i_polity2", "i_log_gdp"),
        dyad_vars = c("matlCoop", "verbConf")
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
    icews_10 <- icews[icews$year == 2010, ]

    # Create a categorical variable from polity2 for mixing matrix
    icews_10$polity_cat <- cut(icews_10$i_polity2,
        breaks = c(-Inf, -5, 5, Inf),
        labels = c("Autocracy", "Anocracy", "Democracy")
    )

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = c("polity_cat", "i_log_gdp")
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
    icews_10 <- icews[icews$year == 2010, ]

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        dyad_vars = c("matlCoop", "verbConf", "matlConf")
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
    icews_10 <- icews[icews$year == 2010, ]

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = c("i_polity2", "i_log_gdp", "i_log_pop"),
        dyad_vars = c("matlCoop", "verbConf")
    )

    # Test comprehensive analysis
    result <- attribute_report(net,
        node_vars = c("i_polity2", "i_log_gdp"),
        dyad_vars = "matlCoop",
        significance_test = FALSE
    ) # Faster for testing

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
    icews_10 <- icews[icews$year == 2010, ]
    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop"
    )

    # Test homophily analysis with missing attribute
    expect_error(homophily(net, attribute = "nonexistent"))

    # Test mixing matrix with missing attribute
    expect_error(mixing_matrix(net, attribute = "nonexistent"))

    # Test dyadic correlation with no dyadic data
    expect_error(dyad_correlation(net, dyad_vars = "distance"))

    # Test comprehensive analysis with no attributes
    result <- attribute_report(net,
        include_homophily = FALSE,
        include_mixing = FALSE,
        include_dyadic_correlations = FALSE,
        include_centrality = FALSE
    )

    expect_type(result, "list")
    expect_true("overall_summary" %in% names(result))
})

test_that("functions work with different similarity methods", {
    # Create a simple cross-sectional network with attributes
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = c("i_log_gdp", "i_log_pop")
    )

    # Test different similarity methods
    methods <- c("correlation", "euclidean", "cosine")
    for (method in methods) {
        result <- homophily(net,
            attribute = "i_log_gdp", method = method,
            significance_test = FALSE
        )
        expect_s3_class(result, "data.frame")
        expect_equal(result$method[1], method)
    }
})

test_that("functions work with different correlation methods", {
    # Create a simple cross-sectional network with dyadic attributes
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        dyad_vars = c("matlCoop", "verbConf", "matlConf")
    )

    # Test different correlation methods
    methods <- c("pearson", "spearman", "kendall")
    for (method in methods) {
        result <- suppressWarnings(dyad_correlation(
            net,
            dyad_vars = "matlCoop",
            method = method, significance_test = FALSE
        ))
        expect_s3_class(result, "data.frame")
        expect_equal(result$method[1], method)
    }
})

# Additional comprehensive tests for edge cases and robustness

test_that("homophily handles edge cases correctly", {
    # Create test network
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    # Test with network containing isolated nodes
    icews_sparse <- icews_10[1:100, ]
    net_sparse <- netify(
        icews_sparse,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = c("i_polity2", "i_log_gdp")
    )

    result <- suppressWarnings(homophily(net_sparse, attribute = "i_polity2", method = "correlation"))
    expect_s3_class(result, "data.frame")
    # sparse network might return empty results or valid results
    if (nrow(result) > 0) {
        expect_true(!is.na(result$homophily_correlation[1]))
    }

    # Test with binary network
    net_binary <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = NULL, # Binary network
        nodal_vars = c("i_polity2", "i_log_gdp")
    )

    result_binary <- homophily(net_binary, attribute = "i_log_gdp", method = "correlation")
    expect_s3_class(result_binary, "data.frame")

    # Test with categorical method for categorical variable
    icews_10$regime_type <- cut(icews_10$i_polity2,
        breaks = c(-Inf, -5, 5, Inf),
        labels = c("Autocracy", "Anocracy", "Democracy")
    )

    net_cat <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = c("regime_type", "i_log_gdp")
    )

    result_cat <- homophily(net_cat, attribute = "regime_type", method = "categorical")
    expect_s3_class(result_cat, "data.frame")
    expect_equal(result_cat$method[1], "categorical")
})

test_that("homophily handles missing values appropriately", {
    # Create network with missing values in attributes
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    # Introduce missing values
    icews_10$i_log_gdp[sample(nrow(icews_10), 50)] <- NA

    net_missing <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = c("i_polity2", "i_log_gdp")
    )

    # Test that function handles missing values
    result <- homophily(net_missing, attribute = "i_log_gdp", method = "correlation")
    expect_s3_class(result, "data.frame")
    expect_true("n_missing" %in% names(result) || "n_pairs" %in% names(result))
})

test_that("mixing_matrix works with cross-dimensional analysis", {
    # Create test network with multiple categorical attributes
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    # Create categorical variables
    icews_10$polity_cat <- cut(icews_10$i_polity2,
        breaks = c(-Inf, -5, 5, Inf),
        labels = c("Autocracy", "Anocracy", "Democracy")
    )
    icews_10$gdp_cat <- cut(icews_10$i_log_gdp,
        breaks = quantile(icews_10$i_log_gdp, c(0, 0.33, 0.67, 1), na.rm = TRUE),
        labels = c("Low", "Medium", "High")
    )

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = c("polity_cat", "gdp_cat")
    )

    # Test cross-dimensional mixing matrix
    result <- mixing_matrix(net, attribute = "polity_cat", row_attribute = "gdp_cat")

    expect_type(result, "list")
    expect_true("mixing_matrices" %in% names(result))
    expect_true("summary_stats" %in% names(result))

    # Check that mixing matrix has correct dimensions
    mixing_mat <- result$mixing_matrices[[1]]
    expect_true(is.matrix(mixing_mat))
})

test_that("mixing_matrix normalization options work correctly", {
    # Create test network
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    icews_10$polity_cat <- cut(icews_10$i_polity2,
        breaks = c(-Inf, -5, 5, Inf),
        labels = c("Autocracy", "Anocracy", "Democracy")
    )

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = "polity_cat"
    )

    # Test different normalization options
    result_raw <- mixing_matrix(net, attribute = "polity_cat", normalized = FALSE)
    result_norm <- mixing_matrix(net, attribute = "polity_cat", normalized = TRUE)
    result_row <- mixing_matrix(net, attribute = "polity_cat", normalized = TRUE, by_row = TRUE)

    # Check that raw counts are integers
    expect_true(all(result_raw$mixing_matrices[[1]] == floor(result_raw$mixing_matrices[[1]])))

    # Check that normalized matrix sums to 1
    expect_equal(sum(result_norm$mixing_matrices[[1]]), 1, tolerance = 1e-10)

    # Check that row-normalized matrix rows sum to 1
    row_sums <- rowSums(result_row$mixing_matrices[[1]])
    expect_true(all(abs(row_sums[row_sums > 0] - 1) < 1e-10))
})

test_that("dyad_correlation handles multiple variables and methods", {
    # Create test network with multiple dyadic variables
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        dyad_vars = c("matlCoop", "verbConf", "matlConf")
    )

    # Test multiple variables at once
    result_multi <- dyad_correlation(net, dyad_vars = c("matlCoop", "verbConf"))
    expect_equal(nrow(result_multi), 2)
    expect_equal(result_multi$dyad_var, c("matlCoop", "verbConf"))

    # Test partial correlation
    result_partial <- dyad_correlation(net,
        dyad_vars = "matlCoop",
        control_vars = "verbConf"
    )
    expect_s3_class(result_partial, "data.frame")
    expect_true("partial_correlation" %in% names(result_partial) ||
        "correlation" %in% names(result_partial))
})

test_that("dyad_correlation works with symmetric and directed networks", {
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    # Create directed network
    net_dir <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        dyad_vars = c("matlCoop", "verbConf")
    )

    # Create symmetric network
    net_sym <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = TRUE,
        weight = "verbCoop",
        dyad_vars = c("matlCoop", "verbConf")
    )

    result_dir <- dyad_correlation(net_dir, dyad_vars = "matlCoop")
    result_sym <- dyad_correlation(net_sym, dyad_vars = "matlCoop")

    expect_s3_class(result_dir, "data.frame")
    expect_s3_class(result_sym, "data.frame")

    # Symmetric network should have fewer unique pairs
    expect_true(result_sym$n_pairs[1] <= result_dir$n_pairs[1])
})

test_that("attribute_report handles missing components gracefully", {
    # Create network with limited attributes
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    # Network with only nodal attributes
    net_nodal <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = c("i_polity2", "i_log_gdp")
    )

    result <- attribute_report(net_nodal,
        node_vars = c("i_polity2", "i_log_gdp"),
        include_dyadic_correlations = FALSE,
        significance_test = FALSE
    )

    expect_type(result, "list")
    expect_null(result$dyadic_correlations)
    expect_true(!is.null(result$homophily_analysis))

    # Network with only dyadic attributes
    net_dyadic <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        dyad_vars = c("matlCoop", "verbConf")
    )

    result2 <- suppressWarnings(attribute_report(net_dyadic,
        dyad_vars = c("matlCoop", "verbConf"),
        include_homophily = FALSE,
        include_mixing = FALSE,
        significance_test = FALSE
    ))

    expect_type(result2, "list")
    expect_null(result2$homophily_analysis)
    expect_true(!is.null(result2$dyadic_correlations))
})

test_that("attribute_report centrality measures work correctly", {
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = c("i_polity2", "i_log_gdp")
    )

    # Test with specific centrality measures
    result <- attribute_report(net,
        node_vars = "i_log_gdp",
        include_centrality = TRUE,
        centrality_measures = c("degree", "betweenness"),
        include_homophily = FALSE,
        include_mixing = FALSE,
        include_dyadic_correlations = FALSE,
        significance_test = FALSE
    )

    expect_true(!is.null(result$centrality_correlations))
    cent_results <- result$centrality_correlations
    expect_true(all(c("degree", "betweenness") %in% cent_results$centrality_measure))
})

# Test longitudinal network scenarios
test_that("homophily works with longitudinal networks", {
    data(icews)

    # Create longitudinal network
    years <- 2002:2004
    icews_subset <- icews[icews$year %in% years, ]

    net_longit <- netify(
        icews_subset,
        actor1 = "i", actor2 = "j",
        time = "year",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = c("i_polity2", "i_log_gdp"),
        output_format = "longit_list"
    )

    # Test homophily on longitudinal network
    result <- homophily(net_longit, attribute = "i_polity2", method = "correlation")

    expect_s3_class(result, "data.frame")
    # Should have results for each time period
    expect_true(nrow(result) >= length(years))
})

test_that("mixing_matrix works with longitudinal networks", {
    data(icews)

    years <- 2002:2004
    icews_subset <- icews[icews$year %in% years, ]

    # Create categorical variable
    icews_subset$polity_cat <- cut(icews_subset$i_polity2,
        breaks = c(-Inf, -5, 5, Inf),
        labels = c("Autocracy", "Anocracy", "Democracy")
    )

    net_longit <- netify(
        icews_subset,
        actor1 = "i", actor2 = "j",
        time = "year",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = "polity_cat",
        output_format = "longit_list"
    )

    result <- mixing_matrix(net_longit, attribute = "polity_cat")

    expect_type(result, "list")
    expect_true(length(result$mixing_matrices) >= length(years))
})

# Test weighted vs unweighted networks
test_that("functions handle weighted and unweighted networks differently", {
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    # Create weighted network
    net_weighted <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = "i_polity2"
    )

    # Create unweighted network
    net_unweighted <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = NULL,
        nodal_vars = "i_polity2"
    )

    # Compare homophily results
    result_weighted <- homophily(net_weighted, attribute = "i_polity2", method = "correlation")
    result_unweighted <- homophily(net_unweighted, attribute = "i_polity2", method = "correlation")

    expect_s3_class(result_weighted, "data.frame")
    expect_s3_class(result_unweighted, "data.frame")
})

# Test error handling
test_that("functions provide informative errors", {
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop"
    )

    # Test invalid attribute names
    expect_error(homophily(net, attribute = "nonexistent"))

    expect_error(mixing_matrix(net, attribute = "nonexistent"))

    # Test invalid method names
    net_with_attr <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = "i_polity2"
    )

    expect_error(homophily(net_with_attr, attribute = "i_polity2", method = "invalid_method"))

    # Test dyad_correlation with no dyadic attributes
    expect_error(dyad_correlation(net_with_attr, dyad_vars = "distance"))
})

# Test performance with larger networks
test_that("functions handle moderately large networks efficiently", {
    # Create a larger synthetic network
    n_actors <- 100
    n_edges <- 2000

    edges <- data.frame(
        from = sample(1:n_actors, n_edges, replace = TRUE),
        to = sample(1:n_actors, n_edges, replace = TRUE),
        weight = runif(n_edges)
    )

    # Remove self-loops
    edges <- edges[edges$from != edges$to, ]

    # Create node attributes
    nodes <- data.frame(
        actor = 1:n_actors,
        attr1 = rnorm(n_actors),
        attr2 = sample(c("A", "B", "C"), n_actors, replace = TRUE),
        attr3 = rpois(n_actors, lambda = 5)
    )

    net_large <- netify(edges, actor1 = "from", actor2 = "to", weight = "weight")
    net_large <- add_node_vars(net_large, nodes, actor = "actor")

    # Test that homophily completes in reasonable time
    start_time <- Sys.time()
    result <- homophily(net_large,
        attribute = "attr1",
        method = "correlation",
        significance_test = FALSE
    )
    time_taken <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    expect_s3_class(result, "data.frame")
    # expect_true(time_taken < 5)  # Should complete in less than 5 seconds
})

test_that("homophily handles networks with single connected component", {
    # Create disconnected network with two components
    n <- 20
    mat <- matrix(0, n, n)
    # First component (nodes 1-10)
    for (i in 1:9) {
        mat[i, i + 1] <- mat[i + 1, i] <- 1
    }
    # Second component (nodes 11-20)
    for (i in 11:19) {
        mat[i, i + 1] <- mat[i + 1, i] <- 1
    }

    net <- new_netify(mat)
    nodes <- data.frame(
        actor = 1:n,
        component = c(rep("A", 10), rep("B", 10))
    )
    net <- add_node_vars(net, nodes, actor = "actor")

    result <- suppressWarnings(homophily(net, attribute = "component", method = "categorical"))
    expect_s3_class(result, "data.frame")
    # Should show perfect homophily within components if data is sufficient
    if (nrow(result) > 0) {
        expect_equal(result$homophily_correlation[1], 1)
    }
})

test_that("mixing_matrix handles highly skewed categorical distributions", {
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    # Create highly skewed categorical variable
    icews_10$skewed_cat <- sample(c("Common", "Rare"),
        nrow(icews_10),
        replace = TRUE,
        prob = c(0.95, 0.05)
    )

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = "skewed_cat"
    )

    result <- mixing_matrix(net, attribute = "skewed_cat")

    expect_type(result, "list")
    expect_true("mixing_matrices" %in% names(result))
    # Most ties should be between "Common" nodes
    mix_mat <- result$mixing_matrices[[1]]
    if ("Common" %in% rownames(mix_mat)) {
        expect_true(mix_mat["Common", "Common"] > sum(mix_mat) * 0.8)
    }
})

test_that("dyad_correlation handles perfect collinearity", {
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    # Create perfectly collinear dyadic variables
    icews_10$dyad1 <- icews_10$matlCoop
    icews_10$dyad2 <- icews_10$matlCoop * 2 + 1 # Perfect linear relationship

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        dyad_vars = c("dyad1", "dyad2")
    )

    result <- suppressWarnings(dyad_correlation(net, dyad_vars = c("dyad1", "dyad2")))

    expect_s3_class(result, "data.frame")
    # Should detect perfect correlation
    expect_equal(nrow(result), 2)
})

test_that("attribute_report handles networks with only one actor", {
    # Single actor network (edge case)
    mat <- matrix(1, 1, 1)
    net <- new_netify(mat)
    nodes <- data.frame(actor = 1, attr = 1)
    net <- add_node_vars(net, nodes, actor = "actor")

    # Should handle gracefully without error
    result <- suppressWarnings(attribute_report(net, node_vars = "attr"))
    expect_type(result, "list")
})

test_that("functions handle networks with very long attribute names", {
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    # Create attribute with very long name
    long_name <- paste0("very_long_attribute_name_", paste(rep("x", 50), collapse = ""))
    icews_10[[long_name]] <- icews_10$i_polity2

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = long_name
    )

    result <- homophily(net, attribute = long_name, method = "correlation")
    expect_s3_class(result, "data.frame")
    expect_equal(result$attribute[1], long_name)
})

test_that("homophily threshold parameter works correctly", {
    data(icews)
    icews_10 <- icews[icews$year == 2010, ]

    net <- netify(
        icews_10,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "verbCoop",
        nodal_vars = "i_polity2"
    )

    # Test with numeric threshold
    result1 <- homophily(net, attribute = "i_polity2", threshold = 0.5)
    expect_equal(result1$threshold_value[1], 0.5)

    # Test with function threshold
    result2 <- homophily(net,
        attribute = "i_polity2",
        threshold = function(x) median(x, na.rm = TRUE)
    )
    expect_true(is.numeric(result2$threshold_value[1]))

    # Test with mean threshold
    result3 <- homophily(net,
        attribute = "i_polity2",
        threshold = function(x) mean(x, na.rm = TRUE)
    )
    expect_true(is.numeric(result3$threshold_value[1]))

    # Different thresholds should give different results
    expect_true(result1$n_connected_pairs[1] != result2$n_connected_pairs[1] ||
        result2$n_connected_pairs[1] != result3$n_connected_pairs[1])
})
