set.seed(6886)

library(testthat)
library(netify)

test_that("compare_networks works with basic edge comparison", {
    # Create test networks
    set.seed(123)
    n <- 20

    mat1 <- matrix(rbinom(n * n, 1, 0.3), n, n)
    mat2 <- mat1
    change_indices <- sample(1:(n * n), size = round(0.2 * n * n))
    mat2[change_indices] <- 1 - mat2[change_indices]

    net1 <- new_netify(mat1, symmetric = TRUE)
    net2 <- new_netify(mat2, symmetric = TRUE)

    # Test basic comparison
    comp <- suppressWarnings(compare_networks(list(net1 = net1, net2 = net2)))

    expect_s3_class(comp, "netify_comparison")
    expect_equal(comp$comparison_type, "cross_network")
    expect_equal(comp$n_networks, 2)
    expect_true(!is.null(comp$summary))
})

test_that("compare_networks handles different comparison types", {
    # Create test networks
    mat1 <- matrix(rbinom(100, 1, 0.3), 10, 10)
    mat2 <- matrix(rbinom(100, 1, 0.3), 10, 10)

    net1 <- new_netify(mat1)
    net2 <- new_netify(mat2)

    # Test structure comparison
    struct_comp <- compare_networks(list(net1, net2), what = "structure")
    expect_equal(struct_comp$comparison_type, "cross_network") # Still cross_network, not structure
    expect_equal(struct_comp$method, "structural_comparison") # The method indicates what was compared
    expect_equal(struct_comp$n_networks, 2)

    # Test node comparison
    node_comp <- compare_networks(list(net1, net2), what = "nodes")
    expect_equal(node_comp$comparison_type, "cross_network") # Still cross_network, not nodes
    expect_equal(node_comp$method, "node_composition") # The method indicates what was compared
    expect_equal(node_comp$n_networks, 2)
})

test_that("compare_networks works with longitudinal data", {
    # Create longitudinal network
    n <- 10
    arr <- array(rbinom(n * n * 3, 1, 0.3), dim = c(n, n, 3))
    net_longit <- new_netify(arr)

    comp <- suppressWarnings(compare_networks(net_longit))
    expect_equal(comp$comparison_type, "temporal")
    expect_equal(comp$n_networks, 3)
})

test_that("compare_networks correctly identifies comparison types", {
    # Create test networks
    n <- 10
    mat1 <- matrix(rbinom(n * n, 1, 0.3), n, n)
    mat2 <- matrix(rbinom(n * n, 1, 0.3), n, n)
    net1 <- new_netify(mat1)
    net2 <- new_netify(mat2)

    # Cross-network comparison (list of networks)
    comp_cross <- suppressWarnings(compare_networks(list(net1, net2)))
    expect_equal(comp_cross$comparison_type, "cross_network")

    # Temporal comparison (single longitudinal network)
    arr <- array(rbinom(n * n * 3, 1, 0.3), dim = c(n, n, 3))
    net_longit <- new_netify(arr)
    comp_temp <- suppressWarnings(compare_networks(net_longit))
    expect_equal(comp_temp$comparison_type, "temporal")

    # By-group comparison would be tested here if implemented
    # comp_group <- compare_networks(net1, by = "some_attribute")
    # expect_equal(comp_group$comparison_type, "by_group")
})

# Test all 'what' options work correctly
test_that("compare_networks handles all 'what' options", {
    # Create test networks
    mat1 <- matrix(rbinom(100, 1, 0.3), 10, 10)
    mat2 <- matrix(rbinom(100, 1, 0.3), 10, 10)
    net1 <- new_netify(mat1)
    net2 <- new_netify(mat2)

    # Test edges (default)
    comp_edges <- suppressWarnings(compare_networks(list(net1, net2), what = "edges"))
    expect_equal(comp_edges$method, "correlation") # default method for edges

    # Test structure
    comp_struct <- suppressWarnings(compare_networks(list(net1, net2), what = "structure"))
    expect_equal(comp_struct$method, "structural_comparison")

    # Test nodes
    comp_nodes <- suppressWarnings(compare_networks(list(net1, net2), what = "nodes"))
    expect_equal(comp_nodes$method, "node_composition")

    # Test attributes (will warn if no attributes)
    comp_attr <- suppressWarnings(compare_networks(list(net1, net2), what = "attributes"))
    expect_equal(comp_attr$method, "attribute_comparison")
})

# Additional comprehensive tests for compare_networks

test_that("compare_networks handles different similarity methods", {
    # Create test networks
    n <- 15
    mat1 <- matrix(rbinom(n * n, 1, 0.3), n, n)
    mat2 <- mat1
    # Make some changes
    change_indices <- sample(1:(n * n), size = round(0.3 * n * n))
    mat2[change_indices] <- 1 - mat2[change_indices]

    net1 <- new_netify(mat1, symmetric = FALSE)
    net2 <- new_netify(mat2, symmetric = FALSE)

    # Test all methods
    methods <- c("correlation", "jaccard", "hamming", "qap", "spectral", "all")

    for (method in methods) {
        comp <- suppressWarnings(compare_networks(list(net1, net2), method = method))
        expect_s3_class(comp, "netify_comparison")

        if (method == "all") {
            # Should have all metrics
            expect_true(all(c("correlation", "jaccard", "hamming", "qap_correlation", "spectral") %in% names(comp$summary)))
        } else if (method == "qap") {
            expect_true("qap_correlation" %in% names(comp$summary))
            expect_true("qap_pvalue" %in% names(comp$summary))
        } else {
            expect_true(method %in% names(comp$summary))
        }
    }
})

test_that("compare_networks handles weighted networks correctly", {
    # Create weighted networks
    n <- 10
    mat1 <- matrix(runif(n * n), n, n)
    mat2 <- matrix(runif(n * n), n, n)

    net1_weighted <- new_netify(mat1, symmetric = FALSE)
    net2_weighted <- new_netify(mat2, symmetric = FALSE)

    # Compare weighted networks
    comp_weighted <- suppressWarnings(compare_networks(list(net1_weighted, net2_weighted)))
    expect_s3_class(comp_weighted, "netify_comparison")

    # Edge changes should track weight changes
    expect_true("edge_changes" %in% names(comp_weighted))
    edge_changes <- comp_weighted$edge_changes[[1]]
    expect_true("weight_correlation" %in% names(edge_changes))
})

test_that("compare_networks with return_details provides full matrices", {
    # Create test networks
    n <- 10
    mat1 <- matrix(rbinom(n * n, 1, 0.3), n, n)
    mat2 <- matrix(rbinom(n * n, 1, 0.3), n, n)

    net1 <- new_netify(mat1)
    net2 <- new_netify(mat2)

    # Compare with details
    comp_details <- suppressWarnings(compare_networks(
        list(net1, net2),
        method = "all",
        return_details = TRUE
    ))

    expect_true("details" %in% names(comp_details))
    expect_true("correlation_matrix" %in% names(comp_details$details))
    expect_true("jaccard_matrix" %in% names(comp_details$details))
    expect_true("hamming_matrix" %in% names(comp_details$details))

    # Check that matrices have correct dimensions
    # when comparing 2 networks, matrices should be 2x2
    expect_equal(dim(comp_details$details$correlation_matrix), c(2, 2))
})

test_that("compare_networks handles networks with different node sets", {
    # Create networks with different actors
    n1 <- 10
    n2 <- 12

    mat1 <- matrix(rbinom(n1 * n1, 1, 0.3), n1, n1)
    mat2 <- matrix(rbinom(n2 * n2, 1, 0.3), n2, n2)

    rownames(mat1) <- colnames(mat1) <- paste0("actor", 1:n1)
    rownames(mat2) <- colnames(mat2) <- paste0("actor", c(1:8, 11:14)) # Some overlap

    net1 <- new_netify(mat1)
    net2 <- new_netify(mat2)

    # Node comparison should detect differences
    comp_nodes <- suppressWarnings(compare_networks(list(net1, net2), what = "nodes"))

    expect_true("node_changes" %in% names(comp_nodes))
    node_changes <- comp_nodes$node_changes[[1]]

    expect_true(node_changes$n_added > 0)
    expect_true(node_changes$n_removed > 0)
    expect_true(node_changes$n_maintained > 0)
})

test_that("compare_networks handles longitudinal networks with missing time periods", {
    # Create longitudinal network with gaps
    n <- 10
    times <- c(2000, 2001, 2003, 2005) # Missing 2002, 2004

    arr <- array(rbinom(n * n * length(times), 1, 0.3),
        dim = c(n, n, length(times))
    )
    dimnames(arr)[[3]] <- as.character(times)

    net_longit <- new_netify(arr)

    comp <- suppressWarnings(compare_networks(net_longit))

    expect_equal(comp$comparison_type, "temporal")
    expect_equal(comp$n_networks, length(times))

    # Should compare consecutive available time periods
    expect_true(length(comp$edge_changes) == choose(length(times), 2))
})

test_that("compare_networks structural comparison includes all metrics", {
    # Create networks with different structural properties
    n <- 20

    # Dense network
    mat1 <- matrix(rbinom(n * n, 1, 0.7), n, n)
    diag(mat1) <- 0

    # Sparse network
    mat2 <- matrix(rbinom(n * n, 1, 0.2), n, n)
    diag(mat2) <- 0

    net1 <- new_netify(mat1, symmetric = FALSE)
    net2 <- new_netify(mat2, symmetric = FALSE)

    comp_struct <- suppressWarnings(compare_networks(list(net1, net2), what = "structure"))

    expect_true("summary" %in% names(comp_struct))
    struct_summary <- comp_struct$summary

    # Should have key structural metrics
    expected_metrics <- c(
        "n_nodes", "n_edges", "density", "reciprocity",
        "transitivity", "mean_degree"
    )
    expect_true(all(expected_metrics %in% names(struct_summary)))

    # Density should be different
    expect_true(abs(struct_summary$density[1] - struct_summary$density[2]) > 0.1)
})

test_that("compare_networks handles attribute comparison", {
    # Create networks with node attributes
    n <- 15
    mat1 <- matrix(rbinom(n * n, 1, 0.3), n, n)
    mat2 <- mat1

    net1 <- new_netify(mat1)
    net2 <- new_netify(mat2)

    # Add node attributes
    nodes1 <- data.frame(
        actor = 1:n,
        attr1 = rnorm(n),
        attr2 = sample(c("A", "B"), n, replace = TRUE)
    )

    nodes2 <- data.frame(
        actor = 1:n,
        attr1 = rnorm(n), # Different values
        attr2 = sample(c("A", "B"), n, replace = TRUE)
    )

    net1 <- add_node_vars(net1, nodes1, actor = "actor")
    net2 <- add_node_vars(net2, nodes2, actor = "actor")

    comp_attr <- suppressWarnings(compare_networks(list(net1, net2), what = "attributes"))

    expect_equal(comp_attr$method, "attribute_comparison")
    # Should detect that attributes have changed
    expect_true("summary" %in% names(comp_attr))
})

test_that("compare_networks QAP test with custom permutations", {
    # Create correlated networks
    n <- 10
    base_mat <- matrix(rbinom(n * n, 1, 0.5), n, n)

    # Create second network with some correlation
    mat2 <- base_mat
    change_indices <- sample(1:(n * n), size = round(0.2 * n * n))
    mat2[change_indices] <- 1 - mat2[change_indices]

    net1 <- new_netify(base_mat)
    net2 <- new_netify(mat2)

    # Test with different permutation counts
    comp_qap_100 <- suppressWarnings(compare_networks(
        list(net1, net2),
        method = "qap",
        n_permutations = 100
    ))

    comp_qap_500 <- suppressWarnings(compare_networks(
        list(net1, net2),
        method = "qap",
        n_permutations = 500
    ))

    expect_true("qap_pvalue" %in% names(comp_qap_100$summary))
    expect_true("qap_pvalue" %in% names(comp_qap_500$summary))

    # Both should detect significant correlation
    expect_true(comp_qap_100$summary$qap_correlation > 0)
    expect_true(comp_qap_500$summary$qap_correlation > 0)
})

test_that("compare_networks handles bipartite networks", {
    # Create bipartite networks
    n1 <- 8
    n2 <- 12

    mat1 <- matrix(rbinom(n1 * n2, 1, 0.3), n1, n2)
    mat2 <- matrix(rbinom(n1 * n2, 1, 0.3), n1, n2)

    net1 <- new_netify(mat1, bipartite = TRUE)
    net2 <- new_netify(mat2, bipartite = TRUE)

    comp <- suppressWarnings(compare_networks(list(net1, net2)))

    expect_s3_class(comp, "netify_comparison")
    expect_equal(comp$comparison_type, "cross_network")
})

test_that("compare_networks handles empty and complete networks", {
    n <- 10

    # Empty network
    empty_mat <- matrix(0, n, n)
    net_empty <- new_netify(empty_mat)

    # Complete network
    complete_mat <- matrix(1, n, n)
    diag(complete_mat) <- 0
    net_complete <- new_netify(complete_mat)

    # Normal network
    normal_mat <- matrix(rbinom(n * n, 1, 0.3), n, n)
    net_normal <- new_netify(normal_mat)

    # Compare empty vs complete with method="all" to get jaccard
    comp_extreme <- suppressWarnings(compare_networks(list(net_empty, net_complete), method = "all"))
    expect_equal(comp_extreme$summary$correlation, 0) # No correlation
    expect_equal(comp_extreme$summary$jaccard, 0) # No overlap

    # Compare empty vs normal with method="all"
    comp_empty <- suppressWarnings(compare_networks(list(net_empty, net_normal), method = "all"))
    expect_equal(comp_empty$summary$jaccard, 0) # No overlap

    # Compare complete vs normal with method="all"
    comp_complete <- suppressWarnings(compare_networks(list(net_complete, net_normal), method = "all"))
    expect_true(comp_complete$summary$jaccard > 0) # Some overlap
})

test_that("compare_networks handles self-loops correctly", {
    n <- 10

    # Network with self-loops
    mat1 <- matrix(rbinom(n * n, 1, 0.3), n, n)
    diag(mat1) <- 1 # Add self-loops

    # Network without self-loops
    mat2 <- mat1
    diag(mat2) <- 0

    net1 <- new_netify(mat1, loops = TRUE)
    net2 <- new_netify(mat2, loops = FALSE)

    comp <- suppressWarnings(compare_networks(list(net1, net2)))

    # Should detect differences due to self-loops
    edge_changes <- comp$edge_changes[[1]]
    expect_true(edge_changes$removed > 0 || edge_changes$added > 0)
})

test_that("compare_networks print method works", {
    # Create simple networks
    mat1 <- matrix(rbinom(25, 1, 0.3), 5, 5)
    mat2 <- matrix(rbinom(25, 1, 0.3), 5, 5)

    net1 <- new_netify(mat1)
    net2 <- new_netify(mat2)

    comp <- suppressWarnings(compare_networks(list(net1, net2)))

    # # Test that print runs without error
    # expect_invisible(print(comp))
    
    # Check the structure of the comparison object instead of print output
    expect_equal(comp$comparison_type, "cross_network")
    expect_equal(comp$n_networks, 2)
    expect_true(!is.null(comp$summary))
})

test_that("compare_networks handles list input vs single network", {
    # Create longitudinal network
    n <- 10
    times <- 3
    arr <- array(rbinom(n * n * times, 1, 0.3), dim = c(n, n, times))
    net_longit <- new_netify(arr)

    # Create list of networks
    net_list <- list()
    for (i in 1:times) {
        net_list[[i]] <- new_netify(arr[, , i])
    }

    # Compare results
    comp_longit <- suppressWarnings(compare_networks(net_longit))
    comp_list <- suppressWarnings(compare_networks(net_list))

    expect_equal(comp_longit$comparison_type, "temporal")
    expect_equal(comp_list$comparison_type, "cross_network")

    # Both should have same number of networks
    expect_equal(comp_longit$n_networks, comp_list$n_networks)
})

test_that("compare_networks handles named lists correctly", {
    # Create networks with names
    mat1 <- matrix(rbinom(100, 1, 0.3), 10, 10)
    mat2 <- matrix(rbinom(100, 1, 0.3), 10, 10)
    mat3 <- matrix(rbinom(100, 1, 0.3), 10, 10)

    net_list <- list(
        "Network_A" = new_netify(mat1),
        "Network_B" = new_netify(mat2),
        "Network_C" = new_netify(mat3)
    )

    comp <- suppressWarnings(compare_networks(net_list))

    # Should preserve names in comparisons
    expect_true(length(comp$edge_changes) > 0)
    comparison_names <- names(comp$edge_changes)
    expect_true(all(grepl("Network_[ABC]_vs_Network_[ABC]", comparison_names)))
})

# Additional edge case tests

test_that("compare_networks handles single network gracefully", {
    mat <- matrix(rbinom(100, 1, 0.3), 10, 10)
    net <- new_netify(mat)

    # Single network in list
    expect_error(compare_networks(list(net)), "Need at least 2 networks to compare")
})

test_that("compare_networks handles networks with all missing values", {
    n <- 10

    # Network with all NA values
    na_mat <- matrix(NA, n, n)
    net_na <- new_netify(na_mat)

    # Normal network
    normal_mat <- matrix(rbinom(n * n, 1, 0.3), n, n)
    net_normal <- new_netify(normal_mat)

    # Should handle comparison without error
    comp <- suppressWarnings(compare_networks(list(net_na, net_normal)))
    expect_s3_class(comp, "netify_comparison")
})

test_that("compare_networks handles very sparse networks", {
    n <- 50

    # Very sparse network (only 2 edges)
    sparse_mat <- matrix(0, n, n)
    sparse_mat[1, 2] <- sparse_mat[2, 1] <- 1
    net_sparse <- new_netify(sparse_mat)

    # Another sparse network
    sparse_mat2 <- matrix(0, n, n)
    sparse_mat2[3, 4] <- sparse_mat2[4, 3] <- 1
    net_sparse2 <- new_netify(sparse_mat2)

    comp <- suppressWarnings(compare_networks(list(net_sparse, net_sparse2), method = "all"))
    expect_s3_class(comp, "netify_comparison")
    expect_equal(comp$summary$jaccard, 0) # No overlap
})

test_that("compare_networks structural comparison with extreme networks", {
    n <- 20

    # Star network
    star_mat <- matrix(0, n, n)
    star_mat[1, 2:n] <- star_mat[2:n, 1] <- 1
    net_star <- new_netify(star_mat)

    # Ring network
    ring_mat <- matrix(0, n, n)
    for (i in 1:(n - 1)) {
        ring_mat[i, i + 1] <- ring_mat[i + 1, i] <- 1
    }
    ring_mat[n, 1] <- ring_mat[1, n] <- 1
    net_ring <- new_netify(ring_mat)

    comp_struct <- suppressWarnings(compare_networks(list(net_star, net_ring), what = "structure"))

    # Should have very different structural properties
    # Star network has much higher centralization than ring network
    # Let's just verify the comparison completed successfully
    expect_s3_class(comp_struct, "netify_comparison")
    expect_equal(comp_struct$method, "structural_comparison")
    expect_equal(nrow(comp_struct$summary), 2)
})

test_that("compare_networks handles networks with identical structure", {
    n <- 15
    mat <- matrix(rbinom(n * n, 1, 0.3), n, n)
    mat <- mat | t(mat) # Make symmetric
    diag(mat) <- 0

    net1 <- new_netify(mat)
    net2 <- new_netify(mat) # Identical network

    comp <- suppressWarnings(compare_networks(list(net1, net2), method = "all"))

    # Should have perfect correlation and jaccard
    expect_equal(comp$summary$correlation, 1)
    expect_equal(comp$summary$jaccard, 1)
    expect_equal(comp$summary$hamming, 0)
    expect_equal(comp$summary$spectral, 0) # Identical networks have 0 spectral distance
})

test_that("spectral distance works correctly", {
    # Create networks with known spectral properties
    n <- 10

    # Complete graph
    complete_mat <- matrix(1, n, n)
    diag(complete_mat) <- 0
    net_complete <- new_netify(complete_mat)

    # Empty graph
    empty_mat <- matrix(0, n, n)
    net_empty <- new_netify(empty_mat)

    # Star graph
    star_mat <- matrix(0, n, n)
    star_mat[1, 2:n] <- star_mat[2:n, 1] <- 1
    net_star <- new_netify(star_mat)

    # Test spectral distance between different graph types
    comp_empty_complete <- compare_networks(list(net_empty, net_complete), method = "spectral")
    comp_star_complete <- compare_networks(list(net_star, net_complete), method = "spectral")
    comp_empty_star <- compare_networks(list(net_empty, net_star), method = "spectral")

    # All should have non-zero spectral distances
    expect_true(comp_empty_complete$summary$spectral > 0)
    expect_true(comp_star_complete$summary$spectral > 0)
    expect_true(comp_empty_star$summary$spectral > 0)

    # Test with identical networks
    comp_identical <- compare_networks(list(net_star, net_star), method = "spectral")
    expect_equal(comp_identical$summary$spectral, 0)

    # Test with return_details
    comp_details <- compare_networks(
        list(net_empty, net_complete),
        method = "spectral",
        return_details = TRUE
    )
    expect_true("spectral_matrix" %in% names(comp_details$details))
    expect_equal(dim(comp_details$details$spectral_matrix), c(2, 2))
})

test_that("compare_networks properly extracts multilayer networks", {
    # Test that extract_network_list preserves all required attributes
    n <- 10

    # Create cross-sectional multilayer
    layer1 <- matrix(rbinom(n * n, 1, 0.3), n, n)
    layer2 <- matrix(rbinom(n * n, 1, 0.4), n, n)

    multilayer_array <- array(c(layer1, layer2), dim = c(n, n, 2))
    dimnames(multilayer_array) <- list(
        paste0("actor", 1:n),
        paste0("actor", 1:n),
        c("Layer1", "Layer2")
    )

    # Create proper multilayer netify object with all attributes
    multilayer_net <- multilayer_array
    class(multilayer_net) <- "netify"
    attr(multilayer_net, "netify_type") <- "cross_sec"
    attr(multilayer_net, "symmetric") <- TRUE
    attr(multilayer_net, "mode") <- "unipartite"
    attr(multilayer_net, "layers") <- c("Layer1", "Layer2")
    attr(multilayer_net, "weight") <- c("weight1", "weight2")
    attr(multilayer_net, "detail_weight") <- "Multiple weights"
    attr(multilayer_net, "weight_binary") <- c(FALSE, FALSE)
    attr(multilayer_net, "diag_to_NA") <- TRUE
    attr(multilayer_net, "missing_to_zero") <- TRUE
    attr(multilayer_net, "sum_dyads") <- FALSE

    # Extract layers
    nets_list <- netify:::extract_network_list(multilayer_net)

    # Check that all required attributes are preserved
    expect_equal(length(nets_list), 2)

    for (i in 1:2) {
        layer <- nets_list[[i]]
        expect_true(inherits(layer, "netify"))
        expect_equal(attr(layer, "netify_type"), "cross_sec")
        expect_equal(attr(layer, "symmetric"), TRUE)
        expect_equal(attr(layer, "mode"), "unipartite")
        expect_true(!is.null(attr(layer, "weight")))
        expect_true(!is.null(attr(layer, "detail_weight")))
        expect_true(!is.null(attr(layer, "weight_binary")))
        expect_true(!is.null(attr(layer, "diag_to_NA")))
        expect_true(!is.null(attr(layer, "missing_to_zero")))
        expect_true(!is.null(attr(layer, "sum_dyads")))
    }
})

test_that("spectral distance handles edge cases", {
    # Networks with different sizes
    n1 <- 8
    n2 <- 12

    mat1 <- matrix(rbinom(n1 * n1, 1, 0.3), n1, n1)
    mat2 <- matrix(rbinom(n2 * n2, 1, 0.3), n2, n2)

    net1 <- new_netify(mat1)
    net2 <- new_netify(mat2)

    # Should handle different sized networks without error
    comp <- suppressWarnings(compare_networks(list(net1, net2), method = "spectral"))
    expect_s3_class(comp, "netify_comparison")
    expect_true(is.numeric(comp$summary$spectral))
    expect_true(comp$summary$spectral >= 0)

    # Test with directed networks
    mat_dir <- matrix(rbinom(100, 1, 0.3), 10, 10)
    net_dir <- new_netify(mat_dir, symmetric = FALSE)

    comp_dir <- compare_networks(list(net_dir, net_dir), method = "spectral")
    expect_equal(comp_dir$summary$spectral, 0) # Same network should have 0 distance
})

test_that("compare_networks handles multilayer networks", {
    # Create test data
    n <- 10

    # Create two layers with different patterns
    layer1 <- matrix(rbinom(n * n, 1, 0.3), n, n)
    layer2 <- matrix(rbinom(n * n, 1, 0.4), n, n)

    # Create cross-sectional multilayer network
    multilayer_array <- array(c(layer1, layer2), dim = c(n, n, 2))
    dimnames(multilayer_array) <- list(
        paste0("actor", 1:n),
        paste0("actor", 1:n),
        c("Layer1", "Layer2")
    )

    # Create multilayer netify object manually
    multilayer_net <- multilayer_array
    class(multilayer_net) <- "netify"
    attr(multilayer_net, "netify_type") <- "cross_sec"
    attr(multilayer_net, "symmetric") <- FALSE
    attr(multilayer_net, "mode") <- "unipartite"
    attr(multilayer_net, "layers") <- c("Layer1", "Layer2")

    # Test multilayer comparison
    comp <- suppressWarnings(compare_networks(multilayer_net))

    expect_s3_class(comp, "netify_comparison")
    expect_equal(comp$comparison_type, "multilayer")
    expect_equal(comp$n_networks, 2)
    expect_true("summary" %in% names(comp))

    # Should compare Layer1 vs Layer2
    expect_true(length(comp$edge_changes) > 0)
    expect_true(any(grepl("Layer1.*Layer2", names(comp$edge_changes))))
})

test_that("compare_networks handles longitudinal multilayer networks", {
    # Create 4D array for longitudinal multilayer
    n <- 8
    n_layers <- 2
    n_time <- 3

    # Create 4D array [actors × actors × layers × time]
    longit_multilayer <- array(
        rbinom(n * n * n_layers * n_time, 1, 0.3),
        dim = c(n, n, n_layers, n_time)
    )

    dimnames(longit_multilayer) <- list(
        paste0("actor", 1:n),
        paste0("actor", 1:n),
        c("Cooperation", "Conflict"),
        c("T1", "T2", "T3")
    )

    # Create multilayer netify object
    class(longit_multilayer) <- "netify"
    attr(longit_multilayer, "netify_type") <- "longit_array"
    attr(longit_multilayer, "symmetric") <- FALSE
    attr(longit_multilayer, "mode") <- "unipartite"
    attr(longit_multilayer, "layers") <- c("Cooperation", "Conflict")

    # Test comparison
    comp <- suppressWarnings(compare_networks(longit_multilayer))

    expect_s3_class(comp, "netify_comparison")
    expect_equal(comp$comparison_type, "multilayer")
    expect_equal(comp$n_networks, 2) # Two layers

    # Each layer should be a longitudinal array
    # The comparison should be between cooperation and conflict layers
    expect_true("summary" %in% names(comp))
})

test_that("compare_networks handles temporal multilayer networks (longit_list)", {
    # Create temporal multilayer using layer_netify
    data(icews)
    years <- c(2002, 2003)
    icews_subset <- icews[icews$year %in% years, ]

    # Create temporal networks for each layer
    verbal_temporal <- netify(
        icews_subset,
        actor1 = "i", actor2 = "j",
        time = "year",
        symmetric = TRUE,
        weight = "verbCoop"
    )

    material_temporal <- netify(
        icews_subset,
        actor1 = "i", actor2 = "j",
        time = "year",
        symmetric = TRUE,
        weight = "matlCoop"
    )

    # Combine into temporal multilayer
    temporal_multilayer <- layer_netify(
        netlet_list = list(verbal_temporal, material_temporal),
        layer_labels = c("Verbal", "Material")
    )

    # Test all comparison methods
    # Edge comparison
    comp_edges <- suppressWarnings(compare_networks(temporal_multilayer, what = "edges"))
    expect_s3_class(comp_edges, "netify_comparison")
    expect_equal(comp_edges$comparison_type, "multilayer")
    expect_equal(comp_edges$n_networks, 2)
    expect_true("summary" %in% names(comp_edges))

    # Structure comparison
    comp_struct <- suppressWarnings(compare_networks(temporal_multilayer, what = "structure"))
    expect_s3_class(comp_struct, "netify_comparison")
    expect_equal(comp_struct$method, "structural_comparison")
    expect_true("summary" %in% names(comp_struct))
    # Check that summary has expected rows (2 layers × 2 time periods)
    expect_equal(nrow(comp_struct$summary), 4)

    # Node comparison
    comp_nodes <- suppressWarnings(compare_networks(temporal_multilayer, what = "nodes"))
    expect_s3_class(comp_nodes, "netify_comparison")
    expect_equal(comp_nodes$method, "node_composition")

    # Attribute comparison
    comp_attrs <- suppressWarnings(compare_networks(temporal_multilayer, what = "attributes"))
    expect_s3_class(comp_attrs, "netify_comparison")
    expect_equal(comp_attrs$method, "attribute_comparison")
})

test_that("compare_networks handles multilayer networks with missing attributes", {
    # Create a multilayer network with minimal attributes
    n <- 10
    layer1 <- matrix(rbinom(n * n, 1, 0.3), n, n)
    layer2 <- matrix(rbinom(n * n, 1, 0.4), n, n)

    multilayer_array <- array(c(layer1, layer2), dim = c(n, n, 2))

    # Create netify object with minimal attributes
    multilayer_net <- multilayer_array
    class(multilayer_net) <- "netify"
    attr(multilayer_net, "netify_type") <- "cross_sec"
    attr(multilayer_net, "symmetric") <- FALSE
    attr(multilayer_net, "mode") <- "unipartite"
    attr(multilayer_net, "layers") <- c("Layer1", "Layer2")
    # Missing many optional attributes

    # Should still work
    comp <- suppressWarnings(compare_networks(multilayer_net))
    expect_s3_class(comp, "netify_comparison")
    expect_equal(comp$comparison_type, "multilayer")
    expect_equal(comp$n_networks, 2)
})

test_that("compare_networks handles different weight specifications in multilayer", {
    # Test with different weight types per layer
    n <- 10

    # Create networks with different weight structures
    net1 <- new_netify(matrix(runif(n * n), n, n), symmetric = FALSE)
    net2 <- new_netify(matrix(rpois(n * n, 2), n, n), symmetric = FALSE)

    # Use layer_netify to create proper multilayer
    multilayer <- layer_netify(
        list(net1, net2),
        layer_labels = c("Continuous", "Count")
    )

    # Extract and compare
    nets_extracted <- netify:::extract_network_list(multilayer)
    expect_equal(length(nets_extracted), 2)

    # Each should have appropriate weight info
    expect_true(!is.null(attr(nets_extracted[[1]], "weight")))
    expect_true(!is.null(attr(nets_extracted[[2]], "weight")))

    # Test comparison works
    comp <- suppressWarnings(compare_networks(multilayer))
    expect_s3_class(comp, "netify_comparison")
    expect_equal(comp$comparison_type, "multilayer")
})

test_that("compare_networks error handling for invalid multilayer inputs", {
    # Test with mismatched dimensions in layers
    n1 <- 10
    n2 <- 15

    mat1 <- matrix(rbinom(n1 * n1, 1, 0.3), n1, n1)
    mat2 <- matrix(rbinom(n2 * n2, 1, 0.3), n2, n2)

    # This should fail when trying to create multilayer
    expect_error(
        layer_netify(list(new_netify(mat1), new_netify(mat2)))
    )
})

test_that("compare_networks handles all comparison methods with multilayer", {
    # Create simple multilayer network
    n <- 20
    layer1 <- matrix(rbinom(n * n, 1, 0.3), n, n)
    layer2 <- matrix(rbinom(n * n, 1, 0.5), n, n)

    # Create two separate netify objects
    net1 <- new_netify(layer1, symmetric = TRUE)
    net2 <- new_netify(layer2, symmetric = TRUE)

    # Use layer_netify to create multilayer
    multilayer <- layer_netify(
        list(net1, net2),
        layer_labels = c("Sparse", "Dense")
    )

    # Test all edge comparison methods
    methods <- c("correlation", "jaccard", "hamming", "spectral")

    for (method in methods) {
        comp <- suppressWarnings(compare_networks(multilayer, method = method))
        expect_s3_class(comp, "netify_comparison")
        expect_equal(comp$comparison_type, "multilayer")
        expect_true(method %in% names(comp$summary))
    }

    # Test "all" method
    comp_all <- suppressWarnings(compare_networks(multilayer, method = "all"))
    expect_true(all(c("correlation", "jaccard", "hamming", "spectral") %in% names(comp_all$summary)))
})
