set.seed(6886)

library(testthat)
library(netify)

test_that("compare_networks works with basic edge comparison", {
    # Create test networks
    set.seed(123)
    n <- 20
    
    mat1 <- matrix(rbinom(n*n, 1, 0.3), n, n)
    mat2 <- mat1
    change_indices <- sample(1:(n*n), size = round(0.2 * n * n))
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
    expect_equal(struct_comp$comparison_type, "cross_network")  # Still cross_network, not structure
    expect_equal(struct_comp$method, "structural_comparison")   # The method indicates what was compared
    expect_equal(struct_comp$n_networks, 2)
    
    # Test node comparison
    node_comp <- compare_networks(list(net1, net2), what = "nodes")
    expect_equal(node_comp$comparison_type, "cross_network")   # Still cross_network, not nodes
    expect_equal(node_comp$method, "node_composition")         # The method indicates what was compared
    expect_equal(node_comp$n_networks, 2)
})

test_that("compare_networks works with longitudinal data", {
    # Create longitudinal network
    n <- 10
    arr <- array(rbinom(n*n*3, 1, 0.3), dim = c(n, n, 3))
    net_longit <- new_netify(arr)
    
    comp <- suppressWarnings(compare_networks(net_longit))
    expect_equal(comp$comparison_type, "temporal")
    expect_equal(comp$n_networks, 3)
})

test_that("compare_networks correctly identifies comparison types", {
    # Create test networks
    n <- 10
    mat1 <- matrix(rbinom(n*n, 1, 0.3), n, n)
    mat2 <- matrix(rbinom(n*n, 1, 0.3), n, n)
    net1 <- new_netify(mat1)
    net2 <- new_netify(mat2)
    
    # Cross-network comparison (list of networks)
    comp_cross <- suppressWarnings(compare_networks(list(net1, net2)))
    expect_equal(comp_cross$comparison_type, "cross_network")
    
    # Temporal comparison (single longitudinal network)
    arr <- array(rbinom(n*n*3, 1, 0.3), dim = c(n, n, 3))
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
    expect_equal(comp_edges$method, "correlation")  # default method for edges
    
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