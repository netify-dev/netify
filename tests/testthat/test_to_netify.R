# library(netify)
# library(igraph)
# library(testthat)
# devtools::load_all('~/Research/netify_dev/netify')

set.seed(6886)

# cross section tests

test_that("to_netify handles single igraph objects", {
  # single igraph
  g <- igraph::make_ring(5)
  igraph::E(g)$myweight <- 1:5
  net_g <- to_netify(g, weight = "myweight")
  expect_s3_class(net_g, "netify")
  expect_equal(attr(net_g, "weight"), "myweight")
})

test_that("to_netify handles single network objects", {
  # single network
  nw <- network::network(
    matrix(
        rbinom(25, 1, 0.3), 5, 5), directed = FALSE)
  network::set.edge.attribute(
    nw, "myweight", 
    1:network::network.size(nw))
  net_nw <- to_netify(nw, weight="myweight")
  expect_s3_class(net_nw, "netify")
  expect_equal(attr(net_nw, "weight"), "myweight")
})

test_that("to_netify handles single matrices", {
  # Single matrix
  mat <- matrix(runif(25), 5, 5)
  rownames(mat) <- colnames(mat) <- paste0("actor", 1:5)
  net_mat <- to_netify(mat)
  expect_s3_class(net_mat, "netify")
  expect_equal(attr(net_mat, "netify_type"), "cross_sec")
})

# longit tests

test_that("to_netify handles 3D arrays (longitudinal)", {
  # 3D array (longitudinal)
  arr <- array(runif(75), dim = c(5, 5, 3))
  dimnames(arr) <- list(
    paste0("actor", 1:5),
    paste0("actor", 1:5),
    c("2001", "2002", "2003")
  )
  net_arr <- to_netify(arr)
  expect_s3_class(net_arr, "netify")
  expect_equal(attr(net_arr, "netify_type"), "longit_array")
})

test_that("to_netify handles arrays comprehensively", {
  # 2D array (should be treated as matrix)
  arr_2d <- array(runif(25), dim = c(5, 5))
  rownames(arr_2d) <- colnames(arr_2d) <- paste0("node", 1:5)
  net_2d <- to_netify(arr_2d)
  expect_s3_class(net_2d, "netify")
  expect_equal(attr(net_2d, "netify_type"), "cross_sec")
  
  # 3D array without dimnames
  arr_3d_no_names <- array(runif(60), dim = c(4, 4, 3))
  net_3d_no_names <- to_netify(arr_3d_no_names)
  expect_s3_class(net_3d_no_names, "netify")
  expect_equal(attr(net_3d_no_names, "netify_type"), "longit_array")
})

test_that("to_netify handles list of igraph objects", {
  g <- igraph::make_ring(5)
  igraph::E(g)$myweight <- 1:5
  g2 <- igraph::make_star(6)
  igraph::E(g2)$edge_color <- "red"
  igraph::E(g2)$myweight <- 6:10
  net_g_list <- to_netify(
    list(g, g2), weight = "myweight")
  expect_s3_class(net_g_list, "netify")
  expect_equal(
    attr(
        net_g_list, "netify_type"), "longit_list")
  # check that adjacency was built for each
  expect_length(net_g_list, 2)
})

test_that("to_netify handles list of network objects", {
  nw <- network::network(
    matrix(
        rbinom(25, 1, 0.3), 5, 5), directed = FALSE)
  network::set.edge.attribute(
    nw, "myweight", 
    1:network::network.size(nw))
  nw2 <- network::network(
    matrix(
        rbinom(36, 1, 0.5), 6, 6), directed = FALSE)
  network::set.edge.attribute(
    nw2, "myweight", 
    1:network::network.size(nw))
  net_nw_list <- to_netify(
    list(nw, nw2), weight="myweight")
  expect_s3_class(net_nw_list, "netify")
  expect_equal(attr(net_nw_list, "netify_type"), "longit_list")
  expect_length(net_nw_list, 2)
})

test_that("to_netify handles list of matrices", {
  mat_list <- list(
    "2001" = matrix(runif(25), 5, 5),
    "2002" = matrix(runif(25), 5, 5)
  )
  for(i in 1:2) {
    rownames(mat_list[[i]]) <- colnames(mat_list[[i]]) <- paste0("actor", 1:5)
  }
  net_mat_list <- to_netify(mat_list)
  expect_s3_class(net_mat_list, "netify")
  expect_equal(attr(net_mat_list, "netify_type"), "longit_list")
  expect_length(net_mat_list, 2)
})

test_that("to_netify handles longitudinal networks with different sizes", {
  # Create networks of different sizes
  nw1 <- network::network(matrix(rbinom(25, 1, 0.3), 5, 5), directed = FALSE)
  nw2 <- network::network(matrix(rbinom(36, 1, 0.4), 6, 6), directed = FALSE)
  nw3 <- network::network(matrix(rbinom(16, 1, 0.5), 4, 4), directed = FALSE)
  
  # Add edge attributes
  network::set.edge.attribute(nw1, "weight", runif(network::network.edgecount(nw1)))
  network::set.edge.attribute(nw2, "weight", runif(network::network.edgecount(nw2)))
  network::set.edge.attribute(nw3, "weight", runif(network::network.edgecount(nw3)))
  
  # Convert list with names
  net_list <- to_netify(
    list("2001" = nw1, "2002" = nw2, "2003" = nw3),
    weight = "weight"
  )
  
  expect_s3_class(net_list, "netify")
  expect_equal(attr(net_list, "netify_type"), "longit_list")
  expect_length(net_list, 3)
  expect_equal(names(net_list), c("2001", "2002", "2003"))
  
  # Check dyad data has correct dimensions for each time period
  dyad_data <- attr(net_list, "dyad_data")
  expect_equal(dim(dyad_data[["2001"]][["weight"]]), c(5, 5))
  expect_equal(dim(dyad_data[["2002"]][["weight"]]), c(6, 6))
  expect_equal(dim(dyad_data[["2003"]][["weight"]]), c(4, 4))
})

# edge cases

test_that("to_netify handles lists without names", {
  # List of matrices without names
  mat1 <- matrix(runif(9), 3, 3)
  mat2 <- matrix(runif(9), 3, 3)
  rownames(mat1) <- colnames(mat1) <- paste0("n", 1:3)
  rownames(mat2) <- colnames(mat2) <- paste0("n", 1:3)
  
  unnamed_list <- to_netify(list(mat1, mat2))
  expect_equal(names(unnamed_list), c("1", "2"))
  
  # List of igraphs without names
  g1 <- igraph::make_ring(3)
  g2 <- igraph::make_star(3)
  unnamed_g_list <- to_netify(list(g1, g2))
  expect_equal(names(unnamed_g_list), c("1", "2"))
})

test_that("to_netify handles edge cases for lists", {
  # Single-element list of matrix
  mat <- matrix(runif(16), 4, 4)
  rownames(mat) <- colnames(mat) <- paste0("a", 1:4)
  single_mat_list <- to_netify(list("2001" = mat))
  expect_s3_class(single_mat_list, "netify")
  expect_equal(attr(single_mat_list, "netify_type"), "longit_list")
  expect_length(single_mat_list, 1)
  
  # Single-element list of igraph
  g <- igraph::make_ring(4)
  single_g_list <- to_netify(list(g))
  expect_s3_class(single_g_list, "netify")
  expect_equal(attr(single_g_list, "netify_type"), "longit_list")
  
  # Single-element list of network
  nw <- network::network(matrix(rbinom(16, 1, 0.5), 4, 4))
  single_nw_list <- to_netify(list(nw))
  expect_s3_class(single_nw_list, "netify")
  expect_equal(attr(single_nw_list, "netify_type"), "longit_list")
})

# attrib specific tests

test_that("to_netify correctly extracts and converts dyad data", {
  # Create igraph with edge attributes
  g <- igraph::make_ring(5, directed = FALSE)
  igraph::E(g)$weight <- 1:5
  igraph::E(g)$type <- c("friend", "family", "work", "friend", "family")
  
  net_g <- to_netify(g, weight = "weight")
  dyad_data <- attr(net_g, "dyad_data")
  
  # Check structure
  expect_true(is.list(dyad_data))
  expect_equal(names(dyad_data), "1")
  expect_equal(names(dyad_data[["1"]]), c("weight", "type"))
  
  # Check weight matrix matches adjacency
  weight_mat <- dyad_data[["1"]][["weight"]]
  expect_equal(dim(weight_mat), c(5, 5))
  expect_true(isSymmetric(weight_mat))
  expect_equal(weight_mat[1,2], 1)
  expect_equal(weight_mat[2,3], 2)
  
  # Check type matrix
  type_mat <- dyad_data[["1"]][["type"]]
  expect_equal(type_mat[1,2], "friend")
  expect_equal(type_mat[2,3], "family")
})

test_that("to_netify handles vertex attributes correctly", {
  # Create igraph with vertex attributes
  g <- igraph::make_star(5)
  igraph::V(g)$name <- c("Center", "Node1", "Node2", "Node3", "Node4")
  igraph::V(g)$type <- c("hub", "spoke", "spoke", "spoke", "spoke")
  igraph::V(g)$value <- c(10, 1, 2, 3, 4)
  
  # suppressing warnings because
  # there will be a warning thrown as we are passing type
  # for a directed igraph object which igraph will
  # then think is bipartite, but since the type attribute
  # is not logical igraph will not handle it correctly, 
  # so we just throw a warning and say we will treat 
  # as unipartite and have type as an attribute
  net_g <- suppressWarnings(to_netify(g))
  nodal_data <- attr(net_g, "nodal_data")
  
  # Check nodal data
  expect_true(is.data.frame(nodal_data))
  expect_equal(nodal_data$name, c("Center", "Node1", "Node2", "Node3", "Node4"))
  expect_equal(nodal_data$type, c("hub", "spoke", "spoke", "spoke", "spoke"))
  expect_equal(nodal_data$value, c(10, 1, 2, 3, 4))
})

test_that("to_netify handles user-provided nodal and dyad data", {
  g <- igraph::make_ring(5)
  
  # Create nodal data
  nodal_data <- data.frame(
    actor = paste0("a", 1:5),
    attribute1 = rnorm(5),
    attribute2 = letters[1:5],
    stringsAsFactors = FALSE
  )
  
  # Create dyad data in correct format
  dyad_data <- list(
    "1" = list(
      custom_weight = matrix(runif(25), 5, 5, 
                             dimnames = list(paste0("a", 1:5), paste0("a", 1:5)))
    )
  )
  
  net_g <- to_netify(g, nodal_data = nodal_data, dyad_data = dyad_data)
  
  # Check that user data was used
  expect_equal(attr(net_g, "nodal_data"), nodal_data)
  expect_equal(attr(net_g, "dyad_data"), dyad_data)
})

# bipartite tests

test_that("to_netify handles bipartite graphs correctly", {
  # Create bipartite igraph
  g_bip <- igraph::make_bipartite_graph(
    c(TRUE, TRUE, FALSE, FALSE, FALSE),
    c(1, 3, 1, 4, 2, 3, 2, 5)
  )
  igraph::E(g_bip)$weight <- 1:4
  
  net_bip <- to_netify(g_bip, weight = "weight")
  adj_mat <- net_bip
  
  # Check dimensions and naming
  expect_equal(rownames(adj_mat), c("r1", "r2", "r3"))
  expect_equal(colnames(adj_mat), c("c1", "c2"))
  expect_equal(dim(adj_mat), c(3, 2))
})

test_that("to_netify handles bipartite networks correctly", {
  # Bipartite network object
  nw_bip <- network::network.initialize(7, bipartite = 3, directed = FALSE)
  network::add.edges(nw_bip, 
                     tail = c(1, 1, 2, 2, 3),
                     head = c(4, 5, 5, 6, 7))
  network::set.edge.attribute(nw_bip, "weight", runif(5))
  
  net_bip <- to_netify(nw_bip, weight = "weight")
  adj_mat <- net_bip
  
  # Check dimensions and naming for bipartite network
  expect_equal(dim(adj_mat), c(3, 4))
  expect_equal(rownames(adj_mat), c("r1", "r2", "r3"))
  expect_equal(colnames(adj_mat), c("c1", "c2", "c3", "c4"))
})

test_that("to_netify handles directed vs undirected networks", {
  # Directed igraph
  g_dir <- igraph::make_ring(4, directed = TRUE)
  igraph::E(g_dir)$weight <- 1:4
  net_dir <- to_netify(g_dir, weight = "weight")
  
  # Check that directed edges are not symmetrized
  dyad_data_dir <- attr(net_dir, "dyad_data")
  weight_mat_dir <- dyad_data_dir[["1"]][["weight"]]
  expect_false(isSymmetric(weight_mat_dir))
  
  # Undirected igraph
  g_undir <- igraph::make_ring(4, directed = FALSE)
  igraph::E(g_undir)$weight <- 1:4
  net_undir <- to_netify(g_undir, weight = "weight")
  
  # Check that undirected edges are symmetrized
  dyad_data_undir <- attr(net_undir, "dyad_data")
  weight_mat_undir <- dyad_data_undir[["1"]][["weight"]]
  expect_true(isSymmetric(weight_mat_undir))
})

test_that("to_netify handles weighted networks correctly", {
  # Weighted matrix
  w_mat <- matrix(runif(16), 4, 4)
  diag(w_mat) <- 0
  rownames(w_mat) <- colnames(w_mat) <- paste0("v", 1:4)
  
  net_wmat <- to_netify(w_mat)
  expect_false(attr(net_wmat, "weight_binary"))
  
  # Binary matrix
  b_mat <- matrix(sample(0:1, 16, replace = TRUE), 4, 4)
  diag(b_mat) <- 0
  rownames(b_mat) <- colnames(b_mat) <- paste0("v", 1:4)
  
  net_bmat <- to_netify(b_mat)
  expect_true(attr(net_bmat, "weight_binary"))
})

# data checks

test_that("to_netify validates and aligns data correctly", {
  # Create networks with specific actor names
  g1 <- igraph::make_ring(4)
  igraph::V(g1)$name <- c("USA", "CHN", "RUS", "GBR")
  igraph::E(g1)$trade <- c(100, 200, 150, 180)
  
  g2 <- igraph::make_ring(4)
  igraph::V(g2)$name <- c("USA", "CHN", "RUS", "GBR")
  igraph::E(g2)$trade <- c(120, 210, 160, 190)
  
  net_list <- to_netify(
    list("2001" = g1, "2002" = g2),
    weight = "trade"
  )
  
  # Check that actor names are preserved
  expect_equal(rownames(net_list[[1]]), c("USA", "CHN", "RUS", "GBR"))
  expect_equal(rownames(net_list[[2]]), c("USA", "CHN", "RUS", "GBR"))
  
  # Check dyad data
  dyad_data <- attr(net_list, "dyad_data")
  expect_equal(dyad_data[["2001"]][["trade"]]["USA", "CHN"], 100)
  expect_equal(dyad_data[["2002"]][["trade"]]["USA", "CHN"], 120)
})

test_that("to_netify preserves attributes across different input types", {
  # Create equivalent networks in different formats
  set.seed(123)
  adj <- matrix(c(0,1,1,0,
                  1,0,1,1,
                  1,1,0,1,
                  0,1,1,0), 4, 4)
  rownames(adj) <- colnames(adj) <- c("A", "B", "C", "D")
  
  # Matrix version
  net_mat <- to_netify(adj)
  
  # igraph version
  g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected")
  net_ig <- to_netify(g)
  
  # network version
  nw <- network::network(adj, directed = FALSE)
  net_nw <- to_netify(nw)
  
  # All should produce same adjacency structure
  expect_equal(get_raw(net_mat), net_ig[,])
  expect_equal(get_raw(net_mat), net_nw[,])
})

# icews tests

test_that("to_netify works with complex real-world data structures", {
  # Simulate ICEWS-like data
  set.seed(6886)
  n_actors <- 10
  actor_names <- paste0("Country", LETTERS[1:n_actors])
  
  # Create cross-sectional network
  mat <- matrix(rpois(n_actors^2, lambda = 2), n_actors, n_actors)
  diag(mat) <- 0
  rownames(mat) <- colnames(mat) <- actor_names
  
  # Add nodal attributes
  nodal_data <- data.frame(
    actor = actor_names,
    gdp = rlnorm(n_actors, 10, 1),
    polity = sample(-10:10, n_actors, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Add dyadic attributes
  dyad_vars <- list(
    "1" = list(
      trade = matrix(rlnorm(n_actors^2, 5, 1), n_actors, n_actors,
                     dimnames = list(actor_names, actor_names)),
      alliance = matrix(sample(0:1, n_actors^2, replace = TRUE), n_actors, n_actors,
                        dimnames = list(actor_names, actor_names))
    )
  )
  
  net <- to_netify(mat, nodal_data = nodal_data, dyad_data = dyad_vars)
  
  expect_s3_class(net, "netify")
  expect_equal(dim(net), c(n_actors, n_actors))
  expect_equal(attr(net, "nodal_data"), nodal_data)
  expect_equal(attr(net, "dyad_data"), dyad_vars)
})

# error handling tests

test_that("to_netify error handling", {
  # Unsupported object type
  expect_error(
    to_netify(data.frame(a = 1:5))
  )
  
  # Mixed list types
  g <- igraph::make_ring(3)
  mat <- matrix(1:9, 3, 3)
  expect_error(
    to_netify(list(g, mat))
  )
  
  # Invalid nodal data
  g <- igraph::make_ring(3)
  expect_error(
    to_netify(g, nodal_data = "not a dataframe")
  )
})