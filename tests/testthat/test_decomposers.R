set.seed(6886)

# library(testthat)
# library(netify)
# library(igraph)
# library(network)

# helper function to create test data
create_test_data <- function() {
  # cross-sectional data
  cs_data <- data.frame(
    from = c("A", "A", "B", "B", "C"),
    to = c("B", "C", "A", "C", "A"),
    weight = c(1, 2, 3, 4, 5),
    dyad_attr = c("x", "y", "x", "y", "x"),
    stringsAsFactors = FALSE
  )
  
  # longitudinal data with nodal attributes as columns in the dyad data
  # for netify to extract nodal vars, they need to be in the dyad data
  longit_data <- data.frame(
    from = rep(c("A", "A", "B", "B", "C"), 2),
    to = rep(c("B", "C", "A", "C", "A"), 2),
    time = c(rep(2020, 5), rep(2021, 5)),
    weight = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6),
    dyad_attr = rep(c("x", "y", "x", "y", "x"), 2),
    from_gdp = c(100, 100, 200, 200, 300, 110, 110, 210, 210, 310),
    from_pop = c(10, 10, 20, 20, 30, 11, 11, 21, 21, 31),
    stringsAsFactors = FALSE
  )
  
  # separate nodal attributes data for add_node_vars
  longit_node_attrs <- data.frame(
    actor = rep(c("A", "B", "C"), 2),
    time = c(rep(2020, 3), rep(2021, 3)),
    node_attr = c(10, 20, 30, 11, 21, 31),
    democracy = c(7, 8, 6, 7.5, 8.5, 6.5),
    stringsAsFactors = FALSE
  )
  
  # cross-sectional nodal attributes
  cs_node_attrs <- data.frame(
    actor = c("A", "B", "C"),
    node_attr = c(10, 20, 30),
    democracy = c(7, 8, 6),
    stringsAsFactors = FALSE
  )
  
  # bipartite data (e.g., countries trading with products)
  bip_data <- data.frame(
    from = c("USA", "USA", "China", "China", "Japan"),
    to = c("Oil", "Tech", "Tech", "Steel", "Tech"),
    weight = c(100, 200, 300, 150, 250),
    stringsAsFactors = FALSE
  )
  
  list(
    cs_data = cs_data,
    longit_data = longit_data,
    longit_node_attrs = longit_node_attrs,
    cs_node_attrs = cs_node_attrs,
    bip_data = bip_data
  )
}

# tests for decompose_netify
test_that("decompose_netify works for cross-sectional networks", {
  test_data <- create_test_data()
  
  # create netify object with dyad_vars (from the dyad_data columns)
  net_cs <- netify(
    dyad_data = test_data$cs_data,
    actor1 = "from",
    actor2 = "to",
    weight = "weight",
    dyad_vars = "dyad_attr",  # this refers to column in cs_data
    symmetric = FALSE,
    output_format = "cross_sec"
  )
  
  # add additional nodal attributes using add_node_vars
  net_cs <- add_node_vars(
    netlet = net_cs,
    node_data = test_data$cs_node_attrs,
    actor = "actor",
    node_vars = c("node_attr", "democracy")
  )
  
  # decompose
  decomposed <- decompose_netify(net_cs)
  
  # test edge data
  expect_true("edge_data" %in% names(decomposed))
  expect_true("nodal_data" %in% names(decomposed))
  
  # check edge data structure
  expect_true(all(c("from", "to", "time", "weight") %in% 
                    names(decomposed$edge_data)))
  expect_equal(as.character(decomposed$edge_data$time[1]), "1")  # cross-sectional should have time = "1"
  expect_equal(nrow(decomposed$edge_data), 5)  # 5 edges
  
  # check if dyad_attr was preserved
  expect_true("dyad_attr" %in% names(decomposed$edge_data))
  
  # check nodal data structure
  expect_true(all(c("name", "time") %in% names(decomposed$nodal_data)))
  expect_equal(as.character(decomposed$nodal_data$time[1]), "1")
  expect_equal(nrow(decomposed$nodal_data), 3)  # 3 nodes
  
  # check if node attributes were preserved
  expect_true("node_attr" %in% names(decomposed$nodal_data))
  expect_true("democracy" %in% names(decomposed$nodal_data))
})

# tests for decompose_igraph
test_that('decompose_igraph: unweighted cross-sec, asymmetric', {
  # generate test data using same approach as netify_to_igraph tests
  adjm <- matrix(
    as.numeric(sample(0:1, 100, replace=TRUE, prob=c(0.5, .5))), 
    ncol=10)
  rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]
  
  # create igraph object
  g1 <- igraph::graph_from_adjacency_matrix(adjm)
  
  # decompose
  decomposed <- decompose_igraph(g1, weight = NULL)
  
  # check structure
  expect_equal(dim(decomposed$adj_mat), c(10, 10))
  expect_equal(rownames(decomposed$adj_mat), letters[1:10])
  expect_equal(colnames(decomposed$adj_mat), letters[1:10])
  expect_identical(decomposed$adj_mat, adjm)
  expect_null(decomposed$ddata)  # no edge attributes
  expect_null(decomposed$weight)
})

test_that('decompose_igraph: weighted cross-sec, asymmetric', {
  # generate test data
  adjm <- matrix(rnorm(10^2), ncol=10)
  rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]
  g1 <- igraph::graph_from_adjacency_matrix(adjm, weighted=TRUE)
  
  # decompose
  decomposed <- decompose_igraph(g1, weight = "weight")
  
  # check structure
  expect_equal(dim(decomposed$adj_mat), c(10, 10))
  expect_identical(decomposed$adj_mat, adjm)
  expect_equal(decomposed$weight, "weight")
  
  # check edge data includes weights
  if (!is.null(decomposed$ddata)) {
    expect_true("weight" %in% names(decomposed$ddata))
  }
})

test_that('decompose_igraph: weighted cross-sec with dyad and nodal attribs', {
  # create fake data matching netify_to_igraph test style
  fakeDyads <- expand.grid(actor1 = letters[1:3], actor2 = letters[1:3])
  fakeDyads$weight <- rnorm(nrow(fakeDyads))
  fakeDyads$var2 <- rnorm(nrow(fakeDyads))
  fakeDyads$var3 <- rnorm(nrow(fakeDyads))
  fakeDyads$var4 <- rnorm(nrow(fakeDyads))
  fakeDyads$actor1 = as.character(fakeDyads$actor1)
  fakeDyads$actor2 = as.character(fakeDyads$actor2)
  fakeDyads <- fakeDyads[fakeDyads$actor1!=fakeDyads$actor2,]
  
  fakeNodes <- data.frame(actor1 = letters[1:3], var1 = rnorm(3), var2 = rnorm(3))
  
  # create igraph object
  g <- igraph::graph_from_data_frame(fakeDyads, directed = TRUE, vertices = fakeNodes)
  
  # decompose
  decomposed <- decompose_igraph(g, weight = "weight")
  
  # check adjacency matrix
  expect_equal(dim(decomposed$adj_mat), c(3, 3))
  expect_equal(rownames(decomposed$adj_mat), letters[1:3])
  
  # check node data
  expect_true(!is.null(decomposed$ndata))
  expect_true(all(c("name", "var1", "var2", "actor") %in% names(decomposed$ndata)))
  expect_equal(nrow(decomposed$ndata), 3)
  
  # check edge data
  expect_true(!is.null(decomposed$ddata))
  expect_true(all(c("from", "to", "weight", "var2", "var3", "var4") %in% names(decomposed$ddata)))
})

test_that('decompose_igraph: bipartite networks', {
  # generate bipartite test data
  adjm <- matrix(
    as.numeric(sample(0:1, 50, replace=TRUE, prob=c(0.5, .5))), 
    nrow=5, ncol=10)
  rownames(adjm) = letters[1:5]
  colnames(adjm) = letters[17:26]
  
  # create bipartite igraph
  g <- igraph::graph_from_biadjacency_matrix(adjm)
  
  # add some attributes
  igraph::V(g)$node_attr <- rnorm(15)
  igraph::E(g)$edge_attr <- rnorm(igraph::ecount(g))
  
  # decompose
  decomposed <- decompose_igraph(g)
  
  # check adjacency matrix dimensions (should be rectangular for bipartite)
  expect_equal(dim(decomposed$adj_mat), c(5, 10))
  expect_equal(rownames(decomposed$adj_mat), letters[1:5])
  expect_equal(colnames(decomposed$adj_mat), letters[17:26])
  
  # check node data includes type
  expect_true("type" %in% names(decomposed$ndata))
  expect_equal(sum(!decomposed$ndata$type), 5)  # 5 type false vertices
  expect_equal(sum(decomposed$ndata$type), 10)  # 10 type true vertices
})

# tests for decompose_statnet
test_that('decompose_statnet: unweighted cross-sec, asymmetric', {
  # generate test data matching netify_to_statnet test style
  adjm <- matrix(
    as.numeric(sample(0:1, 100, replace=TRUE, prob=c(0.5, .5))), 
    ncol=10)
  rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]
  
  # create network object
  net <- network::network(
    adjm,
    matrix.type = 'adjacency', 
    directed=TRUE, 
    loops = TRUE
  )
  
  # decompose
  decomposed <- decompose_statnet(net, weight = NULL)
  
  # check structure
  expect_equal(dim(decomposed$adj_mat), c(10, 10))
  expect_equal(rownames(decomposed$adj_mat), letters[1:10])
  expect_equal(colnames(decomposed$adj_mat), letters[1:10])
  expect_identical(decomposed$adj_mat, adjm)
  expect_null(decomposed$weight)
})

test_that('decompose_statnet: weighted cross-sec, asymmetric', {
  # generate test data
  adjm <- matrix(rnorm(10^2), ncol=10)
  rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]
  
  # create network object
  net <- network::network(
    adjm,
    matrix.type = 'adjacency', 
    directed=TRUE, 
    loops = TRUE, 
    names.eval='value',
    ignore.eval=FALSE
  )
  
  # decompose
  decomposed <- decompose_statnet(net, weight = "value")
  
  # check structure
  expect_equal(dim(decomposed$adj_mat), c(10, 10))
  expect_identical(decomposed$adj_mat, adjm)
  expect_equal(decomposed$weight, "value")
  
  # check edge data includes weights
  if (!is.null(decomposed$ddata)) {
    expect_true("value" %in% names(decomposed$ddata))
  }
})

test_that('decompose_statnet: weighted cross-sec with dyad and nodal attribs', {
  # create test data
  fakeDyads <- expand.grid(actor1 = letters[1:3], actor2 = letters[1:3])
  fakeDyads$weight <- rnorm(nrow(fakeDyads))
  fakeDyads$var2 <- rnorm(nrow(fakeDyads))
  fakeDyads$var3 <- rnorm(nrow(fakeDyads))
  fakeDyads$actor1 = as.character(fakeDyads$actor1)
  fakeDyads$actor2 = as.character(fakeDyads$actor2)
  fakeDyads <- fakeDyads[fakeDyads$actor1!=fakeDyads$actor2,]
  
  # create network from adjacency matrix first
  adjm <- matrix(0, nrow=3, ncol=3)
  rownames(adjm) = colnames(adjm) = letters[1:3]
  for(i in 1:nrow(fakeDyads)) {
    row_idx <- which(rownames(adjm) == fakeDyads$actor1[i])
    col_idx <- which(colnames(adjm) == fakeDyads$actor2[i])
    adjm[row_idx, col_idx] <- fakeDyads$weight[i]
  }
  
  net <- network::network(
    adjm,
    matrix.type = 'adjacency',
    directed = TRUE,
    names.eval = 'weight',
    ignore.eval = FALSE
  )
  
  # add vertex attributes
  network::set.vertex.attribute(net, "var1", rnorm(3))
  network::set.vertex.attribute(net, "var2", rnorm(3))
  
  # add edge attributes
  network::set.edge.attribute(net, "var2", fakeDyads$var2)
  network::set.edge.attribute(net, "var3", fakeDyads$var3)
  
  # decompose
  decomposed <- decompose_statnet(net, weight = "weight")
  
  # check structure
  expect_equal(dim(decomposed$adj_mat), c(3, 3))
  expect_equal(rownames(decomposed$adj_mat), letters[1:3])
  
  # check node data
  expect_true(!is.null(decomposed$ndata))
  expect_true("var1" %in% names(decomposed$ndata))
  expect_true("var2" %in% names(decomposed$ndata))
  expect_equal(nrow(decomposed$ndata), 3)
  
  # check edge data
  expect_true(!is.null(decomposed$ddata))
  expect_true(all(c("from", "to", "weight", "var2", "var3") %in% names(decomposed$ddata)))
})

test_that('decompose_statnet: bipartite networks', {
  # generate bipartite test data
  adjm <- matrix(
    as.numeric(sample(0:1, 50, replace=TRUE, prob=c(0.6, .4))), 
    nrow=5, ncol=10)
  rownames(adjm) = letters[1:5]
  colnames(adjm) = letters[17:26]
  
  # create bipartite network
  net <- network::network(
    adjm,
    matrix.type = 'adjacency',
    directed=TRUE, 
    loops = TRUE, 
    bipartite=TRUE
  )
  
  # add some vertex attributes
  network::set.vertex.attribute(net, "node_attr", rnorm(15))
  
  # decompose
  decomposed <- decompose_statnet(net)
  
  # for statnet bipartite, we get the full square matrix
  # the first 5 rows/cols are the first partition
  expect_equal(dim(decomposed$adj_mat), c(5, 10))
  expect_equal(rownames(decomposed$adj_mat), letters[1:5])
  expect_equal(colnames(decomposed$adj_mat), letters[17:26])
  
  # check node data
  expect_true(!is.null(decomposed$ndata))
  expect_true("node_attr" %in% names(decomposed$ndata))
  expect_equal(nrow(decomposed$ndata), 15)  # total vertices
})

test_that('decompose_statnet: symmetric networks', {
  # generate symmetric test data
  adjm <- matrix(rnorm(10^2), ncol=10)
  adjm <- (adjm + t(adjm))/2
  diag(adjm) <- NA
  rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]
  
  # create undirected network
  net <- network::network(
    adjm,
    matrix.type = 'adjacency',
    directed=FALSE, 
    loops = FALSE, 
    names.eval='value',
    ignore.eval=FALSE
  )
  
  # decompose
  decomposed <- decompose_statnet(net, weight = "value")
  
  # check structure
  expect_equal(dim(decomposed$adj_mat), c(10, 10))
  expect_true(isSymmetric(decomposed$adj_mat, check.attributes = FALSE))
})

test_that("decompose_statnet works for cross-sectional networks", {
  # create network object
  edges <- matrix(c(1, 2, 1, 3, 2, 1, 2, 3, 3, 1), ncol = 2, byrow = TRUE)
  net <- network::network(edges, directed = TRUE, vertices = 3)
  
  # add attributes
  network::set.vertex.attribute(net, "vertex.names", c("A", "B", "C"))
  network::set.vertex.attribute(net, "node_attr", c(10, 20, 30))
  network::set.edge.attribute(net, "weight", c(1, 2, 3, 4, 5))
  network::set.edge.attribute(net, "edge_attr", c("x", "y", "x", "y", "x"))
  
  # decompose
  decomposed <- decompose_statnet(net, weight = "weight")
  
  # check adjacency matrix
  expect_equal(dim(decomposed$adj_mat), c(3, 3))
  expect_true(all(rownames(decomposed$adj_mat) %in% c("A", "B", "C")))
  
  # check node data
  expect_true(!is.null(decomposed$ndata))
  expect_true("node_attr" %in% names(decomposed$ndata))
  expect_equal(nrow(decomposed$ndata), 3)
  
  # check edge data
  expect_true(!is.null(decomposed$ddata))
  expect_true(all(c("from", "to", "weight", "edge_attr") %in% names(decomposed$ddata)))
  expect_equal(nrow(decomposed$ddata), 5)
})

test_that("decompose_statnet works for longitudinal networks (as network list)", {
  # create two network objects for different time points
  net_t1 <- network::network(matrix(c(1, 2, 1, 3, 2, 3), ncol = 2, byrow = TRUE), 
                             directed = TRUE)
  net_t2 <- network::network(matrix(c(1, 2, 2, 3, 3, 1), ncol = 2, byrow = TRUE), 
                             directed = TRUE)
  
  network::set.edge.attribute(net_t1, "weight", c(1, 2, 3))
  network::set.edge.attribute(net_t2, "weight", c(2, 4, 5))
  
  # decompose each
  decomp_t1 <- decompose_statnet(net_t1, weight = "weight")
  decomp_t2 <- decompose_statnet(net_t2, weight = "weight")
  
  # check both have same dimensions
  expect_equal(dim(decomp_t1$adj_mat), c(3, 3))
  expect_equal(dim(decomp_t2$adj_mat), c(3, 3))
  
  # check different edge counts
  expect_equal(nrow(decomp_t1$ddata), 3)
  expect_equal(nrow(decomp_t2$ddata), 3)
})

test_that("decompose_statnet works for bipartite networks", {
  # create bipartite network
  edges <- matrix(c(1, 4, 1, 5, 2, 5, 2, 6, 3, 5), ncol = 2, byrow = TRUE)
  net <- network::network(edges, directed = FALSE, bipartite = 3)
  
  # add names
  network::set.vertex.attribute(net, "vertex.names", 
                                c("USA", "China", "Japan", "Oil", "Tech", "Steel"))
  network::set.edge.attribute(net, "weight", c(100, 200, 300, 150, 250))
  
  # decompose
  decomposed <- decompose_statnet(net, weight = "weight")
  
  # check adjacency matrix (full matrix for statnet bipartite)
  expect_equal(dim(decomposed$adj_mat), c(3, 3))
  expect_true(all(rownames(decomposed$adj_mat) %in% c("USA", "China", "Japan")))
  expect_true(all(colnames(decomposed$adj_mat) %in% c("Oil", "Tech", "Steel")))
  
  # check edge data
  expect_equal(nrow(decomposed$ddata), 5)
})

