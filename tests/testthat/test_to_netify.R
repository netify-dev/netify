set.seed(6886)

# cross section tests

test_that("to_netify handles single igraph objects", {
	# single igraph
	g = igraph::make_ring(5)
	igraph::E(g)$myweight = 1:5
	net_g = to_netify(g, weight = "myweight")
	expect_s3_class(net_g, "netify")
	expect_equal(attr(net_g, "weight"), "myweight")
})

test_that("to_netify handles single network objects", {
	# single network
	nw = network::network(
		matrix(
			rbinom(25, 1, 0.3), 5, 5
		),
		directed = FALSE
	)
	network::set.edge.attribute(
		nw, "myweight",
		1:network::network.size(nw)
	)
	net_nw = to_netify(nw, weight = "myweight")
	expect_s3_class(net_nw, "netify")
	expect_equal(attr(net_nw, "weight"), "myweight")
})

test_that("to_netify handles single matrices", {
	# single matrix
	mat = matrix(runif(25), 5, 5)
	rownames(mat) = colnames(mat) = paste0("actor", 1:5)
	net_mat = to_netify(mat)
	expect_s3_class(net_mat, "netify")
	expect_equal(attr(net_mat, "netify_type"), "cross_sec")
})

# longit tests

test_that("to_netify handles arrays comprehensively", {
	# 2D array (should be treated as matrix)
	arr_2d = array(runif(25), dim = c(5, 5))
	rownames(arr_2d) = colnames(arr_2d) = paste0("node", 1:5)
	net_2d = to_netify(arr_2d)
	expect_s3_class(net_2d, "netify")
	expect_equal(attr(net_2d, "netify_type"), "cross_sec")

	# 3D array without dimnames
	arr_3d_no_names = array(runif(60), dim = c(4, 4, 3))
	net_3d_no_names = to_netify(arr_3d_no_names)
	expect_s3_class(net_3d_no_names, "netify")
	expect_equal(attr(net_3d_no_names, "netify_type"), "longit_array")
})

test_that("to_netify handles list of igraph objects", {
	g = igraph::make_ring(5)
	igraph::E(g)$myweight = 1:5
	g2 = igraph::make_star(6)
	igraph::E(g2)$edge_color = "red"
	igraph::E(g2)$myweight = 6:10
	net_g_list = to_netify(
		list(g, g2),
		weight = "myweight"
	)
	expect_s3_class(net_g_list, "netify")
	expect_equal(
		attr(
			net_g_list, "netify_type"
		), "longit_list"
	)
	# check that adjacency was built for each
	expect_length(net_g_list, 2)
})

test_that("to_netify handles list of network objects", {
	nw = network::network(
		matrix(
			rbinom(25, 1, 0.3), 5, 5
		),
		directed = FALSE
	)
	network::set.edge.attribute(
		nw, "myweight",
		1:network::network.size(nw)
	)
	nw2 = network::network(
		matrix(
			rbinom(36, 1, 0.5), 6, 6
		),
		directed = FALSE
	)
	network::set.edge.attribute(
		nw2, "myweight",
		1:network::network.size(nw)
	)
	net_nw_list = to_netify(
		list(nw, nw2),
		weight = "myweight"
	)
	expect_s3_class(net_nw_list, "netify")
	expect_equal(attr(net_nw_list, "netify_type"), "longit_list")
	expect_length(net_nw_list, 2)
})

test_that("to_netify handles list of matrices", {
	mat_list = list(
		"2001" = matrix(runif(25), 5, 5),
		"2002" = matrix(runif(25), 5, 5)
	)
	for (i in 1:2) {
		rownames(mat_list[[i]]) = colnames(mat_list[[i]]) = paste0("actor", 1:5)
	}
	net_mat_list = to_netify(mat_list)
	expect_s3_class(net_mat_list, "netify")
	expect_equal(attr(net_mat_list, "netify_type"), "longit_list")
	expect_length(net_mat_list, 2)
})

test_that("to_netify handles longitudinal networks with different sizes", {
	# create networks of different sizes
	nw1 = network::network(matrix(rbinom(25, 1, 0.3), 5, 5), directed = FALSE)
	nw2 = network::network(matrix(rbinom(36, 1, 0.4), 6, 6), directed = FALSE)
	nw3 = network::network(matrix(rbinom(16, 1, 0.5), 4, 4), directed = FALSE)

	# add edge attributes
	network::set.edge.attribute(nw1, "weight", runif(network::network.edgecount(nw1)))
	network::set.edge.attribute(nw2, "weight", runif(network::network.edgecount(nw2)))
	network::set.edge.attribute(nw3, "weight", runif(network::network.edgecount(nw3)))

	# convert list with names
	net_list = to_netify(
		list("2001" = nw1, "2002" = nw2, "2003" = nw3),
		weight = "weight"
	)

	expect_s3_class(net_list, "netify")
	expect_equal(attr(net_list, "netify_type"), "longit_list")
	expect_length(net_list, 3)
	expect_equal(names(net_list), c("2001", "2002", "2003"))

	# check dyad data has correct dimensions for each time period
	dyad_data = attr(net_list, "dyad_data")
	expect_equal(dim(dyad_data[["2001"]][["weight"]]), c(5, 5))
	expect_equal(dim(dyad_data[["2002"]][["weight"]]), c(6, 6))
	expect_equal(dim(dyad_data[["2003"]][["weight"]]), c(4, 4))
})

# edge cases

test_that("to_netify handles lists without names", {
	# list of matrices without names
	mat1 = matrix(runif(9), 3, 3)
	mat2 = matrix(runif(9), 3, 3)
	rownames(mat1) = colnames(mat1) = paste0("n", 1:3)
	rownames(mat2) = colnames(mat2) = paste0("n", 1:3)

	unnamed_list = to_netify(list(mat1, mat2))
	expect_equal(names(unnamed_list), c("1", "2"))

	# list of igraphs without names
	g1 = igraph::make_ring(3)
	g2 = igraph::make_star(3)
	unnamed_g_list = to_netify(list(g1, g2))
	expect_equal(names(unnamed_g_list), c("1", "2"))
})

test_that("to_netify handles edge cases for lists", {
	# single-element list of matrix
	mat = matrix(runif(16), 4, 4)
	rownames(mat) = colnames(mat) = paste0("a", 1:4)
	single_mat_list = to_netify(list("2001" = mat))
	expect_s3_class(single_mat_list, "netify")
	expect_equal(attr(single_mat_list, "netify_type"), "longit_list")
	expect_length(single_mat_list, 1)

	# single-element list of igraph
	g = igraph::make_ring(4)
	single_g_list = to_netify(list(g))
	expect_s3_class(single_g_list, "netify")
	expect_equal(attr(single_g_list, "netify_type"), "longit_list")

	# single-element list of network
	nw = network::network(matrix(rbinom(16, 1, 0.5), 4, 4))
	single_nw_list = to_netify(list(nw))
	expect_s3_class(single_nw_list, "netify")
	expect_equal(attr(single_nw_list, "netify_type"), "longit_list")
})

# attrib specific tests

test_that("to_netify correctly extracts and converts dyad data", {
	# create igraph with edge attributes
	g = igraph::make_ring(5, directed = FALSE)
	igraph::E(g)$weight = 1:5
	igraph::E(g)$type = c("friend", "family", "work", "friend", "family")

	net_g = to_netify(g, weight = "weight")
	dyad_data = attr(net_g, "dyad_data")

	# check structure
	expect_true(is.list(dyad_data))
	expect_equal(names(dyad_data), "1")
	# `weight` is stored in the adjacency, so it should not be duplicated
	# in dyad_data; only non-weight edge attributes appear there
	expect_equal(names(dyad_data[["1"]]), "type")

	# weight values land in the adjacency itself
	adj = get_raw(net_g)
	expect_equal(dim(adj), c(5, 5))
	expect_equal(adj[1, 2], 1)
	expect_equal(adj[2, 3], 2)

	# check type matrix
	type_mat = dyad_data[["1"]][["type"]]
	expect_equal(type_mat[1, 2], "friend")
	expect_equal(type_mat[2, 3], "family")
})

test_that("to_netify handles vertex attributes correctly", {
	# create igraph with vertex attributes
	g = igraph::make_star(5)
	igraph::V(g)$name = c("Center", "Node1", "Node2", "Node3", "Node4")
	igraph::V(g)$type = c("hub", "spoke", "spoke", "spoke", "spoke")
	igraph::V(g)$value = c(10, 1, 2, 3, 4)

	# suppress warning: directed igraph with non-logical `type` attribute
	# is treated as unipartite with type kept as an attribute
	net_g = suppressWarnings(to_netify(g))
	nodal_data = attr(net_g, "nodal_data")

	# check nodal data
	expect_true(is.data.frame(nodal_data))
	# `name` is normalized to `actor` when redundant
	expect_equal(nodal_data$actor, c("Center", "Node1", "Node2", "Node3", "Node4"))
	expect_equal(nodal_data$type, c("hub", "spoke", "spoke", "spoke", "spoke"))
	expect_equal(nodal_data$value, c(10, 1, 2, 3, 4))
})

test_that("to_netify handles user-provided nodal and dyad data", {
	g = igraph::make_ring(5)

	# create nodal data
	nodal_data = data.frame(
		actor = paste0("a", 1:5),
		attribute1 = rnorm(5),
		attribute2 = letters[1:5],
		stringsAsFactors = FALSE
	)

	# create dyad data in correct format
	dyad_data = list(
		"1" = list(
			custom_weight = matrix(runif(25), 5, 5,
				dimnames = list(paste0("a", 1:5), paste0("a", 1:5))
			)
		)
	)

	net_g = to_netify(g, nodal_data = nodal_data, dyad_data = dyad_data)

	# check that user data was used
	expect_equal(attr(net_g, "nodal_data"), nodal_data)
	expect_equal(attr(net_g, "dyad_data"), dyad_data)
})

# bipartite tests

test_that("to_netify handles bipartite graphs correctly", {
	# create bipartite igraph
	g_bip = igraph::make_bipartite_graph(
		c(TRUE, TRUE, FALSE, FALSE, FALSE),
		c(1, 3, 1, 4, 2, 3, 2, 5)
	)
	igraph::E(g_bip)$weight = 1:4

	net_bip = to_netify(g_bip, weight = "weight")
	adj_mat = net_bip

	# check dimensions and naming
	expect_equal(rownames(adj_mat), c("r1", "r2", "r3"))
	expect_equal(colnames(adj_mat), c("c1", "c2"))
	expect_equal(dim(adj_mat), c(3, 2))
})

test_that("to_netify handles bipartite networks correctly", {
	# bipartite network object
	nw_bip = network::network.initialize(7, bipartite = 3, directed = FALSE)
	network::add.edges(nw_bip,
		tail = c(1, 1, 2, 2, 3),
		head = c(4, 5, 5, 6, 7)
	)
	network::set.edge.attribute(nw_bip, "weight", runif(5))

	net_bip = to_netify(nw_bip, weight = "weight")
	adj_mat = net_bip

	# check dimensions and naming for bipartite network
	expect_equal(dim(adj_mat), c(3, 4))
	expect_equal(rownames(adj_mat), c("r1", "r2", "r3"))
	expect_equal(colnames(adj_mat), c("c1", "c2", "c3", "c4"))
})

test_that("to_netify handles directed vs undirected networks", {
	# directed igraph
	g_dir = igraph::make_ring(4, directed = TRUE)
	igraph::E(g_dir)$weight = 1:4
	net_dir = to_netify(g_dir, weight = "weight")

	# check that directed edges are not symmetrized; weight lives in the
	# adjacency, so test the adjacency itself rather than dyad_data
	adj_dir = get_raw(net_dir)
	expect_false(isSymmetric(unname(replace(adj_dir, is.na(adj_dir), 0))))

	# undirected igraph
	g_undir = igraph::make_ring(4, directed = FALSE)
	igraph::E(g_undir)$weight = 1:4
	net_undir = to_netify(g_undir, weight = "weight")

	adj_undir = get_raw(net_undir)
	expect_true(isSymmetric(unname(replace(adj_undir, is.na(adj_undir), 0))))
})

test_that("to_netify handles weighted networks correctly", {
	# weighted matrix
	w_mat = matrix(runif(16), 4, 4)
	diag(w_mat) = 0
	rownames(w_mat) = colnames(w_mat) = paste0("v", 1:4)

	net_wmat = to_netify(w_mat)
	expect_false(attr(net_wmat, "is_binary"))

	# binary matrix
	b_mat = matrix(sample(0:1, 16, replace = TRUE), 4, 4)
	diag(b_mat) = 0
	rownames(b_mat) = colnames(b_mat) = paste0("v", 1:4)

	net_bmat = to_netify(b_mat)
	expect_true(attr(net_bmat, "is_binary"))
})

# data checks

test_that("to_netify validates and aligns data correctly", {
	# create networks with specific actor names
	g1 = igraph::make_ring(4)
	igraph::V(g1)$name = c("USA", "CHN", "RUS", "GBR")
	igraph::E(g1)$trade = c(100, 200, 150, 180)

	g2 = igraph::make_ring(4)
	igraph::V(g2)$name = c("USA", "CHN", "RUS", "GBR")
	igraph::E(g2)$trade = c(120, 210, 160, 190)

	net_list = to_netify(
		list("2001" = g1, "2002" = g2),
		weight = "trade"
	)

	# check that actor names are preserved
	expect_equal(rownames(net_list[[1]]), c("USA", "CHN", "RUS", "GBR"))
	expect_equal(rownames(net_list[[2]]), c("USA", "CHN", "RUS", "GBR"))

	# trade is the weight, so it lives in the adjacency
	adj_2001 = net_list[[1]]
	adj_2002 = net_list[[2]]
	expect_equal(adj_2001["USA", "CHN"], 100)
	expect_equal(adj_2002["USA", "CHN"], 120)
})

test_that("to_netify preserves attributes across different input types", {
	# create equivalent networks in different formats
	set.seed(6886)
	adj = matrix(c(
		0, 1, 1, 0,
		1, 0, 1, 1,
		1, 1, 0, 1,
		0, 1, 1, 0
	), 4, 4)
	rownames(adj) = colnames(adj) = c("A", "B", "C", "D")

	# matrix version
	net_mat = to_netify(adj)

	# igraph version
	g = igraph::graph_from_adjacency_matrix(adj, mode = "undirected")
	net_ig = to_netify(g)

	# network version
	nw = network::network(adj, directed = FALSE)
	net_nw = to_netify(nw)

	# all should produce same adjacency structure modulo diag-to-NA stamping
	strip_diag <- function(m) {
		out <- m
		diag(out) <- 0
		out[is.na(out)] <- 0
		out
	}
	expect_equal(strip_diag(get_raw(net_mat)), strip_diag(net_ig[, ]))
	expect_equal(strip_diag(get_raw(net_mat)), strip_diag(net_nw[, ]))
})

# icews tests

test_that("to_netify works with complex real-world data structures", {
	# simulate ICEWS-like data
	set.seed(6886)
	n_actors = 10
	actor_names = paste0("Country", LETTERS[1:n_actors])

	# create cross-sectional network
	mat = matrix(rpois(n_actors^2, lambda = 2), n_actors, n_actors)
	diag(mat) = 0
	rownames(mat) = colnames(mat) = actor_names

	# add nodal attributes
	nodal_data = data.frame(
		actor = actor_names,
		gdp = rlnorm(n_actors, 10, 1),
		polity = sample(-10:10, n_actors, replace = TRUE),
		stringsAsFactors = FALSE
	)

	# add dyadic attributes
	dyad_vars = list(
		"1" = list(
			trade = matrix(rlnorm(n_actors^2, 5, 1), n_actors, n_actors,
				dimnames = list(actor_names, actor_names)
			),
			alliance = matrix(sample(0:1, n_actors^2, replace = TRUE), n_actors, n_actors,
				dimnames = list(actor_names, actor_names)
			)
		)
	)

	net = to_netify(mat, nodal_data = nodal_data, dyad_data = dyad_vars)

	expect_s3_class(net, "netify")
	expect_equal(dim(net), c(n_actors, n_actors))
	expect_equal(attr(net, "nodal_data"), nodal_data)
	expect_equal(attr(net, "dyad_data"), dyad_vars)
})

# error handling tests

test_that("to_netify error handling", {
	# unsupported object type
	expect_error(
		to_netify(data.frame(a = 1:5))
	)

	# mixed list types
	g = igraph::make_ring(3)
	mat = matrix(1:9, 3, 3)
	expect_error(
		to_netify(list(g, mat))
	)

	# invalid nodal data
	g = igraph::make_ring(3)
	expect_error(
		to_netify(g, nodal_data = "not a dataframe")
	)
})
