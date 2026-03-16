# simplified test cases for multilayer support in attribute analysis functions
set.seed(6886)

library(testthat)
library(netify)

test_that("homophily works with basic cross-sectional multilayer networks", {
	# create simple test data with sparse networks
	n = 20
	mat1 = matrix(0, n, n)
	mat2 = matrix(0, n, n)

	# add random edges to make networks sparse
	n_edges = round(n * n * 0.2) # 20% density
	edges1 = sample(n * n, n_edges)
	edges2 = sample(n * n, n_edges)
	mat1[edges1] = runif(n_edges)
	mat2[edges2] = runif(n_edges)

	# remove self-loops
	diag(mat1) = 0
	diag(mat2) = 0

	# create networks
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	# add attributes - use actual actor names from the network
	actors = rownames(net1)
	nodes = data.frame(
		actor = actors,
		attr1 = rnorm(n),
		attr2 = sample(c("A", "B"), n, replace = TRUE)
	)

	net1 = add_node_vars(net1, nodes, actor = "actor")
	net2 = add_node_vars(net2, nodes, actor = "actor")

	# create multilayer
	multi = layer_netify(
		list(net1, net2),
		layer_labels = c("Layer1", "Layer2")
	)

	# test homophily
	result = homophily(multi, attribute = "attr1", method = "correlation")

	expect_s3_class(result, "data.frame")
	expect_equal(nrow(result), 2)
	expect_true("layer" %in% names(result))
	expect_equal(sort(unique(result$layer)), c("Layer1", "Layer2"))
	expect_true(all(!is.na(result$homophily_correlation)))
})

test_that("mixing_matrix works with basic cross-sectional multilayer networks", {
	# create simple test data
	n = 20
	mat1 = matrix(runif(n * n), n, n)
	mat2 = matrix(runif(n * n), n, n)

	# create networks
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	# add categorical attribute
	nodes = data.frame(
		actor = rownames(net1),
		category = sample(c("A", "B", "C"), n, replace = TRUE)
	)

	net1 = add_node_vars(net1, nodes, actor = "actor")
	net2 = add_node_vars(net2, nodes, actor = "actor")

	# create multilayer
	multi = layer_netify(
		list(net1, net2),
		layer_labels = c("Layer1", "Layer2")
	)

	# test mixing matrix
	result = mixing_matrix(multi, attribute = "category")

	expect_type(result, "list")
	expect_true("mixing_matrices" %in% names(result))
	expect_true("summary_stats" %in% names(result))
	expect_equal(length(result$mixing_matrices), 2)
	expect_equal(nrow(result$summary_stats), 2)
})

test_that("dyad_correlation works with basic cross-sectional multilayer networks", {
	# create simple test data
	n = 10
	mat1 = matrix(runif(n * n), n, n)
	mat2 = matrix(runif(n * n), n, n)

	# create dyadic data
	dyad_mat = matrix(rnorm(n * n), n, n)

	# create networks
	edges1 = data.frame(
		from = rep(1:n, each = n),
		to = rep(1:n, n),
		weight = as.vector(mat1),
		dyad_var = as.vector(dyad_mat)
	)
	edges1 = edges1[edges1$from != edges1$to, ]

	edges2 = data.frame(
		from = rep(1:n, each = n),
		to = rep(1:n, n),
		weight = as.vector(mat2),
		dyad_var = as.vector(dyad_mat)
	)
	edges2 = edges2[edges2$from != edges2$to, ]

	net1 = netify(edges1,
		actor1 = "from", actor2 = "to",
		weight = "weight", dyad_vars = "dyad_var"
	)
	net2 = netify(edges2,
		actor1 = "from", actor2 = "to",
		weight = "weight", dyad_vars = "dyad_var"
	)

	# create multilayer
	multi = layer_netify(
		list(net1, net2),
		layer_labels = c("Layer1", "Layer2")
	)

	# test dyad correlation
	result = dyad_correlation(multi, dyad_vars = "dyad_var")

	expect_s3_class(result, "data.frame")
	# by default, dyad_correlation tests against all edge variables (layers)
	# so for 2 layers, we get 2 layers × 2 edge_vars = 4 rows
	expect_equal(nrow(result), 4)
	expect_true("layer" %in% names(result))
	expect_true("edge_var" %in% names(result))
})

test_that("plot_homophily handles multilayer networks correctly", {
	# create simple multilayer network
	n = 20
	mat1 = matrix(runif(n * n), n, n)
	mat2 = matrix(runif(n * n), n, n)

	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	nodes = data.frame(
		actor = rownames(net1),
		attr = rnorm(n)
	)

	net1 = add_node_vars(net1, nodes, actor = "actor")
	net2 = add_node_vars(net2, nodes, actor = "actor")

	multi = layer_netify(
		list(net1, net2),
		layer_labels = c("L1", "L2")
	)

	# get homophily results
	homophily_results = homophily(multi, attribute = "attr")

	# test comparison plot (should work without issues)
	plot_comp = plot_homophily(homophily_results, type = "comparison")
	expect_s3_class(plot_comp, "ggplot")

	# check that the plot has the right aesthetics
	plot_data = ggplot2::ggplot_build(plot_comp)$data[[1]]
	expect_true("y" %in% names(plot_data))
})

test_that("attribute_report works with basic multilayer networks", {
	# create simple test data
	n = 15
	mat1 = matrix(runif(n * n), n, n)
	mat2 = matrix(runif(n * n), n, n)

	# create networks
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	# add attributes
	nodes = data.frame(
		actor = rownames(net1),
		attr1 = rnorm(n),
		attr2 = sample(c("X", "Y"), n, replace = TRUE)
	)

	net1 = add_node_vars(net1, nodes, actor = "actor")
	net2 = add_node_vars(net2, nodes, actor = "actor")

	# create multilayer
	multi = layer_netify(
		list(net1, net2),
		layer_labels = c("Layer1", "Layer2")
	)

	# test attribute report
	result = attribute_report(
		multi,
		node_vars = "attr1",
		include_mixing = FALSE,
		include_dyadic_correlations = FALSE,
		include_centrality = FALSE,
		significance_test = FALSE
	)

	expect_type(result, "list")

	# check homophily results
	if (!is.null(result$homophily_analysis)) {
		expect_s3_class(result$homophily_analysis, "data.frame")
		expect_true("layer" %in% names(result$homophily_analysis))
		expect_equal(nrow(result$homophily_analysis), 2)
	}
})

test_that("multilayer functions handle binary networks correctly", {
	# create binary networks
	n = 15
	mat1 = matrix(sample(0:1, n * n, replace = TRUE), n, n)
	mat2 = matrix(sample(0:1, n * n, replace = TRUE), n, n)

	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	# verify they are binary
	expect_true(attr(net1, "is_binary"))
	expect_true(attr(net2, "is_binary"))

	# add attributes
	nodes = data.frame(
		actor = rownames(net1),
		attr = sample(c("A", "B"), n, replace = TRUE)
	)

	net1 = add_node_vars(net1, nodes, actor = "actor")
	net2 = add_node_vars(net2, nodes, actor = "actor")

	# create multilayer
	multi = layer_netify(
		list(net1, net2),
		layer_labels = c("Binary1", "Binary2")
	)

	# is_binary should be a vector
	expect_equal(length(attr(multi, "is_binary")), 2)
	expect_true(all(attr(multi, "is_binary")))

	# test homophily works
	result = homophily(multi, attribute = "attr", method = "categorical")
	expect_s3_class(result, "data.frame")
	expect_equal(nrow(result), 2)
})

test_that("multilayer functions handle mixed binary/weighted networks", {
	# create one binary and one weighted network
	n = 15
	mat1 = matrix(sample(0:1, n * n, replace = TRUE), n, n) # binary
	mat2 = matrix(runif(n * n), n, n) # weighted

	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	# add attributes
	nodes = data.frame(
		actor = rownames(net1),
		attr = rnorm(n)
	)

	net1 = add_node_vars(net1, nodes, actor = "actor")
	net2 = add_node_vars(net2, nodes, actor = "actor")

	# create multilayer
	multi = layer_netify(
		list(net1, net2),
		layer_labels = c("Binary", "Weighted")
	)

	# is_binary should reflect the mixed nature
	wb = attr(multi, "is_binary")
	expect_equal(length(wb), 2)
	expect_true(wb[1]) # first is binary
	expect_false(wb[2]) # second is weighted

	# test homophily works with mixed types
	result = homophily(multi, attribute = "attr")
	expect_s3_class(result, "data.frame")
	expect_equal(nrow(result), 2)

	# check that threshold is NA for binary layer
	expect_true(is.na(result[result$layer == "Binary", "threshold_value"]))
	expect_false(is.na(result[result$layer == "Weighted", "threshold_value"]))
})

test_that("single layer networks still work with updated functions", {
	# create single layer network
	n = 20
	mat = matrix(runif(n * n), n, n)
	net = new_netify(mat)

	nodes = data.frame(
		actor = rownames(net),
		attr1 = rnorm(n),
		attr2 = sample(c("A", "B"), n, replace = TRUE)
	)

	net = add_node_vars(net, nodes, actor = "actor")

	# test homophily
	result_hom = homophily(net, attribute = "attr1")
	expect_s3_class(result_hom, "data.frame")
	expect_equal(nrow(result_hom), 1)
	expect_true("layer" %in% names(result_hom))
	# for single layer networks, the layer name depends on how it was created
	# new_netify uses "edge_value" as the default layer name
	expect_true(result_hom$layer[1] %in% c("1", "edge_value", attr(net, "weight")))

	# test mixing matrix
	result_mix = mixing_matrix(net, attribute = "attr2")
	expect_type(result_mix, "list")
	expect_equal(length(result_mix$mixing_matrices), 1)
	expect_equal(nrow(result_mix$summary_stats), 1)
})
