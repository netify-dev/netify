# multilayer-specific tests for attribute analysis functions
set.seed(6886)

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

	wb = attr(multi, "is_binary")
	expect_equal(length(wb), 2)
	expect_true(wb[1]) # first is binary
	expect_false(wb[2]) # second is weighted

	# test homophily works with mixed types
	result = homophily(multi, attribute = "attr")
	expect_s3_class(result, "data.frame")
	expect_equal(nrow(result), 2)

	# check that threshold is na for binary layer
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
	expect_true(result_hom$layer[1] %in% c("1", "edge_value", attr(net, "weight")))

	# test mixing matrix
	result_mix = mixing_matrix(net, attribute = "attr2")
	expect_type(result_mix, "list")
	expect_equal(length(result_mix$mixing_matrices), 1)
	expect_equal(nrow(result_mix$summary_stats), 1)
})
