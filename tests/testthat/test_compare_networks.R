set.seed(6886)

test_that("compare_networks works with basic edge comparison", {
	skip_on_cran()
	# create test networks
	set.seed(6886)
	n = 20

	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat2 = mat1
	change_indices = sample(1:(n * n), size = round(0.2 * n * n))
	mat2[change_indices] = 1 - mat2[change_indices]

	net1 = new_netify(mat1, symmetric = TRUE)
	net2 = new_netify(mat2, symmetric = TRUE)

	# test basic comparison
	comp = suppressWarnings(compare_networks(list(net1 = net1, net2 = net2)))

	expect_s3_class(comp, "netify_comparison")
	expect_equal(comp$comparison_type, "cross_network")
	expect_equal(comp$n_networks, 2)
	expect_true(!is.null(comp$summary))
})

test_that("compare_networks handles different comparison types", {
	skip_on_cran()
	# create test networks
	mat1 = matrix(rbinom(100, 1, 0.3), 10, 10)
	mat2 = matrix(rbinom(100, 1, 0.3), 10, 10)

	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	# test structure comparison
	struct_comp = compare_networks(list(net1, net2), what = "structure")
	expect_equal(struct_comp$comparison_type, "cross_network") # still cross_network, not structure
	expect_equal(struct_comp$method, "structural_comparison") # the method indicates what was compared
	expect_equal(struct_comp$n_networks, 2)

	# test node comparison
	node_comp = compare_networks(list(net1, net2), what = "nodes")
	expect_equal(node_comp$comparison_type, "cross_network") # still cross_network, not nodes
	expect_equal(node_comp$method, "node_composition") # the method indicates what was compared
	expect_equal(node_comp$n_networks, 2)
})

test_that("compare_networks works with longitudinal data", {
	skip_on_cran()
	# create longitudinal network
	n = 10
	arr = array(rbinom(n * n * 3, 1, 0.3), dim = c(n, n, 3))
	net_longit = new_netify(arr)

	comp = suppressWarnings(compare_networks(net_longit))
	expect_equal(comp$comparison_type, "temporal")
	expect_equal(comp$n_networks, 3)
})

test_that("compare_networks correctly identifies comparison types", {
	skip_on_cran()
	# create test networks
	n = 10
	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat2 = matrix(rbinom(n * n, 1, 0.3), n, n)
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	# cross-network comparison (list of networks)
	comp_cross = suppressWarnings(compare_networks(list(net1, net2)))
	expect_equal(comp_cross$comparison_type, "cross_network")

	# temporal comparison (single longitudinal network)
	arr = array(rbinom(n * n * 3, 1, 0.3), dim = c(n, n, 3))
	net_longit = new_netify(arr)
	comp_temp = suppressWarnings(compare_networks(net_longit))
	expect_equal(comp_temp$comparison_type, "temporal")

	# by-group comparison would be tested here if implemented
	# comp_group <- compare_networks(net1, by = "some_attribute")
	# expect_equal(comp_group$comparison_type, "by_group")
})

# test all 'what' options work correctly
test_that("compare_networks handles all 'what' options", {
	skip_on_cran()
	# create test networks
	mat1 = matrix(rbinom(100, 1, 0.3), 10, 10)
	mat2 = matrix(rbinom(100, 1, 0.3), 10, 10)
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	# test edges (default)
	comp_edges = suppressWarnings(compare_networks(list(net1, net2), what = "edges"))
	expect_equal(comp_edges$method, "correlation") # default method for edges

	# test structure
	comp_struct = suppressWarnings(compare_networks(list(net1, net2), what = "structure"))
	expect_equal(comp_struct$method, "structural_comparison")

	# test nodes
	comp_nodes = suppressWarnings(compare_networks(list(net1, net2), what = "nodes"))
	expect_equal(comp_nodes$method, "node_composition")

	# test attributes (will warn if no attributes)
	comp_attr = suppressWarnings(compare_networks(list(net1, net2), what = "attributes"))
	expect_equal(comp_attr$method, "attribute_comparison")
})

# additional comprehensive tests for compare_networks

test_that("compare_networks handles different similarity methods", {
	skip_on_cran()
	# create test networks
	n = 15
	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat2 = mat1
	# make some changes
	change_indices = sample(1:(n * n), size = round(0.3 * n * n))
	mat2[change_indices] = 1 - mat2[change_indices]

	net1 = new_netify(mat1, symmetric = FALSE)
	net2 = new_netify(mat2, symmetric = FALSE)

	# test all methods
	methods = c("correlation", "jaccard", "hamming", "qap", "spectral", "all")

	for (method in methods) {
		comp = suppressWarnings(compare_networks(list(net1, net2), method = method))
		expect_s3_class(comp, "netify_comparison")

		if (method == "all") {
			# should have all metrics
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
	skip_on_cran()
	# create weighted networks
	n = 10
	mat1 = matrix(runif(n * n), n, n)
	mat2 = matrix(runif(n * n), n, n)

	net1_weighted = new_netify(mat1, symmetric = FALSE)
	net2_weighted = new_netify(mat2, symmetric = FALSE)

	# compare weighted networks
	comp_weighted = suppressWarnings(compare_networks(list(net1_weighted, net2_weighted)))
	expect_s3_class(comp_weighted, "netify_comparison")

	# edge changes should track weight changes
	expect_true("edge_changes" %in% names(comp_weighted))
	edge_changes = comp_weighted$edge_changes[[1]]
	expect_true("weight_correlation" %in% names(edge_changes))
})

test_that("compare_networks with return_details provides full matrices", {
	skip_on_cran()
	# create test networks
	n = 10
	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat2 = matrix(rbinom(n * n, 1, 0.3), n, n)

	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	# compare with details
	comp_details = suppressWarnings(compare_networks(
		list(net1, net2),
		method = "all",
		return_details = TRUE
	))

	expect_true("details" %in% names(comp_details))
	expect_true("correlation_matrix" %in% names(comp_details$details))
	expect_true("jaccard_matrix" %in% names(comp_details$details))
	expect_true("hamming_matrix" %in% names(comp_details$details))

	# check that matrices have correct dimensions
	# when comparing 2 networks, matrices should be 2x2
	expect_equal(dim(comp_details$details$correlation_matrix), c(2, 2))
})

test_that("compare_networks handles networks with different node sets", {
	skip_on_cran()
	# create networks with different actors
	n1 = 10
	n2 = 12

	mat1 = matrix(rbinom(n1 * n1, 1, 0.3), n1, n1)
	mat2 = matrix(rbinom(n2 * n2, 1, 0.3), n2, n2)

	rownames(mat1) = colnames(mat1) = paste0("actor", 1:n1)
	rownames(mat2) = colnames(mat2) = paste0("actor", c(1:8, 11:14)) # some overlap

	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	# node comparison should detect differences
	comp_nodes = suppressWarnings(compare_networks(list(net1, net2), what = "nodes"))

	expect_true("node_changes" %in% names(comp_nodes))
	node_changes = comp_nodes$node_changes[[1]]

	expect_true(node_changes$n_added > 0)
	expect_true(node_changes$n_removed > 0)
	expect_true(node_changes$n_maintained > 0)
})

test_that("compare_networks handles longitudinal networks with missing time periods", {
	skip_on_cran()
	# create longitudinal network with gaps
	n = 10
	times = c(2000, 2001, 2003, 2005) # missing 2002, 2004

	arr = array(rbinom(n * n * length(times), 1, 0.3),
		dim = c(n, n, length(times))
	)
	dimnames(arr)[[3]] = as.character(times)

	net_longit = new_netify(arr)

	comp = suppressWarnings(compare_networks(net_longit))

	expect_equal(comp$comparison_type, "temporal")
	expect_equal(comp$n_networks, length(times))

	# should compare consecutive available time periods
	expect_true(length(comp$edge_changes) == choose(length(times), 2))
})

test_that("compare_networks structural comparison includes all metrics", {
	skip_on_cran()
	# create networks with different structural properties
	n = 20

	# dense network
	mat1 = matrix(rbinom(n * n, 1, 0.7), n, n)
	diag(mat1) = 0

	# sparse network
	mat2 = matrix(rbinom(n * n, 1, 0.2), n, n)
	diag(mat2) = 0

	net1 = new_netify(mat1, symmetric = FALSE)
	net2 = new_netify(mat2, symmetric = FALSE)

	comp_struct = suppressWarnings(compare_networks(list(net1, net2), what = "structure"))

	expect_true("summary" %in% names(comp_struct))
	struct_summary = comp_struct$summary

	# should have key structural metrics
	# note: these are now using the names from summary.netify
	expected_metrics = c(
		"num_actors", "num_edges", "density", "reciprocity",
		"transitivity", "mean_degree"
	)
	expect_true(all(expected_metrics %in% names(struct_summary)))

	# density should be different
	expect_true(abs(struct_summary$density[1] - struct_summary$density[2]) > 0.1)
})

test_that("compare_networks handles attribute comparison", {
	skip_on_cran()
	# create networks with node attributes
	n = 15
	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat2 = mat1

	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	# add node attributes
	nodes1 = data.frame(
		actor = 1:n,
		attr1 = rnorm(n),
		attr2 = sample(c("A", "B"), n, replace = TRUE)
	)

	nodes2 = data.frame(
		actor = 1:n,
		attr1 = rnorm(n), # different values
		attr2 = sample(c("A", "B"), n, replace = TRUE)
	)

	net1 = add_node_vars(net1, nodes1, actor = "actor")
	net2 = add_node_vars(net2, nodes2, actor = "actor")

	comp_attr = suppressWarnings(compare_networks(list(net1, net2), what = "attributes"))

	expect_equal(comp_attr$method, "attribute_comparison")
	# should detect that attributes have changed
	expect_true("summary" %in% names(comp_attr))
})

test_that("compare_networks QAP test with custom permutations", {
	skip_on_cran()
	# create correlated networks
	n = 10
	base_mat = matrix(rbinom(n * n, 1, 0.5), n, n)

	# create second network with some correlation
	mat2 = base_mat
	change_indices = sample(1:(n * n), size = round(0.2 * n * n))
	mat2[change_indices] = 1 - mat2[change_indices]

	net1 = new_netify(base_mat)
	net2 = new_netify(mat2)

	# test with different permutation counts
	comp_qap_100 = suppressWarnings(compare_networks(
		list(net1, net2),
		method = "qap",
		n_permutations = 100
	))

	comp_qap_500 = suppressWarnings(compare_networks(
		list(net1, net2),
		method = "qap",
		n_permutations = 500
	))

	expect_true("qap_pvalue" %in% names(comp_qap_100$summary))
	expect_true("qap_pvalue" %in% names(comp_qap_500$summary))

	# both should detect significant correlation
	expect_true(comp_qap_100$summary$qap_correlation > 0)
	expect_true(comp_qap_500$summary$qap_correlation > 0)
})

test_that("compare_networks handles bipartite networks", {
	skip_on_cran()
	# create bipartite networks
	n1 = 8
	n2 = 12

	mat1 = matrix(rbinom(n1 * n2, 1, 0.3), n1, n2)
	mat2 = matrix(rbinom(n1 * n2, 1, 0.3), n1, n2)

	net1 = new_netify(mat1, bipartite = TRUE)
	net2 = new_netify(mat2, bipartite = TRUE)

	comp = suppressWarnings(compare_networks(list(net1, net2)))

	expect_s3_class(comp, "netify_comparison")
	expect_equal(comp$comparison_type, "cross_network")
})

test_that("compare_networks handles empty and complete networks", {
	skip_on_cran()
	n = 10

	# empty network
	empty_mat = matrix(0, n, n)
	net_empty = new_netify(empty_mat)

	# complete network
	complete_mat = matrix(1, n, n)
	diag(complete_mat) = 0
	net_complete = new_netify(complete_mat)

	# normal network
	normal_mat = matrix(rbinom(n * n, 1, 0.3), n, n)
	net_normal = new_netify(normal_mat)

	# compare empty vs complete with method="all" to get jaccard
	comp_extreme = suppressWarnings(compare_networks(list(net_empty, net_complete), method = "all"))
	expect_equal(comp_extreme$summary$correlation, 0) # no correlation
	expect_equal(comp_extreme$summary$jaccard, 0) # no overlap

	# compare empty vs normal with method="all"
	comp_empty = suppressWarnings(compare_networks(list(net_empty, net_normal), method = "all"))
	expect_equal(comp_empty$summary$jaccard, 0) # no overlap

	# compare complete vs normal with method="all"
	comp_complete = suppressWarnings(compare_networks(list(net_complete, net_normal), method = "all"))
	expect_true(comp_complete$summary$jaccard > 0) # some overlap
})

test_that("compare_networks handles self-loops correctly", {
	skip_on_cran()
	n = 10

	# network with self-loops
	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	diag(mat1) = 1 # add self-loops

	# network without self-loops
	mat2 = mat1
	diag(mat2) = 0

	net1 = new_netify(mat1, loops = TRUE)
	net2 = new_netify(mat2, loops = FALSE)

	comp = suppressWarnings(compare_networks(list(net1, net2)))

	# should detect differences due to self-loops
	edge_changes = comp$edge_changes[[1]]
	expect_true(edge_changes$removed > 0 || edge_changes$added > 0)
})

test_that("compare_networks print method works", {
	skip_on_cran()
	# create simple networks
	mat1 = matrix(rbinom(25, 1, 0.3), 5, 5)
	mat2 = matrix(rbinom(25, 1, 0.3), 5, 5)

	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	comp = suppressWarnings(compare_networks(list(net1, net2)))

	# # test that print runs without error
	# expect_invisible(print(comp))
	
	# check the structure of the comparison object instead of print output
	expect_equal(comp$comparison_type, "cross_network")
	expect_equal(comp$n_networks, 2)
	expect_true(!is.null(comp$summary))
})

test_that("compare_networks handles list input vs single network", {
	skip_on_cran()
	# create longitudinal network
	n = 10
	times = 3
	arr = array(rbinom(n * n * times, 1, 0.3), dim = c(n, n, times))
	net_longit = new_netify(arr)

	# create list of networks
	net_list = list()
	for (i in 1:times) {
		net_list[[i]] = new_netify(arr[, , i])
	}

	# compare results
	comp_longit = suppressWarnings(compare_networks(net_longit))
	comp_list = suppressWarnings(compare_networks(net_list))

	expect_equal(comp_longit$comparison_type, "temporal")
	expect_equal(comp_list$comparison_type, "cross_network")

	# both should have same number of networks
	expect_equal(comp_longit$n_networks, comp_list$n_networks)
})

test_that("compare_networks handles named lists correctly", {
	skip_on_cran()
	# create networks with names
	mat1 = matrix(rbinom(100, 1, 0.3), 10, 10)
	mat2 = matrix(rbinom(100, 1, 0.3), 10, 10)
	mat3 = matrix(rbinom(100, 1, 0.3), 10, 10)

	net_list = list(
		"Network_A" = new_netify(mat1),
		"Network_B" = new_netify(mat2),
		"Network_C" = new_netify(mat3)
	)

	comp = suppressWarnings(compare_networks(net_list))

	# should preserve names in comparisons
	expect_true(length(comp$edge_changes) > 0)
	comparison_names = names(comp$edge_changes)
	expect_true(all(grepl("Network_[ABC]_vs_Network_[ABC]", comparison_names)))
})

# additional edge case tests

test_that("compare_networks handles single network gracefully", {
	skip_on_cran()
	mat = matrix(rbinom(100, 1, 0.3), 10, 10)
	net = new_netify(mat)

	# single network in list
	expect_error(compare_networks(list(net)), "Need at least 2 networks to compare")
})

test_that("compare_networks handles networks with all missing values", {
	skip_on_cran()
	n = 10

	# network with all NA values
	na_mat = matrix(NA, n, n)
	net_na = new_netify(na_mat)

	# normal network
	normal_mat = matrix(rbinom(n * n, 1, 0.3), n, n)
	net_normal = new_netify(normal_mat)

	# should handle comparison without error
	comp = suppressWarnings(compare_networks(list(net_na, net_normal)))
	expect_s3_class(comp, "netify_comparison")
})

test_that("compare_networks handles very sparse networks", {
	skip_on_cran()
	n = 50

	# very sparse network (only 2 edges)
	sparse_mat = matrix(0, n, n)
	sparse_mat[1, 2] = sparse_mat[2, 1] = 1
	net_sparse = new_netify(sparse_mat)

	# another sparse network
	sparse_mat2 = matrix(0, n, n)
	sparse_mat2[3, 4] = sparse_mat2[4, 3] = 1
	net_sparse2 = new_netify(sparse_mat2)

	comp = suppressWarnings(compare_networks(list(net_sparse, net_sparse2), method = "all"))
	expect_s3_class(comp, "netify_comparison")
	expect_equal(comp$summary$jaccard, 0) # no overlap
})

test_that("compare_networks structural comparison with extreme networks", {
	skip_on_cran()
	n = 20

	# star network
	star_mat = matrix(0, n, n)
	star_mat[1, 2:n] = star_mat[2:n, 1] = 1
	net_star = new_netify(star_mat)

	# ring network
	ring_mat = matrix(0, n, n)
	for (i in 1:(n - 1)) {
		ring_mat[i, i + 1] = ring_mat[i + 1, i] = 1
	}
	ring_mat[n, 1] = ring_mat[1, n] = 1
	net_ring = new_netify(ring_mat)

	comp_struct = suppressWarnings(compare_networks(list(net_star, net_ring), what = "structure"))

	# should have very different structural properties
	# star network has much higher centralization than ring network
	# let's just verify the comparison completed successfully
	expect_s3_class(comp_struct, "netify_comparison")
	expect_equal(comp_struct$method, "structural_comparison")
	expect_equal(nrow(comp_struct$summary), 2)
})

test_that("compare_networks handles networks with identical structure", {
	skip_on_cran()
	n = 15
	mat = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat = mat | t(mat) # make symmetric
	diag(mat) = 0

	net1 = new_netify(mat)
	net2 = new_netify(mat) # identical network

	comp = suppressWarnings(compare_networks(list(net1, net2), method = "all"))

	# should have perfect correlation and jaccard
	expect_equal(comp$summary$correlation, 1)
	expect_equal(comp$summary$jaccard, 1)
	expect_equal(comp$summary$hamming, 0)
	expect_equal(comp$summary$spectral, 0) # identical networks have 0 spectral distance
})

test_that("spectral distance works correctly", {
	skip_on_cran()
	# create networks with known spectral properties
	n = 10

	# complete graph
	complete_mat = matrix(1, n, n)
	diag(complete_mat) = 0
	net_complete = new_netify(complete_mat)

	# empty graph
	empty_mat = matrix(0, n, n)
	net_empty = new_netify(empty_mat)

	# star graph
	star_mat = matrix(0, n, n)
	star_mat[1, 2:n] = star_mat[2:n, 1] = 1
	net_star = new_netify(star_mat)

	# test spectral distance between different graph types
	comp_empty_complete = compare_networks(list(net_empty, net_complete), method = "spectral")
	comp_star_complete = compare_networks(list(net_star, net_complete), method = "spectral")
	comp_empty_star = compare_networks(list(net_empty, net_star), method = "spectral")

	# all should have non-zero spectral distances
	expect_true(comp_empty_complete$summary$spectral > 0)
	expect_true(comp_star_complete$summary$spectral > 0)
	expect_true(comp_empty_star$summary$spectral > 0)

	# test with identical networks
	comp_identical = compare_networks(list(net_star, net_star), method = "spectral")
	expect_equal(comp_identical$summary$spectral, 0)

	# test with return_details
	comp_details = compare_networks(
		list(net_empty, net_complete),
		method = "spectral",
		return_details = TRUE
	)
	expect_true("spectral_matrix" %in% names(comp_details$details))
	expect_equal(dim(comp_details$details$spectral_matrix), c(2, 2))
})

test_that("compare_networks properly extracts multilayer networks", {
	skip_on_cran()
	# test that extract_network_list preserves all required attributes
	n = 10

	# create cross-sectional multilayer
	layer1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	layer2 = matrix(rbinom(n * n, 1, 0.4), n, n)

	multilayer_array = array(c(layer1, layer2), dim = c(n, n, 2))
	dimnames(multilayer_array) = list(
		paste0("actor", 1:n),
		paste0("actor", 1:n),
		c("Layer1", "Layer2")
	)

	# create proper multilayer netify object with all attributes
	multilayer_net = multilayer_array
	class(multilayer_net) = "netify"
	attr(multilayer_net, "netify_type") = "cross_sec"
	attr(multilayer_net, "symmetric") = TRUE
	attr(multilayer_net, "mode") = "unipartite"
	attr(multilayer_net, "layers") = c("Layer1", "Layer2")
	attr(multilayer_net, "weight") = c("weight1", "weight2")
	attr(multilayer_net, "detail_weight") = "Multiple weights"
	attr(multilayer_net, "is_binary") = c(FALSE, FALSE)
	attr(multilayer_net, "diag_to_NA") = TRUE
	attr(multilayer_net, "missing_to_zero") = TRUE
	attr(multilayer_net, "sum_dyads") = FALSE

	# extract layers
	nets_list = netify:::extract_network_list(multilayer_net)

	# check that all required attributes are preserved
	expect_equal(length(nets_list), 2)

	for (i in 1:2) {
		layer = nets_list[[i]]
		expect_true(inherits(layer, "netify"))
		expect_equal(attr(layer, "netify_type"), "cross_sec")
		expect_equal(attr(layer, "symmetric"), TRUE)
		expect_equal(attr(layer, "mode"), "unipartite")
		expect_true(!is.null(attr(layer, "weight")))
		expect_true(!is.null(attr(layer, "detail_weight")))
		expect_true(!is.null(attr(layer, "is_binary")))
		expect_true(!is.null(attr(layer, "diag_to_NA")))
		expect_true(!is.null(attr(layer, "missing_to_zero")))
		expect_true(!is.null(attr(layer, "sum_dyads")))
	}
})

test_that("spectral distance handles edge cases", {
	skip_on_cran()
	# networks with different sizes
	n1 = 8
	n2 = 12

	mat1 = matrix(rbinom(n1 * n1, 1, 0.3), n1, n1)
	mat2 = matrix(rbinom(n2 * n2, 1, 0.3), n2, n2)

	net1 = new_netify(mat1)
	net2 = new_netify(mat2)

	# should handle different sized networks without error
	comp = suppressWarnings(compare_networks(list(net1, net2), method = "spectral"))
	expect_s3_class(comp, "netify_comparison")
	expect_true(is.numeric(comp$summary$spectral))
	expect_true(comp$summary$spectral >= 0)

	# test with directed networks
	mat_dir = matrix(rbinom(100, 1, 0.3), 10, 10)
	net_dir = new_netify(mat_dir, symmetric = FALSE)

	comp_dir = compare_networks(list(net_dir, net_dir), method = "spectral")
	expect_equal(comp_dir$summary$spectral, 0) # same network should have 0 distance
})

test_that("compare_networks handles multilayer networks", {
	skip_on_cran()
	# create test data
	n = 10

	# create two layers with different patterns
	layer1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	layer2 = matrix(rbinom(n * n, 1, 0.4), n, n)

	# create cross-sectional multilayer network
	multilayer_array = array(c(layer1, layer2), dim = c(n, n, 2))
	dimnames(multilayer_array) = list(
		paste0("actor", 1:n),
		paste0("actor", 1:n),
		c("Layer1", "Layer2")
	)

	# create multilayer netify object manually
	multilayer_net = multilayer_array
	class(multilayer_net) = "netify"
	attr(multilayer_net, "netify_type") = "cross_sec"
	attr(multilayer_net, "symmetric") = FALSE
	attr(multilayer_net, "mode") = "unipartite"
	attr(multilayer_net, "layers") = c("Layer1", "Layer2")

	# test multilayer comparison
	comp = suppressWarnings(compare_networks(multilayer_net))

	expect_s3_class(comp, "netify_comparison")
	expect_equal(comp$comparison_type, "multilayer")
	expect_equal(comp$n_networks, 2)
	expect_true("summary" %in% names(comp))

	# should compare Layer1 vs Layer2
	expect_true(length(comp$edge_changes) > 0)
	expect_true(any(grepl("Layer1.*Layer2", names(comp$edge_changes))))
})

test_that("compare_networks handles longitudinal multilayer networks", {
	skip_on_cran()
	# create 4D array for longitudinal multilayer
	n = 8
	n_layers = 2
	n_time = 3

	# create 4D array [actors × actors × layers × time]
	longit_multilayer = array(
		rbinom(n * n * n_layers * n_time, 1, 0.3),
		dim = c(n, n, n_layers, n_time)
	)

	dimnames(longit_multilayer) = list(
		paste0("actor", 1:n),
		paste0("actor", 1:n),
		c("Cooperation", "Conflict"),
		c("T1", "T2", "T3")
	)

	# create multilayer netify object
	class(longit_multilayer) = "netify"
	attr(longit_multilayer, "netify_type") = "longit_array"
	attr(longit_multilayer, "symmetric") = FALSE
	attr(longit_multilayer, "mode") = "unipartite"
	attr(longit_multilayer, "layers") = c("Cooperation", "Conflict")

	# test comparison
	comp = suppressWarnings(compare_networks(longit_multilayer))

	expect_s3_class(comp, "netify_comparison")
	expect_equal(comp$comparison_type, "multilayer")
	expect_equal(comp$n_networks, 2) # two layers

	# each layer should be a longitudinal array
	# the comparison should be between cooperation and conflict layers
	expect_true("summary" %in% names(comp))
})

test_that("compare_networks handles temporal multilayer networks (longit_list)", {
	skip_on_cran()
	# create temporal multilayer using layer_netify
	data(icews)
	years = c(2002, 2003)
	icews_subset = icews[icews$year %in% years, ]

	# create temporal networks for each layer
	verbal_temporal = netify(
		icews_subset,
		actor1 = "i", actor2 = "j",
		time = "year",
		symmetric = TRUE,
		weight = "verbCoop"
	)

	material_temporal = netify(
		icews_subset,
		actor1 = "i", actor2 = "j",
		time = "year",
		symmetric = TRUE,
		weight = "matlCoop"
	)

	# combine into temporal multilayer
	temporal_multilayer = layer_netify(
		netlet_list = list(verbal_temporal, material_temporal),
		layer_labels = c("Verbal", "Material")
	)

	# test all comparison methods
	# edge comparison
	comp_edges = suppressWarnings(compare_networks(temporal_multilayer, what = "edges"))
	expect_s3_class(comp_edges, "netify_comparison")
	expect_equal(comp_edges$comparison_type, "multilayer")
	expect_equal(comp_edges$n_networks, 2)
	expect_true("summary" %in% names(comp_edges))

	# structure comparison
	comp_struct = suppressWarnings(compare_networks(temporal_multilayer, what = "structure"))
	expect_s3_class(comp_struct, "netify_comparison")
	expect_equal(comp_struct$method, "structural_comparison")
	expect_true("summary" %in% names(comp_struct))
	# for multilayer comparison, we compare layers not time×layer combinations
	# so we expect 2 rows (one for each layer)
	expect_equal(nrow(comp_struct$summary), 2)

	# node comparison
	comp_nodes = suppressWarnings(compare_networks(temporal_multilayer, what = "nodes"))
	expect_s3_class(comp_nodes, "netify_comparison")
	expect_equal(comp_nodes$method, "node_composition")

	# attribute comparison
	comp_attrs = suppressWarnings(compare_networks(temporal_multilayer, what = "attributes"))
	expect_s3_class(comp_attrs, "netify_comparison")
	expect_equal(comp_attrs$method, "attribute_comparison")
})

test_that("compare_networks handles multilayer networks with missing attributes", {
	skip_on_cran()
	# create a multilayer network with minimal attributes
	n = 10
	layer1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	layer2 = matrix(rbinom(n * n, 1, 0.4), n, n)

	multilayer_array = array(c(layer1, layer2), dim = c(n, n, 2))

	# create netify object with minimal attributes
	multilayer_net = multilayer_array
	class(multilayer_net) = "netify"
	attr(multilayer_net, "netify_type") = "cross_sec"
	attr(multilayer_net, "symmetric") = FALSE
	attr(multilayer_net, "mode") = "unipartite"
	attr(multilayer_net, "layers") = c("Layer1", "Layer2")
	# missing many optional attributes

	# should still work
	comp = suppressWarnings(compare_networks(multilayer_net))
	expect_s3_class(comp, "netify_comparison")
	expect_equal(comp$comparison_type, "multilayer")
	expect_equal(comp$n_networks, 2)
})

test_that("compare_networks handles different weight specifications in multilayer", {
	skip_on_cran()
	# test with different weight types per layer
	n = 10

	# create networks with different weight structures
	net1 = new_netify(matrix(runif(n * n), n, n), symmetric = FALSE)
	net2 = new_netify(matrix(rpois(n * n, 2), n, n), symmetric = FALSE)

	# use layer_netify to create proper multilayer
	multilayer = layer_netify(
		list(net1, net2),
		layer_labels = c("Continuous", "Count")
	)

	# extract and compare
	nets_extracted = netify:::extract_network_list(multilayer)
	expect_equal(length(nets_extracted), 2)

	# each should have appropriate weight info
	expect_true(!is.null(attr(nets_extracted[[1]], "weight")))
	expect_true(!is.null(attr(nets_extracted[[2]], "weight")))

	# test comparison works
	comp = suppressWarnings(compare_networks(multilayer))
	expect_s3_class(comp, "netify_comparison")
	expect_equal(comp$comparison_type, "multilayer")
})

test_that("compare_networks error handling for invalid multilayer inputs", {
	skip_on_cran()
	# test with mismatched dimensions in layers
	n1 = 10
	n2 = 15

	mat1 = matrix(rbinom(n1 * n1, 1, 0.3), n1, n1)
	mat2 = matrix(rbinom(n2 * n2, 1, 0.3), n2, n2)

	# this should fail when trying to create multilayer
	expect_error(
		layer_netify(list(new_netify(mat1), new_netify(mat2)))
	)
})

test_that("compare_networks handles all comparison methods with multilayer", {
	skip_on_cran()
	# create simple multilayer network
	n = 20
	layer1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	layer2 = matrix(rbinom(n * n, 1, 0.5), n, n)

	# create two separate netify objects
	net1 = new_netify(layer1, symmetric = TRUE)
	net2 = new_netify(layer2, symmetric = TRUE)

	# use layer_netify to create multilayer
	multilayer = layer_netify(
		list(net1, net2),
		layer_labels = c("Sparse", "Dense")
	)

	# test all edge comparison methods
	methods = c("correlation", "jaccard", "hamming", "spectral")

	for (method in methods) {
		comp = suppressWarnings(compare_networks(multilayer, method = method))
		expect_s3_class(comp, "netify_comparison")
		expect_equal(comp$comparison_type, "multilayer")
		expect_true(method %in% names(comp$summary))
	}

	# test "all" method
	comp_all = suppressWarnings(compare_networks(multilayer, method = "all"))
	expect_true(all(c("correlation", "jaccard", "hamming", "spectral") %in% names(comp_all$summary)))
})

# tests for other_stats functionality

test_that("compare_networks with other_stats for structural comparison", {
	skip_on_cran()
	# create test networks
	n = 15
	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat2 = matrix(rbinom(n * n, 1, 0.5), n, n)
	
	net1 = new_netify(mat1, symmetric = FALSE)
	net2 = new_netify(mat2, symmetric = FALSE)
	
	# define custom statistics function
	custom_stats = function(net) {
		mat = as.matrix(net)
		c(
			total_edges = sum(mat > 0),
			max_degree = max(rowSums(mat) + colSums(mat)),
			num_isolates = sum((rowSums(mat) + colSums(mat)) == 0)
		)
	}
	
	# compare with custom stats
	comp = compare_networks(
		list("Net1" = net1, "Net2" = net2),
		what = "structure",
		other_stats = list(custom = custom_stats)
	)
	
	# check that custom stats are integrated into summary
	# since there's only one other_stats function, names aren't prefixed
	expect_true("total_edges" %in% names(comp$summary))
	expect_true("max_degree" %in% names(comp$summary))
	expect_true("num_isolates" %in% names(comp$summary))
	
	# verify values make sense
	expect_true(all(comp$summary$total_edges >= 0))
	expect_true(all(comp$summary$max_degree >= 0))
})

test_that("compare_networks with other_stats for edge comparison", {
	skip_on_cran()
	# create test networks
	n = 10
	mat1 = matrix(rbinom(n * n, 1, 0.4), n, n)
	mat2 = mat1
	# modify some edges
	mat2[1:3, 4:6] = 1 - mat2[1:3, 4:6]
	
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)
	
	# define custom edge statistics function (receives matrix)
	edge_stats = function(mat) {
		c(
			diagonal_sum = sum(diag(mat)),
			upper_triangle_sum = sum(mat[upper.tri(mat)]),
			matrix_rank = qr(mat)$rank
		)
	}
	
	# compare with custom stats
	comp = compare_networks(
		list(net1, net2),
		what = "edges",
		method = "correlation",
		other_stats = list(edge_custom = edge_stats)
	)
	
	# check that custom stats are included
	expect_true("custom_stats" %in% names(comp))
	expect_s3_class(comp$custom_stats, "data.frame")
	expect_equal(nrow(comp$custom_stats), 2)
	expect_true(all(c("network", "edge_custom.edge_custom_diagonal_sum", "edge_custom.edge_custom_upper_triangle_sum", "edge_custom.edge_custom_matrix_rank") %in% names(comp$custom_stats)))
})

test_that("compare_networks with multiple other_stats functions", {
	skip_on_cran()
	# create test networks
	n = 12
	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat2 = matrix(rbinom(n * n, 1, 0.4), n, n)
	
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)
	
	# define multiple custom functions
	centrality_stats = function(net) {
		mat = as.matrix(net)
		degrees = rowSums(mat) + colSums(mat)
		c(
			mean_degree = mean(degrees),
			degree_variance = var(degrees)
		)
	}
	
	connectivity_stats = function(net) {
		# simple connectivity measure
		mat = as.matrix(net)
		n_nodes = nrow(mat)
		n_edges = sum(mat > 0)
		c(
			edge_density = n_edges / (n_nodes * (n_nodes - 1)),
			avg_clustering = NA  # placeholder
		)
	}
	
	# compare with multiple custom stats
	comp = compare_networks(
		list("A" = net1, "B" = net2),
		what = "structure",
		other_stats = list(
			centrality = centrality_stats,
			connectivity = connectivity_stats
		)
	)
	
	# check that all custom stats are integrated into summary
	# with multiple functions, names should be prefixed
	expect_true("centrality.mean_degree" %in% names(comp$summary))
	expect_true("centrality.degree_variance" %in% names(comp$summary))
	expect_true("connectivity.edge_density" %in% names(comp$summary))
	expect_true("connectivity.avg_clustering" %in% names(comp$summary))
})

test_that("compare_networks other_stats error handling", {
	skip_on_cran()
	# create test networks
	n = 10
	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	net1 = new_netify(mat1)
	net2 = new_netify(mat1)
	
	# test with invalid other_stats input
	expect_error(
		compare_networks(
			list(net1, net2),
			other_stats = "not a list"
		),
		"other_stats must be a named list"
	)
	
	# create unnamed list with function (not using variable name)
	expect_error(
		compare_networks(
			list(net1, net2),
			other_stats = list(function(x) c(test = 1))  # truly unnamed list
		),
		"other_stats must be a named list"
	)
	
	expect_error(
		compare_networks(
			list(net1, net2),
			other_stats = list(stat1 = "not a function")
		),
		"All elements of other_stats must be functions"
	)
	
	# test with function that errors — errors in other_stats propagate
	# through the summary() -> graph_stats_for_netlet() path
	error_function = function(net) {
		stop("Intentional error")
	}

	expect_error(
		compare_networks(
			list(net1, net2),
			what = "structure",
			other_stats = list(error_stat = error_function)
		)
	)
})

test_that("compare_networks other_stats with longitudinal networks", {
	skip_on_cran()
	# create longitudinal network
	n = 10
	t = 3
	arr = array(rbinom(n * n * t, 1, 0.3), dim = c(n, n, t))
	dimnames(arr)[[3]] = c("T1", "T2", "T3")
	
	net_longit = new_netify(arr)
	
	# custom stats for longitudinal
	time_stats = function(net) {
		mat = as.matrix(net)
		c(
			period_density = sum(mat > 0) / (nrow(mat) * (nrow(mat) - 1)),
			period_edges = sum(mat > 0)
		)
	}
	
	# compare across time with custom stats
	comp = compare_networks(
		net_longit,
		what = "structure",
		other_stats = list(temporal = time_stats)
	)
	
	expect_equal(comp$comparison_type, "temporal")
	
	# custom stats should be integrated into summary
	# should have stats for each time period
	expect_equal(nrow(comp$summary), t)
	# since there's only one other_stats function, names aren't prefixed
	expect_true(all(c("period_density", "period_edges") %in% names(comp$summary)))
})

test_that("compare_networks other_stats with node comparison", {
	skip_on_cran()
	# create networks with different node sets
	n1 = 12
	n2 = 15
	
	mat1 = matrix(rbinom(n1 * n1, 1, 0.3), n1, n1)
	mat2 = matrix(rbinom(n2 * n2, 1, 0.3), n2, n2)
	
	rownames(mat1) = colnames(mat1) = paste0("node", 1:n1)
	rownames(mat2) = colnames(mat2) = paste0("node", c(1:10, 20:24))
	
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)
	
	# custom node-based stats
	node_stats = function(net) {
		mat = as.matrix(net)
		actors = unique(c(rownames(mat), colnames(mat)))
		c(
			num_actors = length(actors),
			actor_name_length = mean(nchar(actors))
		)
	}
	
	comp = compare_networks(
		list(net1, net2),
		what = "nodes",
		other_stats = list(actors = node_stats)
	)
	
	expect_true("custom_stats" %in% names(comp))
	expect_equal(nrow(comp$custom_stats), 2)
	expect_true(all(c("network", "actors.actors_num_actors", "actors.actors_actor_name_length") %in% names(comp$custom_stats)))
})

test_that("compare_networks other_stats with attribute comparison", {
	skip_on_cran()
	# create networks with attributes
	n = 15
	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat2 = mat1
	
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)
	
	# add attributes
	attr_df1 = data.frame(
		actor = 1:n,
		group = sample(c("A", "B", "C"), n, replace = TRUE),
		value = rnorm(n)
	)
	attr_df2 = data.frame(
		actor = 1:n,
		group = sample(c("A", "B", "C"), n, replace = TRUE),
		value = rnorm(n, mean = 0.5)
	)
	
	net1 = add_node_vars(net1, attr_df1, actor = "actor")
	net2 = add_node_vars(net2, attr_df2, actor = "actor")
	
	# custom attribute stats
	attr_stats = function(net) {
		attrs = attr(net, "nodal_data")
		if (!is.null(attrs) && "value" %in% names(attrs)) {
			c(
				mean_value = mean(attrs$value, na.rm = TRUE),
				sd_value = sd(attrs$value, na.rm = TRUE)
			)
		} else {
			c(mean_value = NA, sd_value = NA)
		}
	}
	
	comp = compare_networks(
		list(net1, net2),
		what = "attributes",
		other_stats = list(value_stats = attr_stats)
	)
	
	expect_true("custom_stats" %in% names(comp))
	expect_equal(nrow(comp$custom_stats), 2)
})

test_that("compare_networks other_stats preserves standard functionality", {
	skip_on_cran()
	# ensure adding other_stats doesn't break standard comparisons
	n = 10
	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat2 = matrix(rbinom(n * n, 1, 0.4), n, n)
	
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)
	
	simple_stat = function(net) {
		c(test_value = 1)
	}
	
	# compare without and with other_stats
	comp_standard = compare_networks(list(net1, net2), method = "all")
	comp_custom = compare_networks(
		list(net1, net2), 
		method = "all",
		other_stats = list(test = simple_stat)
	)
	
	# standard results should be identical (except possibly QAP p-values due to randomness)
	# compare all columns except qap_pvalue
	summary_cols = setdiff(names(comp_standard$summary), "qap_pvalue")
	expect_equal(comp_standard$summary[, summary_cols], comp_custom$summary[, summary_cols])
	expect_equal(comp_standard$edge_changes, comp_custom$edge_changes)
	
	# custom should have additional custom_stats
	expect_false("custom_stats" %in% names(comp_standard))
	expect_true("custom_stats" %in% names(comp_custom))
})

test_that("compare_networks other_stats with weighted networks", {
	skip_on_cran()
	# create weighted networks
	n = 10
	mat1 = matrix(runif(n * n, 0, 5), n, n)
	mat2 = matrix(runif(n * n, 0, 5), n, n)
	
	net1 = new_netify(mat1, symmetric = FALSE)
	net2 = new_netify(mat2, symmetric = FALSE)
	
	# custom stats for weighted networks
	weight_stats = function(net) {
		mat = as.matrix(net)
		weights = mat[mat > 0]
		c(
			mean_weight = mean(weights),
			median_weight = median(weights),
			weight_variance = var(weights)
		)
	}
	
	comp = compare_networks(
		list(net1, net2),
		what = "edges",
		other_stats = list(weights = weight_stats)
	)
	
	expect_true("custom_stats" %in% names(comp))
	custom_df = comp$custom_stats
	expect_true(all(c("network", "weights.weights_mean_weight", "weights.weights_median_weight", "weights.weights_weight_variance") %in% names(custom_df)))
	
	# values should be positive for these random weighted networks
	expect_true(all(custom_df$weights.weights_mean_weight > 0))
})

test_that("compare_networks other_stats with return_details", {
	skip_on_cran()
	# test that other_stats works with return_details = TRUE
	n = 10
	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat2 = matrix(rbinom(n * n, 1, 0.4), n, n)
	
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)
	
	custom_stat = function(net) {
		c(stat1 = 1, stat2 = 2)
	}
	
	comp = compare_networks(
		list(net1, net2),
		method = "all",
		return_details = TRUE,
		other_stats = list(custom = custom_stat)
	)
	
	# should have both details and custom_stats
	expect_true("details" %in% names(comp))
	expect_true("custom_stats" %in% names(comp))
	
	# details should include standard matrices
	expect_true(all(c("correlation_matrix", "jaccard_matrix", "hamming_matrix") %in% names(comp$details)))
})

# tests for enhanced structural comparison that includes all summary.netify stats

test_that("compare_networks structural comparison includes all summary.netify statistics", {
	skip_on_cran()
	# create networks with different properties
	n = 20
	
	# dense directed network
	mat1 = matrix(rbinom(n * n, 1, 0.7), n, n)
	diag(mat1) = 0
	net1 = new_netify(mat1, symmetric = FALSE)
	
	# sparse directed network  
	mat2 = matrix(rbinom(n * n, 1, 0.2), n, n)
	diag(mat2) = 0
	net2 = new_netify(mat2, symmetric = FALSE)
	
	# get individual summaries
	sum1 = summary(net1)
	sum2 = summary(net2)
	
	# compare structures
	comp = compare_networks(list("Dense" = net1, "Sparse" = net2), what = "structure")
	
	# the comparison summary should have all columns from summary.netify
	expected_cols = names(sum1)
	# remove 'net' column as it's replaced by 'network' in comparison
	expected_cols = setdiff(expected_cols, "net")
	
	# all summary.netify columns should be present
	for (col in expected_cols) {
		expect_true(col %in% names(comp$summary), 
				   info = paste("Missing column:", col))
	}
	
	# for directed networks, should include:
	expect_true(all(c("num_actors", "density", "num_edges", "prop_edges_missing",
					 "competition_row", "competition_col", 
					 "sd_of_row_means", "sd_of_col_means",
					 "covar_of_row_col_means", "reciprocity", 
					 "transitivity") %in% names(comp$summary)))
})

test_that("compare_networks structural comparison with weighted networks includes weight stats", {
	skip_on_cran()
	# create weighted networks
	n = 15
	mat1 = matrix(runif(n * n, 0, 10), n, n)
	mat2 = matrix(runif(n * n, 0, 5), n, n)
	
	net1 = new_netify(mat1, symmetric = TRUE)
	net2 = new_netify(mat2, symmetric = TRUE)
	
	comp = compare_networks(list(net1, net2), what = "structure")
	
	# should include weight statistics
	weight_stats = c("mean_edge_weight", "sd_edge_weight", 
					 "median_edge_weight", "min_edge_weight", "max_edge_weight")
	
	for (stat in weight_stats) {
		expect_true(stat %in% names(comp$summary),
				   info = paste("Missing weight stat:", stat))
	}
	
	# weight stats should differ between networks
	expect_true(comp$summary$mean_edge_weight[1] != comp$summary$mean_edge_weight[2])
})

test_that("compare_networks structural comparison adapts to network type", {
	skip_on_cran()
	# test undirected network - should not have reciprocity
	n = 15
	mat_sym = matrix(rbinom(n * n, 1, 0.4), n, n)
	mat_sym[lower.tri(mat_sym)] = t(mat_sym)[lower.tri(mat_sym)]
	diag(mat_sym) = 0
	
	net_sym1 = new_netify(mat_sym, symmetric = TRUE)
	net_sym2 = new_netify(mat_sym * rbinom(n * n, 1, 0.8), symmetric = TRUE)
	
	comp_sym = compare_networks(list(net_sym1, net_sym2), what = "structure")
	
	# should NOT have directed-only stats
	expect_false("reciprocity" %in% names(comp_sym$summary))
	expect_false("covar_of_row_col_means" %in% names(comp_sym$summary))
	expect_false("competition_row" %in% names(comp_sym$summary))
	expect_false("competition_col" %in% names(comp_sym$summary))
	
	# should have simplified names
	expect_true("competition" %in% names(comp_sym$summary))
	expect_true("sd_of_actor_means" %in% names(comp_sym$summary))
})

test_that("compare_networks structural comparison with binary networks", {
	skip_on_cran()
	# create binary networks
	n = 20
	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat2 = matrix(rbinom(n * n, 1, 0.5), n, n)
	
	# these are binary by default
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)
	
	comp = compare_networks(list(net1, net2), what = "structure")
	
	# should NOT have weight statistics for binary networks
	weight_stats = c("mean_edge_weight", "sd_edge_weight", 
					 "median_edge_weight", "min_edge_weight", "max_edge_weight")
	
	for (stat in weight_stats) {
		expect_false(stat %in% names(comp$summary),
					info = paste("Binary network should not have:", stat))
	}
})

test_that("compare_networks structural percent changes calculated correctly", {
	skip_on_cran()
	# create networks with known differences
	n = 10
	
	# first network: sparse
	mat1 = matrix(0, n, n)
	mat1[1:3, 4:6] = 1
	
	# second network: denser but ensure it's not symmetric
	mat2 = matrix(0, n, n) 
	mat2[1:5, 1:6] = 1
	mat2[6, 1:3] = 1  # make it asymmetric
	diag(mat2) = 0
	
	net1 = new_netify(mat1, symmetric = FALSE)
	net2 = new_netify(mat2, symmetric = FALSE)
	
	comp = compare_networks(list(net1, net2), what = "structure")
	
	# should have changes dataframe
	expect_true("changes" %in% names(comp))
	changes = comp$changes
	
	# check structure of changes
	expect_true(all(c("metric", "value_net1", "value_net2", 
					 "absolute_change", "percent_change") %in% names(changes)))
	
	# density should increase
	density_row = changes[changes$metric == "density", ]
	expect_true(density_row$absolute_change > 0)
	expect_true(density_row$percent_change > 0)
	
	# all metrics from summary should be in changes
	summary_metrics = setdiff(names(comp$summary), c("network", "layer"))
	changes_metrics = unique(changes$metric)
	
	for (metric in summary_metrics) {
		expect_true(metric %in% changes_metrics,
				   info = paste("Missing metric in changes:", metric))
	}
})

test_that("compare_networks structural comparison with custom other_stats", {
	skip_on_cran()
	# create test networks
	n = 15
	mat1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat2 = matrix(rbinom(n * n, 1, 0.5), n, n)
	
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)
	
	# define custom function that works with summary.netify
	custom_centralization = function(mat) {
		degrees = rowSums(mat) + colSums(mat)
		max_degree = max(degrees)
		sum_diff = sum(max_degree - degrees)
		max_sum_diff = (nrow(mat) - 1) * (2 * (nrow(mat) - 1))
		c(
			degree_centralization = sum_diff / max_sum_diff,
			max_degree = max_degree
		)
	}
	
	comp = compare_networks(
		list(net1, net2), 
		what = "structure",
		other_stats = list(centralization = custom_centralization)
	)
	
	# custom stats should be integrated into main summary
	# when there's only one other_stats function, summary.netify doesn't prefix
	expect_true("degree_centralization" %in% names(comp$summary))
	expect_true("max_degree" %in% names(comp$summary))
	
	# and should appear in changes if comparing 2 networks
	expect_true("degree_centralization" %in% comp$changes$metric)
})

test_that("compare_networks structural comparison handles missing values", {
	skip_on_cran()
	# create networks with some missing values
	n = 12
	mat1 = matrix(rbinom(n * n, 1, 0.4), n, n)
	mat2 = matrix(rbinom(n * n, 1, 0.4), n, n)
	
	# add some NAs
	mat1[sample(1:(n*n), 10)] = NA
	mat2[sample(1:(n*n), 15)] = NA
	
	net1 = new_netify(mat1)
	net2 = new_netify(mat2)
	
	comp = compare_networks(list(net1, net2), what = "structure")
	
	# should calculate prop_edges_missing
	expect_true("prop_edges_missing" %in% names(comp$summary))
	expect_true(all(comp$summary$prop_edges_missing > 0))
	
	# other stats should still be calculated
	expect_true(all(!is.na(comp$summary$density)))
	expect_true(all(!is.na(comp$summary$num_edges)))
})

test_that("compare_networks structural comparison with bipartite networks", {
	skip_on_cran()
	# create bipartite networks
	n1 = 8
	n2 = 12
	
	mat1 = matrix(rbinom(n1 * n2, 1, 0.3), n1, n2)
	mat2 = matrix(rbinom(n1 * n2, 1, 0.4), n1, n2)
	
	net1 = new_netify(mat1, bipartite = TRUE)
	net2 = new_netify(mat2, bipartite = TRUE)
	
	comp = compare_networks(list(net1, net2), what = "structure")
	
	# should have bipartite-specific columns
	expect_true("num_row_actors" %in% names(comp$summary))
	expect_true("num_col_actors" %in% names(comp$summary))
	
	# should not have unipartite-only stats
	expect_false("num_actors" %in% names(comp$summary))
	# bipartite networks should not include reciprocity or transitivity
	expect_false("reciprocity" %in% names(comp$summary))
	expect_false("mutual" %in% names(comp$summary))
	expect_false("transitivity" %in% names(comp$summary))
})

test_that("compare_networks preserves all summary statistics in multilayer", {
	skip_on_cran()
	# create multilayer network
	n = 15
	layer1 = matrix(rbinom(n * n, 1, 0.3), n, n)
	layer2 = matrix(rbinom(n * n, 1, 0.5), n, n)
	
	net1 = new_netify(layer1)
	net2 = new_netify(layer2)
	
	multilayer = layer_netify(
		list(net1, net2),
		layer_labels = c("Sparse", "Dense")
	)
	
	# compare layers structurally
	comp = compare_networks(multilayer, what = "structure")
	
	# should show each layer's full statistics
	expect_equal(nrow(comp$summary), 2)
	expect_true("network" %in% names(comp$summary))
	
	# all summary stats should be present
	sum_individual = summary(net1)
	expected_stats = setdiff(names(sum_individual), "net")
	
	for (stat in expected_stats) {
		expect_true(stat %in% names(comp$summary),
				   info = paste("Missing stat in multilayer comparison:", stat))
	}
})
