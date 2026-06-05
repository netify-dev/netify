set.seed(6886)

make_inference_net = function() {
	edges = data.frame(
		from = c("a", "a", "b", "c", "d"),
		to = c("b", "c", "c", "d", "a"),
		weight = c(1, 2, 1, 3, 1),
		stringsAsFactors = FALSE
	)
	net = netify(
		edges,
		actor1 = "from",
		actor2 = "to",
		weight = "weight",
		symmetric = FALSE,
		nodelist = c("a", "b", "c", "d")
	)
	nodes = data.frame(
		actor = c("a", "b", "c", "d"),
		group = c("x", "x", "y", "y"),
		score = c(1, 2, 3, 4),
		stringsAsFactors = FALSE
	)
	add_node_vars(net, nodes, actor = "actor")
}

test_that("inference helpers fail when no valid draws remain", {
	net = make_inference_net()

	expect_error(
		bootstrap_netlet(
			net,
			fn = function(x) c(bad = NA_real_),
			n_boot = 2,
			verbose = FALSE
		),
		"finite numeric values"
	)
})

test_that("inference helpers validate counts and align named statistics", {
	net = make_inference_net()
	attr(net, "observed_tag") = TRUE

	stat_fun = function(x) {
		s = suppressMessages(summary(x))
		if (isTRUE(attr(x, "observed_tag"))) {
			c(edges = s$num_edges[1], density = s$density[1])
		} else {
			c(density = s$density[1], edges = s$num_edges[1])
		}
	}

	expect_error(simulate(net, nsim = 1.5), "positive integer")
	expect_error(compare_to_null(net, stat_fun, n_sim = 2.5, verbose = FALSE), "integer")
	expect_error(bootstrap_netlet(net, stat_fun, n_boot = 2.5, verbose = FALSE), "integer")

	null_test = compare_to_null(net, stat_fun, n_sim = 2, seed = 1, verbose = FALSE)
	expect_equal(null_test$metric, c("edges", "density"))
	expect_equal(colnames(attr(null_test, "null_draws")), c("edges", "density"))

	boot = bootstrap_netlet(net, stat_fun, n_boot = 2, seed = 1, verbose = FALSE)
	expect_equal(boot$metric, c("edges", "density"))
	expect_equal(colnames(attr(boot, "bootstrap_draws")), c("edges", "density"))
})

test_that("simulate preserves unknown-edge masks", {
	mat = matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
	diag(mat) = NA
	mat["a", "b"] = 1
	mat["b", "c"] = NA
	net = new_netify(
		mat,
		symmetric = FALSE,
		diag_to_NA = TRUE,
		missing_to_zero = FALSE
	)

	sims = simulate(net, nsim = 1, seed = 1)
	sim_mat = get_raw(sims[[1]])
	expect_true(is.na(sim_mat["b", "c"]))
	expect_true(all(is.na(diag(sim_mat))))
})

test_that("bootstrap_netlet rejects bipartite inputs", {
	mat = matrix(c(1, 0, 0, 1, 1, 0), 2, 3)
	net = new_netify(mat, mode = "bipartite")

	expect_error(
		bootstrap_netlet(net, function(x) c(edges = sum(get_raw(x))), n_boot = 2),
		"square unipartite"
	)
})
