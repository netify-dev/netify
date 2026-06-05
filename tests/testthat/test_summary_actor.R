set.seed(6886)

test_that("summary_actor works for cross-sectional networks", {
	# create a simple cross-sectional network
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net = netify(
		icews_10,
		actor1 = "i", actor2 = "j",
		symmetric = FALSE,
		weight = "verbCoop"
	)

	actor_stats = summary_actor(net)

	expect_s3_class(actor_stats, "data.frame")
	expect_true("actor" %in% names(actor_stats))
	expect_false("time" %in% names(actor_stats)) # no time for cross-sectional
	expect_false("layer" %in% names(actor_stats)) # no layer column for single layer

	# check for directed network columns
	expect_true(all(c("degree_in", "degree_out", "degree_total") %in% names(actor_stats)))
	expect_true(all(c("closeness_in", "closeness_out", "closeness_all") %in% names(actor_stats)))
	expect_true("betweenness" %in% names(actor_stats))
	expect_true("authority_score" %in% names(actor_stats))
	expect_true("hub_score" %in% names(actor_stats))

	# check that each actor appears once
	expect_equal(nrow(actor_stats), length(unique(actor_stats$actor)))
})

test_that("summary_actor works for longitudinal networks", {
	skip_on_cran()
	# create longitudinal network
	data(icews)
	net_longit = netify(
		icews,
		actor1 = "i", actor2 = "j", time = "year",
		symmetric = FALSE,
		weight = "matlConf"
	)

	actor_stats = summary_actor(net_longit)

	expect_s3_class(actor_stats, "data.frame")
	expect_true("actor" %in% names(actor_stats))
	expect_true("time" %in% names(actor_stats))

	# check that we have entries for each actor-time combination
	n_times = length(unique(icews$year))
	expect_true(nrow(actor_stats) > n_times) # multiple actors per time

	# verify time periods are correctly extracted
	expect_true(all(unique(actor_stats$time) %in% as.character(unique(icews$year))))
})

test_that("summary_actor works for symmetric networks", {
	# create symmetric network
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net_sym = netify(
		icews_10,
		actor1 = "i", actor2 = "j",
		symmetric = TRUE,
		weight = "verbCoop"
	)

	actor_stats = summary_actor(net_sym)

	# check that undirected-only columns are present
	expect_true("degree" %in% names(actor_stats))
	expect_true("prop_ties" %in% names(actor_stats))
	expect_true("network_share" %in% names(actor_stats))
	expect_true("closeness" %in% names(actor_stats))
	expect_true("betweenness" %in% names(actor_stats))
	expect_true("eigen_vector" %in% names(actor_stats))

	# check that directed-only columns are absent
	expect_false("degree_in" %in% names(actor_stats))
	expect_false("degree_out" %in% names(actor_stats))
	expect_false("authority_score" %in% names(actor_stats))
	expect_false("hub_score" %in% names(actor_stats))
})

test_that("summary_actor works for binary networks", {
	# create binary network
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net_bin = netify(
		icews_10,
		actor1 = "i", actor2 = "j",
		symmetric = FALSE
	)

	actor_stats = summary_actor(net_bin)

	# check that binary network has degree columns
	expect_true(all(c("degree_in", "degree_out", "degree_total") %in% names(actor_stats)))

	# check that weighted statistics are absent
	expect_false("strength_sum_in" %in% names(actor_stats))
	expect_false("strength_sum_out" %in% names(actor_stats))
	expect_false("strength_avg_in" %in% names(actor_stats))
	expect_false("strength_std_in" %in% names(actor_stats))
})

test_that("summary_actor works for weighted networks", {
	# create weighted network
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net_weighted = netify(
		icews_10,
		actor1 = "i", actor2 = "j",
		symmetric = FALSE,
		weight = "verbCoop"
	)

	actor_stats = summary_actor(net_weighted)

	# check that weighted statistics are present
	expect_true(all(c("strength_sum_in", "strength_sum_out", "strength_sum_total") %in% names(actor_stats)))
	expect_true(all(c("strength_avg_in", "strength_avg_out", "strength_avg_total") %in% names(actor_stats)))
	expect_true(all(c("strength_std_in", "strength_std_out", "strength_std_total") %in% names(actor_stats)))
	expect_true(all(c("strength_median_in", "strength_median_out", "strength_median_total") %in% names(actor_stats)))
})

test_that("summary_actor uses possible ties and realized ties in weighted summaries", {
	mat = matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
	diag(mat) = NA
	mat["a", "b"] = 2
	mat["b", "c"] = 4

	net = new_netify(
		mat,
		symmetric = FALSE,
		weight = "w",
		is_binary = FALSE,
		diag_to_NA = TRUE,
		missing_to_zero = TRUE
	)
	actor_stats = summary_actor(net, stats = "fast")
	a_row = actor_stats[actor_stats$actor == "a", ]
	c_row = actor_stats[actor_stats$actor == "c", ]

	expect_equal(a_row$degree_out, 1)
	expect_equal(a_row$prop_ties_out, 1 / 2)
	expect_equal(a_row$strength_avg_out, 2)
	expect_equal(a_row$strength_avg_total, 2)
	expect_true(is.na(c_row$strength_avg_out))
})

test_that("summary_actor preserves dotted actor and time labels", {
	mat = matrix(c(0, 1, 0, 0), 2, 2,
		dimnames = list(c("a.1", "b.2"), c("a.1", "b.2")))
	net = new_netify(list("wave.1" = mat), symmetric = FALSE)
	actor_stats = summary_actor(net, stats = "fast")

	expect_setequal(actor_stats$actor, c("a.1", "b.2"))
	expect_equal(unique(actor_stats$time), "wave.1")
})

test_that("summary_actor works for multilayer networks", {
	skip_on_cran()
	# create multilayer network
	data(icews)
	icews_10 = icews[icews$year == 2010, ]

	net1 = netify(icews_10, actor1 = "i", actor2 = "j", weight = "verbCoop")
	net2 = netify(icews_10, actor1 = "i", actor2 = "j", weight = "matlCoop")

	multi_net = layer_netify(
		list(net1, net2),
		layer_labels = c("verbal", "material")
	)

	actor_stats = summary_actor(multi_net)

	expect_true("layer" %in% names(actor_stats))
	expect_equal(sort(unique(actor_stats$layer)), c("material", "verbal"))

	n_actors = length(unique(actor_stats$actor))
	expect_equal(nrow(actor_stats), n_actors * 2) # 2 layers
})

test_that("summary_actor handles custom statistics correctly", {
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net = netify(
		icews_10,
		actor1 = "i", actor2 = "j",
		weight = "verbCoop"
	)

	# test single custom function
	max_out = function(mat) {
		apply(mat, 1, max, na.rm = TRUE)
	}

	actor_stats1 = summary_actor(net, other_stats = list(max_out = max_out))
	expect_true("max_out" %in% names(actor_stats1))
	expect_equal(length(actor_stats1$max_out), nrow(actor_stats1))

	# test multiple custom functions
	max_in = function(mat) {
		apply(mat, 2, max, na.rm = TRUE)
	}

	actor_stats2 = summary_actor(net, other_stats = list(
		max_out = max_out,
		max_in = max_in
	))
	expect_true(all(c("max_out", "max_in") %in% names(actor_stats2)))
})

test_that("summary_actor handles bipartite networks", {
	# create bipartite network data
	bip_data = data.frame(
		actor1 = rep(letters[1:5], each = 3),
		actor2 = rep(LETTERS[1:3], 5),
		weight = runif(15, 0, 10)
	)

	net_bip = netify(
		bip_data,
		actor1 = "actor1", actor2 = "actor2",
		mode = "bipartite",
		weight = "weight"
	)

	actor_stats = summary_actor(net_bip)

	# check that all actors from both modes are included
	all_actors = c(letters[1:5], LETTERS[1:3])
	expect_true(all(all_actors %in% actor_stats$actor))
	expect_equal(nrow(actor_stats), 8) # 5 + 3 actors

	expect_true("degree_total" %in% names(actor_stats))
})

test_that("summary_actor handles ego networks", {
	skip_on_cran()
	# first create a netify object
	data(icews)
	net = netify(
		icews,
		actor1 = "i", actor2 = "j", time = "year",
		weight = "verbCoop"
	)

	# then create ego network
	ego_net = ego_netify(
		net,
		ego = "United States"
	)

	actor_stats = summary_actor(ego_net)

	# check that it returns something :)
	expect_true(nrow(actor_stats) > 0)
})

test_that("summary_actor produces consistent output structure", {
	skip_on_cran()
	# create different types of networks
	data(icews)
	icews_10 = icews[icews$year == 2010, ]

	# test directed network
	net_directed = netify(icews_10, "i", "j", symmetric = FALSE, weight = "verbCoop")
	stats_directed = summary_actor(net_directed)

	expect_s3_class(stats_directed, "data.frame")
	expect_true("actor" %in% names(stats_directed))
	expect_true(nrow(stats_directed) > 0)

	# test undirected network
	net_undirected = netify(icews_10, "i", "j", symmetric = TRUE, weight = "verbCoop")
	stats_undirected = summary_actor(net_undirected)

	expect_s3_class(stats_undirected, "data.frame")
	expect_true("actor" %in% names(stats_undirected))
	expect_equal(nrow(stats_undirected), nrow(stats_directed)) # same actors

	# test binary network
	net_binary = netify(icews_10, "i", "j", symmetric = FALSE)
	stats_binary = summary_actor(net_binary)

	expect_s3_class(stats_binary, "data.frame")
	expect_true("actor" %in% names(stats_binary))
	expect_equal(nrow(stats_binary), nrow(stats_directed)) # same actors
})

test_that("summary_actor weight inversion produces different centrality values", {
	# create a network where shortest paths differ based on weight interpretation
	# path 1: a -> b -> e (weights: 1 + 1 = 2)
	# path 2: a -> c -> d -> e (weights: 10 + 10 + 10 = 30)
	# when weights are distances: path 1 is shorter (2 < 30)
	# when weights are inverted (strengths): path 2 is "shorter" (1/10 + 1/10 + 1/10 = 0.3 < 1/1 + 1/1 = 2)

	test_data = data.frame(
		actor1 = c("A", "A", "B", "C", "D"),
		actor2 = c("B", "C", "E", "D", "E"),
		weight = c(1, 10, 1, 10, 10)
	)

	net = netify(
		test_data,
		actor1 = "actor1",
		actor2 = "actor2",
		weight = "weight",
		symmetric = FALSE
	)

	# get stats with inversion (weights as strengths)
	stats_inverted = summary_actor(net, invert_weights_for_igraph = TRUE)

	# get stats without inversion (weights as distances)
	stats_not_inverted = summary_actor(net, invert_weights_for_igraph = FALSE)

	# extract values for node b (which is on the short direct path)
	b_inverted = stats_inverted[stats_inverted$actor == "B", ]
	b_not_inverted = stats_not_inverted[stats_not_inverted$actor == "B", ]

	# extract values for node c (which is on the long indirect path)
	c_inverted = stats_inverted[stats_inverted$actor == "C", ]
	c_not_inverted = stats_not_inverted[stats_not_inverted$actor == "C", ]

	expect_true(
		b_not_inverted$betweenness != b_inverted$betweenness ||
			c_not_inverted$betweenness != c_inverted$betweenness,
		info = "Betweenness centrality should differ with weight inversion"
	)
})

test_that("summary_actor weight inversion affects closeness centrality", {
	# create a star network with different weight patterns
	# center node a connects to all others
	# weights vary significantly
	test_data = data.frame(
		actor1 = c("A", "A", "A", "A"),
		actor2 = c("B", "C", "D", "E"),
		weight = c(1, 2, 10, 20) # very different weights
	)

	net = netify(
		test_data,
		actor1 = "actor1",
		actor2 = "actor2",
		weight = "weight",
		symmetric = TRUE # use symmetric to simplify
	)

	# get stats with and without inversion
	stats_inv = summary_actor(net, invert_weights_for_igraph = TRUE)
	stats_no_inv = summary_actor(net, invert_weights_for_igraph = FALSE)

	# look at closeness for the center node a
	a_inv = stats_inv[stats_inv$actor == "A", ]
	a_no_inv = stats_no_inv[stats_no_inv$actor == "A", ]

	expect_false(
		isTRUE(all.equal(a_inv$closeness, a_no_inv$closeness)),
		info = "Closeness centrality should differ for central node with weight inversion"
	)

	# also check peripheral nodes
	b_inv = stats_inv[stats_inv$actor == "B", ]
	b_no_inv = stats_no_inv[stats_no_inv$actor == "B", ]

	expect_false(
		isTRUE(all.equal(b_inv$closeness, b_no_inv$closeness)),
		info = "Closeness centrality should differ for peripheral nodes with weight inversion"
	)
})
