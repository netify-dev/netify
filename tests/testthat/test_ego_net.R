set.seed(6886)


test_that("ego_netify works for single ego in cross-sectional network", {
	# create simple test data
	test_data = data.frame(
		from = c("A", "A", "B", "B", "C", "D"),
		to = c("B", "C", "A", "C", "A", "B"),
		weight = c(10, 20, 30, 40, 50, 60),
		stringsAsFactors = FALSE
	)

	# create netify object
	net = netify(
		test_data,
		actor1 = "from",
		actor2 = "to",
		weight = "weight",
		symmetric = FALSE
	)

	# extract ego network for actor a
	ego_a = ego_netify(net, ego = "A")

	# check that it's a netify object
	expect_s3_class(ego_a, "netify")

	# check ego network attributes
	expect_equal(attr(ego_a, "ego_id"), "A")

	# check actors in ego network (a and its neighbors: b, c)
	actors = rownames(ego_a)
	expect_equal(sort(actors), c("A", "B", "C"))

	# check that d is not included (not connected to a)
	expect_false("D" %in% actors)
})

test_that("ego_netify works with multiple egos and direction options", {
	test_data = data.frame(
		from = c("A", "A", "B", "C", "D", "D"),
		to = c("B", "C", "A", "A", "A", "B"),
		weight = c(1, 1, 1, 1, 1, 1),
		stringsAsFactors = FALSE
	)

	# create netify object
	net = netify(
		test_data,
		actor1 = "from",
		actor2 = "to",
		symmetric = FALSE
	)

	# test outgoing direction for a
	ego_out = ego_netify(net, ego = "A", ngbd_direction = "out")
	actors_out = rownames(ego_out)
	# a has outgoing ties to b and c
	expect_equal(sort(actors_out), c("A", "B", "C"))

	# test incoming direction for a
	ego_in = ego_netify(net, ego = "A", ngbd_direction = "in")
	actors_in = rownames(ego_in)
	# a has incoming ties from b, c, and d
	expect_equal(sort(actors_in), c("A", "B", "C", "D"))

	# test any direction (default)
	ego_any = ego_netify(net, ego = "A", ngbd_direction = "any")
	actors_any = rownames(ego_any)
	# a has any ties with b, c, and d
	expect_equal(sort(actors_any), c("A", "B", "C", "D"))
})

test_that("ego_netify works for longitudinal networks with thresholds", {
	# create longitudinal test data
	test_data = data.frame(
		from = rep(c("A", "A", "B", "C", "D"), 2),
		to = rep(c("B", "C", "A", "A", "B"), 2),
		time = c(rep(2010, 5), rep(2011, 5)),
		weight = c(10, 5, 20, 15, 25, 30, 2, 40, 35, 45),
		stringsAsFactors = FALSE
	)

	# create longitudinal netify object
	net_longit = netify(
		test_data,
		actor1 = "from",
		actor2 = "to",
		time = "time",
		weight = "weight",
		symmetric = FALSE
	)

	# extract ego network without threshold
	ego_longit = ego_netify(net_longit, ego = "A")

	# check output is a list with correct names
	expect_type(ego_longit, "list")
	expect_equal(names(ego_longit), c("2010", "2011"))

	# check each time period is a netify object
	expect_s3_class(ego_longit[["2010"]], "netify")
	expect_s3_class(ego_longit[["2011"]], "netify")

	ego_thresh = ego_netify(net_longit, ego = "A", threshold = 15)

	# in 2010: a-b (10) < 15, a-c (5) < 15, b-a (20) > 15, c-a (15) = 15
	actors_2010 = rownames(ego_thresh[["2010"]])
	expect_equal(sort(actors_2010), c("A", "B"))

	# in 2011: a-b (30) > 15, a-c (2) < 15, b-a (40) > 15, c-a (35) > 15
	actors_2011 = rownames(ego_thresh[["2011"]])
	expect_equal(sort(actors_2011), c("A", "B", "C"))

	# test exclude ego option
	ego_no_self = ego_netify(net_longit, ego = "A", include_ego = FALSE)
	actors_no_self = rownames(ego_no_self[["2010"]])
	expect_false("A" %in% actors_no_self)
	expect_true(all(c("B", "C") %in% actors_no_self))
})

test_that("ego_netify subsets attributes to the ego network", {
	df = data.frame(
		a = c("A", "A", "B", "C", "B", "C", "D"),
		b = c("B", "C", "C", "D", "D", "D", "E"),
		t = c(1, 1, 1, 2, 2, 3, 3),
		x = 1:7
	)
	net = suppressMessages(netify(
		df,
		actor1 = "a",
		actor2 = "b",
		time = "t",
		actor_time_uniform = FALSE,
		dyad_vars = "x"
	))
	nodes = data.frame(
		actor = rep(c("A", "B", "C", "D", "E"), 3),
		t = rep(1:3, each = 5),
		z = 1:15
	)
	net = suppressMessages(add_node_vars(net, nodes, actor = "actor", time = "t"))

	ego_c = suppressMessages(ego_netify(net, ego = "C"))
	expect_equal(names(ego_c), c("1", "2", "3"))

	# actor periods cover only the ego network actors and periods
	pds = attr(ego_c, "actor_pds")
	expect_equal(pds$actor, c("A", "B", "C", "D"))
	expect_equal(pds$min_time, c(1, 1, 1, 2))
	expect_equal(pds$max_time, c(1, 1, 3, 3))
	expect_false(attr(ego_c, "actor_time_uniform"))

	# nodal rows match the actors in each period
	nodal = attr(ego_c, "nodal_data")
	for (pd in names(ego_c)) {
		expect_setequal(
			nodal$actor[nodal$t == pd],
			rownames(ego_c[[pd]])
		)
	}

	# dyad matrices match each ego network
	dyad = attr(ego_c, "dyad_data")
	expect_equal(names(dyad), names(ego_c))
	for (pd in names(ego_c)) {
		expect_equal(rownames(dyad[[pd]]$x), rownames(ego_c[[pd]]))
	}

	# downstream functions work on the ego network
	expect_no_error(decompose_netify(ego_c))
	expect_no_error(plot(ego_c))
	expect_no_error(suppressWarnings(dyad_correlation(ego_c)))
})

test_that("ego_netify keeps period names when one period remains", {
	df = data.frame(
		a = c("A", "A", "B", "C", "B", "C", "D"),
		b = c("B", "C", "C", "D", "D", "D", "E"),
		t = c(1, 1, 1, 2, 2, 3, 3)
	)
	net = suppressMessages(netify(
		df,
		actor1 = "a",
		actor2 = "b",
		time = "t",
		actor_time_uniform = FALSE
	))

	ego_a = suppressMessages(ego_netify(net, ego = "A"))
	expect_equal(attr(ego_a, "netify_type"), "longit_list")
	expect_equal(names(ego_a), "1")
	expect_equal(attr(ego_a, "actor_pds")$actor, c("A", "B", "C"))
	expect_no_error(plot(ego_a))
	expect_no_error(decompose_netify(ego_a))
	expect_no_error(suppressMessages(add_node_vars(
		ego_a,
		data.frame(actor = c("A", "B", "C"), t = 1, z = 1:3),
		actor = "actor",
		time = "t"
	)))
})

test_that("ego_netify subsets cross-sectional attributes", {
	df = data.frame(
		a = c("A", "A", "B", "D"),
		b = c("B", "C", "C", "E"),
		x = 1:4
	)
	net = suppressMessages(netify(
		df,
		actor1 = "a",
		actor2 = "b",
		dyad_vars = "x",
		nodal_data = data.frame(actor = c("A", "B", "C", "D", "E"), z = 1:5)
	))

	ego_a = ego_netify(net, ego = "A")
	expect_equal(attr(ego_a, "actor_pds")$actor, c("A", "B", "C"))
	expect_equal(attr(ego_a, "nodal_data")$actor, c("A", "B", "C"))
	expect_equal(names(attr(ego_a, "dyad_data")), "1")
	expect_equal(dim(attr(ego_a, "dyad_data")[["1"]]$x), c(3, 3))
	expect_no_error(suppressWarnings(dyad_correlation(ego_a)))
})
