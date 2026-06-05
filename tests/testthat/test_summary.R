set.seed(6886)

test_that("summary.netify works for cross-sectional networks", {
	# create a simple cross-sectional network
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net = netify(
		icews_10,
		actor1 = "i", actor2 = "j",
		symmetric = FALSE,
		weight = "verbCoop"
	)

	summ = summary(net)

	expect_s3_class(summ, "data.frame")
	expect_equal(nrow(summ), 1) # one row for cross-sectional
	expect_true("density" %in% names(summ))
	expect_true("num_actors" %in% names(summ))
	expect_true("transitivity" %in% names(summ))
	expect_true("reciprocity" %in% names(summ))
	expect_true("mutual" %in% names(summ))
	expect_true(summ$mutual >= 0 && summ$mutual <= 1)
	expect_false("layer" %in% names(summ)) # no layer column for single layer
})

test_that("summary.netify works for longitudinal networks", {
	skip_on_cran()
	# create longitudinal network
	data(icews)
	net_longit = netify(
		icews,
		actor1 = "i", actor2 = "j", time = "year",
		symmetric = FALSE,
		weight = "matlConf"
	)

	summ = summary(net_longit)

	expect_s3_class(summ, "data.frame")
	expect_equal(nrow(summ), length(unique(icews$year)))
	expect_true("net" %in% names(summ))
	expect_equal(summ$net, as.character(unique(icews$year)))
})

test_that("summary.netify works for symmetric networks", {
	# create symmetric network
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net_sym = netify(
		icews_10,
		actor1 = "i", actor2 = "j",
		symmetric = TRUE,
		weight = "verbCoop"
	)

	summ = summary(net_sym)

	# check that directed-only columns are absent
	expect_false("reciprocity" %in% names(summ))
	expect_false("mutual" %in% names(summ))
	expect_false("covar_of_row_col_means" %in% names(summ))
	expect_false("competition_col" %in% names(summ))

	# check that renamed columns exist
	expect_true("competition" %in% names(summ))
	expect_true("sd_of_actor_means" %in% names(summ))
})

test_that("summary.netify works for binary networks", {
	# create binary network
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net_bin = netify(
		icews_10,
		actor1 = "i", actor2 = "j",
		symmetric = FALSE
	)

	summ = suppressWarnings(summary(net_bin))

	# check that weight statistics are absent
	expect_false("mean_edge_weight" %in% names(summ))
	expect_false("sd_edge_weight" %in% names(summ))
	expect_false("median_edge_weight" %in% names(summ))
})

test_that("summary.netify works for multilayer networks", {
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

	summ = summary(multi_net)

	expect_equal(nrow(summ), 2) # two layers
	expect_true("layer" %in% names(summ))
	expect_equal(summ$layer, c("verbal", "material"))
})

test_that("summary.netify handles custom statistics correctly", {
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net = netify(
		icews_10,
		actor1 = "i", actor2 = "j",
		weight = "verbCoop"
	)

	# test single-value custom function
	mean_stat = function(mat) {
		mean(mat, na.rm = TRUE)
	}

	summ1 = summary(net, other_stats = list(avg = mean_stat))
	expect_true("avg" %in% names(summ1))
	expect_type(summ1$avg, "double")

	# test multi-value custom function
	multi_stat = function(mat) {
		c(
			total = sum(mat, na.rm = TRUE),
			nonzero = sum(mat > 0, na.rm = TRUE)
		)
	}

	summ2 = summary(net, other_stats = list(custom = multi_stat))
	expect_true("total" %in% names(summ2))
	expect_true("nonzero" %in% names(summ2))

	# test multiple custom functions
	summ3 = summary(net, other_stats = list(
		avg = mean_stat,
		custom = multi_stat
	))
	expect_true(all(c("avg", "custom.total", "custom.nonzero") %in% names(summ3)))
})

test_that("summary.netify handles bipartite networks", {
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

	summ = summary(net_bip)

	# check bipartite-specific columns
	expect_true("num_row_actors" %in% names(summ))
	expect_true("num_col_actors" %in% names(summ))
	expect_false("num_actors" %in% names(summ))

	expect_false("reciprocity" %in% names(summ))
	expect_false("mutual" %in% names(summ))
	expect_false("transitivity" %in% names(summ))
})

test_that("summary.netify handles missing data correctly", {
	skip_on_cran()
	# create network with missing values
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	# introduce some nas
	icews_10$verbCoop[sample(nrow(icews_10), 100)] = NA

	net_missing = netify(
		icews_10,
		actor1 = "i", actor2 = "j",
		weight = "verbCoop",
		missing_to_zero = FALSE
	)

	expect_warning({
		summ = summary(net_missing)
	}, NA)

	expect_true("prop_edges_missing" %in% names(summ))
	expect_true(summ$prop_edges_missing > 0)
		expect_false(is.na(summ$density)) # density is finite
})

test_that("summary.netify reports weighted edge stats over realized ties", {
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
	summ = summary(net)

	expect_equal(summ$num_edges, 2)
	expect_equal(summ$density, 2 / 6)
	expect_equal(summ$mean_edge_weight, 3)
	expect_equal(summ$median_edge_weight, 3)
})

test_that("print.netify does not show NaN for all-missing averaged statistics", {
	mat1 = matrix(c(
		NA, 1, NA,
		NA, NA, 0,
		NA, NA, NA
	), 3, 3, byrow = TRUE, dimnames = list(letters[1:3], letters[1:3]))
	mat2 = matrix(c(
		NA, 0, NA,
		NA, NA, 1,
		NA, NA, NA
	), 3, 3, byrow = TRUE, dimnames = list(letters[1:3], letters[1:3]))
	net = new_netify(list(t1 = mat1, t2 = mat2),
		symmetric = FALSE, missing_to_zero = FALSE, diag_to_NA = TRUE)

	summ = summary(net)
	expect_false(any(is.nan(summ$transitivity)))
	out = capture.output(print(net))
	expect_false(any(grepl("NaN", out, fixed = TRUE)))
})

test_that("summary.netify handles ego networks", {
	skip_on_cran()
	# first create a netify object
	data(icews)
	net = netify(
		icews,
		actor1 = "i", actor2 = "j", time = "year",
		weight = "verbCoop"
	)

	# then create ego network from the netify object
	ego_net = ego_netify(
		net,
		ego = "United States"
	)

	summ = summary(ego_net)

	# check ego network structure
	expect_true("net" %in% names(summ))
	expect_true(all(grepl("United States", summ$layer)))
})
