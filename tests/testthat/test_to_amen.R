set.seed(6886)

# ---------------------------------------------------------------------------
# unipartite, cross-sectional
# ---------------------------------------------------------------------------

test_that("to_amen, unipartite cross-sec returns correct shapes", {
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net = netify(
		icews_10,
		actor1 = "i", actor2 = "j",
		symmetric = FALSE, weight = "verbCoop",
		nodal_vars = c("i_polity2", "i_log_gdp"),
		dyad_vars = c("matlCoop")
	)
	out = to_amen(net)

	# y is n x n
	expect_true(is.matrix(out$Y))
	expect_equal(dim(out$Y)[1], dim(out$Y)[2])
	expect_equal(rownames(out$Y), colnames(out$Y))

	# xrow, xcol agree for unipartite networks
	expect_identical(out$Xrow, out$Xcol)
	expect_equal(nrow(out$Xrow), nrow(out$Y))
	expect_equal(ncol(out$Xrow), 2L)

	# xdyad is n x n x p
	expect_equal(length(dim(out$Xdyad)), 3L)
	expect_equal(dim(out$Xdyad)[1], dim(out$Y)[1])
	expect_equal(dim(out$Xdyad)[3], 1L)
})

# ---------------------------------------------------------------------------
# bipartite, cross-sectional
# ---------------------------------------------------------------------------

test_that("to_amen, bipartite cross-sec separates Xrow / Xcol correctly", {
	rows = paste0("firm_", 1:4)
	cols = paste0("event_", 1:3)
	df = expand.grid(actor1 = rows, actor2 = cols, stringsAsFactors = FALSE)
	df$y = rbinom(nrow(df), 1, 0.4)

	net = netify(
		df, actor1 = "actor1", actor2 = "actor2",
		weight = "y", mode = "bipartite", symmetric = FALSE
	)

	# attach separate row- and col-mode attributes
	node_df = data.frame(
		actor = c(rows, cols),
		attr_x = c(rnorm(length(rows)), rnorm(length(cols))),
		attr_y = c(rnorm(length(rows)), rnorm(length(cols)))
	)
	net = add_node_vars(net, node_df, actor = "actor")

	out = to_amen(net)

	# y has bipartite shape
	expect_equal(dim(out$Y), c(length(rows), length(cols)))

	# xrow only contains row actors
	expect_equal(dim(out$Xrow), c(length(rows), 2L))
	expect_identical(rownames(out$Xrow), rows)
	expect_false(anyNA(out$Xrow))

	# xcol only contains col actors
	expect_equal(dim(out$Xcol), c(length(cols), 2L))
	expect_identical(rownames(out$Xcol), cols)
	expect_false(anyNA(out$Xcol))

	# xrow != xcol for bipartite
	expect_false(identical(out$Xrow, out$Xcol))

	# values come from the right mode
	expect_equal(unname(out$Xrow[, "attr_x"]), node_df$attr_x[1:length(rows)])
	expect_equal(
		unname(out$Xcol[, "attr_x"]),
		node_df$attr_x[(length(rows) + 1):nrow(node_df)]
	)
})

test_that("to_amen preserves cross-sec nodal variables named time or layer", {
	actors = c("a", "b", "c")
	mat = matrix(c(0, 1, 2, 3, 0, 4, 5, 6, 0), 3, 3,
		dimnames = list(actors, actors))
	net = new_netify(mat, symmetric = FALSE)
	node_df = data.frame(
		actor = actors,
		time = c(10, 20, 30),
		layer = c(1, 2, 3),
		score = c(4, 5, 6)
	)
	net = add_node_vars(net, node_df, actor = "actor")

	expect_equal(measurements(net)$nvars, c("time", "layer", "score"))
	out = to_amen(net)
	expect_equal(colnames(out$Xrow), c("time", "layer", "score"))
	expect_equal(unname(out$Xrow[, "time"]), node_df$time)
	expect_equal(unname(out$Xrow[, "layer"]), node_df$layer)
})

test_that("to_amen, bipartite cross-sec without nodal data still works", {
	skip_on_cran()
	rows = paste0("r_", 1:3)
	cols = paste0("c_", 1:5)
	df = expand.grid(actor1 = rows, actor2 = cols, stringsAsFactors = FALSE)
	df$y = rbinom(nrow(df), 1, 0.5)

	net = netify(
		df, actor1 = "actor1", actor2 = "actor2",
		weight = "y", mode = "bipartite", symmetric = FALSE
	)
	out = to_amen(net)

	expect_equal(dim(out$Y), c(length(rows), length(cols)))
	expect_null(out$Xrow)
	expect_null(out$Xcol)
})

# ---------------------------------------------------------------------------
# bipartite, longitudinal (longit_list, lame = true)
# ---------------------------------------------------------------------------

test_that("to_amen(lame=TRUE) bipartite longit_list builds Xrow / Xcol separately", {
	rows = paste0("firm_", 1:4)
	cols = paste0("event_", 1:3)
	years = 2020:2022
	df = expand.grid(
		actor1 = rows, actor2 = cols, year = years,
		stringsAsFactors = FALSE
	)
	df$y = rbinom(nrow(df), 1, 0.4)

	node_df = expand.grid(
		actor = c(rows, cols), year = years,
		stringsAsFactors = FALSE
	)
	node_df$attr_x = rnorm(nrow(node_df))

	net = netify(
		df, actor1 = "actor1", actor2 = "actor2", time = "year",
		weight = "y", mode = "bipartite", symmetric = FALSE
	)
	net = add_node_vars(net, node_df, actor = "actor", time = "year")

	out = to_amen(net, lame = TRUE)

	expect_equal(length(out$Y), length(years))
	for (t in seq_along(years)) {
		expect_equal(dim(out$Y[[t]]), c(length(rows), length(cols)))
		expect_equal(dim(out$Xrow[[t]]), c(length(rows), 1L))
		expect_equal(dim(out$Xcol[[t]]), c(length(cols), 1L))
		expect_identical(rownames(out$Xrow[[t]]), rows)
		expect_identical(rownames(out$Xcol[[t]]), cols)
	}
	expect_false(identical(out$Xrow, out$Xcol))
})

# ---------------------------------------------------------------------------
# bipartite, longitudinal (longit_list, lame = false -- array output)
# ---------------------------------------------------------------------------

test_that("to_amen(lame=FALSE) bipartite longit_list builds correct arrays", {
	rows = paste0("firm_", 1:4)
	cols = paste0("event_", 1:3)
	years = 2020:2022
	df = expand.grid(
		actor1 = rows, actor2 = cols, year = years,
		stringsAsFactors = FALSE
	)
	df$y = rbinom(nrow(df), 1, 0.4)
	node_df = expand.grid(
		actor = c(rows, cols), year = years,
		stringsAsFactors = FALSE
	)
	node_df$attr_x = rnorm(nrow(node_df))

	net = netify(
		df, actor1 = "actor1", actor2 = "actor2", time = "year",
		weight = "y", mode = "bipartite", symmetric = FALSE
	)
	net = add_node_vars(net, node_df, actor = "actor", time = "year")
	out = to_amen(net, lame = FALSE)

	expect_equal(dim(out$Y), c(length(rows), length(cols), length(years)))
	expect_equal(dim(out$Xrow), c(length(rows), 1L, length(years)))
	expect_equal(dim(out$Xcol), c(length(cols), 1L, length(years)))
	expect_identical(dimnames(out$Xrow)[[1]], rows)
	expect_identical(dimnames(out$Xcol)[[1]], cols)
})

# ---------------------------------------------------------------------------
# bipartite, longitudinal (longit_array)
# ---------------------------------------------------------------------------

test_that("to_amen(lame=FALSE) bipartite longit_array builds correct arrays", {
	skip_on_cran()
	rows = paste0("firm_", 1:4)
	cols = paste0("event_", 1:3)
	years = 2020:2022
	df = expand.grid(
		actor1 = rows, actor2 = cols, year = years,
		stringsAsFactors = FALSE
	)
	df$y = rbinom(nrow(df), 1, 0.4)
	node_df = expand.grid(
		actor = c(rows, cols), year = years,
		stringsAsFactors = FALSE
	)
	node_df$attr_x = rnorm(nrow(node_df))

	net = netify(
		df, actor1 = "actor1", actor2 = "actor2", time = "year",
		weight = "y", mode = "bipartite", symmetric = FALSE,
		output_format = "longit_array"
	)
	net = add_node_vars(net, node_df, actor = "actor", time = "year")

	out = to_amen(net, lame = FALSE)
	expect_equal(dim(out$Y), c(length(rows), length(cols), length(years)))
	expect_equal(dim(out$Xrow), c(length(rows), 1L, length(years)))
	expect_equal(dim(out$Xcol), c(length(cols), 1L, length(years)))
})

test_that("to_amen ignores time metadata after subsetting longitudinal to cross-sec", {
	actors = c("a", "b", "c")
	df = expand.grid(i = actors, j = actors, year = 1:2, stringsAsFactors = FALSE)
	df = df[df$i != df$j, ]
	df$y = seq_len(nrow(df))

	node_df = expand.grid(actor = actors, year = 1:2, stringsAsFactors = FALSE)
	node_df$score = seq_len(nrow(node_df))

	net = netify(
		df, actor1 = "i", actor2 = "j", time = "year",
		weight = "y", symmetric = FALSE
	)
	net = add_node_vars(net, node_df, actor = "actor", time = "year")

	one_period = subset_netify(net, time = "1")
	expect_equal(attr(one_period, "netify_type"), "cross_sec")
	expect_false("time" %in% names(attr(one_period, "nodal_data")))
	expect_equal(measurements(one_period)$nvars, "score")

	out = to_amen(one_period)
	expect_equal(ncol(out$Xrow), 1L)
	expect_equal(colnames(out$Xrow), "score")
	expect_identical(out$Xrow, out$Xcol)
})

# ---------------------------------------------------------------------------
# regression: unipartite longit lame shortcut still works
# ---------------------------------------------------------------------------

test_that("to_amen unipartite longit keeps Xcol = Xrow shortcut", {
	skip_on_cran()
	data(icews)
	icews_sub = icews[icews$year %in% 2010:2012, ]
	net = netify(
		icews_sub, actor1 = "i", actor2 = "j", time = "year",
		symmetric = FALSE, weight = "verbCoop",
		nodal_vars = c("i_polity2")
	)
	out_lame = to_amen(net, lame = TRUE)
	expect_identical(out_lame$Xrow, out_lame$Xcol)

	out_arr = to_amen(net, lame = FALSE)
	expect_identical(out_arr$Xrow, out_arr$Xcol)
})
