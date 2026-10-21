set.seed(6886)
skip_on_cran()

test_that("undirected networks draw one edge per tie", {
	data(classroom_edges)
	net = suppressMessages(netify(classroom_edges, actor1 = "from", actor2 = "to"))
	raw = get_raw(net)
	n_ties = sum(raw[upper.tri(raw)] != 0, na.rm = TRUE)

	pieces = plot(net, return_components = TRUE)
	edge_data = pieces$edges$data
	expect_equal(nrow(edge_data), n_ties)

	pair_key = paste(pmin(edge_data$from, edge_data$to), pmax(edge_data$from, edge_data$to))
	expect_false(anyDuplicated(pair_key) > 0)

	built = ggplot2::ggplot_build(plot(net, curve_edges = TRUE))
	edge_layers = Filter(function(d) all(c("x", "xend") %in% names(d)), built$data)
	expect_equal(nrow(edge_layers[[1]]), n_ties)
})

test_that("undirected weighted edges keep their weights after deduplication", {
	data(icews)
	net = suppressWarnings(suppressMessages(netify(
		icews[icews$year == 2010, ],
		actor1 = "i", actor2 = "j",
		weight = "verbCoop"
	)))
	edge_data = net_plot_data(net, list())$net_dfs$edge_data
	raw = get_raw(net)

	expect_equal(nrow(edge_data), sum(raw[upper.tri(raw)] != 0, na.rm = TRUE))
	expect_equal(edge_data$verbCoop, unname(raw[cbind(edge_data$from, edge_data$to)]))
	expect_false(any(is.na(edge_data$x1)))
})

test_that("mixed symmetric and directed layers keep the right edge counts", {
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	sym_net = suppressWarnings(suppressMessages(netify(
		icews_10,
		actor1 = "i", actor2 = "j",
		weight = "verbCoop"
	)))
	dir_net = suppressWarnings(suppressMessages(netify(
		icews_10,
		actor1 = "i", actor2 = "j",
		weight = "verbConf",
		symmetric = FALSE
	)))
	multi = layer_netify(list(sym_net, dir_net), layer_labels = c("sym", "dir"))

	edge_data = net_plot_data(multi, list())$net_dfs$edge_data
	sym_raw = get_raw(sym_net)
	dir_raw = get_raw(dir_net)
	diag(dir_raw) = 0

	expect_equal(
		sum(edge_data$layer == "sym"),
		sum(sym_raw[upper.tri(sym_raw)] != 0, na.rm = TRUE)
	)
	expect_equal(sum(edge_data$layer == "dir"), sum(dir_raw != 0, na.rm = TRUE))
	expect_s3_class(plot(multi), "ggplot")
})

test_that("bipartite actors in both modes plot as two located nodes", {
	df = data.frame(
		a = c("p1", "p1", "p2", "x"),
		b = c("o1", "x", "o2", "o3"),
		w = 1:4
	)
	net = netify(df, actor1 = "a", actor2 = "b", mode = "bipartite", weight = "w")

	pieces = plot(net, return_components = TRUE)
	node_data = pieces$points$data
	edge_data = pieces$edges$data

	expect_equal(nrow(node_data), 7)
	expect_false(any(is.na(node_data$x)))
	expect_false(any(is.na(edge_data$x1)))
	expect_false(any(grepl("^(row|col):", node_data$name)))

	x_nodes = node_data[node_data$name == "x", ]
	expect_equal(sort(x_nodes$actor_mode), c("col", "row"))
	expect_false(isTRUE(all.equal(x_nodes$y[1], x_nodes$y[2])))

	built = ggplot2::ggplot_build(plot(net))
	point_layer = Filter(function(d) nrow(d) == 7 && !"xend" %in% names(d), built$data)
	expect_true(length(point_layer) >= 1)
	expect_false(any(is.na(point_layer[[1]]$x)))
})

test_that("mexico bipartite network plots with coordinates", {
	data(mexico)
	net = suppressMessages(netify(
		mexico,
		actor1 = "side_a", actor2 = "side_b",
		mode = "bipartite",
		sum_dyads = TRUE
	))
	overlap = intersect(rownames(net), colnames(net))
	expect_true(length(overlap) > 0)

	pieces = plot(net, return_components = TRUE)
	expect_true(nrow(pieces$points$data) > 0)
	expect_false(any(is.na(pieces$points$data$x)))
	expect_false(any(is.na(pieces$edges$data$x1)))
	expect_true(all(overlap %in% pieces$points$data$name))

	built = ggplot2::ggplot_build(plot(net))
	expect_true(all(vapply(built$data, function(d) !any(is.na(d$x)), logical(1))))
})

test_that("static actor positions work for longitudinal bipartite networks", {
	df = data.frame(
		a = sample(paste0("r", 1:5), 40, replace = TRUE),
		b = sample(paste0("c", 1:4), 40, replace = TRUE),
		t = rep(2001:2004, 10)
	)
	net = netify(
		df,
		actor1 = "a", actor2 = "b", time = "t",
		mode = "bipartite",
		sum_dyads = TRUE
	)

	pieces = plot(net, static_actor_positions = TRUE, return_components = TRUE)
	node_data = pieces$points$data
	expect_false(any(is.na(node_data$x)))

	# each actor keeps one position across periods
	n_positions = tapply(
		paste(node_data$x, node_data$y),
		node_data$name,
		function(v) length(unique(v))
	)
	expect_true(all(n_positions == 1))
})

test_that("sugiyama layout is supported", {
	data(classroom_edges)
	net = suppressMessages(netify(classroom_edges, actor1 = "from", actor2 = "to"))

	layout = get_node_layout(net, layout = "sugiyama")[[1]]
	expect_equal(nrow(layout), nrow(get_raw(net)))
	expect_false(any(is.na(layout$x)))
	expect_s3_class(plot(net, layout = "sugiyama"), "ggplot")
})

test_that("point_layout accepts a single data frame", {
	data(classroom_edges)
	net = suppressMessages(netify(classroom_edges, actor1 = "from", actor2 = "to"))
	layout = get_node_layout(net)[[1]]

	pieces = plot(net, point_layout = layout, return_components = TRUE)
	node_data = pieces$points$data
	expect_equal(
		node_data$x,
		layout$x[match(node_data$name, layout$actor)]
	)
	expect_false(any(is.na(pieces$edges$data$x1)))

	edges = get_edge_layout(net, layout)
	expect_true(is.data.frame(edges[[1]]))
	expect_false(any(is.na(edges[[1]]$x1)))
})

test_that("a single point_layout data frame is reused across time periods", {
	df = data.frame(
		i = c("a", "a", "b", "a", "b", "c"),
		j = c("b", "c", "c", "b", "c", "a"),
		t = c(1, 1, 1, 2, 2, 2)
	)
	net = suppressMessages(netify(df, actor1 = "i", actor2 = "j", time = "t", symmetric = FALSE))
	layout = data.frame(actor = c("a", "b", "c"), x = c(0, 1, 2), y = c(0, 1, 0))

	pieces = plot(net, point_layout = layout, return_components = TRUE)
	node_data = pieces$points$data
	expect_setequal(unique(node_data$time), c("1", "2"))
	expect_equal(node_data$x, layout$x[match(node_data$name, layout$actor)])
	expect_false(any(is.na(pieces$edges$data$x1)))
})

test_that("node_filter drops edges per period in longitudinal plots", {
	df = data.frame(
		i = c("a", "a", "b", "a", "a", "b"),
		j = c("b", "c", "c", "b", "c", "c"),
		t = c(1, 1, 1, 2, 2, 2)
	)
	net = suppressMessages(netify(df, actor1 = "i", actor2 = "j", time = "t", symmetric = FALSE))
	node_vars = data.frame(
		actor = rep(c("a", "b", "c"), 2),
		time = rep(1:2, each = 3),
		keep = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
	)
	net = add_node_vars(net, node_vars, actor = "actor", time = "time")

	pieces = plot(net, node_filter = ~keep, remove_isolates = FALSE, return_components = TRUE)
	edge_data = pieces$edges$data

	expect_false(any(edge_data$time == "1" & (edge_data$from == "a" | edge_data$to == "a")))
	expect_equal(sum(edge_data$time == "2"), 3)
	expect_equal(sum(edge_data$time == "1"), 1)
})

test_that("node_filter works on attached actor statistics", {
	data(classroom_edges)
	net = suppressMessages(netify(classroom_edges, actor1 = "from", actor2 = "to"))
	net = add_node_vars(net, summary_actor(net), actor = "actor")

	pieces = plot(net, node_filter = ~ degree > 3, return_components = TRUE)
	expect_true(all(pieces$points$data$degree > 3))
	expect_true(all(pieces$edges$data$from %in% pieces$points$data$name))
	expect_true(all(pieces$edges$data$to %in% pieces$points$data$name))
})

test_that("time_filter accepts numeric time labels", {
	df = data.frame(
		i = rep(c("a", "b", "c"), 3),
		j = rep(c("b", "c", "a"), 3),
		t = rep(2011:2013, each = 3)
	)
	net = suppressMessages(netify(df, actor1 = "i", actor2 = "j", time = "t", symmetric = FALSE))

	by_label = plot(net, time_filter = 2012, return_components = TRUE)
	by_char = plot(net, time_filter = "2012", return_components = TRUE)
	expect_equal(by_label$points$data, by_char$points$data)

	by_index = plot(net, time_filter = 2:3, return_components = TRUE)
	expect_equal(sort(unique(by_index$points$data$time)), c("2012", "2013"))

	expect_error(
		plot(net, time_filter = 1999),
		"Available time periods"
	)
})

test_that("time_filter keeps index semantics when values are also labels", {
	df = data.frame(
		i = rep(c("a", "b", "c"), 5),
		j = rep(c("b", "c", "a"), 5),
		t = rep(2:6, each = 3)
	)
	net = suppressMessages(netify(df, actor1 = "i", actor2 = "j", time = "t", symmetric = FALSE))

	pieces = plot(net, time_filter = 2:3, return_components = TRUE)
	expect_equal(sort(unique(pieces$points$data$time)), c("3", "4"))
})
