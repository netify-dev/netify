library(netify)

test_that("plot parameter validation catches common mistakes", {
	# create a simple network for testing
	mat = matrix(rbinom(100, 1, 0.3), 10, 10)
	df = data.frame(
		i = rep(1:10, each = 10),
		j = rep(1:10, times = 10),
		weight = as.vector(mat)
	)
	net = netify(df[df$weight > 0,], actor1 = "i", actor2 = "j")
	
	# catches plural form of highlight_color
	expect_warning(
		plot(net, highlight = c("1", "2"), highlight_colors = c("1" = "red", "2" = "blue")),
		regexp = "highlight_colors.*typo.*highlight_color"
	)
	
	# catches british spelling
	expect_warning(
		plot(net, node_colour = "red"),
		regexp = "node_colour.*typo.*node_color"
	)
	
	# catches completely unknown parameter
	expect_warning(
		plot(net, random_unknown_param = "value"),
		regexp = "Unknown parameter.*random_unknown_param"
	)
	
	# fuzzy matching for close typos
	expect_warning(
		plot(net, higlight = c("1")),
		regexp = "higlight.*Did you mean.*highlight"
	)
	
	# no warning for correct parameters
	expect_no_warning(
		plot(net,
			highlight = c("1", "2"),
			highlight_color = c("1" = "red", "2" = "blue", "Other" = "grey"),
			node_size = 5,
			edge_color = "grey70"
		)
	)
})

test_that("plot errors friendly when *_by references a missing column", {
	# small directed mention-style network
	df = data.frame(
		from = c("a", "a", "b", "c", "d", "e"),
		to   = c("b", "c", "c", "d", "a", "a"),
		stringsAsFactors = FALSE
	)
	net = netify(df, actor1 = "from", actor2 = "to", symmetric = FALSE)

	# node_color_by with bad col: error names the kwarg + lists available cols
	expect_error(
		plot(net, node_color_by = "not_a_col"),
		regexp = "node_color_by.*not_a_col.*does not exist"
	)

	# degree_total is a known network statistic -> hint to use add_node_vars
	expect_error(
		plot(net, node_size_by = "degree_total"),
		regexp = "network statistic.*add_node_vars"
	)

	# edge_color_by with bad col triggers edge-side error
	expect_error(
		plot(net, edge_color_by = "fakecol"),
		regexp = "edge_color_by.*fakecol.*does not exist"
	)
})

test_that("plot network-statistic hint includes time for longitudinal networks", {
	df = data.frame(
		from = c("a", "b", "a", "c"),
		to = c("b", "c", "c", "a"),
		year = c(1, 1, 2, 2),
		stringsAsFactors = FALSE
	)
	net = netify(df, actor1 = "from", actor2 = "to", time = "year",
		symmetric = FALSE)

	expect_error(
		plot(net, node_size_by = "degree_total"),
		regexp = "add_node_vars.*time = \"time\""
	)
})
