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
	
	# test 1: Catch plural form of highlight_color
	expect_warning(
		plot(net, highlight = c("1", "2"), highlight_colors = c("1" = "red", "2" = "blue")),
		regexp = "highlight_colors.*typo.*highlight_color"
	)
	
	# test 2: Catch British spelling
	expect_warning(
		plot(net, node_colour = "red"),
		regexp = "node_colour.*typo.*node_color"
	)
	
	# test 3: Catch completely unknown parameter
	expect_warning(
		plot(net, random_unknown_param = "value"),
		regexp = "Unknown parameter.*random_unknown_param"
	)
	
	# test 4: Fuzzy matching for close typos
	expect_warning(
		plot(net, higlight = c("1")),
		regexp = "higlight.*Did you mean.*highlight"
	)
	
	# test 5: No warning for correct parameters
	expect_no_warning(
		plot(net, 
			highlight = c("1", "2"),
			highlight_color = c("1" = "red", "2" = "blue", "Other" = "grey"),
			node_size = 5,
			edge_color = "grey70"
		)
	)
})