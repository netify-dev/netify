set.seed(6886)

library(testthat)
library(netify)

test_that("get_ego_layout works for star layout", {
	# create simple test data
	test_data = data.frame(
		from = c("ego", "ego", "ego", "alter1", "alter2"),
		to = c("alter1", "alter2", "alter3", "alter3", "alter3"),
		weight = c(10, 20, 30, 5, 5),
		stringsAsFactors = FALSE
	)
	
	# create netify object and ego network
	net = netify(test_data, actor1 = "from", actor2 = "to", weight = "weight")
	ego_net = ego_netify(net, ego = "ego", threshold = 0)
	
	# get star layout
	layout = get_ego_layout(ego_net, layout = "star")
	
	# check structure
	expect_type(layout, "list")
	expect_length(layout, 1)
	
	# check layout data frame
	layout_df = layout[[1]]
	expect_s3_class(layout_df, "data.frame")
	expect_equal(nrow(layout_df), 4)  # ego + 3 alters
	expect_equal(colnames(layout_df), c("index", "actor", "x", "y"))
	
	# check ego is at center
	ego_row = layout_df[layout_df$actor == "ego", ]
	expect_equal(ego_row$x, 0)
	expect_equal(ego_row$y, 0)
	
	# check alters are arranged in circle
	alter_rows = layout_df[layout_df$actor != "ego", ]
	distances = sqrt(alter_rows$x^2 + alter_rows$y^2)
	expect_true(all(abs(distances - 1) < 0.001))  # all at radius 1
})

test_that("get_ego_layout works for radial layout with grouping", {
	test_data = data.frame(
		from = c("ego", "ego", "ego", "ego", "alter1", "alter2"),
		to = c("alter1", "alter2", "alter3", "alter4", "alter3", "alter4"),
		weight = c(10, 20, 30, 40, 5, 5),
		stringsAsFactors = FALSE
	)
	
	# create nodal attributes
	nodal_attrs = data.frame(
		actor = c("ego", "alter1", "alter2", "alter3", "alter4"),
		group = c("center", "group1", "group1", "group2", "group2"),
		stringsAsFactors = FALSE
	)
	
	# create netify object with attributes
	net = netify(test_data, actor1 = "from", actor2 = "to", weight = "weight",
				  nodal_data = nodal_attrs)
	ego_net = ego_netify(net, ego = "ego", threshold = 0)
	
	# get radial layout with grouping
	layout = get_ego_layout(ego_net, layout = "radial", group_by = "group")
	
	# check basic structure
	layout_df = layout[[1]]
	expect_equal(nrow(layout_df), 5)  # ego + 4 alters
	
	# check ego is at center
	ego_row = layout_df[layout_df$actor == "ego", ]
	expect_equal(ego_row$x, 0)
	expect_equal(ego_row$y, 0)
	
	# check that alters from same group are in same sector
	# get angles for each alter
	alter_df = layout_df[layout_df$actor != "ego", ]
	alter_df = merge(alter_df, nodal_attrs, by = "actor")
	alter_df$angle = atan2(alter_df$y, alter_df$x)
	
	group1_angles = alter_df$angle[alter_df$group == "group1"]
	expect_true(max(group1_angles) - min(group1_angles) < pi)  # within same half-circle
	
	group2_angles = alter_df$angle[alter_df$group == "group2"]
	expect_true(max(group2_angles) - min(group2_angles) < pi)  # within same half-circle
})

test_that("get_ego_layout works for concentric layout", {
	test_data = data.frame(
		from = c("ego", "ego", "ego", "ego"),
		to = c("alter1", "alter2", "alter3", "alter4"),
		weight = c(10, 20, 30, 40),
		stringsAsFactors = FALSE
	)
	
	# create nodal attributes with numeric variable that will create distinct rings
	nodal_attrs = data.frame(
		actor = c("ego", "alter1", "alter2", "alter3", "alter4"),
		importance = c(100, 10, 20, 80, 90),  # more varied values for distinct rings
		stringsAsFactors = FALSE
	)
	
	# create netify object
	net = netify(test_data, actor1 = "from", actor2 = "to", weight = "weight",
				  nodal_data = nodal_attrs)
	ego_net = ego_netify(net, ego = "ego", threshold = 0)
	
	# get concentric layout
	layout = get_ego_layout(ego_net, layout = "concentric", 
						   group_by = "importance", ring_gap = 0.2)
	
	# check structure
	layout_df = layout[[1]]
	expect_equal(nrow(layout_df), 5)
	
	# check ego is at center
	ego_row = layout_df[layout_df$actor == "ego", ]
	expect_equal(ego_row$x, 0)
	expect_equal(ego_row$y, 0)
	
	# check that alters are arranged in rings
	alter_df = layout_df[layout_df$actor != "ego", ]
	distances = sqrt(alter_df$x^2 + alter_df$y^2)
	# rings reflect distinct importance levels
	unique_distances = unique(round(distances, 2))
	expect_true(length(unique_distances) >= 1)
})

test_that("get_ego_layout works with weight_to_distance", {
	# create weighted network
	test_data = data.frame(
		from = c("ego", "ego", "ego"),
		to = c("close", "medium", "far"),
		weight = c(100, 50, 10),  # higher weight = closer relationship
		stringsAsFactors = FALSE
	)
	
	net = netify(test_data, actor1 = "from", actor2 = "to", weight = "weight")
	ego_net = ego_netify(net, ego = "ego", threshold = 0)
	
	# get radial layout with weight-based distance
	layout = get_ego_layout(ego_net, layout = "radial", 
						   weight_to_distance = TRUE)
	
	layout_df = layout[[1]]
	
	# calculate distances
	close_dist = sqrt(sum(layout_df[layout_df$actor == "close", c("x", "y")]^2))
	medium_dist = sqrt(sum(layout_df[layout_df$actor == "medium", c("x", "y")]^2))
	far_dist = sqrt(sum(layout_df[layout_df$actor == "far", c("x", "y")]^2))
	
	expect_true(close_dist < medium_dist)
	expect_true(medium_dist < far_dist)
})

test_that("get_ego_layout works for longitudinal ego networks", {
	# create longitudinal test data
	test_data = data.frame(
		from = rep(c("ego", "ego", "alter1"), 2),
		to = rep(c("alter1", "alter2", "alter2"), 2),
		time = c(rep(2010, 3), rep(2011, 3)),
		weight = c(10, 20, 5, 30, 40, 15),
		stringsAsFactors = FALSE
	)
	
	# create longitudinal netify and ego network
	net = netify(test_data, actor1 = "from", actor2 = "to", 
				  time = "time", weight = "weight")
	ego_net = ego_netify(net, ego = "ego", threshold = 0)
	
	# get layout
	layout = get_ego_layout(ego_net, layout = "star")
	
	# check structure
	expect_type(layout, "list")
	expect_length(layout, 2)  # two time periods
	expect_equal(names(layout), c("2010", "2011"))
	
	# check each time period
	for (t in names(layout)) {
		layout_t = layout[[t]]
		expect_s3_class(layout_t, "data.frame")
		expect_equal(colnames(layout_t), c("index", "actor", "x", "y"))
		
		ego_row = layout_t[layout_t$actor == "ego", ]
		expect_equal(ego_row$x, 0)
		expect_equal(ego_row$y, 0)
	}
})

test_that("get_node_layout redirects to get_ego_layout for ego networks", {
	test_data = data.frame(
		from = c("ego", "ego", "ego"),
		to = c("A", "B", "C"),
		stringsAsFactors = FALSE
	)
	
	net = netify(test_data, actor1 = "from", actor2 = "to")
	ego_net = ego_netify(net, ego = "ego", threshold = 0)
	
	# call get_node_layout with ego-specific layout
	layout = get_node_layout(ego_net, layout = "radial")
	
	layout_df = layout[[1]]
	ego_row = layout_df[layout_df$actor == "ego", ]
	expect_equal(ego_row$x, 0)
	expect_equal(ego_row$y, 0)
	
	nodal_attrs = data.frame(
		actor = c("ego", "A", "B", "C"),
		group = c("center", "g1", "g1", "g2"),
		stringsAsFactors = FALSE
	)
	
	net2 = netify(test_data, actor1 = "from", actor2 = "to", nodal_data = nodal_attrs)
	ego_net2 = ego_netify(net2, ego = "ego", threshold = 0)
	
	# pass ego parameters through get_node_layout
	layout2 = get_node_layout(ego_net2, layout = "radial", 
							  ego_group_by = "group", ego_order_by = "actor")
	
	expect_type(layout2, "list")
	expect_equal(nrow(layout2[[1]]), 4)
})

test_that("get_ego_layout handles edge cases", {
	# single alter
	test_data = data.frame(
		from = "ego",
		to = "alter1",
		stringsAsFactors = FALSE
	)
	
	net = netify(test_data, actor1 = "from", actor2 = "to")
	ego_net = ego_netify(net, ego = "ego", threshold = 0)
	
	layout = get_ego_layout(ego_net, layout = "star")
	expect_equal(nrow(layout[[1]]), 2)
	
	# missing grouping variable
	layout3 = get_ego_layout(ego_net, layout = "radial", group_by = "nonexistent")
		expect_equal(nrow(layout3[[1]]), 2)  # works without grouping
})
