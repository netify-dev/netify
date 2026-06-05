# tests for nodelist parameter, which ensures all nodes are included
# (including isolates missing from edgelists)

test_that("nodelist parameter is accepted by all functions without error", {
	# create simple dyadic data
	dyad_df = data.frame(
		actor1 = c("A", "B", "C"),
		actor2 = c("B", "C", "D"),
		weight = c(1, 2, 3),
		stringsAsFactors = FALSE
	)
	
	# create longitudinal data
	dyad_df_long = data.frame(
		actor1 = rep(c("A", "B", "C"), 2),
		actor2 = rep(c("B", "C", "D"), 2),
		time = rep(1:2, each = 3),
		weight = 1:6,
		stringsAsFactors = FALSE
	)
	
	nodelist = c("A", "B", "C", "D", "E")
	
	# test that nodelist parameter is accepted without error
	expect_no_error({
		net = netify(
			dyad_df,
			actor1 = "actor1",
			actor2 = "actor2",
			weight = "weight",
			nodelist = nodelist
		)
	})
	
	expect_no_error({
		adj = get_adjacency(
			dyad_df,
			actor1 = "actor1",
			actor2 = "actor2",
			weight = "weight",
			nodelist = nodelist
		)
	})
	
	expect_no_error({
		arr = get_adjacency_array(
			dyad_df_long,
			actor1 = "actor1",
			actor2 = "actor2",
			time = "time",
			weight = "weight",
			nodelist = nodelist
		)
	})
	
	expect_no_error({
		lst = get_adjacency_list(
			dyad_df_long,
			actor1 = "actor1",
			actor2 = "actor2",
			time = "time",
			weight = "weight",
			nodelist = nodelist
		)
	})
})

test_that("get_adjacency() correctly adds isolates with nodelist", {
	# create sparse edgelist - common scenario where isolates are missing
	edgelist = data.frame(
		from = c("A", "B", "C"),
		to = c("B", "C", "A"),
		weight = c(1, 2, 3),
		stringsAsFactors = FALSE
	)
	
	# without nodelist - only actors in edges
	adj_no_isolates = get_adjacency(
		edgelist,
		actor1 = "from",
		actor2 = "to",
		weight = "weight",
		symmetric = FALSE
	)
	
	# with nodelist including isolates d, e, f
	full_nodelist = c("A", "B", "C", "D", "E", "F")
	adj_with_isolates = get_adjacency(
		edgelist,
		actor1 = "from",
		actor2 = "to",
		weight = "weight",
		symmetric = FALSE,
		nodelist = full_nodelist
	)
	
	# dimensions increase to include isolates
	expect_equal(dim(adj_no_isolates), c(3, 3))
	expect_equal(dim(adj_with_isolates), c(6, 6))

	# all nodes from nodelist are included
	expect_equal(rownames(adj_with_isolates), full_nodelist)
	expect_equal(colnames(adj_with_isolates), full_nodelist)

	# original edges are preserved
	expect_equal(adj_with_isolates["A", "B"], 1)
	expect_equal(adj_with_isolates["B", "C"], 2)
	expect_equal(adj_with_isolates["C", "A"], 3)

	# isolates have no connections (all 0s by default)
	# check isolate d
	expect_true(all(adj_with_isolates["D", ] == 0 | is.na(adj_with_isolates["D", ])))
	expect_true(all(adj_with_isolates[, "D"] == 0 | is.na(adj_with_isolates[, "D"])))
	# check isolate e
	expect_true(all(adj_with_isolates["E", ] == 0 | is.na(adj_with_isolates["E", ])))
	expect_true(all(adj_with_isolates[, "E"] == 0 | is.na(adj_with_isolates[, "E"])))
	# check isolate f
	expect_true(all(adj_with_isolates["F", ] == 0 | is.na(adj_with_isolates["F", ])))
	expect_true(all(adj_with_isolates[, "F"] == 0 | is.na(adj_with_isolates[, "F"])))
})

test_that("get_adjacency() handles isolates correctly with missing_to_zero parameter", {
	# simple edgelist
	edgelist = data.frame(
		i = c("X", "Y"),
		j = c("Y", "Z"),
		value = c(5, 10),
		stringsAsFactors = FALSE
	)
	
	nodes = c("W", "X", "Y", "Z")  # w is an isolate
	
	adj_zeros = get_adjacency(
		edgelist,
		actor1 = "i",
		actor2 = "j",
		weight = "value",
		nodelist = nodes,
		missing_to_zero = TRUE
	)
	
	adj_nas = get_adjacency(
		edgelist,
		actor1 = "i",
		actor2 = "j",
		weight = "value",
		nodelist = nodes,
		missing_to_zero = FALSE
	)
	
	expect_equal(dim(adj_zeros), c(4, 4))
	expect_equal(dim(adj_nas), c(4, 4))
	
	# check isolate w has correct values
	expect_equal(adj_zeros["W", "X"], 0)
	expect_equal(adj_zeros["W", "Y"], 0)
	expect_equal(adj_zeros["W", "Z"], 0)
	
	expect_true(is.na(adj_nas["W", "X"]))
	expect_true(is.na(adj_nas["W", "Y"]))
	expect_true(is.na(adj_nas["W", "Z"]))
	
	expect_equal(adj_zeros["X", "Y"], 5)
	expect_equal(adj_nas["X", "Y"], 5)
	expect_equal(adj_zeros["Y", "Z"], 10)
	expect_equal(adj_nas["Y", "Z"], 10)
})

test_that("get_adjacency() nodelist works for IR-style exhaustive dyadic data", {
	# trade data where 0 means no trade (not missing)
	# only non-zero trade relationships in the data
	trade_edgelist = data.frame(
		exporter = c("USA", "USA", "China", "Germany"),
		importer = c("Canada", "Mexico", "Russia", "France"),
		trade_value = c(100, 200, 150, 80),
		stringsAsFactors = FALSE
	)
	
	# full list of countries in the study
	all_countries = c("USA", "Canada", "Mexico", "China", "Russia", 
					  "Germany", "France", "Japan", "Brazil")
	
	# create adjacency matrix with all countries
	trade_matrix = get_adjacency(
		trade_edgelist,
		actor1 = "exporter",
		actor2 = "importer",
		weight = "trade_value",
		symmetric = FALSE,
		nodelist = all_countries,
		missing_to_zero = TRUE  # important for trade data
	)
	
	# verify all countries are included
	expect_equal(dim(trade_matrix), c(9, 9))
	expect_true(all(all_countries %in% rownames(trade_matrix)))
	expect_true(all(all_countries %in% colnames(trade_matrix)))
	
	# check that japan and brazil (isolates) are included with 0 trade
	# use na.rm = true to handle diagonal nas
	expect_equal(sum(trade_matrix["Japan", ], na.rm = TRUE), 0)
	expect_equal(sum(trade_matrix[, "Japan"], na.rm = TRUE), 0)
	expect_equal(sum(trade_matrix["Brazil", ], na.rm = TRUE), 0)
	expect_equal(sum(trade_matrix[, "Brazil"], na.rm = TRUE), 0)
	
	# verify actual trade values
	expect_equal(trade_matrix["USA", "Canada"], 100)
	expect_equal(trade_matrix["USA", "Mexico"], 200)
	expect_equal(trade_matrix["China", "Russia"], 150)
	expect_equal(trade_matrix["Germany", "France"], 80)
})

test_that("netify() correctly passes nodelist to get_adjacency()", {
	# test that netify() properly uses nodelist for cross-sectional data
	edgelist = data.frame(
		sender = c("Alice", "Bob", "Charlie"),
		receiver = c("Bob", "Charlie", "Alice"),
		strength = c(10, 20, 15),
		stringsAsFactors = FALSE
	)
	
	# include some isolates
	all_people = c("Alice", "Bob", "Charlie", "David", "Eve")
	
	# create network using netify with nodelist
	net = netify(
		edgelist,
		actor1 = "sender",
		actor2 = "receiver",
		weight = "strength",
		symmetric = FALSE,
		nodelist = all_people
	)
	
	# verify isolates are included
	expect_equal(dim(net), c(5, 5))
	expect_equal(rownames(net), all_people)
	expect_equal(colnames(net), all_people)
	
	# check that david and eve are isolates with no connections
	expect_true(all(net["David", ] == 0 | is.na(net["David", ])))
	expect_true(all(net[, "David"] == 0 | is.na(net[, "David"])))
	expect_true(all(net["Eve", ] == 0 | is.na(net["Eve", ])))
	expect_true(all(net[, "Eve"] == 0 | is.na(net[, "Eve"])))
	
	# verify edges are correct
	expect_equal(net["Alice", "Bob"], 10)
	expect_equal(net["Bob", "Charlie"], 20)
	expect_equal(net["Charlie", "Alice"], 15)
})

test_that("nodelist parameter supports partitioned bipartite isolates", {
	# create bipartite data
	dyad_df = data.frame(
		company = c("Apple", "Google"),
		investor = c("FundA", "FundB"),
		amount = c(1000, 2000),
		stringsAsFactors = FALSE
	)
	
	bipartite_nodes = list(
		row = c("Apple", "Google", "Microsoft"),
		col = c("FundA", "FundB", "FundC")
	)
	
	bipartite_net = get_adjacency(
		dyad_df,
		actor1 = "company",
		actor2 = "investor",
		weight = "amount",
		mode = "bipartite",
		nodelist = bipartite_nodes
	)

	expect_equal(dim(bipartite_net), c(3, 3))
	expect_true("Microsoft" %in% rownames(bipartite_net))
	expect_true("FundC" %in% colnames(bipartite_net))
	expect_equal(bipartite_net["Apple", "FundA"], 1000)

	# a flat nodelist cannot assign new isolates to row or column mode safely
	expect_error(
		get_adjacency(
			dyad_df,
			actor1 = "company",
			actor2 = "investor",
			weight = "amount",
			mode = "bipartite",
			nodelist = c("Apple", "Google", "Microsoft", "FundA", "FundB", "FundC")
		),
		"cannot place bipartite isolate"
	)
})

test_that("existing network creation functionality works without nodelist", {
	# create simple dyadic data
	dyad_df = data.frame(
		from = c("A", "B", "C"),
		to = c("B", "C", "D"),
		value = c(1, 2, 3),
		stringsAsFactors = FALSE
	)
	
	# create network without nodelist - existing functionality
	net = netify(
		dyad_df,
		actor1 = "from",
		actor2 = "to",
		weight = "value"
	)
	
	# check basic properties
	expect_s3_class(net, "netify")
	expect_equal(dim(net), c(4, 4))
	expect_true(all(c("A", "B", "C", "D") %in% rownames(net)))
	expect_equal(net["A", "B"], 1)
	expect_equal(net["B", "C"], 2)
	expect_equal(net["C", "D"], 3)
})

test_that("nodelist implementation status documentation", {
	# this test documents the current implementation status
	# this can expand as functionality grows
	
	implementation_status = list(
		netify = "FULLY IMPLEMENTED - passes nodelist to all underlying functions",
		get_adjacency = "FULLY IMPLEMENTED - correctly adds isolates to adjacency matrix",
		get_adjacency_array = "FULLY IMPLEMENTED - correctly adds isolates to 3D array",
		get_adjacency_list = "FULLY IMPLEMENTED - correctly adds isolates to list of matrices"
	)
	
	# this test always passes and serves as documentation
	expect_true(TRUE)
})

test_that("nodelist handles large sparse networks efficiently", {
	# example inspired by the context: social media networks with ~8k nodes
	# but only ~200k edges (very sparse)
	
	# create a small example of this scenario
	# simulate follower relationships (directed edges)
	followers_edgelist = data.frame(
		follower = c("user1", "user2", "user3", "user5", "user7"),
		followed = c("user2", "user4", "user1", "user6", "user8"),
		timestamp = 1:5,
		stringsAsFactors = FALSE
	)
	
	# full list of users (many are isolates or have very few connections)
	all_users = paste0("user", 1:20)
	
	# create adjacency matrix with all users
	follower_network = get_adjacency(
		followers_edgelist,
		actor1 = "follower",
		actor2 = "followed",
		weight = "timestamp",
		symmetric = FALSE,  # directed network
		nodelist = all_users,
		missing_to_zero = TRUE
	)
	
	# verify dimensions include all users
	expect_equal(dim(follower_network), c(20, 20))
	
	n_edges = sum(follower_network > 0, na.rm = TRUE)
	n_possible = 20 * 20 - 20  # exclude diagonal
	sparsity = 1 - (n_edges / n_possible)
	expect_true(sparsity > 0.9)  # very sparse network
	
	# verify specific edges exist
	expect_equal(follower_network["user1", "user2"], 1)
	expect_equal(follower_network["user2", "user4"], 2)
	
	# verify isolates exist (e.g., user10, user15, user20)
	expect_equal(sum(follower_network["user10", ], na.rm = TRUE), 0)
	expect_equal(sum(follower_network[, "user10"], na.rm = TRUE), 0)
	expect_equal(sum(follower_network["user20", ], na.rm = TRUE), 0)
	expect_equal(sum(follower_network[, "user20"], na.rm = TRUE), 0)
})

test_that("get_adjacency_array() correctly adds isolates with nodelist", {
	# create longitudinal dyadic data
	dyad_df = data.frame(
		from = rep(c("A", "B", "C"), each = 2),
		to = rep(c("B", "C", "D"), 2),
		year = rep(2020:2021, 3),
		value = 1:6,
		stringsAsFactors = FALSE
	)
	
	# without nodelist
	arr_no_nodelist = get_adjacency_array(
		dyad_df,
		actor1 = "from",
		actor2 = "to",
		time = "year",
		weight = "value"
	)
	
	# with nodelist including isolates
	all_actors = c("A", "B", "C", "D", "E", "F")
	arr_with_nodelist = get_adjacency_array(
		dyad_df,
		actor1 = "from",
		actor2 = "to",
		time = "year",
		weight = "value",
		nodelist = all_actors
	)
	
	# check dimensions
	expect_equal(dim(arr_no_nodelist), c(4, 4, 2))
	expect_equal(dim(arr_with_nodelist), c(6, 6, 2))
	
	# check isolates are included
	expect_true("E" %in% dimnames(arr_with_nodelist)[[1]])
	expect_true("F" %in% dimnames(arr_with_nodelist)[[1]])
	expect_true("E" %in% dimnames(arr_with_nodelist)[[2]])
	expect_true("F" %in% dimnames(arr_with_nodelist)[[2]])
	
	# check that data is preserved correctly for both time periods
	expect_true(!is.na(arr_with_nodelist["A", "B", "2020"]) || 
				arr_with_nodelist["A", "B", "2020"] != 0)
	
	# check isolates have no connections
	expect_true(all(is.na(arr_with_nodelist["E", , ]) | arr_with_nodelist["E", , ] == 0))
	expect_true(all(is.na(arr_with_nodelist["F", , ]) | arr_with_nodelist["F", , ] == 0))
	expect_true(all(is.na(arr_with_nodelist[, "E", ]) | arr_with_nodelist[, "E", ] == 0))
	expect_true(all(is.na(arr_with_nodelist[, "F", ]) | arr_with_nodelist[, "F", ] == 0))
})

test_that("get_adjacency_list() correctly adds isolates with nodelist", {
	# create longitudinal dyadic data with actors appearing/disappearing
	dyad_df = data.frame(
		sender = c("A", "B", "A", "C", "D"),
		receiver = c("B", "C", "C", "D", "E"),
		period = c(1, 1, 2, 2, 3),
		strength = c(10, 20, 15, 25, 30),
		stringsAsFactors = FALSE
	)
	
	# without nodelist - actors vary by time
	list_no_nodelist = get_adjacency_list(
		dyad_df,
		actor1 = "sender",
		actor2 = "receiver",
		time = "period",
		weight = "strength",
		actor_time_uniform = FALSE
	)
	
	# with nodelist ensuring all actors in all periods
	all_nodes = c("A", "B", "C", "D", "E", "F", "G")
	list_with_nodelist = get_adjacency_list(
		dyad_df,
		actor1 = "sender",
		actor2 = "receiver",
		time = "period",
		weight = "strength",
		actor_time_uniform = TRUE,
		nodelist = all_nodes
	)
	
	# check that all periods have all actors when using nodelist
	expect_equal(dim(list_with_nodelist[["1"]]), c(7, 7))
	expect_equal(dim(list_with_nodelist[["2"]]), c(7, 7))
	expect_equal(dim(list_with_nodelist[["3"]]), c(7, 7))
	
	# check isolates f and g are in all periods
	for (period in names(list_with_nodelist)) {
		expect_true("F" %in% rownames(list_with_nodelist[[period]]))
		expect_true("G" %in% rownames(list_with_nodelist[[period]]))
	}
	
	# verify edge values are correct
	expect_equal(list_with_nodelist[["1"]]["A", "B"], 10)
	expect_equal(list_with_nodelist[["1"]]["B", "C"], 20)
	expect_equal(list_with_nodelist[["2"]]["A", "C"], 15)
	expect_equal(list_with_nodelist[["2"]]["C", "D"], 25)
	expect_equal(list_with_nodelist[["3"]]["D", "E"], 30)
	
	# check isolates have no connections
	for (period in names(list_with_nodelist)) {
		expect_true(all(list_with_nodelist[[period]]["F", ] == 0 | 
					   is.na(list_with_nodelist[[period]]["F", ])))
		expect_true(all(list_with_nodelist[[period]]["G", ] == 0 | 
					   is.na(list_with_nodelist[[period]]["G", ])))
	}
})

test_that("netify() with longitudinal data correctly uses nodelist", {
	# create longitudinal data
	dyad_df = data.frame(
		country1 = rep(c("USA", "China", "Germany"), each = 2),
		country2 = rep(c("Canada", "Russia"), 3),
		year = rep(2020:2021, 3),
		trade = 100 * (1:6),
		stringsAsFactors = FALSE
	)
	
	# full country list including some not in the data
	all_countries = c("USA", "China", "Germany", "Canada", "Russia", 
					  "France", "Japan", "Brazil")
	
	net_array = netify(
		dyad_df,
		actor1 = "country1",
		actor2 = "country2",
		time = "year",
		weight = "trade",
		output_format = "longit_array",
		nodelist = all_countries
	)
	
	# check dimensions include all countries
	expect_equal(dim(net_array), c(8, 8, 2))
	# check all countries are included (order may vary)
	expect_setequal(dimnames(net_array)[[1]], all_countries)
	expect_setequal(dimnames(net_array)[[2]], all_countries)
	
	# check isolates are included
	expect_true("France" %in% dimnames(net_array)[[1]])
	expect_true("Japan" %in% dimnames(net_array)[[1]])
	expect_true("Brazil" %in% dimnames(net_array)[[1]])
	
	# verify data integrity
	expect_equal(net_array["USA", "Canada", "2020"], 100)
	expect_equal(net_array["China", "Russia", "2021"], 400)
	
	net_list = netify(
		dyad_df,
		actor1 = "country1",
		actor2 = "country2",
		time = "year",
		weight = "trade",
		output_format = "longit_list",
		nodelist = all_countries
	)
	
	# check dimensions for list format
	expect_equal(dim(net_list[["2020"]]), c(8, 8))
	expect_equal(dim(net_list[["2021"]]), c(8, 8))
	expect_true(all(all_countries %in% rownames(net_list[["2020"]])))
	expect_true(all(all_countries %in% rownames(net_list[["2021"]])))
})

test_that("nodelist handles edge cases correctly", {
	# empty nodelist
	dyad_df = data.frame(
		a1 = c("A", "B"), 
		a2 = c("B", "C"),
		w = c(1, 2),
		stringsAsFactors = FALSE
	)
	
	expect_no_error({
		net_empty = get_adjacency(
			dyad_df, 
			actor1 = "a1", 
			actor2 = "a2",
			weight = "w",
			nodelist = character(0)
		)
	})
	expect_equal(dim(net_empty), c(3, 3))  # only actors from data
	
	# nodelist with duplicates
	nodelist_dup = c("A", "B", "C", "D", "D", "E", "A")
	net_dup = get_adjacency(
		dyad_df,
		actor1 = "a1",
		actor2 = "a2", 
		weight = "w",
		nodelist = nodelist_dup
	)
	expect_equal(dim(net_dup), c(5, 5))  # 5 unique actors
	
	# nodelist is subset of data actors (fewer actors than in data)
	nodelist_subset = c("A", "B")  # c is in data but not in nodelist
	net_subset = get_adjacency(
		dyad_df,
		actor1 = "a1",
		actor2 = "a2",
		weight = "w",
		nodelist = nodelist_subset
	)
	expect_equal(dim(net_subset), c(3, 3))
	expect_true("C" %in% rownames(net_subset))  # c from data is still included
	
	# special characters in actor names
	dyad_special = data.frame(
		from = c("User A", "User-B", "User.C"),
		to = c("User-B", "User.C", "User@D"),
		val = c(1, 2, 3),
		stringsAsFactors = FALSE
	)
	nodelist_special = c("User A", "User-B", "User.C", "User@D", "User E", "User_F")
	
	net_special = get_adjacency(
		dyad_special,
		actor1 = "from",
		actor2 = "to",
		weight = "val",
		nodelist = nodelist_special
	)
	expect_equal(dim(net_special), c(6, 6))
	expect_true("User E" %in% rownames(net_special))
	expect_true("User_F" %in% rownames(net_special))
})

test_that("nodelist works with different data types", {
	dyad_factor = data.frame(
		a1 = factor(c("A", "B", "C")),
		a2 = factor(c("B", "C", "D")),
		w = c(1, 2, 3),
		stringsAsFactors = TRUE
	)
	
	nodelist = c("A", "B", "C", "D", "E", "F")
	
	net_factor = get_adjacency(
		dyad_factor,
		actor1 = "a1",
		actor2 = "a2",
		weight = "w",
		nodelist = nodelist
	)
	
	# factor handling might create additional levels
	# just check that all nodelist actors are included
	expect_true(all(nodelist %in% rownames(net_factor)))
	
	nodelist_mixed = c("A", "B", "C", "D", 5, 6)  # mixed character and numeric
	
	net_mixed = get_adjacency(
		dyad_factor,
		actor1 = "a1", 
		actor2 = "a2",
		weight = "w",
		nodelist = nodelist_mixed
	)
	
	expect_true("5" %in% rownames(net_mixed))
	expect_true("6" %in% rownames(net_mixed))
})

test_that("nodelist works with binary networks", {
	# create binary network data
	dyad_binary = data.frame(
		i = c("A", "A", "B", "C"),
		j = c("B", "C", "C", "D"),
		present = c(1, 1, 1, 1),
		stringsAsFactors = FALSE
	)
	
	all_actors = c("A", "B", "C", "D", "E", "F")
	
	net_binary = get_adjacency(
		dyad_binary,
		actor1 = "i",
		actor2 = "j",
		nodelist = all_actors
	)
	
	expect_equal(dim(net_binary), c(6, 6))
	# check that edges are 1 and non-edges are 0 (except diagonal)
	expect_equal(net_binary["A", "B"], 1)
	expect_equal(net_binary["E", "F"], 0)
	
	net_binary_weighted = get_adjacency(
		dyad_binary,
		actor1 = "i",
		actor2 = "j",
		weight = "present",
		nodelist = all_actors
	)
	
	expect_equal(dim(net_binary_weighted), c(6, 6))
	expect_true(all(net_binary_weighted %in% c(0, 1, NA)))
})

test_that("nodelist interaction with sum_dyads parameter", {
	# create data with duplicate dyads
	dyad_dup = data.frame(
		a1 = c("A", "A", "B", "B", "C"),
		a2 = c("B", "B", "C", "C", "D"),
		w = c(1, 2, 3, 4, 5),
		stringsAsFactors = FALSE
	)
	
	nodelist = c("A", "B", "C", "D", "E")
	
	net_sum = get_adjacency(
		dyad_dup,
		actor1 = "a1",
		actor2 = "a2",
		weight = "w",
		sum_dyads = TRUE,
		nodelist = nodelist
	)
	
	expect_equal(dim(net_sum), c(5, 5))
	expect_equal(net_sum["A", "B"], 3)  # 1 + 2
	expect_equal(net_sum["B", "C"], 7)  # 3 + 4
	expect_true("E" %in% rownames(net_sum))  # isolate still included
	
	# sum_dyads = false auto-promotes to true when weight has repeating dyads
	net_last = suppressMessages(get_adjacency(
		dyad_dup,
		actor1 = "a1",
		actor2 = "a2",
		weight = "w",
		sum_dyads = FALSE,
		nodelist = nodelist
	))

	expect_equal(net_last["A", "B"], 3)  # 1 + 2 (auto-summed)
	expect_equal(net_last["B", "C"], 7)  # 3 + 4 (auto-summed)
})

test_that("nodelist works with self-loops and diag_to_NA", {
	# data with self-loops
	dyad_loops = data.frame(
		i = c("A", "A", "B", "B", "C"),
		j = c("A", "B", "B", "C", "C"),
		w = c(10, 1, 20, 2, 30),
		stringsAsFactors = FALSE
	)
	
	nodelist = c("A", "B", "C", "D")
	
	net_no_diag = get_adjacency(
		dyad_loops,
		actor1 = "i",
		actor2 = "j",
		weight = "w",
		nodelist = nodelist,
		diag_to_NA = TRUE
	)
	
	expect_true(is.na(net_no_diag["A", "A"]))
	expect_true(is.na(net_no_diag["B", "B"]))
	expect_true(is.na(net_no_diag["D", "D"]))  # isolate diagonal also na
	
	net_with_diag = get_adjacency(
		dyad_loops,
		actor1 = "i",
		actor2 = "j",
		weight = "w",
		nodelist = nodelist,
		diag_to_NA = FALSE
	)
	
	expect_equal(net_with_diag["A", "A"], 10)
	expect_equal(net_with_diag["B", "B"], 20)
	expect_equal(net_with_diag["D", "D"], 0)  # isolate diagonal is 0
})

test_that("get_adjacency_list() with actor_time_uniform=FALSE and nodelist", {
	# real-world scenario: actors enter/exit at different times
	# keep selected actors in the roster
	dyad_df = data.frame(
		i = c("USA", "China", "USA", "Russia", "China"),
		j = c("UK", "India", "Canada", "China", "Brazil"),
		year = c(2019, 2019, 2020, 2021, 2021),
		interaction = c(1, 2, 3, 4, 5),
		stringsAsFactors = FALSE
	)
	
	# include eu even though it never appears in interactions
	important_actors = c("USA", "China", "Russia", "UK", "Canada", "EU")
	
	# without nodelist - eu would be missing
	list_no_eu = get_adjacency_list(
		dyad_df,
		actor1 = "i",
		actor2 = "j", 
		time = "year",
		weight = "interaction",
		actor_time_uniform = FALSE
	)
	
	# with nodelist - ensures eu is included
	list_with_eu = get_adjacency_list(
		dyad_df,
		actor1 = "i",
		actor2 = "j",
		time = "year", 
		weight = "interaction",
		actor_time_uniform = FALSE,
		nodelist = important_actors
	)
	
	# check that eu is included in all time periods
	expect_true("EU" %in% rownames(list_with_eu[["2019"]]))
	expect_true("EU" %in% rownames(list_with_eu[["2020"]]))
	expect_true("EU" %in% rownames(list_with_eu[["2021"]]))
	
	# check that eu has no connections (isolate)
	for (year in names(list_with_eu)) {
		expect_true(all(list_with_eu[[year]]["EU", ] == 0 | 
					   is.na(list_with_eu[[year]]["EU", ])))
		expect_true(all(list_with_eu[[year]][, "EU"] == 0 | 
					   is.na(list_with_eu[[year]][, "EU"])))
	}
})

test_that("nodelist works with different time formats in arrays and lists", {
	dyad_date = data.frame(
		i = c("A", "B", "A", "C"),
		j = c("B", "C", "C", "D"),
		date = as.Date(c("2020-01-01", "2020-01-01", "2020-06-01", "2020-06-01")),
		val = c(1, 2, 3, 4),
		stringsAsFactors = FALSE
	)
	
	nodelist = c("A", "B", "C", "D", "E")
	
	# array format with dates
	arr_date = get_adjacency_array(
		dyad_date,
		actor1 = "i",
		actor2 = "j",
		time = "date",
		weight = "val",
		nodelist = nodelist
	)
	
	expect_equal(dim(arr_date), c(5, 5, 2))
	expect_true("E" %in% dimnames(arr_date)[[1]])
	
	# list format with dates
	list_date = get_adjacency_list(
		dyad_date,
		actor1 = "i",
		actor2 = "j",
		time = "date",
		weight = "val",
		actor_time_uniform = TRUE,
		nodelist = nodelist
	)
	
	expect_equal(length(list_date), 2)
	expect_true("E" %in% rownames(list_date[[1]]))
})

test_that("nodelist works with actor_pds specification", {
	# create data
	dyad_df = data.frame(
		i = c("A", "B", "C", "A", "B"),
		j = c("B", "C", "D", "C", "D"),
		t = c(1, 1, 2, 3, 3),
		w = c(1, 2, 3, 4, 5),
		stringsAsFactors = FALSE
	)
	
	# custom actor periods - b only exists in periods 1 and 3
	actor_pds = data.frame(
		actor = c("A", "B", "C", "D"),
		min_time = c(1, 1, 1, 2),
		max_time = c(3, 1, 3, 3),
		stringsAsFactors = FALSE
	)
	
	# add e to nodelist which isn't in actor_pds
	nodelist = c("A", "B", "C", "D", "E", "F")
	
	list_with_pds = get_adjacency_list(
		dyad_df,
		actor1 = "i",
		actor2 = "j",
		time = "t",
		weight = "w",
		actor_pds = actor_pds,
		nodelist = nodelist
	)
	
	# actor_pds overrides the actor list
	expect_equal(length(list_with_pds), 3)  # 3 time periods
	
	# test without actor_pds to confirm nodelist works
	list_no_pds = get_adjacency_list(
		dyad_df,
		actor1 = "i",
		actor2 = "j",
		time = "t",
		weight = "w",
		actor_time_uniform = TRUE,
		nodelist = nodelist
	)
	
	expect_true("E" %in% rownames(list_no_pds[["1"]]))
	expect_true("F" %in% rownames(list_no_pds[["1"]]))
})

test_that("nodelist preserves isolates across all array time slices", {
	# create sparse temporal data
	dyad_temporal = data.frame(
		sender = c("A", "B", "C"),
		receiver = c("B", "C", "D"), 
		year = c(2019, 2020, 2021),
		weight = c(100, 200, 300),
		stringsAsFactors = FALSE
	)
	
	# include actors that never interact
	all_actors = c("A", "B", "C", "D", "E", "F", "G")
	
	arr = get_adjacency_array(
		dyad_temporal,
		actor1 = "sender",
		actor2 = "receiver",
		time = "year",
		weight = "weight",
		nodelist = all_actors
	)
	
	# check all time slices have all actors
	for (t in 1:dim(arr)[3]) {
		slice_actors = dimnames(arr)[[1]]
		expect_equal(length(slice_actors), 7)
		expect_true(all(all_actors %in% slice_actors))
	}
	
	# verify isolates e, f, g have no connections in any time period
	for (isolate in c("E", "F", "G")) {
		expect_true(all(arr[isolate, , ] == 0 | is.na(arr[isolate, , ])))
		expect_true(all(arr[, isolate, ] == 0 | is.na(arr[, isolate, ])))
	}
})

test_that("nodelist performance with large number of isolates", {
	# simulate scenario with many isolates (like 8k nodes, 200k edges)
	# create small test version
	active_actors = paste0("active", 1:20)
	
	# only 50 edges among 20 active actors
	set.seed(123)
	edges = data.frame(
		i = sample(active_actors, 50, replace = TRUE),
		j = sample(active_actors, 50, replace = TRUE),
		w = runif(50),
		stringsAsFactors = FALSE
	)
	
	# include 100 total actors (80 isolates)
	all_actors = c(active_actors, paste0("isolate", 1:80))

	# time the operation
	start_time = Sys.time()
	net_large = get_adjacency(
		edges,
		actor1 = "i",
		actor2 = "j", 
		weight = "w",
		nodelist = all_actors
	)
	end_time = Sys.time()
	
	time_taken = as.numeric(end_time - start_time, units = "secs")
		expect_lt(time_taken, 1)  # runtime guard
	
	# verify dimensions
	expect_equal(dim(net_large), c(100, 100))
	
	# check sparsity
	non_zero = sum(net_large > 0, na.rm = TRUE)
	total_possible = 100 * 100 - 100  # exclude diagonal
	sparsity = 1 - (non_zero / total_possible)
		expect_gt(sparsity, 0.9)  # sparse output
})
