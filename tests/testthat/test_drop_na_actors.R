set.seed(6886)

test_that("drop_na_actors: cross-sec, NAs in one column", {
	skip_on_cran()
	# build a small cross-sectional netlet
	actors <- letters[1:6]
	dyads <- expand.grid(actor1 = actors, actor2 = actors,
		stringsAsFactors = FALSE)
	dyads <- dyads[dyads$actor1 != dyads$actor2, ]
	dyads$weight <- rnorm(nrow(dyads))

	# nodal frame with one NA actor in `polity`
	nodes <- data.frame(
		actor = actors,
		polity = c(1, 2, NA, 4, 5, 6),
		gdp = c(10, 20, 30, 40, 50, 60),
		stringsAsFactors = FALSE
	)

	net <- netify(
		dyads,
		actor1 = "actor1", actor2 = "actor2",
		symmetric = FALSE, weight = "weight"
	)
	net <- add_node_vars(net, nodes, "actor", NULL, NULL, FALSE)

	cleaned <- drop_na_actors(net)
	# the NA actor "c" is dropped
	expect_false("c" %in% rownames(get_raw(cleaned)))
	expect_equal(nrow(get_raw(cleaned)), 5)
	# remaining nodal_data is complete in inspected columns
	nd_clean <- attr(cleaned, "nodal_data")
	expect_true(all(stats::complete.cases(nd_clean[, c("polity", "gdp"), drop = FALSE])))
})

test_that("drop_na_actors: cross-sec, cols restricts which NAs matter", {
	skip_on_cran()
	actors <- letters[1:6]
	dyads <- expand.grid(actor1 = actors, actor2 = actors,
		stringsAsFactors = FALSE)
	dyads <- dyads[dyads$actor1 != dyads$actor2, ]
	dyads$weight <- rnorm(nrow(dyads))

	# polity NA on "c"; gdp NA on "e"
	nodes <- data.frame(
		actor = actors,
		polity = c(1, 2, NA, 4, 5, 6),
		gdp = c(10, 20, 30, 40, NA, 60),
		stringsAsFactors = FALSE
	)

	net <- netify(
		dyads,
		actor1 = "actor1", actor2 = "actor2",
		symmetric = FALSE, weight = "weight"
	)
	net <- add_node_vars(net, nodes, "actor", NULL, NULL, FALSE)

	# only polity is checked -> only "c" should be dropped
	cleaned <- drop_na_actors(net, cols = "polity")
	keep <- rownames(get_raw(cleaned))
	expect_true("e" %in% keep)
	expect_false("c" %in% keep)

	# default checks both -> both "c" and "e" dropped
	cleaned_all <- drop_na_actors(net)
	keep_all <- rownames(get_raw(cleaned_all))
	expect_false("c" %in% keep_all)
	expect_false("e" %in% keep_all)
})

test_that("drop_na_actors: longit, NA in any period drops actor everywhere", {
	skip_on_cran()
	actors <- letters[1:4]
	years <- 2010:2012
	dyads <- expand.grid(
		actor1 = actors, actor2 = actors, year = years,
		stringsAsFactors = FALSE
	)
	dyads <- dyads[dyads$actor1 != dyads$actor2, ]
	dyads$weight <- rnorm(nrow(dyads))

	# actor "b" is NA only in 2011; should still be dropped from all periods
	nodes <- expand.grid(actor = actors, time = years,
		stringsAsFactors = FALSE)
	nodes$polity <- 1
	nodes$polity[nodes$actor == "b" & nodes$time == 2011] <- NA

	net <- netify(
		dyads,
		actor1 = "actor1", actor2 = "actor2", time = "year",
		symmetric = FALSE, weight = "weight"
	)
	net <- add_node_vars(net, nodes, "actor", "time", NULL, FALSE)

	cleaned <- drop_na_actors(net, cols = "polity")
	# inspect every period: "b" must be gone
	raw <- get_raw(cleaned)
	all_actors <- unique(unlist(lapply(raw, rownames), use.names = FALSE))
	expect_false("b" %in% all_actors)
	# other actors survive
	expect_true(all(c("a", "c", "d") %in% all_actors))
})

test_that("drop_na_actors: clean netlet returns unchanged", {
	skip_on_cran()
	actors <- letters[1:4]
	dyads <- expand.grid(actor1 = actors, actor2 = actors,
		stringsAsFactors = FALSE)
	dyads <- dyads[dyads$actor1 != dyads$actor2, ]
	dyads$weight <- rnorm(nrow(dyads))

	nodes <- data.frame(
		actor = actors,
		polity = c(1, 2, 3, 4),
		gdp = c(10, 20, 30, 40),
		stringsAsFactors = FALSE
	)

	net <- netify(
		dyads,
		actor1 = "actor1", actor2 = "actor2",
		symmetric = FALSE, weight = "weight"
	)
	net <- add_node_vars(net, nodes, "actor", NULL, NULL, FALSE)

	cleaned <- drop_na_actors(net)
	expect_identical(rownames(get_raw(cleaned)), rownames(get_raw(net)))
	expect_identical(
		attr(cleaned, "nodal_data"),
		attr(net, "nodal_data")
	)
})

test_that("drop_na_actors: missing cols aborts loudly", {
	skip_on_cran()
	actors <- letters[1:4]
	dyads <- expand.grid(actor1 = actors, actor2 = actors,
		stringsAsFactors = FALSE)
	dyads <- dyads[dyads$actor1 != dyads$actor2, ]
	dyads$weight <- rnorm(nrow(dyads))

	nodes <- data.frame(
		actor = actors,
		polity = c(1, NA, 3, 4),
		stringsAsFactors = FALSE
	)
	net <- netify(dyads, actor1 = "actor1", actor2 = "actor2",
		symmetric = FALSE, weight = "weight")
	net <- add_node_vars(net, nodes, "actor", NULL, NULL, FALSE)

	expect_error(
		drop_na_actors(net, cols = c("nope")),
		regexp = "not found in"
	)
})

test_that("drop_na_actors: aborts when every actor would be dropped", {
	skip_on_cran()
	# unipartite: all-NA path must abort, not return empty netlet
	actors <- letters[1:4]
	dyads <- expand.grid(actor1 = actors, actor2 = actors,
		stringsAsFactors = FALSE)
	dyads <- dyads[dyads$actor1 != dyads$actor2, ]
	dyads$weight <- rnorm(nrow(dyads))

	nodes <- data.frame(
		actor = actors,
		polity = rep(NA_real_, 4),
		stringsAsFactors = FALSE
	)
	net <- netify(dyads, actor1 = "actor1", actor2 = "actor2",
		symmetric = FALSE, weight = "weight")
	net <- add_node_vars(net, nodes, "actor", NULL, NULL, FALSE)

	expect_error(
		drop_na_actors(net, cols = "polity"),
		regexp = "would leave the netify empty"
	)
})
