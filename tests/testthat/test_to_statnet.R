set.seed(6886)

test_that("netify_to_statnet: unweighted cross-sec, asymmetric", {
	# generate some data
	adjm = matrix(
		as.numeric(sample(0:1, 100, replace = TRUE, prob = c(0.5, .5))),
		ncol = 10
	)
	rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]

	# convert to a statnet network object
	g1 = network::network(
		adjm,
		matrix.type = "adjacency",
		directed = TRUE,
		loops = TRUE
	)

	# convert to dyadic so we can pass into netify
	df = melt_matrix_sparse(adjm, remove_zeros = TRUE, remove_diagonal = FALSE)

	# convert to netify object
	a_matrix = netify(
		df,
		actor1 = "Var1", actor2 = "Var2", symmetric = FALSE,
		weight = NULL,
		diag_to_NA = FALSE
	)

	# convert to a statnet network object
	ng = netify_to_statnet(a_matrix)

	# compare
	expect_identical(ng[, ], g1[, ])
})

test_that("netify_to_statnet: weighted cross-sec, asymmetric", {
	# use network example to generate some data
	adjm = matrix(rnorm(10^2), ncol = 10)
	rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]

	# convert to a statnet network object
	g1 = network::network(
		adjm,
		matrix.type = "adjacency",
		directed = TRUE,
		loops = TRUE,
		names.eval = "value",
		ignore.eval = FALSE
	)

	# convert to dyadic so we can pass into netify
	df = melt_matrix_base(adjm)

	# convert to netify object
	a_matrix = netify(
		df,
		actor1 = "Var1", actor2 = "Var2", symmetric = FALSE,
		weight = "value",
		diag_to_NA = FALSE
	)

	# convert to a statnet network object
	ng = netify_to_statnet(a_matrix)

	# compare top level objects
	expect_identical(ng[, ], g1[, ])
	# compare actual edge weights
	expect_identical(
		network::get.edge.attribute(g1, "value"),
		network::get.edge.attribute(ng, "value")
	)
})

test_that("netify_to_statnet: unweighted cross-sec, symmetric", {
	# use network example to generate some data
	adjm = matrix(rnorm(10^2), ncol = 10)
	adjm = (adjm + t(adjm)) / 2
	adjm[adjm > 0] = 1
	adjm[adjm < 0] = 0
	diag(adjm) = NA
	rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]

	# convert to a statnet network object
	g1 = network::network(
		adjm,
		matrix.type = "adjacency",
		directed = FALSE,
		loops = FALSE,
		names.eval = NULL
	)

	# convert to dyadic so we can pass into netify
	df = melt_matrix_base(adjm)
	df = df[df$Var1 != df$Var2, ]
	df = df[df$value == 1, ]

	# convert to netify object
	a_matrix = netify(
		df,
		actor1 = "Var1", actor2 = "Var2", symmetric = TRUE,
		weight = NULL,
		diag_to_NA = TRUE
	)

	# convert to a statnet network object
	ng = netify_to_statnet(a_matrix)

	# compare
	expect_identical(ng[, ], g1[, ])
})

test_that("netify_to_statnet: weighted cross-sec, symmetric", {
	# use network example to generate some data
	adjm = matrix(rnorm(10^2), ncol = 10)
	adjm = (adjm + t(adjm)) / 2
	diag(adjm) = NA
	rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]

	# convert to a statnet network object
	g1 = network::network(
		adjm,
		matrix.type = "adjacency",
		directed = FALSE,
		loops = FALSE,
		names.eval = "value",
		ignore.eval = FALSE
	)

	# convert to dyadic so we can pass into netify
	df = melt_matrix_base(adjm)
	df = df[df$Var1 != df$Var2, ]

	# convert to netify object
	a_matrix = netify(
		df,
		actor1 = "Var1", actor2 = "Var2", symmetric = TRUE,
		weight = "value",
		diag_to_NA = TRUE
	)

	# convert to a statnet network object
	ng = netify_to_statnet(a_matrix)

	# compare top level objects
	expect_identical(ng[, ], g1[, ])
	# compare actual edge weights
	expect_identical(
		network::get.edge.attribute(g1, "value"),
		network::get.edge.attribute(ng, "value")
	)
})

test_that("netify_to_statnet, bipartite: unweighted cross-sec, asymmetric", {
	# use network example to generate some data
	adjm = matrix(
		as.numeric(sample(0:1, 100, replace = TRUE, prob = c(0.6, .6))),
		ncol = 10
	)
	adjm = adjm[1:5, ]
	rownames(adjm) = letters[1:5]
	colnames(adjm) = letters[(length(letters) - 9):length(letters)]

	# convert to a statnet network object
	g1 = network::network(
		adjm,
		matrix.type = "adjacency",
		directed = TRUE,
		loops = TRUE,
		bipartite = TRUE,
		names.eval = NULL
	)

	# convert to dyadic so we can pass into netify
	df = melt_matrix_sparse(adjm, remove_zeros = TRUE, remove_diagonal = FALSE)

	# convert to netify object
	a_matrix = netify(
		df,
		actor1 = "Var1", actor2 = "Var2", symmetric = FALSE,
		weight = NULL,
		mode = "bipartite"
	)

	# convert to a statnet network object
	ng = netify_to_statnet(a_matrix)

	# compare
	expect_identical(ng[, ], g1[, ])
})

test_that("netify_to_statnet, bipartite: weighted cross-sec, asymmetric", {
	# use network example to generate some data
	adjm = matrix(rnorm(10^2), ncol = 10)
	adjm = adjm[1:5, ]
	rownames(adjm) = letters[1:5]
	colnames(adjm) = letters[(length(letters) - 9):length(letters)]

	# convert to a statnet network object
	g1 = network::network(
		adjm,
		matrix.type = "adjacency",
		directed = TRUE,
		loops = TRUE,
		bipartite = TRUE,
		names.eval = "value",
		ignore.eval = FALSE
	)

	# convert to dyadic so we can pass into netify
	df = melt_matrix_base(adjm)

	# convert to netify object
	a_matrix = netify(
		df,
		actor1 = "Var1", actor2 = "Var2", symmetric = FALSE,
		weight = "value",
		mode = "bipartite"
	)

	# convert to a statnet network object
	ng = netify_to_statnet(a_matrix)

	# compare
	expect_identical(ng[, ], g1[, ])
	# compare actual edge weights
	expect_identical(
		network::get.edge.attribute(g1, "value"),
		network::get.edge.attribute(ng, "value")
	)
})

test_that(
	"netify_to_statnet: weighted cross-sec, dyad and nodal attribs",
	{
		# generate a fake un weighted cross-sec cnet

		# create fake dyad data for cross-sectional case
		fake_dyads = expand.grid(actor1 = letters[1:3], actor2 = letters[1:3])
		fake_dyads$weight = rnorm(nrow(fake_dyads))
		fake_dyads$var2 = rnorm(nrow(fake_dyads))
		fake_dyads$var3 = rnorm(nrow(fake_dyads))
		fake_dyads$var4 = rnorm(nrow(fake_dyads))
		fake_dyads$year = 2312
		fake_dyads$actor1 = as.character(fake_dyads$actor1)
		fake_dyads$actor2 = as.character(fake_dyads$actor2)
		fake_dyads = fake_dyads[fake_dyads$actor1 != fake_dyads$actor2, ]

		# add in random binary variable to fake_dyads
		fake_dyads$dv = rbinom(nrow(fake_dyads), 1, 0.5)

		# create fake node data for cross-sectional case
		fake_nodes = data.frame(actor1 = letters[1:3], var1 = rnorm(3), var2 = rnorm(3))

		# convert to conflictNet object
		a_matrix = netify(
			fake_dyads,
			actor1 = "actor1", actor2 = "actor2", symmetric = FALSE,
			weight = "weight",
			diag_to_NA = TRUE
		)

		# add dyad variables in fake data as a dyadic attribute
		a_matrix = add_dyad_vars(
			a_matrix, fake_dyads,
			"actor1", "actor2", NULL,
			c("var2", "var3", "var4"),
			c(FALSE, FALSE, FALSE, FALSE)
		)
		# add node variables in fake data
		a_matrix = add_node_vars(
			a_matrix, fake_nodes,
			"actor1", NULL, NULL, FALSE
		)

		# gen network versions
		prepped_n = netify_to_statnet(a_matrix)

		# check original variable
		expect_identical(
			get_raw(a_matrix),
			network::get.network.attribute(prepped_n, "weight")
		)

		# check dyadic variables
		d_var_l = lapply(2:4, function(i) {
			# access individual matrices from list-of-lists structure
			dvar = attributes(a_matrix)$dyad_data[["1"]][[paste0("var", i)]]
			diag(dvar) = 0
			return(dvar)
		})
		expect_identical(
			d_var_l[[1]],
			network::get.network.attribute(prepped_n, "var2")
		)
		expect_identical(
			d_var_l[[2]],
			network::get.network.attribute(prepped_n, "var3")
		)
		expect_identical(
			d_var_l[[3]],
			network::get.network.attribute(prepped_n, "var4")
		)

		# check nodal variables
		expect_identical(
			attributes(a_matrix)$nodal_data$var1,
			network::get.vertex.attribute(prepped_n, "var1")
		)
		expect_identical(
			attributes(a_matrix)$nodal_data$var2,
			network::get.vertex.attribute(prepped_n, "var2")
		)
	}
)

test_that(
	"netify_to_statnet, bipartite: weighted cross-sec, dyad and nodal attribs",
	{
		# generate a fake un weighted cross-sec cnet

		# create fake dyad data for cross-sectional case
		ar = letters[1:3]
		nr = length(ar)
		ac = letters[22:26]
		nc = length(ac)
		fake_dyads = expand.grid(actor1 = ar, actor2 = ac)
		fake_dyads$weight = rnorm(nrow(fake_dyads))
		fake_dyads$var2 = rnorm(nrow(fake_dyads))
		fake_dyads$var3 = rnorm(nrow(fake_dyads))
		fake_dyads$var4 = rnorm(nrow(fake_dyads))
		fake_dyads$year = 2312
		fake_dyads$actor1 = as.character(fake_dyads$actor1)
		fake_dyads$actor2 = as.character(fake_dyads$actor2)
		fake_dyads = fake_dyads[fake_dyads$actor1 != fake_dyads$actor2, ]

		# add in random binary variable to fake_dyads
		fake_dyads$dv = rbinom(nrow(fake_dyads), 1, 0.5)

		# create fake node data for cross-sectional case
		actors = c(ar, ac)
		nactors = nr + nc
		fake_nodes = data.frame(actor1 = actors, var1 = rnorm(nactors), var2 = rnorm(nactors))

		# convert to conflictNet object
		a_matrix = netify(
			fake_dyads,
			actor1 = "actor1", actor2 = "actor2",
			weight = "weight",
			mode = "bipartite"
		)

		# add dyad variables in fake data as a dyadic attribute
		a_matrix = add_dyad_vars(
			a_matrix, fake_dyads,
			"actor1", "actor2", NULL,
			c("var2", "var3", "var4"),
			c(FALSE, FALSE, FALSE, FALSE)
		)
		# add node variables in fake data
		a_matrix = add_node_vars(
			a_matrix, fake_nodes,
			"actor1", NULL, NULL, FALSE
		)

		# gen network versions
		prepped_n = netify_to_statnet(a_matrix)

		# check original variable
		expect_identical(
			get_raw(a_matrix),
			network::get.network.attribute(prepped_n, "weight")
		)

		# check dyadic variables
		d_var_l = lapply(2:4, function(i) {
			# access individual matrices from list-of-lists structure
			dvar = attributes(a_matrix)$dyad_data[["1"]][[paste0("var", i)]]
			return(dvar)
		})
		expect_identical(
			d_var_l[[1]],
			network::get.network.attribute(prepped_n, "var2")
		)
		expect_identical(
			d_var_l[[2]],
			network::get.network.attribute(prepped_n, "var3")
		)
		expect_identical(
			d_var_l[[3]],
			network::get.network.attribute(prepped_n, "var4")
		)

		# check nodal variables
		expect_identical(
			attributes(a_matrix)$nodal_data$var1,
			network::get.vertex.attribute(prepped_n, "var1")
		)
		expect_identical(
			attributes(a_matrix)$nodal_data$var2,
			network::get.vertex.attribute(prepped_n, "var2")
		)
	}
)
