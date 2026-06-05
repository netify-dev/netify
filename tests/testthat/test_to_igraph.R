set.seed(6886)

melt_test_matrix = function(mat, remove_zeros = FALSE, remove_diagonal = FALSE, na_rm = FALSE) {
	out = as.data.frame(as.table(mat), stringsAsFactors = FALSE)
	names(out) = c("Var1", "Var2", "value")
	if (remove_diagonal) {
		out = out[out$Var1 != out$Var2, ]
	}
	if (remove_zeros) {
		out = out[!is.na(out$value) & out$value != 0, ]
	} else if (na_rm) {
		out = out[!is.na(out$value), ]
	}
	rownames(out) = NULL
	out
}

melt_matrix_base = function(mat) {
	melt_test_matrix(mat)
}

melt_matrix_sparse = function(mat, remove_zeros = TRUE, remove_diagonal = TRUE) {
	melt_test_matrix(mat,
		remove_zeros = remove_zeros,
		remove_diagonal = remove_diagonal,
		na_rm = TRUE
	)
}

test_that("netify_to_igraph: unweighted cross-sec, asymmetric", {
	# use igraph example to generate some data
	adjm = matrix(
		as.numeric(sample(0:1, 100, replace = TRUE, prob = c(0.5, .5))),
		ncol = 10
	)
	rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]
	g1 = igraph::graph_from_adjacency_matrix(adjm)

	# convert to dyadic so we can pass into netify
	df = melt_matrix_sparse(adjm, remove_zeros = TRUE, remove_diagonal = FALSE)

	# convert to netify object
	a_matrix = netify(
		df,
		actor1 = "Var1", actor2 = "Var2", symmetric = FALSE,
		weight = NULL,
		diag_to_NA = FALSE
	)

	# convert to igraph object
	ng = netify_to_igraph(a_matrix)

	# compare
	expect_identical(ng[, ], g1[, ])
})

test_that("to_igraph warns when non-diagonal NAs are replaced", {
	mat = matrix(c(NA, 1, NA, NA), 2, 2,
		dimnames = list(c("a", "b"), c("a", "b")))
	net = new_netify(mat, symmetric = FALSE, diag_to_NA = TRUE, missing_to_zero = FALSE)

	expect_warning(
		to_igraph(net),
		"Replacing 1 non-diagonal NA value with 0"
	)
})

test_that("netify_to_igraph: weighted cross-sec, asymmetric", {
	# use igraph example to generate some data
	adjm = matrix(rnorm(10^2), ncol = 10)
	rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]
	g1 = igraph::graph_from_adjacency_matrix(adjm, weighted = TRUE)

	# convert to dyadic so we can pass into netify
	df = melt_matrix_base(adjm)

	# convert to netify object
	a_matrix = netify(
		df,
		actor1 = "Var1", actor2 = "Var2", symmetric = FALSE,
		weight = "value",
		diag_to_NA = FALSE
	)

	# convert to igraph object
	ng = netify_to_igraph(a_matrix)

	# compare
	expect_identical(ng[, ], g1[, ])
})

test_that("netify_to_igraph: unweighted cross-sec, symmetric", {
	# use igraph example to generate some data
	adjm = matrix(rnorm(10^2), ncol = 10)
	adjm = (adjm + t(adjm)) / 2
	adjm[adjm > 0] = 1
	adjm[adjm < 0] = 0
	diag(adjm) = NA
	rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]
	adjm_clean = adjm
	adjm_clean[is.na(adjm_clean)] = 0
	g1 = igraph::graph_from_adjacency_matrix(adjm_clean, diag = FALSE)

	# convert to dyadic so we can pass into netify
	df = melt_matrix_sparse(adjm, remove_zeros = FALSE, remove_diagonal = TRUE)
	df = df[df$value == 1, ]

	# convert to netify object
	a_matrix = netify(
		df,
		actor1 = "Var1", actor2 = "Var2", symmetric = TRUE,
		weight = NULL,
		diag_to_NA = TRUE
	)

	# convert to igraph object
	ng = netify_to_igraph(a_matrix)

	# compare
	expect_identical(ng[, ], g1[, ])
})

test_that("netify_to_igraph: weighted cross-sec, symmetric", {
	# use igraph example to generate some data
	adjm = matrix(rnorm(10^2), ncol = 10)
	adjm = (adjm + t(adjm)) / 2
	diag(adjm) = NA
	rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]
	adjm_clean = adjm
	adjm_clean[is.na(adjm_clean)] = 0
	g1 = igraph::graph_from_adjacency_matrix(adjm_clean, weighted = TRUE, diag = FALSE)

	# convert to dyadic so we can pass into netify
	df = melt_matrix_sparse(adjm, remove_zeros = FALSE, remove_diagonal = TRUE)

	# convert to netify object
	a_matrix = netify(
		df,
		actor1 = "Var1", actor2 = "Var2", symmetric = TRUE,
		weight = "value",
		diag_to_NA = TRUE
	)

	# convert to igraph object
	ng = netify_to_igraph(a_matrix)

	# compare
	expect_identical(ng[, ], g1[, ])
})

test_that("netify_to_igraph, bipartite: unweighted cross-sec, asymmetric", {
	# use igraph example to generate some data
	adjm = matrix(
		as.numeric(sample(0:1, 100, replace = TRUE, prob = c(0.5, .5))),
		ncol = 10
	)
	adjm = adjm[1:5, ]
	rownames(adjm) = letters[1:5]
	colnames(adjm) = letters[(length(letters) - 9):length(letters)]

	# create igraph object that we'll compare with
	g1 = igraph::graph_from_biadjacency_matrix(adjm)
	g1_raw = data.matrix(g1[, ])
	vtype = igraph::V(g1)$type
	g1_raw = g1_raw[!vtype, vtype]

	# convert to dyadic so we can pass into netify
	df = melt_matrix_sparse(adjm, remove_zeros = TRUE, remove_diagonal = FALSE)

	# convert to netify object
	a_matrix = netify(
		df,
		actor1 = "Var1", actor2 = "Var2", symmetric = FALSE,
		weight = NULL,
		mode = "bipartite"
	)

	# convert to igraph object
	ng = netify_to_igraph(a_matrix)
	ng_raw = data.matrix(ng[, ])
	vtype = igraph::V(ng)$type
	ng_raw = ng_raw[!vtype, vtype]

	# compare
	expect_identical(ng_raw, g1_raw)
})

test_that("netify_to_igraph, bipartite: weighted cross-sec, asymmetric", {
	# use igraph example to generate some data
	adjm = matrix(rnorm(10^2), ncol = 10)
	adjm = adjm[1:5, ]
	rownames(adjm) = letters[1:5]
	colnames(adjm) = letters[(length(letters) - 9):length(letters)]

	# create igraph object that we'll compare with
	g1 = igraph::graph_from_biadjacency_matrix(adjm, weighted = TRUE)
	g1_raw = data.matrix(g1[, ])
	vtype = igraph::V(g1)$type
	g1_raw = g1_raw[!vtype, vtype]

	# convert to dyadic so we can pass into netify
	df = melt_matrix_base(adjm)

	# convert to netify object
	a_matrix = netify(
		df,
		actor1 = "Var1", actor2 = "Var2", symmetric = FALSE,
		weight = "value",
		mode = "bipartite"
	)

	# convert to igraph object
	ng = netify_to_igraph(a_matrix)
	ng_raw = data.matrix(ng[, ])
	vtype = igraph::V(ng)$type
	ng_raw = ng_raw[!vtype, vtype]

	# compare
	expect_identical(ng_raw, g1_raw)
})

test_that(
	"netify_to_igraph: weighted cross-sec, dyad and nodal attribs",
	{
		fake_dyads = expand.grid(actor1 = letters[1:3], actor2 = letters[1:3])
		fake_dyads$weight = rnorm(nrow(fake_dyads))
		fake_dyads$var2 = rnorm(nrow(fake_dyads))
		fake_dyads$var3 = rnorm(nrow(fake_dyads))
		fake_dyads$var4 = rnorm(nrow(fake_dyads))
		fake_dyads$year = 2312
		fake_dyads$actor1 = as.character(fake_dyads$actor1)
		fake_dyads$actor2 = as.character(fake_dyads$actor2)
		fake_dyads = fake_dyads[fake_dyads$actor1 != fake_dyads$actor2, ]

		fake_nodes = data.frame(actor1 = letters[1:3], var1 = rnorm(3), var2 = rnorm(3))

		# convert to netify object
		a_matrix = netify(
			fake_dyads,
			actor1 = "actor1", actor2 = "actor2", symmetric = FALSE,
			weight = "weight",
			diag_to_NA = FALSE
		)

		# add dyad variables in fake data as a dyadic attribute
			a_matrix = add_dyad_vars(
				a_matrix, fake_dyads,
				"actor1", "actor2", NULL,
				c("var2", "var3", "var4"),
				c(FALSE, FALSE, FALSE)
			)
		# add node variables in fake data
		a_matrix = add_node_vars(
			a_matrix, fake_nodes,
			"actor1", NULL, NULL
		)

		# gen igraph versions
		prepped_g = netify_to_igraph(a_matrix)
		g = igraph::graph_from_data_frame(fake_dyads, directed = TRUE, vertices = fake_nodes)

		# check nodes
		n_names = igraph::V(g)$name
		np_names = igraph::V(prepped_g)$name
		nv1 = igraph::V(g)$var1
		names(nv1) = n_names
		nv2 = igraph::V(g)$var2
		names(nv2) = n_names
		npv1 = igraph::V(prepped_g)$var1
		names(npv1) = np_names
		npv2 = igraph::V(prepped_g)$var2
		names(npv2) = np_names
		expect_identical(npv1[n_names], nv1)
		expect_identical(npv2[n_names], nv2)

		# check edges
		# extract edge names
		gvecs = attributes(igraph::E(g))$vnames
		pgvecs = attributes(igraph::E(prepped_g))$vnames

		# pull out edge vars
		pvar2 = igraph::E(prepped_g)$var2
		names(pvar2) = pgvecs
		pvar3 = igraph::E(prepped_g)$var3
		names(pvar3) = pgvecs
		pvar4 = igraph::E(prepped_g)$var4
		names(pvar4) = pgvecs
		var2 = igraph::E(g)$var2
		names(var2) = gvecs
		var3 = igraph::E(g)$var3
		names(var3) = gvecs
		var4 = igraph::E(g)$var4
		names(var4) = gvecs

		# check identical
		expect_identical(pvar2[names(var2)], var2)
		expect_identical(pvar3[names(var3)], var3)
		expect_identical(pvar4[names(var4)], var4)
	}
)

test_that(
	"netify_to_igraph: unweighted cross-sec, dyad and nodal attribs",
	{
		fake_dyads = expand.grid(actor1 = letters[1:3], actor2 = letters[1:3])
		fake_dyads$var1 = rnorm(nrow(fake_dyads))
		fake_dyads$var2 = rnorm(nrow(fake_dyads))
		fake_dyads$var3 = rnorm(nrow(fake_dyads))
		fake_dyads$var4 = rnorm(nrow(fake_dyads))
		fake_dyads$year = 2312
		fake_dyads$actor1 = as.character(fake_dyads$actor1)
		fake_dyads$actor2 = as.character(fake_dyads$actor2)
		fake_dyads = fake_dyads[fake_dyads$actor1 != fake_dyads$actor2, ]
		fake_dyads$dv = rbinom(nrow(fake_dyads), 1, 0.6)
		fake_dyads = fake_dyads[fake_dyads$dv == 1, ]

		fake_nodes = data.frame(actor1 = letters[1:3], var1 = rnorm(3), var2 = rnorm(3))

		# convert to netify object
		a_matrix = netify(
			fake_dyads,
			actor1 = "actor1", actor2 = "actor2", symmetric = FALSE,
			weight = NULL,
			diag_to_NA = FALSE
		)

		# add dyad variables in fake data as a dyadic attribute
		a_matrix = add_dyad_vars(
			a_matrix, fake_dyads,
			"actor1", "actor2", NULL,
			c("var1", "var2", "var3", "var4"),
			c(FALSE, FALSE, FALSE, FALSE)
		)
		# add node variables in fake data
		a_matrix = add_node_vars(
			a_matrix, fake_nodes,
			"actor1", NULL, NULL
		)

		# gen igraph versions
		prepped_g = netify_to_igraph(a_matrix)
		g = igraph::graph_from_data_frame(fake_dyads, directed = TRUE, vertices = fake_nodes)

		# check nodes
		n_names = igraph::V(g)$name
		np_names = igraph::V(prepped_g)$name
		nv1 = igraph::V(g)$var1
		names(nv1) = n_names
		nv2 = igraph::V(g)$var2
		names(nv2) = n_names
		npv1 = igraph::V(prepped_g)$var1
		names(npv1) = np_names
		npv2 = igraph::V(prepped_g)$var2
		names(npv2) = np_names
		expect_identical(npv1[n_names], nv1)
		expect_identical(npv2[n_names], nv2)

		# check edges
		# extract edge names
		gvecs = attributes(igraph::E(g))$vnames
		pgvecs = attributes(igraph::E(prepped_g))$vnames

		# pull out edge vars
		pvar1 = igraph::E(prepped_g)$var1
		names(pvar1) = pgvecs
		pvar2 = igraph::E(prepped_g)$var2
		names(pvar2) = pgvecs
		pvar3 = igraph::E(prepped_g)$var3
		names(pvar3) = pgvecs
		pvar4 = igraph::E(prepped_g)$var4
		names(pvar4) = pgvecs
		var1 = igraph::E(g)$var1
		names(var1) = gvecs
		var2 = igraph::E(g)$var2
		names(var2) = gvecs
		var3 = igraph::E(g)$var3
		names(var3) = gvecs
		var4 = igraph::E(g)$var4
		names(var4) = gvecs

		# check identical
		expect_identical(pvar1[names(var1)], var1)
		expect_identical(pvar2[names(var2)], var2)
		expect_identical(pvar3[names(var3)], var3)
		expect_identical(pvar4[names(var4)], var4)
	}
)


test_that(
	"netify_to_igraph, bipartite: weighted cross-sec, dyad and nodal attribs",
	{
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

		actors = c(ar, ac)
		nactors = nr + nc
		fake_nodes = data.frame(
			actor1 = actors, var1 = rnorm(nactors), var2 = rnorm(nactors)
		)

		# convert to netify object
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
			c(FALSE, FALSE, FALSE)
		)

		# add node variables in fake data
		a_matrix = add_node_vars(
			a_matrix, fake_nodes,
			"actor1", NULL, NULL
		)

		# gen igraph versions
		prepped_g = netify_to_igraph(a_matrix)

		# from raw data
		g = igraph::graph_from_data_frame(fake_dyads, directed = TRUE, vertices = fake_nodes)

		# check nodes
		n_names = igraph::V(g)$name
		np_names = igraph::V(prepped_g)$name
		nv1 = igraph::V(g)$var1
		names(nv1) = n_names
		nv2 = igraph::V(g)$var2
		names(nv2) = n_names
		npv1 = igraph::V(prepped_g)$var1
		names(npv1) = np_names
		npv2 = igraph::V(prepped_g)$var2
		names(npv2) = np_names
		expect_identical(npv1[n_names], nv1)
		expect_identical(npv2[n_names], nv2)

		# check edges
		# extract edge names
		gvecs = attributes(igraph::E(g))$vnames
		pgvecs = attributes(igraph::E(prepped_g))$vnames

		# pull out edge vars
		pvar2 = igraph::E(prepped_g)$var2
		names(pvar2) = pgvecs
		pvar3 = igraph::E(prepped_g)$var3
		names(pvar3) = pgvecs
		pvar4 = igraph::E(prepped_g)$var4
		names(pvar4) = pgvecs
		var2 = igraph::E(g)$var2
		names(var2) = gvecs
		var3 = igraph::E(g)$var3
		names(var3) = gvecs
		var4 = igraph::E(g)$var4
		names(var4) = gvecs

		# check identical
		expect_identical(pvar2[names(var2)], var2)
		expect_identical(pvar3[names(var3)], var3)
		expect_identical(pvar4[names(var4)], var4)
	}
)
