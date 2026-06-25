set.seed(6886)

test_that("matrix alignment preserves explicit unknown dyads", {
	m = matrix(NA_real_, 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
	aligned = align_matrices(m, m)

	expect_true(is.na(aligned$mat1["a", "b"]))
	expect_true(is.na(aligned$mat2["b", "a"]))
})

test_that("missing and non-finite time values fail early", {
	df = data.frame(i = c("A", "A"), j = c("B", "B"),
		t = c(NA_real_, NA_real_), w = c(1, 2))

	expect_error(
		netify(df, actor1 = "i", actor2 = "j", time = "t",
			weight = "w", symmetric = FALSE, output_format = "longit_array"),
		"missing values"
	)
	expect_error(
		count_duplicate_dyads(df$i, df$j, df$t),
		"non-missing and finite"
	)
})

test_that("melt returns typed empty data frames for empty longitudinal objects", {
	lst = list(
		"1" = matrix(0, 2, 2, dimnames = list(c("a", "b"), c("a", "b"))),
		"2" = matrix(0, 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
	)
	net = new_netify(lst, diag_to_NA = TRUE)
	out = melt(net)

	expect_s3_class(out, "data.frame")
	expect_equal(nrow(out), 0L)
	expect_equal(names(out), c("Var1", "Var2", "time", "value"))
})

test_that("bipartite metadata and summaries do not treat square cells as self-ties", {
	df = data.frame(row = c("A", "B"), col = c("X", "Y"))
	net = netify(df, actor1 = "row", actor2 = "col", mode = "bipartite")
	s = summary(net)

	expect_false(attr(net, "symmetric"))
	expect_false(attr(net, "diag_to_NA"))
	expect_equal(s$density, 0.5)
	expect_true("competition_col" %in% names(s))

	mat = matrix(1:4, 2, 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
	net2 = new_netify(mat, mode = "bipartite", symmetric = TRUE, diag_to_NA = TRUE)
	expect_false(attr(net2, "symmetric"))
	expect_false(attr(net2, "diag_to_NA"))
	expect_false(anyNA(diag(get_raw(net2))))
})

test_that("square bipartite dyad correlations keep all row-column cells", {
	dyad = matrix(1:4, 2, 2)
	edge = matrix(c(0, 1, 1, 0), 2, 2)
	out = calculate_dyadic_correlation(
		dyad, edge, method = "pearson", remove_diagonal = TRUE,
		significance_test = FALSE, alpha = 0.05,
		partial_correlations = FALSE, all_dyad_data = NULL,
		current_var = "x", is_symmetric = FALSE, is_bipartite = TRUE
	)

	expect_equal(out$n_pairs, 4L)
})

test_that("mixing entropy is non-negative for raw count matrices", {
	mat = matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3,
		dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
	net = new_netify(mat, symmetric = TRUE, diag_to_NA = TRUE)
	nodes = data.frame(actor = c("a", "b", "c"), group = c("g1", "g1", "g2"))
	net = add_node_vars(net, nodes, actor = "actor")

	mix = mixing_matrix(net, attribute = "group", normalized = FALSE)
	expect_gte(mix$summary_stats$entropy, 0)
})

test_that("concentration statistics use observed sums with preserved missingness", {
	mat = matrix(c(
		NA, 1, NA,
		1, NA, 0,
		0, 0, NA
	), 3, 3, byrow = TRUE, dimnames = list(letters[1:3], letters[1:3]))
	net = new_netify(mat, symmetric = FALSE, diag_to_NA = TRUE,
		missing_to_zero = FALSE)
	expect_warning({
		s = summary(net)
	}, NA)

	expect_equal(s$competition_row, 0.5)
})

test_that("from_lame_fit preserves square bipartite fitted cells", {
	fit = list(
		EZ = matrix(1:4, 2, 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))),
		mode = "bipartite",
		symmetric = FALSE
	)
	out = from_lame_fit(fit, value = "fitted")

	expect_false(anyNA(diag(get_raw(out))))
	expect_equal(as.numeric(get_raw(out)), 1:4)
})

test_that("bind_netifies maps final names into attributes and pads bipartite nodal data", {
	m1 = matrix(c(1, 0, 0, 1), 2, 2,
		dimnames = list(c("r1", "r2"), c("c1", "c2")))
	m2 = matrix(c(1, 0, 0, 1), 2, 2,
		dimnames = list(c("r2", "r3"), c("c2", "c3")))
	n1 = new_netify(m1, mode = "bipartite", symmetric = FALSE,
		missing_to_zero = FALSE)
	n2 = new_netify(m2, mode = "bipartite", symmetric = FALSE,
		missing_to_zero = FALSE)
	attr(n1, "nodal_data") = data.frame(
		actor = c("r1", "r2", "c1", "c2"),
		score = c(1, 2, 3, 4)
	)
	attr(n2, "nodal_data") = data.frame(
		actor = c("r2", "r3", "c2", "c3"),
		score = c(5, 6, 7, 8)
	)
	attr(n1, "dyad_data") = list("1" = list(cov = m1))
	attr(n2, "dyad_data") = list("1" = list(cov = m2))

	out = bind_netifies(n1, n2, names = c("t1", "t2"), align_actors = "union")
	am = to_amen(out, lame = TRUE)

	expect_equal(unique(attr(out, "nodal_data")$time), c("t1", "t2"))
	expect_equal(names(attr(out, "dyad_data")), c("t1", "t2"))
	expect_equal(dim(attr(out, "dyad_data")$t1$cov), dim(get_raw(out[[1]])))
	expect_identical(rownames(attr(out, "dyad_data")$t1$cov), rownames(get_raw(out[[1]])))
	expect_identical(colnames(attr(out, "dyad_data")$t1$cov), colnames(get_raw(out[[1]])))
	expect_identical(rownames(am$Xrow[["t1"]]), rownames(am$Y[["t1"]]))
	expect_identical(rownames(am$Xcol[["t1"]]), colnames(am$Y[["t1"]]))
})

test_that("multilayer to_lame infers family per layer unless supplied", {
	actors = c("a", "b", "c")
	bin = matrix(c(NA, 1, 0, 1, NA, 1, 0, 1, NA), 3, 3,
		dimnames = list(actors, actors))
	wt = matrix(c(NA, 2, 0, 3, NA, 4, 0, 5, NA), 3, 3,
		dimnames = list(actors, actors))
	n_bin = new_netify(bin, symmetric = FALSE, weight = NULL)
	n_wt = new_netify(wt, symmetric = FALSE, weight = "w")
	ml = layer_netify(list(weighted = n_wt, binary = n_bin))

	out = suppressMessages(to_lame(ml))
	expect_equal(vapply(out, `[[`, character(1), "family"),
		c(weighted = "normal", binary = "binary"))
})

test_that("model exporters reject non-numeric dyadic covariates", {
	mat = matrix(c(NA, 1, 0, NA), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
	net = new_netify(mat, symmetric = FALSE)
	attr(net, "dyad_data") = list("1" = list(label = matrix("x", 2, 2)))
	expect_error(to_amen(net), "dyadic attributes")

	long = new_netify(list("t1" = mat, "t2" = mat), symmetric = FALSE)
	attr(long, "dyad_data") = list(
		"t1" = list(label = matrix("x", 2, 2)),
		"t2" = list(label = matrix("y", 2, 2))
	)
	expect_error(to_dbn(long), "dyadic attributes")
})

test_that("to_dbn rejects bipartite longitudinal inputs clearly", {
	m = matrix(1, 2, 3, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
	net = new_netify(list("t1" = m, "t2" = m), mode = "bipartite",
		symmetric = FALSE)

	expect_error(to_dbn(net), "unipartite")
})

test_that("single-time longit_array subset keeps time labels in actor_pds", {
	df = data.frame(i = c("a", "b", "a", "b"),
		j = c("b", "a", "b", "a"), year = c(2002, 2002, 2003, 2003),
		w = c(1, 0, 1, 0))
	net = netify(df, actor1 = "i", actor2 = "j", time = "year",
		weight = "w", symmetric = FALSE, output_format = "longit_array")
	one = subset_netify(net, time = "2002")

	expect_equal(unique(attr(one, "actor_pds")$min_time), "2002")
	expect_equal(unique(attr(one, "actor_pds")$max_time), "2002")
})

test_that("QAP p-value denominator uses valid permutations under missingness", {
	m1 = matrix(c(NA, 1, NA, 0, NA, 1, 0, NA, NA), 3, 3)
	m2 = matrix(c(NA, 0, 1, 1, NA, NA, 0, 1, NA), 3, 3)
	out = qap_correlation_cpp(m1, m2, n_permutations = 20)

	expect_true("n_valid_permutations" %in% names(out))
	expect_lte(out$n_valid_permutations, 20)
	expect_gte(out$p_value, 0)
	expect_lte(out$p_value, 1)
})

test_that("seed = NULL advances RNG while explicit seeds restore it", {
	mat = matrix(c(NA, 1, 0, 1, NA, 1, 0, 1, NA), 3, 3,
		dimnames = list(letters[1:3], letters[1:3]))
	net = new_netify(mat, symmetric = FALSE)

	set.seed(42)
	before = .Random.seed
	simulate(net, nsim = 1)
	after_one = .Random.seed
	simulate(net, nsim = 1)
	after_two = .Random.seed
	expect_false(identical(before, after_one))
	expect_false(identical(after_one, after_two))

	set.seed(42)
	before_seeded = .Random.seed
	simulate(net, nsim = 1, seed = 1)
	expect_identical(.Random.seed, before_seeded)
})

test_that("dyad permutation preserves permuted weights and missingness", {
	mat = matrix(c(
		NA, 1, NA,
		2, NA, 0,
		3, 0, NA
	), 3, 3, byrow = TRUE, dimnames = list(letters[1:3], letters[1:3]))
	net = new_netify(mat, symmetric = FALSE, missing_to_zero = FALSE,
		diag_to_NA = TRUE, weight = "w")
	draw = simulate(net, nsim = 1, seed = 1,
		model = "dyad_permutation")[[1]]
	vals = sort(as.numeric(get_raw(draw)[!is.na(get_raw(draw)) &
		get_raw(draw) != 0]))

	expect_equal(vals, c(1, 2, 3))
})

test_that("NA-valued statistic draws warn and report n_valid", {
	mat = matrix(c(NA, 1, 0, 1, NA, 1, 0, 1, NA), 3, 3,
		dimnames = list(letters[1:3], letters[1:3]))
	net = new_netify(mat, symmetric = FALSE)
	counter = new.env(parent = emptyenv())
	counter$i = 0L
	stat = function(x) {
		counter$i = counter$i + 1L
		c(value = if (counter$i %% 2L == 0L) NA_real_ else sum(get_raw(x), na.rm = TRUE))
	}

	expect_warning(
		{
			out = compare_to_null(net, stat, n_sim = 4, seed = 1, verbose = FALSE)
		},
		"n_valid"
	)
	expect_lt(out$n_valid, 4L)
})

test_that("homophily reports analyzed pairs after missing attributes", {
	mat = matrix(c(
		NA, 1, 0, 0,
		1, NA, 1, 0,
		0, 1, NA, 1,
		0, 0, 1, NA
	), 4, 4, byrow = TRUE, dimnames = list(letters[1:4], letters[1:4]))
	net = new_netify(mat, symmetric = TRUE, diag_to_NA = TRUE)
	nodes = data.frame(actor = letters[1:4], group = c("x", "x", "y", NA))
	net = add_node_vars(net, nodes, actor = "actor")
	out = homophily(net, attribute = "group", method = "categorical",
		significance_test = FALSE)

	expect_equal(out$n_pairs, 3)
	expect_error(
		homophily(new_netify(matrix(1, 2, 3), mode = "bipartite"),
			attribute = "group"),
		"unipartite"
	)
})

test_that("dyad_correlation alpha controls Pearson confidence intervals", {
	dyad = matrix(c(NA, 1, 2, 3, NA, 4, 5, 6, NA), 3, 3)
	edge = matrix(c(NA, 2, 1, 5, NA, 3, 7, 4, NA), 3, 3)

	wide = calculate_dyadic_correlation(
		dyad, edge, "pearson", TRUE, TRUE, 0.01,
		FALSE, NULL, "x"
	)
	narrow = calculate_dyadic_correlation(
		dyad, edge, "pearson", TRUE, TRUE, 0.20,
		FALSE, NULL, "x"
	)

	expect_gt(wide$ci_upper - wide$ci_lower,
		narrow$ci_upper - narrow$ci_lower)
})

test_that("bipartite igraph conversion keeps overlapping actor modes distinct", {
	df = data.frame(i = c("A", "B"), j = c("A", "B"), w = c(1, 1))
	net = netify(df, actor1 = "i", actor2 = "j", mode = "bipartite",
		symmetric = FALSE, weight = "w")
	g = netify_to_igraph(net)

	expect_identical(anyDuplicated(igraph::V(g)$name), 0L)
	expect_equal(igraph::vertex_attr(g, "netify_actor"), c("A", "B", "A", "B"))
	expect_equal(igraph::vertex_attr(g, "netify_mode"), c("row", "row", "col", "col"))
	expect_false(any(igraph::as_data_frame(g, what = "edges")$from ==
		igraph::as_data_frame(g, what = "edges")$to))
})

test_that("to_igraph honors cross-sectional attribute opt-out flags", {
	mat = matrix(c(0, 1, 0, 0), 2, 2,
		dimnames = list(c("a", "b"), c("a", "b")))
	net = new_netify(mat, symmetric = FALSE, diag_to_NA = FALSE)
	attr(net, "nodal_data") = data.frame(actor = c("a", "b"), group = c("x", "y"))
	attr(net, "dyad_data") = list("1" = list(
		cov = matrix(c(0, 5, 0, 0), 2, 2,
			dimnames = dimnames(mat))
	))

	g = netify_to_igraph(net, add_nodal_attribs = FALSE, add_dyad_attribs = FALSE)
	expect_false("group" %in% igraph::vertex_attr_names(g))
	expect_false("cov" %in% igraph::edge_attr_names(g))

	g_attr = netify_to_igraph(net)
	expect_true("group" %in% igraph::vertex_attr_names(g_attr))
	expect_true("cov" %in% igraph::edge_attr_names(g_attr))
})

test_that("symmetric density includes self-loops when the diagonal is retained", {
	mat = matrix(0, 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
	mat["a", "a"] = 1
	net = new_netify(mat, symmetric = TRUE, diag_to_NA = FALSE)
	s = summary(net)

	expect_equal(s$num_edges, 1)
	expect_equal(s$density, 1 / 3)
})

test_that("actor-period helpers support character time labels for longit arrays", {
	df = data.frame(
		i = c("A", "B", "C"),
		j = c("B", "C", "A"),
		phase = c("pre", "post", "follow"),
		w = c(1, 1, 1)
	)
	info = get_actor_time_info(df, "i", "j", "phase")
	expect_false(anyNA(info$min_time))
	expect_false(anyNA(info$max_time))

	date_df = df
	date_df$phase = as.Date(c("2020-01-01", "2020-01-03", "2020-01-05"))
	date_info = get_actor_time_info(date_df, "i", "j", "phase")
	expect_s3_class(date_info$min_time, "Date")

	net = netify(df, actor1 = "i", actor2 = "j", time = "phase",
		weight = "w", symmetric = FALSE, output_format = "longit_array")
	nodes = expand.grid(
		actor = unique(c(df$i, df$j)),
		phase = sort(unique(df$phase)),
		KEEP.OUT.ATTRS = FALSE,
		stringsAsFactors = FALSE
	)
	nodes$x = seq_len(nrow(nodes))

	net = add_node_vars(net, nodes, actor = "actor", time = "phase",
		node_vars = "x")
	expect_equal(sort(unique(attr(net, "nodal_data")$time)), sort(unique(df$phase)))
})

test_that("bind_netifies aligns dyadic covariates with realigned actors", {
	m1 = matrix(c(1, 0, 0, 1), 2, 2,
		dimnames = list(c("r1", "r2"), c("c1", "c2")))
	m2 = matrix(c(1, 0, 0, 1), 2, 2,
		dimnames = list(c("r2", "r3"), c("c2", "c3")))
	n1 = new_netify(m1, mode = "bipartite", symmetric = FALSE,
		missing_to_zero = FALSE)
	n2 = new_netify(m2, mode = "bipartite", symmetric = FALSE,
		missing_to_zero = FALSE)
	attr(n1, "dyad_data") = list("1" = list(cov = m1))
	attr(n2, "dyad_data") = list("1" = list(cov = m2))

	out = bind_netifies(n1, n2, names = c("t1", "t2"), align_actors = "union")
	cov_t1 = attr(out, "dyad_data")$t1$cov

	expect_identical(dimnames(cov_t1), dimnames(get_raw(out[[1]])))
	expect_true(is.na(cov_t1["r3", "c3"]))
})

test_that("configuration simulation rejects preserved unknown dyads", {
	mat = matrix(c(
		NA, 1, NA,
		0, NA, 1,
		1, 0, NA
	), 3, 3, byrow = TRUE, dimnames = list(letters[1:3], letters[1:3]))
	net = new_netify(mat, symmetric = FALSE, diag_to_NA = TRUE,
		missing_to_zero = FALSE)

	expect_error(
		simulate(net, nsim = 1, model = "configuration", seed = 1),
		"cannot preserve"
	)
})

test_that("bipartite igraph edge covariates keep rectangular row-column alignment", {
	g = igraph::make_bipartite_graph(c(FALSE, FALSE, TRUE, TRUE), c(1, 3, 2, 4))
	igraph::V(g)$name = c("r1", "r2", "c1", "c2")
	igraph::E(g)$w = c(5, 7)
	igraph::E(g)$kind = c("x", "y")

	net = to_netify(g, weight = "w")
	kind = attr(net, "dyad_data")[["1"]][["kind"]]

	expect_identical(dimnames(kind), list(c("r1", "r2"), c("c1", "c2")))
	expect_equal(kind["r1", "c1"], "x")
	expect_equal(kind["r2", "c2"], "y")
})

test_that("explicit actor_pds accepts character and Date time labels", {
	df = data.frame(i = c("a", "b"), j = c("b", "c"),
		t = c("wave1", "wave2"), y = 1)
	pds = get_actor_time_info(df, "i", "j", "t")
	net = netify(df, actor1 = "i", actor2 = "j", time = "t", weight = "y",
		symmetric = FALSE, actor_time_uniform = FALSE, actor_pds = pds)

	expect_equal(names(net), c("wave1", "wave2"))
	expect_equal(attr(net, "actor_pds")$min_time, pds$min_time)
	expect_equal(attr(net, "actor_pds")$max_time, pds$max_time)

	date_df = df
	date_df$t = as.Date(c("2020-01-01", "2020-01-03"))
	date_pds = get_actor_time_info(date_df, "i", "j", "t")
	date_net = netify(date_df, actor1 = "i", actor2 = "j", time = "t",
		weight = "y", symmetric = FALSE, actor_time_uniform = FALSE,
		actor_pds = date_pds)

	expect_equal(names(date_net), as.character(sort(unique(date_df$t))))
	expect_s3_class(attr(date_net, "actor_pds")$min_time, "Date")
})

test_that("statnet vertex attributes survive netify to igraph conversion", {
	nw = network::network(matrix(c(0, 1, 0, 0), 2, 2), directed = TRUE)
	network::set.vertex.attribute(nw, "group", c("A", "B"))

	ig = to_igraph(to_netify(nw))

	expect_equal(igraph::vertex_attr(ig, "group"), c("A", "B"))
	expect_null(igraph::vertex_attr(ig, "actor"))
})

test_that("factor dyad covariates keep labels", {
	df = data.frame(i = c("a", "b"), j = c("b", "a"), y = 1,
		rel = factor(c("ally", "rival")))
	net = netify(df, actor1 = "i", actor2 = "j", weight = "y",
		symmetric = FALSE)
	net = add_dyad_vars(net, df, "i", "j", dyad_vars = "rel",
		dyad_vars_symmetric = FALSE)

	rel = attr(net, "dyad_data")[["1"]][["rel"]]
	expect_equal(rel["a", "b"], "ally")
	expect_equal(rel["b", "a"], "rival")
})

test_that("dyad_correlation binary_network preserves matrix dimensions and NA ties", {
	mat = matrix(c(NA, 2, 0, 1, NA, 3, 0, 1, NA), 3, 3,
		dimnames = list(letters[1:3], letters[1:3]))
	net = new_netify(mat, symmetric = FALSE, missing_to_zero = FALSE)
	attr(net, "dyad_data") = list("1" = list(cov = matrix(c(
		NA, 1, 0,
		2, NA, 3,
		0, 1, NA
	), 3, 3, byrow = TRUE, dimnames = dimnames(mat))))

	out = dyad_correlation(net, dyad_vars = "cov", binary_network = TRUE,
		significance_test = FALSE)

	expect_equal(nrow(out), 1)
	expect_equal(out$dyad_var, "cov")
	expect_false(is.na(out$correlation))
})

test_that("directed spectral comparison uses documented symmetrization", {
	m1 = matrix(c(0, 1, 0, 0), 2, 2, byrow = TRUE,
		dimnames = list(c("a", "b"), c("a", "b")))
	m2 = t(m1)
	n1 = new_netify(m1, symmetric = FALSE)
	n2 = new_netify(m2, symmetric = FALSE)

	out = compare_networks(list(one = n1, two = n2), method = "spectral")

	expect_equal(out$summary$spectral, 0, tolerance = 1e-8)
})

test_that("edge comparison metrics skip dyads missing in either network", {
	m1 = matrix(c(NA, NA, 1, NA), 2, 2, byrow = TRUE,
		dimnames = list(c("a", "b"), c("a", "b")))
	m2 = matrix(c(NA, 1, 1, NA), 2, 2, byrow = TRUE,
		dimnames = list(c("a", "b"), c("a", "b")))
	n1 = new_netify(m1, symmetric = FALSE, missing_to_zero = FALSE)
	n2 = new_netify(m2, symmetric = FALSE, missing_to_zero = FALSE)

	out = compare_networks(list(one = n1, two = n2), method = "all", test = FALSE)

	expect_equal(out$summary$jaccard, 1)
	expect_equal(out$summary$hamming, 0)
	expect_equal(out$edge_changes$one_vs_two$added, 0)
	expect_equal(out$edge_changes$one_vs_two$removed, 0)
	expect_equal(out$edge_changes$one_vs_two$maintained, 1)
})

test_that("degree-preserving QAP works for symmetric binary networks", {
	mat = matrix(c(
		NA, 1, 0, 1,
		1, NA, 1, 0,
		0, 1, NA, 1,
		1, 0, 1, NA
	), 4, 4, byrow = TRUE,
	dimnames = list(letters[1:4], letters[1:4]))
	n1 = new_netify(mat, symmetric = TRUE)
	n2 = new_netify(mat[c(1, 3, 2, 4), c(1, 3, 2, 4)], symmetric = TRUE)

	out = compare_networks(list(one = n1, two = n2), method = "qap",
		permutation_type = "degree_preserving", n_permutations = 10, seed = 1)

	expect_true(is.list(out$significance_tests))
	expect_equal(out$significance_tests$qap_valid_permutations["one", "two"], 10)
	expect_true(is.finite(out$summary$qap_correlation))
})

test_that("multilayer decompose and unnetify keep layer column", {
	m1 = matrix(c(NA, 1, 0, NA), 2, 2,
		dimnames = list(c("a", "b"), c("a", "b")))
	m2 = matrix(c(NA, 0, 2, NA), 2, 2,
		dimnames = list(c("a", "b"), c("a", "b")))
	n1 = new_netify(m1, symmetric = FALSE)
	n2 = new_netify(m2, symmetric = FALSE)
	ml = layer_netify(list(verbal = n1, material = n2))

	dec = decompose_netify(ml)
	flat = unnetify(ml, remove_zeros = TRUE)

	expect_true("layer" %in% names(dec$edge_data))
	expect_equal(sort(unique(dec$edge_data$layer)), c("material", "verbal"))
	expect_true("layer" %in% names(flat))
	expect_equal(nrow(flat), 2)
})

test_that("bipartite decompose keeps same-index row-column ties", {
	df = data.frame(
		person = c("r1", "r2", "r1", "r2"),
		event = c("c1", "c2", "c2", "c1"),
		w = c(5, 6, 7, 8)
	)
	net = netify(df, actor1 = "person", actor2 = "event",
		mode = "bipartite", symmetric = FALSE, weight = "w")

	edges = decompose_netify(net, remove_zeros = TRUE)$edge_data

	expect_equal(nrow(edges), 4L)
	expect_equal(edges$w[match(paste(df$person, df$event),
		paste(edges$from, edges$to))], df$w)
})

test_that("4D multilayer accessors use time dimension correctly", {
	m1 = matrix(c(NA, 1, 0, NA), 2, 2,
		dimnames = list(c("a", "b"), c("a", "b")))
	m2 = matrix(c(NA, 2, 0, NA), 2, 2,
		dimnames = list(c("a", "b"), c("a", "b")))
	l1 = new_netify(list(t1 = m1, t2 = m1 + 1), symmetric = FALSE)
	l2 = new_netify(list(t1 = m2, t2 = m2 + 1), symmetric = FALSE)
	ml = layer_netify(list(L1 = l1, L2 = l2))

	expect_equal(n_periods(ml), 2L)
	expect_equal(as.matrix(ml, time = "t2", layer = "L2"), m2 + 1)
	expect_equal(extract_matrix(ml, time_index = 2), m1 + 1)
})

test_that("subset_netify preserves attributes for numeric time selections", {
	df = data.frame(
		i = c("a", "b", "a", "b"),
		j = c("b", "a", "b", "a"),
		year = c(2001, 2001, 2002, 2002),
		w = 1:4,
		nv = c(10, 20, 30, 40),
		cov = 5:8
	)
	net = netify(df, actor1 = "i", actor2 = "j", time = "year",
		symmetric = FALSE, weight = "w",
		nodal_vars = "nv", dyad_vars = "cov",
		output_format = "longit_list")

	out = subset_netify(net, time = 2)

	expect_equal(dim(out), c(2L, 2L))
	expect_equal(attr(out, "actor_pds")$min_time, rep("2002", 2))
	expect_equal(attr(out, "nodal_data")$nv, c(30, 40))
	expect_equal(names(attr(out, "dyad_data")), "2002")
	expect_true("cov" %in% names(attr(out, "dyad_data")[[1]]))
})

test_that("subset_netify preserves multilayer dimensions for one-actor subsets", {
	mat = matrix(c(NA, 1, 0, NA), 2, 2,
		dimnames = list(c("A", "B"), c("A", "B")))
	ml = layer_netify(list(
		L1 = new_netify(mat, symmetric = FALSE),
		L2 = new_netify(mat * 2, symmetric = FALSE)
	))

	out = subset_netify(ml, actors = "A", layers = c("L1", "L2"))

	expect_equal(dim(out), c(1L, 1L, 2L))
	expect_equal(attr(out, "layers"), c("L1", "L2"))
})

test_that("bind_netifies preserves multilayer period arrays", {
	m1 = matrix(c(NA, 2, 0, NA), 2, 2,
		dimnames = list(c("a", "b"), c("a", "b")))
	m2 = matrix(c(NA, 0, 3, NA), 2, 2,
		dimnames = list(c("a", "b"), c("a", "b")))
	m1b = m1
	m2b = m2
	m1b["a", "b"] = 4
	m2b["b", "a"] = 5
	ml1 = layer_netify(list(verbal = new_netify(m1, symmetric = FALSE),
		material = new_netify(m2, symmetric = FALSE)))
	ml2 = layer_netify(list(verbal = new_netify(m1b, symmetric = FALSE),
		material = new_netify(m2b, symmetric = FALSE)))

	out = bind_netifies(ml1, ml2, names = c("t1", "t2"))
	dec = decompose_netify(out, remove_zeros = TRUE)

	expect_equal(attr(out, "netify_type"), "longit_list")
	expect_equal(attr(out, "layers"), c("verbal", "material"))
	expect_equal(dim(out[[1]]), c(2, 2, 2))
	expect_true(all(c("time", "layer") %in% names(dec$edge_data)))
	expect_equal(sort(unique(dec$edge_data$time)), c("t1", "t2"))
})

test_that("compare_networks QAP guardrails and diagnostics are explicit", {
	mat1 = matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), 3, 3,
		dimnames = list(letters[1:3], letters[1:3]))
	mat2 = matrix(c(0, 0, 1, 1, 0, 0, 0, 1, 0), 3, 3,
		dimnames = list(letters[1:3], letters[1:3]))
	n1 = new_netify(mat1, symmetric = FALSE, diag_to_NA = FALSE)
	n2 = new_netify(mat2, symmetric = FALSE, diag_to_NA = FALSE)

	expect_error(
		compare_networks(list(a = n1, b = n2), method = "qap",
			correlation_type = "spearman", n_permutations = 5),
		"Pearson"
	)

	comp = compare_networks(list(a = n1, b = n2), method = "qap",
		n_permutations = 5, seed = 1)
	expect_true("qap_valid_permutations" %in% names(comp$significance_tests))
	expect_equal(unname(is.na(diag(comp$significance_tests$qap_pvalues))),
		rep(TRUE, 2))

	mat_inf = mat2
	mat_inf[1, 2] = Inf
	n_inf = new_netify(mat_inf, symmetric = FALSE, diag_to_NA = FALSE)
	expect_error(
		compare_networks(list(a = n1, b = n_inf), method = "correlation"),
		"finite edge weights"
	)
})

test_that("QAP handles missing dyads without collapsing valid permutations", {
	mat1 = matrix(c(
		NA, 1, NA,
		0, NA, 1,
		0, NA, NA
	), 3, 3, byrow = TRUE, dimnames = list(letters[1:3], letters[1:3]))
	mat2 = matrix(c(
		NA, 0, 1,
		1, NA, NA,
		0, 1, NA
	), 3, 3, byrow = TRUE, dimnames = list(letters[1:3], letters[1:3]))
	n1 = new_netify(mat1, symmetric = FALSE, missing_to_zero = FALSE)
	n2 = new_netify(mat2, symmetric = FALSE, missing_to_zero = FALSE)

	out = compare_networks(list(a = n1, b = n2), method = "qap",
		n_permutations = 20, seed = 1)

	expect_equal(out$significance_tests$qap_valid_permutations["a", "b"], 20)
})

test_that("degree-preserving QAP validates swap effort with missing dyads", {
	mat = matrix(c(
		NA, 1, NA,
		0, NA, 1,
		1, 0, NA
	), 3, 3, byrow = TRUE, dimnames = list(letters[1:3], letters[1:3]))
	n1 = new_netify(mat, symmetric = FALSE, missing_to_zero = FALSE)
	n2 = new_netify(mat, symmetric = FALSE, missing_to_zero = FALSE)

	expect_error(
		qap_degree_cpp(get_raw(n1), get_raw(n2), n_permutations = 5,
			swaps_factor = 0),
		"swaps_factor"
	)

	out = compare_networks(list(a = n1, b = n2), method = "qap",
		permutation_type = "degree_preserving", n_permutations = 5,
		seed = 1)
	expect_equal(out$significance_tests$qap_valid_permutations["a", "b"], 5)
})

test_that("partial dyad correlations suppress inference when requested", {
	dyad = matrix(c(
		NA, 1, 2, 3,
		4, NA, 5, 6,
		7, 8, NA, 9,
		10, 11, 12, NA
	), 4, 4, byrow = TRUE)
	edge = matrix(c(
		NA, 2, 1, 4,
		5, NA, 4, 7,
		8, 9, NA, 9,
		11, 10, 13, NA
	), 4, 4, byrow = TRUE)
	control = matrix(c(
		NA, 1, 1, 2,
		2, NA, 2, 3,
		3, 3, NA, 4,
		4, 4, 5, NA
	), 4, 4, byrow = TRUE)

	out = calculate_dyadic_correlation(
		dyad, edge, "pearson", TRUE, FALSE, 0.05,
		TRUE, list(x = dyad, z = control), "x"
	)
	expect_true(is.na(out$p_value))
	expect_true(is.na(out$ci_lower))

	out_sig = calculate_dyadic_correlation(
		dyad, edge, "pearson", TRUE, TRUE, 0.05,
		TRUE, list(x = dyad, z = control), "x"
	)
	expect_false(is.na(out_sig$p_value))
})

test_that("dyadic variable storage preserves large and infinite doubles", {
	df = data.frame(
		i = c("a", "b"),
		j = c("b", "a"),
		w = c(1, 0),
		big = c(2147483648, 0),
		inf = c(Inf, 0)
	)
	net = netify(df, actor1 = "i", actor2 = "j", weight = "w",
		symmetric = FALSE, diag_to_NA = TRUE)
	net = add_dyad_vars(net, df, actor1 = "i", actor2 = "j",
		dyad_vars = c("big", "inf"),
		dyad_vars_symmetric = c(FALSE, FALSE))
	dd = attr(net, "dyad_data")[[1]]

	expect_equal(dd$big["a", "b"], 2147483648)
	expect_true(is.infinite(dd$inf["a", "b"]))
})

test_that("binary metadata ignores missing observed weights", {
	df = data.frame(i = c("a", "b"), j = c("b", "a"), w = c(1, NA_real_))
	net = netify(df, actor1 = "i", actor2 = "j", weight = "w",
		symmetric = FALSE, missing_to_zero = FALSE)

	expect_true(attr(net, "is_binary"))
	checks = validate_netify(net, verbose = FALSE)
	expect_true(checks$is_binary_consistent)

	expect_error(netify(df, actor1 = "i", actor2 = "j", symmetric = NA),
		"symmetric")
	expect_error(netify(df, actor1 = "i", actor2 = "j", missing_to_zero = NA),
		"missing_to_zero")
})

test_that("spectral distance rejects empty matrices without aborting R", {
	empty = matrix(numeric(0), 0, 0)

	expect_error(calculate_spectral_distance_cpp(empty, empty, 0L),
		"Non-empty")
})

test_that("homophily preserves unknown dyads and excludes non-finite attributes", {
	actors = letters[1:3]
	mat = matrix(c(
		NA, 1, NA,
		1, NA, 0,
		NA, 0, NA
	), 3, 3, byrow = TRUE, dimnames = list(actors, actors))
	net = new_netify(mat, symmetric = TRUE, diag_to_NA = TRUE,
		missing_to_zero = FALSE)
	nodes = data.frame(actor = actors, group = c("x", "x", "y"))
	net = add_node_vars(net, nodes, actor = "actor")

	out = homophily(net, attribute = "group", method = "categorical",
		significance_test = FALSE)
	expect_equal(out$n_connected_pairs + out$n_unconnected_pairs, 2)

	nodes$score = c(1, Inf, 2)
	net = add_node_vars(net, nodes, actor = "actor",
		node_vars = "score", replace_existing = TRUE)
	out_num = homophily(net, attribute = "score", method = "correlation",
		significance_test = FALSE)
	expect_equal(out_num$n_missing, 1)
	expect_false(any(is.infinite(unlist(out_num[c(
		"homophily_correlation",
		"mean_similarity_connected",
		"mean_similarity_unconnected",
		"similarity_difference"
	)]))))
})

test_that("inference helpers reject zero-length statistics", {
	actors = letters[1:3]
	mat = matrix(c(
		NA, 1, 0,
		0, NA, 1,
		1, 0, NA
	), 3, 3, byrow = TRUE, dimnames = list(actors, actors))
	net = new_netify(mat, symmetric = FALSE, diag_to_NA = TRUE)

	expect_error(compare_to_null(net, function(x) numeric(0), n_sim = 2,
		verbose = FALSE), "at least one numeric value")
	expect_error(bootstrap_netlet(net, function(x) numeric(0), n_boot = 2,
		verbose = FALSE), "at least one numeric value")
})

test_that("inference helpers reject non-finite observed statistics", {
	actors = letters[1:3]
	mat = matrix(c(
		NA, 1, 0,
		0, NA, 1,
		1, 0, NA
	), 3, 3, byrow = TRUE, dimnames = list(actors, actors))
	net = new_netify(mat, symmetric = FALSE, diag_to_NA = TRUE)

	expect_error(compare_to_null(net, function(x) c(bad = Inf), n_sim = 2,
		verbose = FALSE), "finite numeric values")
	expect_error(bootstrap_netlet(net, function(x) c(bad = NA_real_), n_boot = 2,
		verbose = FALSE), "finite numeric values")
})

test_that("native matrix helpers handle rectangular and empty dimensions", {
	rect = matrix(1:6, 2, 3)
	centered = double_center_cpp(rect)
	expect_equal(dim(centered), c(2L, 3L))
	expect_false(anyNA(centered))

	empty = get_matrix_batch(
		0L, 3L,
		list(character()), list(character()),
		list(integer()), list(integer()), list(numeric()),
		FALSE
	)[[1]]
	expect_equal(dim(empty), c(0L, 3L))

	expect_error(
		get_matrix_batch(
			-1L, 3L,
			list(character()), list(character()),
			list(integer()), list(integer()), list(numeric()),
			FALSE
		),
		"non-negative"
	)
})

test_that("custom statistic failures are not returned as successful NA results", {
	mat = matrix(c(NA, 1, 0, NA), 2, 2,
		dimnames = list(c("a", "b"), c("a", "b")))
	net = new_netify(mat, symmetric = FALSE)

	expect_error(
		compare_networks(list(a = net, b = net), what = "edges",
			other_stats = list(bad = function(x) stop("boom"))),
		"boom"
	)
})

test_that("signed weighted shares and concentration use nonnegative mass", {
	mat = matrix(c(
		NA, 10, -9,
		-9, NA, 0,
		0, 0, NA
	), 3, 3, byrow = TRUE, dimnames = list(letters[1:3], letters[1:3]))
	net = new_netify(mat, symmetric = FALSE, missing_to_zero = FALSE,
		diag_to_NA = TRUE, weight = "w")

	graph_out = summary(net)
	actor_out = summary_actor(net, stats = "fast")

	expect_true(graph_out$competition_row >= 0 && graph_out$competition_row <= 1)
	expect_true(graph_out$competition_col >= 0 && graph_out$competition_col <= 1)
	expect_true(all(actor_out$network_share_out >= 0, na.rm = TRUE))
	expect_true(all(actor_out$network_share_out <= 1, na.rm = TRUE))
	expect_true(all(actor_out$network_share_in >= 0, na.rm = TRUE))
	expect_true(all(actor_out$network_share_in <= 1, na.rm = TRUE))
})

test_that("plotting signed missing-aware networks does not expose layout warnings", {
	mat = matrix(c(
		NA, 1, NA,
		-2, NA, 3,
		0, 0, NA
	), 3, 3, byrow = TRUE, dimnames = list(letters[1:3], letters[1:3]))
	net = new_netify(mat, symmetric = FALSE, missing_to_zero = FALSE,
		diag_to_NA = TRUE, weight = "w")

	expect_warning(ggplot2::ggplot_build(plot(net)), NA)
	expect_warning(ggplot2::ggplot_build(plot(net, style = "heatmap")), NA)
	expect_warning({
		components = plot(net, return_components = TRUE)
	}, NA)
	expect_s3_class(components, "netify_plot_components")
})

test_that("static longitudinal layouts work for unweighted networks", {
	net = new_netify(list(
		t1 = matrix(c(NA, 1, 0, 0, NA, 1, 1, 0, NA), 3, 3,
			dimnames = list(letters[1:3], letters[1:3])),
		t2 = matrix(c(NA, 0, 1, 1, NA, 0, 0, 1, NA), 3, 3,
			dimnames = list(letters[1:3], letters[1:3]))
	), symmetric = FALSE)

	expect_warning({
		p = plot(net, static_actor_positions = TRUE)
	}, NA)
	expect_s3_class(p, "ggplot")
	expect_warning(ggplot2::ggplot_build(p), NA)
})

test_that("bind_netifies handles multilayer actor alignment", {
	mk_layered = function(actors) {
		base = matrix(1, length(actors), length(actors),
			dimnames = list(actors, actors))
		diag(base) = NA
		layer_netify(list(
			a = new_netify(base, symmetric = FALSE, weight = "a"),
			b = new_netify(base * 2, symmetric = FALSE, weight = "b")
		))
	}

	left = mk_layered(c("A", "B", "C"))
	right = mk_layered(c("B", "C", "D"))

	inter = bind_netifies(left, right, names = c("t1", "t2"),
		align_actors = "intersection")
	expect_equal(dim(inter[[1]]), c(2L, 2L, 2L))
	expect_equal(rownames(inter[[1]]), c("B", "C"))

	union = bind_netifies(left, right, names = c("t1", "t2"),
		align_actors = "union")
	expect_equal(dim(union[[1]]), c(4L, 4L, 2L))
	expect_true(attr(union, "actor_time_uniform"))
})

test_that("partitioned bipartite nodelists add isolates with non-uniform actors", {
	df = data.frame(
		row = c("r1", "r2"),
		col = c("c1", "c2"),
		t = c(1, 2),
		w = 1
	)
	net = netify(df, actor1 = "row", actor2 = "col", time = "t",
		weight = "w", mode = "bipartite", actor_time_uniform = FALSE,
		nodelist = list(row = c("r1", "r2", "r3"), col = c("c1", "c2", "c3")))

	expect_s3_class(net, "netify")
	expect_true(all(c("r3", "c3") %in% attr(net, "actor_pds")$actor))
})

test_that("statnet import filters system and weight edge attributes", {
	skip_if_not_installed("network")
	nw = network::network(matrix(c(0, 1, 0, 0), 2, 2, byrow = TRUE),
		directed = TRUE)
	network::set.vertex.attribute(nw, "vertex.names", c("a", "b"))
	network::set.edge.attribute(nw, "w", 5)
	network::set.edge.attribute(nw, "kind", "x")

	net = to_netify(nw, weight = "w")
	expect_equal(names(attr(net, "dyad_data")[[1]]), "kind")
})

test_that("single-layer subsets keep the selected weight label", {
	actors = c("a", "b")
	mat = matrix(c(NA, 1, 0, NA), 2, 2, byrow = TRUE,
		dimnames = list(actors, actors))
	ml = layer_netify(list(
		L1 = new_netify(mat, symmetric = FALSE, weight = "w1"),
		L2 = new_netify(mat * 2, symmetric = FALSE, weight = "w2")
	))

	one = subset_netify(ml, layers = "L1")
	expect_equal(attr(one, "weight"), "w1")
})

test_that("layer_netify preserves colliding dyad variables and pivot handles layers", {
	actors = c("a", "b")
	m = function(value) {
		out = matrix(0, 2, 2, dimnames = list(actors, actors))
		out["a", "b"] = value
		out
	}
	n1 = new_netify(m(1), symmetric = FALSE, weight = "w1")
	n2 = new_netify(m(2), symmetric = FALSE, weight = "w2")
	attr(n1, "dyad_data") = list("1" = list(cov = m(10)))
	attr(n2, "dyad_data") = list("1" = list(cov = m(20)))

	ml = layer_netify(list(L1 = n1, L2 = n2))
	expect_equal(names(attr(ml, "dyad_data")[["1"]]), c("L1_cov", "L2_cov"))
	expect_equal(attr(ml, "dyad_data")[["1"]][["L1_cov"]]["a", "b"], 10)
	expect_equal(attr(ml, "dyad_data")[["1"]][["L2_cov"]]["a", "b"], 20)

	pivoted = expect_warning(
		pivot_dyad_to_network(ml, "L1_cov", network_var_name = "old_network"),
		"every layer"
	)
	expect_equal(pivoted["a", "b", "L1"], 10)
	expect_equal(pivoted["a", "b", "L2"], 10)
	expect_equal(dim(attr(pivoted, "dyad_data")[["1"]][["old_network"]]),
		c(2L, 2L, 2L))
})

test_that("netify front door validates matrix inputs before conversion", {
	mat = matrix(c(NA, 1, 0, NA), 2, 2,
		dimnames = list(c("A", "B"), c("A", "B")))

	expect_error(netify(mat, symmetric = "yes"), "symmetric")
	expect_error(netify(mat, output_format = "not_a_format"), "output_format")
})

test_that("unipartite matrix constructor aligns row and column actor order", {
	mat = matrix(c(NA, 1, 2, NA), 2, 2,
		dimnames = list(c("A", "B"), c("B", "A")))
	net = netify(mat, mode = "unipartite")

	expect_identical(rownames(net), colnames(net))
	expect_equal(colnames(net), c("A", "B"))
})

test_that("summaries and igraph conversion ignore dyad covariates on non-edges", {
	df = data.frame(
		i = c("A", "B"),
		j = c("B", "A"),
		w = c(1, 0),
		cov = c(0, 7)
	)
	net = netify(df, actor1 = "i", actor2 = "j",
		weight = "w", symmetric = FALSE)
	net = add_dyad_vars(net, df, actor1 = "i", actor2 = "j",
		dyad_vars = "cov", dyad_vars_symmetric = FALSE)

	expect_equal(summary(net)$num_edges, 1)
	expect_equal(igraph::ecount(netify_to_igraph(net)), 1)
	expect_s3_class(summary_actor(net), "summary_actor")
})

test_that("graph summaries have plot dispatch for longitudinal networks", {
	df = data.frame(
		i = c("A", "B", "A", "C"),
		j = c("B", "C", "C", "A"),
		year = c(2001, 2001, 2002, 2002),
		w = c(1, 0, 2, 3)
	)
	net = netify(df, actor1 = "i", actor2 = "j", time = "year",
		weight = "w", symmetric = FALSE)
	summ = summary(net)

	expect_s3_class(summ, "summary.netify")
	expect_s3_class(summ, "data.frame")
	expect_error(ggplot2::ggplot_build(plot(summ)), NA)
	expect_error(
		ggplot2::ggplot_build(plot(summ, specific_stats = "density")),
		NA
	)
})

test_that("zero-edge binary actor shares are missing rather than NaN", {
	mat = matrix(c(NA, 0, 0, NA), 2, 2,
		dimnames = list(c("A", "B"), c("A", "B")))
	net = netify(mat, symmetric = FALSE)
	actor_stats = summary_actor(net, stats = "fast")

	expect_false(any(is.nan(actor_stats$network_share_total)))
	expect_true(all(is.na(actor_stats$network_share_total)))
})

test_that("longitudinal arrays and low-cardinality numeric color mappings plot", {
	arr = array(0, dim = c(3, 3, 2),
		dimnames = list(LETTERS[1:3], LETTERS[1:3], c("t1", "t2")))
	arr[1, 2, 1] = 1
	arr[2, 3, 2] = 1
	net_arr = netify(arr, symmetric = FALSE)

	components = plot(net_arr, return_components = TRUE)
	expect_s3_class(components, "netify_plot_components")

	net = netify(matrix(c(NA, 1, 0, 0, NA, 1, 1, 0, NA), 3, 3,
		dimnames = list(LETTERS[1:3], LETTERS[1:3])), symmetric = FALSE)
	net = add_node_vars(net, data.frame(actor = LETTERS[1:3], score = c(1, 1, 2)),
		actor = "actor")
	expect_error(ggplot2::ggplot_build(plot(net, node_color_by = "score")), NA)
})

test_that("automatic nodal and imported edge weights use deterministic rules", {
	df = data.frame(
		i = c("A", "A", "B"),
		j = c("B", "C", "C"),
		w = c(1, 1, 1),
		x = c(1, 3, 5)
	)
	net = netify(df, actor1 = "i", actor2 = "j", weight = "w",
		symmetric = FALSE, nodal_vars = "x")
	node_data = attr(net, "nodal_data")
	expect_equal(node_data$x[node_data$actor == "A"], 2)

	g = igraph::make_graph(c("A", "B", "B", "C"), directed = TRUE)
	g = igraph::set_edge_attr(g, "year", value = c(2001, 2002))
	g = igraph::set_edge_attr(g, "weight", value = c(5, 6))
	expect_equal(attr(to_netify(g), "weight"), "weight")

	expect_error(
		add_node_vars(net, data.frame(actor = c("A", "A"), z = c(1, 2)),
			actor = "actor", node_vars = "z"),
		"conflicting values"
	)
})

test_that("bipartite unnetify and plot keep positional diagonal ties", {
	ties = data.frame(
		person = c("peter", "peter", "peter", "mary", "mary", "mary",
			"sandra", "sandra", "sandra"),
		city = c("nyc", "san francisco", "miami", "san francisco", "miami",
			"brussels", "nyc", "miami", "brussels"),
		tie = 1,
		stringsAsFactors = FALSE
	)
	expected = paste(ties$person, ties$city, sep = " -> ")

	net = netify(ties,
		actor1 = "person", actor2 = "city",
		weight = "tie", mode = "bipartite", symmetric = FALSE)

	expect_equal(attr(net, "mode"), "bipartite")
	expect_false(attr(net, "diag_to_NA"))
	expect_equal(sum(as.matrix(net) != 0, na.rm = TRUE), length(expected))

	dyads = unnetify(net, remove_zeros = TRUE)
	dyad_keys = paste(dyads$from, dyads$to, sep = " -> ")
	expect_setequal(dyad_keys, expected)

	components = plot(net, return_components = TRUE, remove_isolates = FALSE)
	plot_edges = components$net_dfs$edge_data
	plot_keys = paste(plot_edges$from, plot_edges$to, sep = " -> ")
	expect_setequal(plot_keys, expected)
})
