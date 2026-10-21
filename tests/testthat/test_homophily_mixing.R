skip_on_cran()

make_multilayer_longit = function() {
	data(icews, package = "netify", envir = environment())
	icews_sub = icews[icews$year %in% 2010:2011, ]
	make_layer = function(wt) {
		suppressMessages(netify(
			icews_sub,
			actor1 = "i", actor2 = "j", time = "year",
			symmetric = FALSE, weight = wt,
			nodal_vars = c("i_polity2", "i_region"),
			dyad_vars = "matlConf",
			output_format = "longit_list"
		))
	}
	verb = make_layer("verbCoop")
	matl = make_layer("matlCoop")
	list(
		verb = verb,
		matl = matl,
		multi = layer_netify(list(verbCoop = verb, matlCoop = matl))
	)
}

classroom_net = function(symmetric = TRUE) {
	data(classroom_edges, package = "netify", envir = environment())
	data(classroom_nodes, package = "netify", envir = environment())
	suppressMessages(netify(
		classroom_edges,
		actor1 = "from", actor2 = "to",
		symmetric = symmetric,
		nodal_data = classroom_nodes
	))
}

test_that("homophily and mixing_matrix handle multilayer longit_list networks", {
	nets = make_multilayer_longit()
	expect_equal(attr(nets$multi, "netify_type"), "longit_list")

	hom = homophily(nets$multi, attribute = "i_polity2", significance_test = FALSE)
	hom_matl = homophily(nets$matl, attribute = "i_polity2", significance_test = FALSE)
	expect_equal(nrow(hom), 4)
	hom_layer = hom[hom$layer == "matlCoop", ]
	expect_equal(
		hom_layer$homophily_correlation,
		hom_matl$homophily_correlation
	)
	expect_equal(hom_layer$n_connected_pairs, hom_matl$n_connected_pairs)

	mix = mixing_matrix(nets$multi, attribute = "i_region")
	mix_verb = mixing_matrix(nets$verb, attribute = "i_region")
	expect_equal(nrow(mix$summary_stats), 4)
	mix_layer = mix$summary_stats[mix$summary_stats$layer == "verbCoop", ]
	expect_equal(mix_layer$total_ties, mix_verb$summary_stats$total_ties)
	expect_equal(mix_layer$assortativity, mix_verb$summary_stats$assortativity)
})

test_that("attribute_report and dyad_correlation handle multilayer longit_list networks", {
	nets = make_multilayer_longit()

	report = suppressMessages(attribute_report(
		nets$multi,
		node_vars = "i_polity2",
		dyad_vars = "matlConf",
		significance_test = FALSE
	))
	expect_true(is.list(report))
	expect_true(all(c("verbCoop", "matlCoop") %in% report$homophily_analysis$layer))

	dc = dyad_correlation(nets$multi, dyad_vars = "matlConf", significance_test = FALSE)
	dc_own = dc[dc$layer == dc$edge_var, ]
	expect_equal(nrow(dc_own), 4)
	dc_matl = dyad_correlation(nets$matl, dyad_vars = "matlConf", significance_test = FALSE)
	expect_equal(
		dc_own$correlation[dc_own$layer == "matlCoop"],
		dc_matl$correlation
	)
})

test_that("homophily hamming and jaccard accept character attributes", {
	net = classroom_net()

	ham = homophily(net, attribute = "gender", method = "hamming", significance_test = FALSE)
	cat_res = homophily(net, attribute = "gender", method = "categorical", significance_test = FALSE)
	expect_equal(ham$homophily_correlation, cat_res$homophily_correlation)
	expect_equal(ham$n_connected_pairs, cat_res$n_connected_pairs)

	jac = homophily(net, attribute = "gender", method = "jaccard", significance_test = FALSE)
	expect_true(is.finite(jac$homophily_correlation))

	# same result when the binary attribute is supplied as 0/1
	nodes = attr(net, "nodal_data")
	nodes$is_male = as.numeric(nodes$gender == "M")
	attr(net, "nodal_data") = nodes
	jac_num = homophily(net, attribute = "is_male", method = "jaccard", significance_test = FALSE)
	expect_equal(jac$homophily_correlation, jac_num$homophily_correlation)

	# jaccard needs a two-category attribute
	nodes$grade_chr = as.character(nodes$grade)
	attr(net, "nodal_data") = nodes
	expect_error(
		homophily(net, attribute = "grade_chr", method = "jaccard", significance_test = FALSE),
		"two-category"
	)
})

test_that("mixing_matrix counts each undirected tie once and matches igraph", {
	net = classroom_net(symmetric = TRUE)
	n_edges = igraph::ecount(to_igraph(net))

	mm_raw = mixing_matrix(net, attribute = "gender", normalized = FALSE)
	expect_equal(mm_raw$summary_stats$total_ties, n_edges)
	expect_equal(sum(mm_raw$mixing_matrices[[1]]), n_edges)
	expect_true(isSymmetric(mm_raw$mixing_matrices[[1]]))

	mm = mixing_matrix(net, attribute = "gender")
	expect_equal(sum(mm$mixing_matrices[[1]]), 1)
	expect_equal(mm$summary_stats$total_ties, n_edges)

	g = to_igraph(net)
	groups = as.integer(factor(igraph::V(g)$gender))
	expect_equal(
		mm$summary_stats$assortativity,
		igraph::assortativity_nominal(g, groups, directed = FALSE)
	)
	expect_equal(mm$summary_stats$modularity, igraph::modularity(g, groups))

	# cross-attribute tables on undirected networks also count each tie once
	mm_cross = mixing_matrix(net, attribute = "gender", row_attribute = "grade", normalized = FALSE)
	expect_equal(mm_cross$summary_stats$total_ties, n_edges)

	# directed networks count each arc
	dnet = classroom_net(symmetric = FALSE)
	md = mixing_matrix(dnet, attribute = "gender", normalized = FALSE)
	dg = to_igraph(dnet)
	expect_equal(md$summary_stats$total_ties, igraph::ecount(dg))
	expect_equal(
		md$summary_stats$assortativity,
		igraph::assortativity_nominal(dg, as.integer(factor(igraph::V(dg)$gender)), directed = TRUE)
	)
})

test_that("dyad_correlation aligns dyadic matrices to network actors by name", {
	data(icews, package = "netify", envir = environment())
	net = suppressMessages(netify(
		icews[icews$year == 2010, ],
		actor1 = "i", actor2 = "j",
		symmetric = FALSE, weight = "verbCoop",
		dyad_vars = c("matlConf", "verbConf")
	))
	base = dyad_correlation(net, dyad_vars = "matlConf", significance_test = FALSE)

	dd = attr(net, "dyad_data")
	ord = rev(rownames(dd[[1]]$matlConf))
	dd[[1]] = lapply(dd[[1]], function(m) m[ord, ord])
	attr(net, "dyad_data") = dd
	shuffled = dyad_correlation(net, dyad_vars = "matlConf", significance_test = FALSE)
	expect_equal(shuffled$correlation, base$correlation)

	partial_base = dyad_correlation(
		net,
		dyad_vars = "matlConf",
		partial_correlations = TRUE,
		significance_test = FALSE
	)
	expect_true(is.finite(partial_base$correlation))
})
