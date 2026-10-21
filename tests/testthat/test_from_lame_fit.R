set.seed(6886)

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------

mk_mock_fit = function(class_tag, draws_slot, n = 6, B = 40,
						family = "binary", link = NULL,
						extra = list()) {
	actors = paste0("a", seq_len(n))
	base   = matrix(rnorm(n * n, 0, 0.5), n, n,
		dimnames = list(actors, actors))
	diag(base) = 0
	# build per-draw array
	arr = array(rnorm(n * n * B, mean = rep(as.vector(base), B), sd = 0.3),
		dim = c(n, n, B),
		dimnames = list(actors, actors, NULL))
	# pick point-estimate slot per class
	pt_slot = if (identical(class_tag, "ame")) "ZpostMean" else "EZ"
	fit = list()
	fit[[pt_slot]] = base
	# attach draws under requested path
	if (identical(draws_slot, c("BOOT", "EZ"))) {
		fit$BOOT = list(EZ = arr)
	} else {
		fit[[draws_slot]] = arr
	}
	fit$family    = family
	fit$mode      = "unipartite"
	fit$symmetric = FALSE
	if (!is.null(link)) fit$link = link
	for (nm in names(extra)) fit[[nm]] = extra[[nm]]
	class(fit) = c(class_tag, "list")
	fit
}

# ---------------------------------------------------------------------------
# from_lame_fit across mock fits
# ---------------------------------------------------------------------------

test_that("from_lame_fit returns fitted matrix from ALS-style bootstrap fit", {
	fit = mk_mock_fit("ame_als", c("BOOT", "EZ"))
	out = from_lame_fit(fit, value = "fitted")
	expect_s3_class(out, "netify")
	expect_equal(dim(get_raw(out)), c(6L, 6L))
})

test_that("from_lame_fit value='prob' applies pnorm for ALS / amen and plogis for lame", {
	# als -> probit
	fa = mk_mock_fit("ame_als", c("BOOT", "EZ"))
	pa = get_raw(from_lame_fit(fa, value = "prob"))
	expect_true(all(pa >= 0 & pa <= 1, na.rm = TRUE))
	z = fa$EZ
	expect_equal(as.vector(pa[upper.tri(pa)]),
		as.vector(stats::pnorm(z[upper.tri(z)])),
		tolerance = 1e-12)

	# lame gibbs -> logit
	fl = mk_mock_fit("lame", "EZps")
	pl = get_raw(from_lame_fit(fl, value = "prob"))
	z2 = fl$EZ
	expect_equal(as.vector(pl[upper.tri(pl)]),
		as.vector(stats::plogis(z2[upper.tri(z2)])),
		tolerance = 1e-12)
})

test_that("from_lame_fit quantile values come from per-draw array and obey lower <= upper", {
	fit = mk_mock_fit("ame", "EZ_draws")
	lo  = get_raw(from_lame_fit(fit, value = "prob_lower", alpha = 0.05))
	hi  = get_raw(from_lame_fit(fit, value = "prob_upper", alpha = 0.05))
	expect_true(all(lo <= hi, na.rm = TRUE))
	expect_true(all(lo >= 0 & hi <= 1, na.rm = TRUE))

	# fitted_lower / fitted_upper on linear-predictor scale
	flo = get_raw(from_lame_fit(fit, value = "fitted_lower", alpha = 0.05))
	fhi = get_raw(from_lame_fit(fit, value = "fitted_upper", alpha = 0.05))
	expect_true(all(flo <= fhi, na.rm = TRUE))
})

test_that("from_lame_fit validates alpha", {
	fit = mk_mock_fit("ame", "EZ_draws")
	expect_error(from_lame_fit(fit, value = "prob_lower", alpha = 0))
	expect_error(from_lame_fit(fit, value = "prob_lower", alpha = 0.5))
	expect_error(from_lame_fit(fit, value = "prob_lower", alpha = NA_real_))
	expect_error(from_lame_fit(fit, value = "prob_lower", alpha = c(0.1, 0.2)))
})

test_that("from_lame_fit aborts when quantile requested but draws missing", {
	fit = list(EZ = matrix(rnorm(16), 4, 4),
		family = "binary", mode = "unipartite")
	expect_error(from_lame_fit(fit, value = "prob_lower"),
		regexp = "per-draw")
})

test_that("from_lame_fit explicit link slot wins over class-based inference", {
	# als class but force logit via explicit slot
	fit = mk_mock_fit("ame_als", c("BOOT", "EZ"), link = "logit")
	p = get_raw(from_lame_fit(fit, value = "prob"))
	z = fit$EZ
	expect_equal(as.vector(p[upper.tri(p)]),
		as.vector(stats::plogis(z[upper.tri(z)])),
		tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# to_lame(lame=true) longit emitter shape
# ---------------------------------------------------------------------------

test_that("to_lame longit emits a single lame::lame call on the per-period lists", {
	df = data.frame(
		i = sample(letters[1:6], 60, TRUE),
		j = sample(letters[1:6], 60, TRUE),
		t = sample(2001:2003, 60, TRUE),
		w = rnorm(60),
		x = runif(60),
		stringsAsFactors = FALSE
	)
	df = df[df$i != df$j, ]

	net_l = suppressWarnings(suppressMessages(
		netify(df, actor1 = "i", actor2 = "j", time = "t",
			weight = "w", symmetric = FALSE,
			actor_time_uniform = FALSE,
			dyad_vars = "x", dyad_vars_symmetric = FALSE)
	))

	nl = suppressMessages(to_lame(net_l, lame = TRUE, pad = TRUE))

	snip = nl$ame_call
	# no manual padding step
	expect_false(grepl("list_to_array", snip, fixed = TRUE))
	# exactly one lame::lame() fit call
	expect_equal(length(gregexpr("lame::lame\\(", snip, perl = TRUE)[[1]]), 1L)
	# the fit call passes the per-period lists
	expect_true(grepl("Y = nl$Y", snip, fixed = TRUE))
	expect_true(grepl("Xdyad = nl$Xdyad", snip, fixed = TRUE))
})

test_that("to_lame longit no-covariate snippet omits X slots", {
	df = data.frame(
		i = sample(letters[1:6], 60, TRUE),
		j = sample(letters[1:6], 60, TRUE),
		t = sample(2001:2003, 60, TRUE),
		w = rnorm(60),
		stringsAsFactors = FALSE
	)
	df = df[df$i != df$j, ]

	net_l = suppressWarnings(suppressMessages(
		netify(df, actor1 = "i", actor2 = "j", time = "t",
			weight = "w", symmetric = FALSE,
			actor_time_uniform = FALSE)
	))
	nl = suppressMessages(to_lame(net_l, lame = TRUE, pad = TRUE))

	snip = nl$ame_call
	expect_false(grepl("Xdyad", snip))
	expect_false(grepl("Xrow",  snip))
	expect_false(grepl("Xcol",  snip))
	expect_true(grepl("lame::lame(Y = nl$Y,", snip, fixed = TRUE))
})

test_that("to_lame longit fit_method = als adds bootstrap and method args", {
	df = data.frame(
		i = sample(letters[1:6], 60, TRUE),
		j = sample(letters[1:6], 60, TRUE),
		t = sample(2001:2003, 60, TRUE),
		w = rnorm(60),
		stringsAsFactors = FALSE
	)
	df = df[df$i != df$j, ]
	net_l = suppressWarnings(suppressMessages(
		netify(df, actor1 = "i", actor2 = "j", time = "t",
			weight = "w", symmetric = FALSE,
			actor_time_uniform = FALSE)
	))
	nl = suppressMessages(to_lame(net_l, lame = TRUE, pad = TRUE,
		fit_method = "als", bootstrap = 200))
	snip = nl$ame_call
	expect_true(grepl('method = "als"', snip, fixed = TRUE))
	expect_true(grepl("bootstrap = 200L", snip, fixed = TRUE))
})

test_that("to_lame bipartite longit passes per-period lists to lame::lame", {
	rows = paste0("firm_", 1:4)
	cols = paste0("event_", 1:3)
	years = 2020:2022
	df = expand.grid(
		actor1 = rows, actor2 = cols, year = years,
		stringsAsFactors = FALSE
	)
	df = df[!(df$year == 2022 & df$actor1 == "firm_4"), ]
	df$y = rbinom(nrow(df), 1, 0.4)
	node_df = expand.grid(
		actor = c(rows, cols), year = years,
		stringsAsFactors = FALSE
	)
	node_df$attr_x = seq_len(nrow(node_df))

	net = netify(
		df, actor1 = "actor1", actor2 = "actor2", time = "year",
		weight = "y", mode = "bipartite", symmetric = FALSE,
		actor_time_uniform = FALSE
	)
	net = add_node_vars(net, node_df, actor = "actor", time = "year")

	nl = suppressMessages(to_lame(net, lame = TRUE, pad = TRUE))
	snip = nl$ame_call
	expect_equal(nl$mode, "bipartite")
	expect_false(grepl("lame::ame(", snip, fixed = TRUE))
	expect_false(grepl("list_to_array", snip, fixed = TRUE))
	expect_true(grepl('mode = "bipartite"', snip, fixed = TRUE))
	expect_true(
		grepl("lame::lame(Y = nl$Y, Xrow = nl$Xrow, Xcol = nl$Xcol", snip, fixed = TRUE)
	)

	# per-period covariate rows line up with the matching Y slice
	for (tt in names(nl$Y)) {
		expect_equal(rownames(nl$Xrow[[tt]]), rownames(nl$Y[[tt]]))
		expect_equal(rownames(nl$Xcol[[tt]]), colnames(nl$Y[[tt]]))
	}
	expect_false("firm_4" %in% rownames(nl$Y[["2022"]]))
})

test_that("to_lame bipartite cross-sectional call includes dyadic covariates", {
	mat = matrix(c(1, 0, 0, 1, 1, 0), 2, 3,
		dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
	x = matrix(seq_len(6), 2, 3,
		dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
	net = new_netify(mat, mode = "bipartite")
	attr(net, "dyad_data") = list("1" = list(x = x))

	nl = suppressMessages(to_lame(net))
	expect_true(grepl("Xdyad = nl$Xdyad", nl$ame_call, fixed = TRUE))
	expect_error(to_lame(net, fit_method = "als", bootstrap = 1.5), "non-negative integer")
})

test_that("from_lame_fit residual uses the same time slice as 3D fitted values", {
	actors = c("a", "b")
	y = array(c(
		0, 1, 0, 0,
		0, 4, 0, 0
	), dim = c(2, 2, 2), dimnames = list(actors, actors, c("t1", "t2")))
	ez = array(c(
		0, 0.25, 0, 0,
		0, 2, 0, 0
	), dim = c(2, 2, 2), dimnames = list(actors, actors, c("t1", "t2")))
	fit = list(Y = y, EZ = ez, family = "normal", mode = "unipartite", symmetric = FALSE)
	class(fit) = c("lame", "list")

	out = suppressMessages(from_lame_fit(fit, value = "residual"))
	resid_mat = get_raw(out)
	expect_equal(resid_mat["b", "a"], 0.75)
})

test_that("to_lame recurses over multilayer netify inputs", {
	actors = c("a", "b", "c")
	df1 = expand.grid(i = actors, j = actors, stringsAsFactors = FALSE)
	df1 = df1[df1$i != df1$j, ]
	df1$y = c(1, 0, 1, 0, 1, 0)
	df2 = df1
	df2$y = 1 - df1$y

	net1 = netify(df1, actor1 = "i", actor2 = "j", weight = "y", symmetric = FALSE)
	net2 = netify(df2, actor1 = "i", actor2 = "j", weight = "y", symmetric = FALSE)
	multi = layer_netify(list(verbal = net1, material = net2))

	out = suppressMessages(to_lame(multi, family = "binary"))
	expect_equal(names(out), c("verbal", "material"))
	expect_equal(out$verbal$mode, "unipartite")
	expect_equal(out$material$family, "binary")
	expect_true(grepl("lame::ame", out$verbal$ame_call, fixed = TRUE))
})

test_that("to_lame longit array snippet splits arrays into per-period lists", {
	actors = letters[1:5]
	df = expand.grid(
		i = actors,
		j = actors,
		t = 2001:2003,
		stringsAsFactors = FALSE
	)
	df = df[df$i != df$j, ]
	df$w = rnorm(nrow(df))
	df$x1 = runif(nrow(df))
	df$x2 = runif(nrow(df))
	df$p = rnorm(nrow(df))

	net = suppressWarnings(suppressMessages(
		netify(
			df,
			actor1 = "i",
			actor2 = "j",
			time = "t",
			weight = "w",
			symmetric = FALSE,
			dyad_vars = c("x1", "x2"),
			dyad_vars_symmetric = c(FALSE, FALSE),
			nodal_vars = "p"
		)
	))
	nl = suppressMessages(to_lame(net))
	snip = nl$ame_call
	expect_true(grepl("lame::lame(Y = Y, Xdyad = Xdyad, Xrow = Xrow, Xcol = Xcol", snip, fixed = TRUE))
	expect_false(grepl("lame::ame(", snip, fixed = TRUE))

	# run everything but the fit call
	snippet_lines = strsplit(snip, "\n", fixed = TRUE)[[1]]
	pre_env = new.env(parent = baseenv())
	pre_env$nl = nl
	eval(parse(text = paste(head(snippet_lines, -1), collapse = "\n")), envir = pre_env)

	Y = get("Y", envir = pre_env)
	Xdyad = get("Xdyad", envir = pre_env)
	Xrow = get("Xrow", envir = pre_env)
	expect_type(Y, "list")
	expect_equal(names(Y), c("2001", "2002", "2003"))
	expect_equal(Y[["2002"]], nl$Y[, , "2002"])
	expect_equal(dim(Xdyad[["2003"]]), c(5L, 5L, 2L))
	expect_equal(dimnames(Xdyad[["2003"]])[[3]], c("x1", "x2"))
	expect_equal(Xdyad[["2003"]][, , "x2"], nl$Xdyad[, , "x2", "2003"])
	expect_true(is.matrix(Xrow[["2001"]]))
	expect_equal(Xrow[["2001"]][, "p"], nl$Xrow[, "p", "2001"])
})

test_that("to_lame generated snippets run against lame", {
	skip_on_cran()
	skip_if_not_installed("lame")
	actors = letters[1:6]
	df = expand.grid(
		i = actors,
		j = actors,
		t = 2001:2003,
		stringsAsFactors = FALSE
	)
	df = df[df$i != df$j, ]
	# drop one actor from the last period
	df = df[!(df$t == 2003 & (df$i == "f" | df$j == "f")), ]
	df$w = rnorm(nrow(df))
	df$x = runif(nrow(df))
	df$p = rnorm(nrow(df))

	net = suppressWarnings(suppressMessages(
		netify(
			df,
			actor1 = "i",
			actor2 = "j",
			time = "t",
			weight = "w",
			symmetric = FALSE,
			actor_time_uniform = FALSE,
			dyad_vars = "x",
			dyad_vars_symmetric = FALSE,
			nodal_vars = "p"
		)
	))
	run_snippet = function(nl) {
		code = gsub(
			"nscan = 1000, burn = 500",
			"nscan = 10, burn = 2, odens = 1, gof = FALSE",
			nl$ame_call,
			fixed = TRUE
		)
		fit_env = new.env()
		fit_env$nl = nl
		suppressWarnings(suppressMessages(
			eval(parse(text = code), envir = fit_env)
		))
	}

	fit_list = run_snippet(suppressMessages(to_lame(net, lame = TRUE)))
	expect_s3_class(fit_list, "lame")
	fit_arr = run_snippet(suppressMessages(to_lame(net, lame = FALSE)))
	expect_s3_class(fit_arr, "lame")

	# fitted values come back as a longitudinal netify
	pred = from_lame_fit(fit_list, value = "fitted")
	expect_s3_class(pred, "netify")
	expect_equal(attr(pred, "netify_type"), "longit_list")
	expect_equal(names(get_raw(pred)), c("2001", "2002", "2003"))
})

test_that("from_lame_fit returns a longitudinal netify for per-period fits", {
	actors = c("a", "b", "c")
	periods = c("t1", "t2")
	ez = list(
		t1 = matrix(1:9, 3, 3, dimnames = list(actors, actors)),
		t2 = matrix(11:14, 2, 2, dimnames = list(c("c", "a"), c("c", "a")))
	)
	y = array(
		100,
		dim = c(3, 3, 2),
		dimnames = list(actors, actors, periods)
	)
	fit = list(
		EZ = ez,
		Y = y,
		family = "normal",
		mode = "unipartite",
		symmetric = FALSE
	)
	class(fit) = c("lame_mock", "list")

	pred = suppressMessages(from_lame_fit(fit, value = "fitted"))
	expect_equal(attr(pred, "netify_type"), "longit_list")
	raw = get_raw(pred)
	expect_equal(names(raw), periods)
	expect_equal(raw[["t2"]]["c", "a"], 13)

	resid = suppressMessages(from_lame_fit(fit, value = "residual"))
	resid_raw = get_raw(resid)
	expect_equal(resid_raw[["t1"]]["b", "a"], 100 - 2)
	expect_equal(resid_raw[["t2"]]["c", "a"], 100 - 13)
})
