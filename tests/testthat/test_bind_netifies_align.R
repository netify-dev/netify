set.seed(6886)

# helper to build a cross-sec netify with a given actor set
mk_xs = function(actors) {
	df = expand.grid(
		i = actors, j = actors,
		stringsAsFactors = FALSE
	)
	df = df[df$i != df$j, ]
	df$w = runif(nrow(df))
	netify(
		df,
		actor1 = "i", actor2 = "j",
		weight = "w", symmetric = FALSE
	)
}

n_left = mk_xs(c("A", "B", "C"))
n_right = mk_xs(c("B", "C", "D"))

test_that("align_actors = 'union' pads each period to the union", {
	out = bind_netifies(
		n_left, n_right,
		align_actors = "union"
	)
	expect_s3_class(out, "netify")
	expect_equal(attr(out, "netify_type"), "longit_list")
	expect_length(out, 2L)

	# both periods now 4x4 over a,b,c,d
	for (k in seq_along(out)) {
		m = get_raw(out[[k]])
		expect_equal(dim(m), c(4L, 4L))
		expect_setequal(rownames(m), c("A", "B", "C", "D"))
		expect_setequal(colnames(m), c("A", "B", "C", "D"))
	}

	m1 = get_raw(out[[1]])
	m2 = get_raw(out[[2]])
	expect_true(all(is.na(m1["D", ])))
	expect_true(all(is.na(m1[, "D"])))
	expect_true(all(is.na(m2["A", ])))
	expect_true(all(is.na(m2[, "A"])))
})

test_that("align_actors = 'intersection' keeps only shared actors", {
	out = bind_netifies(
		n_left, n_right,
		align_actors = "intersection"
	)
	expect_s3_class(out, "netify")
	expect_length(out, 2L)

	for (k in seq_along(out)) {
		m = get_raw(out[[k]])
		expect_equal(dim(m), c(2L, 2L))
		expect_equal(sort(rownames(m)), c("B", "C"))
		expect_equal(sort(colnames(m)), c("B", "C"))
	}
})

test_that("align_actors = 'none' preserves per-input dimensions", {
	out = bind_netifies(
		n_left, n_right,
		align_actors = "none"
	)
	expect_length(out, 2L)
	expect_equal(dim(get_raw(out[[1]])), c(3L, 3L))
	expect_equal(dim(get_raw(out[[2]])), c(3L, 3L))
	expect_setequal(rownames(get_raw(out[[1]])), c("A", "B", "C"))
	expect_setequal(rownames(get_raw(out[[2]])), c("B", "C", "D"))
})

test_that("default align_actors is 'none' (backward compatible)", {
	out_default = bind_netifies(n_left, n_right)
	out_none = bind_netifies(n_left, n_right, align_actors = "none")
	expect_equal(
		lapply(out_default, get_raw),
		lapply(out_none, get_raw)
	)
})

test_that("empty intersection raises a clear error", {
	n_disjoint = mk_xs(c("X", "Y", "Z"))
	expect_error(
		bind_netifies(
			n_left, n_disjoint,
			align_actors = "intersection"
		),
		"empty actor set"
	)
})

test_that("bind_netifies rejects incompatible network invariants", {
	actors = c("A", "B", "C")
	mat = matrix(c(
		0, 1, 0,
		1, 0, 1,
		0, 1, 0
	), 3, 3, byrow = TRUE, dimnames = list(actors, actors))
	sym_net = new_netify(mat, symmetric = TRUE)
	dir_net = new_netify(mat, symmetric = FALSE)
	weighted_net1 = new_netify(mat + 0.5, symmetric = TRUE, weight = "w", layers = "same")
	weighted_net2 = new_netify(mat + 1.5, symmetric = TRUE, weight = "v", layers = "same")

	expect_error(bind_netifies(sym_net, dir_net), "symmetric")
	expect_error(bind_netifies(weighted_net1, weighted_net2), "weight")
})
