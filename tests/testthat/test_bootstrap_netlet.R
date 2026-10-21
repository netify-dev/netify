skip_on_cran()

offdiag_density = function(x) {
	raw = get_raw(x)
	mats = if (is.list(raw)) raw else list(raw)
	vals = unlist(lapply(mats, function(m) m[row(m) != col(m)]))
	c(density = mean(vals > 0, na.rm = TRUE))
}

test_that("bootstrap_netlet density is centered on the point estimate", {
	set.seed(6886)
	n = 15
	mat = matrix(rbinom(n * n, 1, 0.3), n, n)
	mat[lower.tri(mat)] = t(mat)[lower.tri(mat)]
	diag(mat) = NA
	dimnames(mat) = list(paste0("a", 1:n), paste0("a", 1:n))
	net = suppressMessages(new_netify(mat, symmetric = TRUE))

	out = bootstrap_netlet(net, offdiag_density, n_boot = 400, seed = 1, verbose = FALSE)
	expect_equal(out$mean, out$point, tolerance = 0.02)
	expect_lt(abs(out$mean - out$point), 0.01)
})

test_that("bootstrap_netlet marks pairs of duplicated actors as missing", {
	set.seed(6886)
	n = 10
	mat = matrix(rbinom(n * n, 1, 0.4), n, n)
	diag(mat) = NA
	dimnames(mat) = list(paste0("a", 1:n), paste0("a", 1:n))
	net = suppressMessages(new_netify(mat, symmetric = FALSE))

	n_missing = function(x) {
		raw = get_raw(x)
		c(
			n_na = sum(is.na(raw[row(raw) != col(raw)])),
			n_zero = sum(raw[row(raw) != col(raw)] == 0, na.rm = TRUE)
		)
	}
	out = bootstrap_netlet(net, n_missing, n_boot = 50, seed = 2, verbose = FALSE)
	draws = attr(out, "bootstrap_draws")
	# with n = 10 actors drawn with replacement, duplicates are almost certain
	expect_true(any(draws[, "n_na"] > 0))
})

test_that("bootstrap_netlet resamples longitudinal rosters by actor name", {
	# each row carries its actor's number so resampled rows can be traced back
	make_period = function(ids) {
		k = length(ids)
		m = matrix(rep(ids, times = k), k, k)
		diag(m) = NA
		dimnames(m) = list(paste0("a", ids), paste0("a", ids))
		m
	}
	periods = list(
		"1" = make_period(1:4),
		"2" = make_period(1:6),
		"3" = make_period(3:8)
	)
	net = suppressMessages(new_netify(periods, symmetric = FALSE))

	seen = list()
	capture_fn = function(x) {
		seen[[length(seen) + 1]] <<- x
		setNames(vapply(x, nrow, numeric(1)), paste0("n_", names(x)))
	}
	out = bootstrap_netlet(net, capture_fn, n_boot = 200, seed = 3, verbose = FALSE)

	# expected size is the share of the pooled roster present in each period
	expect_equal(out$mean, c(4, 6, 6), tolerance = 0.15)

	# rows made only of duplicated-actor pairs carry no id and are skipped
	row_ids = function(m) {
		ids = vapply(seq_len(nrow(m)), function(i) {
			vals = m[i, !is.na(m[i, ])]
			if (length(vals) == 0) NA_real_ else vals[1]
		}, numeric(1))
		names(ids) = rownames(m)
		ids[!is.na(ids)]
	}
	in_roster = logical(0)
	same_actor = logical(0)
	for (boot_net in seen[-1]) {
		ids = lapply(boot_net, row_ids)
		# only actors present in a period appear in its resample
		in_roster = c(
			in_roster,
			ids[["1"]] %in% 1:4,
			ids[["2"]] %in% 1:6,
			ids[["3"]] %in% 3:8
		)
		# a resampled label maps to the same actor in every period
		labs = unique(unlist(lapply(ids, names)))
		same_actor = c(same_actor, vapply(labs, function(lab) {
			lab_ids = unlist(lapply(ids, function(v) v[names(v) == lab]))
			length(unique(lab_ids)) == 1
		}, logical(1)))
	}
	expect_true(all(in_roster))
	expect_true(all(same_actor))
})

test_that("bootstrap_netlet is reproducible with a seed", {
	set.seed(6886)
	n = 12
	mat = matrix(rbinom(n * n, 1, 0.3), n, n)
	diag(mat) = NA
	dimnames(mat) = list(paste0("a", 1:n), paste0("a", 1:n))
	net = suppressMessages(new_netify(mat, symmetric = FALSE))

	out1 = bootstrap_netlet(net, offdiag_density, n_boot = 30, seed = 9, verbose = FALSE)
	out2 = bootstrap_netlet(net, offdiag_density, n_boot = 30, seed = 9, verbose = FALSE)
	expect_identical(attr(out1, "bootstrap_draws"), attr(out2, "bootstrap_draws"))
	expect_named(out1, c("metric", "point", "n_valid", "mean", "sd", "lower", "upper"))
})
