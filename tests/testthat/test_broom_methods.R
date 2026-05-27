set.seed(6886)

# direct calls to the s3 methods always work; broom dispatch is exercised
# separately when broom is attached

test_that("tidy.netify returns a long edge data frame", {
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net = suppressMessages(netify(
		icews_10, actor1 = "i", actor2 = "j",
		symmetric = FALSE, weight = "verbCoop",
		dyad_vars = "matlCoop"
	))
	td = tidy.netify(net)
	expect_s3_class(td, "data.frame")
	expect_true(all(c("from", "to") %in% names(td)))
	expect_true(nrow(td) > 0)
	# remove_zeros = TRUE by default
	weight_col = attr(net, "weight")
	if (!is.null(weight_col) && weight_col %in% names(td)) {
		expect_true(all(td[[weight_col]] > 0, na.rm = TRUE))
	}
})

test_that("glance.netify returns one row per network/period", {
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net = suppressMessages(netify(
		icews_10, actor1 = "i", actor2 = "j",
		symmetric = FALSE, weight = "verbCoop"
	))
	gl = glance.netify(net)
	expect_s3_class(gl, "data.frame")
	expect_equal(nrow(gl), 1L)
	expect_true("density" %in% names(gl))
})

test_that("tidy / glance handle longitudinal", {
	data(icews)
	suppressMessages(net_l <- netify(
		icews[icews$year %in% 2010:2012, ],
		actor1 = "i", actor2 = "j", time = "year",
		symmetric = FALSE, weight = "verbCoop"
	))
	td = tidy.netify(net_l)
	expect_true("time" %in% names(td))
	expect_true(length(unique(td$time)) == 3)

	gl = glance.netify(net_l)
	expect_equal(nrow(gl), 3L)
})

test_that("broom dispatch works if broom is installed", {
	skip_if_not_installed("broom")
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net = suppressMessages(netify(
		icews_10, actor1 = "i", actor2 = "j",
		symmetric = FALSE, weight = "verbCoop"
	))
	expect_no_error(td <- broom::tidy(net))
	expect_no_error(gl <- broom::glance(net))
	expect_s3_class(td, "data.frame")
	expect_s3_class(gl, "data.frame")
})

test_that("tibble + broom outputs are real tibbles when tibble is installed", {
	skip_on_cran()
	skip_if_not_installed("tibble")
	data(icews)
	icews_10 = icews[icews$year == 2010, ]
	net = suppressMessages(netify(
		icews_10, actor1 = "i", actor2 = "j",
		symmetric = FALSE, weight = "verbCoop"
	))
	expect_s3_class(tibble::as_tibble(net), "tbl_df")
	expect_s3_class(tidy.netify(net), "tbl_df")
	expect_s3_class(glance.netify(net), "tbl_df")
})

test_that("tidy / as_tibble / glance survive a zero-edge netify", {
	# regression: $<- of a length-1 scalar onto a 0-row frame errored out
	# inside finalize_edge_data / decompose_netify
	data(icews)
	icews_10 = icews[icews$year == 2010, ][1:5, ]
	icews_10$verbCoop = 0
	net0 = suppressMessages(netify(
		icews_10, actor1 = "i", actor2 = "j",
		symmetric = FALSE, weight = "verbCoop"
	))
	expect_no_error(td <- tidy.netify(net0))
	expect_equal(nrow(td), 0L)
	# cross-sec tidy() drops the placeholder `time` column
	expect_true(all(c("from", "to", "verbCoop") %in% names(td)))
	expect_false("time" %in% names(td))
	expect_false("from_id" %in% names(td))

	expect_no_error(at <- as.data.frame(net0))
	expect_no_error(gl <- glance.netify(net0))
	expect_equal(nrow(gl), 1L)
})

test_that("as_tibble.netify_comparison returns the pairwise comparisons", {
	skip_on_cran()
	skip_if_not_installed("tibble")
	data(icews)
	n1 = suppressMessages(netify(
		icews[icews$year == 2010, ], actor1 = "i", actor2 = "j",
		symmetric = FALSE, weight = "verbCoop"
	))
	n2 = suppressMessages(netify(
		icews[icews$year == 2011, ], actor1 = "i", actor2 = "j",
		symmetric = FALSE, weight = "verbCoop"
	))
	cmp = suppressMessages(compare_networks(list("2010" = n1, "2011" = n2)))
	tb = tibble::as_tibble(cmp)
	expect_s3_class(tb, "tbl_df")
	expect_true(all(c("net_i", "net_j", "metric", "value") %in% names(tb)))
	expect_true(nrow(tb) >= 1)
})
