set.seed(6886)

# the longit_array hint uses cli's session-scoped .frequency = "once" throttle,
# so these tests cover behavior that does not depend on throttle state

test_that("netify does NOT emit the hint when actors vary across time", {
	df = data.frame(
		a  = c("x", "y", "x", "y", "z", "z"),
		b  = c("y", "z", "y", "x", "x", "y"),
		yr = c(2020, 2020, 2021, 2021, 2021, 2021)
	)
	msg = capture.output(
		invisible(netify(
			df, actor1 = "a", actor2 = "b", time = "yr",
			weight = NULL, sum_dyads = TRUE, symmetric = FALSE
		)),
		type = "message"
	)
	expect_false(any(grepl("longit_array", msg)))
})

test_that("explicit output_format never triggers the hint", {
	data(icews)
	msg = capture.output(
		invisible(netify(
			icews[icews$year %in% 2010:2012, ],
			actor1 = "i", actor2 = "j", time = "year",
			symmetric = FALSE, weight = "verbCoop",
			output_format = "longit_array"
		)),
		type = "message"
	)
	expect_false(any(grepl("longit_array.*consider", msg)))
})

test_that("default longit_list is still the resulting netify_type", {
	# regression: even when the hint suggests longit_array, the actual
	# default behavior is unchanged — netify still returns longit_list
	# until the user opts in
	data(icews)
	n = suppressMessages(netify(
		icews[icews$year %in% 2010:2012, ],
		actor1 = "i", actor2 = "j", time = "year",
		symmetric = FALSE, weight = "verbCoop"
	))
	expect_equal(attr(n, "netify_type"), "longit_list")
})
