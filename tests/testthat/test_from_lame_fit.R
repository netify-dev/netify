set.seed(6886)

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------

mk_mock_fit <- function(class_tag, draws_slot, n = 6, B = 40,
						family = "binary", link = NULL,
						extra = list()) {
	actors <- paste0("a", seq_len(n))
	base   <- matrix(rnorm(n * n, 0, 0.5), n, n,
		dimnames = list(actors, actors))
	diag(base) <- 0
	# build per-draw array
	arr <- array(rnorm(n * n * B, mean = rep(as.vector(base), B), sd = 0.3),
		dim = c(n, n, B),
		dimnames = list(actors, actors, NULL))
	# pick point-estimate slot per class
	pt_slot <- if (identical(class_tag, "ame")) "ZpostMean" else "EZ"
	fit <- list()
	fit[[pt_slot]] <- base
	# attach draws under requested path
	if (identical(draws_slot, c("BOOT", "EZ"))) {
		fit$BOOT <- list(EZ = arr)
	} else {
		fit[[draws_slot]] <- arr
	}
	fit$family    <- family
	fit$mode      <- "unipartite"
	fit$symmetric <- FALSE
	if (!is.null(link)) fit$link <- link
	for (nm in names(extra)) fit[[nm]] <- extra[[nm]]
	class(fit) <- c(class_tag, "list")
	fit
}

# ---------------------------------------------------------------------------
# from_lame_fit across mock fits
# ---------------------------------------------------------------------------

test_that("from_lame_fit returns fitted matrix from ALS-style bootstrap fit", {
	fit <- mk_mock_fit("ame_als", c("BOOT", "EZ"))
	out <- from_lame_fit(fit, value = "fitted")
	expect_s3_class(out, "netify")
	expect_equal(dim(get_raw(out)), c(6L, 6L))
})

test_that("from_lame_fit value='prob' applies pnorm for ALS / amen and plogis for lame", {
	# ALS -> probit
	fa <- mk_mock_fit("ame_als", c("BOOT", "EZ"))
	pa <- get_raw(from_lame_fit(fa, value = "prob"))
	expect_true(all(pa >= 0 & pa <= 1, na.rm = TRUE))
	# fitted vals should equal pnorm(EZ) on the diagonal-NA-stripped cells
	z <- fa$EZ
	expect_equal(as.vector(pa[upper.tri(pa)]),
		as.vector(stats::pnorm(z[upper.tri(z)])),
		tolerance = 1e-12)

	# lame Gibbs -> logit
	fl <- mk_mock_fit("lame", "EZps")
	pl <- get_raw(from_lame_fit(fl, value = "prob"))
	z2 <- fl$EZ
	expect_equal(as.vector(pl[upper.tri(pl)]),
		as.vector(stats::plogis(z2[upper.tri(z2)])),
		tolerance = 1e-12)
})

test_that("from_lame_fit quantile values come from per-draw array and obey lower <= upper", {
	fit <- mk_mock_fit("ame", "EZ_draws")
	lo  <- get_raw(from_lame_fit(fit, value = "prob_lower", alpha = 0.05))
	hi  <- get_raw(from_lame_fit(fit, value = "prob_upper", alpha = 0.05))
	expect_true(all(lo <= hi, na.rm = TRUE))
	expect_true(all(lo >= 0 & hi <= 1, na.rm = TRUE))

	# fitted_lower / fitted_upper on linear-predictor scale
	flo <- get_raw(from_lame_fit(fit, value = "fitted_lower", alpha = 0.05))
	fhi <- get_raw(from_lame_fit(fit, value = "fitted_upper", alpha = 0.05))
	expect_true(all(flo <= fhi, na.rm = TRUE))
})

test_that("from_lame_fit validates alpha", {
	fit <- mk_mock_fit("ame", "EZ_draws")
	expect_error(from_lame_fit(fit, value = "prob_lower", alpha = 0))
	expect_error(from_lame_fit(fit, value = "prob_lower", alpha = 0.5))
	expect_error(from_lame_fit(fit, value = "prob_lower", alpha = NA_real_))
	expect_error(from_lame_fit(fit, value = "prob_lower", alpha = c(0.1, 0.2)))
})

test_that("from_lame_fit aborts when quantile requested but draws missing", {
	fit <- list(EZ = matrix(rnorm(16), 4, 4),
		family = "binary", mode = "unipartite")
	expect_error(from_lame_fit(fit, value = "prob_lower"),
		regexp = "per-draw")
})

test_that("from_lame_fit explicit link slot wins over class-based inference", {
	# ALS class but force logit via explicit slot
	fit <- mk_mock_fit("ame_als", c("BOOT", "EZ"), link = "logit")
	p <- get_raw(from_lame_fit(fit, value = "prob"))
	z <- fit$EZ
	expect_equal(as.vector(p[upper.tri(p)]),
		as.vector(stats::plogis(z[upper.tri(z)])),
		tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# to_lame(lame=TRUE) longit emitter shape
# ---------------------------------------------------------------------------

test_that("to_lame longit emits single list_to_array call destructuring padded slots", {
	df <- data.frame(
		i = sample(letters[1:6], 60, TRUE),
		j = sample(letters[1:6], 60, TRUE),
		t = sample(2001:2003, 60, TRUE),
		w = rnorm(60),
		x = runif(60),
		stringsAsFactors = FALSE
	)
	df <- df[df$i != df$j, ]

	net_l <- suppressWarnings(suppressMessages(
		netify(df, actor1 = "i", actor2 = "j", time = "t",
			weight = "w", symmetric = FALSE,
			actor_time_uniform = FALSE,
			dyad_vars = "x", dyad_vars_symmetric = FALSE)
	))

	nl <- suppressMessages(to_lame(net_l, lame = TRUE, pad = TRUE))

	snip <- nl$ame_call
	# exactly one list_to_array call
	expect_equal(length(gregexpr("list_to_array\\(", snip, perl = TRUE)[[1]]), 1L)
	# exactly one lame::lame() fit call
	expect_equal(length(gregexpr("lame::lame\\(", snip, perl = TRUE)[[1]]), 1L)
	# the fit call uses padded$<slot> rather than nl$<slot>
	expect_true(grepl("padded\\$Y", snip))
	expect_true(grepl("padded\\$Xdyad", snip))
})

test_that("to_lame longit no-covariate snippet omits X slots and still emits one pad call", {
	df <- data.frame(
		i = sample(letters[1:6], 60, TRUE),
		j = sample(letters[1:6], 60, TRUE),
		t = sample(2001:2003, 60, TRUE),
		w = rnorm(60),
		stringsAsFactors = FALSE
	)
	df <- df[df$i != df$j, ]

	net_l <- suppressWarnings(suppressMessages(
		netify(df, actor1 = "i", actor2 = "j", time = "t",
			weight = "w", symmetric = FALSE,
			actor_time_uniform = FALSE)
	))
	nl <- suppressMessages(to_lame(net_l, lame = TRUE, pad = TRUE))

	snip <- nl$ame_call
	expect_equal(length(gregexpr("list_to_array\\(", snip, perl = TRUE)[[1]]), 1L)
	expect_false(grepl("Xdyad", snip))
	expect_false(grepl("Xrow",  snip))
	expect_false(grepl("Xcol",  snip))
	expect_true(grepl("padded\\$Y", snip))
})

test_that("to_lame longit fit_method = als adds bootstrap and method args", {
	df <- data.frame(
		i = sample(letters[1:6], 60, TRUE),
		j = sample(letters[1:6], 60, TRUE),
		t = sample(2001:2003, 60, TRUE),
		w = rnorm(60),
		stringsAsFactors = FALSE
	)
	df <- df[df$i != df$j, ]
	net_l <- suppressWarnings(suppressMessages(
		netify(df, actor1 = "i", actor2 = "j", time = "t",
			weight = "w", symmetric = FALSE,
			actor_time_uniform = FALSE)
	))
	nl <- suppressMessages(to_lame(net_l, lame = TRUE, pad = TRUE,
		fit_method = "als", bootstrap = 200))
	snip <- nl$ame_call
	expect_true(grepl('method = "als"', snip, fixed = TRUE))
	expect_true(grepl("bootstrap = 200L", snip, fixed = TRUE))
})
