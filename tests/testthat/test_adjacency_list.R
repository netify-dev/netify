set.seed(6886)

# longitudinal, asymmetric weighted network
test_that(
	"get_adjacency_list: longitudinal, asymmetric weighted network",
	{
		# create symmetric long data
		long_asym_weight_df = expand.grid(actor1 = letters[1:3], actor2 = letters[1:3], time = 1:10, stringsAsFactors = FALSE)
		long_asym_weight_df$value = rnorm(nrow(long_asym_weight_df))
		long_asym_weight_df = long_asym_weight_df[long_asym_weight_df$actor1 != long_asym_weight_df$actor2, ]

		# create matrix
		long_asym_weight_matrix = matrix(0, nrow = 3, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))

		# create an empty list object to store matrices
		times = sort(unique(long_asym_weight_df[, "time"]))
		result = lapply(times, function(t) {
			# slice to relevant time period
			slice = long_asym_weight_df[long_asym_weight_df[, "time"] == t, ]

			# fill in
			for (ii in 1:nrow(slice)) {
				long_asym_weight_matrix[slice$actor1[ii], slice$actor2[ii]] = slice$value[ii]
			}

			return(long_asym_weight_matrix)
		})
		names(result) = times

		# get adjacency
		a_matrix = get_adjacency_list(
			long_asym_weight_df,
			actor1 = "actor1", actor2 = "actor2", time = "time",
			actor_time_uniform = TRUE, actor_pds = NULL,
			weight = "value", symmetric = FALSE,
			diag_to_NA = FALSE, missing_to_zero = TRUE
		)

		expect_identical(get_raw(a_matrix), result)
	}
)

# longitudinal, symmetric weighted network with repeating dyads summed
test_that(
	"get_adjacency_list: longitudinal, symmetric weighted network",
	{
		# create data
		actor1 = rep(c("a", "a", "b", "b", "c", "c"), 3)
		actor2 = rep(c("b", "c", "a", "c", "b", "a"), 3)
		value = c(c(2, 4, 2, 7, 7, 4), c(4, 5, 4, 6, 6, 5), c(11, 2, 11, 3, 3, 2))
		time = rep(1:3, each = 6)
		long_sym_weight_df = data.frame(actor1 = actor1, actor2 = actor2, value = value, time = time)

		# create matrix
		long_sym_weight_matrix = matrix(0, nrow = 3, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))

		# create an empty list object to store matrices
		times = sort(unique(long_sym_weight_df$time))
		result = lapply(times, function(t) {
			# get data from particular time period
			slice = long_sym_weight_df[long_sym_weight_df$time == t, ]

			# iterate through rows to create summary matrix
			for (ii in 1:nrow(slice)) {
				old_value = long_sym_weight_matrix[
					slice$actor1[ii], slice$actor2[ii]
				]
				new_value = old_value + slice$value[ii]
				long_sym_weight_matrix[slice$actor1[ii], slice$actor2[ii]] = new_value
			}

			return(long_sym_weight_matrix * 2)
		})
		names(result) = times

		# get adjacency
		a_matrix = get_adjacency_list(
			long_sym_weight_df,
			actor1 = "actor1", actor2 = "actor2", time = "time",
			actor_time_uniform = TRUE, actor_pds = NULL,
			weight = "value", symmetric = TRUE, sum_dyads = TRUE,
			diag_to_NA = FALSE, missing_to_zero = TRUE
		)

		expect_identical(get_raw(a_matrix), result)
	}
)

# longitudinal, asymmetric non-weighted network
test_that(
	"get_adjacency_list: longitudinal, asymmetric non-weighted network",
	{
		# create df
		long_asym_non_weight_df = expand.grid(actor1 = letters[1:3], actor2 = letters[1:3], time = 1:3, stringsAsFactors = FALSE)
		long_asym_non_weight_df$value = rbinom(nrow(long_asym_non_weight_df), 1, 0.5)
		long_asym_non_weight_df = long_asym_non_weight_df[long_asym_non_weight_df$actor1 != long_asym_non_weight_df$actor2, ]
		long_asym_non_weight_df = long_asym_non_weight_df[long_asym_non_weight_df$value > 0, ]

		# create matrix
		# long_asym_non_weight_mat = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3],letters[1:3]))

		# create an empty list object to store matrices
		result = list()
		times = 1:3
		for (i in times) {
			# create new matrix at the beginning of each loop
			long_asym_non_weight_mat = matrix(0, nrow = 3, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))
			slice = long_asym_non_weight_df[long_asym_non_weight_df[, "time"] == i, ]
			if (dim(slice)[1] != 0) {
				for (ii in 1:nrow(slice)) {
					long_asym_non_weight_mat[slice$actor1[ii], slice$actor2[ii]] = slice$value[ii]
				}
			}
			# modify the example_matrix to make it non-weighted
			# long_asym_non_weight_mat = 1*(long_asym_non_weight_mat!=0)
			result[[i]] = long_asym_non_weight_mat
		}
		names(result) = times

		# get adjacency
		a_matrix = get_adjacency_list(
			long_asym_non_weight_df,
			actor1 = "actor1", actor2 = "actor2", time = "time",
			actor_time_uniform = TRUE, actor_pds = NULL,
			weight = NULL, symmetric = FALSE,
			diag_to_NA = FALSE, missing_to_zero = TRUE
		)

		expect_identical(get_raw(a_matrix), result)
	}
)

# longitudinal, symmetric non-weighted network
test_that(
	"get_adjacency_list: longitudinal, symmetric non-weighted network",
	{
		# create data
		actor1 = rep(c("a", "a", "b", "b", "c", "c"), 3)
		actor2 = rep(c("b", "c", "a", "c", "b", "a"), 3)
		value = c(c(1, 0, 1, 0, 0, 0), c(1, 1, 1, 0, 0, 1), c(1, 0, 1, 0, 0, 0))
		time = rep(1:3, each = 6)
		long_sym_non_weight_df = data.frame(actor1 = actor1, actor2 = actor2, value = value, time = time)
		long_sym_non_weight_df = long_sym_non_weight_df[long_sym_non_weight_df$value > 0, ]

		# create an empty list object to store matrices
		result = list()
		times = sort(unique(long_sym_non_weight_df[, "time"]))

		for (i in times) {
			# create new matrix at the beginning of each loop
			long_sym_non_weight_matrix = matrix(0, nrow = 3, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))
			slice = long_sym_non_weight_df[long_sym_non_weight_df[, "time"] == i, ]

			for (ii in 1:nrow(slice)) {
				long_sym_non_weight_matrix[slice$actor1[ii], slice$actor2[ii]] = slice$value[ii]

				result[[i]] = long_sym_non_weight_matrix
			}
		}
		names(result) = times

		# get adjacency
		a_matrix = get_adjacency_list(
			long_sym_non_weight_df,
			actor1 = "actor1", actor2 = "actor2", time = "time",
			actor_time_uniform = TRUE, actor_pds = NULL,
			weight = NULL, symmetric = TRUE,
			diag_to_NA = FALSE, missing_to_zero = TRUE
		)

		expect_identical(get_raw(a_matrix), result)
	}
)

test_that(
	"get_adjacency_list: actor_pds with gapped numeric time keeps observed periods",
	{
		df = data.frame(
			i = c("a", "b", "a", "c", "b", "c"),
			j = c("b", "c", "c", "a", "a", "b"),
			t = c(2000, 2000, 2005, 2005, 2010, 2010)
		)
		ap = data.frame(actor = c("a", "b", "c"), min_time = 2000, max_time = 2010)

		net = get_adjacency_list(df, actor1 = "i", actor2 = "j", time = "t",
			actor_pds = ap)
		default_net = get_adjacency_list(df, actor1 = "i", actor2 = "j", time = "t",
			actor_time_uniform = TRUE)

		expect_equal(names(net), c("2000", "2005", "2010"))
		expect_equal(names(net), names(default_net))
		expect_equal(attr(net, "actor_pds")$min_time, rep(2000, 3))

		# a bound named in actor_pds is kept as a period
		ap$min_time[2] = 2001
		net_bound = get_adjacency_list(df, actor1 = "i", actor2 = "j", time = "t",
			actor_pds = ap)
		expect_equal(names(net_bound), c("2000", "2001", "2005", "2010"))
		expect_false("b" %in% rownames(net_bound[["2000"]]))
	}
)
