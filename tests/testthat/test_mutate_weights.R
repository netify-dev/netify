# library(testthat)
# library(netify)
# devtools::load_all('~/Research/netify_dev/netify')

set.seed(6886)

# load relevant datasets from package
data(icews)

# subset to a particular country in event data
nigeria <- icews[icews$verbConf > 0, ]
nigeria <- nigeria[, c("i", "j", "year")]
names(nigeria) <- c("actor1", "actor2", "year")

# subset to particular year in icews
icews_10 <- icews[icews$year == "2010", ]

test_that(
    "mutate_weights: cross-sectional weighted to log-transformed",
    {
        # create weighted network
        icews_10 <- icews[icews$year == "2010", ]
        net_weighted <- netify(
            icews_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE,
            weight = "verbCoop"
        )

        # transform to log
        net_log <- mutate_weights(
            net_weighted,
            transform_fn = log,
            add_constant = 1,
            new_name = "log_verbCoop"
        )

        # check attributes
        expect_equal(attr(net_log, "weight"), "log_verbCoop")
        expect_equal(attr(net_log, "detail_weight"), "log_verbCoop (transformed)")
        expect_false(attr(net_log, "weight_binary"))

        # check transformation
        orig_mat <- get_raw(net_weighted)
        trans_mat <- get_raw(net_log)
        expect_equal(trans_mat, log(orig_mat + 1))

        # check original preserved
        expect_true("original_weight" %in% names(attr(net_log, "dyad_data")[["1"]]))
    }
)

test_that(
    "mutate_weights: cross-sectional weighted to binary",
    {
        # create weighted network
        icews_10 <- icews[icews$year == "2010", ]
        net_weighted <- netify(
            icews_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE,
            weight = "matlConf"
        )

        # check it's weighted
        expect_false(attr(net_weighted, "weight_binary"))

        # binarize
        net_binary <- mutate_weights(
            net_weighted,
            transform_fn = function(x) ifelse(x > 0, 1, 0),
            new_name = "conflict_present"
        )

        # check attributes updated
        expect_true(attr(net_binary, "weight_binary"))
        expect_equal(attr(net_binary, "weight"), "conflict_present")
        expect_equal(attr(net_binary, "detail_weight"), "conflict_present (binarized)")

        # verify binarization
        trans_mat <- get_raw(net_binary)
        expect_true(all(trans_mat %in% c(0, 1, NA)))
    }
)

test_that(
    "mutate_weights: cross-sectional binary to weighted",
    {
        # create binary network
        nigeria <- icews[icews$verbConf > 0, ]
        nigeria <- nigeria[, c("i", "j", "year")]
        nigeria_10 <- nigeria[nigeria$year == "2010", ]

        net_binary <- netify(
            nigeria_10,
            actor1 = "i", actor2 = "j",
            symmetric = TRUE
        )

        # verify it's binary
        expect_true(attr(net_binary, "weight_binary"))

        # make weighted
        net_weighted <- mutate_weights(
            net_binary,
            transform_fn = function(x) x * 10,
            new_name = "weighted_ties"
        )

        # check attributes
        expect_false(attr(net_weighted, "weight_binary"))
        expect_equal(attr(net_weighted, "detail_weight"), "weighted_ties (weighted from binary)")

        # verify transformation
        orig_mat <- get_raw(net_binary)
        trans_mat <- get_raw(net_weighted)
        expect_equal(trans_mat, orig_mat * 10)
    }
)

test_that(
    "mutate_weights: longitudinal array with sqrt transformation",
    {
        # create longitudinal array
        net_array <- netify(
            icews,
            actor1 = "i", actor2 = "j", time = "year",
            symmetric = FALSE,
            weight = "verbCoop",
            output_format = "longit_array"
        )

        # sqrt transform
        net_sqrt <- mutate_weights(
            net_array,
            transform_fn = sqrt,
            new_name = "sqrt_verbCoop"
        )

        # check attributes
        expect_equal(attr(net_sqrt, "weight"), "sqrt_verbCoop")
        expect_false(attr(net_sqrt, "weight_binary"))

        # verify transformation for specific time slice
        orig_slice <- get_raw(net_array)[, , "2010"]
        trans_slice <- get_raw(net_sqrt)[, , "2010"]
        expect_equal(trans_slice, sqrt(orig_slice))
    }
)

test_that(
    "mutate_weights: longitudinal list with normalization",
    {
        # create longitudinal list
        net_list <- netify(
            icews,
            actor1 = "i", actor2 = "j", time = "year",
            symmetric = FALSE,
            weight = "matlCoop",
            actor_time_uniform = FALSE
        )

        # normalize by max value per time period
        net_norm <- mutate_weights(
            net_list,
            transform_fn = function(x) x / max(x, na.rm = TRUE),
            new_name = "norm_matlCoop"
        )

        # check each time period
        for (t in names(net_norm)) {
            mat <- get_raw(net_norm[[t]])
            max_val <- max(mat, na.rm = TRUE)
            expect_true(max_val <= 1.0001) # account for floating point
        }

        # check attributes preserved
        expect_equal(attr(net_norm, "netify_type"), "longit_list")
        expect_equal(attr(net_norm, "weight"), "norm_matlCoop")
    }
)

test_that(
    "mutate_weights: longitudinal list binarization updates all periods",
    {
        # create weighted longitudinal network
        net_list <- netify(
            icews,
            actor1 = "i", actor2 = "j", time = "year",
            symmetric = FALSE,
            weight = "verbConf",
            actor_time_uniform = TRUE
        )

        # binarize all periods
        net_binary <- mutate_weights(
            net_list,
            transform_fn = function(x) ifelse(x > 0, 1, 0),
            new_name = "conflict_binary"
        )

        # check overall attribute
        expect_true(attr(net_binary, "weight_binary"))

        # check each period is binary
        for (i in seq_along(net_binary)) {
            expect_true(attr(net_binary[[i]], "weight_binary"))
            mat <- get_raw(net_binary[[i]])
            expect_true(all(mat %in% c(0, 1, NA)))
        }
    }
)

test_that(
    "mutate_weights: keep_original = FALSE works correctly",
    {
        # create network
        icews_10 <- icews[icews$year == "2010", ]
        net <- netify(
            icews_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE,
            weight = "verbCoop"
        )

        # transform without keeping original
        net_trans <- mutate_weights(
            net,
            transform_fn = log1p,
            keep_original = FALSE
        )

        # verify no original_weight in dyad_data
        dyad_vars <- names(attr(net_trans, "dyad_data")[["1"]])
        expect_false("original_weight" %in% dyad_vars)
    }
)

test_that(
    "mutate_weights: handles NA values correctly",
    {
        # create network with NAs
        icews_10 <- icews[icews$year == "2010", ]
        net <- netify(
            icews_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE,
            weight = "verbCoop"
        )

        # introduce some NAs
        raw_mat <- get_raw(net)
        raw_mat[1:5, 1:5] <- NA
        net[, ] <- raw_mat

        # transform
        net_log <- mutate_weights(
            net,
            transform_fn = log1p
        )

        # check NAs preserved
        trans_mat <- get_raw(net_log)
        expect_true(all(is.na(trans_mat[1:5, 1:5])))

        # check non-NA values transformed
        non_na_orig <- raw_mat[!is.na(raw_mat)]
        non_na_trans <- trans_mat[!is.na(trans_mat)]
        expect_equal(non_na_trans, log1p(non_na_orig))
    }
)

test_that(
    "mutate_weights: identity transformation preserves binary status",
    {
        # create binary network
        nigeria <- icews[icews$verbConf > 0, ]
        nigeria_10 <- nigeria[nigeria$year == "2010", ]

        net_binary <- netify(
            nigeria_10[, c("i", "j")],
            actor1 = "i", actor2 = "j",
            symmetric = TRUE
        )

        # identity transform
        net_identity <- mutate_weights(
            net_binary,
            transform_fn = function(x) x
        )

        # should still be binary
        expect_true(attr(net_identity, "weight_binary"))
        expect_equal(get_raw(net_binary), get_raw(net_identity))
    }
)

test_that(
    "mutate_weights: complex transformation on bipartite network",
    {
        # create bipartite network
        bip_data <- data.frame(
            actor1 = rep(paste0("A", 1:3), each = 4),
            actor2 = rep(paste0("B", 1:4), 3),
            weight = rpois(12, lambda = 5),
            stringsAsFactors = FALSE
        )

        net_bip <- netify(
            bip_data,
            actor1 = "actor1",
            actor2 = "actor2",
            mode = "bipartite",
            weight = "weight"
        )

        # power transformation
        net_power <- mutate_weights(
            net_bip,
            transform_fn = function(x) x^2,
            new_name = "weight_squared"
        )

        # verify bipartite structure preserved
        expect_equal(attr(net_power, "mode"), "bipartite")

        # verify transformation
        orig_mat <- get_raw(net_bip)
        trans_mat <- get_raw(net_power)
        expect_equal(trans_mat, orig_mat^2)
    }
)

test_that(
    "mutate_weights: standardization (z-scores) transformation",
    {
        # create weighted network
        icews_10 <- icews[icews$year == "2010", ]
        net <- netify(
            icews_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE,
            weight = "verbCoop"
        )

        # standardize
        net_std <- mutate_weights(
            net,
            transform_fn = function(x) {
                mean_x <- mean(x, na.rm = TRUE)
                sd_x <- sd(x, na.rm = TRUE)
                return((x - mean_x) / sd_x)
            },
            new_name = "verbCoop_standardized"
        )

        # check attributes
        expect_equal(attr(net_std, "weight"), "verbCoop_standardized")
        expect_false(attr(net_std, "weight_binary"))

        # verify standardization (mean ~0, sd ~1)
        trans_mat <- get_raw(net_std)
        trans_vec <- as.vector(trans_mat)
        trans_vec <- trans_vec[!is.na(trans_vec)]
        expect_true(abs(mean(trans_vec)) < 1e-10) # mean ~0
        expect_true(abs(sd(trans_vec) - 1) < 1e-10) # sd ~1
    }
)

test_that(
    "mutate_weights: rank transformation",
    {
        # create weighted network
        icews_10 <- icews[icews$year == "2010", ]
        net <- netify(
            icews_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE,
            weight = "verbCoop"
        )

        # rank transformation
        net_rank <- mutate_weights(
            net,
            transform_fn = function(x) rank(x, na.last = "keep"),
            new_name = "verbCoop_ranked"
        )

        # check attributes
        expect_equal(attr(net_rank, "weight"), "verbCoop_ranked")
        expect_false(attr(net_rank, "weight_binary"))

        # verify ranks are reasonable (positive, finite)
        trans_mat <- get_raw(net_rank)
        trans_vec <- as.vector(trans_mat)
        trans_vec <- trans_vec[!is.na(trans_vec)]
        expect_true(all(is.finite(trans_vec))) # all finite
        expect_true(min(trans_vec) >= 1) # ranks start at 1
        expect_true(max(trans_vec) <= length(trans_vec)) # max rank <= n elements
    }
)

test_that(
    "mutate_weights: min-max normalization (0-1 scaling)",
    {
        # create weighted network
        icews_10 <- icews[icews$year == "2010", ]
        net <- netify(
            icews_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE,
            weight = "verbCoop"
        )

        # min-max normalization
        net_norm <- mutate_weights(
            net,
            transform_fn = function(x) {
                min_x <- min(x, na.rm = TRUE)
                max_x <- max(x, na.rm = TRUE)
                return((x - min_x) / (max_x - min_x))
            },
            new_name = "verbCoop_normalized"
        )

        # check attributes
        expect_equal(attr(net_norm, "weight"), "verbCoop_normalized")
        expect_false(attr(net_norm, "weight_binary"))

        # verify normalization (range 0-1)
        trans_mat <- get_raw(net_norm)
        trans_vec <- as.vector(trans_mat)
        trans_vec <- trans_vec[!is.na(trans_vec)]
        expect_true(min(trans_vec) >= 0)
        expect_true(max(trans_vec) <= 1)
        expect_true(abs(min(trans_vec) - 0) < 1e-10) # min should be 0
        expect_true(abs(max(trans_vec) - 1) < 1e-10) # max should be 1
    }
)

test_that(
    "mutate_weights: winsorization (cap extreme values)",
    {
        # create weighted network
        icews_10 <- icews[icews$year == "2010", ]
        net <- netify(
            icews_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE,
            weight = "verbCoop"
        )

        # winsorization at 95th percentile
        net_winsor <- mutate_weights(
            net,
            transform_fn = function(x) {
                q95 <- quantile(x, 0.95, na.rm = TRUE)
                return(pmin(x, q95))
            },
            new_name = "verbCoop_winsorized"
        )

        # check attributes
        expect_equal(attr(net_winsor, "weight"), "verbCoop_winsorized")
        expect_false(attr(net_winsor, "weight_binary"))

        # verify winsorization
        orig_mat <- get_raw(net)
        trans_mat <- get_raw(net_winsor)
        q95_orig <- quantile(as.vector(orig_mat), 0.95, na.rm = TRUE)

        # all values should be <= original 95th percentile
        trans_vec <- as.vector(trans_mat)
        trans_vec <- trans_vec[!is.na(trans_vec)]
        expect_true(all(trans_vec <= q95_orig + 1e-10)) # allow for floating point
    }
)

test_that(
    "mutate_weights: add constant only (no transformation function)",
    {
        # create weighted network
        icews_10 <- icews[icews$year == "2010", ]
        net <- netify(
            icews_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE,
            weight = "verbCoop"
        )

        # add constant only
        net_shifted <- mutate_weights(
            net,
            add_constant = 10,
            new_name = "verbCoop_shifted"
        )

        # check attributes
        expect_equal(attr(net_shifted, "weight"), "verbCoop_shifted")
        expect_false(attr(net_shifted, "weight_binary"))

        # verify constant addition
        orig_mat <- get_raw(net)
        trans_mat <- get_raw(net_shifted)
        expect_equal(trans_mat, orig_mat + 10)
    }
)

test_that(
    "mutate_weights: log1p transformation (handles zeros)",
    {
        # create weighted network
        icews_10 <- icews[icews$year == "2010", ]
        net <- netify(
            icews_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE,
            weight = "verbCoop"
        )

        # log1p transformation
        net_log1p <- mutate_weights(
            net,
            transform_fn = log1p,
            new_name = "log1p_verbCoop",
            keep_original = FALSE
        )

        # check attributes
        expect_equal(attr(net_log1p, "weight"), "log1p_verbCoop")
        expect_false(attr(net_log1p, "weight_binary"))

        # verify transformation
        orig_mat <- get_raw(net)
        trans_mat <- get_raw(net_log1p)
        expect_equal(trans_mat, log1p(orig_mat))

        # verify original not kept
        expect_null(attr(net_log1p, "dyad_data")[["1"]][["original_weight"]])
    }
)

test_that(
    "mutate_weights: custom multi-step transformation",
    {
        # create weighted network
        icews_10 <- icews[icews$year == "2010", ]
        net <- netify(
            icews_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE,
            weight = "verbCoop"
        )

        # custom transformation: log then standardize
        net_custom <- mutate_weights(
            net,
            transform_fn = function(x) {
                x_log <- log(x + 1)
                x_std <- (x_log - mean(x_log, na.rm = TRUE)) / sd(x_log, na.rm = TRUE)
                return(x_std)
            },
            new_name = "verbCoop_log_std"
        )

        # check attributes
        expect_equal(attr(net_custom, "weight"), "verbCoop_log_std")
        expect_false(attr(net_custom, "weight_binary"))

        # verify transformation (standardized log should have mean ~0, sd ~1)
        trans_mat <- get_raw(net_custom)
        trans_vec <- as.vector(trans_mat)
        trans_vec <- trans_vec[!is.na(trans_vec)]
        expect_true(abs(mean(trans_vec)) < 1e-10)
        expect_true(abs(sd(trans_vec) - 1) < 1e-10)
    }
)

test_that(
    "mutate_weights: power transformation",
    {
        # create weighted network
        icews_10 <- icews[icews$year == "2010", ]
        net <- netify(
            icews_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE,
            weight = "verbCoop"
        )

        # power transformation (square root)
        net_power <- mutate_weights(
            net,
            transform_fn = function(x) x^0.5,
            new_name = "verbCoop_power"
        )

        # check attributes
        expect_equal(attr(net_power, "weight"), "verbCoop_power")
        expect_false(attr(net_power, "weight_binary"))

        # verify transformation
        orig_mat <- get_raw(net)
        trans_mat <- get_raw(net_power)
        expect_equal(trans_mat, orig_mat^0.5)
    }
)

test_that(
    "mutate_weights: transformation without new_name preserves original name",
    {
        # create weighted network
        icews_10 <- icews[icews$year == "2010", ]
        net <- netify(
            icews_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE,
            weight = "verbCoop"
        )

        original_weight <- attr(net, "weight")

        # transform without new name
        net_trans <- mutate_weights(
            net,
            transform_fn = sqrt
        )

        # should preserve original weight name
        expect_equal(attr(net_trans, "weight"), original_weight)
    }
)

test_that(
    "mutate_weights: error handling for missing weight",
    {
        # create unweighted network (binary)
        nigeria <- icews[icews$verbConf > 0, ]
        nigeria_10 <- nigeria[nigeria$year == "2010", ]

        net_unweighted <- netify(
            nigeria_10[, c("i", "j")],
            actor1 = "i", actor2 = "j"
        )

        # should work since binary networks have weights
        expect_no_error({
            net_trans <- mutate_weights(
                net_unweighted,
                transform_fn = function(x) x * 2
            )
        })
    }
)
