# library(netify)
# library(testthat)
# devtools::load_all('~/Research/netify_dev/netify')

set.seed(6886)

# load relevant datasets from package
data(icews)

# subset to a particular country in event data
nigeria <- icews[icews$verbConf > 0, ]
nigeria <- nigeria[, c('i', 'j', 'year')]
names(nigeria) <- c('actor1', 'actor2', 'year')

# subset to particular year in icews
icews_10 <- icews[icews$year == '2010', ]

test_that(
    'netify: cross-sectional, undirected, unweighted network', {
        nigeria_unweighted <- get_adjacency(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2',
            symmetric = TRUE
        )

        nigeria_unweighted2 <- netify(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2',
            symmetric = TRUE
        )

        expect_identical(nigeria_unweighted, nigeria_unweighted2)
    }
)

test_that(
    'netify: cross-sectional, undirected, weighted network that sums across dyads', {
        nigeria_weighted <- get_adjacency(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2',
            symmetric = TRUE, sum_dyads = TRUE
        )

        nigeria_weighted2 <- netify(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2',
            symmetric = TRUE, sum_dyads = TRUE
        )

        expect_identical(nigeria_weighted, nigeria_weighted2)
    }
)

test_that(
    'netify: cross-sectional, directed, weighted network that uses a weight var', {
        icews_verbCoop <- get_adjacency(
            icews_10, actor1 = 'i', actor2 = 'j',
            symmetric = FALSE, weight = 'verbCoop'
        )

        icews_verbCoop2 <- netify(
            icews_10, actor1 = 'i', actor2 = 'j',
            symmetric = FALSE, weight = 'verbCoop'
        )

        expect_identical(icews_verbCoop, icews_verbCoop2)
    }
)

test_that(
    'netify: cross-sectional, directed, weighted network that uses a weight var 2', {
        icews_matlConf <- get_adjacency(
            icews_10, actor1 = 'i', actor2 = 'j',
            symmetric = FALSE, weight = 'matlConf'
        )

        icews_matlConf2 <- netify(
            icews_10, actor1 = 'i', actor2 = 'j',
            symmetric = FALSE, weight = 'matlConf'
        )

        expect_identical(icews_matlConf, icews_matlConf2)
    }
)

test_that(
    'netify: longitudinal array, directed, weighted network that uses a weight var', {
        icews_matlConf <- get_adjacency_array(
            icews, 
            actor1 = 'i', actor2 = 'j', time = 'year',
            symmetric = FALSE, weight = 'matlConf'
        )

        icews_matlConf2 <- netify(
            icews, 
            actor1 = 'i', actor2 = 'j', time = 'year',
            symmetric = FALSE, weight = 'matlConf',
            output_format = 'longit_array'
        )

        expect_identical(icews_matlConf, icews_matlConf2)
    }
)

test_that(
    'netify: longitudinal list, undirected, and unweighted network, same actor comp', {
        nigeria_unweighted <- get_adjacency_list(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2', time = 'year',
            symmetric = TRUE, actor_time_uniform = TRUE
        )

        nigeria_unweighted2 <- netify(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2', time = 'year',
            symmetric = TRUE, actor_time_uniform = TRUE
        )

        expect_identical(nigeria_unweighted, nigeria_unweighted2)
    }
)

test_that(
    'netify: longitudinal list, undirected, and weighted network that sums across dyads, same actor comp', {
        nigeria_weighted <- get_adjacency_list(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2', time = 'year',
            symmetric = TRUE, actor_time_uniform = TRUE, sum_dyads = TRUE
        )

        nigeria_weighted2 <- netify(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2', time = 'year',
            symmetric = TRUE, actor_time_uniform = TRUE, sum_dyads = TRUE
        )

        expect_identical(nigeria_weighted, nigeria_weighted2)
    }
)

test_that(
    'netify: longitudinal list, undirected, and unweighted network, diff actor comp', {
        nigeria_unweighted <- get_adjacency_list(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2', time = 'year',
            symmetric = TRUE, actor_time_uniform = FALSE
        )

        nigeria_unweighted2 <- netify(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2', time = 'year',
            symmetric = TRUE, actor_time_uniform = FALSE
        )

        expect_identical(nigeria_unweighted, nigeria_unweighted2)
    }
)

test_that(
    'netify: longitudinal list, undirected, and weighted network that sums across dyads, diff actor comp', {
        nigeria_weighted <- get_adjacency_list(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2', time = 'year',
            symmetric = TRUE, actor_time_uniform = FALSE, sum_dyads = TRUE
        )

        nigeria_weighted2 <- netify(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2', time = 'year',
            symmetric = TRUE, actor_time_uniform = FALSE, sum_dyads = TRUE
        )

        expect_identical(nigeria_weighted, nigeria_weighted2)
    }
)

test_that(
    'netify: longitudinal list, undirected, and weighted network that sums across dyads, user specified actor comp', {
        actor_comp <- data.frame(
            actor = c(
                'United States',
                'Afghanistan',
                'United Kingdom',
                'China',
                'Russian Federation',
                'India'
            ), stringsAsFactors = FALSE
        )
        actor_comp$min_time <- c(2001, 2001, 2001, 2001, 2003, 2002)
        actor_comp$max_time <- c(2014, 2014, 2014, 2014, 2013, 2010)

        nigeria_weighted_specific_actors <- get_adjacency_list(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2', time = 'year',
            symmetric = TRUE, actor_pds = actor_comp, sum_dyads = TRUE
        )

        nigeria_weighted_specific_actors2 <- netify(
            nigeria, 
            actor1 = 'actor1', actor2 = 'actor2', time = 'year',
            symmetric = TRUE, actor_pds = actor_comp, sum_dyads = TRUE
        )

        expect_identical(nigeria_weighted_specific_actors, nigeria_weighted_specific_actors2)
    }
)

test_that(
    "netify handles data.frame input by default (auto)", {
        df <- data.frame(
            i = c("A", "B", "A"),
            j = c("B", "C", "C"),
            year = c(2001, 2001, 2002),
            weight = c(1, 2, 3),
            stringsAsFactors = FALSE
        )

        net_obj <- netify(df, actor1 = "i", actor2 = "j", time = "year", weight = "weight")

        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "longit_list")
})

test_that(
    "netify handles data.frame input when input_type = 'dyad_df'", {
        df <- data.frame(
            source = c("X", "Y"),
            target = c("Y", "Z"),
            val = c(5, 10),
            stringsAsFactors = FALSE
        )

        net_obj <- netify(df, input_type = "dyad_df",
                                            actor1 = "source", actor2 = "target", weight = "val")

        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "cross_sec")
})

test_that(
    "netify handles matrix input (auto-detect => goes to to_netify)", {
        mat <- matrix(sample(0:1, 16, replace = TRUE), 4, 4)
        rownames(mat) <- colnames(mat) <- paste0("actor", 1:4)

        net_obj <- netify(mat)

        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "cross_sec")
})

test_that(
    "netify handles igraph input (auto-detect => goes to to_netify)", {
        g <- igraph::make_ring(5)
        igraph::E(g)$myweight <- seq_len(igraph::ecount(g))

        net_obj <- netify(g, weight = "myweight")

        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "cross_sec")
})

test_that(
    "netify handles list of igraph objects (auto => longit_list)", {
        g1 <- igraph::make_ring(4)
        igraph::E(g1)$myweight <- c(1, 2, 3, 4)
        g2 <- igraph::make_star(4)
        igraph::E(g2)$myweight <- c(5, 6, 7)

        net_obj <- netify(list("t1" = g1, "t2" = g2), weight = "myweight")

        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "longit_list")
        expect_length(net_obj, 2)
})

test_that(
    "netify handles input when input_type='netify_obj' (forces to_netify)", {
        df <- data.frame(a = 1:3, b = 4:6)

        expect_error(netify(df, input_type = "netify_obj"))
})

test_that(
    "netify allows passing additional arguments (...) to to_netify", {
        mat <- matrix(0, 3, 3)
        mat[1, 2] <- 1
        mat[2, 3] <- 1
        rownames(mat) <- colnames(mat) <- c("A", "B", "C")

        net_obj <- netify(mat, mode = "unipartite", symmetric = FALSE, some_extra_arg = TRUE)

        expect_s3_class(net_obj, "netify")
})

test_that(
    "netify handles a list of igraph objects (auto => longit_list)", {
        g1 <- igraph::make_ring(4)
        igraph::E(g1)$myweight <- c(1, 2, 3, 4)
        g2 <- igraph::make_star(5)
        igraph::E(g2)$myweight <- seq_len(igraph::ecount(g2))

        net_obj <- netify(list("T1" = g1, "T2" = g2), weight = "myweight")

        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "longit_list")
        expect_length(net_obj, 2)
        expect_equal(names(net_obj), c("T1", "T2"))
})

test_that(
    "netify handles a list of network objects (auto => longit_list)", {
        nw1 <- network::network(matrix(rbinom(9, 1, 0.3), 3, 3), directed = FALSE)
        network::set.edge.attribute(nw1, "wts", runif(network::network.edgecount(nw1)))

        nw2 <- network::network(matrix(rbinom(16, 1, 0.3), 4, 4), directed = FALSE)
        network::set.edge.attribute(nw2, "wts", runif(network::network.edgecount(nw2)))

        net_obj <- netify(list("Year1" = nw1, "Year2" = nw2), weight = "wts")

        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "longit_list")

        expect_length(net_obj, 2)
        expect_equal(dim(net_obj[["Year1"]]), c(3, 3))
        expect_equal(dim(net_obj[["Year2"]]), c(4, 4))
})

test_that(
    "netify handles a list of adjacency matrices (auto => longit_list)", {
        mat1 <- matrix(rbinom(9, 1, 0.5), 3, 3)
        rownames(mat1) <- colnames(mat1) <- paste0("A", 1:3)

        mat2 <- matrix(rbinom(16, 1, 0.5), 4, 4)
        rownames(mat2) <- colnames(mat2) <- paste0("B", 1:4)

        net_obj <- netify(list(mat1, mat2))

        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "longit_list")

        expect_length(net_obj, 2)
        expect_equal(names(net_obj), c("1", "2"))

        expect_equal(dim(net_obj[[1]]), c(3, 3))
        expect_equal(dim(net_obj[[2]]), c(4, 4))
})

test_that(
    "netify handles a single-element list (auto => longit_list with one slice)", {
        single_list <- list("T1" = matrix(rbinom(9, 1, 0.3), 3, 3))

        net_obj <- netify(single_list)
        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "longit_list")
        expect_length(net_obj, 1)
        expect_equal(names(net_obj), "T1")
})

test_that(
    "netify handles lists of different object types gracefully (error)", {
        g <- igraph::make_ring(4)
        mat <- matrix(rbinom(9, 1, 0.3), 3, 3)

        mixed_list <- list(g, mat)
        expect_error(netify(mixed_list))
})


# test_that(
#     "netify handles character date/time variables", {
#         # Create data with character dates
#         df_char_dates <- data.frame(
#             actor1 = c("USA", "USA", "China", "China", "Russia", "Russia"),
#             actor2 = c("China", "Russia", "USA", "Russia", "USA", "China"),
#             date = c("2020-01-01", "2020-01-01", "2020-06-15", 
#                     "2020-06-15", "2021-03-20", "2021-03-20"),
#             trade_value = c(100, 50, 120, 30, 45, 80),
#             stringsAsFactors = FALSE
#         )
        
#         # This should produce an error since time must be numeric
#         expect_error(
#             netify(
#                 df_char_dates,
#                 actor1 = "actor1",
#                 actor2 = "actor2", 
#                 time = "date",
#                 weight = "trade_value"
#             ))
        
#         # Convert character dates to numeric (e.g., year)
#         df_char_dates$year <- as.numeric(format(as.Date(df_char_dates$date), "%Y"))
        
#         # Now it should work with numeric year
#         net_obj <- netify(
#             df_char_dates,
#             actor1 = "actor1",
#             actor2 = "actor2",
#             time = "year",
#             weight = "trade_value"
#         )
        
#         expect_s3_class(net_obj, "netify")
#         expect_equal(attr(net_obj, "netify_type"), "longit_list")
#         expect_equal(names(net_obj), c("2020", "2021"))
        
#         # Verify the network structure
#         expect_equal(dim(net_obj[["2020"]]), c(3, 3))  # USA, China, Russia
#         expect_equal(dim(net_obj[["2021"]]), c(3, 3))
# })


test_that(
    "netify handles character date time variables", {
        # create data with character dates
        df_char_dates <- data.frame(
            actor1 = c("USA", "USA", "China", "China", "Russia", "Russia"),
            actor2 = c("China", "Russia", "USA", "Russia", "USA", "China"),
            date = c("2020-01-01", "2020-01-01", "2020-06-15", 
                    "2020-06-15", "2021-03-20", "2021-03-20"),
            trade_value = c(100, 50, 120, 30, 45, 80),
            stringsAsFactors = FALSE
        )
        
        # this should work with character time
        net_obj <- netify(
            df_char_dates,
            actor1 = "actor1",
            actor2 = "actor2", 
            time = "date",
            weight = "trade_value"
        )
        
        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "longit_list")
        expect_equal(names(net_obj), c("2020-01-01", "2020-06-15", "2021-03-20"))
        
        # verify the network structure
        expect_equal(dim(net_obj[["2020-01-01"]]), c(3, 3))
        expect_equal(dim(net_obj[["2021-03-20"]]), c(3, 3))
})

test_that(
    "netify handles Date class time variables", {
        # create data with actual Date objects
        df_dates <- data.frame(
            i = rep(c("A", "B", "C"), each = 6),
            j = rep(c("A", "B", "C"), 6),
            date = rep(as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", 
                               "2020-04-01", "2020-05-01", "2020-06-01")), 3),
            weight = rpois(18, lambda = 5),
            stringsAsFactors = FALSE
        )
        
        # should handle Date objects
        net_obj <- netify(
            df_dates,
            actor1 = "i",
            actor2 = "j",
            time = "date",
            weight = "weight",
            output_format = "longit_array"
        )
        
        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "longit_array")
        expect_equal(dim(net_obj), c(3, 3, 6))
        
        # check time dimension names are preserved as dates
        time_names <- dimnames(net_obj)[[3]]
        expect_equal(time_names[1], "2020-01-01")
        expect_equal(time_names[6], "2020-06-01")
})

test_that(
    "netify handles POSIXct datetime variables", {
        # create data with POSIXct timestamps
        df_posix <- data.frame(
            sender = c("server1", "server2", "server1", "server3"),
            receiver = c("server2", "server3", "server3", "server1"),
            timestamp = as.POSIXct(c("2023-01-01 10:30:00", "2023-01-01 14:45:00",
                                   "2023-01-02 09:15:00", "2023-01-02 16:20:00")),
            bytes = c(1024, 2048, 512, 4096),
            stringsAsFactors = FALSE
        )
        
        net_obj <- netify(
            df_posix,
            actor1 = "sender",
            actor2 = "receiver",
            time = "timestamp",
            weight = "bytes"
        )
        
        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "longit_list")
        # should aggregate to day level
        expect_equal(length(net_obj), 2)
        expect_equal(names(net_obj), c("2023-01-01", "2023-01-02"))
})

test_that(
    "netify handles character time with custom formats", {
        # create data with quarterly time periods
        df_quarters <- data.frame(
            company1 = rep(c("Apple", "Google", "Microsoft"), 4),
            company2 = rep(c("Google", "Microsoft", "Apple"), 4),
            quarter = rep(c("Q1-2020", "Q2-2020", "Q3-2020", "Q4-2020"), each = 3),
            partnerships = sample(1:10, 12, replace = TRUE),
            stringsAsFactors = FALSE
        )
        
        net_obj <- netify(
            df_quarters,
            actor1 = "company1",
            actor2 = "company2",
            time = "quarter",
            weight = "partnerships",
            symmetric = TRUE
        )
        
        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "longit_list")
        expect_equal(names(net_obj), c("Q1-2020", "Q2-2020", "Q3-2020", "Q4-2020"))
})

test_that(
    "netify handles mixed format time that sorts correctly", {
        # create data with month names
        df_months <- data.frame(
            from = c("A", "B", "A", "C", "B", "C"),
            to = c("B", "C", "C", "A", "A", "B"),
            month = c("January", "February", "March", "January", "February", "March"),
            value = c(10, 20, 30, 15, 25, 35),
            stringsAsFactors = FALSE
        )
        
        net_obj <- netify(
            df_months,
            actor1 = "from",
            actor2 = "to",
            time = "month",
            weight = "value"
        )
        
        expect_s3_class(net_obj, "netify")
        expect_equal(attr(net_obj, "netify_type"), "longit_list")
        # alphabetical sorting
        expect_equal(names(net_obj), c("February", "January", "March"))
        expect_equal(length(net_obj), 3)
})

test_that(
    "netify with character time and specific output format", {
        # test with longit_array output
        df_char_time <- data.frame(
            i = rep(c("X", "Y", "Z"), 3),
            j = rep(c("Y", "Z", "X"), 3),
            period = rep(c("T1", "T2", "T3"), each = 3),
            flow = runif(9, 0, 100),
            stringsAsFactors = FALSE
        )
        
        net_array <- netify(
            df_char_time,
            actor1 = "i",
            actor2 = "j",
            time = "period",
            weight = "flow",
            output_format = "longit_array",
            symmetric = FALSE
        )
        
        expect_s3_class(net_array, "netify")
        expect_equal(attr(net_array, "netify_type"), "longit_array")
        expect_equal(dimnames(net_array)[[3]], c("T1", "T2", "T3"))
})

test_that(
    "netify preserves time ordering with character dates", {
        # create data with dates that need proper ordering
        df_unordered <- data.frame(
            a1 = c("A", "B", "A", "B", "C", "C"),
            a2 = c("B", "C", "C", "A", "A", "B"),
            date = c("2021-12-01", "2021-01-15", "2021-06-30", 
                    "2021-03-10", "2021-09-22", "2021-11-05"),
            weight = 1:6,
            stringsAsFactors = FALSE
        )
        
        net_obj <- netify(
            df_unordered,
            actor1 = "a1",
            actor2 = "a2",
            time = "date",
            weight = "weight"
        )
        
        # check that dates are properly ordered
        expected_order <- c("2021-01-15", "2021-03-10", "2021-06-30", 
                          "2021-09-22", "2021-11-05", "2021-12-01")
        expect_equal(names(net_obj), expected_order)
})