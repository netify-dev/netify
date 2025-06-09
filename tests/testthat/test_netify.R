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
