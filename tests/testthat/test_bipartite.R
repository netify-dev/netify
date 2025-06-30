# library(testthat)
# library(netify)
# devtools::load_all('~/Research/netify_dev/netify')

set.seed(6886)

test_that(
    "get_adjacency, bipartite: cross-sectional, asymmetric, weighted network",
    {
        # create data
        ar <- letters[1:3]
        nr <- length(ar)
        ac <- letters[23:26]
        nc <- length(ac)
        asym_weight_df <- expand.grid(
            actor1 = ar, actor2 = ac, stringsAsFactors = FALSE
        )
        asym_weight_df$value <- rnorm(nrow(asym_weight_df))
        asym_weight_df <- asym_weight_df[asym_weight_df$actor1 != asym_weight_df$actor2, ]

        # create matrix
        asym_weight_matrix <- matrix(0, nrow = nr, ncol = nc, dimnames = list(ar, ac))
        for (ii in 1:nrow(asym_weight_df)) {
            asym_weight_matrix[asym_weight_df$actor1[ii], asym_weight_df$actor2[ii]] <- asym_weight_df$value[ii]
        }

        # get adjacency
        a_matrix <- get_adjacency(
            dyad_data = asym_weight_df,
            actor1 = "actor1", actor2 = "actor2",
            weight = "value", symmetric = FALSE,
            mode = "bipartite"
        )

        # the test
        expect_identical(get_raw(a_matrix), asym_weight_matrix)
    }
)

test_that(
    "get_adjacency, bipartite: cross-sectional, asymmetric, weighted network, sum dyads",
    {
        # create data
        ar <- letters[1:7]
        nr <- length(ar)
        ac <- letters[23:26]
        nc <- length(ac)
        asym_weight_df <- expand.grid(
            actor1 = ar, actor2 = ac, stringsAsFactors = FALSE
        )
        asym_weight_df$value <- rnorm(nrow(asym_weight_df))
        asym_weight_df <- asym_weight_df[asym_weight_df$actor1 != asym_weight_df$actor2, ]
        asym_weight_agg_df <- aggregate(
            value ~ actor1 + actor2,
            data = asym_weight_df, sum
        )

        # create matrix
        asym_weight_matrix <- matrix(0, nrow = nr, ncol = nc, dimnames = list(ar, ac))
        for (ii in 1:nrow(asym_weight_df)) {
            old_edge_val <- asym_weight_matrix[asym_weight_agg_df$actor1[ii], asym_weight_agg_df$actor2[ii]]
            new_edge_val <- asym_weight_agg_df$value[ii]
            asym_weight_matrix[asym_weight_agg_df$actor1[ii], asym_weight_agg_df$actor2[ii]] <- old_edge_val + new_edge_val
        }

        # get adjacency
        a_matrix <- get_adjacency(
            dyad_data = asym_weight_df,
            actor1 = "actor1", actor2 = "actor2",
            weight = "value", symmetric = FALSE,
            sum_dyads = TRUE,
            mode = "bipartite"
        )

        # the test
        expect_identical(get_raw(a_matrix), asym_weight_matrix)
    }
)

test_that(
    "get_adjacency, bipartite: cross-sectional, asymmetric, non-weighted network",
    {
        # create data that is cross-sectional, asymmetric, and non-weighted
        ar <- letters[1:7]
        nr <- length(ar)
        ac <- letters[23:26]
        nc <- length(ac)
        asym_non_weight_df <- expand.grid(
            actor1 = ar, actor2 = ac, stringsAsFactors = FALSE
        )
        asym_non_weight_df$value <- rbinom(nrow(asym_non_weight_df), 1, .5)
        asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$actor1 != asym_non_weight_df$actor2, ]

        # filter out zero responses, equivalent to an event dataset
        asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$value > 0, ]

        # create matrix
        asym_non_weight_matrix <- matrix(0, nrow = nr, ncol = nc, dimnames = list(ar, ac))
        for (ii in 1:nrow(asym_non_weight_df)) {
            asym_non_weight_matrix[asym_non_weight_df$actor1[ii], asym_non_weight_df$actor2[ii]] <- asym_non_weight_df$value[ii]
        }

        # get adjacency
        a_matrix <- get_adjacency(
            dyad_data = asym_non_weight_df,
            actor1 = "actor1", actor2 = "actor2",
            weight = NULL, symmetric = FALSE,
            mode = "bipartite"
        )

        # the test
        expect_identical(get_raw(a_matrix), asym_non_weight_matrix)
    }
)

test_that(
    "get_adjacency, bipartite: cross-sectional, asymmetric, non-weighted network, sum dyads",
    {
        # create data that is cross-sectional, asymmetric, and non-weighted
        ar <- letters[1:7]
        nr <- length(ar)
        ac <- letters[23:26]
        nc <- length(ac)
        asym_non_weight_df <- expand.grid(
            actor1 = ar, actor2 = ac, stringsAsFactors = FALSE
        )
        asym_non_weight_df$value <- rbinom(nrow(asym_non_weight_df), 1, .7)
        asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$actor1 != asym_non_weight_df$actor2, ]
        asym_non_weight_df <- rbind(
            asym_non_weight_df,
            data.frame("actor1" = "a", "actor2" = "z", "value" = 1)
        )

        # filter out zero responses, equivalent to an event dataset
        asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$value > 0, ]

        # create matrix
        asym_non_weight_matrix <- matrix(
            0,
            nrow = nr, ncol = nc, dimnames = list(ar, ac)
        )
        for (ii in 1:nrow(asym_non_weight_df)) {
            old_edge_val <- asym_non_weight_matrix[asym_non_weight_df$actor1[ii], asym_non_weight_df$actor2[ii]]
            new_edge_val <- asym_non_weight_df$value[ii]
            asym_non_weight_matrix[asym_non_weight_df$actor1[ii], asym_non_weight_df$actor2[ii]] <- old_edge_val + new_edge_val
        }

        # get adjacency
        a_matrix <- get_adjacency(
            dyad_data = asym_non_weight_df,
            actor1 = "actor1", actor2 = "actor2",
            sum_dyads = TRUE,
            weight = NULL, symmetric = FALSE,
            mode = "bipartite"
        )

        # the test
        expect_identical(get_raw(a_matrix), asym_non_weight_matrix)
    }
)
