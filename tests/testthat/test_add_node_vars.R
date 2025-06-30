set.seed(6886)

test_that(
    "add_node_vars: cross_sec",
    {
        # create a fake adjacency matrix
        set.seed(6886)

        actors <- c("a", "b", "c", "d")
        n <- length(actors)
        adj_out <- matrix(rnorm(n^2), nrow = n, dimnames = list(actors, actors))

        # add info on actor year sample
        actor_pds <- data.frame(actor = actors, stringsAsFactors = FALSE)
        actor_pds$min_time <- 1
        actor_pds$max_time <- 1

        # create a fake nodal data
        actors2 <- c("c", "b", "a", "f")
        node_data <- data.frame(
            actor = actors2,
            var1 = rnorm(length(actors2)),
            var2 = rnorm(length(actors2)),
            var3 = rnorm(length(actors2)),
            stringsAsFactors = FALSE
        )
        node_data$actor[4] <- "d"

        # extract nodal information and order that matches the actor info in adj_out
        result <- node_data[which(node_data$actor %in% actors), ]
        result <- result[match(actors, result$actor), ]
        rownames(result) <- NULL

        # convert into netify object and add attributes
        class(adj_out) <- "netify"
        attr(adj_out, "netify_type") <- "cross_sec"
        attr(adj_out, "actor_time_uniform") <- TRUE
        attr(adj_out, "actor_pds") <- actor_pds
        attr(adj_out, "weight") <- "hi"
        attr(adj_out, "symmetric") <- FALSE
        attr(adj_out, "mode") <- "unipartite"
        attr(adj_out, "isolates") <- TRUE
        attr(adj_out, "diag_to_NA") <- TRUE
        attr(adj_out, "missing_to_zero") <- TRUE
        attr(adj_out, "nodal_data") <- NULL
        attr(adj_out, "dyad_data") <- NULL


        # add_node_vars()
        add_nodal_result <- add_node_vars(
            adj_out,
            node_data,
            actor = "actor",
            node_vars = c("var1", "var2", "var3")
        )

        # the test
        expect_identical(attributes(add_nodal_result)$nodal_data, result)
    }
)

#######
test_that(
    "add_node_vars: longit_list",
    {
        # create a fake adjacency matrix
        set.seed(6886)
        actors <- c("a", "b", "c")
        actor1 <- rep(c("a", "a", "b", "b", "c", "c"), 3)
        actor2 <- rep(c("b", "c", "a", "c", "b", "a"), 3)
        value <- c(c(2, 4, 2, 7, 7, 4), c(4, 5, 4, 6, 6, 5), c(11, 2, 11, 3, 3, 2))
        time <- rep(1:3, each = 6)
        long_sym_weight_df <- data.frame(actor1 = actor1, actor2 = actor2, value = value, time = time)

        # create matrix
        long_sym_weight_matrix <- matrix(0, nrow = 3, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))

        # create an empty list object to store matrices
        result <- list()
        times <- sort(unique(long_sym_weight_df[, "time"]))
        for (i in times) {
            slice <- long_sym_weight_df[long_sym_weight_df[, "time"] == i, ]

            for (ii in 1:nrow(slice)) {
                long_sym_weight_matrix[slice$actor1[ii], slice$actor2[ii]] <- slice$value[ii]

                result[[i]] <- long_sym_weight_matrix
            }
        }

        names(result) <- times

        # add info on actor year sample
        actor_pds <- data.frame(actor = actors, stringsAsFactors = FALSE)
        actor_pds$min_time <- 1
        actor_pds$max_time <- 3

        # create a fake nodal data
        actors2 <- c("a", "a", "a", "b", "b", "b", "c", "c", "c", "d", "d", "d")
        node_data <- data.frame(
            actor = actors2,
            time = rep(1:3, 4),
            var1 = rnorm(length(actors2)),
            var2 = rnorm(length(actors2)),
            var3 = rnorm(length(actors2)),
            stringsAsFactors = FALSE
        )

        # extract nodal information and order that matches the actor info in result
        node_result <- node_data[which(node_data$actor %in% actors), ]
        node_result <- node_result[which(node_result$time %in% 1:3), ]

        # convert into netify object and add attributes
        class(result) <- "netify"
        attr(result, "netify_type") <- "longit_list"
        attr(result, "actor_time_uniform") <- TRUE
        attr(result, "actor_pds") <- actor_pds
        attr(result, "weight") <- "hi"
        attr(result, "symmetric") <- FALSE
        attr(result, "mode") <- "unipartite"
        attr(result, "isolates") <- TRUE
        attr(result, "diag_to_NA") <- TRUE
        attr(result, "missing_to_zero") <- TRUE
        attr(result, "nodal_data") <- NULL
        attr(result, "dyad_data") <- NULL

        # add_node_vars()
        add_nodal_result <- add_node_vars(
            result,
            node_data,
            actor = "actor",
            time = "time",
            node_vars = c("var1", "var2", "var3")
        )

        # the test
        expect_identical(attributes(add_nodal_result)$nodal_data, node_result)
    }
)
