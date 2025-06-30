#
set.seed(6886)
# library(netify)
# library(testthat)

################################################
# load relevant datasets from package
data(icews)

# create binary edgelist version
icews$matlConfBin <- ifelse(icews$matlConf > mean(icews$matlConf), 1, 0)

# subset to an edge list version
icews_el <- icews[icews$matlConfBin > 0, c("i", "j", "year")]

# create cross-sectional versions
icews_10 <- icews[icews$year == "2010", ]
icews_el_10 <- icews_el[icews_el$year == "2010", ]

# actor vector to subset by
actors_to_keep <- c(
    "Australia", "Brazil",
    "Canada", "Chile", "China",
    "Colombia", "Egypt", "Ethiopia",
    "France", "Germany", "Ghana",
    "Hungary", "India", "Indonesia",
    "Iran, Islamic Republic Of",
    "Israel", "Italy", "Japan", "Kenya",
    "Korea, Democratic People's Republic Of",
    "Korea, Republic Of", "Nigeria", "Pakistan",
    "Qatar", "Russian Federation", "Saudi Arabia",
    "South Africa", "Spain", "Sudan",
    "Syrian Arab Republic", "Thailand",
    "United Kingdom", "United States",
    "Zimbabwe"
)

# create bipartite version
mode1_all <- c(
    "Australia", "Canada",
    "France", "Germany", "Italy",
    "Japan", "Korea, Republic Of",
    "Spain", "United Kingdom", "United States"
)
mode2_all <- c(
    "China", "Hungary", "Iran, Islamic Republic Of",
    "Korea, Democratic People's Republic Of",
    "Russian Federation", "Syrian Arab Republic"
)

# pick some subsets
set.seed(6886)
mode1_sub <- mode1_all[sample(1:length(mode1_all), 5)]
mode2_sub <- mode2_all[sample(1:length(mode2_all), 5)]

# modify icews to be bipartite
icews_bipartite <- icews[icews$i %in% mode1_all & icews$j %in% mode2_all, ]

# create cross_sectional version
icews_bipartite_10 <- icews_bipartite[icews_bipartite$year == "2010", ]

# create cross-sectional multilayer netlet
icews_verbCoop_l <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "verbCoop", output_format = "longit_list"
)
icews_matlCoop_l <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "matlCoop", output_format = "longit_list"
)
icews_verbConf_l <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "verbConf", output_format = "longit_list"
)
icews_matlConf_l <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "matlConf", output_format = "longit_list"
)

# layer together cross-sec netify objects together
icews_all_l <- layer_netify(
    netlet_list = list(icews_verbCoop_l, icews_matlCoop_l, icews_verbConf_l, icews_matlConf_l),
    layer_labels = c("verbCoop", "matlCoop", "verbConf", "matlConf")
)

# create cross-sectional multilayer netlet
icews_verbCoop_a <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "verbCoop", output_format = "longit_array"
)
icews_matlCoop_a <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "matlCoop", output_format = "longit_array"
)
icews_verbConf_a <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "verbConf", output_format = "longit_array"
)
icews_matlConf_a <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "matlConf", output_format = "longit_array"
)

# layer together cross-sec netify objects together
icews_all_a <- layer_netify(
    netlet_list = list(icews_verbCoop_a, icews_matlCoop_a, icews_verbConf_a, icews_matlConf_a),
    layer_labels = c("verbCoop", "matlCoop", "verbConf", "matlConf")
)

# do the same for the cross sectional version
icews_verbCoop_10 <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE, weight = "verbCoop"
)
icews_matlCoop_10 <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE, weight = "matlCoop"
)
icews_verbConf_10 <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE, weight = "verbConf"
)
icews_matlConf_10 <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE, weight = "matlConf"
)

# layer together
icews_all_10 <- layer_netify(
    netlet_list = list(icews_verbCoop_10, icews_matlCoop_10, icews_verbConf_10, icews_matlConf_10),
    layer_labels = c("verbCoop", "matlCoop", "verbConf", "matlConf")
)
################################################

################################################
# test that rows and columns get subsetted correctly for unipartite nets
test_that(
    "subset_netify: longitudinal list, unipartite, actor subset check",
    {
        # create netify object and then subset
        netlet <- icews_matlConf_l

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- Reduce("unique", lapply(netlet_subset, nrow))

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(Reduce("unique", lapply(netlet_subset, rownames)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual),
            list(n_netify, actors_netify)
        )
    }
)

test_that(
    "subset_netify: longitudinal array, unipartite, actor subset check",
    {
        # create netify object and then subset
        netlet <- icews_matlConf_a

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual),
            list(n_netify, actors_netify)
        )
    }
)

test_that(
    "subset_netify: cross_sectional, unipartite, actor subset check",
    {
        # create netify object and then subset
        netlet <- icews_matlConf_10

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual),
            list(n_netify, actors_netify)
        )
    }
)
################################################

################################################
# test that rows and columns get subsetted correctly for bipartite nets
test_that(
    "subset_netify: longitudinal list, bipartite, from subset check",
    {
        # create netify object and then subset
        netlet <- netify(
            icews_bipartite,
            actor1 = "i", actor2 = "j", time = "year",
            symmetric = FALSE, weight = "matlConf",
            mode = "bipartite",
            output_format = "longit_list"
        )

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            from = mode1_sub
        )

        # get number of actors in netlet_subset
        n_actual <- length(mode1_sub)
        n_netify <- Reduce("unique", lapply(netlet_subset, nrow))

        # also check actors are present
        actors_actual <- sort(unique(mode1_sub))
        actors_netify <- sort(Reduce("unique", lapply(netlet_subset, rownames)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual),
            list(n_netify, actors_netify)
        )
    }
)

test_that(
    "subset_netify: longitudinal list, bipartite, to subset check",
    {
        # create netify object and then subset
        netlet <- netify(
            icews_bipartite,
            actor1 = "i", actor2 = "j", time = "year",
            symmetric = FALSE, weight = "matlConf",
            mode = "bipartite",
            output_format = "longit_list"
        )

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            to = mode2_sub
        )

        # get number of actors in netlet_subset
        n_actual <- length(mode2_sub)
        n_netify <- Reduce("unique", lapply(netlet_subset, ncol))

        # also check actors are present
        actors_actual <- sort(unique(mode2_sub))
        actors_netify <- sort(Reduce("unique", lapply(netlet_subset, colnames)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual),
            list(n_netify, actors_netify)
        )
    }
)

test_that(
    "subset_netify: longitudinal list, bipartite, from+to subset check",
    {
        # create netify object and then subset
        netlet <- netify(
            icews_bipartite,
            actor1 = "i", actor2 = "j", time = "year",
            symmetric = FALSE, weight = "matlConf",
            mode = "bipartite",
            output_format = "longit_list"
        )

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            from = mode1_sub,
            to = mode2_sub
        )

        # get number of actors in netlet_subset
        n_actual_from <- length(mode1_sub)
        n_netify_from <- Reduce("unique", lapply(netlet_subset, nrow))
        n_actual_to <- length(mode2_sub)
        n_netify_to <- Reduce("unique", lapply(netlet_subset, ncol))

        # also check actors are present
        actors_actual_from <- sort(unique(mode1_sub))
        actors_netify_from <- sort(Reduce("unique", lapply(netlet_subset, rownames)))
        actors_actual_to <- sort(unique(mode2_sub))
        actors_netify_to <- sort(Reduce("unique", lapply(netlet_subset, colnames)))

        # check that both are identical
        expect_identical(
            list(n_actual_from, n_actual_to, actors_actual_from, actors_actual_to),
            list(n_netify_from, n_netify_to, actors_netify_from, actors_netify_to)
        )
    }
)

test_that(
    "subset_netify: longitudinal array, bipartite, from subset check",
    {
        # create netify object and then subset
        netlet <- netify(
            icews_bipartite,
            actor1 = "i", actor2 = "j", time = "year",
            symmetric = FALSE, weight = "matlConf",
            mode = "bipartite",
            output_format = "longit_array"
        )

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            from = mode1_sub
        )

        # get number of actors in netlet_subset
        n_actual <- length(mode1_sub)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(mode1_sub))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual),
            list(n_netify, actors_netify)
        )
    }
)

test_that(
    "subset_netify: longitudinal array, bipartite, to subset check",
    {
        # create netify object and then subset
        netlet <- netify(
            icews_bipartite,
            actor1 = "i", actor2 = "j", time = "year",
            symmetric = FALSE, weight = "matlConf",
            mode = "bipartite",
            output_format = "longit_array"
        )

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            to = mode2_sub
        )

        # get number of actors in netlet_subset
        n_actual <- length(mode2_sub)
        n_netify <- ncol(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(mode2_sub))
        actors_netify <- sort(unique(colnames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual),
            list(n_netify, actors_netify)
        )
    }
)

test_that(
    "subset_netify: longitudinal array, bipartite, from+to subset check",
    {
        # create netify object and then subset
        netlet <- netify(
            icews_bipartite,
            actor1 = "i", actor2 = "j", time = "year",
            symmetric = FALSE, weight = "matlConf",
            mode = "bipartite",
            output_format = "longit_array"
        )

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            from = mode1_sub,
            to = mode2_sub
        )

        # get number of actors in netlet_subset
        n_actual_from <- length(mode1_sub)
        n_netify_from <- nrow(netlet_subset)
        n_actual_to <- length(mode2_sub)
        n_netify_to <- ncol(netlet_subset)

        # also check actors are present
        actors_actual_from <- sort(unique(mode1_sub))
        actors_netify_from <- sort(unique(rownames(netlet_subset)))
        actors_actual_to <- sort(unique(mode2_sub))
        actors_netify_to <- sort(unique(colnames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual_from, n_actual_to, actors_actual_from, actors_actual_to),
            list(n_netify_from, n_netify_to, actors_netify_from, actors_netify_to)
        )
    }
)

test_that(
    "subset_netify: cross-sectional, bipartite, from subset check",
    {
        # create netify object and then subset
        netlet <- netify(
            icews_bipartite_10,
            actor1 = "i", actor2 = "j",
            symmetric = FALSE, weight = "matlConf",
            mode = "bipartite"
        )

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            from = mode1_sub
        )

        # get number of actors in netlet_subset
        n_actual <- length(mode1_sub)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(mode1_sub))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual),
            list(n_netify, actors_netify)
        )
    }
)

test_that(
    "subset_netify: cross-sectional, bipartite, to subset check",
    {
        # create netify object and then subset
        netlet <- netify(
            icews_bipartite_10,
            actor1 = "i", actor2 = "j", time = "year",
            symmetric = FALSE, weight = "matlConf",
            mode = "bipartite"
        )

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            to = mode2_sub
        )

        # get number of actors in netlet_subset
        n_actual <- length(mode2_sub)
        n_netify <- ncol(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(mode2_sub))
        actors_netify <- sort(unique(colnames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual),
            list(n_netify, actors_netify)
        )
    }
)

test_that(
    "subset_netify: cross-sectional, bipartite, from+to subset check",
    {
        # create netify object and then subset
        netlet <- netify(
            icews_bipartite_10,
            actor1 = "i", actor2 = "j", time = "year",
            symmetric = FALSE, weight = "matlConf",
            mode = "bipartite"
        )

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            from = mode1_sub,
            to = mode2_sub
        )

        # get number of actors in netlet_subset
        n_actual_from <- length(mode1_sub)
        n_netify_from <- nrow(netlet_subset)
        n_actual_to <- length(mode2_sub)
        n_netify_to <- ncol(netlet_subset)

        # also check actors are present
        actors_actual_from <- sort(unique(mode1_sub))
        actors_netify_from <- sort(unique(rownames(netlet_subset)))
        actors_actual_to <- sort(unique(mode2_sub))
        actors_netify_to <- sort(unique(colnames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual_from, n_actual_to, actors_actual_from, actors_actual_to),
            list(n_netify_from, n_netify_to, actors_netify_from, actors_netify_to)
        )
    }
)
################################################

################################################
# check temporal subsetting for longitudinal nets
test_that(
    "subset_netify: longitudinal list, temporal subset check",
    {
        # create netify object and then subset
        netlet <- icews_matlConf_l

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            time = c("2010", "2011")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- Reduce("unique", lapply(netlet_subset, nrow))
        t_actual <- length(c("2010", "2011"))
        t_netify <- length(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(Reduce("unique", lapply(netlet_subset, rownames)))
        pds_actual <- sort(c("2010", "2011"))
        pds_netify <- sort(names(netlet_subset))

        # check that both are identical
        expect_identical(
            list(
                n_actual, t_actual,
                actors_actual, pds_actual, "longit_list"
            ),
            list(
                n_netify, t_netify,
                actors_netify, pds_netify, attr(netlet_subset, "netify_type")
            )
        )
    }
)

# check temporal subsetting for longitudinal nets
test_that(
    "subset_netify: longitudinal array, temporal subset check",
    {
        # create netify object and then subset
        netlet <- icews_matlConf_a

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            time = c("2010", "2011")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)
        t_actual <- length(c("2010", "2011"))
        t_netify <- dim(netlet_subset)[3]

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- rownames(netlet_subset)
        pds_actual <- sort(c("2010", "2011"))
        pds_netify <- dimnames(netlet_subset)[[3]]

        # check that both are identical
        expect_identical(
            list(
                n_actual, t_actual,
                actors_actual, pds_actual, "longit_array"
            ),
            list(
                n_netify, t_netify,
                actors_netify, pds_netify, attr(netlet_subset, "netify_type")
            )
        )
    }
)

# check that if only one pd selected then output is cross_sec
test_that(
    "subset_netify: longitudinal list, temporal subset check",
    {
        # create netify object and then subset
        netlet <- icews_matlConf_l

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            time = c("2010")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- rownames(netlet_subset)

        # check that both are identical
        expect_identical(
            list(
                n_actual, actors_actual, "cross_sec"
            ),
            list(
                n_netify, actors_netify, attr(netlet_subset, "netify_type")
            )
        )
    }
)

# check that if only one pd selected then output is cross_sec
test_that(
    "subset_netify: longitudinal array, temporal subset check",
    {
        # create netify object and then subset
        netlet <- icews_matlConf_a

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            time = c("2010")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- rownames(netlet_subset)

        # check that both are identical
        expect_identical(
            list(
                n_actual, actors_actual, "cross_sec"
            ),
            list(
                n_netify, actors_netify, attr(netlet_subset, "netify_type")
            )
        )
    }
)
################################################

################################################
# check multilayer cross-sectional nets

# check rows and cols
test_that(
    "subset_netify: multilayer longitudinal list, actor subset check",
    {
        # create netify object and then subset
        netlet <- icews_all_l

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- Reduce("unique", lapply(netlet_subset, nrow))

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(Reduce("unique", lapply(netlet_subset, rownames)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual, 4L),
            list(n_netify, actors_netify, length(attr(netlet_subset, "layers")))
        )
    }
)

# check rows and cols
test_that(
    "subset_netify: multilayer longitudinal array, actor subset check",
    {
        # create netify object and then subset
        netlet <- icews_all_a

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual, 4L),
            list(n_netify, actors_netify, length(attr(netlet_subset, "layers")))
        )
    }
)

# check rows and cols
test_that(
    "subset_netify: multilayer cross-sectional, actor subset check",
    {
        # create netify object and then subset
        netlet <- icews_all_10

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual, 4L),
            list(n_netify, actors_netify, length(attr(netlet_subset, "layers")))
        )
    }
)

# check rows and cols and time
test_that(
    "subset_netify: multilayer longitudinal list, actor and time subset check",
    {
        # create netify object and then subset
        netlet <- icews_all_l

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            time = c("2010", "2011")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- Reduce("unique", lapply(netlet_subset, nrow))
        t_actual <- 2L
        t_netify <- length(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(Reduce("unique", lapply(netlet_subset, rownames)))
        pds_actual <- c("2010", "2011")
        pds_netify <- sort(names(netlet_subset))

        # check that both are identical
        expect_identical(
            list(n_actual, t_actual, actors_actual, pds_actual, 4L),
            list(n_netify, t_netify, actors_netify, pds_netify, length(attr(netlet_subset, "layers")))
        )
    }
)

# check rows and cols and time
test_that(
    "subset_netify: multilayer longitudinal array, actor and time subset check",
    {
        # create netify object and then subset
        netlet <- icews_all_a

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            time = c("2010", "2011")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)
        t_actual <- 2L
        t_netify <- dim(netlet_subset)[[4]]

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(unique(rownames(netlet_subset)))
        pds_actual <- c("2010", "2011")
        pds_netify <- dimnames(netlet_subset)[[4]]

        # check that both are identical
        expect_identical(
            list(n_actual, t_actual, actors_actual, pds_actual, 4L),
            list(n_netify, t_netify, actors_netify, pds_netify, length(attr(netlet_subset, "layers")))
        )
    }
)

# check rows and cols and going from time to cross-sec
test_that(
    "subset_netify: multilayer longitudinal list, actor subset and go from longit to cross-sec",
    {
        # create netify object and then subset
        netlet <- icews_all_l

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            time = c("2010")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual, "cross_sec"),
            list(n_netify, actors_netify, attr(netlet_subset, "netify_type"))
        )
    }
)

# check rows and cols and going from time to cross-sec
test_that(
    "subset_netify: multilayer longitudinal array, actor subset and go from longit to cross-sec",
    {
        # create netify object and then subset
        netlet <- icews_all_a

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            time = c("2010")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual, "cross_sec"),
            list(n_netify, actors_netify, attr(netlet_subset, "netify_type"))
        )
    }
)

# check layers
test_that(
    "subset_netify: multilayer longitudinal list, layer subset check",
    {
        # create netify object and then subset
        netlet <- icews_all_l

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            layers = c("verbCoop", "matlCoop")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- Reduce("unique", lapply(netlet_subset, nrow))

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(Reduce("unique", lapply(netlet_subset, rownames)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual, 2L, c("verbCoop", "matlCoop")),
            list(n_netify, actors_netify, length(attr(netlet_subset, "layers")), attr(netlet_subset, "layers"))
        )
    }
)

# check layers
test_that(
    "subset_netify: multilayer longitudinal array, layer subset check",
    {
        # create netify object and then subset
        netlet <- icews_all_a

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            layers = c("verbCoop", "matlCoop")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual, 2L, c("verbCoop", "matlCoop")),
            list(n_netify, actors_netify, length(attr(netlet_subset, "layers")), attr(netlet_subset, "layers"))
        )
    }
)

# check layers
test_that(
    "subset_netify: multilayer cross-sectional, layer subset check",
    {
        # create netify object and then subset
        netlet <- icews_all_10

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            layers = c("verbCoop", "matlCoop")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual, 2L, c("verbCoop", "matlCoop")),
            list(n_netify, actors_netify, length(attr(netlet_subset, "layers")), attr(netlet_subset, "layers"))
        )
    }
)

# multilayer to single layer
test_that(
    "subset_netify: multilayer longitudinal list, multilayer to single layer",
    {
        # create netify object and then subset
        netlet <- icews_all_l

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            layers = c("verbCoop")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- Reduce("unique", lapply(netlet_subset, nrow))

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(Reduce("unique", lapply(netlet_subset, rownames)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual, 1L, c("verbCoop")),
            list(n_netify, actors_netify, length(attr(netlet_subset, "layers")), attr(netlet_subset, "layers"))
        )
    }
)

# multilayer to single layer
test_that(
    "subset_netify: multilayer longitudinal array, multilayer to single layer",
    {
        # create netify object and then subset
        netlet <- icews_all_a

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            layers = c("verbCoop")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual, 1L, c("verbCoop")),
            list(n_netify, actors_netify, length(attr(netlet_subset, "layers")), attr(netlet_subset, "layers"))
        )
    }
)

# multilayer to single layer
test_that(
    "subset_netify: multilayer cross-sectional, multilayer to single layer",
    {
        # create netify object and then subset
        netlet <- icews_all_10

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            layers = c("verbCoop")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual, 1L, c("verbCoop")),
            list(n_netify, actors_netify, length(attr(netlet_subset, "layers")), attr(netlet_subset, "layers"))
        )
    }
)

# multilayer longit to cross-sec
test_that(
    "subset_netify: multilayer longitudinal list to single layer cross-sec",
    {
        # create netify object and then subset
        netlet <- icews_all_l

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            time = "2010",
            layers = c("verbCoop")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual, 1L, c("verbCoop")),
            list(n_netify, actors_netify, length(attr(netlet_subset, "layers")), attr(netlet_subset, "layers"))
        )
    }
)

# multilayer to single layer
test_that(
    "subset_netify: multilayer longitudinal array to single layer cross-sec",
    {
        # create netify object and then subset
        netlet <- icews_all_a

        # subset to select countries
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            time = "2010",
            layers = c("verbCoop")
        )

        # get number of actors in netlet_subset
        n_actual <- length(actors_to_keep)
        n_netify <- nrow(netlet_subset)

        # also check actors are present
        actors_actual <- sort(unique(actors_to_keep))
        actors_netify <- sort(unique(rownames(netlet_subset)))

        # check that both are identical
        expect_identical(
            list(n_actual, actors_actual, 1L, c("verbCoop")),
            list(n_netify, actors_netify, length(attr(netlet_subset, "layers")), attr(netlet_subset, "layers"))
        )
    }
)
################################################

test_that(
    "subset_netify: longitudinal list remains longitudinal list after subset",
    {
        # create netify object
        netlet <- icews_matlConf_l

        # subset to select countries and multiple time periods
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            time = c("2002", "2003", "2004") # 3 time periods to ensure it stays longitudinal
        )

        # verify it's still a longitudinal list
        expect_equal(attr(netlet_subset, "netify_type"), "longit_list")

        # verify it's a list
        expect_true(is.list(netlet_subset))

        # verify correct number of time periods
        expect_equal(length(netlet_subset), 3L)

        # verify time period names are correct
        expect_equal(sort(names(netlet_subset)), c("2002", "2003", "2004"))

        # verify each element is a proper netify matrix
        for (t in names(netlet_subset)) {
            # check class
            expect_true(inherits(netlet_subset[[t]], "netify"))

            # check it's a matrix
            expect_true(is.matrix(netlet_subset[[t]]))

            # check dimensions
            expect_equal(nrow(netlet_subset[[t]]), length(actors_to_keep))
            expect_equal(ncol(netlet_subset[[t]]), length(actors_to_keep))

            # check actor names
            expect_equal(sort(rownames(netlet_subset[[t]])), sort(actors_to_keep))
            expect_equal(sort(colnames(netlet_subset[[t]])), sort(actors_to_keep))

            # verify each element has proper netify attributes
            expect_true(!is.null(attr(netlet_subset[[t]], "netify_type")))
            expect_equal(attr(netlet_subset[[t]], "netify_type"), "cross_sec")
        }

        # verify list-level attributes are preserved
        expect_true(!is.null(attr(netlet_subset, "symmetric")))
        expect_true(!is.null(attr(netlet_subset, "mode")))
        expect_true(!is.null(attr(netlet_subset, "weight")))

        # verify actor_pds is properly updated
        actor_pds <- attr(netlet_subset, "actor_pds")
        expect_true(!is.null(actor_pds))
        expect_equal(sort(unique(actor_pds$actor)), sort(actors_to_keep))
    }
)


test_that(
    "subset_netify: nodal and dyadic attributes are preserved and filtered correctly",
    {
        # create netify object with attributes
        netlet <- netify(
            icews,
            actor1 = "i", actor2 = "j", time = "year",
            symmetric = FALSE, weight = "matlConf",
            nodal_vars = c("i_polity2", "i_log_gdp", "i_log_pop"),
            dyad_vars = c("matlCoop", "verbCoop", "verbConf"),
            dyad_vars_symmetric = c(FALSE, FALSE, FALSE)
        )

        # subset
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = actors_to_keep,
            time = c("2010", "2011")
        )

        # check nodal data
        nodal_data <- attr(netlet_subset, "nodal_data")
        expect_true(!is.null(nodal_data))
        expect_equal(sort(unique(nodal_data$actor)), sort(actors_to_keep))

        # check dyadic data structure
        dyad_data <- attr(netlet_subset, "dyad_data")
        expect_true(!is.null(dyad_data))

        # check dyadic data dimensions
        for (t in c("2010", "2011")) {
            for (var in c("matlCoop", "verbCoop", "verbConf")) {
                expect_equal(dim(dyad_data[[t]][[var]]), c(length(actors_to_keep), length(actors_to_keep)))
                expect_equal(rownames(dyad_data[[t]][[var]]), actors_to_keep)
                expect_equal(colnames(dyad_data[[t]][[var]]), actors_to_keep)
            }
        }
    }
)

test_that(
    "subset_netify: single actor subset works correctly",
    {
        netlet <- icews_matlConf_10

        # subset to single actor
        netlet_subset <- subset_netify(
            netlet = netlet,
            actors = "United States"
        )

        # check dimensions
        expect_equal(dim(netlet_subset), c(1, 1))
        expect_equal(rownames(netlet_subset), "United States")
        expect_equal(colnames(netlet_subset), "United States")

        # verify it's still a valid netify object
        expect_true(inherits(netlet_subset, "netify"))
    }
)
