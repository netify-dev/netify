set.seed(6886)

# library(testthat)
# library(netify)

test_that("ego_netify works for single ego in cross-sectional network", {
    # create simple test data
    test_data <- data.frame(
        from = c("A", "A", "B", "B", "C", "D"),
        to = c("B", "C", "A", "C", "A", "B"),
        weight = c(10, 20, 30, 40, 50, 60),
        stringsAsFactors = FALSE
    )
    
    # create netify object
    net <- netify(
        test_data,
        actor1 = "from",
        actor2 = "to",
        weight = "weight",
        symmetric = FALSE
    )
    
    # extract ego network for actor a
    ego_a <- ego_netify(net, ego = "A")
    
    # check that it's a netify object
    expect_s3_class(ego_a, "netify")
    
    # check ego network attributes
    expect_equal(attr(ego_a, "ego_id"), "A")
    expect_equal(attr(ego_a, "ego_vec"), "A")
    
    # check actors in ego network (a and its neighbors: b, c)
    actors <- rownames(ego_a)
    expect_equal(sort(actors), c("A", "B", "C"))
    
    # check that d is not included (not connected to a)
    expect_false("D" %in% actors)
})

test_that("ego_netify works with multiple egos and direction options", {
    # create test data with clear directional relationships
    test_data <- data.frame(
        from = c("A", "A", "B", "C", "D", "D"),
        to = c("B", "C", "A", "A", "A", "B"),
        weight = c(1, 1, 1, 1, 1, 1),
        stringsAsFactors = FALSE
    )
    
    # create netify object
    net <- netify(
        test_data,
        actor1 = "from",
        actor2 = "to",
        symmetric = FALSE
    )
    
    # test outgoing direction for a
    ego_out <- ego_netify(net, ego = "A", ngbd_direction = "out")
    actors_out <- rownames(ego_out)
    # a has outgoing ties to b and c
    expect_equal(sort(actors_out), c("A", "B", "C"))
    
    # test incoming direction for a  
    ego_in <- ego_netify(net, ego = "A", ngbd_direction = "in")
    actors_in <- rownames(ego_in)
    # a has incoming ties from b, c, and d
    expect_equal(sort(actors_in), c("A", "B", "C", "D"))
    
    # test any direction (default)
    ego_any <- ego_netify(net, ego = "A", ngbd_direction = "any")
    actors_any <- rownames(ego_any)
    # a has any ties with b, c, and d
    expect_equal(sort(actors_any), c("A", "B", "C", "D"))
    
    # test multiple egos
    multi_ego <- ego_netify(net, ego = c("A", "B"))
    expect_type(multi_ego, "list")
    expect_equal(length(multi_ego), 2)
    expect_equal(names(multi_ego), c("A", "B"))
})

test_that("ego_netify works for longitudinal networks with thresholds", {
    # create longitudinal test data
    test_data <- data.frame(
        from = rep(c("A", "A", "B", "C", "D"), 2),
        to = rep(c("B", "C", "A", "A", "B"), 2),
        time = c(rep(2010, 5), rep(2011, 5)),
        weight = c(10, 5, 20, 15, 25, 30, 2, 40, 35, 45),
        stringsAsFactors = FALSE
    )
    
    # create longitudinal netify object
    net_longit <- netify(
        test_data,
        actor1 = "from",
        actor2 = "to",
        time = "time",
        weight = "weight",
        symmetric = FALSE
    )
    
    # extract ego network without threshold
    ego_longit <- ego_netify(net_longit, ego = "A")
    
    # check output is a list with correct names
    expect_type(ego_longit, "list")
    expect_equal(names(ego_longit), c("A__2010", "A__2011"))
    
    # check each time period is a netify object
    expect_s3_class(ego_longit[["A__2010"]], "netify")
    expect_s3_class(ego_longit[["A__2011"]], "netify")
    
    # test with threshold
    ego_thresh <- ego_netify(net_longit, ego = "A", threshold = 15)
    
    # in 2010: a-b (10) < 15, a-c (5) < 15, b-a (20) > 15, c-a (15) = 15
    # only b should be included as neighbor (b->a weight is 20)
    actors_2010 <- rownames(ego_thresh[["A__2010"]])
    expect_equal(sort(actors_2010), c("A", "B"))
    
    # in 2011: a-b (30) > 15, a-c (2) < 15, b-a (40) > 15, c-a (35) > 15
    # both b and c should be included
    actors_2011 <- rownames(ego_thresh[["A__2011"]])
    expect_equal(sort(actors_2011), c("A", "B", "C"))
    
    # test exclude ego option
    ego_no_self <- ego_netify(net_longit, ego = "A", include_ego = FALSE)
    actors_no_self <- rownames(ego_no_self[["A__2010"]])
    expect_false("A" %in% actors_no_self)
    expect_true(all(c("B", "C") %in% actors_no_self))
})