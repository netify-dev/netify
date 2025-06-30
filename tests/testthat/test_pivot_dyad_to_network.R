test_that("pivot_dyad_to_network works for cross-sectional networks", {
    # Create test data
    test_data <- data.frame(
        i = c("A", "A", "B", "B", "C", "C"),
        j = c("B", "C", "A", "C", "A", "B"),
        trade = c(100, 200, 150, 300, 250, 350),
        fdi = c(10, 20, 15, 30, 25, 35)
    )

    # Create netify object with trade as network
    net <- netify(
        test_data,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "trade"
    )

    # Add FDI as dyadic variable
    net <- add_dyad_vars(
        net,
        test_data,
        actor1 = "i", actor2 = "j",
        dyad_vars = "fdi",
        dyad_vars_symmetric = FALSE
    )

    # Test basic pivot
    net_pivoted <- pivot_dyad_to_network(
        net,
        dyad_var = "fdi",
        network_var_name = "trade"
    )

    # Check that FDI is now the network
    expect_equal(attr(net_pivoted, "weight"), "fdi")
    expect_equal(net_pivoted["A", "B"], 10)
    expect_equal(net_pivoted["B", "C"], 30)

    # Check that trade is preserved as dyadic variable
    trade_matrix <- attr(net_pivoted, "dyad_data")[["1"]][["trade"]]
    expect_equal(trade_matrix["A", "B"], 100)
    expect_equal(trade_matrix["B", "C"], 300)

    # Check that FDI is removed from dyad_data
    expect_null(attr(net_pivoted, "dyad_data")[["1"]][["fdi"]])
})

test_that("pivot_dyad_to_network detects symmetry correctly", {
    # Create symmetric test data
    test_data <- data.frame(
        i = c("A", "A", "B", "B", "C", "C"),
        j = c("B", "C", "A", "C", "A", "B"),
        network = c(100, 200, 100, 300, 200, 300), # symmetric
        dyad_asym = c(10, 20, 15, 30, 25, 35), # asymmetric
        dyad_sym = c(50, 60, 50, 70, 60, 70) # symmetric
    )

    # Create symmetric netify object
    net <- netify(
        test_data,
        actor1 = "i", actor2 = "j",
        symmetric = TRUE,
        weight = "network"
    )

    # Add dyadic variables
    net <- add_dyad_vars(
        net,
        test_data,
        actor1 = "i", actor2 = "j",
        dyad_vars = c("dyad_asym", "dyad_sym"),
        dyad_vars_symmetric = c(FALSE, TRUE)
    )

    # Test auto-detection of asymmetric variable
    expect_message(
        net_asym <- pivot_dyad_to_network(net, dyad_var = "dyad_asym"),
        "Auto-detected symmetry"
    )
    expect_false(attr(net_asym, "symmetric"))

    # Test auto-detection of symmetric variable
    expect_message(
        net_sym <- pivot_dyad_to_network(net, dyad_var = "dyad_sym"),
        "Auto-detected symmetry"
    )
    expect_true(attr(net_sym, "symmetric"))
})

test_that("pivot_dyad_to_network works for longitudinal arrays", {
    # Create longitudinal test data
    test_data <- data.frame(
        i = rep(c("A", "A", "B", "B", "C", "C"), 2),
        j = rep(c("B", "C", "A", "C", "A", "B"), 2),
        year = rep(c(2020, 2021), each = 6),
        trade = c(100, 200, 150, 300, 250, 350, 110, 210, 160, 310, 260, 360),
        fdi = c(10, 20, 15, 30, 25, 35, 11, 21, 16, 31, 26, 36)
    )

    # Create longitudinal netify object
    net <- netify(
        test_data,
        actor1 = "i", actor2 = "j", time = "year",
        symmetric = FALSE,
        weight = "trade",
        output_format = "longit_array"
    )

    # Add FDI as dyadic variable
    net <- add_dyad_vars(
        net,
        test_data,
        actor1 = "i", actor2 = "j", time = "year",
        dyad_vars = "fdi",
        dyad_vars_symmetric = FALSE
    )

    # Pivot to FDI
    net_pivoted <- pivot_dyad_to_network(
        net,
        dyad_var = "fdi",
        make_network_dyad_var = TRUE
    )

    # Check 2020 values
    expect_equal(net_pivoted["A", "B", 1], 10)
    expect_equal(net_pivoted["B", "C", 1], 30)

    # Check 2021 values
    expect_equal(net_pivoted["A", "B", 2], 11)
    expect_equal(net_pivoted["B", "C", 2], 31)

    # Check that trade is preserved
    trade_2020 <- attr(net_pivoted, "dyad_data")[["2020"]][["trade"]]
    expect_equal(trade_2020["A", "B"], 100)

    trade_2021 <- attr(net_pivoted, "dyad_data")[["2021"]][["trade"]]
    expect_equal(trade_2021["A", "B"], 110)
})

test_that("pivot_dyad_to_network works for longitudinal lists", {
    # Create longitudinal test data with changing actors
    test_data <- data.frame(
        i = c("A", "A", "B", "B", "A", "A", "B", "B", "C", "C"),
        j = c("B", "C", "A", "C", "B", "D", "A", "D", "A", "B"),
        year = c(2020, 2020, 2020, 2020, 2021, 2021, 2021, 2021, 2021, 2021),
        trade = c(100, 200, 150, 300, 110, 400, 160, 410, 500, 510),
        fdi = c(10, 20, 15, 30, 11, 40, 16, 41, 50, 51)
    )

    # Create longitudinal list netify object
    net <- netify(
        test_data,
        actor1 = "i", actor2 = "j", time = "year",
        symmetric = FALSE,
        weight = "trade",
        actor_time_uniform = FALSE
    )

    # Add FDI as dyadic variable
    net <- add_dyad_vars(
        net,
        test_data,
        actor1 = "i", actor2 = "j", time = "year",
        dyad_vars = "fdi",
        dyad_vars_symmetric = FALSE
    )

    # Pivot to FDI
    net_pivoted <- pivot_dyad_to_network(
        net,
        dyad_var = "fdi",
        network_var_name = "trade"
    )

    # Check 2020 values
    expect_equal(net_pivoted[["2020"]]["A", "B"], 10)
    expect_equal(net_pivoted[["2020"]]["B", "C"], 30)

    # Check 2021 values (with new actor D)
    expect_equal(net_pivoted[["2021"]]["A", "D"], 40)
    expect_equal(net_pivoted[["2021"]]["C", "A"], 50)

    # Check dimensions match original
    expect_equal(dim(net_pivoted[["2020"]]), dim(net[["2020"]]))
    expect_equal(dim(net_pivoted[["2021"]]), dim(net[["2021"]]))
})

test_that("pivot_dyad_to_network handles bipartite networks", {
    # Create bipartite test data
    test_data <- data.frame(
        i = c("A", "A", "B", "B"),
        j = c("X", "Y", "X", "Y"),
        trade = c(100, 200, 300, 400),
        fdi = c(10, 20, 30, 40)
    )

    # Create bipartite netify object
    net <- netify(
        test_data,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        mode = "bipartite",
        weight = "trade"
    )

    # Add FDI as dyadic variable
    net <- add_dyad_vars(
        net,
        test_data,
        actor1 = "i", actor2 = "j",
        dyad_vars = "fdi",
        dyad_vars_symmetric = FALSE
    )

    # Try to pivot with symmetric=TRUE (should warn and set to FALSE)
    expect_warning(
        net_pivoted <- pivot_dyad_to_network(
            net,
            dyad_var = "fdi",
            symmetric = TRUE
        ),
        "Bipartite networks must be asymmetric"
    )

    expect_false(attr(net_pivoted, "symmetric"))
    expect_equal(net_pivoted["A", "X"], 10)
    expect_equal(net_pivoted["B", "Y"], 40)
})

test_that("pivot_dyad_to_network handles missing dyadic variables correctly", {
    # Create test data
    test_data <- data.frame(
        i = c("A", "A", "B"),
        j = c("B", "C", "C"),
        trade = c(100, 200, 300)
    )

    # Create netify object without dyadic variables
    net <- netify(
        test_data,
        actor1 = "i", actor2 = "j",
        weight = "trade"
    )

    # Test error when no dyadic variables exist
    expect_error(
        pivot_dyad_to_network(net, dyad_var = "fdi"),
        "No dyadic variables found"
    )

    # Add a dyadic variable
    net <- add_dyad_vars(
        net,
        data.frame(i = "A", j = "B", investment = 50),
        actor1 = "i", actor2 = "j",
        dyad_vars = "investment"
    )

    # Test error when requested variable doesn't exist
    expect_error(
        pivot_dyad_to_network(net, dyad_var = "fdi"),
        "Dyadic variable 'fdi' not found"
    )
})

test_that("pivot_dyad_to_network preserves all attributes correctly", {
    # Create test data with nodal attributes
    test_data <- data.frame(
        i = c("A", "A", "B", "B", "C", "C"),
        j = c("B", "C", "A", "C", "A", "B"),
        trade = c(100, 200, 150, 300, 250, 350),
        fdi = c(10, 20, 15, 30, 25, 35)
    )

    node_data <- data.frame(
        actor = c("A", "B", "C"),
        gdp = c(1000, 2000, 1500),
        population = c(50, 100, 75)
    )

    # Create netify object
    net <- netify(
        test_data,
        actor1 = "i", actor2 = "j",
        symmetric = FALSE,
        weight = "trade"
    )

    # Add nodal attributes
    net <- add_node_vars(
        net,
        node_data,
        actor = "actor",
        node_vars = c("gdp", "population")
    )

    # Add multiple dyadic variables
    net <- add_dyad_vars(
        net,
        test_data,
        actor1 = "i", actor2 = "j",
        dyad_vars = "fdi",
        dyad_vars_symmetric = FALSE
    )

    # Add another dyadic variable
    net <- add_dyad_vars(
        net,
        data.frame(
            i = c("A", "B", "C"),
            j = c("B", "C", "A"),
            alliance = c(1, 0, 1)
        ),
        actor1 = "i", actor2 = "j",
        dyad_vars = "alliance",
        dyad_vars_symmetric = TRUE
    )

    # Pivot to FDI
    net_pivoted <- pivot_dyad_to_network(
        net,
        dyad_var = "fdi",
        make_network_dyad_var = FALSE # Don't preserve old network
    )

    # Check that nodal attributes are preserved
    expect_equal(attr(net_pivoted, "nodal_data"), attr(net, "nodal_data"))

    # Check that other dyadic variables are preserved
    expect_false(is.null(attr(net_pivoted, "dyad_data")[["1"]][["alliance"]]))
    expect_equal(
        attr(net_pivoted, "dyad_data")[["1"]][["alliance"]],
        attr(net, "dyad_data")[["1"]][["alliance"]]
    )

    # Check that FDI is removed from dyad_data
    expect_null(attr(net_pivoted, "dyad_data")[["1"]][["fdi"]])
})

test_that("pivot_dyad_to_network handles custom attribute settings", {
    # Create test data
    test_data <- data.frame(
        i = c("A", "A", "B"),
        j = c("B", "C", "C"),
        trade = c(100, 200, 300),
        fdi = c(10, 20, 30)
    )

    # Create netify object with specific settings
    net <- netify(
        test_data,
        actor1 = "i", actor2 = "j",
        weight = "trade",
        diag_to_NA = FALSE,
        missing_to_zero = FALSE
    )

    # Add FDI
    net <- add_dyad_vars(
        net,
        test_data,
        actor1 = "i", actor2 = "j",
        dyad_vars = "fdi"
    )

    # Test with custom settings
    net_pivoted <- pivot_dyad_to_network(
        net,
        dyad_var = "fdi",
        weight_type = "Foreign Direct Investment",
        diag_to_NA = TRUE,
        missing_to_zero = TRUE
    )

    # Check custom attributes
    expect_equal(attr(net_pivoted, "detail_weight"), "Weighted: Foreign Direct Investment (fdi)")
    expect_true(attr(net_pivoted, "diag_to_NA"))
    expect_true(attr(net_pivoted, "missing_to_zero"))
})
