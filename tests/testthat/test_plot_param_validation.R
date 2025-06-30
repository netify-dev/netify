library(netify)

test_that("plot parameter validation catches common mistakes", {
    # Create a simple network for testing
    mat <- matrix(rbinom(100, 1, 0.3), 10, 10)
    df <- data.frame(
        i = rep(1:10, each = 10),
        j = rep(1:10, times = 10),
        weight = as.vector(mat)
    )
    net <- netify(df[df$weight > 0,], actor1 = "i", actor2 = "j")
    
    # Test 1: Catch plural form of highlight_color
    expect_warning(
        plot(net, highlight = c("1", "2"), highlight_colors = c("1" = "red", "2" = "blue")),
        regexp = "highlight_colors.*typo.*highlight_color"
    )
    
    # Test 2: Catch British spelling
    expect_warning(
        plot(net, node_colour = "red"),
        regexp = "node_colour.*typo.*node_color"
    )
    
    # Test 3: Catch completely unknown parameter
    expect_warning(
        plot(net, random_unknown_param = "value"),
        regexp = "Unknown parameter.*random_unknown_param"
    )
    
    # Test 4: Fuzzy matching for close typos
    expect_warning(
        plot(net, higlight = c("1")),
        regexp = "higlight.*Did you mean.*highlight"
    )
    
    # Test 5: No warning for correct parameters
    expect_no_warning(
        plot(net, 
            highlight = c("1", "2"),
            highlight_color = c("1" = "red", "2" = "blue", "Other" = "grey"),
            node_size = 5,
            edge_color = "grey70"
        )
    )
})

test_that("highlight_color parameter works correctly", {
    # Create a simple network
    mat <- matrix(rbinom(100, 1, 0.3), 10, 10)
    df <- data.frame(
        i = rep(1:10, each = 10),
        j = rep(1:10, times = 10),
        weight = as.vector(mat)
    )
    net <- netify(df[df$weight > 0,], actor1 = "i", actor2 = "j")
    
    # Test that highlight_color (singular) is accepted without warning
    p <- plot(net, 
        highlight = c("1", "2"),
        highlight_color = c("1" = "#01411cff", "2" = "#ff0000", "Other" = "grey80")
    )
    
    # Check that the plot object is created
    expect_s3_class(p, "gg")
})