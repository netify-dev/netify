set.seed(6886) 

# library(netify)
# library(testthat)

################################################
# Test basic directed aggregation
test_that("aggregate_dyad: basic directed aggregation works", {
    
    # Create test data with repeated dyads
    test_data <- data.frame(
        from = c("A", "A", "A", "B", "B", "C"),
        to = c("B", "B", "C", "A", "C", "A"),
        value = c(10, 20, 30, 40, 50, 60)
    )
    
    # Aggregate
    result <- aggregate_dyad(
        dyad_data = test_data,
        actor1 = "from",
        actor2 = "to",
        weight = "value",
        symmetric = FALSE
    )
    
    # Check correct aggregation
    expect_equal(nrow(result), 5)  # 5 unique directed dyads
    expect_equal(
        result$value[result$from == "A" & result$to == "B"], 
        30  # 10 + 20
    )
    expect_equal(
        result$value[result$from == "B" & result$to == "A"], 
        40  # stays 40 (directed)
    )
})

################################################
# Test symmetric aggregation
test_that("aggregate_dyad: symmetric aggregation works", {
    
    # Create test data
    test_data <- data.frame(
        actor1 = c("USA", "China", "USA", "Russia"),
        actor2 = c("China", "USA", "Russia", "USA"),
        trade = c(100, 80, 50, 30)
    )
    
    # Aggregate symmetrically
    result <- aggregate_dyad(
        dyad_data = test_data,
        actor1 = "actor1",
        actor2 = "actor2",
        weight = "trade",
        symmetric = TRUE
    )
    
    # Check symmetric aggregation
    expect_equal(nrow(result), 4)  # 2 unique undirected dyads × 2 directions
    
    # USA-China should equal China-USA = 180 (100 + 80)
    usa_china <- result$trade[result$actor1 == "USA" & result$actor2 == "China"]
    china_usa <- result$trade[result$actor1 == "China" & result$actor2 == "USA"]
    expect_equal(usa_china, 180)
    expect_equal(china_usa, 180)
    expect_equal(usa_china, china_usa)
})

################################################
# Test temporal aggregation
test_that("aggregate_dyad: temporal aggregation works", {
    
    # Create temporal data
    test_data <- data.frame(
        sender = c("A", "A", "A", "A", "B", "B"),
        receiver = c("B", "B", "B", "B", "A", "A"),
        year = c(2020, 2020, 2021, 2021, 2020, 2021),
        events = c(5, 3, 10, 2, 4, 6)
    )
    
    # Aggregate with time
    result <- aggregate_dyad(
        dyad_data = test_data,
        actor1 = "sender",
        actor2 = "receiver",
        time = "year",
        weight = "events",
        symmetric = FALSE
    )
    
    # Check temporal aggregation
    expect_equal(nrow(result), 4)  # 2 dyads × 2 years
    
    # A->B in 2020 should be 8 (5 + 3)
    ab_2020 <- result$events[
        result$sender == "A" & 
        result$receiver == "B" & 
        result$year == 2020
    ]
    expect_equal(ab_2020, 8)
    
    # A->B in 2021 should be 12 (10 + 2)
    ab_2021 <- result$events[
        result$sender == "A" & 
        result$receiver == "B" & 
        result$year == 2021
    ]
    expect_equal(ab_2021, 12)
})

################################################
# Test missing value handling
test_that("aggregate_dyad: missing value handling works", {

    # Create data with NAs
    test_data <- data.frame(
        i = c("A", "A", "A", "B"),
        j = c("B", "B", "C", "C"),
        weight = c(10, NA, 20, 30)
    )
    
    # Test ignore_missing = TRUE (default)
    result_ignore <- aggregate_dyad(
        dyad_data = test_data,
        actor1 = "i",
        actor2 = "j",
        weight = "weight",
        symmetric = FALSE,
        ignore_missing = TRUE
    )
    
    # A->B should be 10 (ignoring NA)
    expect_equal(
        result_ignore$weight[result_ignore$i == "A" & result_ignore$j == "B"],
        10
    )
    
    # Test ignore_missing = FALSE
    result_na <- aggregate_dyad(
        dyad_data = test_data,
        actor1 = "i",
        actor2 = "j",
        weight = "weight",
        symmetric = FALSE,
        ignore_missing = FALSE
    )
    
    # A->B should be NA (because one value was NA)
    expect_true(
        is.na(result_na$weight[result_na$i == "A" & result_na$j == "B"])
    )
})

################################################
# Test column preservation
test_that("aggregate_dyad: preserves original column names", {
    
    # Create data with custom column names
    test_data <- data.frame(
        exporter = c("Mexico", "Mexico", "Canada"),
        importer = c("USA", "USA", "USA"),
        trade_value = c(100, 200, 300)
    )
    
    # Aggregate
    result <- aggregate_dyad(
        dyad_data = test_data,
        actor1 = "exporter",
        actor2 = "importer",
        weight = "trade_value",
        symmetric = FALSE
    )
    
    # Check column names are preserved
    expect_equal(names(result), c("exporter", "importer", "trade_value"))
})

################################################
# Test self-loops in symmetric aggregation
test_that("aggregate_dyad: handles self-loops correctly in symmetric case", {
    
    # Create data with self-loops
    test_data <- data.frame(
        from = c("A", "A", "B", "B"),
        to = c("A", "B", "A", "B"),
        count = c(10, 20, 30, 40)
    )
    
    # Aggregate symmetrically
    result <- aggregate_dyad(
        dyad_data = test_data,
        actor1 = "from",
        actor2 = "to",
        weight = "count",
        symmetric = TRUE
    )
    
    # Check self-loops appear only once
    self_loops <- result[result$from == result$to, ]
    expect_equal(nrow(self_loops), 2)  # A-A and B-B
    expect_equal(self_loops$count[self_loops$from == "A"], 10)
    expect_equal(self_loops$count[self_loops$from == "B"], 40)
    
    # Check A-B = B-A = 50 (20 + 30)
    ab_count <- result$count[result$from == "A" & result$to == "B"]
    ba_count <- result$count[result$from == "B" & result$to == "A"]
    expect_equal(ab_count, 50)
    expect_equal(ba_count, 50)
})

################################################