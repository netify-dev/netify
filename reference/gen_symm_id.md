# Generate symmetric identifiers for dyadic data

`gen_symm_id` creates symmetric identifiers for dyadic data, ensuring
that each unique pair of actors receives the same ID regardless of
order. This is particularly useful for undirected network data where the
relationship between actors A and B is identical to the relationship
between B and A.

## Usage

``` r
gen_symm_id(dyad_data, actor1, actor2, time = NULL)
```

## Arguments

- dyad_data:

  A data.frame containing dyadic data with at least two columns
  representing actors in each dyad. Will be coerced to data.frame if a
  tibble or data.table is provided.

- actor1:

  Character string specifying the column name for the first actor in
  each dyad.

- actor2:

  Character string specifying the column name for the second actor in
  each dyad.

- time:

  Character string specifying the column name for time periods. If
  provided, the time value will be appended to the symmetric ID to
  create unique identifiers for each time period. Set to NULL (default)
  for cross-sectional data.

## Value

A character vector of symmetric identifiers with the same length as the
number of rows in dyad_data. Each ID is formatted as:

- **Without time**: "actor1_actor2" (alphabetically sorted)

- **With time**: "actor1_actor2_time" (actors alphabetically sorted)

## Details

The function ensures symmetry by alphabetically sorting actor names
before creating the identifier. This guarantees that:

- The dyad "USA-China" receives the same ID as "China-USA"

- The dyad "Brazil-Argentina" receives the same ID as "Argentina-Brazil"

- Actor pairs are consistently ordered regardless of input order

When a time column is specified, it's appended to the symmetric ID to
maintain unique identifiers across different time periods. This allows
for proper aggregation of longitudinal dyadic data while preserving
temporal variation.

## Note

All actor values are converted to character strings before creating IDs
to ensure consistent sorting behavior across different data types.

The underscore character ("\_") is used as a separator in the IDs. If
your actor names contain underscores, the IDs will still be unique but
may be harder to parse visually.

This function is primarily used internally by `aggregate_dyad` for
efficient symmetric aggregation, but can be used independently for
creating symmetric dyad identifiers.

## Author

Shahryar Minhas

## Examples

``` r
# Create example dyadic data
trade_df <- data.frame(
    from = c("USA", "China", "Russia", "USA", "Brazil", "Argentina"),
    to = c("China", "USA", "USA", "Russia", "Argentina", "Brazil"),
    trade_value = c(100, 100, 50, 75, 30, 25),
    year = c(2020, 2020, 2021, 2021, 2021, 2021)
)

# Generate symmetric IDs without time
trade_df$symm_id <- gen_symm_id(trade_df, "from", "to")
print(trade_df[, c("from", "to", "symm_id")])
#>        from        to          symm_id
#> 1       USA     China        China_USA
#> 2     China       USA        China_USA
#> 3    Russia       USA       Russia_USA
#> 4       USA    Russia       Russia_USA
#> 5    Brazil Argentina Argentina_Brazil
#> 6 Argentina    Brazil Argentina_Brazil
# Note: USA-China and China-USA both get "China_USA"

# Generate symmetric IDs with time
trade_df$symm_id_time <- gen_symm_id(trade_df, "from", "to", "year")
print(trade_df[, c("from", "to", "year", "symm_id_time")])
#>        from        to year          symm_id_time
#> 1       USA     China 2020        China_USA_2020
#> 2     China       USA 2020        China_USA_2020
#> 3    Russia       USA 2021       Russia_USA_2021
#> 4       USA    Russia 2021       Russia_USA_2021
#> 5    Brazil Argentina 2021 Argentina_Brazil_2021
#> 6 Argentina    Brazil 2021 Argentina_Brazil_2021
# Note: USA-China in 2020 gets "China_USA_2020"

# Use for aggregation of undirected relationships
trade_df$total_trade <- ave(
    trade_df$trade_value,
    trade_df$symm_id_time,
    FUN = sum
)
print(unique(trade_df[, c("symm_id_time", "total_trade")]))
#>            symm_id_time total_trade
#> 1        China_USA_2020         200
#> 3       Russia_USA_2021         125
#> 5 Argentina_Brazil_2021          55

# Example with longitudinal data
library(netify)
data(icews)
icews_sample <- icews[1:100, ]

# Create symmetric IDs for conflict events
icews_sample$symm_dyad <- gen_symm_id(
    icews_sample,
    actor1 = "i",
    actor2 = "j",
    time = "year"
)

# Check that symmetric pairs get same ID
icews_sample[icews_sample$i == "United States" & icews_sample$j == "Israel", "symm_dyad"]
#> character(0)
icews_sample[icews_sample$i == "Israel" & icews_sample$j == "United States", "symm_dyad"]
#> character(0)
```
