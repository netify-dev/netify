# generate symmetric identifiers for dyadic data

`gen_symm_id` creates symmetric identifiers for dyadic data, ensuring
that each unique pair of actors receives the same id regardless of
order. this is particularly useful for undirected network data where the
relationship between actors a and b is identical to the relationship
between b and a.

## Usage

``` r
gen_symm_id(dyad_data, actor1, actor2, time = NULL)
```

## Arguments

- dyad_data:

  a data.frame containing dyadic data with at least two columns
  representing actors in each dyad. will be coerced to data.frame if a
  tibble or data.table is provided.

- actor1:

  character string specifying the column name for the first actor in
  each dyad.

- actor2:

  character string specifying the column name for the second actor in
  each dyad.

- time:

  character string specifying the column name for time periods. if
  provided, the time value will be appended to the symmetric id to
  create unique identifiers for each time period. set to NULL (default)
  for cross-sectional data.

## Value

a character vector of symmetric identifiers with the same length as the
number of rows in dyad_data. each id is formatted as:

- **without time**: a length-prefixed key for the alphabetically sorted
  actor pair

- **with time**: the same key with the time value appended

## Details

the function ensures symmetry by alphabetically sorting actor names
before creating the identifier. this guarantees that:

- the dyad "usa-china" receives the same id as "china-usa"

- the dyad "brazil-argentina" receives the same id as "argentina-brazil"

- actor pairs are consistently ordered regardless of input order

when a time column is specified, it's appended to the symmetric id to
maintain unique identifiers across different time periods. this allows
for proper aggregation of longitudinal dyadic data while preserving
temporal variation.

## Note

all actor values are converted to character strings before creating ids
to ensure consistent sorting behavior across different data types.

ids are intended as opaque keys. do not parse them to recover actor
names; keep the original actor columns when those values are needed
downstream.

this function is primarily used internally by `aggregate_dyad` for
efficient symmetric aggregation, but can be used independently for
creating symmetric dyad identifiers.

## Author

shahryar minhas

## Examples

``` r
# create example dyadic data
trade_df <- data.frame(
    from = c("usa", "china", "russia", "usa", "brazil", "argentina"),
    to = c("china", "usa", "usa", "russia", "argentina", "brazil"),
    trade_value = c(100, 100, 50, 75, 30, 25),
    year = c(2020, 2020, 2021, 2021, 2021, 2021)
)

# generate symmetric ids without time
trade_df$symm_id <- gen_symm_id(trade_df, "from", "to")
print(trade_df[, c("from", "to", "symm_id")])
#>        from        to              symm_id
#> 1       usa     china        5:china|3:usa
#> 2     china       usa        5:china|3:usa
#> 3    russia       usa       6:russia|3:usa
#> 4       usa    russia       6:russia|3:usa
#> 5    brazil argentina 9:argentina|6:brazil
#> 6 argentina    brazil 9:argentina|6:brazil
# note: usa-china and china-usa both get "china_usa"

# generate symmetric ids with time
trade_df$symm_id_time <- gen_symm_id(trade_df, "from", "to", "year")
print(trade_df[, c("from", "to", "year", "symm_id_time")])
#>        from        to year                symm_id_time
#> 1       usa     china 2020        5:china|3:usa|4:2020
#> 2     china       usa 2020        5:china|3:usa|4:2020
#> 3    russia       usa 2021       6:russia|3:usa|4:2021
#> 4       usa    russia 2021       6:russia|3:usa|4:2021
#> 5    brazil argentina 2021 9:argentina|6:brazil|4:2021
#> 6 argentina    brazil 2021 9:argentina|6:brazil|4:2021
# note: usa-china in 2020 gets "china_usa_2020"

# use for aggregation of undirected relationships
trade_df$total_trade <- ave(
    trade_df$trade_value,
    trade_df$symm_id_time,
    FUN = sum
)
print(unique(trade_df[, c("symm_id_time", "total_trade")]))
#>                  symm_id_time total_trade
#> 1        5:china|3:usa|4:2020         200
#> 3       6:russia|3:usa|4:2021         125
#> 5 9:argentina|6:brazil|4:2021          55

# example with longitudinal data
library(netify)
data(icews)
icews_sample <- icews[1:100, ]

# create symmetric ids for conflict events
icews_sample$symm_dyad <- gen_symm_id(
    icews_sample,
    actor1 = "i",
    actor2 = "j",
    time = "year"
)

# check that symmetric pairs get same id
icews_sample[icews_sample$i == "united states" & icews_sample$j == "israel", "symm_dyad"]
#> character(0)
icews_sample[icews_sample$i == "israel" & icews_sample$j == "united states", "symm_dyad"]
#> character(0)
```
