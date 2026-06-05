# Aggregate dyadic event data by actor pairs

`aggregate_dyad` is designed for use with dyadic event datasets such as
those from acled or icews. these datasets often contain multiple
interactions between the same pair of actors–e.g., protest events,
material cooperation, or verbal conflict–recorded at high frequency.
this function aggregates such repeated observations into a single
summary value per dyad, optionally within specified time periods. it is
particularly useful for preparing network inputs by collapsing daily or
monthly event-level data into actor-to-actor matrices.

## Usage

``` r
aggregate_dyad(
  dyad_data,
  actor1,
  actor2,
  time = NULL,
  weight,
  symmetric,
  ignore_missing = TRUE
)
```

## Arguments

- dyad_data:

  a data.frame containing dyadic observations. must include columns for
  two actors and a weight variable. will be coerced to data.frame if a
  tibble or data.table is provided.

- actor1:

  character string specifying the column name for the first actor in
  each dyad.

- actor2:

  character string specifying the column name for the second actor in
  each dyad.

- time:

  character string specifying the column name for time periods. if NULL
  (default), aggregation is performed across all time periods.

- weight:

  character string specifying the column name containing values to be
  aggregated (summed) for each unique actor pair.

- symmetric:

  logical. if TRUE, treats dyads as undirected (i.e., the dyad a-b is
  treated as identical to b-a). if FALSE, treats dyads as directed
  (i.e., a-b is distinct from b-a).

- ignore_missing:

  logical. if TRUE (default), missing values in the weight variable are
  ignored during aggregation. if FALSE, any dyad containing a missing
  value will result in na for that aggregated dyad.

## Value

a data.frame with unique actor pairs (and time periods if specified) and
their aggregated weight values. the output contains columns:

- **actor1**: first actor in each dyad (using original column name)

- **actor2**: second actor in each dyad (using original column name)

- **time**: time period if time parameter was specified (using original
  column name)

- **weight**: aggregated (summed) values for each unique dyad (using
  original column name)

## Details

the function handles both directed and undirected dyadic aggregation:

**for symmetric (undirected) networks:**

the function uses an efficient aggregation method that:

1.  creates symmetric identifiers for each dyad using `gen_symm_id`
    (where a-b = b-a)

2.  aggregates values by these symmetric identifiers

3.  expands the results back to directed format for consistency with
    other netify functions

this ensures that interactions between actors a and b are combined
regardless of direction, useful for undirected relationships like
friendships or alliances.

**for asymmetric (directed) networks:**

standard aggregation is performed treating each directed dyad
separately. this maintains the distinction between a-\>b and b-\>a
relationships, which is important for directed interactions like
exports/imports or sender/receiver communications.

**missing value handling:**

the `ignore_missing` parameter controls how na values are treated:

- when TRUE: missing values are excluded from the sum (e.g., sum(10,
  na, 20) = 30)

- when FALSE: any missing value results in na for that dyad (e.g.,
  sum(10, na, 20) = na)

## Note

the function preserves the original column names from the input
data.frame in the output, making it easy to chain operations or merge
results.

when symmetric = TRUE, the output still maintains a directed format
(separate rows for a-b and b-a) with identical values for both
directions. this ensures compatibility with other netify functions that
expect directed dyadic data.

## Author

shahryar minhas

## Examples

``` r
# load example data
data(icews)

# example 1: aggregate multiple events between countries
# the icews data contains multiple events per country pair
icews_2010 <- icews[icews$year == 2010, ]

# aggregate directed cooperation events
agg_coop <- aggregate_dyad(
    dyad_data = icews_2010,
    actor1 = "i",
    actor2 = "j",
    weight = "verbCoop",
    symmetric = FALSE
)

# check reduction in observations
nrow(icews_2010) # original observations
#> [1] 22952
nrow(agg_coop) # unique directed dyads
#> [1] 22952

# example 2: create symmetric trade volumes
trade_data <- data.frame(
    exporter = c("usa", "usa", "china", "china", "usa", "china"),
    importer = c("china", "china", "usa", "usa", "uk", "uk"),
    year = c(2020, 2020, 2020, 2021, 2021, 2021),
    trade_value = c(100, 50, 75, 80, 120, 90)
)

# aggregate as total trade between countries (undirected)
total_trade <- aggregate_dyad(
    dyad_data = trade_data,
    actor1 = "exporter",
    actor2 = "importer",
    time = "year",
    weight = "trade_value",
    symmetric = TRUE
)

# usa-china trade in 2020: 100+50+75 = 225 (appears in both directions)
total_trade[total_trade$year == 2020, ]
#>   exporter importer year trade_value
#> 1    china      usa 2020         225
#> 5      usa    china 2020         225

# example 3: aggregate across all time periods
all_time_trade <- aggregate_dyad(
    dyad_data = trade_data,
    actor1 = "exporter",
    actor2 = "importer",
    time = NULL, # aggregate across all years
    weight = "trade_value",
    symmetric = FALSE
)

# usa total exports to china: 100+50 = 150
all_time_trade
#>   exporter importer trade_value
#> 1      usa    china         150
#> 2    china       uk          90
#> 3      usa       uk         120
#> 4    china      usa         155

# example 4: handle missing values
trade_data_na <- trade_data
trade_data_na$trade_value[2] <- NA

# ignore missing values (default)
agg_ignore_na <- aggregate_dyad(
    dyad_data = trade_data_na,
    actor1 = "exporter",
    actor2 = "importer",
    time = "year",
    weight = "trade_value",
    symmetric = FALSE,
    ignore_missing = TRUE
)

# include missing values
agg_with_na <- aggregate_dyad(
    dyad_data = trade_data_na,
    actor1 = "exporter",
    actor2 = "importer",
    time = "year",
    weight = "trade_value",
    symmetric = FALSE,
    ignore_missing = FALSE
)

# compare results for usa->china in 2020
agg_ignore_na[agg_ignore_na$exporter == "usa" &
    agg_ignore_na$importer == "china" &
    agg_ignore_na$year == 2020, ] # 100 (ignored NA)
#>   exporter importer year trade_value
#> 1      usa    china 2020         100

agg_with_na[agg_with_na$exporter == "usa" &
    agg_with_na$importer == "china" &
    agg_with_na$year == 2020, ] # NA
#>   exporter importer year trade_value
#> 1      usa    china 2020          NA
```
