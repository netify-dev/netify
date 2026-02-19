# Subset netify objects

Extracts a subset of a netify object based on specified actors, time
periods, and/or layers while preserving all netify attributes and
structure.

## Usage

``` r
# S3 method for class 'netify'
subset(
  x,
  actors = NULL,
  time = NULL,
  layers = NULL,
  from = NULL,
  to = NULL,
  ...
)
```

## Arguments

- x:

  A netify object to subset

- actors:

  Character vector of actor names or numeric indices to subset. Extracts
  the subgraph among these actors (includes ties both from and to these
  actors). Default is NULL, which includes all actors.

- time:

  Time periods to subset. Can be:

  - Numeric vector: used as indices to the time dimension

  - Character vector: matched against time dimension labels

  - NULL: includes all time periods (default)

- layers:

  Character vector of layer names to subset from multilayer networks.
  For single-layer networks, this is ignored. For multilayer networks,
  at least one layer must be specified.

- from:

  Character vector of actor names or numeric indices for actors sending
  ties (row actors). Overrides `actors`. Set to NULL to include all
  sending actors. In bipartite networks, this refers to actors in the
  first mode.

- to:

  Character vector of actor names or numeric indices for actors
  receiving ties (column actors). Overrides `actors`. Set to NULL to
  include all receiving actors. In bipartite networks, this refers to
  actors in the second mode.

- ...:

  Additional arguments (currently unused)

## Value

A netify object containing the requested subset with:

- Subsetted adjacency matrix/matrices

- Corresponding nodal attributes (filtered to included actors/times)

- Corresponding dyadic attributes (filtered to included actor
  pairs/times)

- Updated netify attributes reflecting the new dimensions

The returned object's structure depends on the subset:

- If one time period is selected from longitudinal data, returns
  cross-sectional

- If one layer is selected from multilayer data, returns single-layer

- Otherwise, maintains the original structure type

## Details

This function is a netify-aware wrapper around the
[`peek`](https://netify-dev.github.io/netify/reference/peek.md)
function, which handles the raw data extraction. While `peek` returns
raw matrices/arrays, `subset` additionally:

- Preserves and updates all netify attributes

- Filters nodal and dyadic attribute data to match the subset

- Adjusts the netify type when dimensions change (e.g., longitudinal to
  cross-sectional)

- Maintains consistency between network data and attributes

The `from` and `to` parameters allow precise control over which ties to
include:

- Use `actors` to get all ties among a set of actors (subgraph
  extraction)

- Use `from` to get all ties sent by specific actors

- Use `to` to get all ties received by specific actors

- Use both `from` and `to` to get ties between specific sets of actors

For bipartite networks, `from` refers to actors in the first mode (e.g.,
people) and `to` refers to actors in the second mode (e.g.,
organizations).

## Note

When subsetting longitudinal data to a single time period, the function
automatically converts the result to a cross-sectional netify object.
Similarly, subsetting multilayer data to a single layer produces a
single-layer object.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
# load example directed event data from ICEWS
data(icews)

# generate a longitudional netify object
# with both dyadic and nodal attributes
icews_matlConf <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "matlConf",
    nodal_vars = c("i_polity2", "i_log_gdp", "i_log_pop"),
    dyad_vars = c("matlCoop", "verbCoop", "verbConf"),
    dyad_vars_symmetric = c(FALSE, FALSE, FALSE)
)

# subset to a few countries using S3 method
icews_subset <- subset(
    icews_matlConf,
    actors = c(
        "United States", "United Kingdom",
        "Russian Federation", "China"
    )
)

# subset to a few countries and a few years
icews_subset_2 <- subset(
    icews_matlConf,
    actors = c(
        "United States", "United Kingdom",
        "Russian Federation", "China"
    ),
    time = c("2010", "2011")
)

# can also use subset_netify directly
icews_subset_3 <- subset_netify(
    netlet = icews_matlConf,
    actors = c(
        "United States", "United Kingdom",
        "Russian Federation", "China"
    ),
    time = c("2010", "2011")
)
```
