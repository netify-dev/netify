# Subset netify objects

extracts a subset of a netify object based on specified actors, time
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

  a netify object to subset

- actors:

  character vector of actor names or numeric indices to subset. extracts
  the subgraph among these actors (includes ties both from and to these
  actors). default is NULL, which includes all actors.

- time:

  time periods to subset. can be:

  - numeric vector: used as indices to the time dimension

  - character vector: matched against time dimension labels

  - NULL: includes all time periods (default)

- layers:

  character vector of layer names to subset from multilayer networks.
  for single-layer networks, this is ignored. for multilayer networks,
  at least one layer must be specified.

- from:

  character vector of actor names or numeric indices for actors sending
  ties (row actors). overrides `actors`. set to NULL to include all
  sending actors. in bipartite networks, this refers to actors in the
  first mode.

- to:

  character vector of actor names or numeric indices for actors
  receiving ties (column actors). overrides `actors`. set to NULL to
  include all receiving actors. in bipartite networks, this refers to
  actors in the second mode.

- ...:

  additional arguments (currently unused)

## Value

a netify object containing the requested subset with:

- subsetted adjacency matrix/matrices

- corresponding nodal attributes (filtered to included actors/times)

- corresponding dyadic attributes (filtered to included actor
  pairs/times)

- updated netify attributes reflecting the new dimensions

the returned object's structure depends on the subset:

- if one time period is selected from longitudinal data, returns
  cross-sectional

- if one layer is selected from multilayer data, returns single-layer

- otherwise, maintains the original structure type

## Details

this function is a netify-aware wrapper around the
[`peek`](https://netify-dev.github.io/netify/reference/peek.md)
function, which handles the raw data extraction. while `peek` returns
raw matrices/arrays, `subset` additionally:

- preserves and updates all netify attributes

- filters nodal and dyadic attribute data to match the subset

- adjusts the netify type when dimensions change (e.g., longitudinal to
  cross-sectional)

- maintains consistency between network data and attributes

the `from` and `to` parameters allow precise control over which ties to
include:

- use `actors` to get all ties among a set of actors (subgraph
  extraction)

- use `from` to get all ties sent by specific actors

- use `to` to get all ties received by specific actors

- use both `from` and `to` to get ties between specific sets of actors

for bipartite networks, `from` refers to actors in the first mode (e.g.,
people) and `to` refers to actors in the second mode (e.g.,
organizations).

## Note

when subsetting longitudinal data to a single time period, the function
automatically converts the result to a cross-sectional netify object.
similarly, subsetting multilayer data to a single layer produces a
single-layer object.

## Author

cassy dorff, shahryar minhas

## Examples

``` r

# \donttest{
# load example directed event data from icews
data(icews)

# generate a longitudinal netify object
# with both dyadic and nodal attributes
icews_matlConf <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "matlConf",
    nodal_vars = c("i_polity2", "i_log_gdp", "i_log_pop"),
    dyad_vars = c("matlCoop", "verbCoop", "verbConf"),
    dyad_vars_symmetric = c(FALSE, FALSE, FALSE)
)

# subset to a few countries using s3 method
icews_subset <- subset(
    icews_matlConf,
    actors = c(
        "united states", "united kingdom",
        "russian federation", "china"
    )
)

# subset to a few countries and a few years
icews_subset_2 <- subset(
    icews_matlConf,
    actors = c(
        "united states", "united kingdom",
        "russian federation", "china"
    ),
    time = c("2010", "2011")
)

# can also use subset_netify directly
icews_subset_3 <- subset_netify(
    netlet = icews_matlConf,
    actors = c(
        "united states", "united kingdom",
        "russian federation", "china"
    ),
    time = c("2010", "2011")
)
# }
```
