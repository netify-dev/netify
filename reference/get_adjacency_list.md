# Create a netify list from longitudinal dyadic data

`get_adjacency_list` converts longitudinal dyadic data into a list of
adjacency matrices of class "netify". this function creates a list
structure where each element is a network matrix for a specific time
period, allowing for time-varying actor composition.

## Usage

``` r
get_adjacency_list(
  dyad_data,
  actor1 = NULL,
  actor2 = NULL,
  time = NULL,
  symmetric = TRUE,
  mode = "unipartite",
  weight = NULL,
  sum_dyads = FALSE,
  actor_time_uniform = FALSE,
  actor_pds = NULL,
  diag_to_NA = TRUE,
  missing_to_zero = TRUE,
  nodelist = NULL
)
```

## Arguments

- dyad_data:

  a data.frame containing longitudinal dyadic observations. will be
  coerced to data.frame if a tibble or data.table is provided.

- actor1:

  character string specifying the column name for the first actor in
  each dyad.

- actor2:

  character string specifying the column name for the second actor in
  each dyad.

- time:

  character string specifying the column name for time periods.

- symmetric:

  logical. if TRUE (default), treats the network as undirected (i.e.,
  edges have no direction). if FALSE, treats the network as directed.

- mode:

  character string specifying network structure. options are:

  - `"unipartite"`: one set of actors (default)

  - `"bipartite"`: two distinct sets of actors

- weight:

  character string specifying the column name containing edge weights.
  if NULL (default), edges are treated as unweighted (binary).

- sum_dyads:

  logical. if TRUE, sums weight values when multiple edges exist between
  the same actor pair in the same time period. if FALSE (default), uses
  the last observed value.

- actor_time_uniform:

  logical indicating how to handle actor composition:

  - `TRUE`: assumes all actors exist across the entire time range

  - `FALSE`: determines actor existence from the data - actors exist
    from their first observed interaction to their last

- actor_pds:

  optional data.frame specifying when actors enter and exit the network.
  must contain columns 'actor', 'min_time', and 'max_time'. can be
  created using
  [`get_actor_time_info()`](https://netify-dev.github.io/netify/reference/get_actor_time_info.md).
  if provided, overrides actor_time_uniform.

- diag_to_NA:

  logical. if TRUE (default), sets diagonal values (self-loops) to na.
  automatically set to FALSE for bipartite networks.

- missing_to_zero:

  logical. if TRUE (default), treats missing edges as zeros. if FALSE,
  missing edges remain as na.

- nodelist:

  character vector of actor names to include in the network. if
  provided, ensures all listed actors appear in the network even if they
  have no edges (isolates). useful when working with edgelists that only
  contain active dyads.

## Value

a list of class "netify" (a netify list) with:

- **elements**: named list where each element is a netify matrix for one
  time period

- **names**: character representation of time periods

- **class**: "netify" - this is a full netify object compatible with all
  netify functions

- **attributes**: extensive metadata including network properties, actor
  composition information, and processing parameters

each matrix in the list may have different dimensions if actor
composition varies over time. the returned object can be used with all
netify functions such as
[`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`to_igraph()`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md),
etc.

## Details

**note on usage:**

while this function is exported and available for direct use, the
primary and recommended way to create netify objects from longitudinal
dyadic data is through the
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
function. the
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
function:

- automatically chooses between array and list representations based on
  your data

- validates inputs before constructing matrices

- can incorporate nodal and dyadic attributes during creation

- offers a unified interface for all types of network data

use `get_adjacency_list()` directly only when you specifically need a
list structure or require low-level control over the creation process.

**actor composition handling:**

this function is particularly useful when actors enter and exit the
network over time. unlike
[`get_adjacency_array()`](https://netify-dev.github.io/netify/reference/get_adjacency_array.md),
which requires constant actor composition, this function can handle:

- new actors appearing in later time periods

- actors exiting and no longer appearing in the data

- different sets of actors active in each time period

## Author

cassy dorff, ha eun choi, shahryar minhas

## Examples

``` r
# load example data
data(icews)

# create a netify list with constant actor composition
icews_list <- get_adjacency_list(
    dyad_data = icews,
    actor1 = "i",
    actor2 = "j",
    time = "year",
    actor_time_uniform = TRUE,
    symmetric = FALSE,
    weight = "verbConf"
)

# verify it's a netify object
class(icews_list) # "netify"
#> [1] "netify"

# check structure
length(icews_list) # number of time periods
#> [1] 13
names(icews_list) # time period labels
#>  [1] "2002" "2003" "2004" "2005" "2006" "2007" "2008" "2009" "2010" "2011"
#> [11] "2012" "2013" "2014"

# access specific time period
icews_2010 <- icews_list[["2010"]]
dim(icews_2010)
#> [1] 152 152
```
