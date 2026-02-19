# Create a netify list from longitudinal dyadic data

`get_adjacency_list` converts longitudinal dyadic data into a list of
adjacency matrices of class "netify". This function creates a list
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

  A data.frame containing longitudinal dyadic observations. Will be
  coerced to data.frame if a tibble or data.table is provided.

- actor1:

  Character string specifying the column name for the first actor in
  each dyad.

- actor2:

  Character string specifying the column name for the second actor in
  each dyad.

- time:

  Character string specifying the column name for time periods.

- symmetric:

  Logical. If TRUE (default), treats the network as undirected (i.e.,
  edges have no direction). If FALSE, treats the network as directed.

- mode:

  Character string specifying network structure. Options are:

  - `"unipartite"`: One set of actors (default)

  - `"bipartite"`: Two distinct sets of actors

- weight:

  Character string specifying the column name containing edge weights.
  If NULL (default), edges are treated as unweighted (binary).

- sum_dyads:

  Logical. If TRUE, sums weight values when multiple edges exist between
  the same actor pair in the same time period. If FALSE (default), uses
  the last observed value.

- actor_time_uniform:

  Logical indicating how to handle actor composition:

  - `TRUE`: Assumes all actors exist across the entire time range

  - `FALSE`: Determines actor existence from the data - actors exist
    from their first observed interaction to their last

- actor_pds:

  Optional data.frame specifying when actors enter and exit the network.
  Must contain columns 'actor', 'min_time', and 'max_time'. Can be
  created using
  [`get_actor_time_info()`](https://netify-dev.github.io/netify/reference/get_actor_time_info.md).
  If provided, overrides actor_time_uniform.

- diag_to_NA:

  Logical. If TRUE (default), sets diagonal values (self-loops) to NA.
  Automatically set to FALSE for bipartite networks.

- missing_to_zero:

  Logical. If TRUE (default), treats missing edges as zeros. If FALSE,
  missing edges remain as NA.

- nodelist:

  Character vector of actor names to include in the network. If
  provided, ensures all listed actors appear in the network even if they
  have no edges (isolates). Useful when working with edgelists that only
  contain active dyads.

## Value

A list of class "netify" (a netify list) with:

- **Elements**: Named list where each element is a netify matrix for one
  time period

- **Names**: Character representation of time periods

- **Class**: "netify" - this is a full netify object compatible with all
  netify functions

- **Attributes**: Extensive metadata including network properties, actor
  composition information, and processing parameters

Each matrix in the list may have different dimensions if actor
composition varies over time. The returned object can be used with all
netify functions such as
[`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`to_igraph()`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md),
etc.

## Details

**Note on usage:**

While this function is exported and available for direct use, the
primary and recommended way to create netify objects from longitudinal
dyadic data is through the
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
function. The
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
function:

- Automatically chooses between array and list representations based on
  your data

- Provides more comprehensive data validation

- Can incorporate nodal and dyadic attributes during creation

- Offers a unified interface for all types of network data

Use `get_adjacency_list()` directly only when you specifically need a
list structure or require low-level control over the creation process.

**Actor composition handling:**

This function is particularly useful when actors enter and exit the
network over time. Unlike
[`get_adjacency_array()`](https://netify-dev.github.io/netify/reference/get_adjacency_array.md),
which requires constant actor composition, this function can handle:

- New actors appearing in later time periods

- Actors exiting and no longer appearing in the data

- Different sets of actors active in each time period

## Author

Cassy Dorff, Ha Eun Choi, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Create a netify list with constant actor composition
icews_list <- get_adjacency_list(
    dyad_data = icews,
    actor1 = "i",
    actor2 = "j",
    time = "year",
    actor_time_uniform = TRUE,
    symmetric = FALSE,
    weight = "verbConf"
)

# Verify it's a netify object
class(icews_list) # "netify"
#> [1] "netify"

# Check structure
length(icews_list) # Number of time periods
#> [1] 13
names(icews_list) # Time period labels
#>  [1] "2002" "2003" "2004" "2005" "2006" "2007" "2008" "2009" "2010" "2011"
#> [11] "2012" "2013" "2014"

# Access specific time period
icews_2010 <- icews_list[["2010"]]
dim(icews_2010)
#> [1] 152 152
```
