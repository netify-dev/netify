# Create a netify array from longitudinal dyadic data

`get_adjacency_array` converts longitudinal dyadic data into a
three-dimensional netify array where the first two dimensions represent
actors and the third dimension represents time periods. This function
creates an array of class "netify" and should only be used when actor
composition remains constant across all time periods.

## Usage

``` r
get_adjacency_array(
  dyad_data,
  actor1 = NULL,
  actor2 = NULL,
  time = NULL,
  symmetric = TRUE,
  mode = "unipartite",
  weight = NULL,
  sum_dyads = FALSE,
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

A three-dimensional array of class "netify" (a netify array) with:

- **Dimensions**: `[n_actors × n_actors × n_time]` for unipartite
  networks or `[n_actors1 × n_actors2 × n_time]` for bipartite networks

- **Class**: "netify" - this is a full netify object compatible with all
  netify functions

- **Attributes**: Extensive metadata including network properties, actor
  information, and processing parameters

The returned object is a netify array that can be used with all netify
functions such as [`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`to_igraph()`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md),
etc.

## Details

**Note on usage:**

While this function is exported and available for direct use, the
primary and recommended way to create netify arrays from longitudinal
dyadic data is through the
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
function. The
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
function:

- Automatically determines whether to create an array or list structure

- Handles time-varying actor composition

- Provides more comprehensive data validation

- Offers a unified interface for all types of network data

Use `get_adjacency_array()` directly only when you specifically need
low-level control over array creation and are certain your actors remain
constant across time.

## Author

Cassy Dorff, Ha Eun Choi, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Create a netify array (longitudinal directed network)
# with material conflict as edge weights
icews_array <- get_adjacency_array(
    dyad_data = icews,
    actor1 = "i",
    actor2 = "j",
    time = "year",
    symmetric = FALSE,
    weight = "matlConf"
)

# Verify it's a netify object
class(icews_array) # "netify"
#> [1] "netify"

# Check dimensions
dim(icews_array) # [n_actors, n_actors, n_years]
#> [1] 152 152  13

# Access specific time period
icews_2010 <- icews_array[, , "2010"]
```
