# Create a netify array from longitudinal dyadic data

`get_adjacency_array` converts longitudinal dyadic data into a
three-dimensional netify array where the first two dimensions represent
actors and the third dimension represents time periods. this function
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

a three-dimensional array of class "netify" (a netify array) with:

- **dimensions**: `[n_actors x n_actors x n_time]` for unipartite
  networks or `[n_actors1 x n_actors2 x n_time]` for bipartite networks

- **class**: "netify" - this is a full netify object compatible with all
  netify functions

- **attributes**: extensive metadata including network properties, actor
  information, and processing parameters

the returned object is a netify array that can be used with all netify
functions such as [`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`to_igraph()`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md),
etc.

## Details

**note on usage:**

while this function is exported and available for direct use, the
primary and recommended way to create netify arrays from longitudinal
dyadic data is through the
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
function. the
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
function:

- automatically determines whether to create an array or list structure

- handles time-varying actor composition

- validates inputs before constructing arrays

- offers a unified interface for all types of network data

use `get_adjacency_array()` directly only when you specifically need
low-level control over array creation and are certain your actors remain
constant across time.

## Author

cassy dorff, ha eun choi, shahryar minhas

## Examples

``` r
# load example data
data(icews)

# create a netify array (longitudinal directed network)
# with material conflict as edge weights
icews_array <- get_adjacency_array(
    dyad_data = icews,
    actor1 = "i",
    actor2 = "j",
    time = "year",
    symmetric = FALSE,
    weight = "matlConf"
)

# verify it's a netify object
class(icews_array) # "netify"
#> [1] "netify"

# check dimensions
dim(icews_array) # [n_actors, n_actors, n_years]
#> [1] 152 152  13

# access specific time period
icews_2010 <- icews_array[, , "2010"]
```
