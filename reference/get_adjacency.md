# Create a netify matrix from cross-sectional dyadic data

`get_adjacency` converts cross-sectional dyadic data into an adjacency
matrix of class "netify". this function creates a single network matrix
representing relationships at one point in time.

## Usage

``` r
get_adjacency(
  dyad_data,
  actor1 = NULL,
  actor2 = NULL,
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

  a data.frame containing dyadic observations. will be coerced to
  data.frame if a tibble or data.table is provided. as a convenience, an
  existing netify object is also accepted: in that case
  `get_adjacency()` returns the underlying adjacency as a plain matrix
  (no class / attributes), and for longitudinal inputs returns the first
  time slice with a cli hint pointing to `as.matrix(net, time = ...)`
  for selecting a specific slice.

- actor1:

  character string specifying the column name for the first actor in
  each dyad.

- actor2:

  character string specifying the column name for the second actor in
  each dyad.

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
  the same actor pair. if FALSE (default), uses the last observed value.

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

a matrix of class "netify" (a netify matrix) with:

- **dimensions**: `[n_actors x n_actors]` for unipartite networks or
  `[n_actors1 x n_actors2]` for bipartite networks

- **class**: "netify" - this is a full netify object compatible with all
  netify functions

- **attributes**: metadata including network properties and processing
  parameters

the returned object is a netify matrix that can be used with all netify
functions such as [`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`to_igraph()`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md),
etc.

## Details

**note on usage:**

while this function is exported and available for direct use, the
primary and recommended way to create netify objects from dyadic data is
through the
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
function. the
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
function:

- provides a consistent interface for both cross-sectional and
  longitudinal data

- includes additional data validation and preprocessing options

- can incorporate nodal and dyadic attributes during creation

- checks parameters before constructing the matrix

use `get_adjacency()` directly only when you need a simple adjacency
matrix creation without additional features.

## Author

ha eun choi, cassy dorff, colin henry, shahryar minhas

## Examples

``` r
# load example data
data(icews)

# subset to one year for cross-sectional analysis
icews_2010 <- icews[icews$year == 2010, ]

# create a directed network with verbal cooperation weights
verbCoop_net <- get_adjacency(
    dyad_data = icews_2010,
    actor1 = "i",
    actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# create a directed network with material conflict weights
matlConf_net <- get_adjacency(
    dyad_data = icews_2010,
    actor1 = "i",
    actor2 = "j",
    symmetric = FALSE,
    weight = "matlConf"
)

# verify class
class(verbCoop_net) # "netify"
#> [1] "netify"

# check dimensions
dim(verbCoop_net)
#> [1] 152 152
```
