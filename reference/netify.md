# Create network object from various data types

This function takes in various types of network data (dyadic datasets,
matrices, arrays, lists, igraph objects, or network objects) and outputs
a netify object.

## Usage

``` r
netify(
  input,
  actor1 = NULL,
  actor2 = NULL,
  time = NULL,
  symmetric = TRUE,
  mode = "unipartite",
  weight = NULL,
  sum_dyads = FALSE,
  actor_time_uniform = TRUE,
  actor_pds = NULL,
  diag_to_NA = TRUE,
  missing_to_zero = TRUE,
  output_format = NULL,
  nodal_vars = NULL,
  dyad_vars = NULL,
  dyad_vars_symmetric = rep(symmetric, length(dyad_vars)),
  input_type = c("auto", "dyad_df", "netify_obj"),
  nodelist = NULL,
  force_dense = FALSE,
  ...
)
```

## Arguments

- input:

  data object to netify. Can be:

  - A data.frame (or tibble/data.table) with dyadic data

  - A matrix representing an adjacency matrix

  - A 3D array representing longitudinal networks

  - A list of matrices representing longitudinal networks

  - An igraph object

  - A network object (from the network package)

  - A list of igraph or network objects

- actor1:

  character: name of the actor 1 variable in the data (required for
  data.frame inputs)

- actor2:

  character: name of the actor 2 variable in the data (required for
  data.frame inputs)

- time:

  character: name of the time variable in the data. Can contain numeric,
  Date, POSIXct/POSIXlt, or character values. Non-numeric types will be
  converted to numeric indices while preserving original labels. If no
  time is provided then a cross-sectional network will be created.

- symmetric:

  logical: whether ties are symmetric, default is TRUE. For matrix,
  array, list, igraph, or network inputs this default is ignored unless
  explicitly set by the caller; symmetry is instead detected from the
  input itself. The default applies for data.frame inputs.

- mode:

  character: whether the network is unipartite or bipartite, default is
  unipartite. As with `symmetric`, this default applies to data.frame
  inputs and is auto-detected for matrix / array / list / igraph /
  network inputs unless explicitly set.

- weight:

  character: name of the weighted edge variable in the data, default is
  NULL

- sum_dyads:

  logical: whether to sum up the `weight` value when there exists
  repeating dyads

- actor_time_uniform:

  logical: whether to assume actors are the same across the full time
  series observed in the data TRUE means that actors are the same across
  the full time series observed in the data and the outputted netify
  object will be in an array format. FALSE means that actors come in and
  out of the observed data and their "existence" should be determined by
  the data, meaning that their first year of existence will be
  determined by the time point of their first event and their last year
  of existence by the time point of their last event. Outputted netify
  object will be in a list format.

- actor_pds:

  a data.frame indicating start and end time point for every actor, this
  can be created using `get_actor_time_info`, unless provided this will
  estimated for the user based on their choice of `actor_time_uniform`

- diag_to_NA:

  logical: whether diagonals should be set to NA, default is TRUE. For
  matrix / array / list inputs, the default is ignored unless explicitly
  set: instead, `diag_to_NA` is auto-detected by inspecting whether the
  diagonal of the supplied matrix is already `NA`. Pass an explicit
  value to override the auto-detection.

- missing_to_zero:

  logical: whether missing values should be set to zero, default is
  TRUE. As with `diag_to_NA`, this is auto-detected for matrix / array /
  list inputs based on whether the supplied data already contains
  off-diagonal `NA`s.

- output_format:

  character: "cross_sec", "longit_array", or "longit_list". If not
  specified and time is NULL then output_format will be "cross_sec" and
  if time is specified then output_format will default to "longit_list".
  Only applies to data.frame inputs.

- nodal_vars:

  character vector: names of the nodal variables in the input that
  should be added as attributes to the netify object (for data.frame
  inputs)

- dyad_vars:

  character vector: names of the dyadic variables in the input that
  should be added as attributes to the netify object (for data.frame
  inputs)

- dyad_vars_symmetric:

  logical vector: whether ties are symmetric, default is to use the same
  choice as the symmetric argument

- input_type:

  character: force specific input type interpretation. Options are
  "auto" (default), "dyad_df", or "netify_obj". Use "dyad_df" to force
  data.frame interpretation or "netify_obj" to force matrix/array/
  igraph/network interpretation.

- nodelist:

  character vector: optional list of all actors (nodes) that should be
  included in the network. This ensures isolates (nodes with no edges)
  are properly represented. Particularly useful when working with
  edgelists that only contain active dyads.

- force_dense:

  logical: when a
  [`Matrix::sparseMatrix`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  input would densify to a large allocation (N \> 5000 and density \<
  1%), `netify()` aborts with a guidance message. Set
  `force_dense = TRUE` to override the guard and proceed with
  densification.

- ...:

  additional arguments passed to `to_netify` when processing network
  objects

## Value

a netify object

## See also

[`netify_workflows`](https://netify-dev.github.io/netify/reference/netify_workflows.md)
for an overview of the Create / Explore / Advance pipeline and how
`netify()` fits into it;
[`add_node_vars`](https://netify-dev.github.io/netify/reference/add_node_vars.md),
[`add_dyad_vars`](https://netify-dev.github.io/netify/reference/add_dyad_vars.md)
for attaching attributes after construction; and
[`classroom_edges`](https://netify-dev.github.io/netify/reference/classroom_edges.md)
/
[`classroom_nodes`](https://netify-dev.github.io/netify/reference/classroom_nodes.md)
for a small worked example.

## Author

Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas

## Examples

``` r

# load example directed event data from ICEWS
# this data comes in the form of a dyadic
# dataframe where all dyad pairs are listed
data(icews)

# From a data.frame: generate a longitudional, directed and weighted network
# where the weights are matlConf
icews_matlConf <- netify(
    input = icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "matlConf"
)

# From a matrix
adj_matrix <- matrix(rbinom(100, 1, 0.3), 10, 10)
net_from_matrix <- netify(adj_matrix)

# From an igraph object
if (FALSE) { # \dontrun{
library(igraph)
g <- sample_gnp(10, 0.3)
net_from_igraph <- netify(g)
} # }
```
