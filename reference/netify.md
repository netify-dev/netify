# Create network object from various data types

this function takes in various types of network data (dyadic datasets,
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

  data object to netify. can be:

  - a data.frame (or tibble/data.table) with dyadic data

  - a matrix representing an adjacency matrix

  - a 3d array representing longitudinal networks

  - a list of matrices representing longitudinal networks

  - an igraph object

  - a network object (from the network package)

  - a list of igraph or network objects

- actor1:

  character: name of the actor 1 variable in the data (required for
  data.frame inputs)

- actor2:

  character: name of the actor 2 variable in the data (required for
  data.frame inputs)

- time:

  character: name of the time variable in the data. can contain numeric,
  date, posixct/posixlt, or character values. non-numeric types will be
  converted to numeric indices while preserving original labels. if no
  time is provided then a cross-sectional network will be created.

- symmetric:

  logical: whether ties are symmetric, default is TRUE. for matrix,
  array, list, igraph, or network inputs this default is ignored unless
  explicitly set by the caller; symmetry is instead detected from the
  input itself. the default applies for data.frame inputs.

- mode:

  character: whether the network is unipartite or bipartite, default is
  unipartite. as with `symmetric`, this default applies to data.frame
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
  of existence by the time point of their last event. outputted netify
  object will be in a list format.

- actor_pds:

  a data.frame indicating start and end time point for every actor, this
  can be created using `get_actor_time_info`, unless provided this will
  estimated for the user based on their choice of `actor_time_uniform`

- diag_to_NA:

  logical: whether diagonals should be set to na, default is TRUE. for
  matrix / array / list inputs, the default is ignored unless explicitly
  set: instead, `diag_to_NA` is auto-detected by inspecting whether the
  diagonal of the supplied matrix is already `na`. pass an explicit
  value to override the auto-detection.

- missing_to_zero:

  logical: whether missing values should be set to zero, default is
  TRUE. as with `diag_to_NA`, this is auto-detected for matrix / array /
  list inputs based on whether the supplied data already contains
  off-diagonal `na`s.

- output_format:

  character: "cross_sec", "longit_array", or "longit_list". if not
  specified and time is NULL then output_format will be "cross_sec" and
  if time is specified then output_format will default to "longit_list".
  only applies to data.frame inputs.

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

  character: force specific input type interpretation. options are
  "auto" (default), "dyad_df", or "netify_obj". use "dyad_df" to force
  data.frame interpretation or "netify_obj" to force matrix/array/
  igraph/network interpretation.

- nodelist:

  optional list of all actors (nodes) that should be included in the
  network. for unipartite networks, pass a character vector. for
  bipartite networks, pass partition-aware input such as
  `list(row = row_actors, col = col_actors)` or a data frame with actor
  and mode columns. a flat vector can include only actors already
  observed in a bipartite row or column mode because new isolates cannot
  be assigned to a partition from their names alone.

- force_dense:

  logical: when a `matrix::sparsematrix` input would densify to a large
  allocation (n \> 5000 and density \< 1%), `netify()` aborts with a
  guidance message. set `force_dense = TRUE` to override the guard and
  proceed with densification.

- ...:

  additional arguments passed to `to_netify` when processing network
  objects

## Value

a netify object

## See also

[`netify_workflows`](https://netify-dev.github.io/netify/reference/netify_workflows.md)
for an overview of the create / explore / model workflow and how
`netify()` fits into it;
[`add_node_vars`](https://netify-dev.github.io/netify/reference/add_node_vars.md),
[`add_dyad_vars`](https://netify-dev.github.io/netify/reference/add_dyad_vars.md)
for attaching attributes after construction; and
[`classroom_edges`](https://netify-dev.github.io/netify/reference/classroom_edges.md)
/
[`classroom_nodes`](https://netify-dev.github.io/netify/reference/classroom_nodes.md)
for a small worked example.

## Author

ha eun choi, cassy dorff, colin henry, shahryar minhas

## Examples

``` r

# load example directed event data from icews
# this data comes in the form of a dyadic
# dataframe where all dyad pairs are listed
data(icews)

# from a data.frame: generate a longitudinal, directed and weighted network
# where the weights are matlConf
icews_matlConf <- netify(
    input = icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "matlConf"
)

# from a matrix
adj_matrix <- matrix(rbinom(100, 1, 0.3), 10, 10)
net_from_matrix <- netify(adj_matrix)

# from an igraph object
if (FALSE) { # \dontrun{
library(igraph)
g <- sample_gnp(10, 0.3)
net_from_igraph <- netify(g)
} # }
```
