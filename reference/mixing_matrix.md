# Create attribute mixing matrices for network data

creates cross-tabulation matrices showing how connections are
distributed across different attribute values. this reveals mixing
patterns and assortativity in networks by examining the frequency of
ties between actors with different attribute combinations.

## Usage

``` r
mixing_matrix(
  netlet,
  attribute,
  row_attribute = NULL,
  normalized = TRUE,
  by_row = FALSE,
  include_weights = FALSE,
  other_stats = NULL,
  ...
)
```

## Arguments

- netlet:

  a netify object containing network data.

- attribute:

  character string specifying the nodal attribute to analyze.

- row_attribute:

  optional different attribute for matrix rows. if NULL, uses the same
  attribute for both dimensions.

- normalized:

  logical. whether to return proportions instead of raw counts. default
  TRUE.

- by_row:

  logical. if TRUE and normalized=TRUE, normalizes by row. default
  FALSE.

- include_weights:

  logical. whether to use edge weights. default FALSE.

- other_stats:

  named list of custom functions for additional statistics.

- ...:

  additional arguments passed to custom functions.

## Value

list containing:

- `mixing_matrices`:

  named list of mixing matrices per time/layer

- `summary_stats`:

  data frame with mixing statistics:

## Details

mixing matrix elements represent ties between actors with attribute
values i and j. for undirected networks, matrices are symmetrized.
assortativity ranges from -1 (disassortative) to 1 (assortative).

## Author

cassy dorff, shahryar minhas

## Examples

``` r
# who tends to befriend whom, by gender, in the bundled classroom data
data(classroom_edges)
data(classroom_nodes)
net <- netify(
    classroom_edges,
    actor1 = "from", actor2 = "to",
    symmetric = TRUE,
    nodal_data = classroom_nodes
)
mm <- mixing_matrix(net, attribute = "gender")
round(mm$mixing_matrices[[1]], 3)
#>       F     M
#> F 0.078 0.167
#> M 0.167 0.588
mm$summary_stats
#>   net   layer attribute assortativity diagonal_proportion  entropy modularity
#> 1   1 weight1    gender    0.09922078           0.6666667 1.109037 0.03671665
#>   n_groups total_ties
#> 1        2        102
```
