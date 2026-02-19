# Create attribute mixing matrices for network data

Creates cross-tabulation matrices showing how connections are
distributed across different attribute values. This reveals mixing
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

  A netify object containing network data.

- attribute:

  Character string specifying the nodal attribute to analyze.

- row_attribute:

  Optional different attribute for matrix rows. If NULL, uses the same
  attribute for both dimensions.

- normalized:

  Logical. Whether to return proportions instead of raw counts. Default
  TRUE.

- by_row:

  Logical. If TRUE and normalized=TRUE, normalizes by row. Default
  FALSE.

- include_weights:

  Logical. Whether to use edge weights. Default FALSE.

- other_stats:

  Named list of custom functions for additional statistics.

- ...:

  Additional arguments passed to custom functions.

## Value

List containing:

- `mixing_matrices`:

  Named list of mixing matrices per time/layer

- `summary_stats`:

  Data frame with mixing statistics:

## Details

Mixing matrix elements represent ties between actors with attribute
values i and j. For undirected networks, matrices are symmetrized.
Assortativity ranges from -1 (disassortative) to 1 (assortative).

## Author

Casy Dorff, Shahryar Minhas
