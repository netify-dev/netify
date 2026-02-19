# Create a multi-panel mixing matrix visualization

Creates a faceted plot showing multiple mixing matrices, useful for
comparing patterns across time periods or network layers.

## Usage

``` r
plot_mixing_matrix_facet(
  mixing_results,
  matrices_to_plot = NULL,
  ncol = NULL,
  shared_scale = TRUE,
  ...
)
```

## Arguments

- mixing_results:

  Output from
  [`mixing_matrix()`](https://netify-dev.github.io/netify/reference/mixing_matrix.md)
  with multiple matrices

- matrices_to_plot:

  Integer vector. Which matrices to include. Default NULL plots all.

- ncol:

  Integer. Number of columns in facet layout. Default NULL
  auto-calculates.

- shared_scale:

  Logical. Whether to use the same color scale across panels. Default
  TRUE.

- ...:

  Additional arguments passed to plot_mixing_matrix for each panel

## Value

A ggplot2 object with faceted mixing matrices

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
if (FALSE) { # \dontrun{
# Create temporal network
data(icews)
net_temporal <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Run mixing matrix analysis across time
mixing_temporal <- mixing_matrix(
    net_temporal,
    attribute = "i_polity2_cat"
)

# Create faceted visualization
plot_mixing_matrix_facet(mixing_temporal, ncol = 2)
} # }
```
