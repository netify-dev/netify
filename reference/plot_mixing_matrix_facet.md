# create a multi-panel mixing matrix visualization

creates a faceted plot showing multiple mixing matrices, useful for
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

  output from
  [`mixing_matrix()`](https://netify-dev.github.io/netify/reference/mixing_matrix.md)
  with multiple matrices

- matrices_to_plot:

  integer vector. which matrices to include. default NULL plots all.

- ncol:

  integer. number of columns in facet layout. default NULL
  auto-calculates.

- shared_scale:

  logical. whether to use the same color scale across panels. default
  TRUE.

- ...:

  additional arguments passed to plot_mixing_matrix for each panel

## Value

a ggplot2 object with faceted mixing matrices

## Author

cassy dorff, shahryar minhas

## Examples

``` r
if (FALSE) { # \dontrun{
# create temporal network
data(icews)
net_temporal <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# run mixing matrix analysis across time
mixing_temporal <- mixing_matrix(
    net_temporal,
    attribute = "i_polity2_cat"
)

# create faceted visualization
plot_mixing_matrix_facet(mixing_temporal, ncol = 2)
} # }
```
