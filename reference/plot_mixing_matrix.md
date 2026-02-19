# Visualize attribute mixing matrix results

Creates heatmap visualizations for attribute mixing matrices from
[`mixing_matrix()`](https://netify-dev.github.io/netify/reference/mixing_matrix.md).
The function creates a tile plot showing how different attribute
categories interact in the network.

## Usage

``` r
plot_mixing_matrix(
  mixing_results,
  which_matrix = 1,
  show_values = TRUE,
  value_digits = 2,
  color_scale = c("#F18F01", "white", "#2E86AB"),
  midpoint = NULL,
  text_size = 4,
  text_color = "black",
  text_color_threshold = NULL,
  tile_border_color = "white",
  tile_border_size = 0.5,
  reorder_categories = FALSE,
  diagonal_emphasis = TRUE,
  ...
)
```

## Arguments

- mixing_results:

  Output from
  [`mixing_matrix()`](https://netify-dev.github.io/netify/reference/mixing_matrix.md)
  containing mixing matrices and summary statistics.

- which_matrix:

  Integer or character. Which matrix to plot if multiple are present.
  Default is 1 (first matrix).

- show_values:

  Logical. Whether to display values in each tile. Default TRUE.

- value_digits:

  Integer. Number of decimal places for displayed values. Default 2.

- color_scale:

  Character vector of three colors for low, mid, and high values.
  Default uses a diverging color scale.

- midpoint:

  Numeric. The midpoint for the diverging color scale. Default NULL
  automatically calculates based on data range.

- text_size:

  Numeric. Size of value labels in tiles. Default 4.

- text_color:

  Character. Color of text labels. Default "black".

- text_color_threshold:

  Numeric. If provided, values above this threshold (0-1 scale) will use
  white text, values below will use black text. Default NULL uses
  text_color for all.

- tile_border_color:

  Character. Color of tile borders. Default "white".

- tile_border_size:

  Numeric. Width of tile borders. Default 0.5.

- reorder_categories:

  Logical. Whether to reorder categories by similarity. Default FALSE.

- diagonal_emphasis:

  Logical. Whether to emphasize diagonal cells (within-group mixing).
  Default TRUE.

- ...:

  Additional arguments passed to ggplot2 functions.

## Value

A ggplot2 object that can be further customized.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a network with categorical attributes
data(icews)
icews_10 <- icews[icews$year == 2010, ]
net <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Run mixing matrix analysis
mixing_result <- mixing_matrix(
    net,
    attribute = "i_polity2_cat"
)

# Create visualization
plot_mixing_matrix(mixing_result)
} # }
```
