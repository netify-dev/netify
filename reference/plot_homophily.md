# Visualize homophily analysis results

Creates visualizations for homophily analysis results from
[`homophily()`](https://netify-dev.github.io/netify/reference/homophily.md).
The function can create different types of plots including similarity
distributions, comparison plots across multiple attributes, and temporal
evolution plots.

## Usage

``` r
plot_homophily(
  homophily_results,
  netlet = NULL,
  type = c("distribution", "comparison", "temporal"),
  attribute = NULL,
  method = "correlation",
  sample_size = NULL,
  colors = c("#2E86AB", "#F18F01"),
  ...
)
```

## Arguments

- homophily_results:

  Data frame output from
  [`homophily()`](https://netify-dev.github.io/netify/reference/homophily.md)
  or a list of such data frames for comparison plots.

- netlet:

  Optional. The netify object used in the analysis. Required for
  distribution plots to extract actual similarity data.

- type:

  Character string specifying the plot type:

  "distribution"

  :   Shows similarity score distributions for connected vs unconnected
      pairs (requires netlet)

  "comparison"

  :   Compares homophily across multiple attributes

  "temporal"

  :   Shows homophily evolution over time (for longitudinal data)

- attribute:

  Character string. For distribution plots, specifies which attribute to
  visualize. Should match the attribute used in
  [`homophily()`](https://netify-dev.github.io/netify/reference/homophily.md).

- method:

  Character string. For distribution plots, the similarity method used.
  Should match the method used in
  [`homophily()`](https://netify-dev.github.io/netify/reference/homophily.md).

- sample_size:

  Integer. For distribution plots with large networks, the number of
  dyad pairs to sample for visualization. Default is NULL (use all
  pairs).

- colors:

  Character vector of two colors for connected/unconnected or
  significant/non-significant pairs. Default uses package theme colors.

- ...:

  Additional arguments passed to ggplot2 functions.

## Value

A ggplot2 object that can be further customized.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example data
data(icews)

# Create a simple network
ntwk <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    time = "year",
    symmetric = FALSE,
    weight = "matlCoop"
)

# Run homophily analysis
homophily_result <- homophily(
    ntwk,
    attribute = "i_polity2",
    method = "correlation"
)

# Create distribution plot
plot_homophily(
    homophily_result,
    netlet = ntwk,
    type = "distribution",
    attribute = "i_polity2"
)
} # }
```
