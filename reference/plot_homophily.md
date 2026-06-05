# Visualize homophily analysis results

creates visualizations for homophily analysis results from
[`homophily()`](https://netify-dev.github.io/netify/reference/homophily.md).
the function can create different types of plots including similarity
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

  data frame output from
  [`homophily()`](https://netify-dev.github.io/netify/reference/homophily.md)
  or a list of such data frames for comparison plots.

- netlet:

  optional. the netify object used in the analysis. required for
  distribution plots to extract actual similarity data.

- type:

  character string specifying the plot type:

  "distribution"

  :   shows similarity score distributions for connected vs unconnected
      pairs (requires netlet)

  "comparison"

  :   compares homophily across multiple attributes

  "temporal"

  :   shows homophily evolution over time (for longitudinal data)

- attribute:

  character string. for distribution plots, specifies which attribute to
  visualize. should match the attribute used in
  [`homophily()`](https://netify-dev.github.io/netify/reference/homophily.md).

- method:

  character string. for distribution plots, the similarity method used.
  should match the method used in
  [`homophily()`](https://netify-dev.github.io/netify/reference/homophily.md).

- sample_size:

  integer. for distribution plots with large networks, the number of
  dyad pairs to sample for visualization. default is NULL (use all
  pairs).

- colors:

  character vector of two colors for connected/unconnected or
  significant/non-significant pairs. default uses package theme colors.

- ...:

  additional arguments passed to ggplot2 functions.

## Value

a ggplot2 object that can be further customized.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
if (FALSE) { # \dontrun{
# load example data
data(icews)

# create a network with nodal attributes
ntwk <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    time = "year",
    symmetric = FALSE,
    weight = "matlCoop",
    nodal_vars = "i_polity2"
)

# run homophily analysis
homophily_result <- homophily(
    ntwk,
    attribute = "i_polity2",
    method = "correlation"
)

# create distribution plot
plot_homophily(
    homophily_result,
    netlet = ntwk,
    type = "distribution",
    attribute = "i_polity2"
)
} # }
```
