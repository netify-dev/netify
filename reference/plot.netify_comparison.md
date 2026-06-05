# Plot method for `netify_comparison` objects

renders a pairwise similarity heatmap from the matrix output of
[`compare_networks()`](https://netify-dev.github.io/netify/reference/compare_networks.md).
picks an appropriate matrix based on the `method` the comparison was run
with (correlation / qap / jaccard / hamming / spectral). for qap
comparisons, the plot renders the qap correlation matrix. p-values
remain available in `x$significance_tests$qap_pvalues`.

## Usage

``` r
# S3 method for class 'netify_comparison'
plot(x, ...)
```

## Arguments

- x:

  a `netify_comparison` object returned by
  [`compare_networks()`](https://netify-dev.github.io/netify/reference/compare_networks.md).

- ...:

  additional arguments:

  `metric`

  :   which matrix to render. default: auto-pick the primary metric for
      the comparison's method. pass `"correlation"` / `"jaccard"` /
      `"hamming"` / `"qap"` / `"spectral"` to override.

  `show_values`

  :   logical. overlay numeric values on each tile. default `TRUE` when
      n \<= 6, else `FALSE`.

  `palette`

  :   two-color gradient endpoints. default `c("#f0f0f0", "#1f78b4")`
      for correlation-like metrics.

## Value

a `ggplot` object.

## Author

cassy dorff, shahryar minhas
