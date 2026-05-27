# Plot method for `netify_comparison` objects

Renders a pairwise similarity heatmap from the matrix output of
[`compare_networks()`](https://netify-dev.github.io/netify/reference/compare_networks.md).
Picks an appropriate matrix based on the `method` the comparison was run
with (correlation / qap / jaccard / hamming / spectral). For QAP
comparisons, the lower triangle shows QAP correlations and (optionally)
the upper triangle shows p-values, so a single plot communicates both
effect and inference.

## Usage

``` r
# S3 method for class 'netify_comparison'
plot(x, ...)
```

## Arguments

- x:

  A `netify_comparison` object returned by
  [`compare_networks()`](https://netify-dev.github.io/netify/reference/compare_networks.md).

- ...:

  Additional arguments:

  `metric`

  :   Which matrix to render. Default: auto-pick the primary metric for
      the comparison's method. Pass `"correlation"` / `"jaccard"` /
      `"hamming"` / `"qap"` / `"spectral"` to override.

  `show_values`

  :   Logical. Overlay numeric values on each tile. Default `TRUE` when
      n \<= 6, else `FALSE`.

  `palette`

  :   Two-color gradient endpoints. Default `c("#f0f0f0", "#1f78b4")`
      for correlation-like metrics.

## Value

A `ggplot` object.

## Author

Cassy Dorff, Shahryar Minhas
