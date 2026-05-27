# One-row-per-network summary of a netify object (broom style)

`glance.netify` is an S3 method for the `glance()` generic from the
broom package. It returns the graph-level statistics produced by
[`summary.netify()`](https://netify-dev.github.io/netify/reference/summary.netify.md)
— one row per network / time period / layer — so the netify object plays
nicely with broom-style workflows. (broom is not a hard dependency; this
method is registered as an S3 method on `glance` and only triggers when
the generic is available.)

## Usage

``` r
glance.netify(x, ...)
```

## Arguments

- x:

  A netify object.

- ...:

  Additional arguments passed to
  [`summary.netify()`](https://netify-dev.github.io/netify/reference/summary.netify.md)
  (e.g., `other_stats = list(my_stat = my_fn)`).

## Value

A tibble (or data.frame if `tibble` isn't installed): one row per
(network, time, layer) combination with density, reciprocity, mutual,
transitivity, edge counts, etc.

## See also

[`summary.netify()`](https://netify-dev.github.io/netify/reference/summary.netify.md)
for the underlying summary, and `tidy.netify` for one-row-per-edge
output.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
data(icews)
icews_10 <- icews[icews$year == 2010, ]
net <- netify(icews_10, actor1 = "i", actor2 = "j",
symmetric = FALSE, weight = "verbCoop")
glance.netify(net)
#> # A tibble: 1 × 18
#>   net   num_actors density num_edges prop_edges_missing mean_edge_weight
#>   <chr>      <dbl>   <dbl>     <dbl>              <dbl>            <dbl>
#> 1 1            152   0.435      9976                  0             18.1
#> # ℹ 12 more variables: sd_edge_weight <dbl>, median_edge_weight <dbl>,
#> #   min_edge_weight <dbl>, max_edge_weight <dbl>, competition_row <dbl>,
#> #   competition_col <dbl>, sd_of_row_means <dbl>, sd_of_col_means <dbl>,
#> #   covar_of_row_col_means <dbl>, reciprocity <dbl>, mutual <dbl>,
#> #   transitivity <dbl>
```
