# plot method for summary_actor output

s3 method that dispatches
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on a
`summary_actor` data frame to
[`plot_actor_stats()`](https://netify-dev.github.io/netify/reference/plot_actor_stats.md)
so the `summary_actor(net) |> plot()` idiom works without the user
having to remember the helper name. pass any
[`plot_actor_stats()`](https://netify-dev.github.io/netify/reference/plot_actor_stats.md)
argument through `...`.

## Usage

``` r
# S3 method for class 'summary_actor'
plot(x, ...)
```

## Arguments

- x:

  a `summary_actor` data frame from
  [`summary_actor()`](https://netify-dev.github.io/netify/reference/summary_actor.md).

- ...:

  additional arguments passed to
  [`plot_actor_stats()`](https://netify-dev.github.io/netify/reference/plot_actor_stats.md)
  (e.g. `across_actor`, `specific_stats`, `specific_actors`).

## Value

a `ggplot` object.

## Author

cassy dorff, shahryar minhas
