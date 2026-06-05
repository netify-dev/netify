# ggplot theme for netify network plots

a ggplot theme with a larger base font, italicized legend titles, and
(by default) stripped axes / panel chrome – the right default for a
force-directed network layout. drop on top of any
[`plot.netify()`](https://netify-dev.github.io/netify/reference/plot.netify.md)
output.

## Usage

``` r
theme_publication_netify(base_size = 12, for_network = TRUE)
```

## Arguments

- base_size:

  base font size, passed to
  [`theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).
  default 12.

- for_network:

  logical. when `TRUE` (default) removes axis text / titles / ticks /
  panel border, which is the right behavior for a force-directed network
  layout. set `FALSE` if you want to keep axes on (e.g. for a heatmap or
  actor-stat plot).

## Value

a ggplot2 theme object.

## Author

cassy dorff, shahryar minhas
