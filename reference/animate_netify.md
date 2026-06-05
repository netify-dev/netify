# Animate a longitudinal netify object with gganimate

returns a ggplot built from the per-period
[`plot.netify()`](https://netify-dev.github.io/netify/reference/plot.netify.md)
output, with
[`gganimate::transition_manual()`](https://gganimate.com/reference/transition_manual.html)
keyed on the time variable so the animation steps through each period.
requires the `gganimate` package.

## Usage

``` r
animate_netify(netlet, ..., static_actor_positions = TRUE)
```

## Arguments

- netlet:

  a longitudinal netify object (`longit_array` / `longit_list`).

- ...:

  additional arguments passed to
  [`plot.netify()`](https://netify-dev.github.io/netify/reference/plot.netify.md)
  (`node_color_by`, `node_size_by`, `style`, etc.).

- static_actor_positions:

  logical. if `TRUE` (default for animation since positions jumping
  around between periods is visually confusing), pin node positions
  across time.

## Value

a `gganim` object. render with `gganimate::animate(.)` or
`gganimate::anim_save("file.gif", .)`.

## Details

for static facet plots, just call `plot(net)` on a longit netlet – that
defaults to faceting by time. use `animate_netify()` for single-panel
transitions instead of grid faceting (better for presentations / videos
where the eye can focus on one period at a time).

## Author

cassy dorff, shahryar minhas

## Examples

``` r
if (FALSE) { # \dontrun{
library(gganimate)
anim <- animate_netify(longit_net, node_color_by = "polity")
anim_save("trade_anim.gif", anim, fps = 4)
} # }
```
