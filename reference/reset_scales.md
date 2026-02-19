# Reset aesthetic scales in ggplot

Creates a scale reset object that can be added to a ggplot to reset
color, fill, alpha, and size scales. This is necessary when using
multiple layers with different aesthetic mappings (e.g., different
colors for edges vs nodes).

## Usage

``` r
reset_scales()
```

## Value

A custom object of class "netify_scale_reset" that can be added to a
ggplot object using the + operator

## Details

This function addresses the limitation in ggplot2 where each aesthetic
can only have one scale. By resetting scales between layers, you can
have different color mappings for edges and nodes, for example.

## See also

[`new_scale_color`](https://eliocamp.github.io/ggnewscale/reference/new_scale.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# create a plot with different colors for edges and nodes
comp <- plot(net, return_components = TRUE)

ggplot() +
    netify_edge(comp) +
    scale_color_manual(values = c("gray", "red")) +
    reset_scales() + # reset before adding nodes
    netify_node(comp) +
    scale_color_viridis_c()
} # }
```
