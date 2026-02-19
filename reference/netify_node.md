# Extract nodes layer from netify plot components

Extracts the node (point) layer from a netify plot components object,
allowing for manual plot construction and customization. Nodes represent
actors in the network and can have various aesthetic mappings like size,
color, and shape.

## Usage

``` r
netify_node(comp)
```

## Arguments

- comp:

  A netify_plot_components object returned from
  `plot(..., return_components = TRUE)`

## Value

A custom object of class "netify_node" that can be added to a ggplot
object using the + operator. The object contains the node layer with all
its aesthetic mappings and data.

## See also

[`plot.netify`](https://netify-dev.github.io/netify/reference/plot.netify.md),
[`netify_edge`](https://netify-dev.github.io/netify/reference/netify_edge.md),
[`assemble_netify_plot`](https://netify-dev.github.io/netify/reference/assemble_netify_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# create a netify object
net <- netify(my_data, actor1 = "from", actor2 = "to")

# get plot components
comp <- plot(net, return_components = TRUE)

# build custom plot with nodes
library(ggplot2)
ggplot() +
    netify_node(comp)
} # }
```
