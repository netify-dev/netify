# Extract edges layer from netify plot components

Extracts the edge layer from a netify plot components object, allowing
for manual plot construction and customization. This function is part of
the modular plotting system that enables fine-grained control over
network visualization elements.

## Usage

``` r
netify_edge(comp)
```

## Arguments

- comp:

  A netify_plot_components object returned from
  `plot(..., return_components = TRUE)`

## Value

A custom object of class "netify_edge" that can be added to a ggplot
object using the + operator. The object contains the edge layer with all
its aesthetic mappings and data.

## See also

[`plot.netify`](https://netify-dev.github.io/netify/reference/plot.netify.md),
[`netify_node`](https://netify-dev.github.io/netify/reference/netify_node.md),
[`assemble_netify_plot`](https://netify-dev.github.io/netify/reference/assemble_netify_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# create a netify object
net <- netify(my_data, actor1 = "from", actor2 = "to")

# get plot components
comp <- plot(net, return_components = TRUE)

# build custom plot with edges
library(ggplot2)
ggplot() +
    netify_edge(comp)
} # }
```
