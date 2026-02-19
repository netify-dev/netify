# Assemble netify plot from components

Assembles a complete network plot from netify plot components. This
function automatically adds all available layers (edges, nodes, text,
labels, and their repel versions) in the correct order with appropriate
scale resets between layers.

## Usage

``` r
assemble_netify_plot(comp)
```

## Arguments

- comp:

  A netify_plot_components object returned from
  `plot(..., return_components = TRUE)`

## Value

A complete ggplot object ready for display or further customization

## Details

This function provides a convenient way to reassemble a plot from its
components after extracting them with `return_components = TRUE`. It
automatically:

- Adds layers in the correct order (edges, nodes, text/text_repel,
  labels/label_repel)

- Inserts scale resets between layers when necessary

- Handles both standard and repel versions of text and label layers

- Includes facets and themes if present

## See also

[`plot.netify`](https://netify-dev.github.io/netify/reference/plot.netify.md),
[`netify_edge`](https://netify-dev.github.io/netify/reference/netify_edge.md),
[`netify_node`](https://netify-dev.github.io/netify/reference/netify_node.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# create a netify object
net <- netify(my_data, actor1 = "from", actor2 = "to")

# get plot components
comp <- plot(net, return_components = TRUE)

# reassemble the plot
p <- assemble_netify_plot(comp)
print(p)
} # }
```
