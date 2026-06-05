# assemble netify plot from components

assembles a complete network plot from netify plot components. this
function automatically adds all available layers (edges, nodes, text,
labels, and their repel versions) in the correct order with appropriate
scale resets between layers.

## Usage

``` r
assemble_netify_plot(comp)
```

## Arguments

- comp:

  a netify_plot_components object returned from
  `plot(..., return_components = TRUE)`

## Value

a complete ggplot object ready for display or further customization

## Details

this function provides a convenient way to reassemble a plot from its
components after extracting them with `return_components = TRUE`. it
automatically:

- adds layers in the correct order (edges, nodes, text/text_repel,
  labels/label_repel)

- inserts scale resets between layers when necessary

- handles both standard and repel versions of text and label layers

- includes facets and themes if present

## See also

[`plot.netify`](https://netify-dev.github.io/netify/reference/plot.netify.md),
[`netify_edge`](https://netify-dev.github.io/netify/reference/netify_edge.md),
[`netify_node`](https://netify-dev.github.io/netify/reference/netify_node.md)

## Author

cassy dorff, shahryar minhas

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
