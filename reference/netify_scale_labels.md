# Set scale labels for netify plots

Provides a convenient way to set labels for aesthetic scales in netify
plots. This function simplifies the process of labeling scales that may
be spread across different layers (edges, nodes, text, labels).

## Usage

``` r
netify_scale_labels(...)
```

## Arguments

- ...:

  Named arguments where the name is the aesthetic_component (e.g.,
  "edge_alpha", "node_size", "edge_color") and the value is the label
  text to display in the legend

## Value

A custom object of class "netify_labels" that can be added to a netify
plot using the + operator

## Details

This function provides a user-friendly interface for setting scale
labels without needing to understand the complexity of ggnewscale. The
naming convention is:

- `edge_*` for edge aesthetics (e.g., edge_color, edge_alpha)

- `node_*` or `point_*` for node aesthetics (both work)

- `text_*` for text label aesthetics

- `label_*` for boxed label aesthetics

## Note

This function only works with plots created using netify's plot method.
It will issue a warning if used with other ggplot objects.

## See also

[`plot.netify`](https://netify-dev.github.io/netify/reference/plot.netify.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# set labels for different scales
plot(my_netify_obj,
    edge_alpha_var = "weight",
    point_size_var = "degree"
) +
    netify_scale_labels(
        edge_alpha = "Connection Strength",
        node_size = "Node Degree" # node_* is converted to point_*
    )
} # }
```
