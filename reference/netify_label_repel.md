# Extract label_repel layer from netify plot components

Extracts the label_repel layer from a netify plot components object.
Label repel annotations display actor names with background boxes and
automatic repositioning to avoid overlaps, providing optimal readability
in dense networks.

## Usage

``` r
netify_label_repel(comp)
```

## Arguments

- comp:

  A netify_plot_components object returned from
  `plot(..., return_components = TRUE)`

## Value

A custom object of class "netify_label_repel" that can be added to a
ggplot object using the + operator. The object contains the label_repel
layer with all its aesthetic mappings and data.

## See also

[`plot.netify`](https://netify-dev.github.io/netify/reference/plot.netify.md),
[`netify_label`](https://netify-dev.github.io/netify/reference/netify_label.md),
[`assemble_netify_plot`](https://netify-dev.github.io/netify/reference/assemble_netify_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# create a netify object
net <- netify(my_data, actor1 = "from", actor2 = "to")

# get plot components with label_repel
comp <- plot(net, add_label_repel = TRUE, return_components = TRUE)

# build custom plot with repelled labels
library(ggplot2)
ggplot() +
    netify_label_repel(comp)
} # }
```
