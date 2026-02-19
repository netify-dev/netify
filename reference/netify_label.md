# Extract label layer from netify plot components

Extracts the label layer from a netify plot components object. Labels
display actor names or other text annotations with background boxes,
making them more visible against complex network backgrounds.

## Usage

``` r
netify_label(comp)
```

## Arguments

- comp:

  A netify_plot_components object returned from
  `plot(..., return_components = TRUE)`

## Value

A custom object of class "netify_label" that can be added to a ggplot
object using the + operator. The object contains the label layer with
all its aesthetic mappings and data.

## See also

[`plot.netify`](https://netify-dev.github.io/netify/reference/plot.netify.md),
[`netify_text`](https://netify-dev.github.io/netify/reference/netify_text.md),
[`assemble_netify_plot`](https://netify-dev.github.io/netify/reference/assemble_netify_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# create a netify object
net <- netify(my_data, actor1 = "from", actor2 = "to")

# get plot components with labels
comp <- plot(net, add_label = TRUE, return_components = TRUE)

# build custom plot with labels
library(ggplot2)
ggplot() +
    netify_label(comp)
} # }
```
