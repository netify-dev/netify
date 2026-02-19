# Extract text_repel layer from netify plot components

Extracts the text_repel layer from a netify plot components object. Text
repel labels display actor names with automatic repositioning to avoid
overlaps, making them more readable in dense networks.

## Usage

``` r
netify_text_repel(comp)
```

## Arguments

- comp:

  A netify_plot_components object returned from
  `plot(..., return_components = TRUE)`

## Value

A custom object of class "netify_text_repel" that can be added to a
ggplot object using the + operator. The object contains the text_repel
layer with all its aesthetic mappings and data.

## See also

[`plot.netify`](https://netify-dev.github.io/netify/reference/plot.netify.md),
[`netify_text`](https://netify-dev.github.io/netify/reference/netify_text.md),
[`assemble_netify_plot`](https://netify-dev.github.io/netify/reference/assemble_netify_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# create a netify object
net <- netify(my_data, actor1 = "from", actor2 = "to")

# get plot components with text_repel
comp <- plot(net, add_text_repel = TRUE, return_components = TRUE)

# build custom plot with repelled text
library(ggplot2)
ggplot() +
    netify_text_repel(comp)
} # }
```
