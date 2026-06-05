# extract text layer from netify plot components

extracts the text label layer from a netify plot components object. text
labels display actor names or other text annotations directly on the
plot without background boxes.

## Usage

``` r
netify_text(comp)
```

## Arguments

- comp:

  a netify_plot_components object returned from
  `plot(..., return_components = TRUE)`

## Value

a custom object of class "netify_text" that can be added to a ggplot
object using the + operator. the object contains the text layer with all
its aesthetic mappings and data.

## See also

[`plot.netify`](https://netify-dev.github.io/netify/reference/plot.netify.md),
[`netify_label`](https://netify-dev.github.io/netify/reference/netify_label.md),
[`assemble_netify_plot`](https://netify-dev.github.io/netify/reference/assemble_netify_plot.md)

## Author

cassy dorff, shahryar minhas

## Examples

``` r
if (FALSE) { # \dontrun{
# create a netify object
net <- netify(my_data, actor1 = "from", actor2 = "to")

# get plot components with text labels
comp <- plot(net, add_text = TRUE, return_components = TRUE)

# build custom plot with text
library(ggplot2)
ggplot() +
    netify_text(comp)
} # }
```
