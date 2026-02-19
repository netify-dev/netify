# Add netify_node to ggplot

S3 method to add netify_node objects to ggplot objects. This method is
called automatically when using the + operator with a netify_node
object.

## Usage

``` r
# S3 method for class 'netify_node'
ggplot_add(object, plot, ...)
```

## Arguments

- object:

  A netify_node object created by
  [`netify_node`](https://netify-dev.github.io/netify/reference/netify_node.md)

- plot:

  A ggplot object to which the node layer will be added

- ...:

  Additional arguments passed by ggplot2 (used internally)

## Value

A ggplot object with the node layer added
