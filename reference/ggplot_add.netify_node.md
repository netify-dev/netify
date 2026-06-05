# add netify_node to ggplot

s3 method to add netify_node objects to ggplot objects. this method is
called automatically when using the + operator with a netify_node
object.

## Usage

``` r
# S3 method for class 'netify_node'
ggplot_add(object, plot, ...)
```

## Arguments

- object:

  a netify_node object created by
  [`netify_node`](https://netify-dev.github.io/netify/reference/netify_node.md)

- plot:

  a ggplot object to which the node layer will be added

- ...:

  additional arguments passed by ggplot2 (used internally)

## Value

a ggplot object with the node layer added

## Author

cassy dorff, shahryar minhas
