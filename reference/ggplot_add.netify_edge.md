# add netify_edge to ggplot

s3 method to add netify_edge objects to ggplot objects. this method is
called automatically when using the + operator with a netify_edge
object.

## Usage

``` r
# S3 method for class 'netify_edge'
ggplot_add(object, plot, ...)
```

## Arguments

- object:

  a netify_edge object created by
  [`netify_edge`](https://netify-dev.github.io/netify/reference/netify_edge.md)

- plot:

  a ggplot object to which the edge layer will be added

- ...:

  additional arguments passed by ggplot2 (used internally)

## Value

a ggplot object with the edge layer added

## Author

cassy dorff, shahryar minhas
