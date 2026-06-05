# add netify_label to ggplot

s3 method to add netify_label objects to ggplot objects. this method is
called automatically when using the + operator with a netify_label
object.

## Usage

``` r
# S3 method for class 'netify_label'
ggplot_add(object, plot, ...)
```

## Arguments

- object:

  a netify_label object created by
  [`netify_label`](https://netify-dev.github.io/netify/reference/netify_label.md)

- plot:

  a ggplot object to which the label layer will be added

- ...:

  additional arguments passed by ggplot2 (used internally)

## Value

a ggplot object with the label layer added

## Author

cassy dorff, shahryar minhas
