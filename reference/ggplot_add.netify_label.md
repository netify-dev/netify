# Add netify_label to ggplot

S3 method to add netify_label objects to ggplot objects. This method is
called automatically when using the + operator with a netify_label
object.

## Usage

``` r
# S3 method for class 'netify_label'
ggplot_add(object, plot, ...)
```

## Arguments

- object:

  A netify_label object created by
  [`netify_label`](https://netify-dev.github.io/netify/reference/netify_label.md)

- plot:

  A ggplot object to which the label layer will be added

- ...:

  Additional arguments passed by ggplot2 (used internally)

## Value

A ggplot object with the label layer added
