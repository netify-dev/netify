# Add netify_label_repel to ggplot

S3 method to add netify_label_repel objects to ggplot objects. This
method is called automatically when using the + operator with a
netify_label_repel object.

## Usage

``` r
# S3 method for class 'netify_label_repel'
ggplot_add(object, plot, ...)
```

## Arguments

- object:

  A netify_label_repel object created by
  [`netify_label_repel`](https://netify-dev.github.io/netify/reference/netify_label_repel.md)

- plot:

  A ggplot object to which the label_repel layer will be added

- ...:

  Additional arguments passed by ggplot2 (used internally)

## Value

A ggplot object with the label_repel layer added
