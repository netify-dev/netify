# Add netify scale labels to ggplot

S3 method to add netify_labels objects to ggplot objects. This method
updates the labels of existing scales based on the specifications in the
netify_labels object.

## Usage

``` r
# S3 method for class 'netify_labels'
ggplot_add(object, plot, ...)
```

## Arguments

- object:

  A netify_labels object created by
  [`netify_scale_labels`](https://netify-dev.github.io/netify/reference/netify_scale_labels.md)

- plot:

  A ggplot object to which the labels will be added

- ...:

  Additional arguments passed by ggplot2 (used internally)

## Value

A ggplot object with updated scale labels
