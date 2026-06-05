# add netify_label_repel to ggplot

s3 method to add netify_label_repel objects to ggplot objects. this
method is called automatically when using the + operator with a
netify_label_repel object.

## Usage

``` r
# S3 method for class 'netify_label_repel'
ggplot_add(object, plot, ...)
```

## Arguments

- object:

  a netify_label_repel object created by
  [`netify_label_repel`](https://netify-dev.github.io/netify/reference/netify_label_repel.md)

- plot:

  a ggplot object to which the label_repel layer will be added

- ...:

  additional arguments passed by ggplot2 (used internally)

## Value

a ggplot object with the label_repel layer added

## Author

cassy dorff, shahryar minhas
