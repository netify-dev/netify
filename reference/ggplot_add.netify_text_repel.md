# add netify_text_repel to ggplot

s3 method to add netify_text_repel objects to ggplot objects. this
method is called automatically when using the + operator with a
netify_text_repel object.

## Usage

``` r
# S3 method for class 'netify_text_repel'
ggplot_add(object, plot, ...)
```

## Arguments

- object:

  a netify_text_repel object created by
  [`netify_text_repel`](https://netify-dev.github.io/netify/reference/netify_text_repel.md)

- plot:

  a ggplot object to which the text_repel layer will be added

- ...:

  additional arguments passed by ggplot2 (used internally)

## Value

a ggplot object with the text_repel layer added

## Author

cassy dorff, shahryar minhas
