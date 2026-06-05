# add netify_text to ggplot

s3 method to add netify_text objects to ggplot objects. this method is
called automatically when using the + operator with a netify_text
object.

## Usage

``` r
# S3 method for class 'netify_text'
ggplot_add(object, plot, ...)
```

## Arguments

- object:

  a netify_text object created by
  [`netify_text`](https://netify-dev.github.io/netify/reference/netify_text.md)

- plot:

  a ggplot object to which the text layer will be added

- ...:

  additional arguments passed by ggplot2 (used internally)

## Value

a ggplot object with the text layer added

## Author

cassy dorff, shahryar minhas
