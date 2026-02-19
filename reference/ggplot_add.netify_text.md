# Add netify_text to ggplot

S3 method to add netify_text objects to ggplot objects. This method is
called automatically when using the + operator with a netify_text
object.

## Usage

``` r
# S3 method for class 'netify_text'
ggplot_add(object, plot, ...)
```

## Arguments

- object:

  A netify_text object created by
  [`netify_text`](https://netify-dev.github.io/netify/reference/netify_text.md)

- plot:

  A ggplot object to which the text layer will be added

- ...:

  Additional arguments passed by ggplot2 (used internally)

## Value

A ggplot object with the text layer added
