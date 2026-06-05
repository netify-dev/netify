# add scale resets to ggplot

s3 method to add scale reset objects to ggplot objects. this method
resets color, fill, alpha, and size scales using the ggnewscale package.

## Usage

``` r
# S3 method for class 'netify_scale_reset'
ggplot_add(object, plot, ...)
```

## Arguments

- object:

  a netify_scale_reset object created by
  [`reset_scales`](https://netify-dev.github.io/netify/reference/reset_scales.md)

- plot:

  a ggplot object to which scale resets will be added

- ...:

  additional arguments passed by ggplot2 (used internally)

## Value

a ggplot object with scale resets applied

## Author

cassy dorff, shahryar minhas
