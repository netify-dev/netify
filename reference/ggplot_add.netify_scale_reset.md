# Add scale resets to ggplot

S3 method to add scale reset objects to ggplot objects. This method
resets color, fill, alpha, and size scales using the ggnewscale package.

## Usage

``` r
# S3 method for class 'netify_scale_reset'
ggplot_add(object, plot, ...)
```

## Arguments

- object:

  A netify_scale_reset object created by
  [`reset_scales`](https://netify-dev.github.io/netify/reference/reset_scales.md)

- plot:

  A ggplot object to which scale resets will be added

- ...:

  Additional arguments passed by ggplot2 (used internally)

## Value

A ggplot object with scale resets applied
