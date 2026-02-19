# Validate plot parameters and warn about common mistakes

This function checks the parameter names passed through ... in
plot.netify and warns users about potential typos or deprecated
parameter names.

## Usage

``` r
validate_plot_params(plot_args, ...)
```

## Arguments

- plot_args:

  List of all plot arguments

- ...:

  Additional arguments passed to plot.netify

## Value

NULL (function is called for side effects - warnings)
