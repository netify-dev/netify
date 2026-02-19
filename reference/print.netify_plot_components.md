# Print netify plot components

Prints a summary of the components available in a netify_plot_components
object. This helps users understand what layers and elements are
available for manual plot construction.

## Usage

``` r
# S3 method for class 'netify_plot_components'
print(x, ...)
```

## Arguments

- x:

  A netify_plot_components object returned from
  `plot(..., return_components = TRUE)`

- ...:

  Additional arguments (currently unused)

## Value

Invisibly returns the input object

## Examples

``` r
if (FALSE) { # \dontrun{
# create plot components
comp <- plot(my_netify_obj, return_components = TRUE)

# print summary
print(comp)
} # }
```
