# print netify plot components

prints a summary of the components available in a netify_plot_components
object. this helps users understand what layers and elements are
available for manual plot construction.

## Usage

``` r
# S3 method for class 'netify_plot_components'
print(x, ...)
```

## Arguments

- x:

  a netify_plot_components object returned from
  `plot(..., return_components = TRUE)`

- ...:

  additional arguments (currently unused)

## Value

invisibly returns the input object

## Author

cassy dorff, shahryar minhas

## Examples

``` r
if (FALSE) { # \dontrun{
# create plot components
comp <- plot(my_netify_obj, return_components = TRUE)

# print summary
print(comp)
} # }
```
