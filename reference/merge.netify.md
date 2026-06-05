# merge method for netify objects (s3 alias for `bind_netifies`)

provides the base-r [`merge()`](https://rdrr.io/r/base/merge.html)
generic dispatch for combining two netify objects along the time axis.
for more than two inputs or programmatic use, prefer
[`bind_netifies()`](https://netify-dev.github.io/netify/reference/bind_netifies.md)
directly.

## Usage

``` r
# S3 method for class 'netify'
merge(x, y, ...)
```

## Arguments

- x, y:

  netify objects.

- ...:

  additional netify objects, or a `names =` argument.

## Value

a `longit_list` netify object.

## Author

cassy dorff, shahryar minhas
