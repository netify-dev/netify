# Merge method for netify objects (S3 alias for `bind_netifies`)

Provides the base-R [`merge()`](https://rdrr.io/r/base/merge.html)
generic dispatch for combining two netify objects along the time axis.
For more than two inputs or programmatic use, prefer
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

  Additional netify objects, or a `names =` argument.

## Value

A `longit_list` netify object.

## Author

Cassy Dorff, Shahryar Minhas
