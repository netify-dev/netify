# Get raw network data without netify attributes

`get_raw` extracts the underlying network data structure (matrix or
list) from a netify object, removing all netify-specific attributes.
This is useful when you need to work with the base R data structures or
pass the network data to functions that don't recognize netify objects.

## Usage

``` r
get_raw(netlet)
```

## Arguments

- netlet:

  A netify object

## Value

A matrix or list object with netify attributes removed. The structure
returned depends on the type of netify object:

- Cross-sectional networks: returns a matrix

- Longitudinal array networks: returns a matrix

- Longitudinal list networks: returns a list

## Author

Shahryar Minhas
