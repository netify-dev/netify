# get raw network data without netify attributes

`get_raw` extracts the underlying network data structure (matrix or
list) from a netify object, removing all netify-specific attributes.
this is useful when you need to work with the base r data structures or
pass the network data to functions that don't recognize netify objects.

## Usage

``` r
get_raw(netlet)
```

## Arguments

- netlet:

  a netify object

## Value

a matrix or list object with netify attributes removed. the structure
returned depends on the type of netify object:

- cross-sectional networks: returns a matrix

- longitudinal array networks: returns a matrix

- longitudinal list networks: returns a list

## Author

shahryar minhas
