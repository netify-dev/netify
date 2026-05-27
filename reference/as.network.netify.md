# as.network method for netify objects

S3 method that lets statnet's
[`as.network`](https://rdrr.io/pkg/network/man/network.html) generic
dispatch on netify objects. Equivalent to `netify_to_statnet(x, ...)`.
Registered against the network namespace in `.onLoad`, so the dispatch
works regardless of whether network is attached before or after netify.

## Usage

``` r
as.network.netify(x, ...)
```

## Arguments

- x:

  A netify object.

- ...:

  Extra arguments forwarded to
  [`netify_to_statnet`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
  (e.g. `add_nodal_attribs`, `add_dyad_attribs`).

## Value

A statnet `network` object or list of network objects, as produced by
`netify_to_statnet`.

## Author

Shahryar Minhas
