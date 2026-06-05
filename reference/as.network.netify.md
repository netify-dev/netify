# as.network method for netify objects

s3 method that lets statnet's
[`as.network`](https://rdrr.io/pkg/network/man/network.html) generic
dispatch on netify objects. equivalent to `netify_to_statnet(x, ...)`.
registered against the network namespace in `.onload`, so the dispatch
works regardless of whether network is attached before or after netify.

## Usage

``` r
as.network.netify(x, ...)
```

## Arguments

- x:

  a netify object.

- ...:

  extra arguments forwarded to
  [`netify_to_statnet`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
  (e.g. `add_nodal_attribs`, `add_dyad_attribs`).

## Value

a statnet `network` object or list of network objects, as produced by
`netify_to_statnet`.

## Author

shahryar minhas
