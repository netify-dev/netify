# as.igraph method for netify objects

s3 method that lets igraph's
[`as.igraph`](https://r.igraph.org/reference/as.igraph.html) generic
dispatch on netify objects. equivalent to `netify_to_igraph(x, ...)`.
registered against the igraph namespace in `.onload`, so the dispatch
works regardless of whether igraph is attached before or after netify.

## Usage

``` r
as.igraph.netify(x, ...)
```

## Arguments

- x:

  a netify object.

- ...:

  extra arguments forwarded to
  [`netify_to_igraph`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md)
  (e.g. `add_nodal_attribs`, `add_dyad_attribs`).

## Value

an igraph object, or a list of igraph objects (longitudinal /
multilayer), as produced by `netify_to_igraph`.

## Author

shahryar minhas
