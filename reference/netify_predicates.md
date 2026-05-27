# Type predicates and convenience accessors for netify objects

These mirror the natural shape questions a user would ask: is this
object bipartite? longitudinal? multilayer? how many actors does it
have? They share a roxygen page with the small attribute-accessor
helpers `is_binary()` and `nodal_data()`.

## Usage

``` r
is_binary(x)

nodal_data(x)

is_bipartite(x)

is_bipartite_netify(x)

is_directed_netify(x)

is_longitudinal(x)

is_multilayer(x)

is_symmetric_netify(x)

n_actors(x)

n_periods(x)

n_layers(x)
```

## Arguments

- x:

  A netify object.

## Value

`is_binary()` returns a single logical: `TRUE` when every off-diagonal
cell of the underlying adjacency is `0`, `1`, or `NA`. Reads the cached
`"is_binary"` attribute when available and falls back to probing the raw
matrix / array / list.

`nodal_data()` returns the nodal-attribute data.frame stored on the
netify object (the `"nodal_data"` attribute), or `NULL` if no nodal
attributes have been attached. Convenience wrapper so users do not have
to remember the [`attr()`](https://rdrr.io/r/base/attr.html) call.

`is_bipartite()` returns a single logical. If `igraph` is loaded after
`netify`, the bare `is_bipartite()` may be masked by
[`igraph::is_bipartite()`](https://r.igraph.org/reference/is_bipartite.html)
(which doesn't accept a netify). Use `is_bipartite_netify()` (alias) or
call as `netify::is_bipartite()` to avoid the collision.

`is_bipartite_netify()` is an alias for `is_bipartite()` that won't
collide with
[`igraph::is_bipartite()`](https://r.igraph.org/reference/is_bipartite.html)
when both packages are attached.

`is_directed_netify()` returns a single logical. Convenience alias for
`!isTRUE(attr(x, "symmetric"))` that won't collide with
[`igraph::is_directed()`](https://r.igraph.org/reference/is_directed.html).

`is_longitudinal()` returns a single logical.

`is_multilayer()` returns a single logical.

`is_symmetric_netify()` returns a single logical (or, for
mixed-directedness multilayer, the per-layer vector).

`n_actors()` returns a single integer (number of unique actors across
all periods / both modes; for bipartite a length-2 integer vector
`c(row, col)`).

`n_periods()` returns a single integer (1 for cross-sectional).

`n_layers()` returns a single integer (1 for single-layer).

## Author

Cassy Dorff, Shahryar Minhas
