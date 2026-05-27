# Read a network from common file formats into a netify object

Thin wrappers around
[`igraph::read_graph()`](https://r.igraph.org/reference/read_graph.html)
for the formats users coming from gephi / pajek / networkx most often
hand over (graphml, pajek `.net`, gml). Each reader loads the file via
igraph and immediately runs
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md) on
the result so the user gets a netify back in one call, with edge weights
auto-detected and directedness preserved.

## Usage

``` r
read_graphml(file, ...)

read_pajek(file, ...)

read_gml(file, ...)
```

## Arguments

- file:

  Path to the input file.

- ...:

  Passed to `netify(igraph_obj, ...)` — typically `symmetric=`, `mode=`,
  or `weight=` overrides.

## Value

A netify object.

## Author

Cassy Dorff, Shahryar Minhas
