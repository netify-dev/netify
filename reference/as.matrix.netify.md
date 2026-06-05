# coerce a netify object to a plain matrix

strips the netify class and netify-specific attributes so the result is
a clean numeric matrix carrying only `dim` and `dimnames`. for
longitudinal netify objects, `time` selects which slice to return; it
defaults to the first time period and emits a hint. round-trips
(`net |> as.matrix() |> netify()`) recover a fresh cross-sectional
netify object, but structural attributes (symmetric / diag_to_NA /
weight) are re-detected from the matrix on the way back in rather than
copied across, so a directed matrix or one with a non-na diagonal will
be flagged accordingly.

## Usage

``` r
# S3 method for class 'netify'
as.matrix(x, time = NULL, layer = NULL, ...)
```

## Arguments

- x:

  a netify object.

- time:

  for longitudinal netify objects, either the integer index or character
  label of the time slice to extract. defaults to the first slice and
  emits a hint when used implicitly.

- layer:

  for multilayer netify objects, either the integer index or character
  label of the layer to extract. defaults to the first layer and emits a
  hint when used implicitly.

- ...:

  additional args (ignored).

## Value

a plain numeric matrix with `dim` and `dimnames` only (no `netify`
class, no netify metadata attributes).

## See also

[`get_adjacency`](https://netify-dev.github.io/netify/reference/get_adjacency.md)
for the data.frame-input counterpart that also accepts a netify object;
[`netify`](https://netify-dev.github.io/netify/reference/netify.md) for
rebuilding a netify object from a plain matrix.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
data(icews)
icews_2010 <- icews[icews$year == 2010, ]
net <- netify(icews_2010, actor1 = "i", actor2 = "j",
    symmetric = FALSE, weight = "verbCoop")
m <- as.matrix(net)
dim(m)
#> [1] 152 152
class(m)
#> [1] "matrix" "array" 
```
