# Binarize a netify object at a threshold

thin wrapper around
[`mutate_weights()`](https://netify-dev.github.io/netify/reference/mutate_weights.md)
for the very common case of "dichotomize the weighted network at a
cut-point." returns a new netify with edge values coerced to 0/1 based
on the supplied threshold (or threshold function).

## Usage

``` r
binarize(netlet, threshold = 0, strict = FALSE, abs = FALSE, new_name = NULL)
```

## Arguments

- netlet:

  a weighted netify object.

- threshold:

  numeric scalar (default `0` – any nonzero edge becomes 1) or a
  function `f(x)` that takes the vector of edge weights and returns a
  single numeric threshold (e.g., `function(x) median(x, na.rm = TRUE)`
  or `function(x) quantile(x, 0.75, na.rm = TRUE)`).

- strict:

  logical. if `TRUE`, edges with weight strictly greater than the
  threshold become 1; if `FALSE` (default), the threshold itself is
  included (\>=). for `threshold = 0`, the default gives the "any
  nonzero edge counts" semantics that matches the rest of the package
  (signed-weight density, homophily-default-threshold, etc.).

- abs:

  logical. if `TRUE`, compare `|x|` to the threshold so that
  negative-magnitude ties also count toward the binarization. defaults
  to `FALSE`. when the network contains both positive and negative
  weights and `abs = FALSE`, `binarize()` informs once that negative
  ties will be dropped.

- new_name:

  optional character. new name for the binarized weight column (default
  keeps the original name).

## Value

a binarized netify object with `is_binary = TRUE`.

## Details

na cells (e.g., the diagonal under `diag_to_NA = TRUE`) propagate as na
in the output rather than becoming 0. use `na.rm = TRUE` when summing
edges if you want them treated as 0. structural zeros stay zero in every
branch – a negative `threshold` will not promote empty cells to 1,
regardless of `strict` or `abs`.

## See also

[`mutate_weights()`](https://netify-dev.github.io/netify/reference/mutate_weights.md)
for arbitrary transformations.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
if (FALSE) { # \dontrun{
data(icews)
net <- netify(icews[icews$year == 2010, ],
actor1 = "i", actor2 = "j", symmetric = FALSE, weight = "verbCoop")
# any-nonzero-edge dichotomization
bin0 <- binarize(net)
# 75th-percentile of nonzero weights
bin75 <- binarize(net, threshold = function(x) {
nz <- x[x > 0]
quantile(nz, 0.75, na.rm = TRUE)
})
} # }
```
