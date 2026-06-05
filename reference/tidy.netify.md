# Tidy a netify object into a long edge data frame

`tidy.netify` is an s3 method for the `tidy()` generic from the broom
package. it returns one row per edge with all attached nodal and dyadic
attributes – equivalent to `unnetify(x, remove_zeros = TRUE)` but
exposed under the broom convention so the netify object plays nicely
with broom-style workflows. (broom is not a hard dependency; this method
is registered as an s3 method on `tidy` and only triggers when the
generic is available, e.g., when the user has
[`library(broom)`](https://broom.tidymodels.org/) loaded.)

## Usage

``` r
tidy.netify(x, remove_zeros = TRUE, ...)
```

## Arguments

- x:

  a netify object.

- remove_zeros:

  logical. drop zero-weight edges? default `TRUE` (matches the typical
  broom expectation that the returned frame is actually-observed
  observations).

- ...:

  additional arguments passed to
  [`unnetify()`](https://netify-dev.github.io/netify/reference/unnetify.md).

## Value

a tibble (or data.frame if `tibble` isn't installed) with one row per
(non-zero) edge. columns include `from`, `to`, optional `time`
(longitudinal), the edge weight column, dyadic covariates, and nodal
covariates merged in with `_from` / `_to` suffixes. zero-edge inputs
return a 0-row tibble with the schema preserved.

## See also

[`unnetify()`](https://netify-dev.github.io/netify/reference/unnetify.md)
for the underlying converter, and `glance.netify` for
one-row-per-network summary statistics.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
data(icews)
icews_10 <- icews[icews$year == 2010, ]
net <- netify(icews_10, actor1 = "i", actor2 = "j",
symmetric = FALSE, weight = "verbCoop")
td <- tidy.netify(net)
head(td)
#> # A tibble: 6 × 3
#>   from        to         verbCoop
#>   <chr>       <chr>         <dbl>
#> 1 Afghanistan Argentina         1
#> 2 Afghanistan Armenia           7
#> 3 Afghanistan Australia       125
#> 4 Afghanistan Austria           1
#> 5 Afghanistan Azerbaijan        7
#> 6 Afghanistan Bahrain           3
```
