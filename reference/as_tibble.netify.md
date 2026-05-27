# Convert a netify object to a tibble (long edge frame)

S3 method for
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).
Returns the same long-format frame as
[`unnetify()`](https://netify-dev.github.io/netify/reference/unnetify.md)
/
[`tidy.netify()`](https://netify-dev.github.io/netify/reference/tidy.netify.md),
wrapped in a tibble for tidyverse-pipe friendliness.

## Usage

``` r
as_tibble.netify(x, ...)
```

## Arguments

- x:

  A netify object.

- ...:

  Passed to
  [`unnetify()`](https://netify-dev.github.io/netify/reference/unnetify.md)
  (e.g., `remove_zeros = TRUE`).

## Value

A tibble (or data.frame if tibble isn't installed) with one row per
dyad. Includes `from`, `to`, optional `time`/`layer`, the edge weight,
dyadic covariates, and nodal covariates merged in with `_from` / `_to`
suffixes.

## Details

Registered against the
[`tibble::as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)
generic via `.onLoad`, so `tibble` is not a hard dependency. When
`tibble` isn't installed, a plain data.frame is returned.

## See also

[`tidy.netify()`](https://netify-dev.github.io/netify/reference/tidy.netify.md)
for the broom-style sibling and
[`unnetify()`](https://netify-dev.github.io/netify/reference/unnetify.md)
for the underlying converter.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
if (FALSE) { # \dontrun{
data(icews)
icews_10 <- icews[icews$year == 2010, ]
net <- netify(icews_10, actor1 = "i", actor2 = "j",
    symmetric = FALSE, weight = "verbCoop")
tibble::as_tibble(net)
} # }
```
