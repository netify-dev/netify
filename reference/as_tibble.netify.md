# convert a netify object to a tibble (long edge frame)

s3 method for
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).
returns the same long-format frame as
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

  a netify object.

- ...:

  passed to
  [`unnetify()`](https://netify-dev.github.io/netify/reference/unnetify.md)
  (e.g., `remove_zeros = TRUE`).

## Value

a tibble (or data.frame if tibble isn't installed) with one row per
dyad. includes `from`, `to`, optional `time`/`layer`, the edge weight,
dyadic covariates, and nodal covariates merged in with `_from` / `_to`
suffixes.

## Details

registered against the
[`tibble::as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)
generic via `.onload`, so `tibble` is not a hard dependency. when
`tibble` isn't installed, a plain data.frame is returned.

## See also

[`tidy.netify()`](https://netify-dev.github.io/netify/reference/tidy.netify.md)
for the broom-style sibling and
[`unnetify()`](https://netify-dev.github.io/netify/reference/unnetify.md)
for the underlying converter.

## Author

cassy dorff, shahryar minhas

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
