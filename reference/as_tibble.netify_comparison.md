# Convert a netify_comparison to a tibble

s3 method for
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
that returns the `$comparisons` data frame directly. the raw
`netify_comparison` object is a list with mixed scalar / nested fields,
which
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
cannot coerce cleanly. the per-pair comparison table is almost always
what tidyverse users want downstream (filter / arrange / pivot / join
with metadata).

## Usage

``` r
as_tibble.netify_comparison(x, ...)
```

## Arguments

- x:

  a `netify_comparison` object from
  [`compare_networks()`](https://netify-dev.github.io/netify/reference/compare_networks.md).

- ...:

  currently unused.

## Value

a tibble of pairwise comparisons (one row per (`net_i`, `net_j`,
`metric`) triple). if the comparison object has no `$comparisons` slot
(e.g., a single-network input), an empty tibble is returned with a
one-shot inform.

## Details

registered against the
[`tibble::as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)
generic via `.onload`, so `tibble` is not a hard dependency.

## See also

[`compare_networks()`](https://netify-dev.github.io/netify/reference/compare_networks.md),
[`as_tibble.netify()`](https://netify-dev.github.io/netify/reference/as_tibble.netify.md).

## Author

cassy dorff, shahryar minhas
