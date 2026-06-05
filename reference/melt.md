# Melt methods for netify objects

convert netify matrices/arrays to long format data frames. these methods
provide a consistent interface for melting different types of netify
objects while leveraging c++ for performance.

## Usage

``` r
melt(data, ...)

# S3 method for class 'netify'
melt(
  data,
  ...,
  remove_diagonal = TRUE,
  remove_zeros = TRUE,
  na.rm = TRUE,
  value.name = "value"
)
```

## Arguments

- data:

  a netify object

- ...:

  additional arguments (see details)

- remove_diagonal:

  logical. remove diagonal elements (default: TRUE)

- remove_zeros:

  logical. remove zero values (default: TRUE)

- na.rm:

  logical. remove na values (default: TRUE)

- value.name:

  character. name for value column (default: "value")

## Value

see method-specific documentation (e.g., `melt.netify`)

data frame with columns: `var1`, `var2`, `value` (and optionally `time`
/ `layer`). the `var1` / `var2` names are inherited from base r's
`as.data.frame.table` / `reshape2::melt` heritage and are used by
internal helpers (`decompose_helpers`, `plot_homophily`, etc.); rename
them yourself downstream if you need snake_case (e.g.
`rlang::set_names(out, c("from", "to", "value", ...))`). for a fully
snake_case, dyad-attribute-merged edge frame, use
[`unnetify()`](https://netify-dev.github.io/netify/reference/unnetify.md)
or
[`as_tibble.netify()`](https://netify-dev.github.io/netify/reference/as_tibble.netify.md)
instead.

## Details

the melt method converts netify objects from their matrix representation
to a long format data frame suitable for analysis and visualization. the
output format depends on the type of netify object:

- cross-sectional: returns columns `var1`, `var2`, `value`

- longitudinal: returns columns `var1`, `var2`, `time`, `value`

- multilayer: returns columns `var1`, `var2`, `layer`, `value` (and
  `time` if longitudinal)

## Author

cassy dorff, shahryar minhas
