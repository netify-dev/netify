# Melt methods for netify objects

Convert netify matrices/arrays to long format data frames. These methods
provide a consistent interface for melting different types of netify
objects while leveraging C++ for performance.

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

  A netify object

- ...:

  Additional arguments (see details)

- remove_diagonal:

  Logical. Remove diagonal elements (default: TRUE)

- remove_zeros:

  Logical. Remove zero values (default: TRUE)

- na.rm:

  Logical. Remove NA values (default: TRUE)

- value.name:

  Character. Name for value column (default: "value")

## Value

See method-specific documentation (e.g., `melt.netify`)

Data frame with columns: `Var1`, `Var2`, `value` (and optionally `time`
/ `layer`). The `Var1` / `Var2` names are inherited from base R's
`as.data.frame.table` / `reshape2::melt` heritage and are used by
internal helpers (`decompose_helpers`, `plot_homophily`, etc.); rename
them yourself downstream if you need snake_case (e.g.
`rlang::set_names(out, c("from", "to", "value", ...))`). For a fully
snake_case, dyad-attribute-merged edge frame, use
[`unnetify()`](https://netify-dev.github.io/netify/reference/unnetify.md)
or
[`as_tibble.netify()`](https://netify-dev.github.io/netify/reference/as_tibble.netify.md)
instead.

## Details

The melt method converts netify objects from their matrix representation
to a long format data frame suitable for analysis and visualization. The
output format depends on the type of netify object:

- Cross-sectional: Returns columns `Var1`, `Var2`, `value`

- Longitudinal: Returns columns `Var1`, `Var2`, `time`, `value`

- Multilayer: Returns columns `Var1`, `Var2`, `layer`, `value` (and
  `time` if longitudinal)

## Author

Cassy Dorff, Shahryar Minhas
